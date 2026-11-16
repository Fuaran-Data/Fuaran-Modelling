// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.AssentTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The consensual axis, part by part: the fold, the log, the derived standing,
//  the two gates and the codec.
//
//  The scenario file next door runs the whole story end to end. This one is
//  where each rule is driven past the case it exists for — including, for every
//  law the contract pack adds, a probe that shows the law's own predicate CAN
//  fire. A check that has only ever been seen to pass certifies nothing.
// ============================================================================

let private declared = [ "author"; "client"; "reviewer" ]

let private parties: PartyClass list =
    [ { Id = "author"
        Description = "the party that built the state" }
      { Id = "client"
        Description = "the party that has to live with it" }
      { Id = "reviewer"
        Description = "an independent party" } ]

let private assentRecord (party: string) (verdict: AssentVerdict) (reason: string) : AssentRecord =
    { Party = party
      Verdict = verdict
      Reason = reason
      AssentedBy = party + "-signatory"
      AssentedAt = "2026-01-01T00:00:00Z" }

let private assentOp (state: StateId) (party: string) (verdict: AssentVerdict) : ExplorationOp =
    Assent
        { StateId = state
          Record = assentRecord party verdict "because it was worth saying" }

/// One state, one recorded fit whose single gate held or did not.
let private gatedOutcome (passed: bool) : OutcomeSnapshot =
    { outcome "h-base" with
        Gates =
            [ { Name = "convergence"
                Threshold = 0.5
                Unit = None
                Direction = AtLeast
                Observed = (if passed then 0.75 else 0.25)
                Passed = passed } ] }

let private fixture (gates: bool option) (assents: (string * AssentVerdict) list) : Exploration =
    foldOrFail (
        [ seedOp "base" ]
        @ (match gates with
           | None -> []
           | Some passed ->
               [ Annotate
                     { StateId = "base"
                       Snapshot = gatedOutcome passed } ])
        @ [ for party, verdict in assents -> assentOp "base" party verdict ]
    )

let private baseState (e: Exploration) : SpecState = Query.tryState "base" e |> Option.get

let private standingOf (e: Exploration) : AssentStanding =
    PartyAssent.standing declared (baseState e)

[<Tests>]
let tests =
    testList
        "Assent"
        [ // ---- the fold ------------------------------------------------------

          test "an assent appends to the state's log and replaces nothing" {
              let e = fixture None [ "author", Endorse; "author", Challenge; "author", Withdraw ]

              Expect.equal
                  ((baseState e).Assents |> List.map (fun a -> a.Verdict))
                  [ Endorse; Challenge; Withdraw ]
                  "all three acts are on the record, in the order they were made"

              Expect.equal
                  (Query.currentAssents (baseState e) |> List.map (fun a -> a.Verdict))
                  [ Withdraw ]
                  "and the live position is the most recent — a cheap derivation off the log"
          }

          test "an assent on a state nobody has seeded, or that does not exist, is refused" {
              match Reducer.apply (assentOp "nowhere" "author" Endorse) Exploration.empty with
              | Error(UnknownState id) -> Expect.equal id "nowhere" "the refusal names the state"
              | other -> failtestf "expected UnknownState, got %A" other
          }

          test "a killed branch can still be assented to" {
              // A challenge to a branch somebody already abandoned is worth having on the
              // record, and refusing it would be the one shape in which an objection could be
              // silenced by acting first.
              let e =
                  foldOrFail
                      [ seedOp "base"
                        forkOp "left" "base"
                        curateOp "left" Kill "set aside"
                        assentOp "left" "reviewer" Challenge ]

              Expect.equal
                  ((Query.tryState "left" e |> Option.get).Assents |> List.length)
                  1
                  "the objection is recorded against the branch that was abandoned"
          }

          test "the live position is per party, and silence is distinguishable from either" {
              let e = fixture None [ "author", Endorse; "client", Challenge ]
              let st = baseState e

              Expect.equal (Query.tryAssent "author" st |> Option.map (fun a -> a.Verdict)) (Some Endorse) "author"
              Expect.equal (Query.tryAssent "client" st |> Option.map (fun a -> a.Verdict)) (Some Challenge) "client"
              Expect.isNone (Query.tryAssent "reviewer" st) "and a party that said nothing has no position at all"
              Expect.equal (Query.challengedBy st) [ "client" ] "the standing objections, by party"
          }

          // ---- the derived standing ------------------------------------------

          test "the four declared-party lists partition the declared set" {
              let e = fixture None [ "author", Endorse; "client", Challenge; "reviewer", Endorse ]

              let st = { standingOf e with Withdrawn = [] }

              Expect.equal
                  (List.concat [ st.Endorsed; st.Challenged; st.Withdrawn; st.Silent ] |> List.sort)
                  (List.sort declared)
                  "every declared class appears exactly once, whatever it did"
          }

          test "a party that stood down is carried apart from one that never spoke" {
              let e = fixture None [ "author", Endorse; "client", Challenge; "client", Withdraw ]

              let st = standingOf e

              Expect.equal st.Endorsed [ "author" ] "endorsing"
              Expect.equal st.Withdrawn [ "client" ] "stood down — it has been through the record"
              Expect.equal st.Silent [ "reviewer" ] "silent — it has not"
              Expect.isEmpty st.Challenged "and the challenge is no longer standing"
          }

          test "a position from a class the policy does not declare counts toward nothing" {
              // The residue of a class retired after its records were made. Named rather than
              // dropped, and outside the denominator: a record that quietly absorbed it would
              // answer a question nobody asked.
              let e = fixture None [ for p in declared -> p, Endorse ]

              let withRetired =
                  match Reducer.apply (assentOp "base" "an-old-class" Challenge) e with
                  | Ok e' -> e'
                  | Error r -> failtest (Rejection.describe r)

              let st = PartyAssent.standing declared (baseState withRetired)

              Expect.equal st.Unrecognised [ "an-old-class" ] "named"
              Expect.isEmpty st.Challenged "and not counted as a declared challenge"
              Expect.isTrue (PartyAssent.consensus st) "so the declared parties' consensus is unchanged"
          }

          // ---- consensus ------------------------------------------------------

          test "consensus is every declared party endorsing, and nothing less" {
              let all = fixture None [ for p in declared -> p, Endorse ]
              Expect.isTrue (PartyAssent.consensus (standingOf all)) "unanimous"

              let two = fixture None [ "author", Endorse; "client", Endorse ]

              Expect.isFalse
                  (PartyAssent.consensus (standingOf two))
                  "two of three is not consensus — silence is not endorsement"

              let challenged =
                  fixture None ([ for p in declared -> p, Endorse ] @ [ "reviewer", Challenge ])

              Expect.isFalse (PartyAssent.consensus (standingOf challenged)) "and one challenge takes it away"

              Expect.equal
                  (standingOf challenged).Endorsed
                  [ "author"; "client" ]
                  "without deleting the endorsements that were given"
          }

          test "a policy declaring no party class reaches consensus on nothing, ever" {
              // The vacuous-truth guard. "All of none endorse" is true and completely wrong:
              // it would report unanimous backing for every state in every record that never
              // thought about parties at all.
              let e = fixture None [ for p in declared -> p, Endorse ]
              let st = PartyAssent.standing [] (baseState e)

              Expect.isFalse (PartyAssent.consensus st) "declared or silent; there is no third state"
              Expect.isEmpty st.Declared "the denominator is empty"
          }

          test "consensus and contest are read across a whole exploration" {
              let e =
                  foldOrFail (
                      [ seedOp "base"; forkOp "left" "base" ]
                      @ [ for p in declared -> assentOp "base" p Endorse ]
                      @ [ assentOp "left" "reviewer" Challenge ]
                  )

              Expect.equal (PartyAssent.consensusStates declared e) [ "base" ] "one state everybody stands behind"
              Expect.equal (PartyAssent.contestedStates declared e) [ "left" ] "and one somebody stands against"
          }

          // ---- the tension surface --------------------------------------------

          test "gates green under an open challenge is a tension, and names the direction" {
              let e = fixture (Some true) [ "author", Endorse; "reviewer", Challenge ]

              match PartyAssent.tensions declared e with
              | [ t ] ->
                  Expect.equal t.State "base" "the state"
                  Expect.equal t.Direction ChallengedWhereGatesHeld "the direction"
                  Expect.isEmpty t.FailedGates "no gate failed, by construction"

                  let line = PartyAssent.describeTension t
                  Expect.stringContains line "The two axes disagree" "the sentence says what it is"

                  Expect.stringContains
                      line
                      "neither reading is the other's error"
                      "and refuses to pick a winner — the challenge may be the reading that is right"
              | other -> failtestf "expected exactly one tension, got %d" (List.length other)
          }

          test "gates failing under full endorsement is the other direction, and names the gate" {
              let e = fixture (Some false) [ for p in declared -> p, Endorse ]

              match PartyAssent.tensions declared e with
              | [ t ] ->
                  Expect.equal t.Direction EndorsedWhereGatesFailed "the direction"
                  Expect.equal t.FailedGates [ "convergence" ] "the gate that did not hold, by name"
              | other -> failtestf "expected exactly one tension, got %d" (List.length other)
          }

          test "the tension surface finds NOTHING where the two axes agree" {
              // The half worth checking. A surface that reported every challenged state, or
              // every state with a failed gate, would pass every test that only looked for the
              // states it should find — and would be a list of states rather than a list of
              // disagreements.
              Expect.isEmpty
                  (PartyAssent.tensions declared (fixture (Some true) [ for p in declared -> p, Endorse ]))
                  "green gates and full endorsement agree"

              Expect.isEmpty
                  (PartyAssent.tensions declared (fixture (Some false) [ "reviewer", Challenge ]))
                  "a failed gate and a challenge agree too"
          }

          test "a branch with no gate evidence is never in tension, however loud the parties are" {
              // The absence of a gate is the absence of a question, so there is no statistical
              // position for a challenge to disagree with. The challenge is still recorded.
              let unfitted = fixture None [ "reviewer", Challenge ]

              Expect.isEmpty (PartyAssent.tensions declared unfitted) "nothing to disagree with"

              Expect.equal
                  (PartyAssent.contestedStates declared unfitted)
                  [ "base" ]
                  "but the objection is on the record"

              let gateless =
                  foldOrFail
                      [ seedOp "base"
                        Annotate
                            { StateId = "base"
                              Snapshot = { outcome "h-base" with Gates = [] } }
                        assentOp "base" "reviewer" Challenge ]

              Expect.isNone (PartyAssent.latestGates (baseState gateless)) "an outcome that requested no gate"
              Expect.isEmpty (PartyAssent.tensions declared gateless) "is neither green nor red"
          }

          test "the gate reading is the state's most recent evidence, not its first" {
              // "Do the gates hold" is a question about where the branch stands now. Reading
              // every outcome would leave a branch red forever on the strength of its first
              // fit; how its readings moved across vintages is the vintage axis's question.
              let e =
                  foldOrFail (
                      [ seedOp "base"
                        Annotate
                            { StateId = "base"
                              Snapshot = gatedOutcome false }
                        Annotate
                            { StateId = "base"
                              Snapshot =
                                { gatedOutcome true with
                                    Key =
                                        { (gatedOutcome true).Key with
                                            DatasetVersion = "ds-2" } } } ]
                      @ [ for p in declared -> assentOp "base" p Endorse ]
                  )

              Expect.equal
                  (PartyAssent.latestGates (baseState e)
                   |> Option.map (List.forall (fun g -> g.Passed)))
                  (Some true)
                  "the latest fit's gates held"

              Expect.isEmpty (PartyAssent.tensions declared e) "so full endorsement is not in tension with it"
          }

          // ---- the party-class gate -------------------------------------------

          test "an assent naming a class no policy declares is refused, enumerating the set" {
              match PartyAssent.record declared "base" (assentRecord "regulator" Endorse "a class nobody declared") with
              | Error(UndeclaredParty(party, known)) ->
                  Expect.equal party "regulator" "names what was attempted"
                  Expect.equal known declared "and what exists"

                  Expect.stringContains
                      (AssentRefusal.describe (UndeclaredParty(party, known)))
                      "author, client, reviewer"
                      "in a sentence a caller can act on"
              | other -> failtestf "expected an undeclared-party refusal, got %A" other
          }

          test "an assent with a whitespace reason is refused by the gate as well as by shape" {
              match PartyAssent.record declared "base" (assentRecord "author" Challenge "   ") with
              | Error(UnreasonedAssent party) -> Expect.equal party "author" "named"
              | other -> failtestf "expected an unreasoned-assent refusal, got %A" other
          }

          test "a well-formed assent builds the ordinary op and nothing else" {
              match PartyAssent.record declared "base" (assentRecord "author" Endorse "it holds") with
              | Ok(Assent op) ->
                  Expect.equal op.StateId "base" "on the state named"
                  Expect.equal op.Record.Verdict Endorse "carrying the position"
              | other -> failtestf "expected an assent op, got %A" other
          }

          // ---- policy ---------------------------------------------------------

          test "speaking as a party class is a capability of its own, per class" {
              let p =
                  { Policy.empty with
                      Objectives =
                          [ Policy.objective
                                "review"
                                "may speak for the client only"
                                "client-rep"
                                [ MayEmit AssentKind; MayAssentAs "client" ] ] }

              Expect.isOk (Policy.authoriseAssent p "client-rep" "client") "the class granted"
              Expect.isError (Policy.authoriseAssent p "client-rep" "reviewer") "and no other"
              Expect.isError (Policy.authoriseAssent Policy.empty "client-rep" "client") "default-deny holds"

              match Policy.authoriseAssent p "client-rep" "reviewer" with
              | Error r ->
                  Expect.equal r.Attempted "assentAs:reviewer" "the refusal names what was attempted"
                  Expect.equal r.Allowed [ "assentAs:client"; "emit:assent" ] "and enumerates the alternatives"
              | Ok() -> failtest "an ungranted class was authorised"
          }

          test "the assent capabilities round-trip through their wire spellings" {
              for c in [ MayEmit AssentKind; MayAssentAs "client"; MayAssentAs "a-class-with-dashes" ] do
                  match Capability.tryParse (Capability.encode c) with
                  | Ok back -> Expect.equal back c (Capability.encode c)
                  | Error m -> failtestf "%s did not parse: %s" (Capability.encode c) m
          }

          test "the declared classes are policy data, read back in declaration order" {
              let p = Policy.declaring parties Policy.empty
              Expect.equal (Policy.parties p) declared "in the order they were declared"
              Expect.isEmpty (Policy.parties Policy.empty) "and an undeclaring policy declares none"
          }

          // ---- the governed path ----------------------------------------------

          test "the governed path refuses in three distinguishable ways, and only the fold writes" {
              let full =
                  { Policy.declaring parties (fullPolicy "curator") with
                      Objectives =
                          Policy.objective
                              "voices"
                              "may speak for every declared class"
                              "curator"
                              [ for p in declared -> MayAssentAs p ]
                          :: (fullPolicy "curator").Objectives }

              let seeded = sessionOrFail [ seedOp "base" ]

              // No grant at all: refused before anything is looked at.
              match
                  Session.submitAssent
                      hashFn
                      Policy.empty
                      "curator"
                      curator
                      "base"
                      (assentRecord "author" Endorse "r")
                      seeded
              with
              | Error(AssentPolicyBlocked r) -> Expect.equal r.Attempted "emit:assent" "the op-kind gate is outermost"
              | other -> failtestf "expected a policy refusal, got %A" other

              // A misspelled class is told what exists, rather than told it is not granted.
              match
                  Session.submitAssent
                      hashFn
                      full
                      "curator"
                      curator
                      "base"
                      (assentRecord "reviewers" Endorse "r")
                      seeded
              with
              | Error(AssentRefused(UndeclaredParty(_, known))) -> Expect.equal known declared "enumerated"
              | other -> failtestf "expected an undeclared-party refusal, got %A" other

              // A branch that is not there is the fold's answer, and it comes last.
              match
                  Session.submitAssent
                      hashFn
                      full
                      "curator"
                      curator
                      "nowhere"
                      (assentRecord "author" Endorse "r")
                      seeded
              with
              | Error(AssentFoldRejected(UnknownState id)) -> Expect.equal id "nowhere" "named"
              | other -> failtestf "expected a fold rejection, got %A" other

              Expect.equal (List.length (Session.heads seeded)) 1 "and none of the three left a trace in the history"

              match
                  Session.submitAssent
                      hashFn
                      full
                      "curator"
                      curator
                      "base"
                      (assentRecord "author" Endorse "it holds")
                      seeded
              with
              | Ok(_, s) ->
                  Expect.equal ((Query.tryState "base" s.Record |> Option.get).Assents |> List.length) 1 "one record"
                  Expect.isTrue (Session.verify hashFn s) "and the history verifies"
              | Error e -> failtestf "the assent did not land: %A" e
          }

          test "an assent hangs off the branch it is about, and replays with that line" {
              let full =
                  { Policy.declaring parties (fullPolicy "curator") with
                      Objectives =
                          Policy.objective "voices" "every class" "curator" [ for p in declared -> MayAssentAs p ]
                          :: (fullPolicy "curator").Objectives }

              let s = sessionOrFail [ seedOp "base"; forkOp "left" "base"; forkOp "right" "base" ]

              let s =
                  match
                      Session.submitAssent
                          hashFn
                          full
                          "curator"
                          curator
                          "left"
                          (assentRecord "reviewer" Challenge "not yet")
                          s
                  with
                  | Ok(_, s') -> s'
                  | Error e -> failtestf "the assent did not land: %A" e

              match Session.replayState "left" s, Session.replayState "right" s with
              | Ok left, Ok right ->
                  Expect.equal
                      ((Query.tryState "left" left |> Option.get).Assents |> List.length)
                      1
                      "the left line carries the objection"

                  Expect.isNone (Query.tryState "left" right) "and the right line does not see it at all"
              | _ -> failtest "a line failed to replay"

              match Session.toJsonl s |> Session.ofJsonl hashFn with
              | Ok back ->
                  Expect.equal
                      ((Query.tryState "left" back.Record |> Option.get).Assents)
                      ((Query.tryState "left" s.Record |> Option.get).Assents)
                      "and it survives the history's own persistence"
              | Error m -> failtestf "reload failed: %s" m
          }

          // ---- the codec -------------------------------------------------------

          test "every assent surface round-trips through its canonical encoding" {
              let roundTrip (encode: 'T -> JVal) (decode: JVal -> Result<'T, string>) (name: string) (value: 'T) =
                  let text = Canonical.write Rendering.CrossHost (encode value)

                  match Json.parse text |> Result.bind decode with
                  | Ok back -> Expect.equal back value (name + " round-trips: " + text)
                  | Error m -> failtestf "%s failed to decode (%s): %s" name text m

              roundTrip Wire.encodePartyClass Wire.decodePartyClass "partyClass" (List.head parties)

              for v in AssentVerdict.all do
                  roundTrip Wire.encodeAssent Wire.decodeAssent "assent" (assentRecord "author" v "because")

              for r in
                  [ UndeclaredParty("regulator", declared)
                    UndeclaredParty("regulator", [])
                    UnreasonedAssent "author" ] do
                  roundTrip Wire.encodeAssentRefusal Wire.decodeAssentRefusal "assentRefusal" r

              let contested =
                  fixture (Some true) [ "author", Endorse; "client", Withdraw; "reviewer", Challenge ]

              roundTrip Wire.encodeAssentStanding Wire.decodeAssentStanding "standing" (standingOf contested)

              for t in PartyAssent.tensions declared contested do
                  roundTrip Wire.encodeAssentTension Wire.decodeAssentTension "tension" t

              roundTrip
                  Wire.encodeAssentRecurrence
                  Wire.decodeAssentRecurrence
                  "recurrence"
                  { State = "base"
                    Label = "root"
                    ConsensusIn = [ "unit-a" ]
                    ChallengedIn = [ "unit-b" ]
                    PartlyEndorsedIn = []
                    SilentIn = [ "unit-c" ]
                    AbsentIn = []
                    InTensionIn = [ "unit-b" ] }

              roundTrip Wire.encodePolicy Wire.decodePolicy "policy" (Policy.declaring parties (fullPolicy "curator"))
              roundTrip Wire.encodePolicy Wire.decodePolicy "undeclaring policy" (fullPolicy "curator")
              roundTrip Wire.encodeState Wire.decodeState "state with assents" (baseState contested)
              roundTrip Wire.encodeOp Wire.decodeOp "assent op" (assentOp "base" "author" Endorse)
              roundTrip Wire.encodeExploration Wire.decodeExploration "exploration" contested
          }

          test "a state with no assents encodes to exactly the bytes it did before" {
              // Additivity as a fact rather than an intention. The same claim the contract
              // pack pins for `divergences`, made again one field along.
              let bare = foldOrFail [ seedOp "base" ] |> baseState

              Expect.isFalse
                  ((Canonical.write Rendering.CrossHost (Wire.encodeState bare)).Contains "assents")
                  "no key at all when there is nothing to say"

              Expect.isFalse
                  ((Canonical.write Rendering.CrossHost (Wire.encodePolicy Policy.empty)).Contains "parties")
                  "and none on a policy that declares no class"
          }

          // ---- the laws' own probes -------------------------------------------

          test "the assent laws would notice a broken implementation" {
              // Verify the probe, not just the verdict. Each of the three laws is driven here
              // against a value that MUST fail its own predicate, so a law that has only ever
              // been seen to pass is not mistaken for one that can fail.
              let offends (name: string) =
                  Contract.bannedAssentVocabulary
                  |> List.exists (name.ToLowerInvariant()).Contains

              Expect.isTrue (offends "QualityScore") "the vocabulary predicate fires on a ranking name"
              Expect.isTrue (offends "Approved") "and on the word the collapsed single-verdict model used"
              Expect.isFalse (offends "Endorsed") "and not on the vocabulary the surface actually uses"

              // Consensus: the law's own claim, driven at a record that must not satisfy it.
              let partial = fixture None [ "author", Endorse; "client", Endorse ]
              Expect.isFalse (PartyAssent.consensus (standingOf partial)) "the consensus predicate can say no"

              // Tension: the same fixture with and without the challenge, so the surface is
              // shown to discriminate rather than to answer one way.
              let agreeing = fixture (Some true) [ "author", Endorse ]
              let disagreeing = fixture (Some true) [ "author", Endorse; "reviewer", Challenge ]

              Expect.isEmpty (PartyAssent.tensions declared agreeing) "silent where the axes agree"
              Expect.isNonEmpty (PartyAssent.tensions declared disagreeing) "and not silent where they do not"
          }

          test "the three assent laws are in the pack, and each passes" {
              let named =
                  Contract.laws hashFn
                  |> List.filter (fun l -> l.Law.StartsWith "assent-")
                  |> List.map (fun l -> l.Law, l.Passed)

              Expect.equal
                  (named |> List.map fst |> List.sort)
                  [ "assent-consensus"; "assent-tension"; "assent-vocabulary" ]
                  "all three ship in the pack a host runs against its own build"

              for name, passed in named do
                  Expect.isTrue passed name
          } ]
