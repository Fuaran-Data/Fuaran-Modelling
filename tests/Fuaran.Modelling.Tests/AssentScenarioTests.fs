// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.AssentScenarioTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The second quality axis as one scripted run.
//
//  One unit explores two forms and fits both. The statistical axis has its
//  answer — one form's gate held, the other's did not — and then the people who
//  have to live with the result say what they think, in the same op-DAG, through
//  the same policy, on the branches they are talking about.
//
//  What the run is written to demonstrate is the DISAGREEMENT. The settled form
//  passes every gate and an independent party will not stand behind it; the
//  stretch form fails a gate and all three parties endorse it anyway. Both are
//  ordinary situations and neither is an error, and a record carrying only gate
//  verdicts would leave both of them in somebody's head.
//
//  Three parties, three subjects, three grants. Nobody speaks for anybody else:
//  the client's representative cannot file the reviewer's position, and a class
//  nobody declared cannot be spoken for at all.
//
//  Every fixture term is neutral. The forms, the parties and the gate are named
//  for what they are in a test.
// ============================================================================

let private instant = "2026-03-01T00:00:00Z"

/// The three declared classes: the party that built the state, the party that has to live
/// with it, and an independent party asked to look at it.
let private parties: PartyClass list =
    [ { Id = "author"
        Description = "the party that built the state" }
      { Id = "client"
        Description = "the party that has to live with it" }
      { Id = "reviewer"
        Description = "an independent party asked to look at it" } ]

let private declared = parties |> List.map (fun p -> p.Id)

/// One subject per party, each granted exactly one voice, plus a curator who may run the
/// exploration itself. The grants are the interesting half: `client-rep` may record assent
/// and may speak as the client, and those are two different capabilities.
let private policy: Policy =
    { Policy.declaring parties Policy.empty with
        Objectives =
            [ Policy.objective
                  "exploration"
                  "run the exploration"
                  "curator"
                  ((OpKind.all |> List.map MayEmit) @ [ MaySubmitFit; MayAssentAs "author" ])
              Policy.objective
                  "client-voice"
                  "speak for the client"
                  "client-rep"
                  [ MayEmit AssentKind; MayAssentAs "client" ]
              Policy.objective
                  "review-voice"
                  "speak for the independent party"
                  "reviewer-rep"
                  [ MayEmit AssentKind; MayAssentAs "reviewer" ] ] }

let private subjectFor (party: PartyClassId) =
    match party with
    | "author" -> "curator", curator
    | "client" -> "client-rep", Human "client-rep"
    | _ -> "reviewer-rep", Human "reviewer-rep"

/// A fit whose single gate held, or did not.
let private fitted (state: StateId) (passed: bool) : ExplorationOp =
    Annotate
        { StateId = state
          Snapshot =
            { outcome ("h-" + state) with
                Key =
                    { compositeKey ("h-" + state) with
                        DatasetVersion = "ds-1" }
                Gates =
                    [ { Name = "convergence"
                        Threshold = 0.5
                        Unit = None
                        Direction = AtLeast
                        Observed = (if passed then 0.8 else 0.2)
                        Passed = passed } ] } }

let private step (op: ExplorationOp) (s: Session) : Session =
    match Session.submitGoverned hashFn policy "curator" curator op s with
    | Ok(_, s') -> s'
    | Error e -> failwithf "op %s did not land: %A" (Op.kindName op) e

/// One party's position, recorded by that party's own subject through the governed path.
let private says
    (state: StateId)
    (party: PartyClassId)
    (verdict: AssentVerdict)
    (reason: string)
    (s: Session)
    : Session =
    let subject, actor = subjectFor party

    match
        Session.submitAssent
            hashFn
            policy
            subject
            actor
            state
            { Party = party
              Verdict = verdict
              Reason = reason
              AssentedBy = subject
              AssentedAt = instant }
            s
    with
    | Ok(_, s') -> s'
    | Error e -> failwithf "%s's %s on '%s' did not land: %A" party (AssentVerdict.encode verdict) state e

/// Seed the settled form and fork a stretch of it, and fit both: the settled form's gate
/// holds, the stretch's does not. The statistical axis has said what it has to say before
/// anybody has said anything at all.
let private explored () : Session =
    sessionOrFail []
    |> step (
        Seed
            { StateId = "settled"
              Label = "the settled form"
              Payload = payload "{\"v\":0}" "h-settled" }
    )
    |> step (
        Fork
            { StateId = "stretch"
              From = "settled"
              Label = "a wider form"
              Payload = payload "{\"v\":1}" "h-stretch" }
    )
    |> step (fitted "settled" true)
    |> step (fitted "stretch" false)

/// Two endorsements and an independent challenge on the state whose gates held.
let private contested () : Session =
    explored ()
    |> says "settled" "author" Endorse "the form is the one the work converged on"
    |> says "settled" "client" Endorse "it answers the question we asked, in terms we use"
    |> says "settled" "reviewer" Challenge "the form is not legible to anyone outside the team that built it"

let private standingOn (state: StateId) (s: Session) : AssentStanding =
    Query.tryState state s.Record |> Option.get |> PartyAssent.standing declared

[<Tests>]
let tests =
    testList
        "AssentScenario"
        [ test "the fixture is a real disagreement — the settled form's gates all held" {
              // A precondition, checked rather than assumed. A scenario in which the
              // challenged state happened to be failing its gates would run to completion and
              // demonstrate nothing, and it would look exactly like this one.
              let s = explored ()

              Expect.equal
                  (PartyAssent.latestGates (Query.tryState "settled" s.Record |> Option.get)
                   |> Option.map (List.forall (fun g -> g.Passed)))
                  (Some true)
                  "the settled form passes every gate it was asked about"

              Expect.equal
                  (PartyAssent.latestGates (Query.tryState "stretch" s.Record |> Option.get)
                   |> Option.map (List.forall (fun g -> g.Passed)))
                  (Some false)
                  "and the stretch does not — so both directions have something to find"
          }

          test "three declared parties, three voices, and each position carries its reason" {
              let s = contested ()
              let st = standingOn "settled" s

              Expect.equal st.Declared declared "the denominator was declared before anybody spoke"
              Expect.equal st.Endorsed [ "author"; "client" ] "two parties stand behind it"
              Expect.equal st.Challenged [ "reviewer" ] "and one stands against it"
              Expect.isEmpty st.Silent "everybody has spoken"

              for a in (Query.tryState "settled" s.Record |> Option.get).Assents do
                  Expect.isNotEmpty a.Reason (a.Party + " said why")
                  Expect.isNotEmpty a.AssentedBy (a.Party + " signed it")

              Expect.stringContains
                  (PartyAssent.describeStanding st)
                  "is endorsed by 2 of 3 declared parties, challenged by 1"
                  "the rendered line counts positions"

              Expect.stringContains
                  (PartyAssent.describeStanding st)
                  "not what is better"
                  "and says what the count is not — two signatures are two signatures"
          }

          test "the tension query surfaces exactly the state where the axes disagree" {
              let s = contested ()

              match PartyAssent.tensions declared s.Record with
              | [ t ] ->
                  Expect.equal t.State "settled" "the state whose gates held under an open challenge"
                  Expect.equal t.Direction ChallengedWhereGatesHeld "in that direction"
                  Expect.isEmpty t.FailedGates "with no failed gate to point at"

                  Expect.stringContains
                      (PartyAssent.describeTension t)
                      "Nothing here resolves them"
                      "and the sentence resolves nothing — this is a question, not a verdict"
              | other ->
                  failtestf "expected exactly one tension, got %A" (other |> List.map (fun t -> t.State, t.Direction))

              // The stretch form fails a gate and nobody has said anything about it, so there
              // is no consensual position to disagree with. Silence is not endorsement here
              // either.
              Expect.isEmpty
                  (PartyAssent.tensions declared s.Record
                   |> List.filter (fun t -> t.State = "stretch"))
                  "an unspoken-for branch is not in tension whatever its gates did"
          }

          test "the other direction: a failed gate that every declared party endorses anyway" {
              let s =
                  contested ()
                  |> says "stretch" "author" Endorse "the wider form is the one the next question needs"
                  |> says "stretch" "client" Endorse "we would rather carry the width than re-ask later"
                  |> says "stretch" "reviewer" Endorse "the gate is asking about a threshold nobody agreed to"

              match
                  PartyAssent.tensions declared s.Record
                  |> List.filter (fun t -> t.State = "stretch")
              with
              | [ t ] ->
                  Expect.equal t.Direction EndorsedWhereGatesFailed "gates red under full endorsement"
                  Expect.equal t.FailedGates [ "convergence" ] "naming the gate that did not hold"
              | other -> failtestf "expected exactly one tension on 'stretch', got %d" (List.length other)

              Expect.equal
                  (PartyAssent.tensions declared s.Record |> List.map (fun t -> t.State))
                  [ "settled"; "stretch" ]
                  "both directions on the books at once, in exploration order"
          }

          test "consensus is false while the challenge stands, and the challenge deletes nothing" {
              let s = contested ()

              Expect.isFalse
                  (PartyAssent.consensus (standingOn "settled" s))
                  "no consensus while a party stands against"

              Expect.isEmpty (PartyAssent.consensusStates declared s.Record) "on any state"

              Expect.equal
                  (standingOn "settled" s).Endorsed
                  [ "author"; "client" ]
                  "and the two endorsements are exactly where they were — a challenge downgrades nothing"

              Expect.equal
                  (Query.keptStates s.Record, Query.killedStates s.Record)
                  ([], [])
                  "it does not curate the branch either: an objection is not a kill"
          }

          test "the challenge is withdrawn by its own party, and the withdrawal is a recorded act" {
              let s =
                  contested ()
                  |> says "settled" "reviewer" Withdraw "the team walked me through it; my objection is answered"

              let st = standingOn "settled" s

              Expect.isEmpty st.Challenged "the objection is no longer standing"
              Expect.equal st.Withdrawn [ "reviewer" ] "the party stood down"
              Expect.isEmpty (PartyAssent.tensions declared s.Record) "so the tension is gone with it"

              // Nothing was erased. All three of the reviewer's acts are on the log, in order,
              // with their reasons — which is the only way a later reader can see that this
              // consensus took work.
              let reviewerActs =
                  (Query.tryState "settled" s.Record |> Option.get).Assents
                  |> List.filter (fun a -> a.Party = "reviewer")

              Expect.equal
                  (reviewerActs |> List.map (fun a -> a.Verdict))
                  [ Challenge; Withdraw ]
                  "the challenge it withdraws is still on the record beneath the withdrawal"

              Expect.stringContains
                  (List.head reviewerActs).Reason
                  "not legible"
                  "with the argument that was made, verbatim"

              // And consensus is STILL false. This is the phase's one honest deviation: a
              // withdrawal is standing down, not signing. "Every declared party endorses"
              // cannot be satisfied by a party that has said only that it no longer objects —
              // the same asymmetry `Query.keptStates` holds, where silence is not abandonment
              // and is not endorsement either.
              Expect.isFalse
                  (PartyAssent.consensus st)
                  "standing down is not endorsement — the reviewer has withdrawn, not signed"
          }

          test "consensus derives true once every declared party has endorsed, and only then" {
              let s =
                  contested ()
                  |> says "settled" "reviewer" Withdraw "the team walked me through it; my objection is answered"
                  |> says "settled" "reviewer" Endorse "on the second reading it is legible, and I stand behind it"

              let st = standingOn "settled" s

              Expect.equal st.Endorsed declared "all three parties"
              Expect.isTrue (PartyAssent.consensus st) "so consensus derives"
              Expect.equal (PartyAssent.consensusStates declared s.Record) [ "settled" ] "on that state alone"

              Expect.equal
                  ((Query.tryState "settled" s.Record |> Option.get).Assents |> List.length)
                  5
                  "and the whole route to it is on the record: two endorsements, a challenge, a withdrawal, an endorsement"

              // It is derived rather than stored, so a policy that declares a fourth party
              // makes the same record fall short of consensus without the record changing at
              // all. A stored flag could not do that, and would then be a second truth.
              let widened =
                  Query.tryState "settled" s.Record
                  |> Option.get
                  |> PartyAssent.standing (declared @ [ "regulator" ])

              Expect.isFalse (PartyAssent.consensus widened) "consensus is a function of the declared set, not a flag"
              Expect.equal widened.Silent [ "regulator" ] "and the party that has not spoken is named"
          }

          test "policy refuses an assent from a party class nobody declared" {
              match
                  Session.submitAssent
                      hashFn
                      policy
                      "curator"
                      curator
                      "settled"
                      { Party = "regulator"
                        Verdict = Endorse
                        Reason = "a class this process never declared"
                        AssentedBy = "curator"
                        AssentedAt = instant }
                      (explored ())
              with
              | Error(AssentRefused(UndeclaredParty(party, known))) ->
                  // The failure this gate exists for: a record standing in the name of an
                  // undeclared class folds perfectly, reads exactly like one that counts, and
                  // counts toward no consensus question at all.
                  Expect.equal party "regulator" "names what was attempted"
                  Expect.equal known declared "and enumerates the classes that exist"
              | other -> failtestf "expected an undeclared-party refusal, got %A" other
          }

          test "policy refuses a party class the subject holds no grant for" {
              // The client's representative may record assent, and may speak for the client.
              // Speaking for the independent party is a different act, and default-deny is
              // what keeps it one.
              match
                  Session.submitAssent
                      hashFn
                      policy
                      "client-rep"
                      (Human "client-rep")
                      "settled"
                      { Party = "reviewer"
                        Verdict = Endorse
                        Reason = "filing somebody else's position"
                        AssentedBy = "client-rep"
                        AssentedAt = instant }
                      (explored ())
              with
              | Error(AssentPolicyBlocked refusal) ->
                  Expect.equal refusal.Attempted "assentAs:reviewer" "the refusal names what was attempted"

                  Expect.equal
                      refusal.Allowed
                      [ "assentAs:client"; "emit:assent" ]
                      "and enumerates the voices this subject does have"

                  Expect.stringContains refusal.Reason "default-deny" "and says why"
              | other -> failtestf "expected a policy refusal, got %A" other

              Expect.isEmpty
                  (PartyAssent.contestedStates declared (explored ()).Record)
                  "nothing was recorded — an unauthorised assent leaves no trace at all"
          }

          test "the whole run survives persistence, and is deterministic byte for byte" {
              let s = contested ()

              match Session.toJsonl s |> Session.ofJsonl hashFn with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  Expect.equal
                      (standingOn "settled" back)
                      (standingOn "settled" s)
                      "the standing is the same read off a reloaded history"

                  Expect.equal
                      (PartyAssent.tensions declared back.Record |> List.map (fun t -> t.Direction))
                      (PartyAssent.tensions declared s.Record |> List.map (fun t -> t.Direction))
                      "and so is the tension surface"

              Expect.equal
                  (Session.toJsonl (contested ()))
                  (Session.toJsonl s)
                  "the same script yields the same history"
          }

          // ---- the forest roll-up ---------------------------------------------

          test "the forest reads where each decision stands with the parties, sibling by sibling" {
              // Three units running one process, exactly as the Rung-2 story does. What rolls
              // up is per-sibling standing — never a pooled position, because there is no
              // cross-sibling op and a forest question is a question.
              let sibling (id: SiblingId) : SiblingRef =
                  { Sibling = id
                    Label = "unit " + id
                    ExplorationRef = "memory:" + id }

              let forest: Forest =
                  { Id = "portfolio"
                    Title = "three units under one assent policy"
                    Siblings = [ sibling "unit-a"; sibling "unit-b"; sibling "unit-c" ]
                    Policy = policy
                    Thresholds = None }

              let unanimous =
                  contested ()
                  |> says "settled" "reviewer" Withdraw "answered"
                  |> says "settled" "reviewer" Endorse "and I stand behind it"

              let records =
                  Map.ofList
                      [ "unit-a", unanimous.Record
                        "unit-b", (contested ()).Record
                        "unit-c", (explored ()).Record ]

              let rows = PartyAssent.recurrence forest records
              let settled = rows |> List.find (fun r -> r.State = "settled")

              Expect.equal settled.ConsensusIn [ "unit-a" ] "one unit's parties all stand behind it"
              Expect.equal settled.ChallengedIn [ "unit-b" ] "one unit has an open objection"
              Expect.equal settled.SilentIn [ "unit-c" ] "and in one, nobody has said anything at all"
              Expect.isEmpty settled.PartlyEndorsedIn "the four are a partition of the members"
              Expect.equal settled.InTensionIn [ "unit-b" ] "the disagreement is a fact about that unit's record"

              Expect.equal
                  (PartyAssent.forestTensions forest records |> List.map (fun (s, t) -> s, t.State))
                  [ "unit-b", "settled" ]
                  "and the forest's tensions name the sibling they belong to"
          }

          test "a forest whose policy declares no party class reports no position at all" {
              let forest =
                  { Forest.empty "quiet" "a process that never declared a party" with
                      Siblings =
                          [ { Sibling = "unit-a"
                              Label = "unit a"
                              ExplorationRef = "memory:unit-a" } ] }

              let records = Map.ofList [ "unit-a", (contested ()).Record ]

              let settled =
                  PartyAssent.recurrence forest records
                  |> List.find (fun r -> r.State = "settled")

              Expect.isEmpty settled.ConsensusIn "no consensus, because there is no declared set to reach it over"
              Expect.isEmpty settled.ChallengedIn "no challenge, because no declared class made one"
              Expect.isEmpty settled.SilentIn "and not silent either — there is simply nothing being asked"
              Expect.isEmpty (PartyAssent.forestTensions forest records) "so nothing can be in tension"
          } ]
