// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  The contract pack.
//
//  This library is a substrate other people build dialects and hosts on, so its
//  contract has to be executable rather than described. What ships here is the
//  machinery a conforming implementation runs against itself: the codecs, a
//  fixture corpus covering every op kind, byte pins on the canonical
//  serialisation, and the laws the manager promises to hold.
//
//  A conformance suite that lives only in this repo's tests certifies this
//  repo. Shipping it in the library certifies anyone's.
// ============================================================================

/// One law check: a name and a verdict.
type LawResult =
    { Law: string
      Passed: bool
      Detail: string }

/// The executable contract of the manager substrate.
[<RequireQualifiedAccess>]
module Contract =

    // ---- codecs --------------------------------------------------------------

    let opCodec: Corpus.Codec<ExplorationOp> =
        { Encode = Wire.renderOp
          Decode = Wire.parseOp }

    let explorationCodec: Corpus.Codec<Exploration> =
        { Encode = Wire.renderExploration
          Decode = Wire.parseExploration }

    // ---- fixtures ------------------------------------------------------------

    let private payload (body: string) (hash: string) : SpecPayload =
        { Dialect = "reference"
          Format = "json"
          Body = body
          SpecHash = hash }

    let private curation (verdict: CurationVerdict) : CurationRecord =
        { Verdict = verdict
          Reason = "fixture reason"
          CuratedBy = "curator"
          CuratedAt = "2026-01-01T00:00:00Z" }

    let private snapshot: OutcomeSnapshot =
        { Key =
            { SpecHash = "h1"
              DatasetVersion = "v1"
              Seed = "7"
              ProviderId = "reference"
              ProviderVersion = "1" }
          ArtifactRef = "ref:abcd1234"
          Diagnostics = Map.ofList [ "convergence", 0.5; "holdoutError", 0.25 ]
          DiagnosticUnits = Map.empty
          Gates =
            [ { Name = "convergence"
                Threshold = 0.1
                Unit = None
                Direction = AtLeast
                Observed = 0.5
                Passed = true } ]
          SubmittedBy = SubmitAgent
          RecordedAt = "2026-01-01T00:00:00Z" }

    let private receipt: ReplayReceipt =
        { ReplayId = "r1"
          DatasetVersion = "v2"
          Requested = [ "s1"; "s2" ]
          Landed =
            [ "s1",
              { snapshot.Key with
                  DatasetVersion = "v2" } ]
          Refused = [ "s2", NoBaselineFit ]
          ReplayedBy = "curator"
          ReplayedAt = "2026-01-01T00:00:00Z" }

    let private assentRecord (party: PartyClassId) (verdict: AssentVerdict) : AssentRecord =
        { Party = party
          Verdict = verdict
          Reason = "fixture reason"
          AssentedBy = "signatory"
          AssentedAt = "2026-01-01T00:00:00Z" }

    /// Three declared party classes, named for their position in a fixture and nothing else.
    ///
    /// THREE rather than two, because two is the one size at which the interesting cases
    /// collapse: with two parties "somebody endorsed and somebody else has not spoken" cannot
    /// be distinguished from "one of the two challenged", and the partial-endorsement case
    /// that the consensus rule turns on would never arise.
    let assentParties: PartyClass list =
        [ { Id = "party-a"
            Description = "the first declared class" }
          { Id = "party-b"
            Description = "the second declared class" }
          { Id = "party-c"
            Description = "the third declared class" } ]

    let private partyIds = assentParties |> List.map (fun c -> c.Id)

    let private declaration: DivergenceDeclaration =
        { InvariantId = "fixture-invariant"
          Justification = "fixture justification"
          DeclaredBy = "curator"
          DeclaredAt = "2026-01-01T00:00:00Z" }

    let private proposal: BranchProposal =
        { ProposalId = "p1"
          From = "s1"
          ProposedId = "s9"
          Label = "proposed branch"
          Payload = payload "{\"x\":2}" "h9"
          Rationale = "fixture rationale"
          ProposedBy = "explorer" }

    /// One representative value per op kind. The corpus quantifies over this list, so a
    /// new op kind that nobody added a sample for fails the coverage gate rather than
    /// slipping through untested.
    let samples: (OpKind * ExplorationOp) list =
        [ SeedKind,
          Seed
              { StateId = "s1"
                Label = "root"
                Payload = payload "{\"x\":0}" "h1" }
          ForkKind,
          Fork
              { StateId = "s2"
                From = "s1"
                Label = "fork"
                Payload = payload "{\"x\":1}" "h2" }
          ReturnKind, ReturnTo { To = "s1" }
          CherryPickKind,
          CherryPick
              { StateId = "s3"
                Source = "s2"
                Onto = "s1"
                Label = "cherry-pick"
                Payload = payload "{\"x\":1}" "h2" }
          CurateKind,
          Curate
              { StateId = "s2"
                Record = curation Kill }
          AnnotateKind, Annotate { StateId = "s2"; Snapshot = snapshot }
          DeclareKind,
          Declare
              { StateId = "s2"
                Declaration = declaration }
          AssentKind,
          Assent
              { StateId = "s2"
                Record = assentRecord "fixture-party" Endorse }
          ReplayKind, Replay { Receipt = receipt }
          ProposeKind, Propose { Proposal = proposal }
          ResolveKind,
          Resolve
              { ProposalId = "p1"
                Outcome = Accepted
                ResolvedBy = "curator" } ]

    /// The tags the coverage gate requires — every op kind, by name.
    let requiredTags: string list = OpKind.all |> List.map OpKind.encode

    /// Inputs a conforming decoder MUST refuse. A codec is only as good as what it
    /// declines: an encoder-only corpus certifies nothing about robustness.
    let rejectFixtures: (string * string) list =
        [ "unknown-op-kind", "{\"$type\":\"teleport\",\"to\":\"s1\"}"
          "missing-tag", "{\"to\":\"s1\"}"
          "missing-field", "{\"$type\":\"return\"}"
          "wrong-field-type", "{\"$type\":\"return\",\"to\":3}"
          "unknown-verdict",
          "{\"$type\":\"curate\",\"stateId\":\"s1\",\"record\":{\"verdict\":\"maybe\",\"reason\":\"r\",\"curatedBy\":\"c\",\"curatedAt\":\"t\"}}"
          "unknown-submitter",
          "{\"$type\":\"annotate\",\"stateId\":\"s1\",\"snapshot\":{\"key\":{\"specHash\":\"h\",\"datasetVersion\":\"v\",\"seed\":\"1\",\"providerId\":\"p\",\"providerVersion\":\"1\"},\"artifactRef\":\"a\",\"diagnostics\":{},\"gates\":[],\"submittedBy\":\"robot\",\"recordedAt\":\"t\"}}"
          "missing-declaration", "{\"$type\":\"declare\",\"stateId\":\"s1\"}"
          "missing-receipt", "{\"$type\":\"replay\",\"replayId\":\"r1\"}"
          "missing-assent-record", "{\"$type\":\"assent\",\"stateId\":\"s1\"}"
          // A position this reader does not know is REFUSED rather than read as the nearest
          // one it does. Collapsing an unknown verdict into `endorse` would manufacture
          // agreement, and collapsing it into `challenge` would manufacture an objection —
          // there is no safe direction to guess in, which is why guessing is not on offer.
          "unknown-assent-verdict",
          "{\"$type\":\"assent\",\"stateId\":\"s1\",\"record\":{\"party\":\"p\",\"verdict\":\"abstain\",\"reason\":\"r\",\"assentedBy\":\"a\",\"assentedAt\":\"t\"}}"
          // A refusal class this reader does not know is REFUSED rather than read as some
          // nearby one. A replay receipt is the account of what became of each branch, and a
          // reason silently collapsed into another reason is an account that reads as
          // complete and is not.
          "unknown-replay-refusal",
          "{\"$type\":\"replay\",\"receipt\":{\"replayId\":\"r1\",\"datasetVersion\":\"v2\",\"requested\":[\"s1\"],\"landed\":[],\"refused\":[{\"state\":\"s1\",\"refusal\":{\"$type\":\"ranOutOfPatience\"}}],\"replayedBy\":\"c\",\"replayedAt\":\"t\"}}"
          "not-an-object", "\"return\"" ]

    /// The whole fixture corpus: a round-trip case per op kind, plus every reject.
    let cases: Corpus.Case list =
        [ for kind, op in samples do
              yield
                  { Name = "roundTrip/" + OpKind.encode kind
                    Kind = Corpus.RoundTrip
                    Json = Wire.renderOp op
                    Tag = OpKind.encode kind }
          for name, json in rejectFixtures do
              yield
                  { Name = "reject/" + name
                    Kind = Corpus.Reject
                    Json = json
                    Tag = "reject" } ]

    /// Run the fixture corpus against the op codec.
    let runCorpus () : Corpus.Outcome list = Corpus.runCorpus opCodec cases

    /// Does the corpus cover every op kind?
    let coverage () : Result<unit, string> = Corpus.coverageGate requiredTags cases

    /// A unit from its canonical text, for the fixtures below; a text that is not one is a
    /// defect in this file, so it fails loudly rather than becoming a `Result` nobody reads.
    let private unitOf (text: string) : UnitOfMeasure =
        match UnitText.tryParse text with
        | Ok u -> u
        | Error e -> failwith e

    /// The vintage laws' thresholds: lower leads, a margin of 0.1 stated in no unit.
    let private thresholds (minVintages: int) : CorrectionThresholds =
        { Metric = "holdoutError"
          Prefer = PreferLower
          MinVintages = minVintages
          MinMargin = 0.1
          MinSiblings = 1
          Unit = None }

    // ---- byte pins -----------------------------------------------------------

    /// Canonical bytes pinned as literals — the one check that catches a silent encoder
    /// change. A round-trip test passes happily when BOTH sides move together; only a
    /// literal notices that the bytes on the wire are no longer the bytes they were.
    let bytePins: (string * string) list =
        [ Wire.renderOp (ReturnTo { To = "s1" }), "{\"$type\":\"return\",\"to\":\"s1\"}"
          Canonical.write Rendering.CrossHost (Wire.encodeSpecPayload (payload "{}" "h")),
          "{\"body\":\"{}\",\"dialect\":\"reference\",\"format\":\"json\",\"specHash\":\"h\"}"
          Wire.renderOp (
              Declare
                  { StateId = "s1"
                    Declaration =
                      { InvariantId = "i"
                        Justification = "j"
                        DeclaredBy = "c"
                        DeclaredAt = "t" } }
          ),
          "{\"$type\":\"declare\",\"declaration\":{\"declaredAt\":\"t\",\"declaredBy\":\"c\",\"invariantId\":\"i\",\"justification\":\"j\"},\"stateId\":\"s1\"}"
          // A state with no declarations encodes to exactly the bytes it did before the field
          // existed — which is the pin that makes "additive" a fact rather than an intention.
          Canonical.write
              Rendering.CrossHost
              (Wire.encodeState
                  { Id = "s1"
                    Label = "root"
                    Parent = None
                    Origin = Seeded
                    Payload = payload "{}" "h"
                    Curations = []
                    Outcomes = []
                    Divergences = []
                    Assents = [] }),
          "{\"curations\":[],\"id\":\"s1\",\"label\":\"root\",\"origin\":{\"$type\":\"seeded\"},\"outcomes\":[],\"payload\":{\"body\":\"{}\",\"dialect\":\"reference\",\"format\":\"json\",\"specHash\":\"h\"}}"
          // The same argument, one level up and one level along. An exploration that has seen
          // no replay campaign, and a forest that declares no correction thresholds, both
          // encode to exactly the bytes they did before either existed — which is what makes
          // "additive" a fact here rather than an intention, and what keeps a forest's silent
          // default silent on the wire as well as in the code.
          Canonical.write Rendering.CrossHost (Wire.encodeExploration Exploration.empty),
          "{\"proposals\":[],\"resolutions\":[],\"states\":[]}"
          Canonical.write Rendering.CrossHost (Wire.encodeForest (Forest.empty "f1" "no thresholds declared")),
          "{\"id\":\"f1\",\"policy\":{\"objectives\":[]},\"siblings\":[],\"title\":\"no thresholds declared\"}"
          // The assent op's bytes, pinned as a literal like every other op's — an encoder
          // that quietly reordered or renamed a field would still round-trip, and only a
          // literal notices.
          Wire.renderOp (
              Assent
                  { StateId = "s1"
                    Record =
                      { Party = "p"
                        Verdict = Endorse
                        Reason = "r"
                        AssentedBy = "c"
                        AssentedAt = "t" } }
          ),
          "{\"$type\":\"assent\",\"record\":{\"assentedAt\":\"t\",\"assentedBy\":\"c\",\"party\":\"p\",\"reason\":\"r\",\"verdict\":\"endorse\"},\"stateId\":\"s1\"}"
          // A policy that declares no party class encodes to exactly the bytes it did before
          // party classes existed — which is the pin that makes the addition additive rather
          // than merely intended, and is why the forest pin above is unchanged. The declared
          // case is pinned beside it so the emitted shape is a fact rather than a guess.
          Canonical.write Rendering.CrossHost (Wire.encodePolicy Policy.empty), "{\"objectives\":[]}"
          Canonical.write
              Rendering.CrossHost
              (Wire.encodePolicy (Policy.declaring [ { Id = "p"; Description = "d" } ] Policy.empty)),
          "{\"objectives\":[],\"parties\":[{\"description\":\"d\",\"id\":\"p\"}]}"
          // The declaration's bytes, including the float layout of the margin — the one field
          // in this release whose encoding is not a string, and therefore the one a host in
          // another language is most likely to lay out differently.
          Wire.renderIndistinguishabilityDeclaration
              { Metric = "holdoutError"
                Margin = 0.05
                Scope = EveryAlternative
                Unit = None },
          "{\"margin\":0.05,\"metric\":\"holdoutError\",\"scope\":{\"$type\":\"everyAlternative\"}}"
          Wire.renderIndistinguishabilityDeclaration
              { Metric = "holdoutError"
                Margin = 0.0
                Scope = NamedAlternatives [ "a"; "b" ]
                Unit = None },
          "{\"margin\":0,\"metric\":\"holdoutError\",\"scope\":{\"$type\":\"named\",\"states\":[\"a\",\"b\"]}}"
          // The no-declaration state is a TAG on the wire and not an absence. Pinned as a
          // literal precisely because the failure it guards against is silent: an encoder that
          // emitted an empty object, or omitted the report entirely, would round-trip happily
          // against a decoder that read a missing key as "nothing was within the margin".
          Wire.renderIndistinguishabilityReport NoDeclaration, "{\"$type\":\"noDeclaration\"}"
          Wire.renderIndistinguishabilityReport (NoReferenceReading("s1", "holdoutError")),
          "{\"$type\":\"noReferenceReading\",\"metric\":\"holdoutError\",\"state\":\"s1\"}"
          // An outcome that states no units encodes to exactly the bytes it did before units
          // existed (D39): no `diagnosticUnits` key, no `unit` on a gate. The stated case is
          // pinned beside it, the units as their canonical text.
          Canonical.write Rendering.CrossHost (Wire.encodeOutcome snapshot),
          "{\"artifactRef\":\"ref:abcd1234\",\"diagnostics\":{\"convergence\":0.5,\"holdoutError\":0.25},\"gates\":[{\"direction\":\"atLeast\",\"name\":\"convergence\",\"observed\":0.5,\"passed\":true,\"threshold\":0.1}],\"key\":{\"datasetVersion\":\"v1\",\"providerId\":\"reference\",\"providerVersion\":\"1\",\"seed\":\"7\",\"specHash\":\"h1\"},\"recordedAt\":\"2026-01-01T00:00:00Z\",\"submittedBy\":\"agent\"}"
          Canonical.write
              Rendering.CrossHost
              (Wire.encodeOutcome
                  { snapshot with
                      DiagnosticUnits = Map.ofList [ "convergence", unitOf "1"; "holdoutError", unitOf "kg" ]
                      Gates = snapshot.Gates |> List.map (fun g -> { g with Unit = Some(unitOf "1") }) }),
          "{\"artifactRef\":\"ref:abcd1234\",\"diagnosticUnits\":{\"convergence\":\"1\",\"holdoutError\":\"kg\"},\"diagnostics\":{\"convergence\":0.5,\"holdoutError\":0.25},\"gates\":[{\"direction\":\"atLeast\",\"name\":\"convergence\",\"observed\":0.5,\"passed\":true,\"threshold\":0.1,\"unit\":\"1\"}],\"key\":{\"datasetVersion\":\"v1\",\"providerId\":\"reference\",\"providerVersion\":\"1\",\"seed\":\"7\",\"specHash\":\"h1\"},\"recordedAt\":\"2026-01-01T00:00:00Z\",\"submittedBy\":\"agent\"}"
          Canonical.write
              Rendering.CrossHost
              (Wire.encodeBackendRefusal (
                  GateUnitMismatch
                      { Gate = "holdoutError"
                        Stated = Some(unitOf "g")
                        Reported = Some(unitOf "kg") }
              )),
          "{\"$type\":\"gateUnitMismatch\",\"gate\":\"holdoutError\",\"reported\":\"kg\",\"stated\":\"g\"}"
          // A margin states its unit beside it (D40), and one that states none encodes to the
          // bytes it did before margins could — the declaration pins above, and the thresholds
          // pin here. The stated cases are pinned beside them, as canonical unit text.
          Canonical.write Rendering.CrossHost (Wire.encodeThresholds (thresholds 2)),
          "{\"metric\":\"holdoutError\",\"minMargin\":0.1,\"minSiblings\":1,\"minVintages\":2,\"prefer\":\"lower\"}"
          Canonical.write
              Rendering.CrossHost
              (Wire.encodeThresholds
                  { thresholds 2 with
                      Unit = Some(unitOf "kg") }),
          "{\"metric\":\"holdoutError\",\"minMargin\":0.1,\"minSiblings\":1,\"minVintages\":2,\"prefer\":\"lower\",\"unit\":\"kg\"}"
          Wire.renderIndistinguishabilityDeclaration
              { Metric = "holdoutError"
                Margin = 0.05
                Scope = EveryAlternative
                Unit = Some(unitOf "kg") },
          "{\"margin\":0.05,\"metric\":\"holdoutError\",\"scope\":{\"$type\":\"everyAlternative\"},\"unit\":\"kg\"}"
          // A series in more than one unit is a TAG, naming what was found — never an empty set.
          Wire.renderIndistinguishabilityReport (
              MismatchedUnits
                  { Metric = "holdoutError"
                    Declared = Some(unitOf "kg")
                    Stated = [ unitOf "g"; unitOf "kg" ]
                    Unstated = true }
          ),
          "{\"$type\":\"mismatchedUnits\",\"declared\":\"kg\",\"metric\":\"holdoutError\",\"stated\":[\"g\",\"kg\"],\"unstated\":true}" ]

    // ---- laws ----------------------------------------------------------------

    let private law name passed detail =
        { Law = name
          Passed = passed
          Detail = detail }

    /// Default-deny: the empty policy authorises nothing, for anyone. Checked over the
    /// whole capability space rather than a sample of it, because the failure mode this
    /// guards against is exactly the capability nobody thought to test.
    let defaultDenyLaw () : LawResult =
        // The party-class axis is an OPEN vocabulary, so the space cannot be enumerated the
        // way the op kinds can. What it can be is sampled where a leak would actually happen:
        // a class the fixture policy declares, and one nobody ever declared. Both must be
        // refused by an empty policy, and the second is the one that would leak if a grant
        // check ever fell back to "no such class, no objection".
        let caps =
            (OpKind.all |> List.map MayEmit)
            @ [ MaySubmitFit ]
            @ (partyIds |> List.map MayAssentAs)
            @ [ MayAssentAs "a-class-nobody-declared" ]

        let leaks =
            caps
            |> List.filter (fun c ->
                match Policy.authorise Policy.empty "anyone" c with
                | Ok() -> true
                | Error _ -> false)

        law
            "default-deny"
            (List.isEmpty leaks)
            (if List.isEmpty leaks then
                 sprintf "%d capabilities, all refused by the empty policy" (List.length caps)
             else
                 "leaked: " + (leaks |> List.map Capability.encode |> String.concat ", "))

    /// A refusal enumerates alternatives: whatever a subject IS granted appears in the
    /// refusal it gets for something else. A refusal a caller cannot act on is a bug in
    /// the refusal, not in the caller.
    let refusalEnumeratesLaw () : LawResult =
        let policy =
            { Policy.empty with
                Objectives = [ Policy.objective "o1" "propose only" "explorer" Policy.proposeOnlyCapabilities ] }

        match Policy.authorise policy "explorer" MaySubmitFit with
        | Ok() -> law "refusal-enumerates" false "a propose-only subject was granted fit submission"
        | Error refusal ->
            let expected = Policy.allowed "explorer" policy

            law
                "refusal-enumerates"
                (refusal.Allowed = expected && not (List.isEmpty refusal.Allowed))
                ("allowed: " + String.concat ", " refusal.Allowed)

    /// Every op kind round-trips through the codec, and the corpus covers every kind.
    let codecLaw () : LawResult =
        let failures = runCorpus () |> List.filter (fun o -> not o.Passed)

        match coverage () with
        | Error m -> law "codec" false ("coverage gate: " + m)
        | Ok() ->
            law
                "codec"
                (List.isEmpty failures)
                (if List.isEmpty failures then
                     sprintf "%d fixtures passed" (List.length cases)
                 else
                     failures |> List.map (fun f -> f.Name + ": " + f.Detail) |> String.concat "; ")

    /// The canonical bytes are the bytes they were.
    let bytePinLaw () : LawResult =
        let drift = bytePins |> List.filter (fun (actual, pinned) -> actual <> pinned)

        law
            "byte-pins"
            (List.isEmpty drift)
            (drift
             |> List.map (fun (actual, pinned) -> "expected " + pinned + ", got " + actual)
             |> String.concat "; ")

    /// Path replay is deterministic and line-local: replaying to a state's head yields
    /// exactly that state's lineage — not its siblings — and a history that has been
    /// serialised and read back replays every line identically.
    ///
    /// This is the promise the whole design rests on. An exploration is only cheap to
    /// abandon if an abandoned branch costs the branch beside it nothing, and that is a
    /// claim about replay, so it is checked as one.
    let pathReplayLaw (hashFn: HashFn) : LawResult =
        let p n h = payload ("{\"x\":" + string n + "}") h
        let actor = Human "contract"

        let ops =
            [ Seed
                  { StateId = "a"
                    Label = "root"
                    Payload = p 0 "h0" }
              Fork
                  { StateId = "b"
                    From = "a"
                    Label = "left"
                    Payload = p 1 "h1" }
              Fork
                  { StateId = "c"
                    From = "a"
                    Label = "right"
                    Payload = p 2 "h2" }
              Curate
                  { StateId = "c"
                    Record = curation Kill } ]

        match Session.submitAll hashFn actor ops Session.empty with
        | Error(i, r) -> law "path-replay" false (sprintf "op %d rejected: %s" i (Rejection.describe r))
        | Ok session ->
            let reloaded = Session.toJsonl session |> Session.ofJsonl hashFn

            match reloaded with
            | Error m -> law "path-replay" false ("reload failed: " + m)
            | Ok back ->
                let liveB = Session.replayState "b" session
                let backB = Session.replayState "b" back
                let liveC = Session.replayState "c" session

                match liveB, backB, liveC with
                | Ok lb, Ok bb, Ok lc ->
                    let lineLocal = (lb.Order = [ "a"; "b" ]) && (lc.Order = [ "a"; "c" ])
                    let stable = lb = bb

                    law
                        "path-replay"
                        (lineLocal && stable)
                        (sprintf "b-line %A, c-line %A, stable-across-reload %b" lb.Order lc.Order stable)
                | _ -> law "path-replay" false "a line failed to replay"

    // ---- the forest laws -----------------------------------------------------

    /// A two-sibling forest with a comparator that always finds one thing, used by the law
    /// below. Deliberately trivial: the law is about what the manager does with a finding, and
    /// a comparator that answered conditionally would test the comparator instead.
    let private divergenceFixture () =
        let sibling (id: SiblingId) : SiblingRef =
            { Sibling = id
              Label = id
              ExplorationRef = "memory:" + id }

        let forest =
            { Id = "f1"
              Title = "contract fixture"
              Siblings = [ sibling "a"; sibling "b" ]
              Policy = Policy.empty
              Thresholds = None }

        let record (n: int) =
            match
                Reducer.applyAll
                    [ Seed
                          { StateId = "s1"
                            Label = "root"
                            Payload = payload ("{\"x\":" + string n + "}") ("h" + string n) }
                      Curate
                          { StateId = "s1"
                            Record = curation Keep } ]
                    Exploration.empty
            with
            | Ok e -> e
            | Error(i, r) -> failwithf "fixture op %d rejected: %s" i (Rejection.describe r)

        let comparator: InvariantComparator =
            { Id = "contract-invariant"
              Description = "always finds one disagreement, so the law is about the response to it"
              Compare =
                fun projected ->
                    [ { InvariantId = "ignored — the manager stamps it"
                        Siblings = projected |> List.map (fun p -> p.Sibling)
                        Detail = "the siblings differ" } ] }

        forest, record, comparator

    /// A declaration downgrades a finding and never deletes it, and carries the justification
    /// with it.
    ///
    /// This is the whole of "permitted but priced", and it is checked rather than described
    /// because the failure mode is silent and attractive: filtering a justified finding out of
    /// the report makes the report shorter and the divergence free. The law fails if the
    /// finding disappears, if it is not downgraded, or if the argument for it is lost.
    let divergenceDowngradeLaw () : LawResult =
        let forest, record, comparator = divergenceFixture ()
        let justification = "the unit's own history warrants it"

        let declared =
            match
                Reducer.apply
                    (Declare
                        { StateId = "s1"
                          Declaration =
                            { InvariantId = comparator.Id
                              Justification = justification
                              DeclaredBy = "curator"
                              DeclaredAt = "2026-01-01T00:00:00Z" } })
                    (record 2)
            with
            | Ok e -> e
            | Error r -> failwith (Rejection.describe r)

        let before =
            Forest.assess KeptStates [ comparator ] forest (Map.ofList [ "a", record 1; "b", record 2 ])

        let after =
            Forest.assess KeptStates [ comparator ] forest (Map.ofList [ "a", record 1; "b", declared ])

        let survives = List.length after.Findings = List.length before.Findings

        let downgraded = after.Findings |> List.forall (fun x -> x.Standing = Downgraded)

        let stated =
            after.Findings
            |> List.collect (fun x -> x.Declarations)
            |> List.exists (fun (_, _, d) -> d.Justification = justification)

        let priced =
            List.isEmpty (Forest.unaccounted after) && not (List.isEmpty after.Findings)

        let stamped =
            after.Findings |> List.forall (fun x -> x.Finding.InvariantId = comparator.Id)

        law
            "divergence-downgrade"
            (survives && downgraded && stated && priced && stamped)
            (sprintf
                "survives %b, downgraded %b, justification carried %b, still reported %b, id stamped %b"
                survives
                downgraded
                stated
                priced
                stamped)

    /// Words a recurrence surface may not use.
    ///
    /// A branch DAG is a human-guided walk, so a count over kept branches is a record of what
    /// was chosen and cannot become a claim about what is better. That is a discipline, and a
    /// discipline stated only in a comment is one a later reader edits away in good faith while
    /// making a field name "clearer".
    let bannedRecurrenceVocabulary: string list =
        [ "better"
          "best"
          "superior"
          "optimal"
          "outperform"
          "win"
          "score"
          "rank"
          "significan"
          "prove" ]

    /// The recurrence surface names acts, never merits.
    ///
    /// Checked over the record's own field names by reflection, so a field added later is
    /// covered without anybody remembering to extend a list. The rendered description is
    /// checked separately and by a different rule — it is REQUIRED to say what the number is
    /// not, so the banned words are exactly what it must be free to contain.
    let recurrenceVocabularyLaw () : LawResult =
        let fields =
            FSharp.Reflection.FSharpType.GetRecordFields typeof<ChosenRecurrence>
            |> Array.map (fun p -> p.Name)
            |> List.ofArray

        let offending =
            fields
            |> List.filter (fun name ->
                let lowered = name.ToLowerInvariant()
                bannedRecurrenceVocabulary |> List.exists lowered.Contains)

        let sample: ChosenRecurrence =
            { State = "s1"
              Label = "root"
              ChosenIn = [ "a"; "b" ]
              SetAsideIn = [ "c" ]
              UncuratedIn = []
              AbsentIn = []
              DivergentIn = [] }

        let sentence = Forest.describeRecurrence 3 sample
        let says = sentence.Contains "chosen" && sentence.Contains "not what is better"

        law
            "recurrence-vocabulary"
            (List.isEmpty offending && says)
            (if List.isEmpty offending then
                 sprintf "%d fields name acts; the description states what it is not" (List.length fields)
             else
                 "merit vocabulary in: " + String.concat ", " offending)

    // ---- the vintage laws ----------------------------------------------------

    /// A two-branch exploration with readings on three vintages: the kept branch reads 0.5
    /// everywhere and the branch that was set aside reads 0.3, under a metric where lower is
    /// the leading direction. So the set-aside branch reads ahead by 0.2 on every vintage —
    /// deliberately flat, because the laws below are about the GATE and a fixture whose
    /// numbers wobbled would test the wobble.
    ///
    /// The first vintage is the baseline: the evidence the curation was taken on. Two
    /// out-of-time vintages remain, which is exactly the population the thresholds are
    /// declared against.
    let private vintageFixtureIn (reads: StateId -> string -> float * UnitOfMeasure option) =
        let vintages = [ "v1"; "v2"; "v3" ]

        let reading (state: StateId) (v: string) =
            let x, unit = reads state v

            Annotate
                { StateId = state
                  Snapshot =
                    { Key =
                        { SpecHash = "h-" + state
                          DatasetVersion = v
                          Seed = "7"
                          ProviderId = "reference"
                          ProviderVersion = "1" }
                      ArtifactRef = "ref:" + state + "-" + v
                      Diagnostics = Map.ofList [ "holdoutError", x ]
                      DiagnosticUnits =
                        unit
                        |> Option.map (fun u -> Map.ofList [ "holdoutError", u ])
                        |> Option.defaultValue Map.empty
                      Gates = []
                      SubmittedBy = SubmitScheduled
                      RecordedAt = "2026-01-01T00:00:00Z" } }

        let ops =
            [ Seed
                  { StateId = "kept"
                    Label = "the settled form"
                    Payload = payload "{\"x\":0}" "h-kept" }
              Fork
                  { StateId = "setAside"
                    From = "kept"
                    Label = "the alternative"
                    Payload = payload "{\"x\":1}" "h-setAside" } ]
            @ [ for v in vintages -> reading "kept" v ]
            @ [ for v in vintages -> reading "setAside" v ]
            @ [ Curate
                    { StateId = "kept"
                      Record = curation Keep }
                Curate
                    { StateId = "setAside"
                      Record = curation Kill } ]

        match Reducer.applyAll ops Exploration.empty with
        | Ok e -> e
        | Error(i, r) -> failwithf "vintage fixture op %d rejected: %s" i (Rejection.describe r)

    /// The vintage fixture as the laws above read it: no reading states a unit.
    let private vintageFixture () =
        vintageFixtureIn (fun state _ -> (if state = "kept" then 0.5 else 0.3), None)

    /// A replayed fit is the same fit asked of different data: its composite key differs
    /// from the baseline's in `datasetVersion` and in nothing else.
    ///
    /// This is the whole basis of the vintage axis and it is checked rather than described,
    /// because every plausible way of getting it wrong is invisible. Re-deriving the seed per
    /// vintage, taking the provider from the executor that happens to be composed today,
    /// re-minting the hash from the payload — each produces a record that reads exactly like
    /// a replay and is a different fit, so the series it joins is measuring something nobody
    /// asked about.
    let replayIdentityLaw () : LawResult =
        let e = vintageFixture ()
        let plan = ReplayCampaign.plan "r1" EveryFittedBranch "v4" [] SubmitScheduled e

        let checks =
            plan.Items
            |> List.map (fun item ->
                let key = ReplayCampaign.expectedKey plan.DatasetVersion item

                let onlyVintage =
                    key.SpecHash = item.Baseline.SpecHash
                    && key.Seed = item.Baseline.Seed
                    && key.ProviderId = item.Baseline.ProviderId
                    && key.ProviderVersion = item.Baseline.ProviderVersion
                    && key.DatasetVersion = "v4"
                    && item.Baseline.DatasetVersion <> "v4"

                // The submission carries the branch's own payload and the baseline's seed —
                // the two inputs the rule holds constant — so a plan built from the record
                // cannot produce a fit that is not a replay of the fit it names.
                let carried =
                    item.Submission.Seed = item.Baseline.Seed
                    && item.Submission.DatasetVersion = "v4"
                    && ReplayCampaign.isReplayOf item.Baseline key

                onlyVintage && carried)

        law
            "replay-identity"
            (not (List.isEmpty plan.Items)
             && List.forall id checks
             && List.isEmpty plan.Refused)
            (sprintf "%d fitted branches planned, every key held but the vintage" (List.length plan.Items))

    /// Below the declared threshold the record accumulates and says nothing; at it, exactly
    /// one question is raised.
    ///
    /// Silence is the half that has to be checked. A gate that fires correctly and also fires
    /// early is not a gate — and "early" here is the failure the whole design is a rejection
    /// of, because a decision that looks wrong after one refresh may be a regime blip, and an
    /// agent re-curating on it is chasing noise. So the law drives the same evidence past a
    /// gate it does not clear and past one it does, and requires both answers.
    let correctionThresholdLaw () : LawResult =
        let e = vintageFixture ()
        let below = Vintage.corrections (thresholds 3) e
        let at = Vintage.corrections (thresholds 2) e

        let silent = List.isEmpty below

        let exactlyOne =
            match at with
            | [ finding ] ->
                finding.Decision = { Kept = "kept"; SetAside = "setAside" }
                && finding.LeadingVintages = [ "v2"; "v3" ]
                && finding.RaisedIn = []
            | _ -> false

        // The baseline is excluded by construction, not by arithmetic: three vintages carry a
        // lead and only two of them are out-of-time.
        let record =
            Vintage.record (thresholds 2) { Kept = "kept"; SetAside = "setAside" } e

        let baselineExcluded =
            record.Baseline = Some "v1"
            && (record.Comparisons |> List.filter (fun c -> c.OutOfTime) |> List.length) = 2
            && List.length record.Comparisons = 3

        // A forest declaring no thresholds raises nothing whatever the evidence says.
        let forest =
            { Forest.empty "f" "no thresholds declared" with
                Siblings =
                    [ { Sibling = "a"
                        Label = "a"
                        ExplorationRef = "memory:a" } ] }

        let undeclaredSilent =
            List.isEmpty (Vintage.forestCorrections forest (Map.ofList [ "a", e ]))

        law
            "correction-threshold"
            (silent && exactlyOne && baselineExcluded && undeclaredSilent)
            (sprintf
                "below %d, at %d, baseline excluded %b, undeclared silent %b"
                (List.length below)
                (List.length at)
                baselineExcluded
                undeclaredSilent)

    /// Words a correction surface may not use.
    ///
    /// Different from the recurrence vocabulary and for a different reason. Recurrence must
    /// not claim a MERIT it cannot support; an out-of-time record can speak of one reading
    /// ahead of another. What this surface must never do is read as an ACT — as something
    /// done, or to be done, to a curation. The human gate is the point of the whole
    /// mechanism, and a field called `Correction` or `ShouldRecurate` would move it into the
    /// machinery by naming alone.
    let bannedCorrectionVocabulary: string list =
        [ "correct"
          "recurate"
          "revert"
          "apply"
          "auto"
          "fix"
          "wrong"
          "should"
          "verdict"
          "resolve"
          "action" ]

    /// The correction surface names evidence and asks a question; it never names an act.
    ///
    /// Reflective over the record's own fields, so a field added later is covered without
    /// anybody remembering to extend a list — the same construction, and the same failure
    /// mode, as the recurrence law: a rename made in good faith that nothing else notices.
    let correctionReviewVocabularyLaw () : LawResult =
        let fields =
            FSharp.Reflection.FSharpType.GetRecordFields typeof<CorrectionFinding>
            |> Array.map (fun p -> p.Name)
            |> List.ofArray

        let offending =
            fields
            |> List.filter (fun name ->
                let lowered = name.ToLowerInvariant()
                bannedCorrectionVocabulary |> List.exists lowered.Contains)

        let sample: CorrectionFinding =
            { Decision = { Kept = "kept"; SetAside = "setAside" }
              Metric = "holdoutError"
              LeadingVintages = [ "v2"; "v3" ]
              SmallestLead = 0.2
              RaisedIn = []
              Thresholds = thresholds 2 }

        let sentence = Vintage.describeCorrection sample

        // The rendered line is REQUIRED to say what it is and what it is not, which is why
        // the banned words above are checked over the field names and not over this.
        let says =
            sentence.Contains "raised for review"
            && sentence.Contains "No curation has been changed"

        law
            "correction-review-vocabulary"
            (List.isEmpty offending && says)
            (if List.isEmpty offending then
                 sprintf "%d fields name evidence; the description raises a question" (List.length fields)
             else
                 "act vocabulary in: " + String.concat ", " offending)

    // ---- the assent laws -----------------------------------------------------

    /// Two branches with gate evidence pointing opposite ways, so both directions of the
    /// tension surface have something to find.
    ///
    /// `green` passed its gate, `red` did not, and `quiet` passed its gate as well — the
    /// third one is there because a law that only ever sees states IN tension cannot tell a
    /// tension surface from a list of states. `unfitted` carries no evidence at all.
    let private assentFixture (assents: (StateId * PartyClassId * AssentVerdict) list) : Exploration =
        let gated (state: StateId) (passed: bool) =
            Annotate
                { StateId = state
                  Snapshot =
                    { snapshot with
                        Key =
                            { snapshot.Key with
                                SpecHash = "h-" + state }
                        Gates =
                            [ { Name = "convergence"
                                Threshold = 0.1
                                Unit = None
                                Direction = AtLeast
                                Observed = (if passed then 0.5 else 0.05)
                                Passed = passed } ] } }

        let ops =
            [ Seed
                  { StateId = "green"
                    Label = "gates held"
                    Payload = payload "{\"x\":0}" "h-green" }
              Fork
                  { StateId = "red"
                    From = "green"
                    Label = "a gate did not hold"
                    Payload = payload "{\"x\":1}" "h-red" }
              Fork
                  { StateId = "quiet"
                    From = "green"
                    Label = "gates held and nobody disagrees"
                    Payload = payload "{\"x\":2}" "h-quiet" }
              Fork
                  { StateId = "unfitted"
                    From = "green"
                    Label = "no evidence at all"
                    Payload = payload "{\"x\":3}" "h-unfitted" }
              gated "green" true
              gated "red" false
              gated "quiet" true ]
            @ [ for state, party, verdict in assents ->
                    Assent
                        { StateId = state
                          Record = assentRecord party verdict } ]

        match Reducer.applyAll ops Exploration.empty with
        | Ok e -> e
        | Error(i, r) -> failwithf "assent fixture op %d rejected: %s" i (Rejection.describe r)

    let private standingOf (state: StateId) (e: Exploration) : AssentStanding =
        Query.tryState state e
        |> Option.map (PartyAssent.standing partyIds)
        |> Option.defaultWith (fun () -> failwithf "the assent fixture has no state '%s'" state)

    /// Consensus is DERIVED, every declared party counts, and silence is not endorsement.
    ///
    /// Four claims, and the last two are the ones a plausible implementation gets wrong.
    /// A policy declaring NO party class must reach consensus on nothing — "all of none
    /// endorse" is vacuously true and would report unanimous backing for every state in every
    /// record that never thought about parties. And a withdrawal must not be read as
    /// agreement: a party that stood down has stopped objecting, which is a different fact
    /// from having signed, and the gap between those two is where a consensus nobody gave
    /// would appear.
    let assentConsensusLaw () : LawResult =
        let all (state: StateId) (verdict: AssentVerdict) =
            [ for p in partyIds -> state, p, verdict ]

        let unanimous = assentFixture (all "green" Endorse)

        let challenged =
            assentFixture (all "green" Endorse @ [ "green", "party-c", Challenge ])

        let withdrawn =
            assentFixture (
                all "green" Endorse
                @ [ "green", "party-c", Challenge; "green", "party-c", Withdraw ]
            )

        let reached = PartyAssent.consensus (standingOf "green" unanimous)

        // Undeclared: the same unanimous record, read against a policy that declares nobody.
        let vacuous =
            Query.tryState "green" unanimous
            |> Option.get
            |> PartyAssent.standing []
            |> PartyAssent.consensus

        let challengedStanding = standingOf "green" challenged

        let takenAway =
            not (PartyAssent.consensus challengedStanding)
            && challengedStanding.Challenged = [ "party-c" ]
            // And the endorsements are still there. A challenge downgrades nothing.
            && challengedStanding.Endorsed = [ "party-a"; "party-b" ]

        let withdrawnStanding = standingOf "green" withdrawn

        let standDown =
            not (PartyAssent.contested withdrawnStanding)
            && withdrawnStanding.Withdrawn = [ "party-c" ]
            && not (PartyAssent.consensus withdrawnStanding)
            // The withdrawal is an ACT, not an erasure: the endorsement and the challenge it
            // supersedes are both still on the log beneath it.
            && (Query.tryState "green" withdrawn |> Option.get).Assents
               |> List.filter (fun a -> a.Party = "party-c")
               |> List.map (fun a -> a.Verdict) = [ Endorse; Challenge; Withdraw ]

        law
            "assent-consensus"
            (reached && not vacuous && takenAway && standDown)
            (sprintf
                "unanimous %b, vacuous-when-undeclared %b, challenge takes it away without deleting %b, withdrawal is recorded and is not endorsement %b"
                reached
                vacuous
                takenAway
                standDown)

    /// The tension surface finds both directions, and finds nothing where the axes agree.
    ///
    /// The silent half is the half worth checking. A surface that reported every state with a
    /// challenge, or every state with a failed gate, would pass any test that only looked for
    /// the states it should find — and would then be a list of states rather than a list of
    /// disagreements, which is the one thing it exists to be.
    let assentTensionLaw () : LawResult =
        let e =
            assentFixture (
                [ "green", "party-c", Challenge ]
                @ [ for p in partyIds -> "red", p, Endorse ]
                @ [ for p in partyIds -> "quiet", p, Endorse ]
                @ [ "unfitted", "party-c", Challenge ]
            )

        let found = PartyAssent.tensions partyIds e

        let bothDirections =
            found |> List.map (fun t -> t.State, t.Direction) |> List.sort = [ "green", ChallengedWhereGatesHeld
                                                                               "red", EndorsedWhereGatesFailed ]

        // `quiet` agrees on both axes and `unfitted` has no statistical axis to disagree
        // with — a challenge on a branch nobody fitted is a recorded challenge, not a tension.
        let quietAndUnfittedExcluded =
            not (found |> List.exists (fun t -> t.State = "quiet" || t.State = "unfitted"))

        let named =
            found
            |> List.exists (fun t -> t.Direction = EndorsedWhereGatesFailed && t.FailedGates = [ "convergence" ])

        let says =
            found
            |> List.forall (fun t ->
                let line = PartyAssent.describeTension t

                line.Contains "The two axes disagree"
                && line.Contains "Nothing here resolves them")

        law
            "assent-tension"
            (bothDirections && quietAndUnfittedExcluded && named && says)
            (sprintf
                "%d tensions, both directions %b, agreement and unfitted excluded %b, failed gate named %b, sentence resolves nothing %b"
                (List.length found)
                bothDirections
                quietAndUnfittedExcluded
                named
                says)

    /// Words an assent surface may not use.
    ///
    /// A third ban, for a third reason. Recurrence must not claim a MERIT it cannot support;
    /// a correction surface must not read as an ACT; and assent must not read as a RANKING.
    /// The failure this one guards is specific and attractive: a count of endorsements looks
    /// exactly like a quality score, and one field renamed in good faith — `Approved`,
    /// `Grade`, `Score` — turns "three people signed this" into "this is the better form",
    /// which is a claim no number of signatures can license.
    let bannedAssentVocabulary: string list =
        [ "better"
          "best"
          "superior"
          "optimal"
          "outperform"
          "win"
          "score"
          "rank"
          "grade"
          "quality"
          "correct"
          "significan"
          "prove" ]

    /// The assent surface names who stands behind a state, never how good it is.
    ///
    /// Reflective over the surfaces' own field names, so a field added later is covered
    /// without anybody remembering to extend a list — the same construction as the recurrence
    /// and correction laws, and against the same failure: a rename made in good faith that
    /// nothing else in the build notices.
    let assentVocabularyLaw () : LawResult =
        let fieldsOf (t: System.Type) =
            FSharp.Reflection.FSharpType.GetRecordFields t
            |> Array.map (fun p -> t.Name + "." + p.Name)
            |> List.ofArray

        let fields =
            fieldsOf typeof<AssentRecord>
            @ fieldsOf typeof<AssentStanding>
            @ fieldsOf typeof<AssentTension>
            @ fieldsOf typeof<AssentRecurrence>

        let offending =
            fields
            |> List.filter (fun name ->
                let lowered = (name.Split '.' |> Array.last).ToLowerInvariant()
                bannedAssentVocabulary |> List.exists lowered.Contains)

        let sample =
            standingOf "green" (assentFixture [ "green", "party-a", Endorse; "green", "party-b", Challenge ])

        let sentence = PartyAssent.describeStanding sample

        // The rendered line is REQUIRED to say what the count is not, which is exactly why
        // the banned words are checked over the field names and not over this.
        let says =
            sentence.Contains "is endorsed by 1 of 3 declared parties"
            && sentence.Contains "not what is better"

        law
            "assent-vocabulary"
            (List.isEmpty offending && says)
            (if List.isEmpty offending then
                 sprintf "%d fields name a position; the description states what it is not" (List.length fields)
             else
                 "ranking vocabulary in: " + String.concat ", " offending)

    // ---- the indistinguishability laws ---------------------------------------

    /// Four fitted branches on one vintage, three of them alternatives of the reference and
    /// one deliberately not.
    ///
    /// ```
    ///   root                          (an alternative — never fitted)
    ///   ├── reference   0.40
    ///   ├── near        0.42          within a margin of 0.05
    ///   ├── far         0.55          outside it
    ///   └── far
    ///        └── unrelated  0.41      the CLOSEST reading in the record, and out of scope
    /// ```
    ///
    /// `unrelated` is the fixture's whole point. It reads nearer the reference than `near`
    /// does, and it is not a member, because nobody ever chose between it and the reference —
    /// it descends from a branch that was itself an alternative, which makes it a continuation
    /// of a different line rather than a competitor on this one. A set built on closeness
    /// alone would contain it, and would then be a list of branches that read alike rather
    /// than a family of specifications somebody weighed.
    ///
    /// `root` carries no fit, so it is the unread case: an alternative the record cannot place
    /// on this vintage, which must be named rather than dropped.
    let private indistinguishabilityFixtureIn (unitOn: StateId -> UnitOfMeasure option) =
        let reading (state: StateId) (v: string) (x: float) =
            Annotate
                { StateId = state
                  Snapshot =
                    { Key =
                        { SpecHash = "h-" + state
                          DatasetVersion = v
                          Seed = "7"
                          ProviderId = "reference"
                          ProviderVersion = "1" }
                      ArtifactRef = "ref:" + state + "-" + v
                      Diagnostics = Map.ofList [ "holdoutError", x ]
                      DiagnosticUnits =
                        unitOn state
                        |> Option.map (fun u -> Map.ofList [ "holdoutError", u ])
                        |> Option.defaultValue Map.empty
                      Gates = []
                      SubmittedBy = SubmitScheduled
                      RecordedAt = "2026-01-01T00:00:00Z" } }

        let ops =
            [ Seed
                  { StateId = "root"
                    Label = "the starting form"
                    Payload = payload "{\"x\":0}" "h-root" }
              Fork
                  { StateId = "reference"
                    From = "root"
                    Label = "the form the question is about"
                    Payload = payload "{\"x\":1}" "h-reference" }
              Fork
                  { StateId = "near"
                    From = "root"
                    Label = "an alternative that reads alike"
                    Payload = payload "{\"x\":2}" "h-near" }
              Fork
                  { StateId = "far"
                    From = "root"
                    Label = "an alternative that reads apart"
                    Payload = payload "{\"x\":3}" "h-far" }
              Fork
                  { StateId = "unrelated"
                    From = "far"
                    Label = "a continuation of another line"
                    Payload = payload "{\"x\":4}" "h-unrelated" }
              reading "reference" "v1" 0.40
              reading "near" "v1" 0.42
              reading "far" "v1" 0.55
              reading "unrelated" "v1" 0.41 ]

        match Reducer.applyAll ops Exploration.empty with
        | Ok e -> e
        | Error(i, r) -> failwithf "indistinguishability fixture op %d rejected: %s" i (Rejection.describe r)

    /// The indistinguishability fixture as the set law reads it: no reading states a unit.
    let private indistinguishabilityFixture () =
        indistinguishabilityFixtureIn (fun _ -> None)

    /// An undeclared margin yields no set; a declared one yields exactly the branches within
    /// it; and only genuine alternatives are eligible.
    ///
    /// Three claims, and the first is the one a plausible implementation gets wrong in the
    /// most expensive way. A surface that answered an undeclared question with an empty list
    /// would report "nothing is close to this specification" — a strong, decisive, entirely
    /// fabricated finding — to every caller who never declared a margin. The other two are the
    /// discipline: membership is a symmetric distance, and eligibility is a fact about what
    /// somebody actually chose between.
    let indistinguishabilitySetLaw () : LawResult =
        let e = indistinguishabilityFixture ()

        let declared margin =
            match Indistinguishability.declare e "holdoutError" margin EveryAlternative with
            | Ok d -> Some d
            | Error r -> failwith (IndistinguishabilityRefusal.describe r)

        // No declaration: the explicit state, never an empty-list lookalike.
        let silent = Indistinguishability.report None "reference" e = NoDeclaration

        let membersUnder margin =
            match Indistinguishability.report (declared margin) "reference" e with
            | Derived s -> Some(s.Members |> List.map (fun m -> m.State), s.Unread)
            | NoDeclaration
            | NoReferenceReading _
            | MismatchedUnits _ -> None

        // The declared margin admits the reference and the one alternative within it. The
        // closest reading in the whole record belongs to `unrelated` and is absent, because it
        // was never an alternative — which is the claim, stated as a membership test.
        let twoMembers = membersUnder 0.05 = Some([ "reference"; "near" ], [ "root" ])

        // Tightened to zero, the reference stands alone. Nothing is excluded by rounding: the
        // margin is a declared number and the distances are real.
        let alone = membersUnder 0.0 = Some([ "reference" ], [ "root" ])

        // A metric nothing measured is REFUSED rather than answered with a set of one — the
        // shape a genuine "nothing else is close" answer takes.
        let refusesUnmeasured =
            match Indistinguishability.declare e "aMetricNobodyMeasured" 0.05 EveryAlternative with
            | Error(UnmeasuredMetric(metric, declarable)) ->
                metric = "aMetricNobodyMeasured" && declarable = [ "holdoutError" ]
            | _ -> false

        law
            "indistinguishability-set"
            (silent && twoMembers && alone && refusesUnmeasured)
            (sprintf
                "undeclared is silent %b, declared margin yields the two-member set %b, zero margin yields the reference alone %b, an unmeasured metric is refused with the alternatives %b"
                silent
                twoMembers
                alone
                refusesUnmeasured)

    /// Words an indistinguishability surface may not use.
    ///
    /// A FOURTH ban, for a fourth reason, and the sharpest of them. Recurrence must not claim a
    /// merit it cannot support; a correction surface must not read as an act; assent must not
    /// read as a ranking. This surface's entire content is that the evidence CANNOT TELL these
    /// branches apart — so a ranking here is not merely unsupported, it contradicts the finding
    /// it would be attached to. One field renamed in good faith — `Best`, `Winner`, `Rank`,
    /// `Preferred` — turns "four specifications this data cannot separate" into a
    /// recommendation, which is the one thing an under-determined problem cannot yield.
    let bannedIndistinguishabilityVocabulary: string list =
        [ "better"
          "best"
          "superior"
          "optimal"
          "outperform"
          "win"
          "beat"
          "score"
          "rank"
          "grade"
          "prefer"
          "leading"
          "ahead"
          "significan"
          "prove" ]

    /// The indistinguishability surface names a relation, never an order.
    ///
    /// Reflective over the surfaces' own field names, so a field added later is covered without
    /// anybody remembering to extend a list — the same construction as the recurrence,
    /// correction and assent laws, and against the same failure: a rename made in good faith
    /// that nothing else in the build notices.
    let indistinguishabilityVocabularyLaw () : LawResult =
        let fieldsOf (t: System.Type) =
            FSharp.Reflection.FSharpType.GetRecordFields t
            |> Array.map (fun p -> t.Name + "." + p.Name)
            |> List.ofArray

        let fields =
            fieldsOf typeof<IndistinguishabilityDeclaration>
            @ fieldsOf typeof<IndistinguishabilityMember>
            @ fieldsOf typeof<IndistinguishabilitySet>
            @ fieldsOf typeof<IndistinguishabilityRecurrence>
            @ fieldsOf typeof<IndistinguishabilityForestView>

        let offending =
            fields
            |> List.filter (fun name ->
                let lowered = (name.Split '.' |> Array.last).ToLowerInvariant()
                bannedIndistinguishabilityVocabulary |> List.exists lowered.Contains)

        let sentence =
            match
                Indistinguishability.report
                    (match
                        Indistinguishability.declare
                            (indistinguishabilityFixture ())
                            "holdoutError"
                            0.05
                            EveryAlternative
                     with
                     | Ok d -> Some d
                     | Error r -> failwith (IndistinguishabilityRefusal.describe r))
                    "reference"
                    (indistinguishabilityFixture ())
            with
            | Derived s -> Indistinguishability.describeSet s
            | other -> failwithf "the vocabulary law's fixture derived no set: %A" other

        // The rendered line is REQUIRED to say what the set is and that it is not an order,
        // which is exactly why the banned words are checked over the field names and not over
        // this — the sentence has to be free to contain the word it is denying.
        let says =
            sentence.Contains "indistinguishable under the declared margin"
            && sentence.Contains "Nothing here orders them"

        law
            "indistinguishability-vocabulary"
            (List.isEmpty offending && says)
            (if List.isEmpty offending then
                 sprintf
                     "%d fields name a relation; the description states that it is not an order"
                     (List.length fields)
             else
                 "ranking vocabulary in: " + String.concat ", " offending)

    /// A gate never compares across units (D39). Through the reference backend, a gate stated
    /// in the unit its diagnostic is reported in is evaluated; one stated in grams against a
    /// diagnostic in kilograms is refused BY NAME, and so is one stating no unit against a
    /// diagnostic that has one — a guessed unit is a gate that can silently pass. And a backend
    /// that states no units evaluates a gate that states none exactly as before.
    let gateUnitLaw () : LawResult =
        let kg = unitOf "kg"
        let units = Map.ofList [ "holdoutError", kg ]

        let submit (backend: IExecutionBackend) (unit: UnitOfMeasure option) =
            backend.Submit
                { StateId = "s1"
                  Payload = payload "{}" "h"
                  DatasetVersion = "v1"
                  Seed = "7"
                  Gates =
                    [ { Name = "holdoutError"
                        Threshold = 1.0
                        Unit = unit
                        Direction = AtMost } ]
                  SubmittedBy = SubmitAgent }

        let stated = ReferenceBackend.createWith units "reference" "1" "t"
        let unstated = ReferenceBackend.create "reference" "1" "t"

        let failures =
            [ match submit stated (Some kg) with
              | Ok o when o.DiagnosticUnits = units && o.Gates |> List.forall (fun g -> g.Unit = Some kg) -> ()
              | other -> yield sprintf "same unit: %A" other
              match submit stated (Some(unitOf "g")) with
              | Error(GateUnitMismatch m) when
                  m.Gate = "holdoutError" && m.Stated = Some(unitOf "g") && m.Reported = Some kg
                  ->
                  ()
              | other -> yield sprintf "grams against kilograms: %A" other
              match submit stated None with
              | Error(GateUnitMismatch m) when m.Stated = None && m.Reported = Some kg -> ()
              | other -> yield sprintf "no unit against kilograms: %A" other
              match submit unstated None with
              | Ok o when Map.isEmpty o.DiagnosticUnits -> ()
              | other -> yield sprintf "no units anywhere: %A" other ]

        law "gate-units" (List.isEmpty failures) (String.concat "; " failures)

    /// A metric series is compared in one unit only (D40).
    ///
    /// The vintage axis: the kept branch reads 0.5 kg on v1 and v2 and 500 g on v3, the
    /// set-aside branch 0.3 kg throughout. Read as alike, v3 is a lead of 499.7 and the gate of
    /// two out-of-time vintages opens on a unit artefact; read by D40, v3 is named with both
    /// units and never compared, and the gate stays shut. The same holds with v3 stating no
    /// unit at all, and thresholds stating none against a series in kilograms compare nothing.
    ///
    /// Indistinguishability: an in-scope alternative in grams, or stating no unit, beside a
    /// reference in kilograms refuses the set by name; a reading outside the scope is not part
    /// of the series and refuses nothing. And a record with no units anywhere, asked with no
    /// unit, answers exactly as before.
    let seriesUnitLaw () : LawResult =
        let kg = unitOf "kg"
        let g = unitOf "g"
        let decision = { Kept = "kept"; SetAside = "setAside" }
        let inKg = { thresholds 2 with Unit = Some kg }

        let vintages (keptOnV3: float * UnitOfMeasure option) =
            vintageFixtureIn (fun state v ->
                match state, v with
                | "kept", "v3" -> keptOnV3
                | "kept", _ -> 0.5, Some kg
                | _ -> 0.3, Some kg)

        let mixed = vintages (500.0, Some g)
        let unstated = vintages (0.5, None)
        let uniform = vintages (0.5, Some kg)

        let compared (r: DecisionRecord) =
            r.Comparisons |> List.map (fun c -> c.DatasetVersion)

        let indistinguishable (unitOn: StateId -> UnitOfMeasure option) (unit: UnitOfMeasure option) =
            let e = indistinguishabilityFixtureIn unitOn

            match Indistinguishability.declareIn unit e "holdoutError" 0.05 EveryAlternative with
            | Ok d -> Indistinguishability.report (Some d) "reference" e
            | Error r -> failwith (IndistinguishabilityRefusal.describe r)

        let failures =
            [ let r = Vintage.record inKg decision mixed

              if
                  not (
                      compared r = [ "v1"; "v2" ]
                      && r.Unit = Some kg
                      && r.UnitMismatched = [ { DatasetVersion = "v3"
                                                Kept = Some g
                                                SetAside = Some kg } ]
                      && r.Baseline = Some "v1"
                  )
              then
                  yield sprintf "grams beside kilograms: %A" r

              if not (List.isEmpty (Vintage.corrections inKg mixed)) then
                  yield "a mixed vintage opened the correction gate"

              let r = Vintage.record inKg decision unstated

              if
                  r.UnitMismatched
                  <> [ { DatasetVersion = "v3"
                         Kept = None
                         SetAside = Some kg } ]
              then
                  yield sprintf "a stated unit beside an unstated one: %A" r.UnitMismatched

              let r = Vintage.record inKg decision uniform

              if compared r <> [ "v1"; "v2"; "v3" ] || not (List.isEmpty r.UnitMismatched) then
                  yield sprintf "one unit throughout: %A" r

              let r = Vintage.record (thresholds 2) decision uniform

              if not (List.isEmpty r.Comparisons) || List.length r.UnitMismatched <> 3 then
                  yield sprintf "no unit declared against kilograms: %A" r

              let r = Vintage.record (thresholds 2) decision (vintageFixture ())

              if compared r <> [ "v1"; "v2"; "v3" ] || not (List.isEmpty r.UnitMismatched) then
                  yield sprintf "no units anywhere: %A" r

              let kgBut (state: StateId) (other: UnitOfMeasure option) =
                  fun s -> if s = state then other else Some kg

              match indistinguishable (kgBut "near" (Some g)) (Some kg) with
              | MismatchedUnits m when
                  m = { Metric = "holdoutError"
                        Declared = Some kg
                        Stated = [ g; kg ]
                        Unstated = false }
                  ->
                  ()
              | other -> yield sprintf "an alternative in grams: %A" other

              match indistinguishable (kgBut "near" None) (Some kg) with
              | MismatchedUnits m when m.Stated = [ kg ] && m.Unstated -> ()
              | other -> yield sprintf "an alternative stating no unit: %A" other

              match indistinguishable (kgBut "unrelated" (Some g)) (Some kg) with
              | Derived s when s.Members |> List.map (fun m -> m.State) = [ "reference"; "near" ] -> ()
              | other -> yield sprintf "a reading outside the scope: %A" other

              match indistinguishable (fun _ -> Some kg) None with
              | MismatchedUnits m when m.Declared = None && m.Stated = [ kg ] && not m.Unstated -> ()
              | other -> yield sprintf "no unit declared against kilograms: %A" other

              match indistinguishable (fun _ -> None) None with
              | Derived s when s.Members |> List.map (fun m -> m.State) = [ "reference"; "near" ] -> ()
              | other -> yield sprintf "no units anywhere: %A" other ]

        law "series-units" (List.isEmpty failures) (String.concat "; " failures)

    /// Every law, in one pass. A host that runs this and gets all-passed has certified
    /// the substrate's promises against its own build.
    let laws (hashFn: HashFn) : LawResult list =
        [ defaultDenyLaw ()
          refusalEnumeratesLaw ()
          codecLaw ()
          bytePinLaw ()
          pathReplayLaw hashFn
          divergenceDowngradeLaw ()
          recurrenceVocabularyLaw ()
          replayIdentityLaw ()
          correctionThresholdLaw ()
          correctionReviewVocabularyLaw ()
          assentConsensusLaw ()
          assentTensionLaw ()
          assentVocabularyLaw ()
          indistinguishabilitySetLaw ()
          indistinguishabilityVocabularyLaw ()
          gateUnitLaw ()
          seriesUnitLaw () ]

    /// The laws as a single verdict.
    let certify (hashFn: HashFn) : Result<unit, string> =
        match laws hashFn |> List.filter (fun l -> not l.Passed) with
        | [] -> Ok()
        | failures -> Error(failures |> List.map (fun l -> l.Law + ": " + l.Detail) |> String.concat "; ")
