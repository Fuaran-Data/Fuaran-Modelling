// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.VintageTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The vintage axis, unit by unit: the replay plan and its identity rule, the
//  fold's account of a campaign, the evidence-base queries, and the gate.
//
//  The scenario suite runs the whole thing through the execution port. This one
//  drives the pieces directly, because the properties that matter here are about
//  what is REFUSED and what is SILENT, and those are the two things an
//  end-to-end run is least likely to exercise.
// ============================================================================

let private reading (state: StateId) (v: string) (seed: string) (x: float) : ExplorationOp =
    Annotate
        { StateId = state
          Snapshot =
            { Key =
                { SpecHash = "h-" + state
                  DatasetVersion = v
                  Seed = seed
                  ProviderId = "reference"
                  ProviderVersion = "1" }
              ArtifactRef = "ref:" + state + "-" + v
              Diagnostics = Map.ofList [ "holdoutError", x ]
              DiagnosticUnits = Map.empty
              Gates = []
              SubmittedBy = SubmitScheduled
              RecordedAt = "2026-01-01T00:00:00Z" } }

let private gate (minVintages: int) (minSiblings: int) (minMargin: float) : CorrectionThresholds =
    { Metric = "holdoutError"
      Prefer = PreferLower
      MinVintages = minVintages
      MinMargin = minMargin
      MinSiblings = minSiblings
      Unit = None }

/// The kept branch reads 0.5 on every vintage and the branch that was set aside reads 0.3,
/// under a metric where lower leads — so the set-aside branch reads ahead by 0.2 throughout.
/// Deliberately flat: these tests are about the GATE, and readings that wobbled would be
/// testing the wobble.
let private evidence (vintages: string list) : Exploration =
    foldOrFail (
        [ seedOp "kept"; forkOp "setAside" "kept" ]
        @ [ for v in vintages -> reading "kept" v "7" 0.5 ]
        @ [ for v in vintages -> reading "setAside" v "7" 0.3 ]
        @ [ curateOp "kept" Keep "the settled form"
            curateOp "setAside" Kill "not interpretable on this unit" ]
    )

let private theDecision = { Kept = "kept"; SetAside = "setAside" }

let private receiptFor (requested: StateId list) (landed: (StateId * CompositeKey) list) : ReplayReceipt =
    { ReplayId = "r1"
      DatasetVersion = "v2"
      Requested = requested
      Landed = landed
      Refused =
        requested
        |> List.filter (fun id -> not (landed |> List.exists (fun (l, _) -> l = id)))
        |> List.map (fun id -> id, NoBaselineFit)
      ReplayedBy = "curator"
      ReplayedAt = "2026-02-01T00:00:00Z" }

[<Tests>]
let planTests =
    testList
        "Vintage / the replay plan"
        [ test "the default selection is every fitted branch — kept AND killed" {
              // The whole reason the axis exists. An abandoned branch was retained, so it is
              // a counterfactual that can be tested; a default that quietly re-fitted only
              // the survivors would gather exactly the evidence that cannot answer the
              // question being asked.
              let e = evidence [ "v1" ]
              let plan = ReplayCampaign.plan "r1" EveryFittedBranch "v2" [] SubmitScheduled e

              Expect.equal (plan.Items |> List.map _.State) [ "kept"; "setAside" ] "both branches, in exploration order"

              Expect.contains (Query.killedStates e) "setAside" "and one of them is killed"
              Expect.isEmpty plan.Refused "nothing was refused before submission"
          }

          test "a branch that was never fitted is not selected, and a NAMED one is refused" {
              let e =
                  foldOrFail [ seedOp "kept"; forkOp "unfitted" "kept"; reading "kept" "v1" "7" 0.5 ]

              let byDefault = ReplayCampaign.plan "r1" EveryFittedBranch "v2" [] SubmitScheduled e
              Expect.equal (byDefault.Items |> List.map _.State) [ "kept" ] "an unfitted branch is not a fitted one"

              let named =
                  ReplayCampaign.plan "r2" (NamedBranches [ "kept"; "unfitted" ]) "v2" [] SubmitScheduled e

              Expect.equal named.Refused [ "unfitted", NoBaselineFit ] "a caller that named it is owed an answer"

              Expect.equal
                  (ReplayCampaign.requested named)
                  [ "kept"; "unfitted" ]
                  "and the campaign is answerable for both"
          }

          test "the replayed key differs from the baseline in the dataset version, and in nothing else" {
              let e = evidence [ "v1" ]
              let plan = ReplayCampaign.plan "r1" EveryFittedBranch "v2" [] SubmitScheduled e

              for item in plan.Items do
                  let key = ReplayCampaign.expectedKey plan.DatasetVersion item

                  Expect.equal key.SpecHash item.Baseline.SpecHash "the same specification"
                  Expect.equal key.Seed item.Baseline.Seed "the same seed"
                  Expect.equal key.ProviderId item.Baseline.ProviderId "the same provider"
                  Expect.equal key.ProviderVersion item.Baseline.ProviderVersion "at the same version"
                  Expect.equal key.DatasetVersion "v2" "and only the vintage moved"
                  Expect.isTrue (ReplayCampaign.isReplayOf item.Baseline key) "so it IS a replay of that fit"

                  Expect.isFalse
                      (ReplayCampaign.isReplayOf item.Baseline { key with Seed = "99" })
                      "and a re-derived seed would not be"
          }

          test "the seed comes from the FIRST fit, not the most recent one" {
              // The seed rule, stated as a difference. A branch fitted at v1 with seed 7 and
              // re-fitted at v2 with seed 9 has two records; a replay onto v3 carries the
              // baseline's 7, so every fit in the series differs from the original in the
              // vintage alone. Taking the latest instead would let one stray submission
              // re-key everything after it.
              let e =
                  foldOrFail [ seedOp "kept"; reading "kept" "v1" "7" 0.5; reading "kept" "v2" "9" 0.4 ]

              let plan = ReplayCampaign.plan "r1" EveryFittedBranch "v3" [] SubmitScheduled e
              let item = List.exactlyOne plan.Items

              Expect.equal item.Baseline.DatasetVersion "v1" "the baseline is the first fit"
              Expect.equal item.Submission.Seed "7" "and its seed is what a replay carries"
          }

          test "a named branch the record does not hold is not selected at all" {
              // Distinct from a named branch that exists and was never fitted, which IS
              // planned and refused: a caller that named a branch this record has never heard
              // of is naming another record's branch, and answering about it would be a
              // fabrication rather than an account.
              let e = evidence [ "v1" ]

              Expect.equal
                  (ReplayCampaign.selected (NamedBranches [ "kept"; "ghost" ]) e)
                  [ "kept" ]
                  "a branch that is not there is not a branch"

              Expect.equal
                  (ReplayCampaign.selected EveryFittedBranch e)
                  [ "kept"; "setAside" ]
                  "and the default is every fitted one"

              Expect.equal (ReplaySelection.encode EveryFittedBranch) "everyFitted" "the selection has a wire spelling"
              Expect.equal (ReplaySelection.encode (NamedBranches [])) "named" "for both cases"
          }

          test "every per-branch refusal says something a caller can act on" {
              for r in
                  [ NoBaselineFit
                    ExecutorRefused(BudgetDenied(100.0, 99.5))
                    NotRecorded(DuplicateOutcome("kept", compositeKey "h")) ] do
                  Expect.isNotEmpty (ReplayRefusal.describe r) "the refusal says something"

              Expect.stringContains
                  (ReplayRefusal.describe (ExecutorRefused(UnknownProvider [ "reference" ])))
                  "reference"
                  "and carries the alternatives the layer below enumerated"
          }

          test "the submission carries the branch's own payload, unread" {
              let e = evidence [ "v1" ]
              let plan = ReplayCampaign.plan "r1" EveryFittedBranch "v2" [] SubmitScheduled e

              for item in plan.Items do
                  let state = Query.tryState item.State e |> Option.get

                  Expect.equal item.Submission.Payload state.Payload "the same specification, verbatim"
          } ]

[<Tests>]
let foldTests =
    testList
        "Vintage / the fold's account of a campaign"
        [ test "a receipt lands on the record and is a log, not a field" {
              let e = evidence [ "v1" ]

              let landed = [ "kept", compositeKey "h-kept" ]

              let folded =
                  match
                      Reducer.applyAll
                          [ Replay { Receipt = receiptFor [ "kept"; "setAside" ] landed }
                            Replay
                                { Receipt =
                                    { receiptFor [ "kept" ] landed with
                                        ReplayId = "r2"
                                        DatasetVersion = "v3" } } ]
                          e
                  with
                  | Ok x -> x
                  | Error(i, r) -> failtestf "op %d rejected: %s" i (Rejection.describe r)

              Expect.equal (Query.replays folded |> List.map _.ReplayId) [ "r1"; "r2" ] "both campaigns are kept"

              Expect.equal
                  (Query.replays folded |> List.map _.DatasetVersion)
                  [ "v2"; "v3" ]
                  "one campaign says nothing; the accumulation is the signal"
          }

          test "a second campaign under an id already recorded is refused" {
              let e = evidence [ "v1" ]
              let receipt = receiptFor [ "kept" ] [ "kept", compositeKey "h-kept" ]

              match Reducer.applyAll [ Replay { Receipt = receipt }; Replay { Receipt = receipt } ] e with
              | Ok _ -> failtest "a reused replay id silently overwrote an account"
              | Error(i, DuplicateReplay id) ->
                  Expect.equal i 1 "the second one"
                  Expect.equal id "r1" "named"
              | Error(_, other) -> failtestf "expected a duplicate-replay refusal, got %s" (Rejection.describe other)
          }

          test "a receipt naming a branch that is not there is refused" {
              match
                  Reducer.apply
                      (Replay { Receipt = receiptFor [ "kept"; "ghost" ] [ "kept", compositeKey "h-kept" ] })
                      (evidence [ "v1" ])
              with
              | Ok _ -> failtest "a receipt named a branch the record does not hold"
              | Error(UnknownState id) -> Expect.equal id "ghost" "named"
              | Error other -> failtestf "expected an unknown-state refusal, got %s" (Rejection.describe other)
          }

          test "a receipt that loses a branch between asking and answering is refused" {
              // The failure the port's own receipt is shaped to prevent, one layer up: a
              // branch in neither list is a fit the submitter believes was taken and nothing
              // has a record of. Here the fold can simply refuse it.
              let incomplete =
                  { receiptFor [ "kept"; "setAside" ] [ "kept", compositeKey "h-kept" ] with
                      Refused = [] }

              Expect.isFalse (ReplayReceipt.isTotal incomplete) "the receipt does not account for both"

              match Reducer.apply (Replay { Receipt = incomplete }) (evidence [ "v1" ]) with
              | Ok _ -> failtest "an unaccounted branch was recorded as if it were accounted for"
              | Error(IncompleteReplay id) -> Expect.equal id "r1" "named"
              | Error other -> failtestf "expected an incomplete-replay refusal, got %s" (Rejection.describe other)
          }

          test "a receipt on an unseeded record is refused" {
              match Reducer.apply (Replay { Receipt = receiptFor [] [] }) Exploration.empty with
              | Ok _ -> failtest "a campaign was recorded against nothing"
              | Error NotSeeded -> ()
              | Error other -> failtestf "expected a not-seeded refusal, got %s" (Rejection.describe other)
          } ]

[<Tests>]
let sessionTests =
    testList
        "Vintage / recording a campaign through the session"
        [ test "recording a replay is a governed act, default-denied like every other" {
              let session = sessionOrFail [ seedOp "kept"; reading "kept" "v1" "7" 0.5 ]
              let receipt = receiptFor [ "kept" ] [ "kept", compositeKey "h-kept" ]

              match Session.recordReplay hashFn Policy.empty "explorer" explorer receipt session with
              | Ok _ -> failtest "the empty policy authorised a replay record"
              | Error(PolicyBlocked refusal) -> Expect.equal refusal.Attempted "emit:replay" "the refusal names it"
              | Error other -> failtestf "expected a policy refusal, got %A" other

              match Session.recordReplay hashFn (fullPolicy "curator") "curator" curator receipt session with
              | Error failure -> failtestf "the granted subject was refused: %A" failure
              | Ok(_, s) -> Expect.equal (Query.replays s.Record |> List.length) 1 "and a granted one lands"
          }

          test "a campaign receipt is exploration-level, and a branch's line stays its own" {
              // The receipt hangs off the cursor and claims no branch's mark, so replaying a
              // branch's line yields that branch's EVIDENCE — which is line-local, because
              // each outcome arrived by an ordinary annotate — and not the bookkeeping of the
              // campaign that gathered it.
              let session =
                  sessionOrFail
                      [ seedOp "kept"
                        forkOp "setAside" "kept"
                        reading "kept" "v1" "7" 0.5
                        reading "setAside" "v1" "7" 0.3 ]

              let session =
                  match
                      Session.recordReplay
                          hashFn
                          (fullPolicy "curator")
                          "curator"
                          curator
                          (receiptFor [ "kept"; "setAside" ] [])
                          session
                  with
                  | Ok(_, s) -> s
                  | Error failure -> failtestf "the receipt did not land: %A" failure

              match Session.replayState "setAside" session with
              | Error detail -> failtest detail
              | Ok line ->
                  Expect.equal line.Order [ "kept"; "setAside" ] "the line is its own lineage"

                  Expect.equal ((Map.find "setAside" line.States).Outcomes |> List.length) 1 "carrying its own evidence"

                  Expect.isEmpty line.Replays "and not the campaign's bookkeeping"

              Expect.equal (Query.replays session.Record |> List.length) 1 "which the whole record does carry"
          }

          test "a campaign survives the history, byte for byte" {
              let session = sessionOrFail [ seedOp "kept"; reading "kept" "v1" "7" 0.5 ]

              let session =
                  match
                      Session.recordReplay
                          hashFn
                          (fullPolicy "curator")
                          "curator"
                          curator
                          (receiptFor [ "kept" ] [ "kept", compositeKey "h-kept" ])
                          session
                  with
                  | Ok(_, s) -> s
                  | Error failure -> failtestf "the receipt did not land: %A" failure

              match Session.toJsonl session |> Session.ofJsonl hashFn with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  Expect.equal back.Record.Replays session.Record.Replays "the account replays identically"
                  Expect.isTrue (Session.verify hashFn back) "and the history still verifies"
          } ]

[<Tests>]
let queryTests =
    testList
        "Vintage / the evidence base"
        [ test "vintage order is history, not an alphabet" {
              // The alternative — sorting the version strings — puts `ds-10` before `ds-2`
              // and calls the result chronology.
              let e = evidence [ "ds-2"; "ds-10" ]
              Expect.equal (Vintage.order e) [ "ds-2"; "ds-10" ] "the order the record first saw each"
          }

          test "a branch's series reads one metric across the vintages it has" {
              let e = evidence [ "v1"; "v2" ]
              let s = Vintage.series "holdoutError" "setAside" e

              Expect.equal s.Readings [ "v1", 0.3; "v2", 0.3 ] "one reading per fitted vintage"

              Expect.isEmpty
                  (Vintage.series "notMeasured" "setAside" e).Readings
                  "a metric nobody asked for was not measured, and is absent rather than zero"
          }

          test "a decision is a pair somebody was actually choosing between" {
              // Two branches on unrelated lines were never weighed against each other, and
              // pairing them would manufacture decisions nobody took — hundreds of them in an
              // exploration of any size, each able to raise a question.
              let e =
                  foldOrFail
                      [ seedOp "root"
                        forkOp "a" "root"
                        forkOp "b" "root"
                        forkOp "a1" "a"
                        forkOp "b1" "b"
                        curateOp "a1" Keep "kept on one line"
                        curateOp "b1" Kill "set aside on another" ]

              Expect.isEmpty (Vintage.decisions e) "an unrelated kept/killed pair is not a decision"

              let related =
                  foldOrFail
                      [ seedOp "root"
                        forkOp "a" "root"
                        forkOp "b" "root"
                        curateOp "a" Keep "kept"
                        curateOp "b" Kill "set aside" ]

              Expect.equal (Vintage.decisions related) [ { Kept = "a"; SetAside = "b" } ] "two forks off one parent are"

              Expect.equal
                  (Vintage.decisions (evidence [ "v1" ]))
                  [ theDecision ]
                  "and so is a branch and the parent it was tried against"
          }

          test "the baseline vintage is named and excluded from the out-of-time set" {
              let e = evidence [ "v1"; "v2"; "v3" ]
              let r = Vintage.record (gate 2 1 0.1) theDecision e

              Expect.equal r.Baseline (Some "v1") "the first vintage both branches were comparable on"
              Expect.equal (List.length r.Comparisons) 3 "every comparable vintage is compared"

              Expect.equal
                  (r.Comparisons |> List.filter _.OutOfTime |> List.map _.DatasetVersion)
                  [ "v2"; "v3" ]
                  "and only the later ones are out-of-time"

              for c in r.Comparisons do
                  Expect.floatClose Accuracy.high c.SetAsideLead 0.2 "the set-aside branch reads ahead by 0.2"
          }

          test "a vintage only one branch has is named, never quietly dropped" {
              let e =
                  foldOrFail (
                      [ seedOp "kept"; forkOp "setAside" "kept" ]
                      @ [ reading "kept" "v1" "7" 0.5
                          reading "setAside" "v1" "7" 0.3
                          reading "kept" "v2" "7" 0.5 ]
                      @ [ curateOp "kept" Keep "settled"; curateOp "setAside" Kill "set aside" ]
                  )

              let r = Vintage.record (gate 1 1 0.1) theDecision e

              Expect.equal (r.Comparisons |> List.map _.DatasetVersion) [ "v1" ] "one comparable vintage"

              Expect.equal r.Incomparable [ "v2" ] "and the one where the comparison base shrank is named"
          } ]

[<Tests>]
let gateTests =
    testList
        "Vintage / the slow-accumulation guard"
        [ test "below the declared threshold the record accumulates and says nothing" {
              let e = evidence [ "v1"; "v2" ]

              // One out-of-time vintage. The evidence is real and is recorded; the gate is
              // not met, and silence is the correct answer rather than a hedged one.
              let r = Vintage.record (gate 2 1 0.1) theDecision e
              Expect.equal (List.length (Vintage.leadingVintages (gate 2 1 0.1) r)) 1 "the lead IS recorded"

              Expect.isEmpty (Vintage.corrections (gate 2 1 0.1) e) "and nothing is raised on it"
          }

          test "at the threshold, exactly one question is raised" {
              let e = evidence [ "v1"; "v2"; "v3" ]

              match Vintage.corrections (gate 2 1 0.1) e with
              | [ finding ] ->
                  Expect.equal finding.Decision theDecision "about the decision it is about"
                  Expect.equal finding.LeadingVintages [ "v2"; "v3" ] "on the out-of-time vintages, listed"
                  Expect.floatClose Accuracy.high finding.SmallestLead 0.2 "with the weakest link in the run"
                  Expect.isEmpty finding.RaisedIn "one exploration names no siblings rather than implying breadth"
                  Expect.equal finding.Thresholds (gate 2 1 0.1) "and carries the gate it cleared"
              | other -> failtestf "expected exactly one finding, got %d" (List.length other)
          }

          test "a margin the lead does not clear is silent, however many vintages there are" {
              let e = evidence [ "v1"; "v2"; "v3"; "v4" ]

              Expect.isEmpty
                  (Vintage.corrections (gate 2 1 0.3) e)
                  "a lead of 0.2 does not clear a declared margin of 0.3"

              Expect.equal (List.length (Vintage.corrections (gate 2 1 0.2) e)) 1 "and clears one of exactly 0.2"
          }

          test "a sibling threshold above one silences a single exploration" {
              // One exploration is ONE sibling and is counted as one. Skipping the gate here
              // instead would let the cheapest read bypass the gate the expensive one
              // enforces, which is how a declared threshold becomes a decorative one.
              let e = evidence [ "v1"; "v2"; "v3" ]

              Expect.isEmpty (Vintage.corrections (gate 2 2 0.1) e) "the breadth axis is what a single unit lacks"
          }

          test "a threshold of zero is read as one — a gate that opens completely is not a gate" {
              let e = evidence [ "v1" ]

              // One vintage, and it is the baseline: no out-of-time evidence at all. A
              // MinVintages of 0 taken literally would raise a question on nothing.
              Expect.isEmpty (Vintage.corrections (gate 0 0 0.1) e) "nothing out-of-time, nothing raised"
          }

          test "the rendered line raises a question and denies having acted" {
              let e = evidence [ "v1"; "v2"; "v3" ]
              let finding = List.exactlyOne (Vintage.corrections (gate 2 1 0.1) e)
              let line = Vintage.describeCorrection finding

              Expect.stringContains line "out-of-time" "the evidence is named as what it is"
              Expect.stringContains line "raised for review" "it is a question"
              Expect.stringContains line "No curation has been changed" "and it says what it has not done"
          } ]

[<Tests>]
let forestTests =
    testList
        "Vintage / the forest roll-up"
        [ let sibling (id: SiblingId) : SiblingRef =
              { Sibling = id
                Label = "unit " + id
                ExplorationRef = "memory:" + id }

          let forestWith (thresholds: CorrectionThresholds option) : Forest =
              { Id = "portfolio"
                Title = "three units under one process"
                Siblings = [ sibling "a"; sibling "b"; sibling "c" ]
                Policy = fullPolicy "curator"
                Thresholds = thresholds }

          let allThree () =
              Map.ofList
                  [ "a", evidence [ "v1"; "v2"; "v3" ]
                    "b", evidence [ "v1"; "v2"; "v3" ]
                    "c", evidence [ "v1"; "v2" ] ]

          yield
              test "a forest that declares no thresholds raises nothing, whatever the evidence says" {
                  // Declared or silent; there is no third state, and the third state is the
                  // one a defaulted record would have invented.
                  Expect.isEmpty (Vintage.forestCorrections (forestWith None) (allThree ())) "the gate is shut"
              }

          yield
              test "the sibling axis is the breadth the depth cannot supply" {
                  // Two of the three siblings have three vintages and clear the vintage gate;
                  // the third has only its baseline and one refresh. So a sibling threshold of
                  // two is met and one of three is not.
                  match Vintage.forestCorrections (forestWith (Some(gate 2 2 0.1))) (allThree ()) with
                  | [ finding ] ->
                      Expect.equal finding.Decision theDecision "one question, about one decision"
                      Expect.equal finding.RaisedIn [ "a"; "b" ] "naming the siblings that raised it"
                  | other -> failtestf "expected exactly one finding, got %d" (List.length other)

                  Expect.isEmpty
                      (Vintage.forestCorrections (forestWith (Some(gate 2 3 0.1))) (allThree ()))
                      "and a third sibling that has not accumulated yet keeps the gate shut"
              }

          yield
              test "recurrence and the out-of-time record sit beside each other, distinguishable" {
                  // The answer to a count that cannot license a verdict is not to rename the
                  // count. It is to put a different KIND of evidence next to it and keep the
                  // two apart.
                  let rows =
                      Vintage.recurrenceWithPower (forestWith (Some(gate 2 2 0.1))) (allThree ())

                  let kept = rows |> List.find (fun r -> r.Recurrence.State = "kept")

                  Expect.equal kept.Recurrence.ChosenIn [ "a"; "b"; "c" ] "the count is still a count of choices"
                  Expect.equal (kept.Records |> List.map fst) [ "a"; "b"; "c" ] "with every sibling's record beside it"
                  Expect.equal kept.RaisedIn [ "a"; "b" ] "and the two whose record clears the gate"

                  Expect.stringContains
                      (Forest.describeRecurrence 3 kept.Recurrence)
                      "not what is better"
                      "the recurrence sentence is unchanged — it still says what it is not"

                  let withoutThresholds =
                      Vintage.recurrenceWithPower (forestWith None) (allThree ())
                      |> List.find (fun r -> r.Recurrence.State = "kept")

                  Expect.isEmpty withoutThresholds.Records "an undeclared gate carries no records"
                  Expect.isEmpty withoutThresholds.RaisedIn "and raises nothing"
              } ]

[<Tests>]
let wireTests =
    testList
        "Vintage / the wire"
        [ let roundTrip (encode: 'T -> JVal) (decode: JVal -> Result<'T, string>) (name: string) (value: 'T) =
              let text = Canonical.write Rendering.CrossHost (encode value)

              match Json.parse text |> Result.bind decode with
              | Ok back -> Expect.equal back value (name + " round-trips: " + text)
              | Error m -> failtestf "%s failed to decode (%s): %s" name text m

          yield
              test "every replay refusal round-trips" {
                  for r in
                      [ NoBaselineFit
                        ExecutorRefused(BudgetDenied(100.0, 99.5))
                        ExecutorRefused(UnknownProvider [ "reference" ])
                        NotRecorded(DuplicateOutcome("kept", compositeKey "h"))
                        NotRecorded(IncompleteReplay "r1") ] do
                      roundTrip Wire.encodeReplayRefusal Wire.decodeReplayRefusal "replayRefusal" r
              }

          yield
              test "a replay receipt round-trips, landed and refused rows alike" {
                  let receipt =
                      { receiptFor [ "kept"; "setAside" ] [ "kept", compositeKey "h-kept" ] with
                          Refused =
                              [ "setAside", ExecutorRefused(BackendPolicyRefused "no automated fits at month end") ] }

                  roundTrip Wire.encodeReplayReceipt Wire.decodeReplayReceipt "replayReceipt" receipt
              }

          yield
              test "the two new fold rejections round-trip" {
                  for r in [ DuplicateReplay "r1"; IncompleteReplay "r1" ] do
                      roundTrip Wire.encodeRejection Wire.decodeRejection "rejection" r
              }

          yield
              test "an exploration that has seen a campaign carries it across the wire" {
                  let e =
                      match
                          Reducer.apply
                              (Replay { Receipt = receiptFor [ "kept"; "setAside" ] [ "kept", compositeKey "h-kept" ] })
                              (evidence [ "v1" ])
                      with
                      | Ok x -> x
                      | Error r -> failtestf "the receipt did not land: %s" (Rejection.describe r)

                  match Wire.parseExploration (Wire.renderExploration e) with
                  | Ok back -> Expect.equal back.Replays e.Replays "the campaign survives"
                  | Error m -> failtestf "failed to decode: %s" m

                  // And a record that has seen none encodes to the bytes it always did.
                  Expect.equal
                      (Wire.renderExploration Exploration.empty)
                      "{\"proposals\":[],\"resolutions\":[],\"states\":[]}"
                      "additive by construction, not by intention"
              }

          yield
              test "thresholds and the vintage records round-trip" {
                  for prefer in MetricPreference.all do
                      roundTrip
                          Wire.encodeThresholds
                          Wire.decodeThresholds
                          "thresholds"
                          { gate 2 1 0.1 with Prefer = prefer }

                  roundTrip Wire.encodeDecision Wire.decodeDecision "decision" theDecision

                  let e = evidence [ "v1"; "v2"; "v3" ]

                  roundTrip
                      Wire.encodeVintageSeries
                      Wire.decodeVintageSeries
                      "series"
                      (Vintage.series "holdoutError" "kept" e)

                  let r = Vintage.record (gate 2 1 0.1) theDecision e
                  roundTrip Wire.encodeDecisionRecord Wire.decodeDecisionRecord "decisionRecord" r

                  for c in r.Comparisons do
                      roundTrip Wire.encodeComparison Wire.decodeComparison "comparison" c

                  roundTrip
                      Wire.encodeCorrection
                      Wire.decodeCorrection
                      "correction"
                      (List.exactlyOne (Vintage.corrections (gate 2 1 0.1) e))
              }

          yield
              test "a forest carries declared thresholds and encodes nothing when it declares none" {
                  let bare = Forest.empty "f1" "no thresholds declared"

                  roundTrip Wire.encodeForest Wire.decodeForest "forest" bare

                  Expect.equal
                      (Wire.renderForest bare)
                      "{\"id\":\"f1\",\"policy\":{\"objectives\":[]},\"siblings\":[],\"title\":\"no thresholds declared\"}"
                      "a silent default is silent on the wire too"

                  let declared =
                      { bare with
                          Thresholds = Some(gate 3 2 0.05) }

                  roundTrip Wire.encodeForest Wire.decodeForest "forest with thresholds" declared

                  Expect.stringContains (Wire.renderForest declared) "\"thresholds\"" "and a declared gate is carried"
              }

          yield
              test "recurrence-with-power round-trips whole" {
                  let sibling (id: SiblingId) : SiblingRef =
                      { Sibling = id
                        Label = "unit " + id
                        ExplorationRef = "memory:" + id }

                  let forest =
                      { Id = "portfolio"
                        Title = "two units"
                        Siblings = [ sibling "a"; sibling "b" ]
                        Policy = Policy.empty
                        Thresholds = Some(gate 2 1 0.1) }

                  let records =
                      Map.ofList [ "a", evidence [ "v1"; "v2"; "v3" ]; "b", evidence [ "v1"; "v2"; "v3" ] ]

                  for row in Vintage.recurrenceWithPower forest records do
                      roundTrip Wire.encodeRecurrenceWithPower Wire.decodeRecurrenceWithPower "recurrenceWithPower" row
              } ]
