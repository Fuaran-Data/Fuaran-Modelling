// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.VintageScenarioTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Execution

// ============================================================================
//  The Rung-3 story as one scripted run, end to end through the execution port.
//
//  One unit explores two forms, keeps one and kills the other on the evidence of
//  the first vintage. Then new data arrives — twice — and the exploration is
//  REPLAYED onto it: the branch that was kept and the branch that was killed
//  alike, as one batch, through the port, with every outcome landing by the
//  ordinary annotate path.
//
//  What the run is written to demonstrate is the shape of the guard rather than
//  the arithmetic of it. After one refresh the branch that was set aside is
//  reading ahead and the record says so — and NOTHING IS RAISED, because one
//  refresh is one refresh and a decision that looks wrong after it may be a
//  regime blip. After the second, the declared threshold is met and exactly one
//  question is raised, for a person, in a sentence that says it has changed
//  nothing.
//
//  Every fixture term is neutral. The forms, the metric and the dataset are
//  named for what they are in a test.
// ============================================================================

let private instant = "2026-02-01T00:00:00+00:00"

let private panel (version: int) =
    ReferenceExecutor.vintage "quarterly-panel" version

let private config: ReferenceExecutorConfig =
    { ProviderId = "reference-linear"
      ProviderVersion = "1.4.0"
      ProviderKinds = [ "reference-linear" ]
      Vintages = [ panel 1; panel 2; panel 3 ]
      Instant = instant
      Quota = Some 100000.0
      DiagnosticUnits = Map.empty }

let private bridgeConfig: BridgeConfig =
    { ProviderKind = "reference-linear"
      SpecHashAlgorithm = Minting.algorithmId
      BatchPrefix = "manager" }

let private policy = Sample.fullPolicy "curator"

let private minted (body: string) : SpecPayload =
    match Minting.mint body with
    | Error refusal -> failwithf "the fixture payload is not mintable: %s" (Minting.describeRefusal refusal)
    | Ok hash ->
        { Dialect = "reference"
          Format = "json"
          Body = body
          SpecHash = hash }

/// The two forms this unit weighed against one another.
let private settledForm = minted "{\"family\":\"linear\",\"terms\":[\"a\",\"b\"]}"

let private alternativeForm = minted "{\"family\":\"linear\",\"terms\":[\"a\"]}"

/// The metric the comparison reads. It has to be REQUESTED as a gate for the executor to
/// carry it in diagnostics, which is the manager's whole relationship with a diagnostic
/// name: it enumerates none, so a metric nobody asked for is a metric nobody measured.
///
/// `AtMost 1.0` always holds, because the reference executor's diagnostics live in `[0, 1)`.
/// The gate is here to put the reading on the record, not to fail anything.
let private metric = "holdoutError"

// Qualified: the mirror tier declares its own `GateDirection` / `Gate` / `FitSubmission`
// under the same names, because it models a foreign specification rather than this one.
let private gates: Fuaran.Modelling.GateRequest list =
    [ { Name = metric
        Threshold = 1.0
        Unit = None
        Direction = Fuaran.Modelling.AtMost } ]

/// The declared gate: two out-of-time vintages, a lead of at least 0.05, one sibling.
let private thresholds: CorrectionThresholds =
    { Metric = metric
      Prefer = PreferLower
      MinVintages = 2
      MinMargin = 0.05
      MinSiblings = 1
      Unit = None }

let private theDecision =
    { Kept = "settled"
      SetAside = "alternative" }

let private submission (stateId: StateId) (payload: SpecPayload) (version: int) : Fuaran.Modelling.FitSubmission =
    { StateId = stateId
      Payload = payload
      DatasetVersion = "quarterly-panel@" + string version
      Seed = "20260720"
      Gates = gates
      SubmittedBy = SubmitHuman }

/// Seed the settled form, fork the alternative, fit both on the FIRST vintage, and curate on
/// that evidence: keep one, kill the other with a reason. Nothing is deleted — which is the
/// property everything below depends on.
let private explore (port: IModelExecutionBackend) : Session =
    let step op s =
        match Session.submitGoverned Sample.hashFn policy "curator" Sample.curator op s with
        | Ok(_, s') -> s'
        | Error e -> failwithf "op %s did not land: %A" (Op.kindName op) e

    let fit (stateId: StateId) (payload: SpecPayload) s =
        match
            Bridge.submitFit
                Sample.hashFn
                bridgeConfig
                port
                policy
                "curator"
                Sample.curator
                (submission stateId payload 1)
                s
        with
        | Ok(_, _, s') -> s'
        | Error failure -> failwithf "the baseline fit on '%s' did not land: %A" stateId failure

    Session.empty
    |> step (
        Seed
            { StateId = "settled"
              Label = "the settled form"
              Payload = settledForm }
    )
    |> step (
        Fork
            { StateId = "alternative"
              From = "settled"
              Label = "one term fewer"
              Payload = alternativeForm }
    )
    |> fit "settled" settledForm
    |> fit "alternative" alternativeForm
    |> step (Sample.curateOp "settled" Keep "the fuller form was more legible to the people who read it")
    |> step (Sample.curateOp "alternative" Kill "the shorter form was harder to argue for at the time")

/// Replay every fitted branch — kept AND killed — onto a vintage, as one batch.
///
/// The campaign id is explicit rather than derived from the vintage, because running two
/// campaigns against one vintage is a thing this suite deliberately does — and a derived id
/// would be refused as a duplicate before the interesting refusal could happen.
let private replayWith
    (port: IModelExecutionBackend)
    (replayId: ReplayId)
    (version: int)
    (session: Session)
    : ReplayRun =
    let plan =
        ReplayCampaign.plan
            replayId
            EveryFittedBranch
            ("quarterly-panel@" + string version)
            gates
            SubmitScheduled
            session.Record

    match
        Bridge.replayOnto Sample.hashFn bridgeConfig port policy "curator" Sample.curator "curator" instant plan session
    with
    | Ok run -> run
    | Error failure -> failwithf "the replay onto vintage %d did not run: %A" version failure

let private replayOnto (port: IModelExecutionBackend) (version: int) (session: Session) : ReplayRun =
    replayWith port ("replay-v" + string version) version session

let private executor () = ReferenceExecutor.create config

[<Tests>]
let tests =
    testList
        "VintageScenario"
        [ test "the fixture is a real disagreement — the branch that was killed reads ahead" {
              // A precondition, checked rather than assumed. A scenario in which the set-aside
              // branch happened not to lead would run to completion and demonstrate nothing,
              // and it would look exactly like this one.
              let settledReading = ReferenceExecutor.diagnostic settledForm.SpecHash metric

              let alternativeReading =
                  ReferenceExecutor.diagnostic alternativeForm.SpecHash metric

              Expect.isLessThan
                  alternativeReading
                  settledReading
                  "the branch the curator set aside reads lower on a metric where lower leads"

              Expect.isGreaterThan
                  (settledReading - alternativeReading)
                  thresholds.MinMargin
                  "and by more than the declared margin, so the margin gate is exercised rather than tripped"
          }

          test "the exploration replays kept AND killed branches onto a new vintage, as one batch" {
              let port = executor ()
              let session = explore port

              Expect.equal (Query.keptStates session.Record) [ "settled" ] "one branch was kept"
              Expect.equal (Query.killedStates session.Record) [ "alternative" ] "and one was killed, not deleted"

              let run = replayOnto port 2 session

              Expect.equal
                  run.Receipt.Requested
                  [ "settled"; "alternative" ]
                  "the campaign asked about both — the killed branch is the counterfactual"

              Expect.equal (run.Receipt.Landed |> List.map fst) [ "settled"; "alternative" ] "and both landed"
              Expect.isEmpty run.Receipt.Refused "with nothing refused"
              Expect.isTrue (ReplayReceipt.isTotal run.Receipt) "the account is total"

              // The identity rule, on real records: each replayed key differs from the fit it
              // replays in the dataset version and nothing else.
              for state, key in run.Receipt.Landed do
                  let baseline =
                      Query.tryState state session.Record
                      |> Option.bind ReplayCampaign.baseline
                      |> Option.get

                  Expect.isTrue
                      (ReplayCampaign.isReplayOf baseline.Key key)
                      (state + "'s replayed fit is the same fit asked of different data")

                  Expect.equal key.DatasetVersion "quarterly-panel@2" "on the new vintage"
                  Expect.notEqual key.DatasetVersion baseline.Key.DatasetVersion "which the baseline was not"

              // The evidence joined the branches beside the originals.
              for state in [ "settled"; "alternative" ] do
                  Expect.equal
                      ((Map.find state run.Session.Record.States).Outcomes |> List.length)
                      2
                      (state + " now carries the original fit and its replay")

              Expect.equal
                  (Query.replays run.Session.Record |> List.map _.ReplayId)
                  [ "replay-v2" ]
                  "one campaign, recorded"
          }

          test "after one refresh the record accumulates and NOTHING is raised" {
              let port = executor ()
              let run = explore port |> replayOnto port 2

              let record = Vintage.record thresholds theDecision run.Session.Record

              Expect.equal
                  record.Baseline
                  (Some "quarterly-panel@1")
                  "the baseline is the vintage the decision was taken on"

              Expect.equal
                  (record.Comparisons |> List.filter _.OutOfTime |> List.map _.DatasetVersion)
                  [ "quarterly-panel@2" ]
                  "one out-of-time vintage so far"

              Expect.isTrue
                  (record.Comparisons |> List.forall (fun c -> c.SetAsideLead > 0.0))
                  "on which the branch that was set aside is reading ahead — the evidence IS accumulating"

              Expect.isEmpty
                  (Vintage.corrections thresholds run.Session.Record)
                  "and nothing whatever is raised: one refresh is one refresh"
          }

          test "a third vintage crosses the threshold and raises exactly one question, for review" {
              let port = executor ()

              let run =
                  explore port |> replayOnto port 2 |> (fun r -> replayOnto port 3 r.Session)

              Expect.equal
                  (Query.replays run.Session.Record |> List.map _.DatasetVersion)
                  [ "quarterly-panel@2"; "quarterly-panel@3" ]
                  "two campaigns, both accounted for"

              for state in [ "settled"; "alternative" ] do
                  Expect.equal
                      ((Map.find state run.Session.Record.States).Outcomes |> List.length)
                      3
                      (state + " carries one fit per vintage")

              match Vintage.corrections thresholds run.Session.Record with
              | [ finding ] ->
                  Expect.equal finding.Decision theDecision "about the decision that was taken"

                  Expect.equal
                      finding.LeadingVintages
                      [ "quarterly-panel@2"; "quarterly-panel@3" ]
                      "on the out-of-time vintages, and not on the one the curation was made on"

                  Expect.isGreaterThan finding.SmallestLead thresholds.MinMargin "clearing the declared margin"
                  Expect.equal finding.Thresholds thresholds "and carrying the gate it cleared"

                  let line = Vintage.describeCorrection finding
                  Expect.stringContains line "raised for review" "the finding is a question"

                  Expect.stringContains
                      line
                      "No curation has been changed"
                      "and says so — the human gate is structural, not procedural"
              | other -> failtestf "expected exactly one finding, got %d" (List.length other)

              // The curation itself is untouched. Nothing in this library re-curates, and the
              // record says what it always said.
              Expect.equal (Query.keptStates run.Session.Record) [ "settled" ] "the kept branch is still kept"
              Expect.equal (Query.killedStates run.Session.Record) [ "alternative" ] "and the killed one still killed"

              Expect.equal
                  ((Query.tryState "alternative" run.Session.Record |> Option.get).Curations
                   |> List.length)
                  1
                  "one curation act, unchanged — the finding is a question, not an edit"
          }

          test "replaying onto a vintage already recorded refuses per branch, and completes" {
              // The duplicate-outcome guard doing its job. This is why a replay carries no
              // evidence of its own: every outcome enters by the ordinary annotate path, so
              // running a campaign twice cannot double an evidence base — and the second run
              // still produces an honest, total account of what happened to each branch.
              let port = executor ()
              let first = explore port |> replayOnto port 2
              let second = replayWith port "replay-v2-again" 2 first.Session

              Expect.isEmpty second.Receipt.Landed "nothing landed the second time"

              Expect.equal (second.Receipt.Refused |> List.map fst) [ "settled"; "alternative" ] "both were refused"

              for _, refusal in second.Receipt.Refused do
                  match refusal with
                  | NotRecorded(DuplicateOutcome _) -> ()
                  | other -> failtestf "expected a duplicate-outcome refusal, got %s" (ReplayRefusal.describe other)

              Expect.isTrue (ReplayReceipt.isTotal second.Receipt) "and the account is still total"

              for state in [ "settled"; "alternative" ] do
                  Expect.equal
                      ((Map.find state second.Session.Record.States).Outcomes |> List.length)
                      2
                      (state + "'s evidence base was not doubled")

              Expect.equal (Query.replays second.Session.Record |> List.length) 2 "both campaigns are on the record"
          }

          test "a campaign completes PARTIALLY — one branch refused, the other landed, both accounted for" {
              // The property the receipt exists for. One branch was already fitted on this
              // vintage by an ordinary submission, so its replay is refused by the
              // duplicate-outcome guard; the other has nothing there and lands. Abandoning
              // the run because one item could not proceed would throw away evidence already
              // paid for, and would make the outcome depend on which item happened to fail.
              let port = executor ()
              let session = explore port

              let session =
                  match
                      Bridge.submitFit
                          Sample.hashFn
                          bridgeConfig
                          port
                          policy
                          "curator"
                          Sample.curator
                          (submission "settled" settledForm 2)
                          session
                  with
                  | Ok(_, _, s) -> s
                  | Error failure -> failtestf "the individual fit did not land: %A" failure

              let run = replayWith port "replay-partial" 2 session

              Expect.equal (run.Receipt.Landed |> List.map fst) [ "alternative" ] "the branch with room landed"
              Expect.equal (run.Receipt.Refused |> List.map fst) [ "settled" ] "and the one already fitted was refused"
              Expect.isTrue (ReplayReceipt.isTotal run.Receipt) "with both accounted for"

              match run.Receipt.Refused with
              | [ _, NotRecorded(DuplicateOutcome(state, key)) ] ->
                  Expect.equal state "settled" "named"
                  Expect.equal key.DatasetVersion "quarterly-panel@2" "on the vintage it was already fitted at"
              | other -> failtestf "expected a duplicate-outcome refusal, got %A" other
          }

          test "a plan the port cannot be asked about at all is a whole-campaign failure" {
              // Refusing ITEMS within a batch is data on the receipt; refusing the batch is
              // the executor declining to take the work, and the two are not the same answer.
              let port = executor ()
              let session = explore port

              let empty =
                  ReplayCampaign.plan
                      "replay-empty"
                      (NamedBranches [])
                      "quarterly-panel@2"
                      gates
                      SubmitScheduled
                      session.Record

              Expect.isEmpty empty.Items "the plan asks about nothing"

              match
                  Bridge.replayOnto
                      Sample.hashFn
                      bridgeConfig
                      port
                      policy
                      "curator"
                      Sample.curator
                      "curator"
                      instant
                      empty
                      session
              with
              | Ok _ -> failtest "a receipt was produced for a campaign that submitted nothing"
              | Error(ReplayBatchRefused refusal) ->
                  Expect.equal (Refusal.wireClass refusal) "invalidSubmission" "refused as an invalid submission"
              | Error other -> failtestf "expected a batch refusal, got %A" other
          }

          test "a baseline the wire cannot carry is named, and nothing is submitted" {
              // The seed is part of the composite key, so a seed the wire cannot express is a
              // fit nobody can re-address. Refused by branch name before any submission,
              // rather than silently re-keyed.
              let port = executor ()

              let session =
                  match
                      Session.submitAll
                          Sample.hashFn
                          Sample.curator
                          [ Seed
                                { StateId = "settled"
                                  Label = "the settled form"
                                  Payload = settledForm }
                            Annotate
                                { StateId = "settled"
                                  Snapshot =
                                    { Key =
                                        { SpecHash = settledForm.SpecHash
                                          DatasetVersion = "quarterly-panel@1"
                                          Seed = "not-a-number"
                                          ProviderId = "reference-linear"
                                          ProviderVersion = "1.4.0" }
                                      ArtifactRef = ""
                                      Diagnostics = Map.empty
                                      DiagnosticUnits = Map.empty
                                      Gates = []
                                      SubmittedBy = SubmitHuman
                                      RecordedAt = instant } } ]
                          Session.empty
                  with
                  | Ok s -> s
                  | Error(i, r) -> failtestf "op %d rejected: %s" i (Rejection.describe r)

              let plan =
                  ReplayCampaign.plan
                      "replay-bad-seed"
                      EveryFittedBranch
                      "quarterly-panel@2"
                      gates
                      SubmitScheduled
                      session.Record

              match
                  Bridge.replayOnto
                      Sample.hashFn
                      bridgeConfig
                      port
                      policy
                      "curator"
                      Sample.curator
                      "curator"
                      instant
                      plan
                      session
              with
              | Ok _ -> failtest "a submission was built from a seed the wire cannot carry"
              | Error(ReplayUnsubmittable(state, detail)) ->
                  Expect.equal state "settled" "the branch is named"
                  Expect.stringContains detail "not-a-number" "and so is what could not be carried"
              | Error other -> failtestf "expected an unsubmittable refusal, got %A" other
          }

          test "a subject that may not record a campaign never reaches the executor" {
              // Both gates are checked before anything is submitted, and this is the second
              // one. A subject permitted to spend compute but not to record a replay would
              // otherwise annotate branches and then fail to write the account of why they are
              // there — evidence with no provenance.
              let port = executor ()
              let session = explore port

              let mayFitOnly =
                  { Policy.empty with
                      Objectives = [ Policy.objective "fits" "fit submission only" "explorer" [ MaySubmitFit ] ] }

              let plan =
                  ReplayCampaign.plan
                      "replay-v2"
                      EveryFittedBranch
                      "quarterly-panel@2"
                      gates
                      SubmitScheduled
                      session.Record

              match
                  Bridge.replayOnto
                      Sample.hashFn
                      bridgeConfig
                      port
                      mayFitOnly
                      "explorer"
                      Sample.explorer
                      "explorer"
                      instant
                      plan
                      session
              with
              | Ok _ -> failtest "a subject with no replay grant ran a campaign"
              | Error(ReplayPolicyBlocked refusal) ->
                  Expect.equal refusal.Attempted "emit:replay" "the refusal names what was attempted"
                  Expect.contains refusal.Allowed "submitFit" "and enumerates what the subject may do"
              | Error other -> failtestf "expected a policy refusal, got %A" other

              for state in [ "settled"; "alternative" ] do
                  Expect.equal
                      ((Map.find state session.Record.States).Outcomes |> List.length)
                      1
                      (state + " was not fitted again — nothing was spent")
          }

          test "the whole run is deterministic, byte for byte" {
              let run () =
                  let port = executor ()
                  let s = explore port |> replayOnto port 2 |> (fun r -> replayOnto port 3 r.Session)
                  Session.toJsonl s.Session

              Expect.equal (run ()) (run ()) "the same script yields the same history"
          } ]
