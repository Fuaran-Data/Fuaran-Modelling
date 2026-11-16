// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ExplorationScenarioTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The end-to-end scripted exploration: seed, fork, fit each branch through a
//  backend, curate the results, return to an ancestor and fork again, then
//  replay a single line.
//
//  This is the whole spine exercised as one story, which is the only way to
//  find out whether the pieces actually compose. Each smaller suite proves a
//  part in isolation; this one proves they add up.
// ============================================================================

let private recordedAt = "2026-01-01T00:00:00Z"
let private backend = ReferenceBackend.create "reference" "1" recordedAt
let private policy = fullPolicy "curator"

let private gates =
    [ { Name = "convergence"
        Threshold = 0.0
        Unit = None
        Direction = AtLeast } ]

let private submissionFor (stateId: StateId) (s: Session) : FitSubmission =
    let st = Query.tryState stateId s.Record |> Option.get

    { StateId = stateId
      Payload = st.Payload
      DatasetVersion = "ds-2026-01"
      Seed = "7"
      Gates = gates
      SubmittedBy = SubmitHuman }

/// Run the scripted exploration. Returns the session and the two outcomes.
let private runScenario () : Session * OutcomeSnapshot * OutcomeSnapshot =
    let step op s =
        match Session.submitGoverned hashFn policy "curator" curator op s with
        | Ok(_, s') -> s'
        | Error e -> failwithf "op %A did not land: %A" (Op.kindName op) e

    let fit stateId s =
        match Session.submitFit hashFn policy "curator" curator backend (submissionFor stateId s) s with
        | Ok(outcome, _, s') -> outcome, s'
        | Error e -> failwithf "fit on %s did not land: %A" stateId e

    // Seed the exploration, then fork two rival branches off it.
    let s =
        Session.empty
        |> step (seedOp "base")
        |> step (forkOp "left" "base")
        |> step (forkOp "right" "base")

    // Fit each branch, recording what came back INTO the branch record.
    let leftOutcome, s = fit "left" s
    let rightOutcome, s = fit "right" s

    // Curate: one kept, one killed — with reasons, which is the part that teaches.
    let s =
        s
        |> step (curateOp "left" Keep "clean decomposition, gates held")
        |> step (curateOp "right" Kill "unstable under the holdout window")

    // Return to the root and fork again — abandonment costs nothing.
    let s = s |> step (ReturnTo { To = "base" }) |> step (forkOp "third" "base")

    s, leftOutcome, rightOutcome

[<Tests>]
let tests =
    testList
        "ExplorationScenario"
        [ test "the scripted exploration folds, records and replays end to end" {
              let s, leftOutcome, rightOutcome = runScenario ()

              // The working record holds everything that was tried, abandoned included.
              Expect.equal s.Record.Order [ "base"; "left"; "right"; "third" ] "every state tried is retained"
              Expect.equal s.Record.Head (Some "third") "the head is where work resumed"

              // Outcomes are IN the record — the evidence base needs no live backend.
              let left = Query.tryState "left" s.Record |> Option.get
              let right = Query.tryState "right" s.Record |> Option.get
              Expect.equal left.Outcomes [ leftOutcome ] "the left branch carries its own outcome"
              Expect.equal right.Outcomes [ rightOutcome ] "and so does the right"
              Expect.notEqual leftOutcome.Key.SpecHash rightOutcome.Key.SpecHash "two branches, two specs"

              // Curation, with reasons, is first-class.
              Expect.equal (Query.currentVerdict left) (Some Keep) "left kept"
              Expect.equal (Query.currentVerdict right) (Some Kill) "right killed"

              Expect.stringContains
                  (List.head right.Curations).Reason
                  "holdout"
                  "and the reason for killing it is what survives"

              // The abandoned branch is still there, in full.
              Expect.isTrue (Map.containsKey "right" s.Record.States) "a killed branch is not deleted"
              Expect.equal (List.length right.Outcomes) 1 "its evidence is still readable"
          }

          test "replaying one line yields that line alone, with its own outcome and curation" {
              let s, leftOutcome, _ = runScenario ()

              match Session.replayState "left" s with
              | Error m -> failtestf "replay failed: %s" m
              | Ok line ->
                  Expect.equal line.Order [ "base"; "left" ] "the left line knows nothing of its rivals"
                  let l = Query.tryState "left" line |> Option.get
                  Expect.equal l.Outcomes [ leftOutcome ] "carrying its outcome"
                  Expect.equal (Query.currentVerdict l) (Some Keep) "and its verdict"
                  Expect.isFalse (Map.containsKey "right" line.States) "and nothing of the killed branch"
          }

          test "the whole exploration round-trips through persistence, line for line" {
              let s, _, _ = runScenario ()
              let jsonl = Session.toJsonl s

              match Session.ofJsonl hashFn jsonl with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  Expect.isTrue (Session.verify hashFn back) "the reloaded history verifies"

                  for id in [ "base"; "left"; "right"; "third" ] do
                      match Session.replayState id s, Session.replayState id back with
                      | Ok live, Ok reloaded -> Expect.equal reloaded live ("the " + id + " line survives")
                      | x, y -> failtestf "replay of %s failed: %A / %A" id x y

                  Expect.equal (Session.toJsonl back) jsonl "and re-serialises byte-identically"
          }

          test "running the whole scenario twice produces byte-identical history" {
              let s1, _, _ = runScenario ()
              let s2, _, _ = runScenario ()

              Expect.equal
                  (Session.toJsonl s2)
                  (Session.toJsonl s1)
                  "the spine is deterministic: no clock, no id generator, no ambient state"
          }

          test "an automated explorer may propose but not fit, and a human resolves" {
              let explorerPolicy =
                  { Policy.empty with
                      Objectives =
                          [ Policy.objective "supervised" "propose only" "explorer" Policy.proposeOnlyCapabilities
                            Policy.objective
                                "review"
                                "human review"
                                "curator"
                                [ MayEmit ResolveKind; MayEmit SeedKind; MaySubmitFit ] ] }

              let seeded =
                  match
                      Session.submitGoverned hashFn explorerPolicy "curator" curator (seedOp "base") Session.empty
                  with
                  | Ok(_, s) -> s
                  | Error e -> failwithf "seed did not land: %A" e

              // The explorer may not fork directly...
              match Session.submitGoverned hashFn explorerPolicy "explorer" explorer (forkOp "x" "base") seeded with
              | Error(PolicyBlocked r) -> Expect.contains r.Allowed "emit:propose" "but it is told what it may do"
              | other -> failtestf "expected the fork to be blocked, got %A" other

              // ...nor spend compute.
              let submission =
                  { StateId = "base"
                    Payload = payload "{\"v\":0}" "h-base"
                    DatasetVersion = "ds-2026-01"
                    Seed = "7"
                    Gates = gates
                    SubmittedBy = SubmitAgent }

              match Session.submitFit hashFn explorerPolicy "explorer" explorer backend submission seeded with
              | Error(FitPolicyBlocked _) -> ()
              | other -> failtestf "expected the fit to be blocked, got %A" other

              // ...but it may propose, and the human's acceptance materialises exactly that.
              let proposal =
                  { ProposalId = "p1"
                    From = "base"
                    ProposedId = "x"
                    Label = "suggested branch"
                    Payload = payload "{\"v\":\"x\"}" "h-x"
                    Rationale = "the control looks collinear; try it without"
                    ProposedBy = "explorer" }

              let proposed =
                  match
                      Session.submitGoverned
                          hashFn
                          explorerPolicy
                          "explorer"
                          explorer
                          (Propose { Proposal = proposal })
                          seeded
                  with
                  | Ok(_, s) -> s
                  | Error e -> failwithf "the proposal did not land: %A" e

              Expect.isFalse (Map.containsKey "x" proposed.Record.States) "proposing materialises nothing"

              let resolved =
                  match
                      Session.submitGoverned
                          hashFn
                          explorerPolicy
                          "curator"
                          curator
                          (Resolve
                              { ProposalId = "p1"
                                Outcome = Accepted
                                ResolvedBy = "curator" })
                          proposed
                  with
                  | Ok(_, s) -> s
                  | Error e -> failwithf "the resolution did not land: %A" e

              let x = Query.tryState "x" resolved.Record |> Option.get
              Expect.equal x.Payload.SpecHash "h-x" "what was reviewed is what landed"
              Expect.equal x.Origin (FromProposal("p1", "base")) "and the record says who suggested it"
          } ]
