// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ReducerTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

[<Tests>]
let tests =
    testList
        "Reducer"
        [ test "an exploration must be seeded before anything else can happen" {
              match Reducer.apply (forkOp "b" "a") Exploration.empty with
              | Error NotSeeded -> ()
              | other -> failtestf "expected NotSeeded, got %A" other
          }

          test "a second seed is refused and names the existing root" {
              let e = foldOrFail [ seedOp "a" ]

              match Reducer.apply (seedOp "z") e with
              | Error(AlreadySeeded "a") -> ()
              | other -> failtestf "expected AlreadySeeded 'a', got %A" other
          }

          test "forking is additive — the parent is untouched and both states survive" {
              let e = foldOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a" ]
              Expect.equal e.Order [ "a"; "b"; "c" ] "exploration order is insertion order"
              Expect.equal (Query.children "a" e) [ "b"; "c" ] "both forks hang off the root"

              let parent = Query.tryState "a" e |> Option.get
              Expect.isEmpty parent.Curations "forking curates nothing"
              Expect.equal parent.Payload.Body "{\"v\":0}" "the parent payload is unchanged"
          }

          test "state ids are permanent — reusing one is refused" {
              let e = foldOrFail [ seedOp "a"; forkOp "b" "a" ]

              match Reducer.apply (forkOp "b" "a") e with
              | Error(DuplicateState "b") -> ()
              | other -> failtestf "expected DuplicateState, got %A" other
          }

          test "forking from an unknown state names the unknown state" {
              let e = foldOrFail [ seedOp "a" ]

              match Reducer.apply (forkOp "c" "nowhere") e with
              | Error(UnknownState "nowhere") -> ()
              | other -> failtestf "expected UnknownState, got %A" other
          }

          test "kill is a verdict, not a deletion — the state stays, with its reason" {
              let e =
                  foldOrFail [ seedOp "a"; forkOp "b" "a"; curateOp "b" Kill "collinear with the baseline" ]

              let killed = Query.tryState "b" e |> Option.get
              Expect.isTrue (Query.isKilled killed) "b is killed"
              Expect.equal (List.length killed.Curations) 1 "the curation is recorded"
              Expect.stringContains killed.Curations.Head.Reason "collinear" "the reason survives"
              Expect.isTrue (Map.containsKey "b" e.States) "the state itself is still there"
          }

          test "a killed branch cannot be extended until it is revived" {
              let e = foldOrFail [ seedOp "a"; forkOp "b" "a"; curateOp "b" Kill "abandoned" ]

              match Reducer.apply (forkOp "d" "b") e with
              | Error(KilledState "b") -> ()
              | other -> failtestf "expected KilledState, got %A" other

              // Reviving is a deliberate act, and then the fork lands.
              let revived =
                  foldOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        curateOp "b" Kill "abandoned"
                        curateOp "b" Keep "worth another look" ]

              Expect.isFalse (Query.isKilled (Query.tryState "b" revived |> Option.get)) "b is live again"

              match Reducer.apply (forkOp "d" "b") revived with
              | Ok e2 -> Expect.isTrue (Map.containsKey "d" e2.States) "the fork lands on the revived branch"
              | Error r -> failtestf "expected the fork to land, got %s" (Rejection.describe r)
          }

          test "curation is a log — a kill followed by a keep records both acts in order" {
              let e =
                  foldOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        curateOp "b" Kill "looked wrong"
                        curateOp "b" Keep "was right after all" ]

              let s = Query.tryState "b" e |> Option.get
              Expect.equal (s.Curations |> List.map (fun c -> c.Verdict)) [ Kill; Keep ] "both acts, in order"
              Expect.equal (Query.currentVerdict s) (Some Keep) "the current verdict is the latest"
          }

          test "return-to-ancestor moves the head and destroys nothing" {
              let e = foldOrFail [ seedOp "a"; forkOp "b" "a"; ReturnTo { To = "a" } ]
              Expect.equal e.Head (Some "a") "the head moved back"
              Expect.equal e.Order [ "a"; "b" ] "no state was lost"
          }

          test "cherry-pick lands a source payload as a new child of the target" {
              let e =
                  foldOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        forkOp "c" "a"
                        CherryPick
                            { StateId = "d"
                              Source = "b"
                              Onto = "c"
                              Label = "b's idea on c"
                              Payload = payload "{\"v\":\"b\"}" "h-b" } ]

              let d = Query.tryState "d" e |> Option.get
              Expect.equal d.Parent (Some "c") "the new state descends from the target"
              Expect.equal d.Origin (CherryPickedFrom("b", "c")) "provenance names both sides"
              Expect.equal (Query.path "d" e) [ "a"; "c"; "d" ] "lineage follows the target, not the source"
          }

          test "the same fit cannot be annotated onto one state twice" {
              let e = foldOrFail [ seedOp "a" ]

              let ann =
                  Annotate
                      { StateId = "a"
                        Snapshot = outcome "h-a" }

              let e1 = Reducer.apply ann e |> Result.defaultWith (fun _ -> e)

              match Reducer.apply ann e1 with
              | Error(DuplicateOutcome("a", key)) -> Expect.equal key.SpecHash "h-a" "the refusal names the key"
              | other -> failtestf "expected DuplicateOutcome, got %A" other
          }

          test "outcomes accumulate on the state and are readable offline" {
              let e = foldOrFail [ seedOp "a" ]

              let e =
                  Reducer.apply
                      (Annotate
                          { StateId = "a"
                            Snapshot = outcome "h-a" })
                      e
                  |> Result.defaultWith (fun _ -> e)

              Expect.equal (Query.outcomes e |> List.length) 1 "one recorded outcome"
              let _, o = (Query.outcomes e).Head
              Expect.isTrue (OutcomeSnapshot.gatesPassed o) "its gates held"
              Expect.isEmpty (OutcomeSnapshot.failedGates o) "and none failed"
          }

          test "a proposal is held unapplied until it is resolved" {
              let proposal =
                  { ProposalId = "p1"
                    From = "a"
                    ProposedId = "b"
                    Label = "suggested"
                    Payload = payload "{\"v\":1}" "h-b"
                    Rationale = "worth trying"
                    ProposedBy = "explorer" }

              let e = foldOrFail [ seedOp "a"; Propose { Proposal = proposal } ]
              Expect.isFalse (Map.containsKey "b" e.States) "the proposed state does not exist yet"
              Expect.equal (Query.openProposals e |> List.length) 1 "it is open"

              let accepted =
                  foldOrFail
                      [ seedOp "a"
                        Propose { Proposal = proposal }
                        Resolve
                            { ProposalId = "p1"
                              Outcome = Accepted
                              ResolvedBy = "curator" } ]

              let b = Query.tryState "b" accepted |> Option.get
              Expect.equal b.Origin (FromProposal("p1", "a")) "provenance records that it came from a proposal"
              Expect.equal b.Payload.SpecHash "h-b" "what was reviewed is what landed"
              Expect.isEmpty (Query.openProposals accepted) "and it is no longer open"
          }

          test "a declined proposal is recorded and materialises nothing" {
              let proposal =
                  { ProposalId = "p1"
                    From = "a"
                    ProposedId = "b"
                    Label = "suggested"
                    Payload = payload "{\"v\":1}" "h-b"
                    Rationale = "worth trying"
                    ProposedBy = "explorer" }

              let e =
                  foldOrFail
                      [ seedOp "a"
                        Propose { Proposal = proposal }
                        Resolve
                            { ProposalId = "p1"
                              Outcome = Declined "not identifiable"
                              ResolvedBy = "curator" } ]

              Expect.isFalse (Map.containsKey "b" e.States) "nothing was materialised"
              Expect.equal (Map.tryFind "p1" e.Resolutions) (Some(Declined "not identifiable")) "the decision is kept"
          }

          test "a proposal cannot be resolved twice" {
              let proposal =
                  { ProposalId = "p1"
                    From = "a"
                    ProposedId = "b"
                    Label = "suggested"
                    Payload = payload "{\"v\":1}" "h-b"
                    Rationale = "worth trying"
                    ProposedBy = "explorer" }

              let resolve =
                  Resolve
                      { ProposalId = "p1"
                        Outcome = Accepted
                        ResolvedBy = "curator" }

              let e = foldOrFail [ seedOp "a"; Propose { Proposal = proposal }; resolve ]

              match Reducer.apply resolve e with
              | Error(ProposalAlreadyResolved "p1") -> ()
              | other -> failtestf "expected ProposalAlreadyResolved, got %A" other
          }

          test "applyAll names the index of the op that failed" {
              match Reducer.applyAll [ seedOp "a"; forkOp "b" "a"; forkOp "c" "nowhere" ] Exploration.empty with
              | Error(2, UnknownState "nowhere") -> ()
              | other -> failtestf "expected a failure at index 2, got %A" other
          }

          test "leaves are the exploration frontier" {
              let e = foldOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a"; forkOp "d" "b" ]
              Expect.equal (Query.leaves e) [ "c"; "d" ] "a and b have children; c and d do not"
          } ]
