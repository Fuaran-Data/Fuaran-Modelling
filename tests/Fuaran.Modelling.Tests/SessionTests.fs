// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.SessionTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

[<Tests>]
let tests =
    testList
        "Session"
        [ test "two forks off one state fork the history, not just the record" {
              let s = sessionOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a" ]
              Expect.equal (Session.heads s |> List.length) 2 "two lines, two heads"

              let nodeB = Session.headOf "b" s |> Option.get
              let nodeC = Session.headOf "c" s |> Option.get
              Expect.notEqual nodeB nodeC "the two forks are distinct nodes"

              let parents id = s.Dag.Nodes.[id].Parents
              Expect.equal (parents nodeB) (parents nodeC) "and they share one parent"
          }

          test "replaying to a line's head yields that line, not its siblings" {
              let s =
                  sessionOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a"; curateOp "c" Kill "abandoned" ]

              match Session.replayState "b" s, Session.replayState "c" s with
              | Ok onB, Ok onC ->
                  Expect.equal onB.Order [ "a"; "b" ] "the b line knows nothing of c"
                  Expect.equal onC.Order [ "a"; "c" ] "and the c line nothing of b"
                  Expect.isTrue (Query.isKilled (Query.tryState "c" onC |> Option.get)) "c's curation is on c's line"
              | a, b -> failtestf "replay failed: %A / %A" a b
          }

          test "the working record is the union of every line" {
              let s = sessionOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a" ]
              Expect.equal s.Record.Order [ "a"; "b"; "c" ] "everything tried is in the working view"
          }

          test "a refused op leaves no trace in the history" {
              let s = sessionOrFail [ seedOp "a"; forkOp "b" "a" ]
              let before = Map.count s.Dag.Nodes

              match Session.submit hashFn curator (forkOp "b" "a") s with
              | Error(DuplicateState "b") -> ()
              | other -> failtestf "expected DuplicateState, got %A" other

              Expect.equal (Map.count s.Dag.Nodes) before "the session is unchanged — submit is pure"
          }

          test "every node's content id verifies" {
              let s =
                  sessionOrFail [ seedOp "a"; forkOp "b" "a"; forkOp "c" "a"; curateOp "b" Keep "promising" ]

              Expect.isTrue (Session.verify hashFn s) "the history is intact"
          }

          test "a tampered node is detected on load" {
              let s = sessionOrFail [ seedOp "a"; forkOp "b" "a" ]
              let tampered = (Session.toJsonl s).Replace("branch b", "branch tampered")

              match Session.ofJsonl hashFn tampered with
              | Error m -> Expect.stringContains m "content-id mismatch" "the load names the fault"
              | Ok _ -> failtest "a tampered history loaded cleanly"
          }

          test "history survives a JSONL round-trip, and every line replays identically" {
              let s =
                  sessionOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        forkOp "c" "a"
                        curateOp "c" Kill "abandoned"
                        Annotate
                            { StateId = "b"
                              Snapshot = outcome "h-b" } ]

              match Session.ofJsonl hashFn (Session.toJsonl s) with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  Expect.equal (Map.count back.Dag.Nodes) (Map.count s.Dag.Nodes) "same node count"

                  for id in [ "a"; "b"; "c" ] do
                      match Session.replayState id s, Session.replayState id back with
                      | Ok live, Ok reloaded -> Expect.equal reloaded live ("the " + id + " line is identical")
                      | x, y -> failtestf "replay of %s failed: %A / %A" id x y

                  Expect.equal (Session.toJsonl back) (Session.toJsonl s) "and re-serialises byte-identically"
          }

          test "identical histories converge on identical content ids" {
              let ops = [ seedOp "a"; forkOp "b" "a" ]
              let s1 = sessionOrFail ops
              let s2 = sessionOrFail ops
              Expect.equal (Session.toJsonl s1) (Session.toJsonl s2) "content addressing converges"
          }

          test "a different actor is a different history — attribution is in the content id" {
              let ops = [ seedOp "a" ]
              let s1 = sessionOrFail ops

              let s2 =
                  match Session.submitAll hashFn explorer ops Session.empty with
                  | Ok s -> s
                  | Error(i, r) -> failwithf "op %d rejected: %s" i (Rejection.describe r)

              Expect.notEqual (Session.heads s1) (Session.heads s2) "who acted is part of what happened"
          }

          test "the policy gate runs before the fold — an unauthorised op never lands" {
              let s = sessionOrFail [ seedOp "a" ]
              let before = Map.count s.Dag.Nodes

              match Session.submitGoverned hashFn Policy.empty "explorer" explorer (forkOp "b" "a") s with
              | Error(PolicyBlocked r) ->
                  Expect.equal r.Attempted "emit:fork" "the refusal names the op kind"
                  Expect.equal (Map.count s.Dag.Nodes) before "and nothing was written"
              | other -> failtestf "expected PolicyBlocked, got %A" other
          }

          test "a governed op lands once the subject is granted it" {
              let policy = fullPolicy "curator"
              let s = sessionOrFail [ seedOp "a" ]

              match Session.submitGoverned hashFn policy "curator" curator (forkOp "b" "a") s with
              | Ok(_, s') -> Expect.isTrue (Map.containsKey "b" s'.Record.States) "the fork landed"
              | Error e -> failtestf "expected the op to land, got %A" e
          }

          test "a governed op that the fold refuses reports the fold's reason, not the policy's" {
              let policy = fullPolicy "curator"
              let s = sessionOrFail [ seedOp "a"; forkOp "b" "a" ]

              match Session.submitGoverned hashFn policy "curator" curator (forkOp "b" "a") s with
              | Error(FoldRejected(DuplicateState "b")) -> ()
              | other -> failtestf "expected FoldRejected, got %A" other
          }

          test "replaying an unknown state names it rather than returning an empty record" {
              let s = sessionOrFail [ seedOp "a" ]

              match Session.replayState "nowhere" s with
              | Error m -> Expect.stringContains m "nowhere" "the error names the state"
              | Ok _ -> failtest "an unknown state replayed to something"
          }

          test "a cherry-pick joins both lines, and the landed payload is the source's" {
              let s =
                  sessionOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        forkOp "c" "a"
                        CherryPick
                            { StateId = "d"
                              Source = "b"
                              Onto = "c"
                              Label = "b's idea on c"
                              Payload = payload "{\"v\":\"b\"}" "h-b" } ]

              match Session.replayState "d" s with
              | Ok line ->
                  Expect.equal
                      (Set.ofList line.Order)
                      (Set.ofList [ "a"; "b"; "c"; "d" ])
                      "the merge brings the source branch into the line's closure"

                  Expect.equal (List.last line.Order) "d" "and the cherry-pick is its head"
                  Expect.equal (Query.path "d" line) [ "a"; "c"; "d" ] "lineage still follows the target"

                  Expect.equal
                      (Query.tryState "d" line |> Option.get).Payload.SpecHash
                      "h-b"
                      "carrying the source payload"
              | Error m -> failtestf "replay failed: %s" m
          } ]
