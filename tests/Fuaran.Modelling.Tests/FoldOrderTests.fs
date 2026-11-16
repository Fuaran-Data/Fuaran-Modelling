// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.FoldOrderTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  `Session.ofJsonl` must re-fold a stored history the same way every time, and
//  on every history — not on the ones a content hash happens to favour.
//
//  The bug these tests pin (D34) was that the global fold order was assembled
//  head-major from `Dag.heads`, which is ID-SORTED — so which of two genuinely
//  concurrent nodes folded first was decided by a hash over content that
//  includes caller-chosen state ids. Both failure faces are here with the ids
//  that force them, so each test fails on EVERY run of the old code rather
//  than on roughly half:
//
//    * the state ids below are literals, not generated, so the content ids they
//      hash to are fixed and the losing order is the one that is taken;
//    * the sweeps that follow use random ids to show the fix is not a fixture
//      that happens to sort correctly.
//
//  This file sits after `CorpusFixtures.fs` in compile order only to reuse its
//  `repoRoot` walk for the persisted fixture below.
// ============================================================================

let private replayOp (rid: string) (vintage: string) (requested: StateId list) : ExplorationOp =
    Replay
        { Receipt =
            { ReplayId = rid
              DatasetVersion = vintage
              Requested = requested
              Landed =
                requested
                |> List.map (fun s ->
                    s,
                    { compositeKey ("h-" + s) with
                        DatasetVersion = vintage })
              Refused = []
              ReplayedBy = "curator"
              ReplayedAt = "2026-01-01T00:00:00Z" } }

let private annotateOp (id: StateId) : ExplorationOp =
    Annotate
        { StateId = id
          Snapshot = outcome ("h-" + id) }

let private reloadOrFail (label: string) (s: Session) : Session =
    match Session.ofJsonl hashFn (Session.toJsonl s) with
    | Ok back -> back
    | Error m -> failtestf "%s: the stored history did not re-fold: %s" label m

/// A fresh, unrelated id triple — the consumer's shape, where state ids are minted
/// per run and so land wherever the hash puts them.
let private freshIds () =
    let g () = System.Guid.NewGuid().ToString()
    g (), g (), g ()

/// Every state's parent must appear before it in the folded exploration order. This is
/// the causal law the fold order exists to respect; a tie-break that reorders a
/// dependent pair breaks it, and it is observable without exposing the order itself.
let private expectCausalOrder (label: string) (e: Exploration) =
    let position = e.Order |> List.mapi (fun i id -> id, i) |> Map.ofList

    for st in Query.states e do
        match st.Parent with
        | None -> ()
        | Some p ->
            match Map.tryFind p position, Map.tryFind st.Id position with
            | Some pi, Some si ->
                Expect.isLessThan pi si (label + ": '" + p + "' must fold before its child '" + st.Id + "'")
            | _ -> failtestf "%s: '%s' or its parent '%s' is missing from the folded order" label st.Id p

[<Tests>]
let tests =
    testList
        "Session / fold order"
        [
          // ---- face one: the kill that folded first ---------------------------------
          //
          // `Curate` claims its state's mark; `Fork` does NOT claim its SOURCE's. So a
          // fork off a state and a later kill of that same state both hang off whatever
          // last touched it — siblings, with nothing in the history saying which came
          // first. Fold the kill first and the fork it must precede is refused as
          // `KilledState`, and the whole load fails.
          test "a kill concurrent with the forks off the killed state does not fold first" {
              let s =
                  sessionOrFail
                      [ seedOp "s1-a"
                        forkOp "s1-b" "s1-a"
                        forkOp "s1-c" "s1-a"
                        curateOp "s1-a" Kill "done with the root" ]

              let back = reloadOrFail "kill-vs-fork" s

              Expect.equal
                  (Set.ofList back.Record.Order)
                  (Set.ofList [ "s1-a"; "s1-b"; "s1-c" ])
                  "every branch survived the reload"

              expectCausalOrder "kill-vs-fork" back.Record

              Expect.isTrue
                  (Query.isKilled (Query.tryState "s1-a" back.Record |> Option.get))
                  "and the kill is still recorded — it was reordered, not dropped"
          }

          // ---- face two: the receipt that folded before its branches existed --------
          //
          // A `Replay` receipt is deliberately anchored on the cursor and claims no
          // mark, so the branches its `Requested` names are not its DAG ancestors. Fold
          // it before a branch on another line and the fold reports `UnknownState`.
          test "a replay receipt folds after the branches it names, not before them" {
              let s =
                  sessionOrFail
                      [ seedOp "s1-a"
                        forkOp "s1-b" "s1-a"
                        forkOp "s1-c" "s1-a"
                        replayOp "r1" "ds-2" [ "s1-b"; "s1-c" ] ]

              let back = reloadOrFail "receipt-vs-branch" s

              Expect.equal
                  (Query.replays back.Record |> List.map (fun r -> r.ReplayId))
                  [ "r1" ]
                  "the campaign is on the reloaded record"

              expectCausalOrder "receipt-vs-branch" back.Record
          }

          // ---- the consumer's shape, repeatedly ------------------------------------
          test "the fork -> fit -> curate -> replay shape reloads 25 times out of 25" {
              for i in 1..25 do
                  let a, b, c = freshIds ()

                  let s =
                      sessionOrFail
                          [ seedOp a
                            forkOp b a
                            forkOp c a
                            annotateOp b
                            annotateOp c
                            curateOp b Keep "promising"
                            curateOp c Kill "abandoned"
                            replayOp "r1" "ds-2" [ b; c ]
                            curateOp a Kill "done with the root" ]

                  let back = reloadOrFail (sprintf "iteration %d" i) s

                  Expect.equal
                      (Set.ofList back.Record.Order)
                      (Set.ofList [ a; b; c ])
                      (sprintf "iteration %d: every branch survived" i)

                  expectCausalOrder (sprintf "iteration %d" i) back.Record
          }

          // A fixed-id test proves the fix on the ids that used to lose. This proves it
          // is not a fixture that sorts favourably: over freshly-minted ids the hash
          // lands wherever it lands, and the reload must not care. On the old code both
          // shapes failed for well over half of these.
          test "reload succeeds for every state-id set, not the ones the hash favours" {
              for _ in 1..200 do
                  let a, b, c = freshIds ()

                  let killShape =
                      sessionOrFail [ seedOp a; forkOp b a; forkOp c a; curateOp a Kill "done" ]

                  let receiptShape =
                      sessionOrFail [ seedOp a; forkOp b a; forkOp c a; replayOp "r1" "ds-2" [ b; c ] ]

                  reloadOrFail "kill sweep" killShape |> ignore
                  reloadOrFail "receipt sweep" receiptShape |> ignore
          }

          // ---- determinism, the property the whole thing is for ---------------------
          test "the same bytes fold to the same record every time" {
              let a, b, c = freshIds ()

              let s =
                  sessionOrFail
                      [ seedOp a
                        forkOp b a
                        forkOp c a
                        annotateOp b
                        replayOp "r1" "ds-2" [ b; c ]
                        curateOp a Kill "done" ]

              let bytes = Session.toJsonl s
              let first = reloadOrFail "determinism" s

              for _ in 1..25 do
                  match Session.ofJsonl hashFn bytes with
                  | Error m -> failtestf "a later load of identical bytes failed: %s" m
                  | Ok again ->
                      Expect.equal again.Record first.Record "the same bytes fold to the same record"
                      Expect.equal again.Cursor first.Cursor "and recover the same cursor"
          }

          test "every line replays identically live and reloaded" {
              let s =
                  sessionOrFail
                      [ seedOp "s1-a"
                        forkOp "s1-b" "s1-a"
                        forkOp "s1-c" "s1-a"
                        curateOp "s1-b" Keep "promising"
                        replayOp "r1" "ds-2" [ "s1-b"; "s1-c" ]
                        curateOp "s1-a" Kill "done with the root" ]

              let back = reloadOrFail "line replay" s

              for id in [ "s1-a"; "s1-b"; "s1-c" ] do
                  match Session.replayState id s, Session.replayState id back with
                  | Ok live, Ok reloaded -> Expect.equal reloaded live ("the " + id + " line is identical")
                  | x, y -> failtestf "replay of %s failed: %A / %A" id x y
          }

          // ---- the persisted stream ------------------------------------------------
          //
          // `fixtures/pre-fix-session.jsonl` was written by the build that carried the
          // defect, and the build that carried the defect could not read it back. It is
          // the regression's own witness: the node ids in it were minted by the old
          // anchoring, so a fix that quietly changed the DAG's shape or its content
          // addressing would fail the byte comparison below rather than pass it.
          test "a history persisted before the fix re-folds, and re-serialises byte-identically" {
              let path =
                  System.IO.Path.Combine(
                      CorpusFixtures.repoRoot (),
                      "tests",
                      "Fuaran.Modelling.Tests",
                      "fixtures",
                      "pre-fix-session.jsonl"
                  )

              let stored = System.IO.File.ReadAllText(path).Replace("\r\n", "\n")

              match Session.ofJsonl hashFn stored with
              | Error m -> failtestf "the persisted history did not re-fold: %s" m
              | Ok s ->
                  Expect.equal
                      (Set.ofList s.Record.Order)
                      (Set.ofList [ "s1-a"; "s1-b"; "s1-c" ])
                      "the three branches are all there"

                  expectCausalOrder "persisted fixture" s.Record

                  Expect.isTrue
                      (Query.isKilled (Query.tryState "s1-a" s.Record |> Option.get))
                      "the root's kill is recorded"

                  Expect.equal
                      (Query.currentVerdict (Query.tryState "s1-b" s.Record |> Option.get))
                      (Some Keep)
                      "and b's keep"

                  Expect.equal
                      (Query.replays s.Record |> List.map (fun r -> r.ReplayId))
                      [ "r1" ]
                      "and the campaign receipt"

                  Expect.isTrue (Session.verify hashFn s) "every stored content id still verifies"

                  Expect.equal
                      (Session.toJsonl s)
                      stored
                      "the fix moved no node id and no byte — it changed only the order they fold in"
          } ]
