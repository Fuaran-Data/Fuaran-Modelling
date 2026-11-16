// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.IndistinguishabilityTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The indistinguishability surface, part by part.
//
//  The shared fixture is deliberately awkward in one specific way: the reading
//  CLOSEST to the reference belongs to a branch that was never an alternative to
//  it. A membership rule built on distance alone passes every other test in this
//  file and fails that one, which is why it is in the fixture rather than in a
//  test of its own.
//
//    root                       an alternative, never fitted
//    ├── a   0.40               the reference
//    ├── b   0.42               within a margin of 0.05
//    ├── c   0.55               outside it
//    └── c
//         └── d   0.41          the closest reading, and out of scope
// ============================================================================

let private reading (state: StateId) (v: string) (x: float) : ExplorationOp =
    Annotate
        { StateId = state
          Snapshot =
            { outcome ("h-" + state) with
                Key =
                    { compositeKey ("h-" + state) with
                        DatasetVersion = v }
                Diagnostics = Map.ofList [ "holdoutError", x ]
                Gates = [] } }

let private record () : Exploration =
    foldOrFail
        [ seedOp "root"
          forkOp "a" "root"
          forkOp "b" "root"
          forkOp "c" "root"
          forkOp "d" "c"
          reading "a" "ds-1" 0.40
          reading "b" "ds-1" 0.42
          reading "c" "ds-1" 0.55
          reading "d" "ds-1" 0.41 ]

/// A declaration over the shared fixture, failing loudly if the gate refuses one the test
/// meant to be well-formed.
let private declaring (metric: string) (margin: float) (scope: IndistinguishabilityScope) =
    match Indistinguishability.declare (record ()) metric margin scope with
    | Ok d -> Some d
    | Error r -> failwithf "the declaration was refused: %s" (IndistinguishabilityRefusal.describe r)

let private setOrFail (d: IndistinguishabilityDeclaration option) (reference: StateId) (e: Exploration) =
    match Indistinguishability.report d reference e with
    | Derived s -> s
    | other -> failwithf "expected a derived set, got %A" other

[<Tests>]
let tests =
    testList
        "Indistinguishability"
        [ test "the metric census reports what this record measured, and nothing it did not" {
              Expect.equal (Vintage.metrics (record ())) [ "holdoutError" ] "one metric, because one was measured"

              Expect.isEmpty
                  (Vintage.metrics (foldOrFail [ seedOp "root" ]))
                  "and a record with no fits has measured nothing — the honest answer, not a vocabulary"
          }

          test "a declaration with no metric is refused, and the refusal enumerates what is declarable" {
              match Indistinguishability.declare (record ()) "  " 0.05 EveryAlternative with
              | Error(UndeclaredMetric declarable) ->
                  Expect.equal declarable [ "holdoutError" ] "naming what this record can answer about"
              | other -> failtestf "expected an undeclared-metric refusal, got %A" other
          }

          test "a metric nothing measured is refused rather than answered with a set of one" {
              // The sharp case. Answered, it derives the reference alone — which is exactly
              // the shape a genuine "nothing else is close" finding takes, so the mistake is
              // invisible at the point it matters.
              match Indistinguishability.declare (record ()) "aicc" 0.05 EveryAlternative with
              | Error(UnmeasuredMetric(metric, declarable)) ->
                  Expect.equal metric "aicc" "naming what was attempted"
                  Expect.equal declarable [ "holdoutError" ] "and what exists instead"
              | other -> failtestf "expected an unmeasured-metric refusal, got %A" other
          }

          test "a negative margin is refused; zero is not" {
              match Indistinguishability.declare (record ()) "holdoutError" (-0.01) EveryAlternative with
              | Error(NegativeMargin m) -> Expect.equal m (-0.01) "naming the margin"
              | other -> failtestf "expected a negative-margin refusal, got %A" other

              match Indistinguishability.declare (record ()) "holdoutError" 0.0 EveryAlternative with
              | Ok d -> Expect.equal d.Margin 0.0 "zero is a strict question, not a malformed one"
              | Error r -> failtestf "zero was refused: %s" (IndistinguishabilityRefusal.describe r)
          }

          test "a scope naming a branch this record does not hold is refused" {
              match
                  Indistinguishability.declare (record ()) "holdoutError" 0.05 (NamedAlternatives [ "b"; "ghost" ])
              with
              | Error(UnknownScopeState(state, known)) ->
                  Expect.equal state "ghost" "naming what was attempted"
                  Expect.equal known [ "root"; "a"; "b"; "c"; "d" ] "and the branches that exist, in exploration order"
              | other -> failtestf "expected an unknown-scope-state refusal, got %A" other
          }

          test "the scope is the reference's alternatives, in exploration order — never the whole record" {
              let d = (declaring "holdoutError" 0.05 EveryAlternative).Value

              Expect.equal
                  (Indistinguishability.scoped d "a" (record ()))
                  [ "root"; "b"; "c" ]
                  "the ancestor and the two branches sharing its parent — and not 'd', which nobody weighed against 'a'"

              Expect.isFalse
                  (Vintage.wereAlternatives "a" "d" (record ()))
                  "the same rule the decision pairs use, asked directly"
          }

          test "an undeclared margin yields the no-declaration state, not an empty list" {
              // The failure this case exists for: an empty member list is a decisive finding
              // about the evidence, and returning one for a question nobody asked reports that
              // finding to every caller who forgot to declare anything.
              Expect.equal
                  (Indistinguishability.report None "a" (record ()))
                  NoDeclaration
                  "no set, and the reason said out loud"

              Expect.isNone
                  (Indistinguishability.members (Indistinguishability.report None "a" (record ())))
                  "and the convenience accessor cannot reach a list without passing through the distinction"
          }

          test "a reference with no reading of the metric yields its own state, also not an empty list" {
              // 'root' is fitted on nothing at all, so there is no anchor to measure from.
              match Indistinguishability.report (declaring "holdoutError" 0.05 EveryAlternative) "root" (record ()) with
              | NoReferenceReading(state, metric) ->
                  Expect.equal state "root" "naming the reference"
                  Expect.equal metric "holdoutError" "and the metric it carries no reading of"
              | other -> failtestf "expected a no-reference-reading report, got %A" other
          }

          test "membership is a symmetric distance within the declared margin" {
              let s = setOrFail (declaring "holdoutError" 0.05 EveryAlternative) "a" (record ())

              Expect.equal
                  (s.Members |> List.map (fun m -> m.State))
                  [ "a"; "b" ]
                  "the reference heads its own set, and the one alternative within the margin follows"

              Expect.floatClose
                  Accuracy.high
                  (s.Members |> List.last).Distance
                  0.02
                  "the distance is unsigned and in the metric's own units"

              Expect.equal s.ReferenceReading 0.40 "the anchor every distance is measured from"
              Expect.equal s.DatasetVersion "ds-1" "one vintage for the whole set"
              Expect.equal s.Unread [ "root" ] "the alternative with no reading is named, not dropped"

              // 'd' reads NEARER the reference than 'b' does and is absent, because it was
              // never an alternative. A rule built on closeness alone would contain it.
              Expect.isFalse
                  (s.Members |> List.exists (fun m -> m.State = "d"))
                  "the closest reading in the record is out of scope"
          }

          test "a branch reading further ahead is separated, not promoted" {
              // 'c' reads 0.55 where the reference reads 0.40, and under a metric where lower
              // is the leading direction it is the one behind. Widen the margin past its
              // distance and it becomes a member — the surface has no direction and no opinion
              // about which side of the reference anything fell on.
              let wide = setOrFail (declaring "holdoutError" 0.2 EveryAlternative) "a" (record ())

              Expect.equal
                  (wide.Members |> List.map (fun m -> m.State))
                  [ "a"; "b"; "c" ]
                  "distance alone decides membership, in both directions"

              Expect.isTrue
                  (wide.Members |> List.forall (fun m -> m.Distance >= 0.0))
                  "and every distance is unsigned — nothing here records which way a difference ran"
          }

          test "tightening the margin to zero leaves the reference alone" {
              let s = setOrFail (declaring "holdoutError" 0.0 EveryAlternative) "a" (record ())

              Expect.equal (s.Members |> List.map (fun m -> m.State)) [ "a" ] "only an exact agreement would join it"
              Expect.equal (Indistinguishability.multiplicity s) 1 "a family of one"
              Expect.equal s.Unread [ "root" ] "and the unread alternative is still unread — that is not a distance"
          }

          test "a named scope narrows the question and is still read in exploration order" {
              let s =
                  setOrFail (declaring "holdoutError" 0.2 (NamedAlternatives [ "c"; "b" ])) "a" (record ())

              Expect.equal
                  (s.Members |> List.map (fun m -> m.State))
                  [ "a"; "b"; "c" ]
                  "the caller's order does not decide the answer's order"

              Expect.isEmpty
                  s.Unread
                  "'root' was not named, so it is not an unread alternative — it is not in the question"
          }

          test "a named scope cannot admit a branch the reference was never weighed against" {
              // The scope narrows; it never widens. Naming 'd' explicitly does not make it an
              // alternative, because being an alternative is a fact about the record.
              let s =
                  setOrFail (declaring "holdoutError" 0.2 (NamedAlternatives [ "b"; "d" ])) "a" (record ())

              Expect.equal
                  (s.Members |> List.map (fun m -> m.State))
                  [ "a"; "b" ]
                  "'d' is refused by the record, not by the scope"
          }

          test "the baseline vintage is labelled rather than excluded, and a later one reads out-of-time" {
              let onBaseline =
                  setOrFail (declaring "holdoutError" 0.05 EveryAlternative) "a" (record ())

              Expect.isFalse
                  onBaseline.OutOfTime
                  "'ds-1' is the reference's first reading — the evidence its curation would have been taken on"

              // A second vintage arrives for two of the branches. The reference's latest
              // reading moves, so the set moves with it, and the label flips.
              let refreshed =
                  match Reducer.applyAll [ reading "a" "ds-2" 0.44; reading "b" "ds-2" 0.45 ] (record ()) with
                  | Ok e -> e
                  | Error(i, r) -> failwithf "op %d rejected: %s" i (Rejection.describe r)

              let later =
                  match Indistinguishability.declare refreshed "holdoutError" 0.05 EveryAlternative with
                  | Ok d -> setOrFail (Some d) "a" refreshed
                  | Error r -> failwithf "refused: %s" (IndistinguishabilityRefusal.describe r)

              Expect.equal later.DatasetVersion "ds-2" "the reference's most recent vintage anchors the comparison"
              Expect.isTrue later.OutOfTime "and this one arrived after the baseline"
              Expect.equal (later.Members |> List.map (fun m -> m.State)) [ "a"; "b" ] "the members on that vintage"

              Expect.equal
                  later.Unread
                  [ "root"; "c" ]
                  "and the alternatives with no reading on it are named — a branch nobody re-fitted is not a branch that reads differently"

              // Asked about the earlier vintage explicitly, the answer is the earlier one.
              Expect.equal
                  (match
                      Indistinguishability.reportOn
                          (declaring "holdoutError" 0.05 EveryAlternative)
                          "ds-1"
                          "a"
                          refreshed
                   with
                   | Derived s -> s.DatasetVersion, s.OutOfTime
                   | other -> failtestf "expected a derived set, got %A" other)
                  ("ds-1", false)
                  "a named vintage is honoured, and labelled for what it is"
          }

          test "the rendered line says what the set is, and that it is not an order" {
              let s = setOrFail (declaring "holdoutError" 0.05 EveryAlternative) "a" (record ())
              let line = Indistinguishability.describeSet s

              Expect.stringContains line "indistinguishable under the declared margin" "what it is"
              Expect.stringContains line "the reference's own baseline, so in-sample" "on what evidence"
              Expect.stringContains line "1 alternative(s) unread on it" "and what it could not place"
              Expect.stringContains line "Nothing here orders them" "and what it is not"
          }

          test "the set survives the wire, declaration and non-derived cases alike" {
              let s = setOrFail (declaring "holdoutError" 0.05 EveryAlternative) "a" (record ())

              match Wire.parseIndistinguishabilityReport (Wire.renderIndistinguishabilityReport (Derived s)) with
              | Ok back -> Expect.equal back (Derived s) "the derived set round-trips whole"
              | Error m -> failtestf "the report failed to decode: %s" m

              for r in [ NoDeclaration; NoReferenceReading("a", "holdoutError") ] do
                  match Wire.parseIndistinguishabilityReport (Wire.renderIndistinguishabilityReport r) with
                  | Ok back -> Expect.equal back r "the non-derived cases are tags, not absences"
                  | Error m -> failtestf "%A failed to decode: %s" r m
          }

          // ---- the forest roll-up ---------------------------------------------

          test "the forest reports membership per sibling, and names the ones that could not answer" {
              let sibling (id: SiblingId) : SiblingRef =
                  { Sibling = id
                    Label = "unit " + id
                    ExplorationRef = "memory:" + id }

              let forest: Forest =
                  { Id = "portfolio"
                    Title = "three units, one question"
                    Siblings = [ sibling "unit-a"; sibling "unit-b"; sibling "unit-c" ]
                    Policy = Policy.empty
                    Thresholds = None }

              // unit-b's 'b' reads far from its reference, so its evidence DOES separate the
              // two. unit-c never fitted the reference at all, which is a different fact from
              // finding nothing near it.
              let separated =
                  foldOrFail
                      [ seedOp "root"
                        forkOp "a" "root"
                        forkOp "b" "root"
                        reading "a" "ds-1" 0.40
                        reading "b" "ds-1" 0.90 ]

              let unfitted = foldOrFail [ seedOp "root"; forkOp "a" "root"; forkOp "b" "root" ]

              let records =
                  Map.ofList [ "unit-a", record (); "unit-b", separated; "unit-c", unfitted ]

              let view =
                  Indistinguishability.forestView (declaring "holdoutError" 0.05 EveryAlternative) "a" forest records

              Expect.equal view.DerivedIn [ "unit-a"; "unit-b" ] "two units could answer"

              Expect.equal
                  (view.Reports
                   |> List.map (fun (s, r) ->
                       s,
                       (match r with
                        | Derived _ -> "derived"
                        | NoReferenceReading _ -> "noReferenceReading"
                        | MismatchedUnits _ -> "mismatchedUnits"
                        | NoDeclaration -> "noDeclaration")))
                  [ "unit-a", "derived"; "unit-b", "derived"; "unit-c", "noReferenceReading" ]
                  "and the one that could not is reported as such rather than as finding nothing"

              let row (id: StateId) =
                  view.Recurrence |> List.find (fun r -> r.State = id)

              Expect.equal
                  (row "b").IndistinguishableIn
                  [ "unit-a" ]
                  "one unit's evidence does not separate 'b' from the reference"

              Expect.equal (row "b").SeparatedIn [ "unit-b" ] "and the other's does"
              Expect.equal (row "root").UnreadIn [ "unit-a"; "unit-b" ] "the unfitted ancestor is unread in both"
              Expect.equal (row "d").AbsentIn [ "unit-b" ] "and a branch only one unit explored is absent in the other"
              Expect.equal (row "d").OutOfScopeIn [ "unit-a" ] "where it exists but was never an alternative"

              Expect.equal
                  (view.Recurrence |> List.map (fun r -> r.State))
                  [ "root"; "a"; "b"; "c"; "d" ]
                  "the rows are in record order — deliberately NOT sorted by how often anything was indistinguishable"
          }

          test "a forest asked without a declaration derives nothing anywhere, and says so per sibling" {
              let forest =
                  { Forest.empty "quiet" "a question nobody declared" with
                      Siblings =
                          [ { Sibling = "unit-a"
                              Label = "unit a"
                              ExplorationRef = "memory:unit-a" } ] }

              let view =
                  Indistinguishability.forestView None "a" forest (Map.ofList [ "unit-a", record () ])

              Expect.equal
                  (view.Reports |> List.map snd)
                  [ NoDeclaration ]
                  "the sibling is asked and answers that nothing was asked"

              Expect.isEmpty view.DerivedIn "no set was derived"

              Expect.isEmpty
                  view.Recurrence
                  "and the roll-up is empty because there is nothing to roll up — not because nothing matched"
          } ]
