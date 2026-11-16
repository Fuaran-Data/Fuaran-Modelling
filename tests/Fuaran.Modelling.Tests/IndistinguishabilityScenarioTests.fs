// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.IndistinguishabilityScenarioTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  Spec multiplicity as one scripted run.
//
//  One unit explores four forms of the same specification and fits all four on
//  one vintage. Then somebody asks the question the whole exercise exists to
//  answer, and it is NOT "which one is right": the solution space is unbounded,
//  there is no calibration metric that settles it, and the record was never
//  going to produce a single answer. The question is how many forms this
//  evidence cannot tell apart — because that number, and the spread of business
//  quantities across it, is what an honest report carries instead of a point
//  estimate somebody would otherwise read as the truth.
//
//  The run is written to demonstrate four things, in order:
//
//    * Asked with no declared margin, the record says so. It does not say
//      "nothing is close", which is what an empty list would say and is a much
//      stronger claim than anybody made.
//    * Asked with a declared margin, two forms are indistinguishable.
//    * Tightened to zero, the reference stands alone — the same record, a
//      different question, and the declaration is the only thing that moved.
//    * A form reading NEARER the reference than either member is not in the set,
//      because nobody was ever choosing between the two.
//
//  Every fixture term is neutral. The forms, the metric and the vintages are
//  named for what they are in a test.
// ============================================================================

let private instant = "2026-04-01T00:00:00Z"

/// One fit of one form on one vintage, carrying one diagnostic and no gates. Gates are the
/// statistical axis's other question and would only be noise here.
let private fit (state: StateId) (vintage: string) (holdoutError: float) : ExplorationOp =
    Annotate
        { StateId = state
          Snapshot =
            { Key =
                { SpecHash = "h-" + state
                  DatasetVersion = vintage
                  Seed = "7"
                  ProviderId = "reference"
                  ProviderVersion = "1" }
              ArtifactRef = "ref:" + state + "-" + vintage
              Diagnostics = Map.ofList [ "holdoutError", holdoutError ]
              DiagnosticUnits = Map.empty
              Gates = []
              SubmittedBy = SubmitScheduled
              RecordedAt = instant } }

let private step (op: ExplorationOp) (s: Session) : Session =
    match Session.submitGoverned hashFn (fullPolicy "curator") "curator" curator op s with
    | Ok(_, s') -> s'
    | Error e -> failwithf "op %s did not land: %A" (Op.kindName op) e

/// The exploration:
///
/// ```
///   starting-form                       explored, never fitted
///   ├── trend-form      0.40            the reference
///   ├── seasonal-form   0.42            a different way of doing the same thing
///   ├── damped-form     0.55            a third
///   └── damped-form
///        └── damped-refit   0.41        a continuation of the damped line
/// ```
///
/// Three of the four fitted forms were alternatives to the reference — one is an ancestor's
/// line, two were tried side by side with it. `damped-refit` was not: it is where the damped
/// line went AFTER it was set aside, and nobody ever weighed it against the trend form.
let private explored () : Session =
    sessionOrFail []
    |> step (
        Seed
            { StateId = "starting-form"
              Label = "the form the work started from"
              Payload = payload "{\"terms\":0}" "h-starting-form" }
    )
    |> step (
        Fork
            { StateId = "trend-form"
              From = "starting-form"
              Label = "a trend term"
              Payload = payload "{\"terms\":1}" "h-trend-form" }
    )
    |> step (
        Fork
            { StateId = "seasonal-form"
              From = "starting-form"
              Label = "a seasonal term instead"
              Payload = payload "{\"terms\":2}" "h-seasonal-form" }
    )
    |> step (
        Fork
            { StateId = "damped-form"
              From = "starting-form"
              Label = "a damped term instead"
              Payload = payload "{\"terms\":3}" "h-damped-form" }
    )
    |> step (
        Fork
            { StateId = "damped-refit"
              From = "damped-form"
              Label = "where the damped line went next"
              Payload = payload "{\"terms\":4}" "h-damped-refit" }
    )
    |> step (fit "trend-form" "ds-1" 0.40)
    |> step (fit "seasonal-form" "ds-1" 0.42)
    |> step (fit "damped-form" "ds-1" 0.55)
    |> step (fit "damped-refit" "ds-1" 0.41)

/// The curator keeps the trend form and sets the other two side-by-side alternatives aside —
/// an ordinary curation, and one that changes NOTHING about the set below. A form that was
/// killed is still a form this evidence cannot distinguish, which is the entire reason the
/// killed branches are retained.
let private curated () : Session =
    explored ()
    |> step (curateOp "trend-form" Keep "the form the work converged on")
    |> step (curateOp "seasonal-form" Kill "not carried forward")
    |> step (curateOp "damped-form" Kill "not carried forward")

let private declaration (margin: float) (e: Exploration) : IndistinguishabilityDeclaration option =
    match Indistinguishability.declare e "holdoutError" margin EveryAlternative with
    | Ok d -> Some d
    | Error r -> failwithf "the declaration was refused: %s" (IndistinguishabilityRefusal.describe r)

let private setOn (margin: float) (s: Session) : IndistinguishabilitySet =
    match Indistinguishability.report (declaration margin s.Record) "trend-form" s.Record with
    | Derived derived -> derived
    | other -> failwithf "expected a derived set, got %A" other

[<Tests>]
let tests =
    testList
        "IndistinguishabilityScenario"
        [ test "the fixture is a real question — four forms fitted on one vintage, readings apart" {
              // A precondition, checked rather than assumed. A scenario whose four forms all
              // read identically would run to completion and demonstrate nothing, and it would
              // look exactly like this one.
              let e = (explored ()).Record

              Expect.equal (Query.fittedStates e |> List.length) 4 "four forms carry evidence"

              Expect.equal
                  (Query.fittedStates e
                   |> List.map (fun id -> Query.tryState id e |> Option.get |> Vintage.readingOn "holdoutError" "ds-1"))
                  [ Some 0.40; Some 0.42; Some 0.55; Some 0.41 ]
                  "on one vintage, with four different readings — so a margin has something to decide"
          }

          test "asked with no declared margin, the record says nothing was declared" {
              // Not an empty set. "Nobody declared a margin" and "nothing lies within the
              // declared margin" are different facts, and only one of them is about the
              // evidence — a surface that returned an empty list for both would report the
              // second to everyone who meant the first.
              Expect.equal
                  (Indistinguishability.report None "trend-form" (explored ()).Record)
                  NoDeclaration
                  "the explicit state, never an empty-list lookalike"
          }

          test "asked with a declared margin, two forms are indistinguishable" {
              let s = setOn 0.05 (explored ())

              Expect.equal
                  (s.Members |> List.map (fun m -> m.State))
                  [ "trend-form"; "seasonal-form" ]
                  "the reference and the one alternative within 0.05 of it"

              Expect.equal
                  (Indistinguishability.multiplicity s)
                  2
                  "a family of two, which is the number a report carries"

              Expect.equal
                  s.Unread
                  [ "starting-form" ]
                  "and the alternative nobody fitted is named — unread is not the same as far away"

              Expect.stringContains
                  (Indistinguishability.describeSet s)
                  "'trend-form' and 1 other branch(es) are indistinguishable under the declared margin of 0.05"
                  "the rendered line counts the family"

              Expect.stringContains
                  (Indistinguishability.describeSet s)
                  "Nothing here orders them"
                  "and refuses the sentence that would come next"
          }

          test "the form reading closest to the reference is not in the set, because nobody weighed it" {
              // 'damped-refit' reads 0.41 — nearer the reference than the member at 0.42. It is
              // absent, and this is the discipline rather than an accident: it is where the
              // damped line went after being set aside, so the two were never alternatives.
              // A set built on closeness alone would contain it, and would be a list of
              // branches that read alike rather than a family of specifications somebody chose
              // between.
              let s = setOn 0.05 (explored ())

              Expect.isFalse (s.Members |> List.exists (fun m -> m.State = "damped-refit")) "not a member"

              Expect.isFalse
                  (List.contains "damped-refit" s.Unread)
                  "and not unread either — it is not in the question at all"

              Expect.isTrue
                  ((Vintage.readingOn
                      "holdoutError"
                      "ds-1"
                      (Query.tryState "damped-refit" (explored ()).Record |> Option.get))
                   |> Option.map (fun x -> abs (x - s.ReferenceReading) < (s.Members |> List.last).Distance)
                   |> Option.defaultValue false)
                  "even though it reads nearer the reference than the member that IS in the set"
          }

          test "tightening the margin to zero leaves the reference alone — same record, different question" {
              let s = setOn 0.0 (explored ())

              Expect.equal (s.Members |> List.map (fun m -> m.State)) [ "trend-form" ] "the reference and nothing else"
              Expect.equal (Indistinguishability.multiplicity s) 1 "a family of one"

              Expect.equal
                  (setOn 0.2 (explored ())
                   |> fun wide -> wide.Members |> List.map (fun m -> m.State))
                  [ "trend-form"; "seasonal-form"; "damped-form" ]
                  "and widened, all three alternatives — the declaration is the only thing that moved"
          }

          test "curating the alternatives away does not shrink the set" {
              // The killed forms are still forms this evidence cannot distinguish. That is the
              // whole reason an abandoned branch is retained rather than deleted: a curation is
              // a decision somebody took, not a claim that the alternatives stopped being
              // plausible.
              let before = setOn 0.05 (explored ())
              let after = setOn 0.05 (curated ())

              Expect.equal
                  (after.Members |> List.map (fun m -> m.State))
                  (before.Members |> List.map (fun m -> m.State))
                  "the same two forms, one of them killed"

              Expect.equal
                  (Query.killedStates (curated ()).Record)
                  [ "seasonal-form"; "damped-form" ]
                  "and killed it is"
          }

          test "the set is read on the reference's own baseline here, and says so" {
              let s = setOn 0.05 (explored ())

              Expect.equal s.DatasetVersion "ds-1" "one vintage for the whole set"

              Expect.isFalse
                  s.OutOfTime
                  "the evidence the forms were curated on — labelled, because a reader who cannot tell will use it the same way"

              Expect.stringContains
                  (Indistinguishability.describeSet s)
                  "so in-sample"
                  "and the rendered line says which kind of evidence it is standing on"
          }

          test "a new vintage arrives, and the question is answered again on it" {
              // The point of the vintage axis, asked of this surface: a family that survives a
              // refresh is a different fact from one that was an artefact of one dataset.
              let refreshed =
                  curated ()
                  |> step (fit "trend-form" "ds-2" 0.44)
                  |> step (fit "seasonal-form" "ds-2" 0.62)
                  |> step (fit "damped-form" "ds-2" 0.46)

              let s =
                  match
                      Indistinguishability.report (declaration 0.05 refreshed.Record) "trend-form" refreshed.Record
                  with
                  | Derived derived -> derived
                  | other -> failtestf "expected a derived set, got %A" other

              Expect.equal s.DatasetVersion "ds-2" "the reference's most recent vintage anchors it"
              Expect.isTrue s.OutOfTime "and this one arrived after the baseline"

              Expect.equal
                  (s.Members |> List.map (fun m -> m.State))
                  [ "trend-form"; "damped-form" ]
                  "a different pair — on this data the seasonal form separates and the damped one does not"

              // The family is the same SIZE and a different SET, which is exactly the finding
              // worth having: the multiplicity did not go away, it moved.
              Expect.equal
                  (Indistinguishability.multiplicity s)
                  (Indistinguishability.multiplicity (setOn 0.05 (curated ())))
                  "two forms either way, and not the same two"
          }

          test "the whole run survives persistence, and the answer is the same read off a reload" {
              let s = curated ()

              match Session.toJsonl s |> Session.ofJsonl hashFn with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  Expect.equal
                      (Indistinguishability.report (declaration 0.05 back.Record) "trend-form" back.Record)
                      (Indistinguishability.report (declaration 0.05 s.Record) "trend-form" s.Record)
                      "the set is a derivation, so it is the same derivation off a reloaded history"

              Expect.equal (Session.toJsonl (curated ())) (Session.toJsonl s) "the same script yields the same history"
          }

          test "the declaration and the derived answer both cross the wire" {
              let s = setOn 0.05 (explored ())
              let d = (declaration 0.05 (explored ()).Record).Value

              match Wire.parseIndistinguishabilityDeclaration (Wire.renderIndistinguishabilityDeclaration d) with
              | Ok back -> Expect.equal back d "the question round-trips"
              | Error m -> failtestf "the declaration failed to decode: %s" m

              match Wire.parseIndistinguishabilityReport (Wire.renderIndistinguishabilityReport (Derived s)) with
              | Ok back -> Expect.equal back (Derived s) "and so does the answer, declaration included"
              | Error m -> failtestf "the report failed to decode: %s" m

              // A report read back without the declaration that produced it would be a list of
              // branches with no statement of what made them one.
              Expect.stringContains
                  (Wire.renderIndistinguishabilityReport (Derived s))
                  "\"margin\":0.05"
                  "the margin travels with the answer"
          }

          // ---- the forest roll-up ---------------------------------------------

          test "three units under one question — membership per sibling, and no order over them" {
              let sibling (id: SiblingId) : SiblingRef =
                  { Sibling = id
                    Label = "unit " + id
                    ExplorationRef = "memory:" + id }

              let forest: Forest =
                  { Id = "portfolio"
                    Title = "three units, one declared margin"
                    Siblings = [ sibling "unit-a"; sibling "unit-b"; sibling "unit-c" ]
                    Policy = Policy.empty
                    Thresholds = None }

              // unit-b's seasonal form reads far apart, so its evidence separates the pair
              // unit-a's cannot. unit-c has explored the forms and fitted none of them.
              let separating =
                  explored ()
                  |> step (fit "seasonal-form" "ds-2" 0.90)
                  |> step (fit "trend-form" "ds-2" 0.40)

              let unfitted =
                  sessionOrFail []
                  |> step (
                      Seed
                          { StateId = "starting-form"
                            Label = "the form the work started from"
                            Payload = payload "{\"terms\":0}" "h-starting-form" }
                  )
                  |> step (
                      Fork
                          { StateId = "trend-form"
                            From = "starting-form"
                            Label = "a trend term"
                            Payload = payload "{\"terms\":1}" "h-trend-form" }
                  )

              let records =
                  Map.ofList
                      [ "unit-a", (curated ()).Record
                        "unit-b", separating.Record
                        "unit-c", unfitted.Record ]

              let view =
                  Indistinguishability.forestView (declaration 0.05 (curated ()).Record) "trend-form" forest records

              Expect.equal view.DerivedIn [ "unit-a"; "unit-b" ] "two units could answer the question"

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
                  "and the third is reported as having no evidence, not as finding nothing indistinguishable"

              let row (id: StateId) =
                  view.Recurrence |> List.find (fun r -> r.State = id)

              Expect.equal
                  (row "seasonal-form").IndistinguishableIn
                  [ "unit-a" ]
                  "one unit's evidence does not separate the seasonal form from the trend form"

              Expect.equal
                  (row "seasonal-form").SeparatedIn
                  [ "unit-b" ]
                  "and the other unit's, on its later data, does"

              Expect.equal
                  (row "damped-refit").OutOfScopeIn
                  [ "unit-a"; "unit-b" ]
                  "the continuation of the damped line was never an alternative in either"

              Expect.equal
                  (row "trend-form").IndistinguishableIn
                  [ "unit-a"; "unit-b" ]
                  "and the reference is a member of its own set everywhere, which is why the count is a family size"

              // The one ordering claim this surface makes is that it makes none. Sorting by
              // how often something was indistinguishable would read as a ranking to everyone
              // who did not write it, on the surface where a ranking is self-contradicting.
              Expect.equal
                  (view.Recurrence |> List.map (fun r -> r.State))
                  [ "starting-form"
                    "trend-form"
                    "seasonal-form"
                    "damped-form"
                    "damped-refit" ]
                  "record order, not a league table"
          } ]
