// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.SeriesUnitTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  A metric series is compared in one unit only (D40).
//
//  Every case the decision names: two different unit texts, a stated unit
//  beside an unstated one, a series in one unit other than the declared one,
//  and the record with no units anywhere — which must read exactly as it did
//  before units existed, down to the bytes.
// ============================================================================

let private unitOf (text: string) : UnitOfMeasure =
    match UnitText.tryParse text with
    | Ok u -> u
    | Error e -> failwith e

let private kg = unitOf "kg"
let private g = unitOf "g"

let private reading (state: StateId) (v: string) (x: float) (unit: UnitOfMeasure option) : ExplorationOp =
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

let private thresholds (unit: UnitOfMeasure option) : CorrectionThresholds =
    { Metric = "holdoutError"
      Prefer = PreferLower
      MinVintages = 2
      MinMargin = 0.1
      MinSiblings = 1
      Unit = unit }

let private theDecision = { Kept = "kept"; SetAside = "setAside" }

/// A kept and a set-aside branch on three vintages. The set-aside branch reads 0.3 kg
/// throughout; the kept branch reads 0.5 kg on v1 and v2, and whatever `keptOnV3` says on v3.
let private decisionEvidence (keptOnV3: float * UnitOfMeasure option) (setAsideUnit: UnitOfMeasure option) =
    let x3, u3 = keptOnV3

    foldOrFail (
        [ seedOp "kept"; forkOp "setAside" "kept" ]
        @ [ reading "kept" "v1" 0.5 (Some kg)
            reading "kept" "v2" 0.5 (Some kg)
            reading "kept" "v3" x3 u3 ]
        @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "setAside" v 0.3 setAsideUnit ]
        @ [ curateOp "kept" Keep "the settled form"
            curateOp "setAside" Kill "not interpretable on this unit" ]
    )

/// The same pair with no unit stated anywhere — the record as it was before units existed.
let private unitlessEvidence () =
    foldOrFail (
        [ seedOp "kept"; forkOp "setAside" "kept" ]
        @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "kept" v 0.5 None ]
        @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "setAside" v 0.3 None ]
        @ [ curateOp "kept" Keep "the settled form"
            curateOp "setAside" Kill "not interpretable on this unit" ]
    )

let private compared (r: DecisionRecord) =
    r.Comparisons |> List.map (fun c -> c.DatasetVersion)

/// A reference and three alternatives on one vintage, plus a continuation that is out of scope.
/// `unitOn` says which unit each branch's reading states.
let private familyEvidence (unitOn: StateId -> UnitOfMeasure option) =
    foldOrFail
        [ seedOp "root"
          forkOp "reference" "root"
          forkOp "near" "root"
          forkOp "far" "root"
          forkOp "unrelated" "far"
          reading "reference" "v1" 0.40 (unitOn "reference")
          reading "near" "v1" 0.42 (unitOn "near")
          reading "far" "v1" 0.55 (unitOn "far")
          reading "unrelated" "v1" 0.41 (unitOn "unrelated") ]

let private ask (unit: UnitOfMeasure option) (e: Exploration) =
    match Indistinguishability.declareIn unit e "holdoutError" 0.05 EveryAlternative with
    | Ok d -> Indistinguishability.report (Some d) "reference" e
    | Error r -> failtestf "declaration refused: %s" (IndistinguishabilityRefusal.describe r)

let private allIn (u: UnitOfMeasure option) = fun (_: StateId) -> u

let private allBut (state: StateId) (other: UnitOfMeasure option) =
    fun s -> if s = state then other else Some kg

let private memberIds (r: IndistinguishabilityReport) = Indistinguishability.members r

[<Tests>]
let vintageAxisTests =
    testList
        "Series units / the vintage axis"
        [ test "a vintage stated in grams beside kilograms is named with both units and never compared" {
              let e = decisionEvidence (500.0, Some g) (Some kg)
              let r = Vintage.record (thresholds (Some kg)) theDecision e

              Expect.equal (compared r) [ "v1"; "v2" ] "only the vintages read in the declared unit are compared"

              Expect.equal
                  r.UnitMismatched
                  [ { DatasetVersion = "v3"
                      Kept = Some g
                      SetAside = Some kg } ]
                  "the grams vintage is named with the units found"

              Expect.equal r.Unit (Some kg) "the record states the unit its comparisons are in"
              Expect.isEmpty r.Incomparable "a unit mismatch is not a missing reading"
          }

          test "read as alike, the mixed vintage would open the gate; read by unit, it cannot" {
              // 500 g against 0.3 kg is a lead of 499.7 when the numbers are taken as alike —
              // a second out-of-time vintage past the margin, and a question raised about a
              // curation on the strength of a unit nobody converted.
              let e = decisionEvidence (500.0, Some g) (Some kg)
              Expect.isEmpty (Vintage.corrections (thresholds (Some kg)) e) "the gate stays shut"

              let uniform = decisionEvidence (0.5, Some kg) (Some kg)

              Expect.equal
                  (Vintage.corrections (thresholds (Some kg)) uniform
                   |> List.map (fun f -> f.LeadingVintages))
                  [ [ "v2"; "v3" ] ]
                  "the same evidence in one unit raises the question it always did"
          }

          test "a stated unit beside an unstated one is a mixed series" {
              let e = decisionEvidence (0.5, None) (Some kg)
              let r = Vintage.record (thresholds (Some kg)) theDecision e

              Expect.equal
                  r.UnitMismatched
                  [ { DatasetVersion = "v3"
                      Kept = None
                      SetAside = Some kg } ]
                  "the unstated reading is named as stating none"

              Expect.equal (compared r) [ "v1"; "v2" ] "and is not compared"
          }

          test "thresholds that state no unit compare nothing stated in one" {
              let e = decisionEvidence (0.5, Some kg) (Some kg)
              let r = Vintage.record (thresholds None) theDecision e

              Expect.isEmpty r.Comparisons "a guessed unit is a margin that can silently pass"
              Expect.equal (r.UnitMismatched |> List.map _.DatasetVersion) [ "v1"; "v2"; "v3" ] "every vintage named"
              Expect.equal r.Baseline (Some "v1") "the baseline is a line in time, drawn before units are read"
          }

          test "the baseline stays on the first vintage even when that vintage is unit-mismatched" {
              let e =
                  foldOrFail (
                      [ seedOp "kept"; forkOp "setAside" "kept" ]
                      @ [ reading "kept" "v1" 500.0 (Some g)
                          reading "kept" "v2" 0.5 (Some kg)
                          reading "kept" "v3" 0.5 (Some kg) ]
                      @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "setAside" v 0.3 (Some kg) ]
                      @ [ curateOp "kept" Keep "k"; curateOp "setAside" Kill "s" ]
                  )

              let r = Vintage.record (thresholds (Some kg)) theDecision e
              Expect.equal r.Baseline (Some "v1") "the curation was taken on v1's evidence"

              Expect.equal
                  (r.Comparisons |> List.map (fun c -> c.DatasetVersion, c.OutOfTime))
                  [ "v2", true; "v3", true ]
                  "so v2 and v3 are out-of-time, and v1 is named rather than compared"
          }

          test "a record with no units anywhere reads exactly as before" {
              let e = unitlessEvidence ()
              let r = Vintage.record (thresholds None) theDecision e

              Expect.equal (compared r) [ "v1"; "v2"; "v3" ] "every vintage compared"
              Expect.isEmpty r.UnitMismatched "nothing named"
              Expect.equal r.Unit None "and no unit stated"

              Expect.equal
                  (Vintage.corrections (thresholds None) e |> List.map (fun f -> f.LeadingVintages))
                  [ [ "v2"; "v3" ] ]
                  "the question it always raised"

              let bytes = Wire.encodeDecisionRecord r |> Canonical.write Rendering.CrossHost
              Expect.isFalse (bytes.Contains "\"unit") "no unit key on the wire"
          }

          test "the forest roll-up compares each sibling only in the declared unit" {
              let forest =
                  { Forest.empty "f" "two units" with
                      Siblings =
                          [ { Sibling = "a"
                              Label = "in kilograms"
                              ExplorationRef = "memory:a" }
                            { Sibling = "b"
                              Label = "in grams"
                              ExplorationRef = "memory:b" } ]
                      Thresholds =
                          Some
                              { thresholds (Some kg) with
                                  MinSiblings = 2 } }

              let inGrams =
                  foldOrFail (
                      [ seedOp "kept"; forkOp "setAside" "kept" ]
                      @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "kept" v 500.0 (Some g) ]
                      @ [ for v in [ "v1"; "v2"; "v3" ] -> reading "setAside" v 300.0 (Some g) ]
                      @ [ curateOp "kept" Keep "k"; curateOp "setAside" Kill "s" ]
                  )

              let records =
                  Map.ofList [ "a", decisionEvidence (0.5, Some kg) (Some kg); "b", inGrams ]

              Expect.isEmpty
                  (Vintage.forestCorrections forest records)
                  "a sibling in grams is not a second sibling raising the question in kilograms"

              let rows =
                  Vintage.recurrenceWithPower forest records |> List.collect (fun r -> r.Records)

              let b = rows |> List.find (fun (s, _) -> s = "b") |> snd
              Expect.isEmpty b.Comparisons "the grams sibling compares nothing"
              Expect.equal (List.length b.UnitMismatched) 3 "and names all three vintages"
          }

          test "a series carries each reading's unit beside it" {
              let s =
                  Vintage.series "holdoutError" "kept" (decisionEvidence (500.0, Some g) (Some kg))

              Expect.equal s.Readings [ "v1", 0.5; "v2", 0.5; "v3", 500.0 ] "the numbers as reported"

              Expect.equal
                  s.Units
                  (Map.ofList [ "v1", kg; "v2", kg; "v3", g ])
                  "and what each one measures, so 500 beside 0.5 does not read as a thousandfold change"

              let unitless = Vintage.series "holdoutError" "kept" (unitlessEvidence ())
              Expect.isEmpty unitless.Units "a series that states no units carries none"

              Expect.isFalse
                  ((Wire.encodeVintageSeries unitless |> Canonical.write Rendering.CrossHost).Contains "\"units\"")
                  "and encodes as it always did"
          }

          test "the rendered correction names the margin's unit only when it has one" {
              let inKg =
                  Vintage.corrections (thresholds (Some kg)) (decisionEvidence (0.5, Some kg) (Some kg))
                  |> List.exactlyOne
                  |> Vintage.describeCorrection

              Expect.stringContains inKg " kg — raised for review" "the lead in its unit"

              let unitless =
                  Vintage.corrections (thresholds None) (unitlessEvidence ())
                  |> List.exactlyOne
                  |> Vintage.describeCorrection

              Expect.isFalse (unitless.Contains " kg") "unchanged where no unit is declared"
          } ]

[<Tests>]
let indistinguishabilityTests =
    testList
        "Series units / indistinguishability"
        [ test "an alternative in grams beside a reference in kilograms refuses the set by name" {
              match ask (Some kg) (familyEvidence (allBut "near" (Some g))) with
              | MismatchedUnits m ->
                  Expect.equal
                      m
                      { Metric = "holdoutError"
                        Declared = Some kg
                        Stated = [ g; kg ]
                        Unstated = false }
                      "the metric, the declared unit and the units found"

                  let line = SeriesUnits.describe m
                  Expect.stringContains line "'holdoutError'" "the line names the metric"
                  Expect.stringContains line "'g', 'kg'" "and the units found"
                  Expect.stringContains line "declared in 'kg'" "and the declared one"
              | other -> failtestf "expected a refusal, got %A" other
          }

          test "an alternative stating no unit beside the reference's kilograms is a mixed series" {
              match ask (Some kg) (familyEvidence (allBut "near" None)) with
              | MismatchedUnits m ->
                  Expect.equal m.Stated [ kg ] "the stated unit"
                  Expect.isTrue m.Unstated "and the reading that states none"
              | other -> failtestf "expected a refusal, got %A" other
          }

          test "a reference in another unit than the declaration is refused even when the series agrees with itself" {
              match ask (Some g) (familyEvidence (allIn (Some kg))) with
              | MismatchedUnits m ->
                  Expect.equal m.Declared (Some g) "declared grams"
                  Expect.equal m.Stated [ kg ] "read in kilograms"
              | other -> failtestf "expected a refusal, got %A" other

              match ask None (familyEvidence (allIn (Some kg))) with
              | MismatchedUnits m -> Expect.equal m.Declared None "a margin stating no unit against kilograms"
              | other -> failtestf "expected a refusal, got %A" other
          }

          test "a refused set has no member list to reach" {
              let r = ask (Some kg) (familyEvidence (allBut "near" (Some g)))
              Expect.isNone (memberIds r) "a refusal is not an empty family"
          }

          test "a reading outside the scope is not part of the series" {
              // `unrelated` was never an alternative to the reference, so its grams are not a
              // reading the set would compare, and they refuse nothing.
              Expect.equal
                  (memberIds (ask (Some kg) (familyEvidence (allBut "unrelated" (Some g)))))
                  (Some [ "reference"; "near" ])
                  "the set is derived as it would be without it"
          }

          test "one unit throughout, declared, derives the set it always did — and says the unit" {
              let r = ask (Some kg) (familyEvidence (allIn (Some kg)))
              Expect.equal (memberIds r) (Some [ "reference"; "near" ]) "the two-member set"

              match r with
              | Derived s ->
                  Expect.stringContains
                      (Indistinguishability.describeSet s)
                      "margin of 0.05 kg on"
                      "the rendered line names the margin's unit"
              | other -> failtestf "expected a set, got %A" other
          }

          test "a record with no units anywhere, asked with no unit, reads exactly as before" {
              let e = familyEvidence (allIn None)
              let r = ask None e
              Expect.equal (memberIds r) (Some [ "reference"; "near" ]) "the two-member set"

              match Indistinguishability.declare e "holdoutError" 0.05 EveryAlternative with
              | Ok d ->
                  Expect.equal d.Unit None "declare states no unit"
                  Expect.equal (Indistinguishability.report (Some d) "reference" e) r "and answers identically"

                  match r with
                  | Derived s ->
                      Expect.isFalse ((Indistinguishability.describeSet s).Contains "0.05 kg") "and renders no unit"
                  | other -> failtestf "expected a set, got %A" other
              | Error r -> failtestf "refused: %s" (IndistinguishabilityRefusal.describe r)
          }

          test "a forest sibling whose series mixes units is reported, and answers no recurrence" {
              let forest =
                  { Forest.empty "f" "two units" with
                      Siblings =
                          [ { Sibling = "a"
                              Label = "a"
                              ExplorationRef = "memory:a" }
                            { Sibling = "b"
                              Label = "b"
                              ExplorationRef = "memory:b" } ] }

              let records =
                  Map.ofList
                      [ "a", familyEvidence (allIn (Some kg))
                        "b", familyEvidence (allBut "near" (Some g)) ]

              let declaration =
                  match
                      Indistinguishability.declareIn
                          (Some kg)
                          (familyEvidence (allIn (Some kg)))
                          "holdoutError"
                          0.05
                          EveryAlternative
                  with
                  | Ok d -> d
                  | Error r -> failtestf "refused: %s" (IndistinguishabilityRefusal.describe r)

              let view =
                  Indistinguishability.forestView (Some declaration) "reference" forest records

              Expect.equal view.DerivedIn [ "a" ] "only the sibling read in one unit could answer"

              match view.Reports |> List.find (fun (s, _) -> s = "b") |> snd with
              | MismatchedUnits m -> Expect.equal m.Stated [ g; kg ] "and the other says why it could not"
              | other -> failtestf "expected a refusal, got %A" other
          } ]

[<Tests>]
let wireTests =
    let roundTrip (encode: 'T -> JVal) (decode: JVal -> Result<'T, string>) (name: string) (value: 'T) =
        let text = Canonical.write Rendering.CrossHost (encode value)

        match Json.parse text |> Result.bind decode with
        | Ok back -> Expect.equal back value (name + " round-trips: " + text)
        | Error m -> failtestf "%s failed to decode (%s): %s" name text m

    testList
        "Series units / the wire"
        [ test "every new shape round-trips" {
              roundTrip Wire.encodeThresholds Wire.decodeThresholds "thresholds in kg" (thresholds (Some kg))

              let e = decisionEvidence (500.0, Some g) (Some kg)

              roundTrip
                  Wire.encodeVintageSeries
                  Wire.decodeVintageSeries
                  "series with units"
                  (Vintage.series "holdoutError" "kept" e)

              let r = Vintage.record (thresholds (Some kg)) theDecision e
              roundTrip Wire.encodeDecisionRecord Wire.decodeDecisionRecord "record with a mismatch" r

              let unstated =
                  Vintage.record (thresholds (Some kg)) theDecision (decisionEvidence (0.5, None) (Some kg))

              roundTrip Wire.encodeDecisionRecord Wire.decodeDecisionRecord "record naming an unstated unit" unstated

              for report in
                  [ ask (Some kg) (familyEvidence (allBut "near" (Some g)))
                    ask (Some kg) (familyEvidence (allBut "near" None))
                    ask None (familyEvidence (allIn (Some kg)))
                    ask (Some kg) (familyEvidence (allIn (Some kg))) ] do
                  roundTrip
                      Wire.encodeIndistinguishabilityReport
                      Wire.decodeIndistinguishabilityReport
                      "indistinguishability report"
                      report
          }

          test "a series' units must name a vintage it has a reading on, in canonical unit text" {
              let parse text =
                  Json.parse text |> Result.bind Wire.decodeVintageSeries

              let series units =
                  "{\"metric\":\"holdoutError\",\"readings\":[{\"datasetVersion\":\"v1\",\"observed\":0.5}],\"state\":\"kept\",\"units\":"
                  + units
                  + "}"

              Expect.isOk (parse (series "{\"v1\":\"kg\"}")) "a unit on a read vintage"
              Expect.isError (parse (series "{\"v9\":\"kg\"}")) "a unit on a vintage with no reading"
              Expect.isError (parse (series "{\"v1\":\"s-1\"}")) "a unit that is not canonical text"
          }

          test "a mismatch report's units are canonical unit text" {
              let parse text =
                  Json.parse text |> Result.bind Wire.decodeIndistinguishabilityReport

              Expect.isOk
                  (parse
                      "{\"$type\":\"mismatchedUnits\",\"metric\":\"holdoutError\",\"stated\":[\"kg\"],\"unstated\":true}")
                  "a mismatch against a margin stating none"

              Expect.isError
                  (parse
                      "{\"$type\":\"mismatchedUnits\",\"metric\":\"holdoutError\",\"stated\":[\"m.kg\"],\"unstated\":false}")
                  "a non-canonical spelling is refused"
          } ]
