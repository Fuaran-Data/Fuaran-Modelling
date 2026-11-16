// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  The canonical wire codec.
//
//  Every record and every op round-trips through canonical JSON: `$type`-tagged
//  discriminated objects, Ordinal-sorted keys, the pinned float layout. Two
//  hosts serialising the same value therefore produce identical BYTES, which is
//  what makes a content-addressed history converge and a hash mean anything.
//
//  There is no `null` on this wire. An absent optional is an absent key, and a
//  decoder reads a missing key as absence — never as a sentinel value.
// ============================================================================

/// Canonical encode / decode for the manager's records and op vocabulary.
[<RequireQualifiedAccess>]
module Wire =

    type private ResultBuilder() =
        member _.Bind(m: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) = Result.bind f m
        member _.Return(v: 'a) : Result<'a, 'e> = Ok v
        member _.ReturnFrom(m: Result<'a, 'e>) = m

    let private res = ResultBuilder()

    /// The string-error readers this file is written against. Core 0.37.0 retired its own
    /// (`Decode.getProp`, `strField`, `asString` and the rest) for the typed `Decoder`; these
    /// are its migration table's one-line replacements, kept under the old names so that every
    /// call site reads exactly as it did and the error stays a string at this boundary.
    module private Decode =
        let getProp (name: string) (el: JVal) : Result<JVal, string> =
            Decoder.describing (Decoder.field name Decoder.json) el

        let strField (name: string) (el: JVal) : Result<string, string> =
            Decoder.describing (Decoder.field name Decoder.str) el

        let asString (el: JVal) : Result<string, string> = Decoder.describing Decoder.str el
        let asFloat (el: JVal) : Result<float, string> = Decoder.describing Decoder.float el
        let asBool (el: JVal) : Result<bool, string> = Decoder.describing Decoder.bool el
        let asInt (el: JVal) : Result<int, string> = Decoder.describing Decoder.int el

        let mapList (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
            Decoder.describing Decoder.items el
            |> Result.bind (fun items ->
                (items, Ok [])
                ||> List.foldBack (fun item acc ->
                    match d item, acc with
                    | Ok x, Ok xs -> Ok(x :: xs)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e))

    // ---- small helpers -------------------------------------------------------

    let private floatField (name: string) (el: JVal) : Result<float, string> =
        Decode.getProp name el |> Result.bind Decode.asFloat

    let private boolField (name: string) (el: JVal) : Result<bool, string> =
        Decode.getProp name el |> Result.bind Decode.asBool

    let private listField (name: string) (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
        Decode.getProp name el |> Result.bind (Decode.mapList d)

    /// An optional list field: absent key ⇒ empty.
    ///
    /// The no-null discipline extended to a list, and the reason a list field can be ADDED to
    /// a record this wire already carries. Paired with `optListKey` below — emit nothing when
    /// there is nothing — it means a record written before the field existed still decodes,
    /// and a record with no entries still encodes to the bytes it always did.
    let private optListField (name: string) (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
        match el with
        | JObj fields when not (fields |> List.exists (fun (k, _) -> k = name)) -> Ok []
        | _ -> listField name d el

    /// Add a list key only when it has entries — the counterpart to `optKey`.
    let private optListKey (name: string) (encode: 'T -> JVal) (xs: 'T list) : (string * JVal) list =
        match xs with
        | [] -> []
        | _ -> [ name, JArr(xs |> List.map encode) ]

    /// An optional string field: absent key ⇒ `None`. Absence is not an error.
    let private optStrField (name: string) (el: JVal) : Result<string option, string> =
        match el with
        | JObj fields ->
            match fields |> List.tryFind (fun (k, _) -> k = name) with
            | None -> Ok None
            | Some(_, v) -> Decode.asString v |> Result.map Some
        | _ -> Error("expected object for '" + name + "'")

    /// Add a key only when the option is populated — the no-null discipline.
    let private optKey (name: string) (v: string option) : (string * JVal) list =
        match v with
        | Some s -> [ name, JStr s ]
        | None -> []

    /// An optional unit field (D39): absent key ⇒ `None`; present, it must be canonical unit text.
    let private optUnitField (name: string) (el: JVal) : Result<UnitOfMeasure option, string> =
        optStrField name el
        |> Result.bind (function
            | None -> Ok None
            | Some text -> UnitText.tryParse text |> Result.map Some)

    /// Add a unit key only when there is a unit — the no-null discipline, and the reason a gate
    /// or an outcome with no units encodes to the bytes it did before units existed.
    let private optUnitKey (name: string) (u: UnitOfMeasure option) : (string * JVal) list =
        optKey name (u |> Option.map Unit.render)

    /// The `$type` tag of a discriminated object.
    let private tagOf (el: JVal) : Result<string, string> =
        Decode.getProp "$type" el |> Result.bind Decode.asString

    let private unknownTag (what: string) (tag: string) (known: string list) : Result<'T, string> =
        Error(
            "unknown "
            + what
            + " '"
            + tag
            + "' (expected one of "
            + String.concat " | " known
            + ")"
        )

    // ---- SpecPayload ---------------------------------------------------------

    let encodeSpecPayload (p: SpecPayload) : JVal =
        JObj
            [ "dialect", JStr p.Dialect
              "format", JStr p.Format
              "body", JStr p.Body
              "specHash", JStr p.SpecHash ]

    let decodeSpecPayload (el: JVal) : Result<SpecPayload, string> =
        res {
            let! dialect = Decode.strField "dialect" el
            let! format = Decode.strField "format" el
            let! body = Decode.strField "body" el
            let! specHash = Decode.strField "specHash" el

            return
                { Dialect = dialect
                  Format = format
                  Body = body
                  SpecHash = specHash }
        }

    // ---- gates ---------------------------------------------------------------

    let encodeGateRequest (g: GateRequest) : JVal =
        JObj(
            [ "name", JStr g.Name; "threshold", JFloat g.Threshold ]
            @ optUnitKey "unit" g.Unit
            @ [ "direction", JStr(GateDirection.encode g.Direction) ]
        )

    let decodeGateRequest (el: JVal) : Result<GateRequest, string> =
        res {
            let! name = Decode.strField "name" el
            let! threshold = floatField "threshold" el
            let! unit = optUnitField "unit" el
            let! dirText = Decode.strField "direction" el
            let! direction = GateDirection.tryParse dirText

            return
                { Name = name
                  Threshold = threshold
                  Unit = unit
                  Direction = direction }
        }

    let encodeGateVerdict (g: GateVerdict) : JVal =
        JObj(
            [ "name", JStr g.Name; "threshold", JFloat g.Threshold ]
            @ optUnitKey "unit" g.Unit
            @ [ "direction", JStr(GateDirection.encode g.Direction)
                "observed", JFloat g.Observed
                "passed", JBool g.Passed ]
        )

    let decodeGateVerdict (el: JVal) : Result<GateVerdict, string> =
        res {
            let! name = Decode.strField "name" el
            let! threshold = floatField "threshold" el
            let! unit = optUnitField "unit" el
            let! dirText = Decode.strField "direction" el
            let! direction = GateDirection.tryParse dirText
            let! observed = floatField "observed" el
            let! passed = boolField "passed" el

            return
                { Name = name
                  Threshold = threshold
                  Unit = unit
                  Direction = direction
                  Observed = observed
                  Passed = passed }
        }

    // ---- composite key + outcome --------------------------------------------

    let encodeCompositeKey (k: CompositeKey) : JVal =
        JObj
            [ "specHash", JStr k.SpecHash
              "datasetVersion", JStr k.DatasetVersion
              "seed", JStr k.Seed
              "providerId", JStr k.ProviderId
              "providerVersion", JStr k.ProviderVersion ]

    let decodeCompositeKey (el: JVal) : Result<CompositeKey, string> =
        res {
            let! specHash = Decode.strField "specHash" el
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! seed = Decode.strField "seed" el
            let! providerId = Decode.strField "providerId" el
            let! providerVersion = Decode.strField "providerVersion" el

            return
                { SpecHash = specHash
                  DatasetVersion = datasetVersion
                  Seed = seed
                  ProviderId = providerId
                  ProviderVersion = providerVersion }
        }

    let private encodeDiagnostics (m: Map<string, float>) : JVal =
        JObj(m |> Map.toList |> List.map (fun (k, v) -> k, JFloat v))

    let private decodeDiagnostics (el: JVal) : Result<Map<string, float>, string> =
        match el with
        | JObj fields ->
            let rec go acc =
                function
                | [] -> Ok(Map.ofList (List.rev acc))
                | (k, v) :: rest ->
                    match Decode.asFloat v with
                    | Ok f -> go ((k, f) :: acc) rest
                    | Error m -> Error("diagnostic '" + k + "': " + m)

            go [] fields
        | _ -> Error "expected an object for diagnostics"

    /// The units map, omitted when it has no entries (D39): an outcome that states no units
    /// encodes to exactly the bytes it did before units existed.
    let private diagnosticUnitsKey (m: Map<string, UnitOfMeasure>) : (string * JVal) list =
        if Map.isEmpty m then
            []
        else
            [ "diagnosticUnits", JObj(m |> Map.toList |> List.map (fun (k, u) -> k, JStr(Unit.render u))) ]

    /// The units map back, every key naming a diagnostic the outcome carries.
    let private decodeDiagnosticUnits
        (diagnostics: Map<string, float>)
        (el: JVal)
        : Result<Map<string, UnitOfMeasure>, string> =
        match el with
        | JObj fields when not (fields |> List.exists (fun (k, _) -> k = "diagnosticUnits")) -> Ok Map.empty
        | _ ->
            Decode.getProp "diagnosticUnits" el
            |> Result.bind (function
                | JObj entries ->
                    (entries, Ok Map.empty)
                    ||> List.foldBack (fun (k, v) acc ->
                        acc
                        |> Result.bind (fun m ->
                            if not (Map.containsKey k diagnostics) then
                                Error("diagnosticUnits names '" + k + "', which is not a diagnostic of this outcome")
                            else
                                Decode.asString v
                                |> Result.bind UnitText.tryParse
                                |> Result.map (fun u -> Map.add k u m)
                                |> Result.mapError (fun e -> "diagnostic unit '" + k + "': " + e)))
                | _ -> Error "expected an object for diagnosticUnits")

    let encodeOutcome (o: OutcomeSnapshot) : JVal =
        JObj(
            [ "key", encodeCompositeKey o.Key
              "artifactRef", JStr o.ArtifactRef
              "diagnostics", encodeDiagnostics o.Diagnostics ]
            @ diagnosticUnitsKey o.DiagnosticUnits
            @ [ "gates", JArr(o.Gates |> List.map encodeGateVerdict)
                "submittedBy", JStr(SubmitterClass.encode o.SubmittedBy)
                "recordedAt", JStr o.RecordedAt ]
        )

    let decodeOutcome (el: JVal) : Result<OutcomeSnapshot, string> =
        res {
            let! keyEl = Decode.getProp "key" el
            let! key = decodeCompositeKey keyEl
            let! artifactRef = Decode.strField "artifactRef" el
            let! diagEl = Decode.getProp "diagnostics" el
            let! diagnostics = decodeDiagnostics diagEl
            let! diagnosticUnits = decodeDiagnosticUnits diagnostics el
            let! gates = listField "gates" decodeGateVerdict el

            // A verdict compares in its diagnostic's unit or not at all (D39), so an outcome
            // carrying one stated in another unit is a record of a comparison nobody may make.
            let! () =
                match gates |> List.tryFind (fun g -> g.Unit <> Map.tryFind g.Name diagnosticUnits) with
                | None -> Ok()
                | Some g -> Error("gate '" + g.Name + "' is not stated in the unit its diagnostic is reported in")

            let! submitterText = Decode.strField "submittedBy" el
            let! submittedBy = SubmitterClass.tryParse submitterText
            let! recordedAt = Decode.strField "recordedAt" el

            return
                { Key = key
                  ArtifactRef = artifactRef
                  Diagnostics = diagnostics
                  DiagnosticUnits = diagnosticUnits
                  Gates = gates
                  SubmittedBy = submittedBy
                  RecordedAt = recordedAt }
        }

    // ---- refusals ------------------------------------------------------------
    //
    //  Both refusal vocabularies are encoded HERE, ahead of the records that carry them,
    //  rather than beside the seams they describe. A replay receipt names a backend refusal
    //  or a fold rejection per branch that did not land, and the receipt is part of the
    //  folded record — so the vocabularies have to be encodable before the record is.

    let encodeBackendRefusal (r: BackendRefusal) : JVal =
        match r with
        | BudgetDenied(quota, spent) -> Canonical.typed "budgetDenied" [ "quota", JFloat quota; "spent", JFloat spent ]
        | GateFailed verdicts ->
            Canonical.typed "gateFailed" [ "verdicts", JArr(verdicts |> List.map encodeGateVerdict) ]
        | UnknownProvider known -> Canonical.typed "unknownProvider" [ "known", JArr(known |> List.map JStr) ]
        | BackendPolicyRefused rule -> Canonical.typed "policyRefused" [ "rule", JStr rule ]
        | GateUnitMismatch m ->
            Canonical.typed
                "gateUnitMismatch"
                ([ "gate", JStr m.Gate ]
                 @ optUnitKey "stated" m.Stated
                 @ optUnitKey "reported" m.Reported)

    let decodeBackendRefusal (el: JVal) : Result<BackendRefusal, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "budgetDenied" ->
                let! quota = floatField "quota" el
                let! spent = floatField "spent" el
                return BudgetDenied(quota, spent)
            | "gateFailed" ->
                let! verdicts = listField "verdicts" decodeGateVerdict el
                return GateFailed verdicts
            | "unknownProvider" ->
                let! known = listField "known" Decode.asString el
                return UnknownProvider known
            | "policyRefused" ->
                let! rule = Decode.strField "rule" el
                return BackendPolicyRefused rule
            | "gateUnitMismatch" ->
                let! gate = Decode.strField "gate" el
                let! stated = optUnitField "stated" el
                let! reported = optUnitField "reported" el

                return
                    GateUnitMismatch
                        { Gate = gate
                          Stated = stated
                          Reported = reported }
            | other ->
                return!
                    unknownTag
                        "backend refusal"
                        other
                        [ "budgetDenied"
                          "gateFailed"
                          "unknownProvider"
                          "policyRefused"
                          "gateUnitMismatch" ]
        }

    let encodeRejection (r: Rejection) : JVal =
        match r with
        | NotSeeded -> Canonical.typed "notSeeded" []
        | AlreadySeeded existing -> Canonical.typed "alreadySeeded" [ "existing", JStr existing ]
        | UnknownState id -> Canonical.typed "unknownState" [ "stateId", JStr id ]
        | DuplicateState id -> Canonical.typed "duplicateState" [ "stateId", JStr id ]
        | KilledState id -> Canonical.typed "killedState" [ "stateId", JStr id ]
        | UnknownProposal id -> Canonical.typed "unknownProposal" [ "proposalId", JStr id ]
        | ProposalAlreadyTabled id -> Canonical.typed "proposalAlreadyTabled" [ "proposalId", JStr id ]
        | ProposalAlreadyResolved id -> Canonical.typed "proposalAlreadyResolved" [ "proposalId", JStr id ]
        | DuplicateOutcome(id, key) ->
            Canonical.typed "duplicateOutcome" [ "stateId", JStr id; "key", encodeCompositeKey key ]
        | DuplicateReplay id -> Canonical.typed "duplicateReplay" [ "replayId", JStr id ]
        | HistoryCollision id -> Canonical.typed "historyCollision" [ "nodeId", JStr id ]
        | IncompleteReplay id -> Canonical.typed "incompleteReplay" [ "replayId", JStr id ]

    let decodeRejection (el: JVal) : Result<Rejection, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "notSeeded" -> return NotSeeded
            | "alreadySeeded" ->
                let! existing = Decode.strField "existing" el
                return AlreadySeeded existing
            | "unknownState" ->
                let! id = Decode.strField "stateId" el
                return UnknownState id
            | "duplicateState" ->
                let! id = Decode.strField "stateId" el
                return DuplicateState id
            | "killedState" ->
                let! id = Decode.strField "stateId" el
                return KilledState id
            | "unknownProposal" ->
                let! id = Decode.strField "proposalId" el
                return UnknownProposal id
            | "proposalAlreadyTabled" ->
                let! id = Decode.strField "proposalId" el
                return ProposalAlreadyTabled id
            | "proposalAlreadyResolved" ->
                let! id = Decode.strField "proposalId" el
                return ProposalAlreadyResolved id
            | "duplicateOutcome" ->
                let! id = Decode.strField "stateId" el
                let! keyEl = Decode.getProp "key" el
                let! key = decodeCompositeKey keyEl
                return DuplicateOutcome(id, key)
            | "duplicateReplay" ->
                let! id = Decode.strField "replayId" el
                return DuplicateReplay id
            | "incompleteReplay" ->
                let! id = Decode.strField "replayId" el
                return IncompleteReplay id
            | "historyCollision" ->
                let! id = Decode.strField "nodeId" el
                return HistoryCollision id
            | other ->
                return!
                    unknownTag
                        "rejection"
                        other
                        [ "notSeeded"
                          "alreadySeeded"
                          "unknownState"
                          "duplicateState"
                          "killedState"
                          "unknownProposal"
                          "proposalAlreadyTabled"
                          "proposalAlreadyResolved"
                          "duplicateOutcome"
                          "duplicateReplay"
                          "incompleteReplay"
                          "historyCollision" ]
        }

    // ---- the vintage axis ----------------------------------------------------

    let encodeReplayRefusal (r: ReplayRefusal) : JVal =
        match r with
        | NoBaselineFit -> Canonical.typed "noBaselineFit" []
        | ExecutorRefused refusal -> Canonical.typed "executorRefused" [ "refusal", encodeBackendRefusal refusal ]
        | NotRecorded rejection -> Canonical.typed "notRecorded" [ "rejection", encodeRejection rejection ]

    let decodeReplayRefusal (el: JVal) : Result<ReplayRefusal, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "noBaselineFit" -> return NoBaselineFit
            | "executorRefused" ->
                let! refusalEl = Decode.getProp "refusal" el
                let! refusal = decodeBackendRefusal refusalEl
                return ExecutorRefused refusal
            | "notRecorded" ->
                let! rejectionEl = Decode.getProp "rejection" el
                let! rejection = decodeRejection rejectionEl
                return NotRecorded rejection
            | other -> return! unknownTag "replay refusal" other [ "noBaselineFit"; "executorRefused"; "notRecorded" ]
        }

    /// A receipt's per-branch rows are objects rather than positional pairs, because a
    /// wire-level pair is the shape a reader gets silently backwards.
    let encodeReplayReceipt (r: ReplayReceipt) : JVal =
        JObj
            [ "replayId", JStr r.ReplayId
              "datasetVersion", JStr r.DatasetVersion
              "requested", JArr(r.Requested |> List.map JStr)
              "landed",
              JArr(
                  r.Landed
                  |> List.map (fun (state, key) -> JObj [ "state", JStr state; "key", encodeCompositeKey key ])
              )
              "refused",
              JArr(
                  r.Refused
                  |> List.map (fun (state, refusal) ->
                      JObj [ "state", JStr state; "refusal", encodeReplayRefusal refusal ])
              )
              "replayedBy", JStr r.ReplayedBy
              "replayedAt", JStr r.ReplayedAt ]

    let decodeReplayReceipt (el: JVal) : Result<ReplayReceipt, string> =
        res {
            let! replayId = Decode.strField "replayId" el
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! requested = listField "requested" Decode.asString el

            let! landed =
                listField
                    "landed"
                    (fun row ->
                        res {
                            let! state = Decode.strField "state" row
                            let! keyEl = Decode.getProp "key" row
                            let! key = decodeCompositeKey keyEl
                            return state, key
                        })
                    el

            let! refused =
                listField
                    "refused"
                    (fun row ->
                        res {
                            let! state = Decode.strField "state" row
                            let! refusalEl = Decode.getProp "refusal" row
                            let! refusal = decodeReplayRefusal refusalEl
                            return state, refusal
                        })
                    el

            let! replayedBy = Decode.strField "replayedBy" el
            let! replayedAt = Decode.strField "replayedAt" el

            return
                { ReplayId = replayId
                  DatasetVersion = datasetVersion
                  Requested = requested
                  Landed = landed
                  Refused = refused
                  ReplayedBy = replayedBy
                  ReplayedAt = replayedAt }
        }

    let encodeMetricPreference (p: MetricPreference) : JVal = JStr(MetricPreference.encode p)

    /// The margin's unit is omitted when it states none (D40), so thresholds declared before
    /// units existed encode to the bytes they always did.
    let encodeThresholds (t: CorrectionThresholds) : JVal =
        JObj(
            [ "metric", JStr t.Metric
              "prefer", encodeMetricPreference t.Prefer
              "minVintages", JInt t.MinVintages
              "minMargin", JFloat t.MinMargin
              "minSiblings", JInt t.MinSiblings ]
            @ optUnitKey "unit" t.Unit
        )

    let decodeThresholds (el: JVal) : Result<CorrectionThresholds, string> =
        res {
            let! metric = Decode.strField "metric" el
            let! preferText = Decode.strField "prefer" el
            let! prefer = MetricPreference.tryParse preferText
            let! minVintages = Decode.getProp "minVintages" el |> Result.bind Decode.asInt
            let! minMargin = floatField "minMargin" el
            let! minSiblings = Decode.getProp "minSiblings" el |> Result.bind Decode.asInt
            let! unit = optUnitField "unit" el

            return
                { Metric = metric
                  Prefer = prefer
                  MinVintages = minVintages
                  MinMargin = minMargin
                  MinSiblings = minSiblings
                  Unit = unit }
        }

    let encodeDecision (d: CurationDecision) : JVal =
        JObj [ "kept", JStr d.Kept; "setAside", JStr d.SetAside ]

    let decodeDecision (el: JVal) : Result<CurationDecision, string> =
        res {
            let! kept = Decode.strField "kept" el
            let! setAside = Decode.strField "setAside" el
            return { Kept = kept; SetAside = setAside }
        }

    /// The units map is omitted when no reading states a unit (D40), as an outcome's is (D39).
    let encodeVintageSeries (s: VintageSeries) : JVal =
        JObj(
            [ "state", JStr s.State
              "metric", JStr s.Metric
              "readings",
              JArr(
                  s.Readings
                  |> List.map (fun (v, x) -> JObj [ "datasetVersion", JStr v; "observed", JFloat x ])
              ) ]
            @ (if Map.isEmpty s.Units then
                   []
               else
                   [ "units", JObj(s.Units |> Map.toList |> List.map (fun (v, u) -> v, JStr(Unit.render u))) ])
        )

    let decodeVintageSeries (el: JVal) : Result<VintageSeries, string> =
        res {
            let! state = Decode.strField "state" el
            let! metric = Decode.strField "metric" el

            let! readings =
                listField
                    "readings"
                    (fun row ->
                        res {
                            let! v = Decode.strField "datasetVersion" row
                            let! x = floatField "observed" row
                            return v, x
                        })
                    el

            let! units =
                match el with
                | JObj fields when not (fields |> List.exists (fun (k, _) -> k = "units")) -> Ok Map.empty
                | _ ->
                    Decode.getProp "units" el
                    |> Result.bind (function
                        | JObj entries ->
                            (entries, Ok Map.empty)
                            ||> List.foldBack (fun (v, u) acc ->
                                acc
                                |> Result.bind (fun m ->
                                    if not (readings |> List.exists (fun (r, _) -> r = v)) then
                                        Error("units names vintage '" + v + "', which this series has no reading on")
                                    else
                                        Decode.asString u
                                        |> Result.bind UnitText.tryParse
                                        |> Result.map (fun u -> Map.add v u m)
                                        |> Result.mapError (fun e -> "unit on vintage '" + v + "': " + e)))
                        | _ -> Error "expected an object for units")

            return
                { State = state
                  Metric = metric
                  Readings = readings
                  Units = units }
        }

    let encodeComparison (c: VintageComparison) : JVal =
        JObj
            [ "datasetVersion", JStr c.DatasetVersion
              "keptReading", JFloat c.KeptReading
              "setAsideReading", JFloat c.SetAsideReading
              "setAsideLead", JFloat c.SetAsideLead
              "outOfTime", JBool c.OutOfTime ]

    let decodeComparison (el: JVal) : Result<VintageComparison, string> =
        res {
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! keptReading = floatField "keptReading" el
            let! setAsideReading = floatField "setAsideReading" el
            let! setAsideLead = floatField "setAsideLead" el
            let! outOfTime = boolField "outOfTime" el

            return
                { DatasetVersion = datasetVersion
                  KeptReading = keptReading
                  SetAsideReading = setAsideReading
                  SetAsideLead = setAsideLead
                  OutOfTime = outOfTime }
        }

    let encodeVintageUnits (u: VintageUnits) : JVal =
        JObj(
            [ "datasetVersion", JStr u.DatasetVersion ]
            @ optUnitKey "kept" u.Kept
            @ optUnitKey "setAside" u.SetAside
        )

    let decodeVintageUnits (el: JVal) : Result<VintageUnits, string> =
        res {
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! kept = optUnitField "kept" el
            let! setAside = optUnitField "setAside" el

            return
                { DatasetVersion = datasetVersion
                  Kept = kept
                  SetAside = setAside }
        }

    /// The record's unit and its unit-mismatched vintages are omitted when they carry nothing
    /// (D40), so a record over a series that states no units encodes as it always did.
    let encodeDecisionRecord (r: DecisionRecord) : JVal =
        JObj(
            [ "decision", encodeDecision r.Decision
              "metric", JStr r.Metric
              "prefer", encodeMetricPreference r.Prefer
              "comparisons", JArr(r.Comparisons |> List.map encodeComparison)
              "incomparable", JArr(r.Incomparable |> List.map JStr) ]
            @ optKey "baseline" r.Baseline
            @ optUnitKey "unit" r.Unit
            @ optListKey "unitMismatched" encodeVintageUnits r.UnitMismatched
        )

    let decodeDecisionRecord (el: JVal) : Result<DecisionRecord, string> =
        res {
            let! decisionEl = Decode.getProp "decision" el
            let! decision = decodeDecision decisionEl
            let! metric = Decode.strField "metric" el
            let! preferText = Decode.strField "prefer" el
            let! prefer = MetricPreference.tryParse preferText
            let! baseline = optStrField "baseline" el
            let! comparisons = listField "comparisons" decodeComparison el
            let! incomparable = listField "incomparable" Decode.asString el
            let! unit = optUnitField "unit" el
            let! unitMismatched = optListField "unitMismatched" decodeVintageUnits el

            return
                { Decision = decision
                  Metric = metric
                  Prefer = prefer
                  Unit = unit
                  Baseline = baseline
                  Comparisons = comparisons
                  Incomparable = incomparable
                  UnitMismatched = unitMismatched }
        }

    /// A correction finding carries the thresholds that were in force, and the encoding keeps
    /// them: a finding read back without its gate is a number nobody can weigh.
    let encodeCorrection (f: CorrectionFinding) : JVal =
        JObj
            [ "decision", encodeDecision f.Decision
              "metric", JStr f.Metric
              "leadingVintages", JArr(f.LeadingVintages |> List.map JStr)
              "smallestLead", JFloat f.SmallestLead
              "raisedIn", JArr(f.RaisedIn |> List.map JStr)
              "thresholds", encodeThresholds f.Thresholds ]

    let decodeCorrection (el: JVal) : Result<CorrectionFinding, string> =
        res {
            let! decisionEl = Decode.getProp "decision" el
            let! decision = decodeDecision decisionEl
            let! metric = Decode.strField "metric" el
            let! leadingVintages = listField "leadingVintages" Decode.asString el
            let! smallestLead = floatField "smallestLead" el
            let! raisedIn = listField "raisedIn" Decode.asString el
            let! thresholdsEl = Decode.getProp "thresholds" el
            let! thresholds = decodeThresholds thresholdsEl

            return
                { Decision = decision
                  Metric = metric
                  LeadingVintages = leadingVintages
                  SmallestLead = smallestLead
                  RaisedIn = raisedIn
                  Thresholds = thresholds }
        }

    let encodeRecurrence (r: ChosenRecurrence) : JVal =
        JObj
            [ "state", JStr r.State
              "label", JStr r.Label
              "chosenIn", JArr(r.ChosenIn |> List.map JStr)
              "setAsideIn", JArr(r.SetAsideIn |> List.map JStr)
              "uncuratedIn", JArr(r.UncuratedIn |> List.map JStr)
              "absentIn", JArr(r.AbsentIn |> List.map JStr)
              "divergentIn", JArr(r.DivergentIn |> List.map JStr) ]

    let decodeRecurrence (el: JVal) : Result<ChosenRecurrence, string> =
        res {
            let! state = Decode.strField "state" el
            let! label = Decode.strField "label" el
            let! chosenIn = listField "chosenIn" Decode.asString el
            let! setAsideIn = listField "setAsideIn" Decode.asString el
            let! uncuratedIn = listField "uncuratedIn" Decode.asString el
            let! absentIn = listField "absentIn" Decode.asString el
            let! divergentIn = listField "divergentIn" Decode.asString el

            return
                { State = state
                  Label = label
                  ChosenIn = chosenIn
                  SetAsideIn = setAsideIn
                  UncuratedIn = uncuratedIn
                  AbsentIn = absentIn
                  DivergentIn = divergentIn }
        }

    let encodeRecurrenceWithPower (r: RecurrenceWithPower) : JVal =
        JObj
            [ "recurrence", encodeRecurrence r.Recurrence
              "records",
              JArr(
                  r.Records
                  |> List.map (fun (sibling, rec') ->
                      JObj [ "sibling", JStr sibling; "record", encodeDecisionRecord rec' ])
              )
              "raisedIn", JArr(r.RaisedIn |> List.map JStr) ]

    let decodeRecurrenceWithPower (el: JVal) : Result<RecurrenceWithPower, string> =
        res {
            let! recurrenceEl = Decode.getProp "recurrence" el
            let! recurrence = decodeRecurrence recurrenceEl

            let! records =
                listField
                    "records"
                    (fun row ->
                        res {
                            let! sibling = Decode.strField "sibling" row
                            let! recordEl = Decode.getProp "record" row
                            let! record = decodeDecisionRecord recordEl
                            return sibling, record
                        })
                    el

            let! raisedIn = listField "raisedIn" Decode.asString el

            return
                { Recurrence = recurrence
                  Records = records
                  RaisedIn = raisedIn }
        }

    // ---- indistinguishability ------------------------------------------------

    let encodeIndistinguishabilityScope (s: IndistinguishabilityScope) : JVal =
        match s with
        | EveryAlternative -> Canonical.typed "everyAlternative" []
        | NamedAlternatives ids -> Canonical.typed "named" [ "states", JArr(ids |> List.map JStr) ]

    let decodeIndistinguishabilityScope (el: JVal) : Result<IndistinguishabilityScope, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "everyAlternative" -> return EveryAlternative
            | "named" ->
                let! ids = listField "states" Decode.asString el
                return NamedAlternatives ids
            | other -> return! unknownTag "indistinguishability scope" other [ "everyAlternative"; "named" ]
        }

    /// The declaration crosses the wire whole. A report read back without the declaration that
    /// produced it is a set of branches with no statement of what made them one — the same
    /// argument `encodeCorrection` makes for carrying its thresholds.
    let encodeIndistinguishabilityDeclaration (d: IndistinguishabilityDeclaration) : JVal =
        JObj(
            [ "metric", JStr d.Metric
              "margin", JFloat d.Margin
              "scope", encodeIndistinguishabilityScope d.Scope ]
            @ optUnitKey "unit" d.Unit
        )

    let decodeIndistinguishabilityDeclaration (el: JVal) : Result<IndistinguishabilityDeclaration, string> =
        res {
            let! metric = Decode.strField "metric" el
            let! margin = floatField "margin" el
            let! scopeEl = Decode.getProp "scope" el
            let! scope = decodeIndistinguishabilityScope scopeEl
            let! unit = optUnitField "unit" el

            return
                { Metric = metric
                  Margin = margin
                  Scope = scope
                  Unit = unit }
        }

    let encodeIndistinguishabilityRefusal (r: IndistinguishabilityRefusal) : JVal =
        match r with
        | UndeclaredMetric declarable ->
            Canonical.typed "undeclaredMetric" [ "declarable", JArr(declarable |> List.map JStr) ]
        | UnmeasuredMetric(metric, declarable) ->
            Canonical.typed
                "unmeasuredMetric"
                [ "metric", JStr metric; "declarable", JArr(declarable |> List.map JStr) ]
        | NegativeMargin margin -> Canonical.typed "negativeMargin" [ "margin", JFloat margin ]
        | UnknownScopeState(state, known) ->
            Canonical.typed "unknownScopeState" [ "state", JStr state; "known", JArr(known |> List.map JStr) ]

    let decodeIndistinguishabilityRefusal (el: JVal) : Result<IndistinguishabilityRefusal, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "undeclaredMetric" ->
                let! declarable = listField "declarable" Decode.asString el
                return UndeclaredMetric declarable
            | "unmeasuredMetric" ->
                let! metric = Decode.strField "metric" el
                let! declarable = listField "declarable" Decode.asString el
                return UnmeasuredMetric(metric, declarable)
            | "negativeMargin" ->
                let! margin = floatField "margin" el
                return NegativeMargin margin
            | "unknownScopeState" ->
                let! state = Decode.strField "state" el
                let! known = listField "known" Decode.asString el
                return UnknownScopeState(state, known)
            | other ->
                return!
                    unknownTag
                        "indistinguishability refusal"
                        other
                        [ "undeclaredMetric"
                          "unmeasuredMetric"
                          "negativeMargin"
                          "unknownScopeState" ]
        }

    let encodeIndistinguishabilityMember (m: IndistinguishabilityMember) : JVal =
        JObj
            [ "state", JStr m.State
              "reading", JFloat m.Reading
              "distance", JFloat m.Distance ]

    let decodeIndistinguishabilityMember (el: JVal) : Result<IndistinguishabilityMember, string> =
        res {
            let! state = Decode.strField "state" el
            let! reading = floatField "reading" el
            let! distance = floatField "distance" el

            return
                { State = state
                  Reading = reading
                  Distance = distance }
        }

    let encodeIndistinguishabilitySet (s: IndistinguishabilitySet) : JVal =
        JObj
            [ "reference", JStr s.Reference
              "declaration", encodeIndistinguishabilityDeclaration s.Declaration
              "datasetVersion", JStr s.DatasetVersion
              "outOfTime", JBool s.OutOfTime
              "referenceReading", JFloat s.ReferenceReading
              "members", JArr(s.Members |> List.map encodeIndistinguishabilityMember)
              // Emitted always, empty or not. This one is NOT the `optListKey` case the state's
              // additive fields are: an absent `unread` would read as "every alternative was
              // measured", which is a claim about the evidence rather than the absence of one.
              "unread", JArr(s.Unread |> List.map JStr) ]

    let decodeIndistinguishabilitySet (el: JVal) : Result<IndistinguishabilitySet, string> =
        res {
            let! reference = Decode.strField "reference" el
            let! declarationEl = Decode.getProp "declaration" el
            let! declaration = decodeIndistinguishabilityDeclaration declarationEl
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! outOfTime = boolField "outOfTime" el
            let! referenceReading = floatField "referenceReading" el
            let! members = listField "members" decodeIndistinguishabilityMember el
            let! unread = listField "unread" Decode.asString el

            return
                { Reference = reference
                  Declaration = declaration
                  DatasetVersion = datasetVersion
                  OutOfTime = outOfTime
                  ReferenceReading = referenceReading
                  Members = members
                  Unread = unread }
        }

    /// The report's cases are TAGS on the wire, not a set that may be missing.
    ///
    /// A serialised report that dropped the non-derived cases — or encoded them as an absent
    /// key — would collapse "nobody declared a margin" into "nothing was within it" one layer
    /// down, where nobody would look for it. The distinction the type exists to hold is only
    /// held if the encoding holds it too.
    let encodeIndistinguishabilityReport (r: IndistinguishabilityReport) : JVal =
        match r with
        | NoDeclaration -> Canonical.typed "noDeclaration" []
        | NoReferenceReading(state, metric) ->
            Canonical.typed "noReferenceReading" [ "state", JStr state; "metric", JStr metric ]
        | MismatchedUnits m ->
            Canonical.typed
                "mismatchedUnits"
                ([ "metric", JStr m.Metric
                   "stated", JArr(m.Stated |> List.map (Unit.render >> JStr))
                   "unstated", JBool m.Unstated ]
                 @ optUnitKey "declared" m.Declared)
        | Derived s -> Canonical.typed "derived" [ "set", encodeIndistinguishabilitySet s ]

    let decodeIndistinguishabilityReport (el: JVal) : Result<IndistinguishabilityReport, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "noDeclaration" -> return NoDeclaration
            | "noReferenceReading" ->
                let! state = Decode.strField "state" el
                let! metric = Decode.strField "metric" el
                return NoReferenceReading(state, metric)
            | "mismatchedUnits" ->
                let! metric = Decode.strField "metric" el
                let! declared = optUnitField "declared" el
                let! stated = listField "stated" (Decode.asString >> Result.bind UnitText.tryParse) el
                let! unstated = boolField "unstated" el

                return
                    MismatchedUnits
                        { Metric = metric
                          Declared = declared
                          Stated = stated
                          Unstated = unstated }
            | "derived" ->
                let! setEl = Decode.getProp "set" el
                let! s = decodeIndistinguishabilitySet setEl
                return Derived s
            | other ->
                return!
                    unknownTag
                        "indistinguishability report"
                        other
                        [ "noDeclaration"; "noReferenceReading"; "mismatchedUnits"; "derived" ]
        }

    let encodeIndistinguishabilityRecurrence (r: IndistinguishabilityRecurrence) : JVal =
        JObj
            [ "state", JStr r.State
              "label", JStr r.Label
              "indistinguishableIn", JArr(r.IndistinguishableIn |> List.map JStr)
              "separatedIn", JArr(r.SeparatedIn |> List.map JStr)
              "unreadIn", JArr(r.UnreadIn |> List.map JStr)
              "outOfScopeIn", JArr(r.OutOfScopeIn |> List.map JStr)
              "absentIn", JArr(r.AbsentIn |> List.map JStr) ]

    let decodeIndistinguishabilityRecurrence (el: JVal) : Result<IndistinguishabilityRecurrence, string> =
        res {
            let! state = Decode.strField "state" el
            let! label = Decode.strField "label" el
            let! indistinguishableIn = listField "indistinguishableIn" Decode.asString el
            let! separatedIn = listField "separatedIn" Decode.asString el
            let! unreadIn = listField "unreadIn" Decode.asString el
            let! outOfScopeIn = listField "outOfScopeIn" Decode.asString el
            let! absentIn = listField "absentIn" Decode.asString el

            return
                { State = state
                  Label = label
                  IndistinguishableIn = indistinguishableIn
                  SeparatedIn = separatedIn
                  UnreadIn = unreadIn
                  OutOfScopeIn = outOfScopeIn
                  AbsentIn = absentIn }
        }

    let encodeIndistinguishabilityForestView (v: IndistinguishabilityForestView) : JVal =
        JObj
            [ "reference", JStr v.Reference
              "reports",
              JArr(
                  v.Reports
                  |> List.map (fun (sibling, r) ->
                      JObj [ "sibling", JStr sibling; "report", encodeIndistinguishabilityReport r ])
              )
              "derivedIn", JArr(v.DerivedIn |> List.map JStr)
              "recurrence", JArr(v.Recurrence |> List.map encodeIndistinguishabilityRecurrence) ]

    let decodeIndistinguishabilityForestView (el: JVal) : Result<IndistinguishabilityForestView, string> =
        res {
            let! reference = Decode.strField "reference" el

            let! reports =
                listField
                    "reports"
                    (fun row ->
                        res {
                            let! sibling = Decode.strField "sibling" row
                            let! reportEl = Decode.getProp "report" row
                            let! report = decodeIndistinguishabilityReport reportEl
                            return sibling, report
                        })
                    el

            let! derivedIn = listField "derivedIn" Decode.asString el
            let! recurrence = listField "recurrence" decodeIndistinguishabilityRecurrence el

            return
                { Reference = reference
                  Reports = reports
                  DerivedIn = derivedIn
                  Recurrence = recurrence }
        }

    // ---- curation + proposals -----------------------------------------------

    let encodeCuration (c: CurationRecord) : JVal =
        JObj
            [ "verdict", JStr(CurationVerdict.encode c.Verdict)
              "reason", JStr c.Reason
              "curatedBy", JStr c.CuratedBy
              "curatedAt", JStr c.CuratedAt ]

    let decodeCuration (el: JVal) : Result<CurationRecord, string> =
        res {
            let! verdictText = Decode.strField "verdict" el
            let! verdict = CurationVerdict.tryParse verdictText
            let! reason = Decode.strField "reason" el
            let! curatedBy = Decode.strField "curatedBy" el
            let! curatedAt = Decode.strField "curatedAt" el

            return
                { Verdict = verdict
                  Reason = reason
                  CuratedBy = curatedBy
                  CuratedAt = curatedAt }
        }

    // ---- divergence declarations ---------------------------------------------

    let encodeDeclaration (d: DivergenceDeclaration) : JVal =
        JObj
            [ "invariantId", JStr d.InvariantId
              "justification", JStr d.Justification
              "declaredBy", JStr d.DeclaredBy
              "declaredAt", JStr d.DeclaredAt ]

    let decodeDeclaration (el: JVal) : Result<DivergenceDeclaration, string> =
        res {
            let! invariantId = Decode.strField "invariantId" el
            let! justification = Decode.strField "justification" el
            let! declaredBy = Decode.strField "declaredBy" el
            let! declaredAt = Decode.strField "declaredAt" el

            return
                { InvariantId = invariantId
                  Justification = justification
                  DeclaredBy = declaredBy
                  DeclaredAt = declaredAt }
        }

    // ---- the consensual axis -------------------------------------------------

    let encodePartyClass (c: PartyClass) : JVal =
        JObj [ "id", JStr c.Id; "description", JStr c.Description ]

    let decodePartyClass (el: JVal) : Result<PartyClass, string> =
        res {
            let! id = Decode.strField "id" el
            let! description = Decode.strField "description" el
            return { Id = id; Description = description }
        }

    let encodeAssent (a: AssentRecord) : JVal =
        JObj
            [ "party", JStr a.Party
              "verdict", JStr(AssentVerdict.encode a.Verdict)
              "reason", JStr a.Reason
              "assentedBy", JStr a.AssentedBy
              "assentedAt", JStr a.AssentedAt ]

    let decodeAssent (el: JVal) : Result<AssentRecord, string> =
        res {
            let! party = Decode.strField "party" el
            let! verdictText = Decode.strField "verdict" el
            let! verdict = AssentVerdict.tryParse verdictText
            let! reason = Decode.strField "reason" el
            let! assentedBy = Decode.strField "assentedBy" el
            let! assentedAt = Decode.strField "assentedAt" el

            return
                { Party = party
                  Verdict = verdict
                  Reason = reason
                  AssentedBy = assentedBy
                  AssentedAt = assentedAt }
        }

    let encodeAssentRefusal (r: AssentRefusal) : JVal =
        match r with
        | UndeclaredParty(party, declared) ->
            Canonical.typed "undeclaredParty" [ "party", JStr party; "declared", JArr(declared |> List.map JStr) ]
        | UnreasonedAssent party -> Canonical.typed "unreasonedAssent" [ "party", JStr party ]

    let decodeAssentRefusal (el: JVal) : Result<AssentRefusal, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "undeclaredParty" ->
                let! party = Decode.strField "party" el
                let! declared = listField "declared" Decode.asString el
                return UndeclaredParty(party, declared)
            | "unreasonedAssent" ->
                let! party = Decode.strField "party" el
                return UnreasonedAssent party
            | other -> return! unknownTag "assent refusal" other [ "undeclaredParty"; "unreasonedAssent" ]
        }

    /// The standing survives the encoding whole, `Silent` included. A serialised standing
    /// that dropped the parties who said nothing would read as agreement among those who
    /// spoke — the one reading the type exists to prevent.
    let encodeAssentStanding (st: AssentStanding) : JVal =
        JObj
            [ "state", JStr st.State
              "declared", JArr(st.Declared |> List.map JStr)
              "endorsed", JArr(st.Endorsed |> List.map JStr)
              "challenged", JArr(st.Challenged |> List.map JStr)
              "withdrawn", JArr(st.Withdrawn |> List.map JStr)
              "silent", JArr(st.Silent |> List.map JStr)
              "unrecognised", JArr(st.Unrecognised |> List.map JStr) ]

    let decodeAssentStanding (el: JVal) : Result<AssentStanding, string> =
        res {
            let! state = Decode.strField "state" el
            let! declared = listField "declared" Decode.asString el
            let! endorsed = listField "endorsed" Decode.asString el
            let! challenged = listField "challenged" Decode.asString el
            let! withdrawn = listField "withdrawn" Decode.asString el
            let! silent = listField "silent" Decode.asString el
            let! unrecognised = listField "unrecognised" Decode.asString el

            return
                { State = state
                  Declared = declared
                  Endorsed = endorsed
                  Challenged = challenged
                  Withdrawn = withdrawn
                  Silent = silent
                  Unrecognised = unrecognised }
        }

    let encodeTensionDirection (d: TensionDirection) : string =
        match d with
        | ChallengedWhereGatesHeld -> "challengedWhereGatesHeld"
        | EndorsedWhereGatesFailed -> "endorsedWhereGatesFailed"

    let decodeTensionDirection (s: string) : Result<TensionDirection, string> =
        match s with
        | "challengedWhereGatesHeld" -> Ok ChallengedWhereGatesHeld
        | "endorsedWhereGatesFailed" -> Ok EndorsedWhereGatesFailed
        | other ->
            Error(
                "unknown tension direction '"
                + other
                + "' (expected challengedWhereGatesHeld | endorsedWhereGatesFailed)"
            )

    let encodeAssentTension (t: AssentTension) : JVal =
        JObj
            [ "state", JStr t.State
              "direction", JStr(encodeTensionDirection t.Direction)
              "failedGates", JArr(t.FailedGates |> List.map JStr)
              "standing", encodeAssentStanding t.Standing ]

    let decodeAssentTension (el: JVal) : Result<AssentTension, string> =
        res {
            let! state = Decode.strField "state" el
            let! directionText = Decode.strField "direction" el
            let! direction = decodeTensionDirection directionText
            let! failedGates = listField "failedGates" Decode.asString el
            let! standingEl = Decode.getProp "standing" el
            let! standing = decodeAssentStanding standingEl

            return
                { State = state
                  Direction = direction
                  FailedGates = failedGates
                  Standing = standing }
        }

    let encodeAssentRecurrence (r: AssentRecurrence) : JVal =
        JObj
            [ "state", JStr r.State
              "label", JStr r.Label
              "consensusIn", JArr(r.ConsensusIn |> List.map JStr)
              "challengedIn", JArr(r.ChallengedIn |> List.map JStr)
              "partlyEndorsedIn", JArr(r.PartlyEndorsedIn |> List.map JStr)
              "silentIn", JArr(r.SilentIn |> List.map JStr)
              "absentIn", JArr(r.AbsentIn |> List.map JStr)
              "inTensionIn", JArr(r.InTensionIn |> List.map JStr) ]

    let decodeAssentRecurrence (el: JVal) : Result<AssentRecurrence, string> =
        res {
            let! state = Decode.strField "state" el
            let! label = Decode.strField "label" el
            let! consensusIn = listField "consensusIn" Decode.asString el
            let! challengedIn = listField "challengedIn" Decode.asString el
            let! partlyEndorsedIn = listField "partlyEndorsedIn" Decode.asString el
            let! silentIn = listField "silentIn" Decode.asString el
            let! absentIn = listField "absentIn" Decode.asString el
            let! inTensionIn = listField "inTensionIn" Decode.asString el

            return
                { State = state
                  Label = label
                  ConsensusIn = consensusIn
                  ChallengedIn = challengedIn
                  PartlyEndorsedIn = partlyEndorsedIn
                  SilentIn = silentIn
                  AbsentIn = absentIn
                  InTensionIn = inTensionIn }
        }

    let encodeProposal (p: BranchProposal) : JVal =
        JObj
            [ "proposalId", JStr p.ProposalId
              "from", JStr p.From
              "proposedId", JStr p.ProposedId
              "label", JStr p.Label
              "payload", encodeSpecPayload p.Payload
              "rationale", JStr p.Rationale
              "proposedBy", JStr p.ProposedBy ]

    let decodeProposal (el: JVal) : Result<BranchProposal, string> =
        res {
            let! proposalId = Decode.strField "proposalId" el
            let! from = Decode.strField "from" el
            let! proposedId = Decode.strField "proposedId" el
            let! label = Decode.strField "label" el
            let! payloadEl = Decode.getProp "payload" el
            let! payload = decodeSpecPayload payloadEl
            let! rationale = Decode.strField "rationale" el
            let! proposedBy = Decode.strField "proposedBy" el

            return
                { ProposalId = proposalId
                  From = from
                  ProposedId = proposedId
                  Label = label
                  Payload = payload
                  Rationale = rationale
                  ProposedBy = proposedBy }
        }

    let encodeProposalOutcome (o: ProposalOutcome) : JVal =
        match o with
        | Accepted -> Canonical.typed "accepted" []
        | Declined reason -> Canonical.typed "declined" [ "reason", JStr reason ]

    let decodeProposalOutcome (el: JVal) : Result<ProposalOutcome, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "accepted" -> return Accepted
            | "declined" ->
                let! reason = Decode.strField "reason" el
                return Declined reason
            | other -> return! unknownTag "proposal outcome" other [ "accepted"; "declined" ]
        }

    // ---- state ---------------------------------------------------------------

    let encodeOrigin (o: StateOrigin) : JVal =
        match o with
        | Seeded -> Canonical.typed "seeded" []
        | ForkedFrom from -> Canonical.typed "forked" [ "from", JStr from ]
        | CherryPickedFrom(source, onto) -> Canonical.typed "cherryPicked" [ "source", JStr source; "onto", JStr onto ]
        | FromProposal(proposalId, from) ->
            Canonical.typed "fromProposal" [ "proposalId", JStr proposalId; "from", JStr from ]

    let decodeOrigin (el: JVal) : Result<StateOrigin, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "seeded" -> return Seeded
            | "forked" ->
                let! from = Decode.strField "from" el
                return ForkedFrom from
            | "cherryPicked" ->
                let! source = Decode.strField "source" el
                let! onto = Decode.strField "onto" el
                return CherryPickedFrom(source, onto)
            | "fromProposal" ->
                let! proposalId = Decode.strField "proposalId" el
                let! from = Decode.strField "from" el
                return FromProposal(proposalId, from)
            | other -> return! unknownTag "state origin" other [ "seeded"; "forked"; "cherryPicked"; "fromProposal" ]
        }

    let encodeState (s: SpecState) : JVal =
        JObj(
            [ "id", JStr s.Id
              "label", JStr s.Label
              "origin", encodeOrigin s.Origin
              "payload", encodeSpecPayload s.Payload
              "curations", JArr(s.Curations |> List.map encodeCuration)
              "outcomes", JArr(s.Outcomes |> List.map encodeOutcome) ]
            @ optKey "parent" s.Parent
            // Emitted only when populated. That is what made this field ADDITIVE: a state
            // recorded before divergences existed decodes unchanged, and a state with none
            // encodes to exactly the bytes it did before. `curations` and `outcomes` are
            // always emitted because they were there from the start and moving them now
            // would change bytes for nothing.
            @ optListKey "divergences" encodeDeclaration s.Divergences
            // Same construction, same argument, one release later: a state recorded before
            // assent existed decodes unchanged, and a state nobody has assented to encodes to
            // exactly the bytes it did before. The `divergences` byte pin already asserts the
            // shape; this field inherits it rather than restating it.
            @ optListKey "assents" encodeAssent s.Assents
        )

    let decodeState (el: JVal) : Result<SpecState, string> =
        res {
            let! id = Decode.strField "id" el
            let! label = Decode.strField "label" el
            let! parent = optStrField "parent" el
            let! originEl = Decode.getProp "origin" el
            let! origin = decodeOrigin originEl
            let! payloadEl = Decode.getProp "payload" el
            let! payload = decodeSpecPayload payloadEl
            let! curations = listField "curations" decodeCuration el
            let! outcomes = listField "outcomes" decodeOutcome el
            let! divergences = optListField "divergences" decodeDeclaration el
            let! assents = optListField "assents" decodeAssent el

            return
                { Id = id
                  Label = label
                  Parent = parent
                  Origin = origin
                  Payload = payload
                  Curations = curations
                  Outcomes = outcomes
                  Divergences = divergences
                  Assents = assents }
        }

    // ---- the folded exploration ---------------------------------------------

    /// The whole decision record as one canonical value. States are emitted in
    /// EXPLORATION order, not key order, so the encoding carries history rather than
    /// an alphabet.
    let encodeExploration (e: Exploration) : JVal =
        JObj(
            [ "states", JArr(Query.states e |> List.map encodeState)
              "proposals", JArr(e.Proposals |> Map.toList |> List.map (fun (_, p) -> encodeProposal p))
              "resolutions",
              JArr(
                  e.Resolutions
                  |> Map.toList
                  |> List.map (fun (id, o) -> JObj [ "proposalId", JStr id; "outcome", encodeProposalOutcome o ])
              ) ]
            @ optKey "root" e.Root
            @ optKey "head" e.Head
            // Emitted only when populated, for the reason `divergences` is on a state: a
            // record written before replays existed decodes unchanged, and a record with no
            // campaigns encodes to exactly the bytes it always did. A byte pin asserts it
            // rather than leaving it as an intention.
            @ optListKey "replays" encodeReplayReceipt e.Replays
        )

    let decodeExploration (el: JVal) : Result<Exploration, string> =
        res {
            let! states = listField "states" decodeState el
            let! proposals = listField "proposals" decodeProposal el

            let! resolutions =
                listField
                    "resolutions"
                    (fun r ->
                        res {
                            let! id = Decode.strField "proposalId" r
                            let! outEl = Decode.getProp "outcome" r
                            let! outcome = decodeProposalOutcome outEl
                            return id, outcome
                        })
                    el

            let! root = optStrField "root" el
            let! head = optStrField "head" el
            let! replays = optListField "replays" decodeReplayReceipt el

            return
                { States = states |> List.map (fun s -> s.Id, s) |> Map.ofList
                  Order = states |> List.map (fun s -> s.Id)
                  Root = root
                  Head = head
                  Proposals = proposals |> List.map (fun p -> p.ProposalId, p) |> Map.ofList
                  Resolutions = Map.ofList resolutions
                  Replays = replays }
        }

    // ---- the op vocabulary ---------------------------------------------------

    let encodeOp (op: ExplorationOp) : JVal =
        match op with
        | Seed s ->
            Canonical.typed
                (OpKind.encode SeedKind)
                [ "stateId", JStr s.StateId
                  "label", JStr s.Label
                  "payload", encodeSpecPayload s.Payload ]
        | Fork f ->
            Canonical.typed
                (OpKind.encode ForkKind)
                [ "stateId", JStr f.StateId
                  "from", JStr f.From
                  "label", JStr f.Label
                  "payload", encodeSpecPayload f.Payload ]
        | ReturnTo r -> Canonical.typed (OpKind.encode ReturnKind) [ "to", JStr r.To ]
        | CherryPick c ->
            Canonical.typed
                (OpKind.encode CherryPickKind)
                [ "stateId", JStr c.StateId
                  "source", JStr c.Source
                  "onto", JStr c.Onto
                  "label", JStr c.Label
                  "payload", encodeSpecPayload c.Payload ]
        | Curate c ->
            Canonical.typed (OpKind.encode CurateKind) [ "stateId", JStr c.StateId; "record", encodeCuration c.Record ]
        | Annotate a ->
            Canonical.typed
                (OpKind.encode AnnotateKind)
                [ "stateId", JStr a.StateId; "snapshot", encodeOutcome a.Snapshot ]
        | Declare d ->
            Canonical.typed
                (OpKind.encode DeclareKind)
                [ "stateId", JStr d.StateId; "declaration", encodeDeclaration d.Declaration ]
        | Assent a ->
            Canonical.typed (OpKind.encode AssentKind) [ "stateId", JStr a.StateId; "record", encodeAssent a.Record ]
        | Replay r -> Canonical.typed (OpKind.encode ReplayKind) [ "receipt", encodeReplayReceipt r.Receipt ]
        | Propose p -> Canonical.typed (OpKind.encode ProposeKind) [ "proposal", encodeProposal p.Proposal ]
        | Resolve r ->
            Canonical.typed
                (OpKind.encode ResolveKind)
                [ "proposalId", JStr r.ProposalId
                  "outcome", encodeProposalOutcome r.Outcome
                  "resolvedBy", JStr r.ResolvedBy ]

    let decodeOp (el: JVal) : Result<ExplorationOp, string> =
        res {
            let! tag = tagOf el
            let! kind = OpKind.tryParse tag

            match kind with
            | SeedKind ->
                let! stateId = Decode.strField "stateId" el
                let! label = Decode.strField "label" el
                let! payloadEl = Decode.getProp "payload" el
                let! payload = decodeSpecPayload payloadEl

                return
                    Seed
                        { StateId = stateId
                          Label = label
                          Payload = payload }
            | ForkKind ->
                let! stateId = Decode.strField "stateId" el
                let! from = Decode.strField "from" el
                let! label = Decode.strField "label" el
                let! payloadEl = Decode.getProp "payload" el
                let! payload = decodeSpecPayload payloadEl

                return
                    Fork
                        { StateId = stateId
                          From = from
                          Label = label
                          Payload = payload }
            | ReturnKind ->
                let! target = Decode.strField "to" el
                return ReturnTo { To = target }
            | CherryPickKind ->
                let! stateId = Decode.strField "stateId" el
                let! source = Decode.strField "source" el
                let! onto = Decode.strField "onto" el
                let! label = Decode.strField "label" el
                let! payloadEl = Decode.getProp "payload" el
                let! payload = decodeSpecPayload payloadEl

                return
                    CherryPick
                        { StateId = stateId
                          Source = source
                          Onto = onto
                          Label = label
                          Payload = payload }
            | CurateKind ->
                let! stateId = Decode.strField "stateId" el
                let! recordEl = Decode.getProp "record" el
                let! record = decodeCuration recordEl
                return Curate { StateId = stateId; Record = record }
            | AnnotateKind ->
                let! stateId = Decode.strField "stateId" el
                let! snapshotEl = Decode.getProp "snapshot" el
                let! snapshot = decodeOutcome snapshotEl

                return
                    Annotate
                        { StateId = stateId
                          Snapshot = snapshot }
            | DeclareKind ->
                let! stateId = Decode.strField "stateId" el
                let! declEl = Decode.getProp "declaration" el
                let! declaration = decodeDeclaration declEl

                return
                    Declare
                        { StateId = stateId
                          Declaration = declaration }
            | AssentKind ->
                let! stateId = Decode.strField "stateId" el
                let! recordEl = Decode.getProp "record" el
                let! record = decodeAssent recordEl
                return Assent { StateId = stateId; Record = record }
            | ReplayKind ->
                let! receiptEl = Decode.getProp "receipt" el
                let! receipt = decodeReplayReceipt receiptEl
                return Replay { Receipt = receipt }
            | ProposeKind ->
                let! proposalEl = Decode.getProp "proposal" el
                let! proposal = decodeProposal proposalEl
                return Propose { Proposal = proposal }
            | ResolveKind ->
                let! proposalId = Decode.strField "proposalId" el
                let! outcomeEl = Decode.getProp "outcome" el
                let! outcome = decodeProposalOutcome outcomeEl
                let! resolvedBy = Decode.strField "resolvedBy" el

                return
                    Resolve
                        { ProposalId = proposalId
                          Outcome = outcome
                          ResolvedBy = resolvedBy }
        }

    // ---- policy --------------------------------------------------------------

    let encodeDirective (d: Directive) : JVal =
        JObj
            [ "subject", JStr d.Subject
              "capabilities", JArr(d.Capabilities |> Set.toList |> List.map (Capability.encode >> JStr)) ]

    let decodeDirective (el: JVal) : Result<Directive, string> =
        res {
            let! subject = Decode.strField "subject" el
            let! capTexts = listField "capabilities" Decode.asString el

            let! caps =
                capTexts
                |> List.fold
                    (fun acc t ->
                        match acc, Capability.tryParse t with
                        | Error e, _ -> Error e
                        | Ok xs, Ok c -> Ok(c :: xs)
                        | Ok _, Error e -> Error e)
                    (Ok [])

            return
                { Subject = subject
                  Capabilities = Set.ofList caps }
        }

    let encodeObjective (o: Objective) : JVal =
        JObj
            [ "id", JStr o.Id
              "title", JStr o.Title
              "active", JBool o.Active
              "directives", JArr(o.Directives |> List.map encodeDirective) ]

    let decodeObjective (el: JVal) : Result<Objective, string> =
        res {
            let! id = Decode.strField "id" el
            let! title = Decode.strField "title" el
            let! active = boolField "active" el
            let! directives = listField "directives" decodeDirective el

            return
                { Id = id
                  Title = title
                  Active = active
                  Directives = directives }
        }

    let encodePolicy (p: Policy) : JVal =
        JObj(
            [ "objectives", JArr(p.Objectives |> List.map encodeObjective) ]
            // Emitted only when declared, so a policy that declares no party class — and
            // therefore every forest recorded before party classes existed — encodes to
            // exactly the bytes it always did. The forest byte pin asserts it.
            @ optListKey "parties" encodePartyClass p.Parties
        )

    let decodePolicy (el: JVal) : Result<Policy, string> =
        res {
            let! objectives = listField "objectives" decodeObjective el
            let! parties = optListField "parties" decodePartyClass el

            return
                { Objectives = objectives
                  Parties = parties }
        }

    let encodePolicyRefusal (r: PolicyRefusal) : JVal =
        JObj
            [ "subject", JStr r.Subject
              "attempted", JStr r.Attempted
              "reason", JStr r.Reason
              "allowed", JArr(r.Allowed |> List.map JStr) ]

    let decodePolicyRefusal (el: JVal) : Result<PolicyRefusal, string> =
        res {
            let! subject = Decode.strField "subject" el
            let! attempted = Decode.strField "attempted" el
            let! reason = Decode.strField "reason" el
            let! allowed = listField "allowed" Decode.asString el

            return
                { Subject = subject
                  Attempted = attempted
                  Reason = reason
                  Allowed = allowed }
        }

    // ---- the forest and the invariant seam -----------------------------------

    let encodeSiblingRef (r: SiblingRef) : JVal =
        JObj
            [ "sibling", JStr r.Sibling
              "label", JStr r.Label
              "explorationRef", JStr r.ExplorationRef ]

    let decodeSiblingRef (el: JVal) : Result<SiblingRef, string> =
        res {
            let! sibling = Decode.strField "sibling" el
            let! label = Decode.strField "label" el
            let! explorationRef = Decode.strField "explorationRef" el

            return
                { Sibling = sibling
                  Label = label
                  ExplorationRef = explorationRef }
        }

    /// A forest carries references and a policy, and nothing of the siblings themselves — so
    /// this encoding is the whole stored artefact. There is no forest op stream to serialise,
    /// because there is no forest op.
    let encodeForest (f: Forest) : JVal =
        JObj(
            [ "id", JStr f.Id
              "title", JStr f.Title
              "siblings", JArr(f.Siblings |> List.map encodeSiblingRef)
              "policy", encodePolicy f.Policy ]
            // Emitted only when declared. A forest that declares no correction thresholds
            // encodes to exactly the bytes it did before they existed — and decodes back to
            // `None`, which is the state in which it raises no question at all. Additive by
            // construction, and the silent default stays silent.
            @ (match f.Thresholds with
               | Some t -> [ "thresholds", encodeThresholds t ]
               | None -> [])
        )

    let decodeForest (el: JVal) : Result<Forest, string> =
        res {
            let! id = Decode.strField "id" el
            let! title = Decode.strField "title" el
            let! siblings = listField "siblings" decodeSiblingRef el
            let! policyEl = Decode.getProp "policy" el
            let! policy = decodePolicy policyEl

            let! thresholds =
                match el with
                | JObj fields when not (fields |> List.exists (fun (k, _) -> k = "thresholds")) -> Ok None
                | _ ->
                    Decode.getProp "thresholds" el
                    |> Result.bind decodeThresholds
                    |> Result.map Some

            return
                { Id = id
                  Title = title
                  Siblings = siblings
                  Policy = policy
                  Thresholds = thresholds }
        }

    let encodeSelection (s: StateSelection) : JVal =
        match s with
        | KeptStates -> Canonical.typed "kept" []
        | FrontierStates -> Canonical.typed "frontier" []
        | NamedStates ids -> Canonical.typed "named" [ "states", JArr(ids |> List.map JStr) ]

    let decodeSelection (el: JVal) : Result<StateSelection, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "kept" -> return KeptStates
            | "frontier" -> return FrontierStates
            | "named" ->
                let! ids = listField "states" Decode.asString el
                return NamedStates ids
            | other -> return! unknownTag "state selection" other [ "kept"; "frontier"; "named" ]
        }

    let encodeProjectedState (p: ProjectedState) : JVal =
        JObj
            [ "sibling", JStr p.Sibling
              "state", JStr p.State
              "payload", encodeSpecPayload p.Payload ]

    let decodeProjectedState (el: JVal) : Result<ProjectedState, string> =
        res {
            let! sibling = Decode.strField "sibling" el
            let! state = Decode.strField "state" el
            let! payloadEl = Decode.getProp "payload" el
            let! payload = decodeSpecPayload payloadEl

            return
                { Sibling = sibling
                  State = state
                  Payload = payload }
        }

    let encodeInvariantFinding (x: InvariantFinding) : JVal =
        JObj
            [ "invariantId", JStr x.InvariantId
              "siblings", JArr(x.Siblings |> List.map JStr)
              "detail", JStr x.Detail ]

    let decodeInvariantFinding (el: JVal) : Result<InvariantFinding, string> =
        res {
            let! invariantId = Decode.strField "invariantId" el
            let! siblings = listField "siblings" Decode.asString el
            let! detail = Decode.strField "detail" el

            return
                { InvariantId = invariantId
                  Siblings = siblings
                  Detail = detail }
        }

    let encodeStanding (s: Standing) : string =
        match s with
        | Unaccounted -> "unaccounted"
        | Downgraded -> "downgraded"

    let decodeStanding (s: string) : Result<Standing, string> =
        match s with
        | "unaccounted" -> Ok Unaccounted
        | "downgraded" -> Ok Downgraded
        | other -> Error("unknown standing '" + other + "' (expected unaccounted | downgraded)")

    /// The finding survives the encoding as well as the assessment. A serialised assessment
    /// that dropped downgraded findings would be the same erasure the type refuses, performed
    /// one layer down where nobody would look for it.
    let encodeAssessedFinding (a: AssessedFinding) : JVal =
        JObj
            [ "finding", encodeInvariantFinding a.Finding
              "standing", JStr(encodeStanding a.Standing)
              "declarations",
              JArr(
                  a.Declarations
                  |> List.map (fun (sibling, state, d) ->
                      JObj
                          [ "sibling", JStr sibling
                            "state", JStr state
                            "declaration", encodeDeclaration d ])
              )
              "undeclared", JArr(a.Undeclared |> List.map JStr) ]

    let decodeAssessedFinding (el: JVal) : Result<AssessedFinding, string> =
        res {
            let! findingEl = Decode.getProp "finding" el
            let! finding = decodeInvariantFinding findingEl
            let! standingText = Decode.strField "standing" el
            let! standing = decodeStanding standingText

            let! declarations =
                listField
                    "declarations"
                    (fun d ->
                        res {
                            let! sibling = Decode.strField "sibling" d
                            let! state = Decode.strField "state" d
                            let! declEl = Decode.getProp "declaration" d
                            let! decl = decodeDeclaration declEl
                            return sibling, state, decl
                        })
                    el

            let! undeclared = listField "undeclared" Decode.asString el

            return
                { Finding = finding
                  Standing = standing
                  Declarations = declarations
                  Undeclared = undeclared }
        }

    let encodeAssessment (a: ForestAssessment) : JVal =
        JObj
            [ "forest", JStr a.Forest
              "selection", encodeSelection a.Selection
              "projected", JInt a.Projected
              "invariants", JArr(a.Invariants |> List.map JStr)
              "findings", JArr(a.Findings |> List.map encodeAssessedFinding) ]

    let decodeAssessment (el: JVal) : Result<ForestAssessment, string> =
        res {
            let! forest = Decode.strField "forest" el
            let! selectionEl = Decode.getProp "selection" el
            let! selection = decodeSelection selectionEl
            let! projected = Decode.getProp "projected" el |> Result.bind Decode.asInt
            let! invariants = listField "invariants" Decode.asString el
            let! findings = listField "findings" decodeAssessedFinding el

            return
                { Forest = forest
                  Selection = selection
                  Projected = projected
                  Invariants = invariants
                  Findings = findings }
        }

    let encodeDeclarationRefusal (r: DeclarationRefusal) : JVal =
        match r with
        | UnknownInvariant(id, known) ->
            Canonical.typed "unknownInvariant" [ "invariantId", JStr id; "known", JArr(known |> List.map JStr) ]
        | UnjustifiedDivergence id -> Canonical.typed "unjustifiedDivergence" [ "invariantId", JStr id ]

    let decodeDeclarationRefusal (el: JVal) : Result<DeclarationRefusal, string> =
        res {
            let! tag = tagOf el

            match tag with
            | "unknownInvariant" ->
                let! id = Decode.strField "invariantId" el
                let! known = listField "known" Decode.asString el
                return UnknownInvariant(id, known)
            | "unjustifiedDivergence" ->
                let! id = Decode.strField "invariantId" el
                return UnjustifiedDivergence id
            | other -> return! unknownTag "declaration refusal" other [ "unknownInvariant"; "unjustifiedDivergence" ]
        }

    // ---- the execution seam --------------------------------------------------

    let encodeSubmission (s: FitSubmission) : JVal =
        JObj
            [ "stateId", JStr s.StateId
              "payload", encodeSpecPayload s.Payload
              "datasetVersion", JStr s.DatasetVersion
              "seed", JStr s.Seed
              "gates", JArr(s.Gates |> List.map encodeGateRequest)
              "submittedBy", JStr(SubmitterClass.encode s.SubmittedBy) ]

    let decodeSubmission (el: JVal) : Result<FitSubmission, string> =
        res {
            let! stateId = Decode.strField "stateId" el
            let! payloadEl = Decode.getProp "payload" el
            let! payload = decodeSpecPayload payloadEl
            let! datasetVersion = Decode.strField "datasetVersion" el
            let! seed = Decode.strField "seed" el
            let! gates = listField "gates" decodeGateRequest el
            let! submitterText = Decode.strField "submittedBy" el
            let! submittedBy = SubmitterClass.tryParse submitterText

            return
                { StateId = stateId
                  Payload = payload
                  DatasetVersion = datasetVersion
                  Seed = seed
                  Gates = gates
                  SubmittedBy = submittedBy }
        }

    // ---- canonical text ------------------------------------------------------

    /// Canonical bytes for one op — the form the content-addressed history hashes.
    let renderOp (op: ExplorationOp) : string =
        Canonical.write Rendering.CrossHost (encodeOp op)

    let parseOp (json: string) : Result<ExplorationOp, string> = Json.parse json |> Result.bind decodeOp

    let renderExploration (e: Exploration) : string =
        Canonical.write Rendering.CrossHost (encodeExploration e)

    let parseExploration (json: string) : Result<Exploration, string> =
        Json.parse json |> Result.bind decodeExploration

    /// Canonical bytes for a forest declaration — the form a host stores it in.
    let renderForest (f: Forest) : string =
        Canonical.write Rendering.CrossHost (encodeForest f)

    let parseForest (json: string) : Result<Forest, string> =
        Json.parse json |> Result.bind decodeForest

    let renderAssessment (a: ForestAssessment) : string =
        Canonical.write Rendering.CrossHost (encodeAssessment a)

    let parseAssessment (json: string) : Result<ForestAssessment, string> =
        Json.parse json |> Result.bind decodeAssessment

    /// Canonical bytes for an indistinguishability declaration — the form a host stores the
    /// question in, beside the record it is asked of.
    let renderIndistinguishabilityDeclaration (d: IndistinguishabilityDeclaration) : string =
        Canonical.write Rendering.CrossHost (encodeIndistinguishabilityDeclaration d)

    let parseIndistinguishabilityDeclaration (json: string) : Result<IndistinguishabilityDeclaration, string> =
        Json.parse json |> Result.bind decodeIndistinguishabilityDeclaration

    /// Canonical bytes for the derived answer — the form a downstream multiplicity report
    /// consumes, whether or not a set was derived at all.
    let renderIndistinguishabilityReport (r: IndistinguishabilityReport) : string =
        Canonical.write Rendering.CrossHost (encodeIndistinguishabilityReport r)

    let parseIndistinguishabilityReport (json: string) : Result<IndistinguishabilityReport, string> =
        Json.parse json |> Result.bind decodeIndistinguishabilityReport

    let renderIndistinguishabilityForestView (v: IndistinguishabilityForestView) : string =
        Canonical.write Rendering.CrossHost (encodeIndistinguishabilityForestView v)

    let parseIndistinguishabilityForestView (json: string) : Result<IndistinguishabilityForestView, string> =
        Json.parse json |> Result.bind decodeIndistinguishabilityForestView
