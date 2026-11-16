// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The codec: mirror shapes to canonical bytes, and back.
//
//  Two halves that are NOT symmetric, on purpose.
//
//  Writing is total: a mirror value is by construction well-formed, so encoding
//  it cannot fail, and the emitter applies §5's sort keys itself rather than
//  trusting a caller to have applied them. A list whose order is load-bearing is
//  sorted here or it is not sorted at all.
//
//  Reading is where refusal lives. Every document this reader declines carries
//  the §7.9 REJECT CLASS naming what was wrong AND the §5.7 refusal a conformant
//  executor emits for it — two different things that are easy to conflate: the
//  class is a stable corpus identifier, the refusal is what crosses the wire.
//
//  One refusal is deliberately NOT here. §4.2 rule 2 forbids a reader from
//  checking `specHash` against `specPayload` — not even where the algorithm
//  named is one it implements — because a party that verifies a hash it was
//  handed has no conformant action available in the failing case. That check is
//  a SUBMITTER's pre-emit obligation and lives in `Submitter` below, apart from
//  the reader, so that no future edit can quietly wire it into the read path.
// ============================================================================

/// The stable identifiers for documents an implementation must refuse (§7.9).
///
/// The class is normative; the wording of a refusal is not. Each but the last maps
/// to a §5.7 refusal a conformant executor emits; the last is a submitter's pre-emit
/// obligation, where the document is never emitted and so no refusal ever crosses.
[<RequireQualifiedAccess>]
module RejectClass =

    let envelopeVersionUnsupported = "envelope-version-unsupported"
    let envelopeKindUnknown = "envelope-kind-unknown"
    let envelopeMalformed = "envelope-malformed"
    let vintageFormatInvalid = "vintage-format-invalid"
    let submissionBatchEmpty = "submission-batch-empty"
    let submissionGateDirectionUnknown = "submission-gate-direction-unknown"
    let submissionGateDuplicate = "submission-gate-duplicate"
    let submissionSubmitterClassUnknown = "submission-submitter-class-unknown"
    let submissionSpecHashAbsent = "submission-spec-hash-absent"
    let outcomeCompositeKeyMismatch = "outcome-composite-key-mismatch"
    let outcomeDiagnosticUnitInvalid = "outcome-diagnostic-unit-invalid"
    let outcomeDiagnosticUnitOrphan = "outcome-diagnostic-unit-orphan"
    let outcomeGateUnitMismatch = "outcome-gate-unit-mismatch"
    let submissionGateUnitInvalid = "submission-gate-unit-invalid"
    let queryLimitOutOfRange = "query-limit-out-of-range"
    let queryStatusUnknown = "query-status-unknown"
    let queryCursorInvalid = "query-cursor-invalid"
    let scoreOutputCollidesWithInput = "score-output-collides-with-input"

    /// The one class a submitter rather than an executor must catch (§2, §4.5).
    let submissionSpecHashNonCanonical = "submission-spec-hash-non-canonical"

/// Unit text (§3.5): a unit crosses as the ONE canonical text of an admitted unit.
///
/// The one reference this tier makes outside FSharp.Core and the BCL, and it is to a value
/// algebra rather than a wire model: `Fuaran.Core.Unit` has no wire of its own, so reading
/// a unit through it borrows no encoding convention. Qualified rather than opened, so that
/// nothing else of the substrate can arrive by the same door.
[<RequireQualifiedAccess>]
module UnitText =

    /// `Ok` when `text` is the canonical text of an admitted unit; otherwise why not. A
    /// parseable spelling that is not canonical (`s-1` for `/s`) is refused rather than
    /// normalised: two texts for one unit would give one document two encodings.
    let check (text: string) : Result<unit, string> =
        match Fuaran.Core.Unit.parse text with
        | Error refusal -> Error(sprintf "'%s' is not a unit (%A)" text refusal)
        | Ok u ->
            let canonical = Fuaran.Core.Unit.render u

            if canonical = text then
                Ok()
            else
                Error(
                    "'"
                    + text
                    + "' is not canonical unit text; the unit is written '"
                    + canonical
                    + "'"
                )

/// Why a document was refused: the corpus class, the refusal that crosses the wire,
/// and a human-readable detail that is diagnostic only.
type ReadRefusal =
    { RejectClass: string
      Refusal: Refusal
      Detail: string }

/// Canonical encode / decode for the mirrored wire objects.
[<RequireQualifiedAccess>]
module Codec =

    type private ResultBuilder() =
        member _.Bind(m: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) = Result.bind f m
        member _.Return(v: 'a) : Result<'a, 'e> = Ok v
        member _.ReturnFrom(m: Result<'a, 'e>) = m

    let private res = ResultBuilder()

    let private ordinal (a: string) (b: string) = System.String.CompareOrdinal(a, b)

    // ---- encoding ------------------------------------------------------------

    let private optDoc (f: 'T -> Doc) (v: 'T option) : Doc =
        match v with
        | Some x -> f x
        | None -> DNull

    let encodeContentRef (c: ContentRef) : Doc =
        DRec
            [ "format", DStr c.Format
              "hash", DStr c.Hash
              "rowCount", optDoc DI64 c.RowCount ]

    let encodeVintageRef (v: VintageRef) : Doc =
        DRec
            [ "datasetId", DStr v.DatasetId
              "version", DInt v.Version
              "contentRef", optDoc encodeContentRef v.ContentRef ]

    let encodeResolvedVintage (r: ResolvedVintage) : Doc =
        DRec
            [ "ref", encodeVintageRef r.Ref
              "createdAt", DStr r.CreatedAt
              "isLatest", DBool r.IsLatest ]

    /// A late member (§3.1 rule 5): present only when it carries something, so a
    /// document that states nothing in it encodes as it did before the member existed.
    let private late (name: string) (value: Doc option) : (string * Doc) list =
        match value with
        | Some v -> [ name, v ]
        | None -> []

    let encodeGate (g: Gate) : Doc =
        DRec(
            [ "name", DStr g.Name; "threshold", DReal g.Threshold ]
            @ late "unit" (g.Unit |> Option.map DStr)
            @ [ "direction", DStr(GateDirection.encode g.Direction) ]
        )

    let encodeGateVerdict (g: GateVerdict) : Doc =
        DRec(
            [ "name", DStr g.Name; "threshold", DReal g.Threshold ]
            @ late "unit" (g.Unit |> Option.map DStr)
            @ [ "direction", DStr(GateDirection.encode g.Direction)
                "observed", DReal g.Observed
                "passed", DBool g.Passed ]
        )

    let encodeArtifactRef (a: ArtifactRef) : Doc =
        DRec
            [ "artifactId", DStr a.ArtifactId
              "contentHash", DStr a.ContentHash
              "format", optDoc DStr a.Format ]

    let encodeCompositeKey (k: CompositeKey) : Doc =
        DRec
            [ "specHash", DStr k.SpecHash
              "vintage", encodeVintageRef k.Vintage
              "seed", DI64 k.Seed
              "providerId", DStr k.ProviderId
              "providerVersion", DStr k.ProviderVersion ]

    /// The addressable identifier for a registered outcome (§4.3): the content address
    /// over the composite key's canonical bytes.
    ///
    /// This is the one number in the whole interface that two parties sharing no type,
    /// no database and no vocabulary must independently agree on.
    let compositeKeyHash (k: CompositeKey) : string =
        Digest.sha256 (Canonical.render (encodeCompositeKey k))

    let encodeTiming (t: Timing) : Doc =
        DRec
            [ "submittedAt", DStr t.SubmittedAt
              "startedAt", optDoc DStr t.StartedAt
              "completedAt", optDoc DStr t.CompletedAt
              "durationMs", optDoc DI64 t.DurationMs ]

    let encodeCost (c: Cost) : Doc =
        DRec [ "unit", DStr c.Unit; "amount", DReal c.Amount ]

    let encodeFitOutcome (o: FitOutcome) : Doc =
        // A non-finite value is OMITTED rather than encoded as null or zero, because a
        // diagnostic that did not exist and a diagnostic that was zero are different facts —
        // and the unit of an omitted diagnostic goes with it, since a unit names a member
        // `diagnostics` carries (§5.4).
        let finite =
            o.Diagnostics
            |> Map.filter (fun _ v -> not (System.Double.IsNaN v || System.Double.IsInfinity v))

        let units =
            o.DiagnosticUnits
            |> Map.filter (fun name _ -> Map.containsKey name finite)
            |> Map.toList

        DRec(
            [ "compositeKeyHash", DStr o.CompositeKeyHash
              "compositeKey", encodeCompositeKey o.CompositeKey
              "artifactRef", optDoc encodeArtifactRef o.ArtifactRef
              // A map, so its keys sort.
              "diagnostics", DMap(finite |> Map.toList |> List.map (fun (k, v) -> k, DReal v)) ]
            @ late
                "diagnosticUnits"
                (if List.isEmpty units then
                     None
                 else
                     Some(DMap(units |> List.map (fun (k, u) -> k, DStr u))))
            @ [ "gateVerdicts",
                DList(
                    o.GateVerdicts
                    |> List.sortWith (fun a b -> ordinal a.Name b.Name)
                    |> List.map encodeGateVerdict
                )
                "status", DStr o.Status
                "timing", encodeTiming o.Timing
                "cost", optDoc encodeCost o.Cost
                "annotations", DMap(o.Annotations |> Map.toList |> List.map (fun (k, v) -> k, DStr v))
                "registeredAt", DStr o.RegisteredAt ]
        )

    let rec encodeRefusal (r: Refusal) : Doc =
        let tagged (fields: (string * Doc) list) =
            DRec(("class", DStr(Refusal.wireClass r)) :: fields)

        match r with
        | EnvelopeVersionMismatch(received, accepted) ->
            tagged
                [ "received", DInt received
                  "accepted", DList(accepted |> List.sort |> List.map DInt) ]
        | UnknownDocumentKind(kind, known) ->
            tagged
                [ "kind", DStr kind
                  "known", DList(known |> List.sortWith ordinal |> List.map DStr) ]
        | InvalidSubmission reason -> tagged [ "reason", DStr reason ]
        | InvalidQuery reason -> tagged [ "reason", DStr reason ]
        | UnknownProvider(kind, known) ->
            tagged
                [ "kind", DStr kind
                  "known", DList(known |> List.sortWith ordinal |> List.map DStr) ]
        | BudgetDenied(quota, spent, costUnit) ->
            tagged [ "quota", DReal quota; "spent", DReal spent; "unit", DStr costUnit ]
        | GateFailed verdicts ->
            tagged
                [ "verdicts",
                  DList(
                      verdicts
                      |> List.sortWith (fun a b -> ordinal a.Name b.Name)
                      |> List.map encodeGateVerdict
                  ) ]
        | PolicyRefused rule -> tagged [ "rule", DStr rule ]
        // A class added after version 1, so its members are declared from its first day:
        // every one is present, `null` where the gate or the diagnostic states no unit.
        | GateUnitMismatch(name, gateUnit, reported) ->
            tagged
                [ "name", DStr name
                  "unit", optDoc DStr gateUnit
                  "reported", optDoc DStr reported ]
        | ScopeUnavailable -> tagged []
        | Forbidden reason -> tagged [ "reason", DStr reason ]
        | NotFound(what, id) -> tagged [ "what", DStr what; "id", DStr id ]
        | SubstrateUnavailable surface -> tagged [ "surface", DStr surface ]
        | ScoreRefused(reason, detail) -> tagged [ "reason", DStr reason; "detail", DStr detail ]
        | StorageFailure reason -> tagged [ "reason", DStr reason ]
        | Unspecified message -> tagged [ "message", DStr message ]
        // A class this snapshot does not know, re-emitted. It carries only what was
        // recoverable, so it is NOT a faithful re-encoding of what arrived — which is
        // why nothing relays an unrecognised refusal and no round-trip fixture covers
        // one. Emitting the class name keeps it legible in a log; inventing the rest
        // would be worse than losing it.
        | Unrecognised(_, message) -> tagged [ "message", DStr message ]

    let encodeFitSubmission (s: FitSubmission) : Doc =
        DRec
            [ "vintage", encodeVintageRef s.Vintage
              "specPayload", DStr s.SpecPayload
              "specHash", DStr s.SpecHash
              "specHashAlgorithm", DStr s.SpecHashAlgorithm
              "providerKind", DStr s.ProviderKind
              "seed", DI64 s.Seed
              "gates",
              DList(
                  s.Gates
                  |> List.sortWith (fun a b -> ordinal a.Name b.Name)
                  |> List.map encodeGate
              )
              "submitterClass", DStr(SubmitterClass.encode s.SubmitterClass) ]

    let encodeBatch (b: FitSubmissionBatch) : Doc =
        DRec
            [ "batchId", DStr b.BatchId
              // Submitter-ordered — the receipt indexes into it, so an emitter that
              // sorts this list renumbers the submitter's own work.
              "submissions", DList(b.Submissions |> List.map encodeFitSubmission) ]

    let encodeReceipt (r: SubmissionReceipt) : Doc =
        DRec
            [ "batchId", DStr r.BatchId
              "itemCount", DInt r.ItemCount
              "accepted",
              DList(
                  r.Accepted
                  |> List.sortBy (fun a -> a.Index)
                  |> List.map (fun a -> DRec [ "index", DInt a.Index; "jobId", DStr a.JobId ])
              )
              "rejected",
              DList(
                  r.Rejected
                  |> List.sortBy (fun a -> a.Index)
                  |> List.map (fun a -> DRec [ "index", DInt a.Index; "reason", encodeRefusal a.Reason ])
              ) ]

    let encodePage (p: Page) : Doc =
        DRec [ "cursor", optDoc DStr p.Cursor; "limit", DInt p.Limit ]

    let encodeRegistryQuery (q: RegistryQuery) : Doc =
        DRec
            [ "specHashes", DList(q.SpecHashes |> List.sortWith ordinal |> List.map DStr)
              "vintages",
              DList(
                  q.Vintages
                  |> List.sortWith (fun a b ->
                      match ordinal a.DatasetId b.DatasetId with
                      | 0 -> compare a.Version b.Version
                      | c -> c)
                  |> List.map encodeVintageRef
              )
              "statuses", DList(q.Statuses |> List.sortWith ordinal |> List.map DStr)
              "batchId", optDoc DStr q.BatchId
              "page", encodePage q.Page ]

    let encodeOutcomePage (p: OutcomePage) : Doc =
        DRec
            [ "outcomes",
              DList(
                  p.Outcomes
                  |> List.sortWith (fun a b -> ordinal a.CompositeKeyHash b.CompositeKeyHash)
                  |> List.map encodeFitOutcome
              )
              "nextCursor", optDoc DStr p.NextCursor ]

    let encodeScoreRequest (s: ScoreRequest) : Doc =
        DRec
            [ "artifactKeyHash", DStr s.ArtifactKeyHash
              "input", encodeVintageRef s.Input
              "outputDatasetId", DStr s.OutputDatasetId ]

    let private encodeBody (d: Document) : Doc =
        match d with
        | VintageRefDoc v -> encodeVintageRef v
        | ResolvedVintageDoc r -> encodeResolvedVintage r
        | FitSubmissionDoc s -> encodeFitSubmission s
        | FitSubmissionBatchDoc b -> encodeBatch b
        | SubmissionReceiptDoc r -> encodeReceipt r
        | CompositeKeyDoc k -> encodeCompositeKey k
        | FitOutcomeDoc o -> encodeFitOutcome o
        | RegistryQueryDoc q -> encodeRegistryQuery q
        | OutcomePageDoc p -> encodeOutcomePage p
        | ScoreRequestDoc s -> encodeScoreRequest s
        | RefusalDoc r -> encodeRefusal r

    /// A document in its envelope (§5.1). There is no bare-body form.
    let encode (d: Document) : Doc =
        DRec
            [ "envelopeVersion", DInt Document.envelopeVersion
              "kind", DStr(Document.kindOf d)
              "body", encodeBody d ]

    /// The canonical bytes of a document.
    let render (d: Document) : string = Canonical.render (encode d)

    // ---- decoding: shape -----------------------------------------------------

    let private prop (name: string) (v: JsonValue) : Result<JsonValue, string> =
        match v with
        | JsonObject fields ->
            match fields |> List.tryFind (fun (k, _) -> k = name) with
            | Some(_, value) -> Ok value
            | None -> Error("missing member '" + name + "'")
        | _ -> Error("expected an object to read '" + name + "' from")

    let private asString (v: JsonValue) : Result<string, string> =
        match v with
        | JsonString s -> Ok s
        | _ -> Error "expected a string"

    let private asBool (v: JsonValue) : Result<bool, string> =
        match v with
        | JsonBool b -> Ok b
        | _ -> Error "expected a boolean"

    let private asReal (v: JsonValue) : Result<float, string> =
        match v with
        | JsonNumber(value, _) -> Ok value
        | _ -> Error "expected a number"

    let private asInt (v: JsonValue) : Result<int, string> =
        match v with
        | JsonNumber(value, token) when not (token.Contains "." || token.Contains "e" || token.Contains "E") ->
            if value >= -2147483648.0 && value <= 2147483647.0 then
                Ok(int value)
            else
                Error("32-bit integer out of range: " + token)
        | _ -> Error "expected a 32-bit integer"

    /// A 64-bit integer: a sign-prefixed decimal string (§3.1 rule 8).
    let private asInt64 (v: JsonValue) : Result<int64, string> =
        asString v |> Result.bind Canonical.tryParseInt64Text

    /// An optional member: `null` is absence, and absence is never an omitted key.
    let private optional (d: JsonValue -> Result<'T, string>) (v: JsonValue) : Result<'T option, string> =
        match v with
        | JsonNull -> Ok None
        | other -> d other |> Result.map Some

    let private field (name: string) (d: JsonValue -> Result<'T, string>) (v: JsonValue) : Result<'T, string> =
        prop name v
        |> Result.bind (fun x -> d x |> Result.mapError (fun e -> "'" + name + "': " + e))

    let private optField
        (name: string)
        (d: JsonValue -> Result<'T, string>)
        (v: JsonValue)
        : Result<'T option, string> =
        field name (optional d) v

    /// A late member (§3.1 rule 5): absent — or `null` — is `None`. The emitter never
    /// writes the `null`; a reader tolerates it rather than refusing a document over it.
    let private lateField
        (name: string)
        (d: JsonValue -> Result<'T, string>)
        (v: JsonValue)
        : Result<'T option, string> =
        match prop name v with
        | Error _ -> Ok None
        | Ok _ -> optField name d v

    let private asList (d: JsonValue -> Result<'T, string>) (v: JsonValue) : Result<'T list, string> =
        match v with
        | JsonArray items ->
            let rec go acc =
                function
                | [] -> Ok(List.rev acc)
                | x :: rest ->
                    match d x with
                    | Ok value -> go (value :: acc) rest
                    | Error e -> Error e

            go [] items
        | _ -> Error "expected an array"

    let private asMap (d: JsonValue -> Result<'T, string>) (v: JsonValue) : Result<Map<string, 'T>, string> =
        match v with
        | JsonObject fields ->
            let rec go acc =
                function
                | [] -> Ok(Map.ofList (List.rev acc))
                | (k, value) :: rest ->
                    match d value with
                    | Ok x -> go ((k, x) :: acc) rest
                    | Error e -> Error("map key '" + k + "': " + e)

            go [] fields
        | _ -> Error "expected an object"

    // ---- decoding: the shapes ------------------------------------------------

    let private decodeContentRef (v: JsonValue) : Result<ContentRef, string> =
        res {
            let! format = field "format" asString v
            let! hash = field "hash" asString v
            let! rowCount = optField "rowCount" asInt64 v

            return
                { Format = format
                  Hash = hash
                  RowCount = rowCount }
        }

    let private decodeVintageRef (v: JsonValue) : Result<VintageRef, string> =
        res {
            let! datasetId = field "datasetId" asString v
            let! version = field "version" asInt v
            let! contentRef = optField "contentRef" decodeContentRef v

            return
                { DatasetId = datasetId
                  Version = version
                  ContentRef = contentRef }
        }

    let private decodeResolvedVintage (v: JsonValue) : Result<ResolvedVintage, string> =
        res {
            let! r = field "ref" decodeVintageRef v
            let! createdAt = field "createdAt" asString v
            let! isLatest = field "isLatest" asBool v

            return
                { Ref = r
                  CreatedAt = createdAt
                  IsLatest = isLatest }
        }

    let private decodeGate (v: JsonValue) : Result<Gate, string> =
        res {
            let! name = field "name" asString v
            let! threshold = field "threshold" asReal v
            let! unit = lateField "unit" asString v
            let! directionText = field "direction" asString v
            let! direction = GateDirection.tryParse directionText

            return
                { Name = name
                  Threshold = threshold
                  Unit = unit
                  Direction = direction }
        }

    let private decodeGateVerdict (v: JsonValue) : Result<GateVerdict, string> =
        res {
            let! name = field "name" asString v
            let! threshold = field "threshold" asReal v
            let! unit = lateField "unit" asString v
            let! directionText = field "direction" asString v
            let! direction = GateDirection.tryParse directionText
            let! observed = field "observed" asReal v
            let! passed = field "passed" asBool v

            return
                { Name = name
                  Threshold = threshold
                  Unit = unit
                  Direction = direction
                  Observed = observed
                  Passed = passed }
        }

    let private decodeArtifactRef (v: JsonValue) : Result<ArtifactRef, string> =
        res {
            let! artifactId = field "artifactId" asString v
            let! contentHash = field "contentHash" asString v
            let! format = optField "format" asString v

            return
                { ArtifactId = artifactId
                  ContentHash = contentHash
                  Format = format }
        }

    let private decodeCompositeKey (v: JsonValue) : Result<CompositeKey, string> =
        res {
            let! specHash = field "specHash" asString v
            let! vintage = field "vintage" decodeVintageRef v
            let! seed = field "seed" asInt64 v
            let! providerId = field "providerId" asString v
            let! providerVersion = field "providerVersion" asString v

            return
                { SpecHash = specHash
                  Vintage = vintage
                  Seed = seed
                  ProviderId = providerId
                  ProviderVersion = providerVersion }
        }

    let private decodeTiming (v: JsonValue) : Result<Timing, string> =
        res {
            let! submittedAt = field "submittedAt" asString v
            let! startedAt = optField "startedAt" asString v
            let! completedAt = optField "completedAt" asString v
            let! durationMs = optField "durationMs" asInt64 v

            return
                { SubmittedAt = submittedAt
                  StartedAt = startedAt
                  CompletedAt = completedAt
                  DurationMs = durationMs }
        }

    let private decodeCost (v: JsonValue) : Result<Cost, string> =
        res {
            let! unitName = field "unit" asString v
            let! amount = field "amount" asReal v
            return { Unit = unitName; Amount = amount }
        }

    let private decodeFitOutcome (v: JsonValue) : Result<FitOutcome, string> =
        res {
            let! compositeKeyHash = field "compositeKeyHash" asString v
            let! key = field "compositeKey" decodeCompositeKey v
            let! artifactRef = optField "artifactRef" decodeArtifactRef v
            let! diagnostics = field "diagnostics" (asMap asReal) v
            let! diagnosticUnits = lateField "diagnosticUnits" (asMap asString) v
            let! gateVerdicts = field "gateVerdicts" (asList decodeGateVerdict) v
            let! status = field "status" asString v
            let! timing = field "timing" decodeTiming v
            let! cost = optField "cost" decodeCost v
            let! annotations = field "annotations" (asMap asString) v
            let! registeredAt = field "registeredAt" asString v

            return
                { CompositeKeyHash = compositeKeyHash
                  CompositeKey = key
                  ArtifactRef = artifactRef
                  Diagnostics = diagnostics
                  DiagnosticUnits = diagnosticUnits |> Option.defaultValue Map.empty
                  GateVerdicts = gateVerdicts
                  Status = status
                  Timing = timing
                  Cost = cost
                  Annotations = annotations
                  RegisteredAt = registeredAt }
        }

    let private decodeFitSubmission (v: JsonValue) : Result<FitSubmission, string> =
        res {
            let! vintage = field "vintage" decodeVintageRef v
            let! specPayload = field "specPayload" asString v
            let! specHash = field "specHash" asString v
            let! specHashAlgorithm = field "specHashAlgorithm" asString v
            let! providerKind = field "providerKind" asString v
            let! seed = field "seed" asInt64 v
            let! gates = field "gates" (asList decodeGate) v
            let! submitterClassText = field "submitterClass" asString v
            let! submitterClass = SubmitterClass.tryParse submitterClassText

            return
                { Vintage = vintage
                  SpecPayload = specPayload
                  SpecHash = specHash
                  SpecHashAlgorithm = specHashAlgorithm
                  ProviderKind = providerKind
                  Seed = seed
                  Gates = gates
                  SubmitterClass = submitterClass }
        }

    let private decodeBatch (v: JsonValue) : Result<FitSubmissionBatch, string> =
        res {
            let! batchId = field "batchId" asString v
            let! submissions = field "submissions" (asList decodeFitSubmission) v

            return
                { BatchId = batchId
                  Submissions = submissions }
        }

    let private decodePage (v: JsonValue) : Result<Page, string> =
        res {
            let! cursor = optField "cursor" asString v
            let! limit = field "limit" asInt v
            return { Cursor = cursor; Limit = limit }
        }

    let private decodeRegistryQuery (v: JsonValue) : Result<RegistryQuery, string> =
        res {
            let! specHashes = field "specHashes" (asList asString) v
            let! vintages = field "vintages" (asList decodeVintageRef) v
            let! statuses = field "statuses" (asList asString) v
            let! batchId = optField "batchId" asString v
            let! page = field "page" decodePage v

            return
                { SpecHashes = specHashes
                  Vintages = vintages
                  Statuses = statuses
                  BatchId = batchId
                  Page = page }
        }

    let private decodeRefusal (v: JsonValue) : Result<Refusal, string> =
        res {
            let! className = field "class" asString v

            match className with
            | "envelopeVersionMismatch" ->
                let! received = field "received" asInt v
                let! accepted = field "accepted" (asList asInt) v
                return EnvelopeVersionMismatch(received, accepted)
            | "unknownDocumentKind" ->
                let! kind = field "kind" asString v
                let! known = field "known" (asList asString) v
                return UnknownDocumentKind(kind, known)
            | "invalidSubmission" ->
                let! reason = field "reason" asString v
                return InvalidSubmission reason
            | "invalidQuery" ->
                let! reason = field "reason" asString v
                return InvalidQuery reason
            | "unknownProvider" ->
                let! kind = field "kind" asString v
                let! known = field "known" (asList asString) v
                return UnknownProvider(kind, known)
            | "budgetDenied" ->
                let! quota = field "quota" asReal v
                let! spent = field "spent" asReal v
                let! costUnit = field "unit" asString v
                return BudgetDenied(quota, spent, costUnit)
            | "gateFailed" ->
                let! verdicts = field "verdicts" (asList decodeGateVerdict) v
                return GateFailed verdicts
            | "policyRefused" ->
                let! rule = field "rule" asString v
                return PolicyRefused rule
            | "gateUnitMismatch" ->
                let! name = field "name" asString v
                let! gateUnit = optField "unit" asString v
                let! reported = optField "reported" asString v
                return GateUnitMismatch(name, gateUnit, reported)
            | "scopeUnavailable" -> return ScopeUnavailable
            | "forbidden" ->
                let! reason = field "reason" asString v
                return Forbidden reason
            | "notFound" ->
                let! what = field "what" asString v
                let! id = field "id" asString v
                return NotFound(what, id)
            | "substrateUnavailable" ->
                let! surface = field "surface" asString v
                return SubstrateUnavailable surface
            | "scoreRefused" ->
                let! reason = field "reason" asString v
                let! detail = field "detail" asString v
                return ScoreRefused(reason, detail)
            | "storageFailure" ->
                let! reason = field "reason" asString v
                return StorageFailure reason
            | "unspecified" ->
                let! message = field "message" asString v
                return Unspecified message
            // §5.7.2 rule 2 — an unrecognised class MUST NOT fail. It is read as
            // `unspecified`, keeping whatever human-readable text can be found, and the
            // class name is reported so an operator can see an upgrade exists. An
            // implementation that refuses this is one that breaks on the next additive
            // change.
            | other ->
                let text =
                    match field "message" asString v with
                    | Ok m -> m
                    | Error _ ->
                        match field "reason" asString v with
                        | Ok m -> m
                        | Error _ -> ""

                return Unrecognised(other, text)
        }

    let private decodeReceipt (v: JsonValue) : Result<SubmissionReceipt, string> =
        let item (d: JsonValue) : Result<AcceptedItem, string> =
            res {
                let! index = field "index" asInt d
                let! jobId = field "jobId" asString d
                return { Index = index; JobId = jobId }
            }

        let rejected (d: JsonValue) : Result<RejectedItem, string> =
            res {
                let! index = field "index" asInt d
                let! reason = field "reason" decodeRefusal d
                return { Index = index; Reason = reason }
            }

        res {
            let! batchId = field "batchId" asString v
            let! itemCount = field "itemCount" asInt v
            let! accepted = field "accepted" (asList item) v
            let! refused = field "rejected" (asList rejected) v

            return
                { BatchId = batchId
                  ItemCount = itemCount
                  Accepted = accepted
                  Rejected = refused }
        }

    let private decodeOutcomePage (v: JsonValue) : Result<OutcomePage, string> =
        res {
            let! outcomes = field "outcomes" (asList decodeFitOutcome) v
            let! nextCursor = optField "nextCursor" asString v

            return
                { Outcomes = outcomes
                  NextCursor = nextCursor }
        }

    let private decodeScoreRequest (v: JsonValue) : Result<ScoreRequest, string> =
        res {
            let! artifactKeyHash = field "artifactKeyHash" asString v
            let! input = field "input" decodeVintageRef v
            let! outputDatasetId = field "outputDatasetId" asString v

            return
                { ArtifactKeyHash = artifactKeyHash
                  Input = input
                  OutputDatasetId = outputDatasetId }
        }

    // ---- decoding: the semantic rules ---------------------------------------

    let private refuse (rejectClass: string) (refusal: Refusal) (detail: string) : Result<'T, ReadRefusal> =
        Error
            { RejectClass = rejectClass
              Refusal = refusal
              Detail = detail }

    /// A shape failure. Every one of these is a document that is not well formed
    /// against §5 — a member missing, a member mistyped, a closed vocabulary violated
    /// in a way the more specific rules below did not already name.
    let private malformed (detail: string) : Result<'T, ReadRefusal> =
        refuse RejectClass.envelopeMalformed (InvalidSubmission detail) detail

    // These are public because they are not a decoder's private business: an executor
    // holding a value that arrived some other way — built in process, replayed from a
    // store — owes it exactly the same checks, and a second, drifting copy of them is
    // how a rule ends up enforced on one path and not the other.

    let validateVintage (v: VintageRef) : Result<unit, ReadRefusal> =
        match v.ContentRef with
        | Some c when not (Registry.isWellFormedIdentifier c.Format) ->
            refuse
                RejectClass.vintageFormatInvalid
                (InvalidSubmission("format identifier '" + c.Format + "' violates the §7.2 lexical rule"))
                ("an identifier that can never be registered: '" + c.Format + "'")
        | _ -> Ok()

    let validateSubmission (s: FitSubmission) : Result<unit, ReadRefusal> =
        if s.SpecHash = "" || not (Registry.isContentAddress s.SpecHash) then
            refuse
                RejectClass.submissionSpecHashAbsent
                (InvalidSubmission "specHash is empty or is not a content address")
                // The executor cannot mint one — the payload is opaque to it — so a
                // submission with no hash is a fit that can never be joined to the
                // decision that asked for it.
                ("specHash '" + s.SpecHash + "' is not a content address (§4.1)")
        else
            let duplicates =
                s.Gates
                |> List.countBy (fun g -> g.Name)
                |> List.filter (fun (_, n) -> n > 1)
                |> List.map fst

            if not (List.isEmpty duplicates) then
                refuse
                    RejectClass.submissionGateDuplicate
                    (InvalidSubmission("gate '" + String.concat ", " duplicates + "' declared twice"))
                    ("duplicate gate names: " + String.concat ", " duplicates)
            else
                let badUnit =
                    s.Gates
                    |> List.tryPick (fun g ->
                        g.Unit
                        |> Option.bind (fun u ->
                            match UnitText.check u with
                            | Ok() -> None
                            | Error e -> Some(g.Name, e)))

                match badUnit with
                | Some(name, e) ->
                    refuse
                        RejectClass.submissionGateUnitInvalid
                        (InvalidSubmission("gate '" + name + "': " + e))
                        ("gate '" + name + "': " + e)
                | None -> validateVintage s.Vintage

    let validateBatch (b: FitSubmissionBatch) : Result<unit, ReadRefusal> =
        if b.BatchId = "" || List.isEmpty b.Submissions then
            refuse
                RejectClass.submissionBatchEmpty
                (InvalidSubmission "a batch must carry a non-empty batchId and at least one submission")
                // A receipt for nothing is indistinguishable from a receipt whose items
                // were all lost.
                "empty batch"
        else
            b.Submissions
            |> List.fold
                (fun acc s ->
                    match acc with
                    | Error e -> Error e
                    | Ok() -> validateSubmission s)
                (Ok())

    let validateOutcome (o: FitOutcome) : Result<unit, ReadRefusal> =
        // §4.3 consequence 3 — an outcome whose stated identity does not match a
        // recomputation over its own composite key is CORRUPT and must be refused,
        // never stored. It is a different condition from an unknown key and is
        // reported differently.
        let recomputed = compositeKeyHash o.CompositeKey

        if recomputed <> o.CompositeKeyHash then
            refuse
                RejectClass.outcomeCompositeKeyMismatch
                (InvalidSubmission "compositeKeyHash does not match a recomputation over compositeKey")
                ("stated " + o.CompositeKeyHash + ", recomputed " + recomputed)
        else
            // §5.4 — a unit is canonical unit text (§3.5), it names a diagnostic the outcome
            // carries, and a verdict is stated in its diagnostic's unit (§5.3's gate rule).
            let invalid =
                [ for KeyValue(name, u) in o.DiagnosticUnits -> "diagnostic '" + name + "'", u
                  for g in o.GateVerdicts do
                      match g.Unit with
                      | Some u -> yield "gate '" + g.Name + "'", u
                      | None -> () ]
                |> List.tryPick (fun (owner, u) ->
                    match UnitText.check u with
                    | Ok() -> None
                    | Error e -> Some(owner + ": " + e))

            let orphan =
                o.DiagnosticUnits
                |> Map.toList
                |> List.tryFind (fun (name, _) -> not (Map.containsKey name o.Diagnostics))

            let mismatch =
                o.GateVerdicts
                |> List.tryFind (fun g -> g.Unit <> Map.tryFind g.Name o.DiagnosticUnits)

            match invalid, orphan, mismatch with
            | Some detail, _, _ -> refuse RejectClass.outcomeDiagnosticUnitInvalid (InvalidSubmission detail) detail
            | None, Some(name, _), _ ->
                refuse
                    RejectClass.outcomeDiagnosticUnitOrphan
                    (InvalidSubmission("diagnosticUnits names '" + name + "', which is not in diagnostics"))
                    // A unit for a value nobody reported is a statement about nothing — and
                    // the one shape in which a reader could not tell a lost diagnostic from
                    // an invented unit.
                    ("a unit for the absent diagnostic '" + name + "'")
            | None, None, Some g ->
                let text =
                    function
                    | Some u -> "'" + u + "'"
                    | None -> "no unit"

                let detail =
                    "gate '"
                    + g.Name
                    + "' is stated in "
                    + text g.Unit
                    + " but its diagnostic is reported in "
                    + text (Map.tryFind g.Name o.DiagnosticUnits)

                refuse RejectClass.outcomeGateUnitMismatch (InvalidSubmission detail) detail
            | None, None, None -> validateVintage o.CompositeKey.Vintage

    let validateQuery (q: RegistryQuery) : Result<unit, ReadRefusal> =
        if q.Page.Limit < Page.minLimit || q.Page.Limit > Page.maxLimit then
            refuse
                RejectClass.queryLimitOutOfRange
                (InvalidQuery(
                    "page.limit must be between "
                    + string Page.minLimit
                    + " and "
                    + string Page.maxLimit
                ))
                ("limit " + string q.Page.Limit)
        else
            match
                q.Statuses
                |> List.tryFind (fun s -> not (List.contains s Registry.outcomeStatuses))
            with
            | Some unknown ->
                refuse
                    RejectClass.queryStatusUnknown
                    (InvalidQuery("unknown status '" + unknown + "'"))
                    ("status '" + unknown + "' is not registered (§7.7)")
            | None -> Ok()

    let validateScoreRequest (s: ScoreRequest) : Result<unit, ReadRefusal> =
        if s.OutputDatasetId = s.Input.DatasetId then
            refuse
                RejectClass.scoreOutputCollidesWithInput
                (InvalidSubmission "outputDatasetId must not equal input.datasetId")
                // Writing predictions as a new version of the very dataset they were
                // derived from makes the input irreproducible and creates a dataset whose
                // later versions silently depend on a model.
                ("both name '" + s.Input.DatasetId + "'")
        else
            Ok()

    /// The two CLOSED vocabularies inside a submission, checked against the raw
    /// document before it is decoded into the mirror.
    ///
    /// They are checked here rather than left to the decoder because §7.9 gives each of
    /// them its own reject class, and a decoder that fails on both with one error has
    /// collapsed two conditions the specification deliberately separates. Both are
    /// refused rather than defaulted, and for the same reason in each case: a gate whose
    /// direction is guessed is a gate that silently passes, and an unknown submitter
    /// class defaulted to `human` makes the stricter policy path unreachable by exactly
    /// the callers it exists for.
    let private validateClosedVocabularies (submission: JsonValue) : Result<unit, ReadRefusal> =
        let direction (gate: JsonValue) =
            match field "direction" asString gate with
            // A missing or mistyped member is a shape failure, and the decoder will say
            // so in its own words — this check is about the VALUE.
            | Error _ -> Ok()
            | Ok text ->
                match GateDirection.tryParse text with
                | Ok _ -> Ok()
                | Error detail -> refuse RejectClass.submissionGateDirectionUnknown (InvalidSubmission detail) detail

        let gates =
            match field "gates" (asList (fun g -> Ok g)) submission with
            | Error _ -> []
            | Ok items -> items

        let firstBadGate =
            gates
            |> List.tryPick (fun g ->
                match direction g with
                | Error e -> Some e
                | Ok() -> None)

        match firstBadGate with
        | Some e -> Error e
        | None ->
            match field "submitterClass" asString submission with
            | Error _ -> Ok()
            | Ok text ->
                match SubmitterClass.tryParse text with
                | Ok _ -> Ok()
                | Error detail -> refuse RejectClass.submissionSubmitterClassUnknown (InvalidSubmission detail) detail

    // ---- reading -------------------------------------------------------------

    /// Read a document from canonical bytes.
    ///
    /// The order of refusals is the order a reader can establish them in: an envelope
    /// it cannot accept is refused whole and is never read partially, because a member
    /// it has no field for would otherwise satisfy a requirement by omission.
    let read (json: string) : Result<Document, ReadRefusal> =
        let body
            (v: JsonValue)
            (decode: JsonValue -> Result<'T, string>)
            (wrap: 'T -> Document)
            (check: 'T -> Result<unit, ReadRefusal>)
            =
            match decode v with
            | Error detail -> malformed detail
            | Ok value -> check value |> Result.map (fun () -> wrap value)

        match Canonical.parse json with
        | Error detail -> malformed ("not a well-formed JSON document: " + detail)
        | Ok parsed ->
            match field "envelopeVersion" asInt parsed with
            | Error detail -> malformed detail
            | Ok version when not (List.contains version Document.acceptedVersions) ->
                refuse
                    RejectClass.envelopeVersionUnsupported
                    (EnvelopeVersionMismatch(version, Document.acceptedVersions))
                    ("envelope version " + string version + " is not accepted")
            | Ok _ ->
                match field "kind" asString parsed with
                | Error detail -> malformed detail
                | Ok kind ->
                    match prop "body" parsed with
                    | Error detail -> malformed detail
                    | Ok bodyValue ->
                        match kind with
                        | "vintageRef" -> body bodyValue decodeVintageRef VintageRefDoc validateVintage
                        | "resolvedVintage" ->
                            body bodyValue decodeResolvedVintage ResolvedVintageDoc (fun r -> validateVintage r.Ref)
                        | "fitSubmission" ->
                            validateClosedVocabularies bodyValue
                            |> Result.bind (fun () ->
                                body bodyValue decodeFitSubmission FitSubmissionDoc validateSubmission)
                        | "fitSubmissionBatch" ->
                            let submissions =
                                match field "submissions" (asList Ok) bodyValue with
                                | Error _ -> []
                                | Ok items -> items

                            let firstBad =
                                submissions
                                |> List.tryPick (fun s ->
                                    match validateClosedVocabularies s with
                                    | Error e -> Some e
                                    | Ok() -> None)

                            match firstBad with
                            | Some e -> Error e
                            | None -> body bodyValue decodeBatch FitSubmissionBatchDoc validateBatch
                        | "submissionReceipt" -> body bodyValue decodeReceipt SubmissionReceiptDoc (fun _ -> Ok())
                        | "compositeKey" ->
                            body bodyValue decodeCompositeKey CompositeKeyDoc (fun k -> validateVintage k.Vintage)
                        | "fitOutcome" -> body bodyValue decodeFitOutcome FitOutcomeDoc validateOutcome
                        | "registryQuery" -> body bodyValue decodeRegistryQuery RegistryQueryDoc validateQuery
                        | "outcomePage" ->
                            body bodyValue decodeOutcomePage OutcomePageDoc (fun p ->
                                (Ok(), p.Outcomes)
                                ||> List.fold (fun acc o -> acc |> Result.bind (fun () -> validateOutcome o)))
                        | "scoreRequest" -> body bodyValue decodeScoreRequest ScoreRequestDoc validateScoreRequest
                        | "refusal" -> body bodyValue decodeRefusal RefusalDoc (fun _ -> Ok())
                        | unknown ->
                            // §5.1.1 — never guessed at from the body's shape.
                            // Registering a new kind is additive, so an unknown kind is
                            // an ordinary "I am older than you" condition and its refusal
                            // is the mechanism by which that is discovered.
                            refuse
                                RejectClass.envelopeKindUnknown
                                (UnknownDocumentKind(unknown, Registry.documentKinds))
                                ("kind '" + unknown + "' is not in the §7.1 registry")

/// The obligations that belong to a SUBMITTER, before a document is ever emitted.
///
/// Kept apart from the reader deliberately. §4.2 rule 2 forbids an executor from
/// checking a hash it was handed — not even where the algorithm named is one it
/// implements — so the minting rule cannot be an executor's obligation at all. A
/// submission that violates it is one no party downstream is permitted to detect and
/// every party will key by, which is why the only place it can be caught is here.
[<RequireQualifiedAccess>]
module Submitter =

    /// Is this submission's `specHash` the minting of its own `specPayload` under the
    /// algorithm it names?
    ///
    /// Returns `Ok` where the named algorithm is not one this snapshot implements: an
    /// unregistered identifier is a legible statement that interop is bounded by
    /// agreement between submitters, not a defect. What it is NOT is licence to skip
    /// the check for a rule we DO implement.
    let checkMinting (s: FitSubmission) : Result<unit, ReadRefusal> =
        if s.SpecHashAlgorithm <> Minting.algorithmId then
            Ok()
        else
            match Minting.mint s.SpecPayload with
            | Error refusal ->
                Error
                    { RejectClass = RejectClass.submissionSpecHashNonCanonical
                      Refusal = InvalidSubmission(Minting.describeRefusal refusal)
                      Detail = Minting.describeRefusal refusal }
            | Ok minted when minted = s.SpecHash -> Ok()
            | Ok minted ->
                Error
                    { RejectClass = RejectClass.submissionSpecHashNonCanonical
                      Refusal = InvalidSubmission "specHash is not the minting of specPayload under the named algorithm"
                      Detail = "the payload mints to " + minted + " but the submission carries " + s.SpecHash }

    /// Mint a submission's hash from its payload — the shape a conformant submitter
    /// uses, so that `checkMinting` can never fail on a document this library built.
    let mintFor (payload: string) : Result<string, Minting.MintRefusal> = Minting.mint payload
