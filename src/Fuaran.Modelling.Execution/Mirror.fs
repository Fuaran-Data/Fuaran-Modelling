// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The execution-interface MIRROR — the model-execution wire objects, as
//  FSharp.Core records.
//
//  These are not the manager's types and they are not a library's types. They
//  are a PINNED SNAPSHOT of a language-neutral wire specification, mirrored
//  field for field using names and strings only, so that this library can speak
//  the interface without taking a dependency on anybody's implementation of it.
//
//  Three consequences, all deliberate:
//
//    * Everything here is FSharp.Core plus the BCL. No substrate, no codec
//      library, no host — because a mirror that borrows another wire model's
//      conventions is not a mirror, it is a translation, and translations drift
//      silently. (This tier's own JSON reader exists for exactly one reason: the
//      specification requires `null`, and the substrate's canonical wire model has
//      no null. Inheriting that would have made an absent optional
//      inexpressible.)
//    * The shapes are the specification's, not the manager's. The manager's
//      composite key is five strings; the wire's carries a vintage reference.
//      Where they differ, the mirror follows the wire and the bridge maps —
//      never the reverse.
//    * Drift is CHECKED, not remembered. The conformance harness runs this
//      mirror against the specification's own corpus, and a pinned digest over
//      the corpus manifest fails the build when the specification moves. A
//      snapshot nobody re-checks is a copy of something that used to be true.
//
//  Every `§` reference below is into the model-execution wire specification.
// ============================================================================

/// The open string constants the specification registers (§7), and the lexical
/// rules that decide whether an identifier could ever be registered.
///
/// An UNREGISTERED identifier is not an error — the registries are open, and a
/// party relaying a reference to a format it does not handle is doing its job. What
/// is always a defect is an identifier that violates the lexical rule, because such
/// an identifier can never be registered by anyone.
[<RequireQualifiedAccess>]
module Registry =

    /// §7.1 — the values an envelope's `kind` may take, naming the §5 shapes.
    let documentKinds =
        [ "vintageRef"
          "resolvedVintage"
          "fitSubmission"
          "fitSubmissionBatch"
          "submissionReceipt"
          "compositeKey"
          "fitOutcome"
          "registryQuery"
          "outcomePage"
          "scoreRequest"
          "refusal" ]

    /// §7.2 — dataset and artifact formats registered at version 1.
    let datasetFormats = [ "parquet"; "arrow-ipc"; "csv"; "ndjson" ]

    /// §7.4 — content-address algorithms registered at version 1.
    let digestAlgorithms = [ "sha256" ]

    /// §7.4 — `specHashAlgorithm` values registered at version 1: the minting
    /// canonicalisation of §4.5, and the pipeline hash, which another specification
    /// defines and §7.4 registers by citation.
    let mintingAlgorithms = [ "canonical-json-sha256-v1"; "compute-pipeline-sha256-v1" ]

    /// §7.7 — outcome lifecycle statuses registered at version 1.
    let outcomeStatuses = [ "registered"; "accepted"; "approved"; "retired"; "failed" ]

    /// §7.8 — the sub-reasons a `scoreRefused` may carry.
    let scoreRefusalReasons =
        [ "provider-not-found"
          "not-approved"
          "input-schema-mismatch"
          "input-unavailable"
          "provider-failed"
          "storage-failure" ]

    /// The common lexical rule (§7): `^[a-z0-9]([a-z0-9-]*[a-z0-9])?$` — lowercase,
    /// digits, interior hyphens. Written out rather than delegated to a regex engine,
    /// because the rule is small and a mirror should be readable end to end.
    let isWellFormedIdentifier (s: string) : bool =
        let ok (c: char) =
            (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

        s.Length > 0
        && ok s[0]
        && ok s[s.Length - 1]
        && s |> Seq.forall (fun c -> ok c || c = '-')

    /// §7.1's own lexical rule — kinds are lowerCamelCase (`^[a-z][A-Za-z0-9]*$`),
    /// because they name the §5 shapes and reusing the shape names verbatim is what
    /// makes an unknown kind diagnosable.
    let isWellFormedKind (s: string) : bool =
        s.Length > 0
        && s[0] >= 'a'
        && s[0] <= 'z'
        && s
           |> Seq.forall (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))

    /// §4.1 — a content address is `"{algorithm}:{lowercase hex}"`. A bare hex string
    /// with no algorithm prefix is NOT one, and the distinction is load-bearing: the
    /// algorithm is named inside the value so that a future digest change is a visible
    /// discontinuity rather than a silent one.
    let isContentAddress (s: string) : bool =
        match s.IndexOf ':' with
        | i when i <= 0 || i = s.Length - 1 -> false
        | i ->
            let algorithm = s.Substring(0, i)
            let hex = s.Substring(i + 1)

            isWellFormedIdentifier algorithm
            && hex.Length > 0
            && hex |> Seq.forall (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

// ---- §5.2 vintage ref -------------------------------------------------------

/// Where a body of data's bytes are and what they are (§5.2).
///
/// `RowCount` is a 64-bit integer, so on the wire it is a sign-prefixed decimal
/// STRING (§3.1 rule 8) — a row count is not exactly representable as a double, and
/// silently rounding one is worse than encoding it as text.
type ContentRef =
    { Format: string
      Hash: string
      RowCount: int64 option }

/// The pin: which immutable version of a named dataset (§5.2).
///
/// This is the shape that appears inside a composite key, so its encoding is
/// load-bearing for identity. A `null` `ContentRef` is fully conformant and is the
/// ordinary shape a submitter emits: it pins WHICH vintage and lets the executor
/// resolve where the bytes are.
type VintageRef =
    { DatasetId: string
      Version: int
      ContentRef: ContentRef option }

/// The executor's answer to "what is this, exactly?" (§5.2).
///
/// `IsLatest` is advisory and is never an identity input — it is true of a moment,
/// not of the vintage.
type ResolvedVintage =
    { Ref: VintageRef
      CreatedAt: string
      IsLatest: bool }

// ---- §5.3 / §5.4 gates ------------------------------------------------------

/// Which side of the threshold a gate passes on (§7.5). The vocabulary is CLOSED:
/// an unrecognised direction is refused, never defaulted, because a gate whose
/// direction is guessed is a gate that silently passes.
type GateDirection =
    | AtLeast
    | AtMost

[<RequireQualifiedAccess>]
module GateDirection =

    let encode (d: GateDirection) : string =
        match d with
        | AtLeast -> "atLeast"
        | AtMost -> "atMost"

    let tryParse (s: string) : Result<GateDirection, string> =
        match s with
        | "atLeast" -> Ok AtLeast
        | "atMost" -> Ok AtMost
        | other -> Error("unknown gate direction '" + other + "' (§7.5 registers atLeast | atMost)")

/// A diagnostic threshold the executor evaluates after the fit (§5.3).
///
/// `Unit` is the unit the threshold is stated in, as canonical unit text (§3.5), or
/// `None` where the gate states none. A late member (§3.1 rule 5): omitted from the
/// wire when absent, so a gate with no unit encodes as it did before units existed.
type Gate =
    { Name: string
      Threshold: float
      Unit: string option
      Direction: GateDirection }

/// A gate plus what was observed (§5.4).
///
/// `Passed` is carried explicitly rather than left to the reader to recompute,
/// because a reader that recomputes it is asserting a floating-point comparison rule
/// the specification does not fix. The executor's evaluation is the record.
type GateVerdict =
    {
        Name: string
        Threshold: float
        /// As submitted: the unit both `Threshold` and `Observed` are in (§5.3's gate rule).
        Unit: string option
        Direction: GateDirection
        Observed: float
        Passed: bool
    }

[<RequireQualifiedAccess>]
module GateVerdict =

    /// Evaluate a gate against an observed value — the comparison rule an executor
    /// applies before recording its verdict, once §5.3's gate rule has established that
    /// the gate and its diagnostic are stated in one unit.
    let evaluate (g: Gate) (observed: float) : GateVerdict =
        { Name = g.Name
          Threshold = g.Threshold
          Unit = g.Unit
          Direction = g.Direction
          Observed = observed
          Passed =
            match g.Direction with
            | AtLeast -> observed >= g.Threshold
            | AtMost -> observed <= g.Threshold }

// ---- §7.6 submitter class ---------------------------------------------------

/// Who submitted a fit (§7.6). On the wire from version 1 and CLOSED.
///
/// The distinction that matters is between work a person is watching and work nothing
/// is watching. Defaulting an unknown class to `human` would make the stricter policy
/// path unreachable by exactly the callers it exists for, which is why an unrecognised
/// class is refused instead.
type SubmitterClass =
    | SubmitterHuman
    | SubmitterScheduled
    | SubmitterAgent

[<RequireQualifiedAccess>]
module SubmitterClass =

    let encode (s: SubmitterClass) : string =
        match s with
        | SubmitterHuman -> "human"
        | SubmitterScheduled -> "scheduled"
        | SubmitterAgent -> "agent"

    let tryParse (s: string) : Result<SubmitterClass, string> =
        match s with
        | "human" -> Ok SubmitterHuman
        | "scheduled" -> Ok SubmitterScheduled
        | "agent" -> Ok SubmitterAgent
        | other ->
            Error(
                "unknown submitter class '"
                + other
                + "' (§7.6 registers human | scheduled | agent)"
            )

    let all = [ SubmitterHuman; SubmitterScheduled; SubmitterAgent ]

// ---- §5.4 fit outcome -------------------------------------------------------

/// Where a fitted artifact is stored and what it is (§5.4).
type ArtifactRef =
    { ArtifactId: string
      ContentHash: string
      Format: string option }

/// The join (§4.3). Five values name a fit forever and across parties.
///
/// Re-fitting with the same five values is the same fit; changing any of them is a
/// different fit and MUST produce a different address. In particular a new dataset
/// version is a new fit, not an update of an old one.
type CompositeKey =
    { SpecHash: string
      Vintage: VintageRef
      Seed: int64
      ProviderId: string
      ProviderVersion: string }

/// When a fit moved through its lifecycle (§5.4). Every member may be absent on an
/// outcome that never ran.
type Timing =
    { SubmittedAt: string
      StartedAt: string option
      CompletedAt: string option
      DurationMs: int64 option }

/// What a fit cost, in whatever unit the executor accounts in (§5.4). The unit is
/// uninterpreted here.
type Cost = { Unit: string; Amount: float }

/// What came out, and what names it forever (§5.4).
///
/// `Diagnostics` and `Annotations` are MAPS — keys chosen by the emitter at runtime,
/// so they sort (§3.1 rule 4) where a record's members do not. Nothing in the
/// specification assigns meaning to a diagnostic name or defines a good value; a gate
/// is the only place a threshold appears, and the threshold came from the submitter.
///
/// `DiagnosticUnits` states what a diagnostic measures, as canonical unit text (§3.5):
/// every key names a member of `Diagnostics`, and a diagnostic with no entry states no
/// unit. A late member, omitted from the wire when it has no entries.
type FitOutcome =
    { CompositeKeyHash: string
      CompositeKey: CompositeKey
      ArtifactRef: ArtifactRef option
      Diagnostics: Map<string, float>
      DiagnosticUnits: Map<string, string>
      GateVerdicts: GateVerdict list
      Status: string
      Timing: Timing
      Cost: Cost option
      Annotations: Map<string, string>
      RegisteredAt: string }

// ---- §5.7 refusal -----------------------------------------------------------

/// Every denial, as enumerable data (§5.7).
///
/// No shape in this interface carries a bare error string, an exception, or a numeric
/// code whose meaning lives elsewhere. A consumer branches on the class; the
/// human-readable text an implementation may add is diagnostic and is not contract.
///
/// The vocabulary is closed for INTERPRETATION and open for ADDITION (§5.7.2): a
/// reader that meets a class registered after this snapshot MUST NOT fail. It reads
/// it as `Unspecified`, keeps whatever text it can find, and reports the class name —
/// which is what `Unrecognised` is, and why it is a case of this type rather than a
/// decode failure.
type Refusal =
    | EnvelopeVersionMismatch of received: int * accepted: int list
    | UnknownDocumentKind of kind: string * known: string list
    | InvalidSubmission of reason: string
    | InvalidQuery of reason: string
    | UnknownProvider of providerKind: string * knownProviders: string list
    | BudgetDenied of quota: float * spent: float * costUnit: string
    | GateFailed of verdicts: GateVerdict list
    | PolicyRefused of rule: string
    /// A gate stated in one unit against a diagnostic reported in another (§5.3's gate
    /// rule). Never converted; `None` is "states no unit", which agrees only with `None`.
    | GateUnitMismatch of name: string * gateUnit: string option * reported: string option
    | ScopeUnavailable
    | Forbidden of reason: string
    | NotFound of what: string * id: string
    | SubstrateUnavailable of surface: string
    | ScoreRefused of scoreReason: string * detail: string
    | StorageFailure of reason: string
    | Unspecified of message: string
    /// A class registered after this snapshot (§5.7.2 rule 2). Held as data so the
    /// class name survives into a log and an operator can see that an upgrade exists.
    | Unrecognised of className: string * message: string

[<RequireQualifiedAccess>]
module Refusal =

    /// The `class` discriminator this refusal encodes under.
    let wireClass (r: Refusal) : string =
        match r with
        | EnvelopeVersionMismatch _ -> "envelopeVersionMismatch"
        | UnknownDocumentKind _ -> "unknownDocumentKind"
        | InvalidSubmission _ -> "invalidSubmission"
        | InvalidQuery _ -> "invalidQuery"
        | UnknownProvider _ -> "unknownProvider"
        | BudgetDenied _ -> "budgetDenied"
        | GateFailed _ -> "gateFailed"
        | PolicyRefused _ -> "policyRefused"
        | GateUnitMismatch _ -> "gateUnitMismatch"
        | ScopeUnavailable -> "scopeUnavailable"
        | Forbidden _ -> "forbidden"
        | NotFound _ -> "notFound"
        | SubstrateUnavailable _ -> "substrateUnavailable"
        | ScoreRefused _ -> "scoreRefused"
        | StorageFailure _ -> "storageFailure"
        | Unspecified _ -> "unspecified"
        | Unrecognised(className, _) -> className

    /// How a reader of THIS snapshot interprets the refusal. Every known class is
    /// itself; an unregistered one is `unspecified` (§5.7.2 rule 2) — so inventing a
    /// class in a private namespace buys a log line, not a behaviour.
    let interpretation (r: Refusal) : string =
        match r with
        | Unrecognised _ -> "unspecified"
        | known -> wireClass known

    /// The closed vocabulary, in registry order — what a `known` list enumerates.
    let registeredClasses =
        [ "envelopeVersionMismatch"
          "unknownDocumentKind"
          "invalidSubmission"
          "invalidQuery"
          "unknownProvider"
          "budgetDenied"
          "gateFailed"
          "policyRefused"
          "scopeUnavailable"
          "forbidden"
          "notFound"
          "substrateUnavailable"
          "scoreRefused"
          "storageFailure"
          "unspecified"
          "gateUnitMismatch" ]

    /// A human-readable rendering. Diagnostic only — nothing in the interface compares
    /// these, and an implementation in another language will and should word them
    /// differently.
    let describe (r: Refusal) : string =
        match r with
        | EnvelopeVersionMismatch(received, accepted) ->
            "envelope version "
            + string received
            + " is not accepted (this participant accepts "
            + (accepted |> List.map string |> String.concat ", ")
            + ")"
        | UnknownDocumentKind(kind, known) ->
            "unknown document kind '" + kind + "' (known: " + String.concat ", " known + ")"
        | InvalidSubmission reason -> "invalid submission: " + reason
        | InvalidQuery reason -> "invalid query: " + reason
        | UnknownProvider(kind, known) ->
            "no provider of kind '"
            + kind
            + "' here (known: "
            + String.concat ", " known
            + ")"
        | BudgetDenied(quota, spent, costUnit) ->
            "budget denied: "
            + string spent
            + " of "
            + string quota
            + " "
            + costUnit
            + " already spent"
        | GateFailed verdicts ->
            "gates failed: "
            + (verdicts
               |> List.filter (fun v -> not v.Passed)
               |> List.map (fun v -> v.Name)
               |> String.concat ", ")
        | PolicyRefused rule -> "refused by rule '" + rule + "'"
        | GateUnitMismatch(name, gateUnit, reported) ->
            let text =
                function
                | Some u -> "'" + u + "'"
                | None -> "no unit"

            "gate '"
            + name
            + "' states "
            + text gateUnit
            + " but its diagnostic is reported in "
            + text reported
        | ScopeUnavailable -> "no resolved scope, and every operation here requires one"
        | Forbidden reason -> "forbidden: " + reason
        | NotFound(what, id) -> "no such " + what + " in this scope: '" + id + "'"
        | SubstrateUnavailable surface -> "the '" + surface + "' surface is not composed in this deployment"
        | ScoreRefused(reason, detail) -> "scoring refused (" + reason + "): " + detail
        | StorageFailure reason -> "storage failure: " + reason
        | Unspecified message -> "unspecified: " + message
        | Unrecognised(className, message) ->
            "unrecognised refusal class '"
            + className
            + "' — read as unspecified; an upgrade is available. "
            + message

// ---- §5.3 fit submission ----------------------------------------------------

/// Fit this configuration, on this data, under these gates (§5.3).
///
/// `SpecPayload` is OPAQUE: its bytes are the value, and nothing here parses,
/// normalises, re-indents or re-encodes it. `SpecHash` is minted by the submitter
/// over the specification (§4.2, §4.5) and is stored by every other party exactly as
/// handed — never re-derived, not even where the algorithm named is one the reader
/// implements.
type FitSubmission =
    { Vintage: VintageRef
      SpecPayload: string
      SpecHash: string
      SpecHashAlgorithm: string
      ProviderKind: string
      Seed: int64
      Gates: Gate list
      SubmitterClass: SubmitterClass }

/// The primary submission shape (§5.3). A single fit is the degenerate batch.
///
/// `Submissions` is the one list in the specification that is NOT sorted, because the
/// receipt refers to its members by index. An emitter that sorts it renumbers the
/// submitter's own work.
type FitSubmissionBatch =
    { BatchId: string
      Submissions: FitSubmission list }

/// One submission the executor took, and the handle to poll for it (§5.3).
type AcceptedItem = { Index: int; JobId: string }

/// One submission the executor would not take, and why (§5.3).
type RejectedItem = { Index: int; Reason: Refusal }

/// What came back from a submission, before any fit has finished (§5.3).
///
/// Partial acceptance is DATA, not an error: an executor that accepts some items and
/// refuses others returns both, and never refuses a whole batch because one item was
/// bad. The two lists partition the submitted indices exactly.
///
/// Submission is not completion. A receipt says the work was taken, never that it
/// finished — a submitter learns outcomes by querying, not by holding a connection
/// open.
type SubmissionReceipt =
    { BatchId: string
      ItemCount: int
      Accepted: AcceptedItem list
      Rejected: RejectedItem list }

// ---- §5.5 registry query ----------------------------------------------------

/// Where in a result set to read from, and how much (§5.5).
///
/// A cursor is OPAQUE to the caller: it is echoed unmodified and is never parsed,
/// compared or constructed. An exhausted or unrecognised cursor is refused, never
/// silently restarted from the beginning — a silent restart turns a paging loop into
/// an infinite one.
type Page = { Cursor: string option; Limit: int }

[<RequireQualifiedAccess>]
module Page =

    /// §5.5 — `1 ≤ limit ≤ 1000`.
    let minLimit = 1
    let maxLimit = 1000

    /// The first page of a given size.
    let first (limit: int) : Page = { Cursor = None; Limit = limit }

/// Which outcomes match (§5.5). Filters are CONJUNCTIVE; an empty list matches
/// anything, so the degenerate query is a single-outcome read rather than a separate
/// shape.
type RegistryQuery =
    { SpecHashes: string list
      Vintages: VintageRef list
      Statuses: string list
      BatchId: string option
      Page: Page }

/// One page of matching outcomes (§5.5).
///
/// Ordered by `CompositeKeyHash` — a value that exists before the outcome is stored
/// and never changes — precisely so that paging is stable while new outcomes are being
/// registered concurrently.
type OutcomePage =
    { Outcomes: FitOutcome list
      NextCursor: string option }

// ---- §5.6 score request -----------------------------------------------------

/// Apply a registered artifact to this data (§5.6).
///
/// The answer is a `ResolvedVintage` — a new version of `OutputDatasetId` — never an
/// inline array of numbers. A prediction set is data, and data here has identity, a
/// content hash and a version, so a model fitted to scored output is as reproducible
/// as one fitted to source.
type ScoreRequest =
    { ArtifactKeyHash: string
      Input: VintageRef
      OutputDatasetId: string }

// ---- port-local shapes (NOT mirrored — no wire kind exists) -----------------

/// Which stored artifacts match, a page at a time.
///
/// **Not a mirrored shape.** The specification registers no artifact-query document,
/// so this is the port's own, marked as such rather than smuggled in beside the
/// mirror: a reader must be able to tell what is pinned to a specification from what
/// is this library's convenience. It reuses the mirrored `Page` and `ArtifactRef` so
/// that it costs nothing to promote if a wire kind is ever registered.
type ArtifactQuery =
    { ArtifactKeyHashes: string list
      Page: Page }

/// One artifact, addressed by the outcome that produced it.
type ArtifactEntry =
    { ArtifactKeyHash: string
      Artifact: ArtifactRef }

/// One page of artifacts. **Not a mirrored shape** — see `ArtifactQuery`.
type ArtifactPage =
    { Artifacts: ArtifactEntry list
      NextCursor: string option }

/// Ask that a dataset be re-derived into a new vintage.
///
/// **Not a mirrored shape**, and deliberately the thinnest possible one: dataset
/// authoring does not cross this interface, so `AssemblyRef` is an OPAQUE handle the
/// executor already understands and this library never constructs, inspects or
/// validates. Anything richer would couple the manager to an executor's transform
/// vocabulary for no gain.
type RevintageRequest =
    { DatasetId: string
      AssemblyRef: string }

// ---- §5.1 the envelope ------------------------------------------------------

/// A document, discriminated by the `kind` its envelope names (§5.1).
///
/// There is no bare-body form: a document with no version is a document whose meaning
/// cannot be established later, which is why the envelope is not optional and why
/// this type is what the codec reads and writes.
type Document =
    | VintageRefDoc of VintageRef
    | ResolvedVintageDoc of ResolvedVintage
    | FitSubmissionDoc of FitSubmission
    | FitSubmissionBatchDoc of FitSubmissionBatch
    | SubmissionReceiptDoc of SubmissionReceipt
    | CompositeKeyDoc of CompositeKey
    | FitOutcomeDoc of FitOutcome
    | RegistryQueryDoc of RegistryQuery
    | OutcomePageDoc of OutcomePage
    | ScoreRequestDoc of ScoreRequest
    | RefusalDoc of Refusal

[<RequireQualifiedAccess>]
module Document =

    /// The envelope version this snapshot implements (§8). Not a per-family version:
    /// every family moves together, because the families are joined by the identity
    /// rule and a partial skew across them is exactly the state in which two records
    /// disagree about what a key means.
    let envelopeVersion = 1

    /// The versions a reader built from this snapshot accepts.
    let acceptedVersions = [ 1 ]

    /// The `kind` this document rides under (§7.1).
    let kindOf (d: Document) : string =
        match d with
        | VintageRefDoc _ -> "vintageRef"
        | ResolvedVintageDoc _ -> "resolvedVintage"
        | FitSubmissionDoc _ -> "fitSubmission"
        | FitSubmissionBatchDoc _ -> "fitSubmissionBatch"
        | SubmissionReceiptDoc _ -> "submissionReceipt"
        | CompositeKeyDoc _ -> "compositeKey"
        | FitOutcomeDoc _ -> "fitOutcome"
        | RegistryQueryDoc _ -> "registryQuery"
        | OutcomePageDoc _ -> "outcomePage"
        | ScoreRequestDoc _ -> "scoreRequest"
        | RefusalDoc _ -> "refusal"
