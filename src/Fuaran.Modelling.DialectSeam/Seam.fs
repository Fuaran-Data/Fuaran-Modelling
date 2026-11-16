// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.DialectSeam

open Fuaran.Core
open Fuaran.Modelling

// ============================================================================
//  The dialect seam.
//
//  The manager records decisions about spec states without ever reading one.
//  Everything it deliberately does not know — what a model is, which parameters
//  are legal, what the payload crossing the execution wire looks like — lives
//  behind this seam, in a DIALECT.
//
//  A dialect supplies six things, and this file is the contract for all six:
//
//    1. spec-tree types      the closed vocabulary of its model class
//    2. validators           the deterministic verify tier
//    3. a projector          typed tree -> canonical payload bytes (+ the hash)
//    4. a parse-back importer with a RESIDUE report
//    5. default gate vocabulary, on the generic (name, threshold, direction) triple
//    6. a pack schema        priors / calibration / hierarchy bundles
//
//  The seam is a PUBLIC extension point: dialects are written by people who did
//  not write this library and cannot be asked to change it. So the contract is
//  executable (see `Conformance`) rather than described, it is versioned, and
//  version skew is a typed refusal rather than a subtle misbehaviour.
// ============================================================================

/// The version of the seam contract itself.
///
/// Two integers, compared by a stated rule rather than by ordering intuition: a MAJOR
/// change removes or re-types an obligation, a MINOR change adds one that older dialects
/// can decline to use. Anything that would break a conforming dialect is major, and that
/// is the only test.
type SeamVersion = { Major: int; Minor: int }

/// Why a dialect could not be used at all — the skew refusal, kept apart from every
/// other refusal in this file because it is answered differently. A dialect that fails a
/// validator obligation has a bug; a dialect that targets a seam this host does not
/// implement is not broken, it is not applicable, and the caller's move is to obtain a
/// different build rather than to fix anything.
type SeamRefusal =
    /// The dialect targets a different major seam. No compatibility is possible.
    | MajorSkew of dialect: SeamVersion * host: SeamVersion
    /// The dialect targets a newer minor of this major — it may rely on obligations this
    /// host does not implement.
    | NewerMinor of dialect: SeamVersion * host: SeamVersion

[<RequireQualifiedAccess>]
module SeamVersion =

    /// The seam version this library implements.
    ///
    /// 1.1 added the OPTIONAL cross-sibling invariant obligation (`IInvariantComparators`).
    /// Minor, on D14's only test — a dialect written against 1.0 declines to implement the new
    /// interface, keeps compiling, and keeps conforming, because the obligation is carried by a
    /// separate interface rather than by a member on `IDialect`. Adding a member there would
    /// have stopped every existing dialect compiling, which is a MAJOR change however small the
    /// member.
    let current: SeamVersion = { Major = 1; Minor = 1 }

    let encode (v: SeamVersion) : string = string v.Major + "." + string v.Minor

    let tryParse (s: string) : Result<SeamVersion, string> =
        match s.Split '.' with
        | [| major; minor |] ->
            match System.Int32.TryParse major, System.Int32.TryParse minor with
            | (true, ma), (true, mi) when ma >= 0 && mi >= 0 -> Ok { Major = ma; Minor = mi }
            | _ ->
                Error(
                    "malformed seam version '"
                    + s
                    + "' (expected <major>.<minor>, both non-negative)"
                )
        | _ -> Error("malformed seam version '" + s + "' (expected <major>.<minor>)")

    /// May a host at `host` run a dialect targeting `dialect`?
    ///
    /// Same major, and the dialect's minor no newer than the host's. An OLDER minor is
    /// fine in both directions of reading: the dialect uses fewer obligations than the
    /// host implements, which is exactly what an additive minor means.
    let check (host: SeamVersion) (dialect: SeamVersion) : Result<unit, SeamRefusal> =
        if dialect.Major <> host.Major then
            Error(MajorSkew(dialect, host))
        elif dialect.Minor > host.Minor then
            Error(NewerMinor(dialect, host))
        else
            Ok()

[<RequireQualifiedAccess>]
module SeamRefusal =

    let describe (r: SeamRefusal) : string =
        match r with
        | MajorSkew(d, h) ->
            "dialect targets seam "
            + SeamVersion.encode d
            + "; this host implements "
            + SeamVersion.encode h
            + " — a major difference removes or re-types an obligation, so no compatibility is possible"
        | NewerMinor(d, h) ->
            "dialect targets seam "
            + SeamVersion.encode d
            + "; this host implements "
            + SeamVersion.encode h
            + " — the dialect may rely on obligations this host does not implement"

/// Who a dialect says it is.
///
/// `Id` and `PayloadFormat` are the two strings the manager already carries on every
/// `SpecPayload` (`Dialect` / `Format`), so they are not decoration: a projected payload
/// whose tags disagree with its dialect's identity is unattributable the moment it is
/// stored, and the conformance harness treats the disagreement as a failure rather than
/// a style note.
///
/// `SpecHashAlgorithm` names the MINTING RULE — the byte-level canonicalisation applied
/// to a spec before hashing. It is recorded rather than assumed because the manager never
/// re-derives a hash (it cannot: the payload's canonical form is dialect knowledge), so
/// the algorithm identifier is the only thing that lets a later reader tell two minting
/// rules apart. A dialect that rotates its rule declares a new identifier.
type DialectIdentity =
    { Id: string
      Version: string
      PayloadFormat: string
      SpecHashAlgorithm: string
      TargetsSeam: SeamVersion }

// ============================================================================
//  The minting rule lives in ONE place, and it is not this file.
//
//  `canonical-json-sha256-v1` is a REGISTERED identifier (model-execution wire
//  specification §7.4), and this seam is not its home — the execution mirror is,
//  where the §3 encoder it is defined in terms of already lives and is certified
//  against the specification's own `spec-hash` vectors. A dialect that mints
//  reaches `Fuaran.Modelling.Execution.Minting`; there is no second
//  implementation here to drift from it. See D25.
//
//  This mattered more than it looks. The rule is stated in terms of §3.1's
//  escaping and number layout, and a second implementation built on the substrate's
//  own canonical-JSON discipline agreed with the registered rule on simple
//  documents and diverged on three whole classes of them — control characters
//  (short escapes against `\u00xx`), real numbers (the ECMAScript layout against
//  the .NET round-trip one), and `null`, which the substrate's value model cannot
//  represent at all. Two tiers declaring one identifier and minting three
//  different answers is precisely the failure §4.2 rule 2 makes undetectable
//  downstream: nobody is permitted to check a hash they were handed, so it
//  surfaces as a join that silently misses.
//
//  What survives here is the one piece that is genuinely the SEAM's rather than
//  the wire's: `Seam.mintTotal`, the totalisation that makes `IDialect.Mint`
//  answerable for arbitrary bytes.
// ============================================================================

/// How much a validator finding matters.
///
/// Only two levels, and the boundary is load-bearing rather than editorial: `Blocking`
/// means the projector MUST refuse, `Advisory` means it MUST NOT. A third level would
/// have no answer to "does this project?", which is the only question the seam asks of a
/// finding.
type Severity =
    | Blocking
    | Advisory

[<RequireQualifiedAccess>]
module Severity =

    let encode (s: Severity) : string =
        match s with
        | Blocking -> "blocking"
        | Advisory -> "advisory"

    let tryParse (s: string) : Result<Severity, string> =
        match s with
        | "blocking" -> Ok Blocking
        | "advisory" -> Ok Advisory
        | other -> Error("unknown severity '" + other + "' (expected blocking | advisory)")

/// One thing a validator found.
///
/// `Code` is a stable identifier the dialect owns and never re-purposes — a caller
/// branches on it, and a fixture pins it. `Message` is for a human and may be reworded
/// freely; nothing in the seam compares messages, on purpose.
type Finding =
    { Code: string
      Severity: Severity
      Path: string
      Message: string }

[<RequireQualifiedAccess>]
module Finding =

    /// Does this finding stop a projection?
    let blocks (f: Finding) : bool = f.Severity = Blocking

    /// The canonical comparison order — code, then path. Findings are compared as sorted
    /// sequences so a dialect is not forced into an arbitrary emission order, while its
    /// own emission order still has to be STABLE (checked separately).
    let sortKey (f: Finding) : string * string = f.Code, f.Path

/// One thing an import did not bring across.
///
/// The residue report is the difference between an importer and a parser. A brownfield
/// document written for another tool will contain terms this dialect does not model;
/// dropping them silently produces a spec that looks complete and is not, and the person
/// who finds out is whoever compares the fit to the original months later. So the
/// importer's contract is: model what you can, and NAME the rest.
type Residue =
    { Path: string
      Term: string
      Detail: string }

/// What an import produced: the typed spec, and everything that did not fit in it.
type ImportOutcome<'Spec> = { Spec: 'Spec; Residue: Residue list }

/// Why an import produced nothing at all. Distinct from residue by kind, not by degree:
/// residue is a successful import that left something behind, this is no import.
type ImportFailure =
    /// The document is not well-formed in the declared format.
    | Malformed of detail: string
    /// The document is in a format this dialect does not read.
    | UnsupportedFormat of format: string * supported: string list
    /// The document is well-formed but says something the dialect's tree cannot hold at
    /// all — as opposed to something it can drop and report.
    | Unmappable of path: string * detail: string

[<RequireQualifiedAccess>]
module ImportFailure =

    let describe (f: ImportFailure) : string =
        match f with
        | Malformed detail -> "malformed document: " + detail
        | UnsupportedFormat(format, supported) ->
            "unsupported format '"
            + format
            + "' (this dialect reads "
            + String.concat ", " supported
            + ")"
        | Unmappable(path, detail) -> "unmappable at '" + path + "': " + detail

/// A gate the dialect offers by default, described for a human.
///
/// The request itself is the manager's generic `(name, threshold, direction)` triple —
/// the dialect's own gate vocabulary maps ONTO that triple rather than replacing it, so
/// a host, a policy or an execution backend can reason about gates from any dialect
/// without knowing any of them.
type GateSpec =
    { Request: GateRequest
      Description: string }

/// A parameter bundle a dialect can fold into a spec — priors, calibration results, a
/// hierarchy. Opaque to the seam exactly as a spec payload is opaque to the manager: the
/// seam carries the envelope, the dialect reads the body.
type Pack =
    { Id: string
      Kind: string
      Version: string
      Body: string }

/// Why a pack was not applied.
type PackRefusal =
    /// A kind this dialect does not declare. Refused rather than ignored — a pack that
    /// silently does nothing is indistinguishable from one that worked.
    | UnknownPackKind of kind: string * declared: string list
    | MalformedPack of detail: string
    /// The pack is well-formed and applicable but disagrees with the spec it was applied
    /// to (a prior for a term that is not there, two packs setting one bound).
    | PackConflicts of path: string * detail: string

[<RequireQualifiedAccess>]
module PackRefusal =

    let describe (r: PackRefusal) : string =
        match r with
        | UnknownPackKind(kind, declared) ->
            "unknown pack kind '"
            + kind
            + "' (this dialect declares "
            + String.concat ", " declared
            + ")"
        | MalformedPack detail -> "malformed pack: " + detail
        | PackConflicts(path, detail) -> "pack conflicts at '" + path + "': " + detail

/// The dialect contract: the six obligations, as one interface.
///
/// Generic in `'Spec` — the dialect's own closed spec tree — because that type is the one
/// thing the seam must NOT specify. Everything else crossing this interface is a string,
/// a number, or a closed enumeration declared here, so a dialect couples to the seam and
/// to nothing else.
///
/// The obligations are stated as laws in `Conformance`, and every one of them is checked
/// executably. In particular:
///
///   * `Project` refuses EXACTLY when `Validate` returns a blocking finding — neither
///     more (a valid spec must project) nor less (a blocking-invalid one must not);
///   * `Project` is deterministic to the byte, and `SpecHash` is `Mint` applied to those
///     bytes — nothing else;
///   * `Import` reports residue for everything it did not model, and is deterministic;
///   * an undeclared pack kind is refused.
type IDialect<'Spec when 'Spec: equality> =

    /// Who this dialect is, and which seam it targets.
    abstract Identity: DialectIdentity

    /// The closed term vocabulary of this dialect's spec tree, for introspection and for
    /// the corpus coverage gate. Publishing it is what makes "closed" checkable: a
    /// vocabulary term no fixture exercises, and a fixture exercising a term the dialect
    /// does not publish, are both corpus failures.
    abstract Vocabulary: string list

    /// Obligation 2 — the deterministic verify tier. Total: a validator that throws
    /// cannot be the thing that decides whether something is valid.
    abstract Validate: 'Spec -> Finding list

    /// Obligation 3 — the projector. Renders the typed tree to the opaque payload that
    /// crosses the execution wire, minting the spec hash over its canonical bytes. The
    /// rendered projection IS the interface payload; nothing downstream sees the tree.
    ///
    /// Refuses with the blocking findings rather than a bare error, so a caller can show
    /// the user why without validating again.
    abstract Project: 'Spec -> Result<SpecPayload, Finding list>

    /// The minting rule named by `Identity.SpecHashAlgorithm`: canonical payload bytes to
    /// spec hash. Exposed separately from `Project` so the relationship between them is
    /// checkable — a dialect whose hash is not this function of its bytes has a hash that
    /// means nothing.
    abstract Mint: string -> string

    /// Obligation 4 — the parse-back importer. Brownfield acquisition and the
    /// app-native → manager-governed migration path, which are the same operation.
    abstract Import: string -> Result<ImportOutcome<'Spec>, ImportFailure>

    /// Obligation 5 — the default gate vocabulary, on the generic triple.
    abstract DefaultGates: GateSpec list

    /// Obligation 6 — the pack kinds this dialect declares.
    abstract PackKinds: string list

    /// Apply a pack to a spec. Deterministic, and refuses an undeclared kind.
    abstract ApplyPack: Pack -> 'Spec -> Result<'Spec, PackRefusal>

/// The OPTIONAL seventh obligation, added at seam 1.1: comparators over sibling states.
///
/// It is a SEPARATE interface, and that is the whole of why it could be added at all. A new
/// member on `IDialect` would stop every existing dialect compiling — a major change by D14's
/// only test, however small the member — whereas an interface a dialect may additionally
/// implement leaves every 1.0 dialect conforming, untouched, and honestly described by its own
/// `TargetsSeam`. A host discovers it by type test and gets an empty list where it is absent,
/// so "declines to implement" and "declares no invariants" are the same thing to a caller,
/// which is what makes the obligation optional rather than merely new.
///
/// It is deliberately NOT generic in `'Spec`. A comparator reads projected payloads — the
/// dialect's own renderings, imported back into its own trees on its own side of the seam —
/// so nothing about the manager's call needs the spec type, and a non-generic interface is one
/// a host can type-test without knowing which dialect it holds.
///
/// The comparator answers in the manager's vocabulary (`InvariantFinding`), which is what
/// keeps the manager the host of evaluation: it calls a function over opaque payloads and
/// stamps the result, and acquires no opinion about what an invariant is.
type IInvariantComparators =

    /// The invariants this dialect declares, each with a stable id it owns and never
    /// re-purposes — a declaration recorded against an id is only meaningful while the id
    /// means what it did — and a description a person choosing invariants reads.
    abstract Invariants: InvariantComparator list

/// Seam-level operations over any dialect.
[<RequireQualifiedAccess>]
module Seam =

    /// The minting rule a dialect uses unless it has a reason not to — the registered
    /// `canonical-json-sha256-v1`, taken from the execution mirror rather than
    /// reimplemented here (D25). Re-exported under this name so a dialect author reads one
    /// identifier in one place and cannot pick up a second rule by accident.
    let mintingAlgorithmId = Fuaran.Modelling.Execution.Minting.algorithmId

    /// The total form the seam's `Mint` obligation takes.
    ///
    /// A rendering outside the rule's domain is hashed AS GIVEN, under a distinct
    /// `raw:`-prefixed content address so the two cases can never be confused for one
    /// another. This never arises from a dialect's own projector — a projector emits
    /// renderings it can mint — and exists so that `Mint` is total, which is what lets a
    /// conformance harness probe it with arbitrary bytes.
    ///
    /// The totalisation is the SEAM's, which is why it is here and not in the mirror: the
    /// wire specification has no notion of a hash over unmintable bytes, and inventing one
    /// there would put a shape on the wire that no document may carry.
    let mintTotal (rendered: string) : string =
        match Fuaran.Modelling.Execution.Minting.mint rendered with
        | Ok hash -> hash
        | Error _ -> "raw:" + Fuaran.Modelling.Execution.Digest.sha256Hex rendered

    /// Is this dialect usable by a host implementing `host`? The skew check, as data.
    let checkVersion (host: SeamVersion) (d: IDialect<'Spec>) : Result<unit, SeamRefusal> =
        SeamVersion.check host d.Identity.TargetsSeam

    /// Is this dialect usable by THIS build?
    let check (d: IDialect<'Spec>) : Result<unit, SeamRefusal> = checkVersion SeamVersion.current d

    /// The blocking findings for a spec, in canonical order.
    let blockingFindings (d: IDialect<'Spec>) (spec: 'Spec) : Finding list =
        d.Validate spec |> List.filter Finding.blocks |> List.sortBy Finding.sortKey

    /// Project a spec, refusing first on seam skew.
    ///
    /// The order matters: a dialect this host cannot run must not be asked to produce a
    /// payload that would then be stored under a contract nobody implements.
    let project (d: IDialect<'Spec>) (spec: 'Spec) : Result<SpecPayload, Choice<SeamRefusal, Finding list>> =
        match check d with
        | Error skew -> Error(Choice1Of2 skew)
        | Ok() -> d.Project spec |> Result.mapError Choice2Of2

    /// The gate requests a submission would carry if it took the dialect's defaults.
    let defaultGateRequests (d: IDialect<'Spec>) : GateRequest list =
        d.DefaultGates |> List.map (fun g -> g.Request)

    // ---- the optional invariant obligation -----------------------------------

    /// The invariant comparators a dialect declares — empty when it declares none, and equally
    /// empty when it does not implement the obligation at all.
    ///
    /// Those two cases are deliberately indistinguishable to a caller. An optional obligation
    /// that a host could tell had been DECLINED would invite the host to treat declining as a
    /// defect, and then it is not optional; a dialect with nothing to say across siblings and
    /// one that predates the question are in the same position, and it is a correct one.
    let invariants (d: IDialect<'Spec>) : InvariantComparator list =
        match box d with
        | :? IInvariantComparators as c -> c.Invariants
        | _ -> []

    /// The invariant ids a dialect declares — the vocabulary `Forest.declareDivergence` gates
    /// a declaration against.
    let invariantIds (d: IDialect<'Spec>) : string list =
        invariants d |> List.map (fun c -> c.Id)

    /// Run a dialect's invariants over a projection, refusing first on seam skew.
    ///
    /// Same ordering argument as `project`: a dialect this host cannot run must not be asked a
    /// question whose answer would then be acted on — here, a finding downgraded (or not) by
    /// declarations recorded under a contract nobody implements.
    let compare (d: IDialect<'Spec>) (projected: ProjectedState list) : Result<InvariantFinding list, SeamRefusal> =
        match check d with
        | Error skew -> Error skew
        | Ok() -> Ok(Forest.evaluate (invariants d) projected)
