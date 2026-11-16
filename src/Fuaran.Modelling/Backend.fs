// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  The execution-backend seam — the minimal port the manager needs in order to
//  turn a spec state into an annotated outcome.
//
//  Deliberately narrow. The manager submits an opaque payload and receives an
//  outcome snapshot back; it never sees an estimator, a dataset, an artifact
//  format or a scheduler. Everything crossing this boundary is a plain record
//  of strings, numbers and enumerations, so a binding to any concrete executor
//  is a mapping exercise rather than a coupling.
//
//  Two things it does NOT do, both on purpose: it does not stay in the read
//  path (an outcome is snapshotted into the decision record, so analysis is
//  offline and needs no live backend), and it does not enforce process policy
//  (that is the manager's, above) — it enforces cost, which is its own.
// ============================================================================

/// A request to execute one fit for one spec state.
type FitSubmission =
    { StateId: StateId
      Payload: SpecPayload
      DatasetVersion: string
      Seed: string
      Gates: GateRequest list
      SubmittedBy: SubmitterClass }

// `BackendRefusal` used to live here, beside the port it describes, and moved to
// `Types.fs` when a replay receipt began carrying one per branch. The receipt is part of
// the folded record, so its refusal vocabulary has to be declared before the record is.
// Nothing about the type changed and no consumer sees a difference — it is the same name
// in the same namespace.

/// The port. One method, because Rung-0 needs exactly one: submit a fit, get an outcome
/// or a typed refusal. A batch submission is a list of these, and a fuller port —
/// scoring, artifact query, re-derivation of a dataset version — grows behind the same
/// seam without touching anything above it.
type IExecutionBackend =
    /// The provider identity recorded in every composite key this backend mints.
    abstract ProviderId: string
    abstract ProviderVersion: string
    abstract Submit: FitSubmission -> Result<OutcomeSnapshot, BackendRefusal>

/// A deterministic, in-memory reference backend.
///
/// Explicitly NOT statistics: it fits nothing and estimates nothing. Diagnostics are a
/// pure function of the spec hash and the diagnostic name, so the same submission always
/// yields the same numbers on every host — which is precisely what a reference
/// implementation is for. It exists to keep the seam honest and to let the manager, and
/// any dialect, develop with no executor present at all.
[<RequireQualifiedAccess>]
module ReferenceBackend =

    /// A stable unit-interval value derived from a string. FNV-1a via the portable
    /// substrate hash, read as hex — arbitrary but reproducible everywhere.
    let private unitOf (s: string) : float =
        let h = OpStream.defaultHash "" s
        let mutable acc = 0

        for ch in h do
            let d =
                if ch >= '0' && ch <= '9' then int ch - int '0'
                elif ch >= 'a' && ch <= 'f' then int ch - int 'a' + 10
                else 0

            acc <- acc * 16 + d
            // Keep the accumulator inside int range while staying a function of every digit.
            acc <- acc % 1000003

        float acc / 1000003.0

    /// A diagnostic value for `(specHash, name)` — deterministic, in [0, 1).
    let diagnostic (specHash: string) (name: string) : float = unitOf (specHash + "|" + name)

    /// The artifact reference this backend mints for a composite key. Content-derived, so
    /// the same fit names the same artifact on every host.
    let artifactRef (key: CompositeKey) : string =
        "ref:"
        + OpStream.defaultHash
            ""
            (key.SpecHash
             + "|"
             + key.DatasetVersion
             + "|"
             + key.Seed
             + "|"
             + key.ProviderId
             + "|"
             + key.ProviderVersion)

    /// Build a reference backend that reports `units` for the diagnostics they name (D39).
    ///
    /// The units are the provider's: a diagnostic's unit is a property of what the provider
    /// measures, so it is known before anything runs, and a gate stating another unit is
    /// refused by name before its comparison is made — never converted.
    ///
    /// `recordedAt` is supplied by the caller rather than read from a clock: an ambient
    /// clock is the one thing that would make an otherwise-deterministic record
    /// irreproducible, and a timestamp the caller controls is also a timestamp a test can
    /// assert on.
    let createWith
        (units: Map<string, UnitOfMeasure>)
        (providerId: string)
        (providerVersion: string)
        (recordedAt: string)
        : IExecutionBackend =
        { new IExecutionBackend with
            member _.ProviderId = providerId
            member _.ProviderVersion = providerVersion

            member _.Submit(sub: FitSubmission) =
                let key =
                    { SpecHash = sub.Payload.SpecHash
                      DatasetVersion = sub.DatasetVersion
                      Seed = sub.Seed
                      ProviderId = providerId
                      ProviderVersion = providerVersion }

                let evaluated =
                    (Ok [], sub.Gates)
                    ||> List.fold (fun acc g ->
                        acc
                        |> Result.bind (fun verdicts ->
                            GateVerdict.evaluateIn (Map.tryFind g.Name units) g (diagnostic key.SpecHash g.Name)
                            |> Result.map (fun v -> v :: verdicts)))
                    |> Result.map List.rev

                match evaluated with
                | Error mismatch -> Error(GateUnitMismatch mismatch)
                | Ok gates ->
                    let diagnostics = gates |> List.map (fun g -> g.Name, g.Observed) |> Map.ofList

                    Ok
                        { Key = key
                          ArtifactRef = artifactRef key
                          Diagnostics = diagnostics
                          DiagnosticUnits = units |> Map.filter (fun name _ -> Map.containsKey name diagnostics)
                          Gates = gates
                          SubmittedBy = sub.SubmittedBy
                          RecordedAt = recordedAt } }

    /// A reference backend that states no units — every gate that states none is evaluated
    /// exactly as before units existed.
    let create (providerId: string) (providerVersion: string) (recordedAt: string) : IExecutionBackend =
        createWith Map.empty providerId providerVersion recordedAt

    /// A backend that refuses everything with a fixed reason — the other half of the
    /// seam's contract. A port whose refusal path is never exercised is a port whose
    /// refusal path does not work.
    let refusing (providerId: string) (providerVersion: string) (refusal: BackendRefusal) : IExecutionBackend =
        { new IExecutionBackend with
            member _.ProviderId = providerId
            member _.ProviderVersion = providerVersion
            member _.Submit(_: FitSubmission) = Error refusal }
