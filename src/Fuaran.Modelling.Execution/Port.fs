// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The execution-backend PORT — the manager's entire backend-facing surface.
//
//  Six operations, every one of them returning a typed refusal in the error
//  channel. Nothing crossing this interface is an exception, an error string, or
//  a numeric code whose meaning lives elsewhere: a caller deciding whether to
//  wait, reduce scope, re-aim at another provider or ask a human has to branch
//  on the reason, and it cannot branch on prose.
//
//  BATCH AND PAGINATION ARE FIRST-CLASS FROM v1, and single-item is the
//  degenerate case rather than the other way round. This is not symmetry for its
//  own sake. The design load is a submitter replaying many configurations across
//  many vintages — a forest, not a model — and a single-item interface
//  retrofitted with batching produces a batch endpoint whose semantics are a
//  loop. A loop has no partial-acceptance shape, so the receipt, which is the
//  whole point, could not be added later without breaking every caller.
//
//  Four of the six mirror a specified wire document. Two do not, and are marked:
//  the interface registers no artifact-query and no re-vintage document, so
//  `QueryArtifacts` and `RequestRevintage` are this port's own. Saying which is
//  which is the difference between a snapshot and a pile.
// ============================================================================

/// The port. One interface, six operations, one error channel.
///
/// Deliberately says nothing about transport. The same operations are satisfied in
/// process, over HTTP, through a queue, or by a file on disk — a binding is a mapping
/// exercise, and if it ever needs to be more than that, something has leaked through
/// here that should not have.
type IModelExecutionBackend =

    /// Which provider ran a fit, and which version of it. Both are components of the
    /// composite key, so an implementation MUST NOT change either without accepting
    /// that every fit run after the change is a different fit.
    abstract ProviderId: string

    abstract ProviderVersion: string

    /// Submit a batch of fits. The answer is a RECEIPT, not an outcome: it says the
    /// work was taken, never that it finished, and partial acceptance is data rather
    /// than an error. A submitter learns outcomes by querying, not by holding a
    /// connection open — the alternative makes long-running work unrecoverable across
    /// a restart on either side.
    abstract SubmitFits: FitSubmissionBatch -> Result<SubmissionReceipt, Refusal>

    /// Read registered outcomes, a page at a time. Filters are conjunctive and an
    /// empty filter matches anything, so a single-outcome read is the degenerate query.
    abstract GetOutcomes: RegistryQuery -> Result<OutcomePage, Refusal>

    /// Resolve a vintage pin to the bytes it names.
    abstract ResolveVintage: VintageRef -> Result<ResolvedVintage, Refusal>

    /// Apply a registered artifact to data. Predictions land as a new vintage of the
    /// output dataset — never as an inline array — so a model fitted to scored output
    /// is as reproducible as one fitted to source.
    abstract Score: ScoreRequest -> Result<ResolvedVintage, Refusal>

    /// Which stored artifacts match. **Not a mirrored operation** — no wire document
    /// is registered for it.
    abstract QueryArtifacts: ArtifactQuery -> Result<ArtifactPage, Refusal>

    /// Ask that a dataset be re-derived into a new vintage. **Not a mirrored
    /// operation**, and deliberately opaque: dataset authoring does not cross this
    /// interface, so the assembly reference is a handle the executor already
    /// understands and this library never constructs or inspects.
    abstract RequestRevintage: RevintageRequest -> Result<ResolvedVintage, Refusal>

/// Operations over any backend.
[<RequireQualifiedAccess>]
module Port =

    /// Submit a single fit — the degenerate batch, spelled once here so that no caller
    /// has to invent a batch id or remember that a receipt indexes by position.
    let submitOne (backend: IModelExecutionBackend) (batchId: string) (submission: FitSubmission) =
        backend.SubmitFits
            { BatchId = batchId
              Submissions = [ submission ] }

    /// Did a receipt account for everything it was given?
    ///
    /// `accepted` and `rejected` MUST partition the submitted indices exactly. Checked
    /// rather than assumed because the failure it catches — an item that is in neither
    /// list — is a fit the submitter believes was taken and the executor has no record
    /// of, which nothing downstream can detect.
    let receiptIsTotal (receipt: SubmissionReceipt) : bool =
        let indices =
            (receipt.Accepted |> List.map (fun a -> a.Index))
            @ (receipt.Rejected |> List.map (fun r -> r.Index))

        List.length indices = receipt.ItemCount
        && List.length (List.distinct indices) = receipt.ItemCount
        && indices |> List.forall (fun i -> i >= 0 && i < receipt.ItemCount)

    /// Read every page of a query.
    ///
    /// The paging loop, written once and correctly, because every part of it is a place
    /// to get it wrong: the cursor is echoed EXACTLY as received and is never parsed or
    /// constructed; an exhausted cursor is a refusal rather than a silent restart, which
    /// would turn this loop into an infinite one; and `maxPages` bounds it regardless,
    /// because a backend that keeps handing back a cursor is a backend this function
    /// must still return from.
    let readAll
        (backend: IModelExecutionBackend)
        (maxPages: int)
        (query: RegistryQuery)
        : Result<FitOutcome list, Refusal> =
        let rec go (pagesLeft: int) (cursor: string option) (acc: FitOutcome list) =
            if pagesLeft <= 0 then
                Ok(List.rev acc)
            else
                let request =
                    { query with
                        Page = { query.Page with Cursor = cursor } }

                match backend.GetOutcomes request with
                | Error refusal -> Error refusal
                | Ok page ->
                    let acc = (List.rev page.Outcomes) @ acc

                    match page.NextCursor with
                    | None -> Ok(List.rev acc)
                    | Some next -> go (pagesLeft - 1) (Some next) acc

        go maxPages query.Page.Cursor []

    /// A backend that offers nothing, refusing every operation with
    /// `substrateUnavailable` naming the surface asked for.
    ///
    /// This is not a test double. A deployment that composes some of the interface and
    /// not the rest needs a shape for "not offered here" that is distinguishable from
    /// "absent" and from "failed" — and a port whose refusal path is never exercised is
    /// a port whose refusal path does not work.
    let unavailable (providerId: string) (providerVersion: string) : IModelExecutionBackend =
        { new IModelExecutionBackend with
            member _.ProviderId = providerId
            member _.ProviderVersion = providerVersion

            member _.SubmitFits(_: FitSubmissionBatch) =
                Error(SubstrateUnavailable "submitFits")

            member _.GetOutcomes(_: RegistryQuery) =
                Error(SubstrateUnavailable "getOutcomes")

            member _.ResolveVintage(_: VintageRef) =
                Error(SubstrateUnavailable "resolveVintage")

            member _.Score(_: ScoreRequest) = Error(SubstrateUnavailable "score")

            member _.QueryArtifacts(_: ArtifactQuery) =
                Error(SubstrateUnavailable "queryArtifacts")

            member _.RequestRevintage(_: RevintageRequest) =
                Error(SubstrateUnavailable "requestRevintage") }
