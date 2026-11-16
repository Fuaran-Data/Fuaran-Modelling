// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

open Fuaran.Core

// ============================================================================
//  The bridge: the manager's decision record and the execution interface.
//
//  This is the ONLY file in this tier that knows the manager exists. Everything
//  above it is the mirror and nothing else, so that the snapshot cannot quietly
//  acquire the manager's shapes — which is the way a mirror stops being one.
//
//  It is also the only way an outcome reaches the decision record. The manager's
//  narrow `IExecutionBackend` is not reimplemented here; it is DERIVED from the
//  port, so a fit submitted by the manager travels: manager submission -> wire
//  batch -> receipt -> registry query -> outcome snapshot -> `Annotate` op. Every
//  step crosses the port. There is no path that annotates a branch with an
//  outcome the port did not produce, and there is no second entry point through
//  which one could be added without deleting this sentence first.
//
//  Two projections are worth stating outright, because both look arbitrary and
//  neither is:
//
//    * A dataset version is ONE string to the manager and a `(datasetId,
//      version)` pair on the wire. The projection is `datasetId@version`, and it
//      is lossless because a vintage is one IMMUTABLE version of a named dataset
//      — the pair determines the bytes, so nothing is dropped by not carrying
//      the content ref through it.
//    * The manager's refusal vocabulary is NARROWER than the wire's. Four
//      classes map across exactly; the rest arrive as a policy refusal naming
//      the wire class they came from. That is a deliberate narrowing rather than
//      a silent loss — the class survives as data, in a string a caller can
//      branch on — and it is not widened here because widening the manager's
//      closed refusal union is a breaking change to a contract that ships.
// ============================================================================

/// How the manager's one-string dataset version and the wire's `(datasetId, version)`
/// pair name the same vintage.
[<RequireQualifiedAccess>]
module VintageName =

    let encode (v: VintageRef) : string = v.DatasetId + "@" + string v.Version

    /// Read `datasetId@version` back as an UNPINNED reference — the ordinary shape a
    /// submitter emits, which pins which vintage and leaves the executor to resolve
    /// where the bytes are.
    let tryParse (s: string) : Result<VintageRef, string> =
        match s.LastIndexOf '@' with
        | i when i > 0 && i < s.Length - 1 ->
            match System.Int32.TryParse(s.Substring(i + 1)) with
            | true, version when version >= 1 ->
                Ok
                    { DatasetId = s.Substring(0, i)
                      Version = version
                      ContentRef = None }
            | _ -> Error("'" + s + "' does not name a dataset version (expected <datasetId>@<version>)")
        | _ -> Error("'" + s + "' does not name a dataset version (expected <datasetId>@<version>)")

/// What the bridge must be told, because the manager's own records do not carry it.
type BridgeConfig =
    {
        /// Which class of provider these fits are aimed at (§7.3). Not on the manager's
        /// submission because the manager has no opinion about providers — it records
        /// decisions, and which engine runs them is a deployment's business.
        ProviderKind: string
        /// The minting rule the submitting dialect used (§7.4).
        ///
        /// The manager's `SpecPayload` carries the hash but not the algorithm that
        /// produced it, so it is supplied here. It is not defaulted: a hash whose
        /// minting rule is guessed is a hash that joins two records which may never
        /// have been the same fit.
        SpecHashAlgorithm: string
        /// The correlation-id prefix for the one-item batches this bridge submits.
        BatchPrefix: string
    }

/// What a replay campaign did, and the session it left behind.
type ReplayRun =
    {
        Receipt: Fuaran.Modelling.ReplayReceipt
        Session: Fuaran.Modelling.Session
        /// The snapshots that landed, in the order the plan named their branches.
        Landed: (Fuaran.Modelling.StateId * Fuaran.Modelling.OutcomeSnapshot) list
    }

/// Why a replay campaign did not run at all.
///
/// Note what is NOT here: a branch the executor would not take, a branch whose outcome the
/// fold refused. Those are per-branch facts, they live on the receipt, and the campaign
/// completes around them. Only the four things that stop the campaign as a whole are
/// failures, and each of them stops it before or after any evidence exists rather than in
/// the middle of it.
type ReplayFailure =
    /// The subject may not spend compute, or may not record a replay. Checked BEFORE
    /// anything is submitted, so a refused campaign leaves no trace and spends nothing.
    | ReplayPolicyBlocked of Fuaran.Modelling.PolicyRefusal
    /// A submission could not be built at all — a seed that is not an integer, a dataset
    /// version that names no vintage. Names the branch it came from.
    | ReplayUnsubmittable of Fuaran.Modelling.StateId * string
    /// The executor refused the BATCH as a whole. Refusing ITEMS within a batch is data on
    /// the receipt; refusing the batch is the executor declining to take the work.
    | ReplayBatchRefused of Refusal
    /// The receipt itself could not be recorded — the one failure that leaves annotated
    /// outcomes with no account of the campaign that gathered them, which is why the fold's
    /// rejection is carried out whole rather than summarised.
    | ReplayReceiptRejected of Fuaran.Modelling.Rejection

/// Mapping between the manager's records and the mirrored wire objects.
[<RequireQualifiedAccess>]
module Bridge =

    // ---- manager -> wire -----------------------------------------------------

    let toWireDirection (d: Fuaran.Modelling.GateDirection) : GateDirection =
        match d with
        | Fuaran.Modelling.AtLeast -> AtLeast
        | Fuaran.Modelling.AtMost -> AtMost

    let toWireSubmitter (s: Fuaran.Modelling.SubmitterClass) : SubmitterClass =
        match s with
        | Fuaran.Modelling.SubmitHuman -> SubmitterHuman
        | Fuaran.Modelling.SubmitScheduled -> SubmitterScheduled
        | Fuaran.Modelling.SubmitAgent -> SubmitterAgent

    let toWireGate (g: Fuaran.Modelling.GateRequest) : Gate =
        { Name = g.Name
          Threshold = g.Threshold
          // A unit crosses as its one canonical text (§3.5), which is what `render` writes.
          Unit = g.Unit |> Option.map Unit.render
          Direction = toWireDirection g.Direction }

    /// A manager fit submission as a wire submission.
    ///
    /// `SpecHash` is CARRIED, never re-derived — the manager cannot re-derive it (the
    /// payload is opaque to it, so it has no canonicalisation for the interior) and the
    /// executor is forbidden to. It was minted by whoever authored the specification,
    /// and everything downstream keys by exactly that value.
    let toWireSubmission (config: BridgeConfig) (s: Fuaran.Modelling.FitSubmission) : Result<FitSubmission, string> =
        VintageName.tryParse s.DatasetVersion
        |> Result.bind (fun vintage ->
            match System.Int64.TryParse s.Seed with
            | true, seed ->
                Ok
                    { Vintage = vintage
                      SpecPayload = s.Payload.Body
                      SpecHash = s.Payload.SpecHash
                      SpecHashAlgorithm = config.SpecHashAlgorithm
                      ProviderKind = config.ProviderKind
                      Seed = seed
                      Gates = s.Gates |> List.map toWireGate
                      SubmitterClass = toWireSubmitter s.SubmittedBy }
            | _ ->
                // The seed is part of the composite key, so it is the submitter's
                // choice and must survive as one. A seed that is not an integer names
                // nothing on the wire, and inventing one here would silently re-key
                // somebody else's fit.
                Error(
                    "the seed '"
                    + s.Seed
                    + "' is not a 64-bit integer, and the composite key requires one"
                ))

    // ---- wire -> manager -----------------------------------------------------

    let toManagerDirection (d: GateDirection) : Fuaran.Modelling.GateDirection =
        match d with
        | AtLeast -> Fuaran.Modelling.AtLeast
        | AtMost -> Fuaran.Modelling.AtMost

    /// A unit's wire text as the manager's unit. The codec has already refused a document
    /// whose unit text is not canonical; a mirror value that arrived some other way owes
    /// the same check, so it is made again here rather than assumed.
    let private toManagerUnit (text: string) : Result<UnitOfMeasure, string> = Fuaran.Modelling.UnitText.tryParse text

    let private toManagerUnitOpt (text: string option) : Result<UnitOfMeasure option, string> =
        match text with
        | None -> Ok None
        | Some t -> toManagerUnit t |> Result.map Some

    let toManagerVerdict (v: GateVerdict) : Result<Fuaran.Modelling.GateVerdict, string> =
        toManagerUnitOpt v.Unit
        |> Result.map (fun u ->
            { Name = v.Name
              Threshold = v.Threshold
              Unit = u
              Direction = toManagerDirection v.Direction
              Observed = v.Observed
              Passed = v.Passed })

    let private traverse (f: 'a -> Result<'b, string>) (xs: 'a list) : Result<'b list, string> =
        (xs, Ok [])
        ||> List.foldBack (fun x acc ->
            match f x, acc with
            | Ok y, Ok ys -> Ok(y :: ys)
            | Error e, _
            | _, Error e -> Error e)

    /// The manager's five-string composite key, from the wire's.
    let toManagerKey (k: CompositeKey) : Fuaran.Modelling.CompositeKey =
        { SpecHash = k.SpecHash
          DatasetVersion = VintageName.encode k.Vintage
          // Kept in the wire's own sign-prefixed form rather than re-rendered, so that
          // the manager's record and the executor's agree character for character on a
          // component of the join.
          Seed = Canonical.int64Text k.Seed
          ProviderId = k.ProviderId
          ProviderVersion = k.ProviderVersion }

    /// A wire outcome as the snapshot that lands in the decision record.
    ///
    /// `submittedBy` is supplied rather than read off the outcome: the wire's outcome
    /// records what the executor observed, not who asked — which is right, because who
    /// asked is a fact about the decision and belongs on the decision's side.
    ///
    /// An outcome that retained no artifact carries an empty reference. It is not an
    /// error: a fit that did not clear its gates is registered anyway, because a
    /// failure is evidence, and it is exactly the case where there is nothing to point
    /// at.
    ///
    /// An outcome whose units are not unit text is refused rather than recorded with them
    /// dropped: a snapshot is the evidence base, and evidence that silently lost what its
    /// numbers measure is worse than evidence that was never taken.
    let toManagerOutcome
        (submittedBy: Fuaran.Modelling.SubmitterClass)
        (o: FitOutcome)
        : Result<Fuaran.Modelling.OutcomeSnapshot, Fuaran.Modelling.BackendRefusal> =
        let units =
            o.DiagnosticUnits
            |> Map.toList
            |> traverse (fun (name, text) -> toManagerUnit text |> Result.map (fun u -> name, u))
            |> Result.map Map.ofList

        match units, traverse toManagerVerdict o.GateVerdicts with
        | Ok units, Ok gates ->
            Ok
                { Key = toManagerKey o.CompositeKey
                  ArtifactRef = o.ArtifactRef |> Option.map (fun a -> a.ArtifactId) |> Option.defaultValue ""
                  Diagnostics = o.Diagnostics
                  DiagnosticUnits = units
                  Gates = gates
                  SubmittedBy = submittedBy
                  RecordedAt = o.RegisteredAt }
        | Error e, _
        | _, Error e -> Error(Fuaran.Modelling.BackendPolicyRefused("unreadable outcome: " + e))

    /// A wire refusal as the manager's narrower one.
    ///
    /// Four classes map across exactly, because they are the four the manager was given
    /// a case for. Everything else becomes a policy refusal whose rule NAMES the wire
    /// class — so a caller can still branch on what happened, and nothing is silently
    /// flattened into "it failed".
    let toManagerRefusal (r: Refusal) : Fuaran.Modelling.BackendRefusal =
        match r with
        | BudgetDenied(quota, spent, _) -> Fuaran.Modelling.BudgetDenied(quota, spent)
        | GateFailed verdicts ->
            match traverse toManagerVerdict verdicts with
            | Ok vs -> Fuaran.Modelling.GateFailed vs
            | Error e -> Fuaran.Modelling.BackendPolicyRefused("wire:gateFailed with an unreadable unit: " + e)
        | GateUnitMismatch(name, gateUnit, reported) ->
            match toManagerUnitOpt gateUnit, toManagerUnitOpt reported with
            | Ok stated, Ok reported ->
                Fuaran.Modelling.GateUnitMismatch
                    { Gate = name
                      Stated = stated
                      Reported = reported }
            | Error e, _
            | _, Error e -> Fuaran.Modelling.BackendPolicyRefused("wire:gateUnitMismatch with an unreadable unit: " + e)
        | UnknownProvider(_, known) -> Fuaran.Modelling.UnknownProvider known
        | PolicyRefused rule -> Fuaran.Modelling.BackendPolicyRefused rule
        | other -> Fuaran.Modelling.BackendPolicyRefused("wire:" + Refusal.wireClass other)

    // ---- the narrow projection ----------------------------------------------

    /// Present a port as the manager's one-method backend.
    ///
    /// This is what "the port absorbs the narrow seam" means concretely: the manager
    /// keeps the interface it was built against, and that interface is now DERIVED from
    /// the wide one rather than implemented beside it. There is one path from a manager
    /// fit to an execution record, and it is this.
    ///
    /// **It requires an executor that registers synchronously.** The manager's seam
    /// answers a submission with an outcome, and the wire's answers it with a receipt —
    /// so this projection submits, then queries, and refuses if the registry has nothing
    /// yet. That is not a defect in either: submission is not completion, and an
    /// asynchronous executor is consumed through the wide port directly, where the
    /// receipt IS the answer and the outcome is collected later. The projection exists
    /// for the case the manager was designed around — a fit whose result is available
    /// when it is asked for — and says so rather than pretending to cover the other.
    let asExecutionBackend (config: BridgeConfig) (port: IModelExecutionBackend) : Fuaran.Modelling.IExecutionBackend =
        { new Fuaran.Modelling.IExecutionBackend with
            member _.ProviderId = port.ProviderId
            member _.ProviderVersion = port.ProviderVersion

            member _.Submit(submission: Fuaran.Modelling.FitSubmission) =
                match toWireSubmission config submission with
                | Error detail -> Error(Fuaran.Modelling.BackendPolicyRefused("unsubmittable: " + detail))
                | Ok wire ->
                    let batchId = config.BatchPrefix + "-" + submission.StateId

                    match Port.submitOne port batchId wire with
                    | Error refusal -> Error(toManagerRefusal refusal)
                    | Ok receipt ->
                        match receipt.Rejected with
                        | item :: _ -> Error(toManagerRefusal item.Reason)
                        | [] ->
                            // Read the outcome back through the port. The registry is
                            // the authoritative execution record; what lands in the
                            // decision record is a snapshot of it, which is what keeps
                            // every later analysis offline and re-derivable.
                            let query =
                                { SpecHashes = [ wire.SpecHash ]
                                  Vintages = [ wire.Vintage ]
                                  Statuses = []
                                  BatchId = Some batchId
                                  Page = Page.first Page.maxLimit }

                            match port.GetOutcomes query with
                            | Error refusal -> Error(toManagerRefusal refusal)
                            | Ok page ->
                                let mine =
                                    page.Outcomes
                                    |> List.tryFind (fun o ->
                                        o.CompositeKey.SpecHash = wire.SpecHash
                                        && o.CompositeKey.Seed = wire.Seed
                                        && o.CompositeKey.Vintage.DatasetId = wire.Vintage.DatasetId
                                        && o.CompositeKey.Vintage.Version = wire.Vintage.Version
                                        && o.CompositeKey.ProviderId = port.ProviderId
                                        && o.CompositeKey.ProviderVersion = port.ProviderVersion)

                                match mine with
                                | Some outcome -> toManagerOutcome submission.SubmittedBy outcome
                                | None ->
                                    Error(
                                        Fuaran.Modelling.BackendPolicyRefused
                                            "the executor took the submission but has registered no outcome for it yet"
                                    ) }

    /// Run one governed fit through the port and annotate the branch with what came
    /// back.
    ///
    /// The manager's three gates in their order: process policy decides whether this
    /// subject may spend compute at all, the executor decides whether this submission is
    /// admissible and what it produced, and the fold decides whether the resulting
    /// outcome may be recorded. Only the last one writes history.
    let submitFit
        (hashFn: HashFn)
        (config: BridgeConfig)
        (port: IModelExecutionBackend)
        (policy: Fuaran.Modelling.Policy)
        (subject: string)
        (actor: Actor)
        (submission: Fuaran.Modelling.FitSubmission)
        (session: Fuaran.Modelling.Session)
        =
        Fuaran.Modelling.Session.submitFit
            hashFn
            policy
            subject
            actor
            (asExecutionBackend config port)
            submission
            session

    // ---- replay onto a vintage -----------------------------------------------

    /// Does this registered outcome answer that submission?
    ///
    /// Matched on the composite key's own components rather than on the key hash, because the
    /// submitter does not compute the hash — §4.2 leaves that to whoever registers the
    /// outcome — and matching on a value one side derives is matching on an agreement rather
    /// than on an identity.
    let private answers (providerId: string) (providerVersion: string) (wire: FitSubmission) (o: FitOutcome) : bool =
        o.CompositeKey.SpecHash = wire.SpecHash
        && o.CompositeKey.Seed = wire.Seed
        && o.CompositeKey.Vintage.DatasetId = wire.Vintage.DatasetId
        && o.CompositeKey.Vintage.Version = wire.Vintage.Version
        && o.CompositeKey.ProviderId = providerId
        && o.CompositeKey.ProviderVersion = providerVersion

    /// Run one replay campaign: re-submit a planned branch set onto a new vintage AS ONE
    /// BATCH, land what came back, and record the receipt.
    ///
    /// This is the port's batch shape being used for the thing it was designed around. A
    /// submitter replaying many configurations across many vintages is the load the interface
    /// was widened for, and the receipt's partial-acceptance shape is what makes the campaign
    /// honest: an executor that will not take three of twenty branches says so per item, the
    /// other seventeen still run, and the account names all twenty.
    ///
    /// Both gates are checked BEFORE anything is submitted, and the second one matters more
    /// than it looks. A subject permitted to spend compute but not to record a replay would
    /// otherwise annotate a dozen branches and then fail to write the account of why they are
    /// there — evidence with no provenance, which is the one state this library is built to
    /// make impossible.
    let replayOnto
        (hashFn: HashFn)
        (config: BridgeConfig)
        (port: IModelExecutionBackend)
        (policy: Fuaran.Modelling.Policy)
        (subject: string)
        (actor: Actor)
        (replayedBy: string)
        (replayedAt: string)
        (plan: Fuaran.Modelling.ReplayPlan)
        (session: Fuaran.Modelling.Session)
        : Result<ReplayRun, ReplayFailure> =

        let authorised =
            Fuaran.Modelling.Policy.authoriseFit policy subject
            |> Result.bind (fun () ->
                Fuaran.Modelling.Policy.authorise policy subject (Fuaran.Modelling.MayEmit Fuaran.Modelling.ReplayKind))

        match authorised with
        | Error refusal -> Error(ReplayPolicyBlocked refusal)
        | Ok() ->

            let built =
                plan.Items
                |> List.fold
                    (fun acc item ->
                        match acc with
                        | Error e -> Error e
                        | Ok built ->
                            match toWireSubmission config item.Submission with
                            | Error detail -> Error(ReplayUnsubmittable(item.State, detail))
                            | Ok wire -> Ok((item, wire) :: built))
                    (Ok [])
                |> Result.map List.rev

            match built with
            | Error failure -> Error failure
            | Ok pairs ->
                let batchId = config.BatchPrefix + "-replay-" + plan.ReplayId

                match
                    port.SubmitFits
                        { BatchId = batchId
                          Submissions = pairs |> List.map snd }
                with
                | Error refusal -> Error(ReplayBatchRefused refusal)
                | Ok receipt ->
                    let rejectedAt =
                        receipt.Rejected |> List.map (fun r -> r.Index, r.Reason) |> Map.ofList

                    // One read for the whole campaign, filtered to this batch. The page bound is
                    // derived from the work asked for rather than guessed, so a campaign can
                    // never silently read fewer outcomes than it submitted.
                    let pages = (List.length pairs / Page.maxLimit) + 2

                    let registered =
                        match
                            Port.readAll
                                port
                                pages
                                { SpecHashes = []
                                  Vintages = []
                                  Statuses = []
                                  BatchId = Some batchId
                                  Page = Page.first Page.maxLimit }
                        with
                        | Ok outcomes -> Ok outcomes
                        | Error refusal -> Error refusal

                    // A registry read that failed does NOT abort the campaign: it refuses every
                    // branch that was accepted, with the executor's own reason, and the receipt
                    // says so. The alternative — throwing the whole run away — loses the account
                    // of work the executor has already done and will still bill for.
                    let landedOrRefused =
                        pairs
                        |> List.mapi (fun index (item, wire) ->
                            match Map.tryFind index rejectedAt with
                            | Some reason ->
                                item, Choice2Of2(Fuaran.Modelling.ExecutorRefused(toManagerRefusal reason))
                            | None ->
                                match registered with
                                | Error refusal ->
                                    item, Choice2Of2(Fuaran.Modelling.ExecutorRefused(toManagerRefusal refusal))
                                | Ok outcomes ->
                                    match
                                        outcomes |> List.tryFind (answers port.ProviderId port.ProviderVersion wire)
                                    with
                                    | None ->
                                        item,
                                        Choice2Of2(
                                            Fuaran.Modelling.ExecutorRefused(
                                                Fuaran.Modelling.BackendPolicyRefused
                                                    "the executor took the submission but has registered no outcome for it yet"
                                            )
                                        )
                                    | Some outcome ->
                                        match toManagerOutcome item.Submission.SubmittedBy outcome with
                                        | Ok snapshot -> item, Choice1Of2 snapshot
                                        | Error refusal -> item, Choice2Of2(Fuaran.Modelling.ExecutorRefused refusal))

                    // Each landed outcome enters the record by the ORDINARY annotate path, one at
                    // a time, through the ordinary duplicate-outcome guard. A branch already
                    // replayed onto this vintage is refused there and appears on the receipt as
                    // such — which is the identity rule doing its job, not the campaign failing.
                    let folded =
                        landedOrRefused
                        |> List.fold
                            (fun (s: Fuaran.Modelling.Session, landed, refused) (item, result) ->
                                match result with
                                | Choice2Of2 refusal -> s, landed, refused @ [ item.State, refusal ]
                                | Choice1Of2(snapshot: Fuaran.Modelling.OutcomeSnapshot) ->
                                    match
                                        Fuaran.Modelling.Session.submit
                                            hashFn
                                            actor
                                            (Fuaran.Modelling.Annotate
                                                { StateId = item.State
                                                  Snapshot = snapshot })
                                            s
                                    with
                                    | Ok(_, s') -> s', landed @ [ item.State, snapshot ], refused
                                    | Error rejection ->
                                        s, landed, refused @ [ item.State, Fuaran.Modelling.NotRecorded rejection ])
                            (session, [], [])

                    let session', landed, refused = folded

                    let account: Fuaran.Modelling.ReplayReceipt =
                        { ReplayId = plan.ReplayId
                          DatasetVersion = plan.DatasetVersion
                          Requested = Fuaran.Modelling.ReplayCampaign.requested plan
                          Landed = landed |> List.map (fun (state, snapshot) -> state, snapshot.Key)
                          Refused = plan.Refused @ refused
                          ReplayedBy = replayedBy
                          ReplayedAt = replayedAt }

                    match Fuaran.Modelling.Session.recordReplay hashFn policy subject actor account session' with
                    | Ok(_, s) ->
                        Ok
                            { Receipt = account
                              Session = s
                              Landed = landed }
                    | Error(Fuaran.Modelling.PolicyBlocked refusal) -> Error(ReplayPolicyBlocked refusal)
                    | Error(Fuaran.Modelling.FoldRejected rejection) -> Error(ReplayReceiptRejected rejection)
