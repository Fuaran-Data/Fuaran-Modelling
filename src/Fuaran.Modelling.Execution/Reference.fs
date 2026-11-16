// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The in-memory reference implementation of the port.
//
//  EXPLICITLY NOT STATISTICS. It fits nothing and estimates nothing. Every
//  number it produces is a pure function of the composite key and a name, so the
//  same submission yields the same diagnostics, the same verdicts, the same
//  artifact address and the same cost on every host and in every run — which is
//  precisely what a reference implementation is for, and precisely what an
//  estimator could not offer.
//
//  It exists so that the manager and a dialect can be developed, tested and
//  reasoned about with NO execution backend present at all. A seam whose other
//  side is imaginary is a seam nobody has tried to satisfy; this is the smallest
//  thing that makes it real.
//
//  Two disciplines it keeps that a real executor must also keep, and which are
//  the reason this file is longer than "return a plausible record":
//
//    * it REFUSES, in the vocabulary of the interface — an unknown provider
//      kind, an unresolvable vintage, a spent budget, an unusable cursor. A port
//      whose refusal path is never exercised is a port whose refusal path does
//      not work.
//    * it PAGES deterministically, ordered by an address that exists before the
//      outcome is stored and never changes, so paging is stable while new
//      outcomes are being registered.
//
//  It reads no clock. Every instant it records is one the caller supplied — an
//  ambient clock is the one thing that would make an otherwise-deterministic
//  record irreproducible, and a caller-supplied instant is also one a test can
//  assert on.
// ============================================================================

/// How a reference executor is composed.
type ReferenceExecutorConfig =
    {
        ProviderId: string
        ProviderVersion: string
        /// The provider kinds this deployment offers. A submission naming anything
        /// else is refused with the kinds that ARE available, so a submitter can
        /// re-aim without a second round trip.
        ProviderKinds: string list
        /// The vintages this executor can resolve. Each carries its content ref: a
        /// resolution that resolves nothing is not an answer.
        Vintages: VintageRef list
        /// Every instant this executor records. Supplied, never read from a clock.
        Instant: string
        /// A cost quota, in `core-seconds`. `None` means this deployment does not
        /// account for cost — which is a different statement from a quota of zero.
        Quota: float option
        /// The unit this provider reports each named diagnostic in, as canonical unit text
        /// (§3.5). A property of the provider, so it is known before anything runs, and a
        /// gate stated in another unit is refused before its comparison is made (§5.3's gate
        /// rule). A diagnostic with no entry is reported with no unit.
        DiagnosticUnits: Map<string, string>
    }

/// A deterministic, in-memory backend.
[<RequireQualifiedAccess>]
module ReferenceExecutor =

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        else 0

    /// A stable value in `[0, 1)` derived from a string.
    ///
    /// Thirteen hex digits — 52 bits — because that is the most that is exactly
    /// representable in a binary64, so the value round-trips through the canonical
    /// real encoding without the last digit moving. Arbitrary, but reproducible
    /// everywhere, which is the only property asked of it.
    let unitOf (s: string) : float =
        let hex = Digest.sha256Hex s
        let mutable acc = 0.0

        for i in 0..12 do
            acc <- acc * 16.0 + float (hexValue hex[i])

        acc / 4503599627370496.0

    /// A diagnostic value for `(specHash, name)`. Deterministic, in `[0, 1)`, and
    /// meaning nothing whatever — the interface assigns no meaning to a diagnostic
    /// name, and neither does this.
    let diagnostic (specHash: string) (name: string) : float = unitOf (specHash + "|" + name)

    /// A content ref for a dataset version, derived from its own identity so that the
    /// same vintage names the same bytes on every host.
    let contentRef (datasetId: string) (version: int) : ContentRef =
        { Format = "parquet"
          Hash = Digest.sha256 (datasetId + "@" + string version)
          RowCount = Some(1000L + int64 (unitOf (datasetId + "#rows") * 100000.0)) }

    /// A fully-pinned vintage — the shape an executor holds, as against the unpinned
    /// shape a submitter ordinarily emits.
    let vintage (datasetId: string) (version: int) : VintageRef =
        { DatasetId = datasetId
          Version = version
          ContentRef = Some(contentRef datasetId version) }

    // ---- cursors -------------------------------------------------------------

    /// A cursor is OPAQUE to the caller. It is base64 of an address the executor
    /// already holds, encoded rather than handed over raw so that a caller which
    /// parses one — which the interface forbids — finds nothing useful when it does.
    let private encodeCursor (key: string) : string =
        System.Convert
            .ToBase64String(System.Text.Encoding.UTF8.GetBytes key)
            .TrimEnd('=')
            .Replace('+', '-')
            .Replace('/', '_')

    let private decodeCursor (cursor: string) : string option =
        try
            let padded = cursor.Replace('-', '+').Replace('_', '/')

            let padded =
                match padded.Length % 4 with
                | 0 -> padded
                | n -> padded + System.String('=', 4 - n)

            Some(System.Text.Encoding.UTF8.GetString(System.Convert.FromBase64String padded))
        with _ ->
            None

    /// One page of an addressed, ordered collection.
    ///
    /// An exhausted or unrecognised cursor is REFUSED, never silently restarted from
    /// the beginning: a silent restart turns a paging loop into an infinite one, and
    /// the caller has no way to tell that it happened.
    let private paginate
        (limit: int)
        (cursor: string option)
        (keyed: (string * 'T) list)
        : Result<'T list * string option, Refusal> =
        let ordered =
            keyed |> List.sortWith (fun (a, _) (b, _) -> System.String.CompareOrdinal(a, b))

        let after =
            match cursor with
            | None -> Ok ordered
            | Some c ->
                match decodeCursor c with
                | None -> Error(InvalidQuery("cursor is not one this executor issued: '" + c + "'"))
                | Some key ->
                    match ordered |> List.tryFindIndex (fun (k, _) -> k = key) with
                    | None -> Error(InvalidQuery("cursor names a position that no longer exists: '" + c + "'"))
                    | Some i -> Ok(ordered |> List.skip (i + 1))

        after
        |> Result.map (fun rest ->
            let taken = rest |> List.truncate limit
            let remaining = List.length rest - List.length taken

            let next =
                if remaining > 0 then
                    taken |> List.tryLast |> Option.map (fst >> encodeCursor)
                else
                    None

            taken |> List.map snd, next)

    // ---- the executor --------------------------------------------------------

    /// Build a reference executor.
    ///
    /// The state is closed over rather than exposed: a caller reads it back through the
    /// port, exactly as it would from anything else, because a reference implementation
    /// that offers a side door teaches its callers to use one.
    let create (config: ReferenceExecutorConfig) : IModelExecutionBackend =
        // A provider whose own units are not unit text would emit outcomes no conformant
        // reader accepts; that is a defect in how this executor was composed, and it is
        // refused at composition rather than discovered at the first read.
        for KeyValue(name, u) in config.DiagnosticUnits do
            match UnitText.check u with
            | Ok() -> ()
            | Error e -> invalidArg "config" ("diagnostic '" + name + "': " + e)

        // Registered outcomes, addressed by composite-key hash; the batch that produced
        // each is kept beside it so a query can filter on it.
        let registered =
            System.Collections.Generic.Dictionary<string, string * FitOutcome>()

        let vintages = ResizeArray<VintageRef>(config.Vintages)
        let mutable spent = 0.0

        let resolveRef (v: VintageRef) : VintageRef option =
            vintages
            |> Seq.tryFind (fun known -> known.DatasetId = v.DatasetId && known.Version = v.Version)

        let latestVersion (datasetId: string) : int option =
            vintages
            |> Seq.filter (fun v -> v.DatasetId = datasetId)
            |> Seq.map (fun v -> v.Version)
            |> Seq.sortDescending
            |> Seq.tryHead

        let resolve (v: VintageRef) : Result<ResolvedVintage, Refusal> =
            match resolveRef v with
            | None -> Error(NotFound("vintage", v.DatasetId + "@" + string v.Version))
            | Some known ->
                Ok
                    { Ref = known
                      CreatedAt = config.Instant
                      IsLatest = latestVersion known.DatasetId = Some known.Version }

        /// Mint the next version of a dataset from an opaque provenance handle. The
        /// handle is hashed, never inspected: what produced the bytes is the executor's
        /// business on a real deployment and is nobody's here.
        let deriveVintage (datasetId: string) (provenance: string) : ResolvedVintage =
            let version = (latestVersion datasetId |> Option.defaultValue 0) + 1

            let derived =
                { DatasetId = datasetId
                  Version = version
                  ContentRef =
                    Some
                        { Format = "parquet"
                          Hash = Digest.sha256 (datasetId + "@" + string version + "|" + provenance)
                          RowCount = Some(1L + int64 (unitOf provenance * 100000.0)) } }

            vintages.Add derived

            { Ref = derived
              CreatedAt = config.Instant
              IsLatest = true }

        /// What this executor reports about one fit. Every number is a function of the
        /// key; nothing is measured, because nothing ran.
        let outcomeOf (pinned: VintageRef) (s: FitSubmission) : FitOutcome =
            let key =
                { SpecHash = s.SpecHash
                  Vintage = pinned
                  Seed = s.Seed
                  ProviderId = config.ProviderId
                  ProviderVersion = config.ProviderVersion }

            let keyHash = Codec.compositeKeyHash key

            let verdicts =
                s.Gates
                |> List.map (fun g -> GateVerdict.evaluate g (diagnostic s.SpecHash g.Name))

            let diagnostics = verdicts |> List.map (fun v -> v.Name, v.Observed) |> Map.ofList

            let passed = verdicts |> List.forall (fun v -> v.Passed)

            { CompositeKeyHash = keyHash
              CompositeKey = key
              // A fit that did not clear its gates retains no artifact, and is
              // registered anyway: a failure is evidence.
              ArtifactRef =
                if passed then
                    Some
                        { ArtifactId = "artifact-" + (Digest.sha256Hex keyHash).Substring(0, 8)
                          ContentHash = Digest.sha256 (keyHash + "|artifact")
                          Format = Some "arrow-ipc" }
                else
                    None
              Diagnostics = diagnostics
              DiagnosticUnits =
                config.DiagnosticUnits
                |> Map.filter (fun name _ -> Map.containsKey name diagnostics)
              GateVerdicts = verdicts
              Status = (if passed then "registered" else "failed")
              Timing =
                { SubmittedAt = config.Instant
                  StartedAt = Some config.Instant
                  CompletedAt = Some config.Instant
                  DurationMs = Some(1L + int64 (unitOf (keyHash + "|duration") * 600000.0)) }
              Cost =
                config.Quota
                |> Option.map (fun _ ->
                    { Unit = "core-seconds"
                      Amount = unitOf (keyHash + "|cost") * 1000.0 })
              Annotations = Map.empty
              RegisteredAt = config.Instant }

        let costOf (o: FitOutcome) =
            o.Cost |> Option.map (fun c -> c.Amount) |> Option.defaultValue 0.0

        { new IModelExecutionBackend with
            member _.ProviderId = config.ProviderId
            member _.ProviderVersion = config.ProviderVersion

            member _.SubmitFits(batch: FitSubmissionBatch) =
                // The batch as a whole is validated first — an empty batch or a
                // malformed submission is refused outright — but a batch that is
                // well-formed is never refused as a whole because one of its items
                // could not be run. That difference is the receipt.
                match Codec.validateBatch batch with
                | Error r -> Error r.Refusal
                | Ok() ->
                    let results =
                        batch.Submissions
                        |> List.mapi (fun index s ->
                            if not (List.contains s.ProviderKind config.ProviderKinds) then
                                Choice2Of2
                                    { Index = index
                                      Reason = UnknownProvider(s.ProviderKind, config.ProviderKinds) }
                            else
                                match resolveRef s.Vintage with
                                | None ->
                                    Choice2Of2
                                        { Index = index
                                          Reason =
                                            NotFound("vintage", s.Vintage.DatasetId + "@" + string s.Vintage.Version) }
                                | Some pinned ->
                                    let outcome = outcomeOf pinned s
                                    let cost = costOf outcome

                                    // §5.3's gate rule: a comparison is never made across
                                    // units. The provider's units are known before scheduling,
                                    // so the gate is refused here, by name, and no verdict
                                    // over the mismatch is ever recorded.
                                    let mismatch =
                                        s.Gates
                                        |> List.tryFind (fun g -> g.Unit <> Map.tryFind g.Name config.DiagnosticUnits)

                                    match mismatch, config.Quota with
                                    | Some g, _ ->
                                        Choice2Of2
                                            { Index = index
                                              Reason =
                                                GateUnitMismatch(
                                                    g.Name,
                                                    g.Unit,
                                                    Map.tryFind g.Name config.DiagnosticUnits
                                                ) }
                                    | None, Some quota when spent + cost > quota ->
                                        Choice2Of2
                                            { Index = index
                                              Reason = BudgetDenied(quota, spent, "core-seconds") }
                                    | None, _ ->
                                        spent <- spent + cost
                                        registered[outcome.CompositeKeyHash] <- (batch.BatchId, outcome)

                                        Choice1Of2
                                            { Index = index
                                              JobId =
                                                "job-" + (Digest.sha256Hex outcome.CompositeKeyHash).Substring(0, 8) })

                    Ok
                        { BatchId = batch.BatchId
                          ItemCount = List.length batch.Submissions
                          Accepted =
                            results
                            |> List.choose (function
                                | Choice1Of2 a -> Some a
                                | _ -> None)
                          Rejected =
                            results
                            |> List.choose (function
                                | Choice2Of2 r -> Some r
                                | _ -> None) }

            member _.GetOutcomes(query: RegistryQuery) =
                match Codec.validateQuery query with
                | Error r -> Error r.Refusal
                | Ok() ->
                    let matches (batchId: string, o: FitOutcome) =
                        (List.isEmpty query.SpecHashes
                         || List.contains o.CompositeKey.SpecHash query.SpecHashes)
                        && (List.isEmpty query.Vintages
                            // A query is matched on `(datasetId, version)` alone — never
                            // narrowed by a content ref the caller happened to carry.
                            || query.Vintages
                               |> List.exists (fun v ->
                                   v.DatasetId = o.CompositeKey.Vintage.DatasetId
                                   && v.Version = o.CompositeKey.Vintage.Version))
                        && (List.isEmpty query.Statuses || List.contains o.Status query.Statuses)
                        && (match query.BatchId with
                            | None -> true
                            | Some b -> b = batchId)

                    registered
                    |> Seq.map (fun kv -> kv.Key, kv.Value)
                    |> Seq.filter (snd >> matches)
                    |> Seq.map (fun (k, (_, o)) -> k, o)
                    |> List.ofSeq
                    |> paginate query.Page.Limit query.Page.Cursor
                    |> Result.map (fun (outcomes, next) ->
                        { Outcomes = outcomes
                          NextCursor = next })

            member _.ResolveVintage(v: VintageRef) =
                match Codec.validateVintage v with
                | Error r -> Error r.Refusal
                | Ok() -> resolve v

            member _.Score(request: ScoreRequest) =
                match Codec.validateScoreRequest request with
                | Error r -> Error r.Refusal
                | Ok() ->
                    match registered.TryGetValue request.ArtifactKeyHash with
                    | false, _ -> Error(NotFound("artifact", request.ArtifactKeyHash))
                    | true, (_, outcome) ->
                        match outcome.ArtifactRef with
                        | None ->
                            Error(
                                ScoreRefused(
                                    "not-approved",
                                    "the outcome retained no artifact — it did not clear its gates"
                                )
                            )
                        | Some _ ->
                            match resolveRef request.Input with
                            | None ->
                                Error(
                                    ScoreRefused(
                                        "input-unavailable",
                                        "no such vintage: "
                                        + request.Input.DatasetId
                                        + "@"
                                        + string request.Input.Version
                                    )
                                )
                            | Some pinned ->
                                Ok(
                                    deriveVintage
                                        request.OutputDatasetId
                                        (request.ArtifactKeyHash + "|" + pinned.DatasetId + "@" + string pinned.Version)
                                )

            member _.QueryArtifacts(query: ArtifactQuery) =
                if query.Page.Limit < Page.minLimit || query.Page.Limit > Page.maxLimit then
                    Error(
                        InvalidQuery(
                            "page.limit must be between "
                            + string Page.minLimit
                            + " and "
                            + string Page.maxLimit
                        )
                    )
                else
                    registered
                    |> Seq.choose (fun kv ->
                        let _, outcome = kv.Value

                        match outcome.ArtifactRef with
                        | Some artifact when
                            List.isEmpty query.ArtifactKeyHashes
                            || List.contains kv.Key query.ArtifactKeyHashes
                            ->
                            Some(
                                kv.Key,
                                { ArtifactKeyHash = kv.Key
                                  Artifact = artifact }
                            )
                        | _ -> None)
                    |> List.ofSeq
                    |> paginate query.Page.Limit query.Page.Cursor
                    |> Result.map (fun (artifacts, next) ->
                        { Artifacts = artifacts
                          NextCursor = next })

            member _.RequestRevintage(request: RevintageRequest) =
                if request.DatasetId = "" || request.AssemblyRef = "" then
                    Error(InvalidSubmission "a re-vintage request names a dataset and an assembly")
                else
                    Ok(deriveVintage request.DatasetId request.AssemblyRef) }
