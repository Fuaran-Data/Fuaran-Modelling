// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

// ============================================================================
//  The fold: one exploration op applied to one decision record.
//
//  Total and pure — every refusal is a typed `Rejection`, nothing throws, and no
//  clock, id generator or ambient state is read. That is what makes replay
//  deterministic: two hosts folding the same ops from the same start reach the
//  same record, byte for byte.
// ============================================================================

/// Derived reads over a folded exploration. Nothing here is stored — a stored
/// derivation is a second truth waiting to drift.
[<RequireQualifiedAccess>]
module Query =

    let tryState (id: StateId) (e: Exploration) : SpecState option = Map.tryFind id e.States

    /// States in the order they entered the exploration.
    let states (e: Exploration) : SpecState list =
        e.Order |> List.choose (fun id -> Map.tryFind id e.States)

    /// The current verdict on a state — the most recent curation, if any. Curation is a
    /// LOG, not a field: a state that was killed and later revived carries both acts, and
    /// the sequence is the interesting part.
    let currentVerdict (s: SpecState) : CurationVerdict option =
        match List.rev s.Curations with
        | latest :: _ -> Some latest.Verdict
        | [] -> None

    /// Is this state killed? An uncurated state is live — silence is not abandonment.
    let isKilled (s: SpecState) : bool = currentVerdict s = Some Kill

    /// The live declaration per invariant — the most recent one for each id.
    ///
    /// Declaration is a LOG (D3's argument, applied again): a branch whose justification was
    /// restated carries both acts, and which one is current is a cheap derivation. Ordered by
    /// invariant id so two hosts reading one record enumerate it identically.
    let currentDeclarations (s: SpecState) : DivergenceDeclaration list =
        s.Divergences
        |> List.fold (fun acc d -> Map.add d.InvariantId d acc) Map.empty
        |> Map.toList
        |> List.map snd

    /// Has this state declared a divergence from `invariantId`?
    let hasDeclared (invariantId: string) (s: SpecState) : bool =
        s.Divergences |> List.exists (fun d -> d.InvariantId = invariantId)

    /// The live declaration for one invariant on one state, if there is one.
    let tryDeclaration (invariantId: string) (s: SpecState) : DivergenceDeclaration option =
        currentDeclarations s |> List.tryFind (fun d -> d.InvariantId = invariantId)

    /// Where each party that has spoken currently stands — the most recent record from each
    /// party class, ordered by party id so two hosts reading one record enumerate it
    /// identically.
    ///
    /// Assent is a LOG (D3's argument, applied a third time), so the live position is a cheap
    /// derivation off it and the sequence that produced it is not recoverable once discarded.
    /// A party that endorsed, challenged and then stood down appears here once, standing down,
    /// with the other two acts still on the record above.
    let currentAssents (s: SpecState) : AssentRecord list =
        s.Assents
        |> List.fold (fun acc a -> Map.add a.Party a acc) Map.empty
        |> Map.toList
        |> List.map snd

    /// Where one party currently stands on one state. `None` means it has said nothing —
    /// which is neither an endorsement nor a challenge, and must not be read as either.
    let tryAssent (party: PartyClassId) (s: SpecState) : AssentRecord option =
        currentAssents s |> List.tryFind (fun a -> a.Party = party)

    /// Parties whose live position on this state is a challenge.
    let challengedBy (s: SpecState) : PartyClassId list =
        currentAssents s
        |> List.filter (fun a -> a.Verdict = Challenge)
        |> List.map (fun a -> a.Party)

    /// The lineage from the root down to `id`, inclusive. Empty when `id` is unknown.
    /// Total on a well-founded record; the visited set makes it total unconditionally.
    let path (id: StateId) (e: Exploration) : StateId list =
        let rec walk (seen: Set<StateId>) (acc: StateId list) (cur: StateId) =
            if Set.contains cur seen then
                acc
            else
                match Map.tryFind cur e.States with
                | None -> acc
                | Some s ->
                    let acc = cur :: acc

                    match s.Parent with
                    | Some p -> walk (Set.add cur seen) acc p
                    | None -> acc

        match Map.tryFind id e.States with
        | None -> []
        | Some _ -> walk Set.empty [] id

    /// The direct children of `id`, in exploration order.
    let children (id: StateId) (e: Exploration) : StateId list =
        states e
        |> List.filter (fun s -> s.Parent = Some id)
        |> List.map (fun s -> s.Id)

    /// States with no children — the frontier of the exploration.
    let leaves (e: Exploration) : StateId list =
        let parents = states e |> List.choose (fun s -> s.Parent) |> Set.ofList
        e.Order |> List.filter (fun id -> not (Set.contains id parents))

    /// States whose current verdict is `Keep` — what a curator settled on, in exploration
    /// order. Note the asymmetry with `isKilled`: silence is not abandonment, but silence is
    /// not endorsement either, so an uncurated state is neither kept nor killed.
    let keptStates (e: Exploration) : StateId list =
        states e
        |> List.filter (fun s -> currentVerdict s = Some Keep)
        |> List.map (fun s -> s.Id)

    /// States carrying an explicit `Kill`.
    let killedStates (e: Exploration) : StateId list =
        states e |> List.filter isKilled |> List.map (fun s -> s.Id)

    /// States nobody has curated either way.
    let uncuratedStates (e: Exploration) : StateId list =
        states e
        |> List.filter (fun s -> (currentVerdict s).IsNone)
        |> List.map (fun s -> s.Id)

    /// Every outcome recorded anywhere in the exploration, paired with its state, in
    /// exploration order. The self-contained evidence base, read offline.
    let outcomes (e: Exploration) : (StateId * OutcomeSnapshot) list =
        states e |> List.collect (fun s -> s.Outcomes |> List.map (fun o -> s.Id, o))

    /// Every outcome one state carries on ONE dataset vintage, in the order they were
    /// recorded.
    ///
    /// A branch can carry more than one fit on one vintage — a different seed, a different
    /// provider, a re-fit after a fix — and all of them are evidence, so this returns the list
    /// rather than picking. WHICH of them a question reads is that question's business and is
    /// stated where the question is asked: a comparison "on this vintage" takes the most
    /// recent, because where a branch stands now is what it is asking, and a reader who wanted
    /// the whole series would be asking the vintage axis's other question instead.
    let outcomesOn (datasetVersion: string) (s: SpecState) : OutcomeSnapshot list =
        s.Outcomes |> List.filter (fun o -> o.Key.DatasetVersion = datasetVersion)

    /// States carrying at least one outcome — the branches that have actually been fitted,
    /// in exploration order.
    ///
    /// Deliberately blind to curation: a killed branch that was fitted is fitted, and it is
    /// precisely the branch a replay exists to re-test. Filtering by verdict here would be
    /// the quiet way to lose the counterfactuals the whole design keeps.
    let fittedStates (e: Exploration) : StateId list =
        states e
        |> List.filter (fun s -> not (List.isEmpty s.Outcomes))
        |> List.map (fun s -> s.Id)

    /// Every replay campaign recorded on this exploration, in the order they were recorded.
    let replays (e: Exploration) : ReplayReceipt list = e.Replays

    /// Proposals that have been tabled and not yet resolved.
    let openProposals (e: Exploration) : BranchProposal list =
        e.Proposals
        |> Map.toList
        |> List.filter (fun (id, _) -> not (Map.containsKey id e.Resolutions))
        |> List.map snd

/// The op fold.
[<RequireQualifiedAccess>]
module Reducer =

    let private addState (s: SpecState) (e: Exploration) : Exploration =
        { e with
            States = Map.add s.Id s e.States
            Order = e.Order @ [ s.Id ]
            Head = Some s.Id }

    /// The state must exist AND be extendable: a killed branch is a decision, and
    /// silently growing one would erase it.
    let private requireLive (id: StateId) (e: Exploration) : Result<SpecState, Rejection> =
        match Map.tryFind id e.States with
        | None -> Error(UnknownState id)
        | Some s when Query.isKilled s -> Error(KilledState id)
        | Some s -> Ok s

    let private requireFresh (id: StateId) (e: Exploration) : Result<unit, Rejection> =
        if Map.containsKey id e.States then
            Error(DuplicateState id)
        else
            Ok()

    let private requireSeeded (e: Exploration) : Result<unit, Rejection> =
        match e.Root with
        | Some _ -> Ok()
        | None -> Error NotSeeded

    /// Apply one op. Total: every failure is a typed `Rejection`.
    let apply (op: ExplorationOp) (e: Exploration) : Result<Exploration, Rejection> =
        match op with
        | Seed s ->
            match e.Root with
            | Some existing -> Error(AlreadySeeded existing)
            | None ->
                let st =
                    { Id = s.StateId
                      Label = s.Label
                      Parent = None
                      Origin = Seeded
                      Payload = s.Payload
                      Curations = []
                      Outcomes = []
                      Divergences = []
                      Assents = [] }

                Ok
                    { addState st e with
                        Root = Some s.StateId }

        | Fork f ->
            requireSeeded e
            |> Result.bind (fun () -> requireFresh f.StateId e)
            |> Result.bind (fun () -> requireLive f.From e)
            |> Result.map (fun _ ->
                let st =
                    { Id = f.StateId
                      Label = f.Label
                      Parent = Some f.From
                      Origin = ForkedFrom f.From
                      Payload = f.Payload
                      Curations = []
                      Outcomes = []
                      Divergences = []
                      Assents = [] }

                addState st e)

        | ReturnTo r ->
            requireSeeded e
            |> Result.bind (fun () ->
                if Map.containsKey r.To e.States then
                    Ok { e with Head = Some r.To }
                else
                    Error(UnknownState r.To))

        | CherryPick c ->
            requireSeeded e
            |> Result.bind (fun () -> requireFresh c.StateId e)
            |> Result.bind (fun () ->
                match Map.tryFind c.Source e.States with
                | None -> Error(UnknownState c.Source)
                | Some _ -> Ok())
            |> Result.bind (fun () -> requireLive c.Onto e)
            |> Result.map (fun _ ->
                let st =
                    { Id = c.StateId
                      Label = c.Label
                      Parent = Some c.Onto
                      Origin = CherryPickedFrom(c.Source, c.Onto)
                      Payload = c.Payload
                      Curations = []
                      Outcomes = []
                      Divergences = []
                      Assents = [] }

                addState st e)

        | Curate c ->
            match Map.tryFind c.StateId e.States with
            | None -> Error(UnknownState c.StateId)
            | Some st ->
                let st' =
                    { st with
                        Curations = st.Curations @ [ c.Record ] }

                Ok
                    { e with
                        States = Map.add st.Id st' e.States }

        | Annotate a ->
            match Map.tryFind a.StateId e.States with
            | None -> Error(UnknownState a.StateId)
            | Some st ->
                if st.Outcomes |> List.exists (fun o -> o.Key = a.Snapshot.Key) then
                    Error(DuplicateOutcome(a.StateId, a.Snapshot.Key))
                else
                    let st' =
                        { st with
                            Outcomes = st.Outcomes @ [ a.Snapshot ] }

                    Ok
                        { e with
                            States = Map.add st.Id st' e.States }

        // A declaration is appended, never replaced, and no duplicate guard stands over it.
        // Re-declaring one invariant on one branch is a legitimate act — the argument was
        // restated, or restated differently — and the sequence is the interesting part, so
        // this follows `Curate` rather than `Annotate`. `Query.currentDeclarations` derives
        // the live position off the log.
        //
        // The fold does NOT check the invariant id. It cannot: the invariant vocabulary is
        // declared by a dialect and is exactly as opaque here as a spec payload's interior.
        // That gate lives one layer out, in `Forest.declareDivergence`, which has the
        // comparator set to check against — gate before fold, as everywhere else.
        | Declare d ->
            match Map.tryFind d.StateId e.States with
            | None -> Error(UnknownState d.StateId)
            | Some st ->
                let st' =
                    { st with
                        Divergences = st.Divergences @ [ d.Declaration ] }

                Ok
                    { e with
                        States = Map.add st.Id st' e.States }

        // An assent is appended, never replaced, and no duplicate guard stands over it —
        // this follows `Curate` and `Declare` rather than `Annotate`. A party that endorses,
        // later challenges, and later stands down has said three things, and every one of
        // them is a legitimate act; `Query.currentAssents` derives the live position off the
        // log. Nothing here removes a record, including the record a withdrawal withdraws:
        // "withdrawn" is a position a party now holds, not a hole where one used to be.
        //
        // The fold does NOT check the party class, for exactly the reason it does not check
        // an invariant id: the declared set is policy data the fold cannot see. That gate
        // lives one layer out, in `PartyAssent.record`, which is handed the declared classes
        // — gate before fold, as everywhere else.
        //
        // A challenge is recordable on any state that exists, killed or live. An objection to
        // a branch somebody already abandoned is still worth having on the record, and
        // refusing it would be the one shape in which a challenge could be silenced by acting
        // first.
        | Assent a ->
            match Map.tryFind a.StateId e.States with
            | None -> Error(UnknownState a.StateId)
            | Some st ->
                let st' =
                    { st with
                        Assents = st.Assents @ [ a.Record ] }

                Ok
                    { e with
                        States = Map.add st.Id st' e.States }

        // A replay receipt is the record of an act, so the fold checks the act's account
        // rather than its evidence. Three things, and the third is the one worth having:
        // the exploration must be seeded, every branch it names must exist, and the receipt
        // must ACCOUNT for every branch it says it requested. A receipt that lost a branch
        // between requesting and answering is the failure the execution port's own receipt
        // is shaped to prevent — a submission the submitter believes was taken and nothing
        // has a record of — and here, one layer up, the fold can simply refuse it.
        //
        // Nothing about the outcomes is checked, because none of them are here. They
        // arrived by `Annotate`, each through the duplicate-outcome guard, which is the
        // whole reason a replay does not carry its own evidence.
        | Replay r ->
            let receipt = r.Receipt

            let unknown =
                receipt.Requested |> List.tryFind (fun id -> not (Map.containsKey id e.States))

            requireSeeded e
            |> Result.bind (fun () ->
                if e.Replays |> List.exists (fun x -> x.ReplayId = receipt.ReplayId) then
                    Error(DuplicateReplay receipt.ReplayId)
                else
                    Ok())
            |> Result.bind (fun () ->
                match unknown with
                | Some id -> Error(UnknownState id)
                | None -> Ok())
            |> Result.bind (fun () ->
                if ReplayReceipt.isTotal receipt then
                    Ok()
                else
                    Error(IncompleteReplay receipt.ReplayId))
            |> Result.map (fun () ->
                { e with
                    Replays = e.Replays @ [ receipt ] })

        | Propose p ->
            let pr = p.Proposal

            if Map.containsKey pr.ProposalId e.Proposals then
                Error(ProposalAlreadyTabled pr.ProposalId)
            else
                requireSeeded e
                |> Result.bind (fun () -> requireFresh pr.ProposedId e)
                |> Result.bind (fun () -> requireLive pr.From e)
                |> Result.map (fun _ ->
                    { e with
                        Proposals = Map.add pr.ProposalId pr e.Proposals })

        | Resolve r ->
            match Map.tryFind r.ProposalId e.Proposals with
            | None -> Error(UnknownProposal r.ProposalId)
            | Some pr ->
                if Map.containsKey r.ProposalId e.Resolutions then
                    Error(ProposalAlreadyResolved r.ProposalId)
                else
                    let recorded =
                        { e with
                            Resolutions = Map.add r.ProposalId r.Outcome e.Resolutions }

                    match r.Outcome with
                    | Declined _ -> Ok recorded
                    | Accepted ->
                        requireFresh pr.ProposedId recorded
                        |> Result.bind (fun () -> requireLive pr.From recorded)
                        |> Result.map (fun _ ->
                            let st =
                                { Id = pr.ProposedId
                                  Label = pr.Label
                                  Parent = Some pr.From
                                  Origin = FromProposal(pr.ProposalId, pr.From)
                                  Payload = pr.Payload
                                  Curations = []
                                  Outcomes = []
                                  Divergences = []
                                  Assents = [] }

                            addState st recorded)

    /// Fold a whole op sequence. Stops at the first rejection, naming its 0-based index —
    /// a partial fold with no position is not a diagnosis.
    let applyAll (ops: ExplorationOp list) (e: Exploration) : Result<Exploration, int * Rejection> =
        let rec go i st =
            function
            | [] -> Ok st
            | op :: rest ->
                match apply op st with
                | Ok st' -> go (i + 1) st' rest
                | Error r -> Error(i, r)

        go 0 e ops
