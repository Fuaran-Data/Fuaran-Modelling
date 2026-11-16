// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  The exploration session — the op vocabulary hosted on a content-addressed,
//  branching op-DAG.
//
//  A linear hash chain is a degenerate DAG with one branch, and a model
//  exploration is exactly the case that needs the other kind. So the history
//  here is a DAG: an op attaches to the node that last touched the state it
//  targets, which means two forks off one state produce two lines, and
//  replaying to a node yields the exploration AS IT STOOD ON THAT LINE — not
//  the union of everything ever tried.
//
//  That is the load-bearing property. Path replay along one line is
//  deterministic without qualification: within a path every node has exactly
//  one parent and one child, so the topological order is forced, and any two
//  hosts replaying the same line reach the same record. It is what lets an
//  abandoned branch stay in the history at no cost to the branch beside it.
// ============================================================================

/// Why a governed op submission did not land.
type SubmitFailure =
    /// The policy gate refused the subject this op kind.
    | PolicyBlocked of PolicyRefusal
    /// The op reached the fold and the fold refused it.
    | FoldRejected of Rejection

/// Why a governed fit submission did not land. Three gates, three distinct answers —
/// collapsing them would lose the only information a caller can act on.
type FitFailure =
    | FitPolicyBlocked of PolicyRefusal
    | FitBackendRefused of BackendRefusal
    | FitFoldRejected of Rejection

/// Why a governed divergence declaration did not land. Three gates again, and the same
/// argument for keeping them apart: "you may not declare", "there is no such invariant" and
/// "there is no such branch" have three different remedies.
type DeclareFailure =
    | DeclarePolicyBlocked of PolicyRefusal
    | DeclareRefused of DeclarationRefusal
    | DeclareFoldRejected of Rejection

/// Why a governed assent did not land. Three gates once more, and the three answers are
/// "you may not record assent (as this party)", "there is no such party class or no reason",
/// and "there is no such branch" — three different remedies, and a caller that could not
/// tell them apart would be left retrying the one thing that cannot work.
type AssentFailure =
    | AssentPolicyBlocked of PolicyRefusal
    | AssentRefused of AssentRefusal
    | AssentFoldRejected of Rejection

/// An exploration session: the op-DAG, the working record folded from it, and the
/// bookkeeping that decides where the next op attaches.
type Session =
    {
        /// The content-addressed history. Provenance — every op, who emitted it, and what
        /// it descended from.
        Dag: Dag.T<ExplorationOp>
        /// `state:<id>` / `proposal:<id>` → the DAG node that last touched it. This is what
        /// makes the history branch in the same shape the exploration does.
        Marks: Map<string, string>
        /// The most recently appended node ("" before the seed).
        Cursor: string
        /// The working view: every op folded in submission order. The union of all lines.
        /// For the view along ONE line, replay to that line's head.
        Record: Exploration
    }

[<RequireQualifiedAccess>]
module Session =

    let private stateKey (id: StateId) : string = "state:" + id
    let private proposalKey (id: ProposalId) : string = "proposal:" + id

    /// The two-seam witness the substrate lifts over: the domain fold plus the domain
    /// codec. Everything the op-DAG does — content addressing, integrity verification,
    /// replay, persistence — is generic over exactly this.
    let witness: StreamWitness<ExplorationOp, Exploration, Rejection> =
        { Apply = Reducer.apply
          Encode = Wire.renderOp
          Decode = Wire.parseOp }

    let empty: Session =
        { Dag = Dag.empty
          Marks = Map.empty
          Cursor = ""
          Record = Exploration.empty }

    /// Where an op attaches. Each op hangs off the node that last touched the state it
    /// concerns, so the history's shape mirrors the exploration's: two forks off one
    /// state fork the DAG, and a curation on a state extends that state's own line.
    let private anchorOf (op: ExplorationOp) (s: Session) : string =
        let mark k =
            Map.tryFind k s.Marks |> Option.defaultValue s.Cursor

        match op with
        | Seed _ -> ""
        | Fork f -> mark (stateKey f.From)
        | ReturnTo r -> mark (stateKey r.To)
        | CherryPick c -> mark (stateKey c.Onto)
        | Curate c -> mark (stateKey c.StateId)
        | Annotate a -> mark (stateKey a.StateId)
        | Declare d -> mark (stateKey d.StateId)
        | Assent a -> mark (stateKey a.StateId)
        // A replay receipt is the one op that concerns MANY branches, so it hangs off the
        // cursor rather than off any of their lines, and claims no state's mark (below).
        // That placement is deliberate and it costs nothing: the receipt is the account of a
        // campaign, and the campaign's EVIDENCE is line-local already — each outcome reached
        // its branch by an ordinary `Annotate`, on that branch's own line, and replays with
        // it. Anchoring the receipt into one participant's line would put a fact about ten
        // branches inside the closure of one, and leave it out of the other nine.
        | Replay _ -> s.Cursor
        | Propose p -> mark (stateKey p.Proposal.From)
        | Resolve r ->
            match Map.tryFind r.ProposalId s.Record.Proposals with
            | Some pr -> mark (proposalKey pr.ProposalId)
            | None -> s.Cursor

    /// Which marks the new node claims. Read against the record as it stood BEFORE the
    /// op, because an accepted proposal names a state that did not exist until now.
    let private marksAfter
        (op: ExplorationOp)
        (nodeId: string)
        (before: Exploration)
        (marks: Map<string, string>)
        : Map<string, string> =
        match op with
        | Seed s -> Map.add (stateKey s.StateId) nodeId marks
        | Fork f -> Map.add (stateKey f.StateId) nodeId marks
        | ReturnTo r -> Map.add (stateKey r.To) nodeId marks
        | CherryPick c -> Map.add (stateKey c.StateId) nodeId marks
        | Curate c -> Map.add (stateKey c.StateId) nodeId marks
        | Annotate a -> Map.add (stateKey a.StateId) nodeId marks
        | Declare d -> Map.add (stateKey d.StateId) nodeId marks
        | Assent a -> Map.add (stateKey a.StateId) nodeId marks
        // Claims nothing — see `anchorOf`. A receipt that took a branch's mark would put the
        // next op on that branch downstream of a campaign it has nothing to do with.
        | Replay _ -> marks
        | Propose p -> Map.add (proposalKey p.Proposal.ProposalId) nodeId marks
        | Resolve r ->
            let marks = Map.add (proposalKey r.ProposalId) nodeId marks

            match r.Outcome, Map.tryFind r.ProposalId before.Proposals with
            | Accepted, Some pr -> Map.add (stateKey pr.ProposedId) nodeId marks
            | _ -> marks

    /// Append one op. The fold runs FIRST: a refused op never reaches the history, so a
    /// stored node is by construction one that applied.
    let submit (hashFn: HashFn) (actor: Actor) (op: ExplorationOp) (s: Session) : Result<string * Session, Rejection> =
        match Reducer.apply op s.Record with
        | Error r -> Error r
        | Ok record ->
            let appended =
                match op with
                // A cherry-pick genuinely JOINS two lines: the target it lands on and the
                // branch the idea came from. Recording it as a merge node — rather than a
                // plain child of the target — is what keeps the source branch inside the
                // resulting line's closure, so replaying that line can still see where the
                // idea came from. Anything else would make the provenance a dangling label.
                | CherryPick c ->
                    let onto = anchorOf op s

                    let source = Map.tryFind (stateKey c.Source) s.Marks |> Option.defaultValue onto

                    Dag.merge (OpStream.canonical hashFn witness) actor op onto source s.Dag
                | _ -> Dag.append (OpStream.canonical hashFn witness) actor op (anchorOf op s) s.Dag

            match appended with
            | Ok written ->
                let nodeId, dag = written.Id, written.Dag

                Ok(
                    nodeId,
                    { Dag = dag
                      Marks = marksAfter op nodeId s.Record s.Marks
                      Cursor = nodeId
                      Record = record }
                )
            // The one graph refusal a well-formed session can meet: the content id is already
            // held by a different node. The node held first stays and this op is refused, where
            // the history used to replace it silently.
            | Error(DagAppendFault.ContentIdCollision id) -> Error(HistoryCollision id)
            // Every other fault names a parent this session minted itself, so it is a defect in
            // this module rather than something a caller can act on.
            | Error fault -> invalidOp ("Session.submit: " + DagAppendFault.toString fault)

    /// Append one op through the policy gate. Default-deny: an unauthorised subject never
    /// reaches the fold, so an unauthorised op leaves no trace at all.
    let submitGoverned
        (hashFn: HashFn)
        (policy: Policy)
        (subject: string)
        (actor: Actor)
        (op: ExplorationOp)
        (s: Session)
        : Result<string * Session, SubmitFailure> =
        match Policy.authoriseOp policy subject op with
        | Error refusal -> Error(PolicyBlocked refusal)
        | Ok() -> submit hashFn actor op s |> Result.mapError FoldRejected

    /// Fold a whole op sequence, ungoverned. Stops at the first rejection with its index.
    let submitAll
        (hashFn: HashFn)
        (actor: Actor)
        (ops: ExplorationOp list)
        (s: Session)
        : Result<Session, int * Rejection> =
        let rec go i st =
            function
            | [] -> Ok st
            | op :: rest ->
                match submit hashFn actor op st with
                | Ok(_, st') -> go (i + 1) st' rest
                | Error r -> Error(i, r)

        go 0 s ops

    /// The DAG node that last touched a state.
    let headOf (id: StateId) (s: Session) : string option = Map.tryFind (stateKey id) s.Marks

    /// The heads of the history — nodes nothing descends from.
    let heads (s: Session) : string list = Dag.heads s.Dag

    /// Replay to a history node: the exploration as it stood on THAT line. Ops on other
    /// lines are not in the node's ancestor closure and so are not applied.
    let replayTo (s: Session) (nodeId: string) : Result<Exploration, string> =
        match Dag.tryReplayTo witness Exploration.empty s.Dag nodeId with
        | Ok e -> Ok e
        | Error(Dag.ReplayFault.UnknownHead head) -> Error("no such node in the history: '" + head + "'")
        | Error(Dag.ReplayFault.Cyclic cyclic) -> Error("cyclic history at node " + String.concat ", " cyclic.Unplaced)
        | Error(Dag.ReplayFault.Rejected(node, rej)) -> Error("node " + node + ": " + Rejection.describe rej)

    /// Replay the line that produced `id` — the path view of one state.
    let replayState (id: StateId) (s: Session) : Result<Exploration, string> =
        match headOf id s with
        | None -> Error("no such state in the history: '" + id + "'")
        | Some node -> replayTo s node

    /// The lineage of `id` in the working record, root first.
    let path (id: StateId) (s: Session) : StateId list = Query.path id s.Record

    /// Is every node's content id the hash of what it holds, and does every parent exist?
    let verify (hashFn: HashFn) (s: Session) : bool =
        Dag.verifyDag (OpStream.canonical hashFn witness) s.Dag

    /// The governed fit path: policy gate → backend → outcome annotation, in that order.
    ///
    /// Three gates, and they are not interchangeable. Process policy decides whether this
    /// subject may spend compute at all; the backend decides whether this particular
    /// submission is admissible and what it produced; the fold decides whether the
    /// resulting outcome may be recorded. Only the last one writes history.
    let submitFit
        (hashFn: HashFn)
        (policy: Policy)
        (subject: string)
        (actor: Actor)
        (backend: IExecutionBackend)
        (submission: FitSubmission)
        (s: Session)
        : Result<OutcomeSnapshot * string * Session, FitFailure> =
        match Policy.authoriseFit policy subject with
        | Error refusal -> Error(FitPolicyBlocked refusal)
        | Ok() ->
            match backend.Submit submission with
            | Error refusal -> Error(FitBackendRefused refusal)
            | Ok outcome ->
                let op =
                    Annotate
                        { StateId = submission.StateId
                          Snapshot = outcome }

                match submit hashFn actor op s with
                | Error r -> Error(FitFoldRejected r)
                | Ok(nodeId, s') -> Ok(outcome, nodeId, s')

    /// Record a replay campaign's receipt, through the policy gate.
    ///
    /// **This does not submit anything and does not annotate anything.** It records that a
    /// campaign happened and what became of each branch it named. The campaign itself runs
    /// through the execution port — `Bridge.replayOnto` in the execution tier — and each
    /// outcome it gathers lands by the ordinary `Annotate` path with the ordinary
    /// duplicate-outcome guard over it.
    ///
    /// There is deliberately NO manager-tier replay loop over the narrow one-fit backend. It
    /// would be a second route from a fit to a decision record, and a second route is always
    /// the one that eventually skips a rule — here the identity rule that holds every key
    /// component but the vintage. One route, through the port, and this is its receipt.
    let recordReplay
        (hashFn: HashFn)
        (policy: Policy)
        (subject: string)
        (actor: Actor)
        (receipt: ReplayReceipt)
        (s: Session)
        : Result<string * Session, SubmitFailure> =
        submitGoverned hashFn policy subject actor (Replay { Receipt = receipt }) s

    /// The governed declaration path: policy gate → invariant vocabulary → fold.
    ///
    /// Same shape and same ordering argument as `submitFit`. Default-deny is the outer gate,
    /// so a subject with no grant leaves no trace at all; the invariant gate is next, because a
    /// declaration against an id no comparator declares would fold perfectly and protect
    /// nothing; the fold is last, and is the only one that writes history.
    ///
    /// The comparator set is passed in rather than looked up. The manager holds no dialect and
    /// resolves none — a caller that has a dialect reaches its comparators through the seam and
    /// hands them here, which keeps this function as spec-agnostic as everything around it.
    let submitDeclaration
        (hashFn: HashFn)
        (policy: Policy)
        (subject: string)
        (actor: Actor)
        (comparators: InvariantComparator list)
        (state: StateId)
        (declaration: DivergenceDeclaration)
        (s: Session)
        : Result<string * Session, DeclareFailure> =
        match Policy.authorise policy subject (MayEmit DeclareKind) with
        | Error refusal -> Error(DeclarePolicyBlocked refusal)
        | Ok() ->
            match Forest.declareDivergence comparators state declaration with
            | Error refusal -> Error(DeclareRefused refusal)
            | Ok op -> submit hashFn actor op s |> Result.mapError DeclareFoldRejected

    /// The governed assent path: may-record gate → party vocabulary → may-speak-as gate →
    /// fold.
    ///
    /// Four checks, three of which can refuse, and the ORDER is the design. The op-kind gate
    /// is outermost, as everywhere else, so a subject with no assent grant at all leaves no
    /// trace. The party-class check comes next — ahead of the per-class grant — because a
    /// misspelled class would otherwise be refused as an ungranted one, and "default-deny"
    /// tells a caller nothing about a name it got wrong while "no policy declares 'reviewer';
    /// declared: modeller, client, third-party-reviewer" tells it everything. The per-class
    /// grant is last of the gates, and the fold is the only thing that writes history.
    ///
    /// The declared classes are read from the policy that is governing the act, rather than
    /// passed separately. That is the whole point of declaring them as policy data: the set a
    /// record is checked against and the set a consensus question is quantified over are the
    /// same set, and there is no call site at which they could drift apart.
    let submitAssent
        (hashFn: HashFn)
        (policy: Policy)
        (subject: string)
        (actor: Actor)
        (state: StateId)
        (assent: AssentRecord)
        (s: Session)
        : Result<string * Session, AssentFailure> =
        match Policy.authorise policy subject (MayEmit AssentKind) with
        | Error refusal -> Error(AssentPolicyBlocked refusal)
        | Ok() ->
            match PartyAssent.record (Policy.parties policy) state assent with
            | Error refusal -> Error(AssentRefused refusal)
            | Ok op ->
                match Policy.authoriseAssent policy subject assent.Party with
                | Error refusal -> Error(AssentPolicyBlocked refusal)
                | Ok() -> submit hashFn actor op s |> Result.mapError AssentFoldRejected

    // ---- persistence ---------------------------------------------------------

    /// The history as JSONL — one line per node, id-sorted, the op embedded as its own
    /// canonical bytes.
    let toJsonl (s: Session) : string = Dag.toJsonl Wire.renderOp s.Dag

    /// Where an op sits in the fold's precedence when the history leaves two nodes
    /// GENUINELY CONCURRENT — neither an ancestor of the other. This decides nothing
    /// causal: the topological sort below emits a node only after every one of its DAG
    /// parents, so a rank can never reorder a dependent pair. It only picks which of
    /// several simultaneously-eligible nodes folds next, and a content-hash tie-break in
    /// that position is what made a stored history replay on luck (D34).
    ///
    /// Two rules, and each answers one of the two ways the DAG's edges under-describe the
    /// domain's dependencies:
    ///
    ///   0. **Creation dominates.** Every op that demands a LIVE state (`Fork`'s `From`,
    ///      `CherryPick`'s `Onto`, `Propose`'s / `Resolve`'s `From`) also creates one, and
    ///      `Curate` is the only op that can take liveness away. A creator left behind a
    ///      concurrent kill is therefore the whole of the first failure face — the kill
    ///      folds first and the fork it must precede is refused as `KilledState`.
    ///   1. State ops (`Curate` / `Annotate` / `Declare` / `Assent` / `ReturnTo`): each already
    ///      claims its state's mark, so two of them on one state are never concurrent.
    ///   2. **A receipt observes, so it folds last.** `Replay` is the one op deliberately
    ///      anchored on the cursor and claiming no mark (see `anchorOf`), so the branches
    ///      its `Requested` names are NOT its DAG ancestors — the second failure face,
    ///      `UnknownState`. Deferring it costs nothing: no other op reads `e.Replays`.
    let private foldRank (op: ExplorationOp) : int =
        match op with
        | Seed _
        | Fork _
        | CherryPick _
        | Propose _
        | Resolve _ -> 0
        | ReturnTo _
        | Curate _
        | Annotate _
        | Declare _
        | Assent _ -> 1
        | Replay _ -> 2

    /// The deterministic global order over a whole DAG: one topological sort of ALL nodes,
    /// draining the eligible frontier by `(foldRank, id)`. A pure function of the DAG, so
    /// every host derives the same sequence from the same bytes.
    ///
    /// The order is total and every node follows its parents, so it is a linear extension
    /// of the history's own causal order — the property the previous head-major
    /// concatenation also had, and which was never the problem. What it adds is that the
    /// remaining freedom is spent on the domain's precedence rather than on a content hash.
    ///
    /// Shorter than the DAG ⇒ a cycle left nodes unreachable; `ofJsonl` reports that rather
    /// than folding the truncated prefix.
    let private globalOrder (dag: Dag.T<ExplorationOp>) : DagNode<ExplorationOp> list =
        let nodes = dag.Nodes

        let parentsIn (n: DagNode<ExplorationOp>) =
            n.Parents |> List.filter (fun p -> Map.containsKey p nodes)

        let children =
            nodes
            |> Map.toList
            |> List.collect (fun (id, n) -> parentsIn n |> List.map (fun p -> p, id))
            |> List.groupBy fst
            |> List.map (fun (p, ps) -> p, List.map snd ps)
            |> Map.ofList

        let rank (id: string) = foldRank (Map.find id nodes).Op, id

        let mutable indeg =
            nodes
            |> Map.toList
            |> List.map (fun (id, n) -> id, List.length (parentsIn n))
            |> Map.ofList

        let mutable ready =
            nodes
            |> Map.toList
            |> List.map fst
            |> List.filter (fun id -> indeg[id] = 0)
            |> List.sortBy rank

        let result = ResizeArray<DagNode<ExplorationOp>>()

        while not (List.isEmpty ready) do
            let id = List.head ready
            ready <- List.tail ready
            result.Add(Map.find id nodes)

            match Map.tryFind id children with
            | Some kids ->
                for k in kids do
                    indeg <- Map.add k (indeg[k] - 1) indeg

                    if indeg[k] = 0 then
                        ready <- (k :: ready) |> List.sortBy rank
            | None -> ()

        List.ofSeq result

    /// Rebuild a session from JSONL, verifying every content id and parent link on the
    /// way in. A tampered id or a dangling parent is a named error, never a silently
    /// corrupt load.
    ///
    /// The rebuilt `Record` is folded in the canonical global order above — deterministic
    /// across hosts, and not necessarily the order a live session happened to submit in.
    /// Where exactness matters, compare PATH replays: within one line the order is forced,
    /// so `replayState` agrees between a live session and a reloaded one, always.
    let ofJsonl (hashFn: HashFn) (text: string) : Result<Session, string> =
        Dag.fromJsonlVerified (OpStream.canonical hashFn witness) text
        |> Result.mapError Dag.loadFaultToString
        |> Result.bind (fun dag ->
            let ordered = globalOrder dag

            if List.length ordered <> Map.count dag.Nodes then
                Error(
                    "Session.ofJsonl: cyclic history — "
                    + string (Map.count dag.Nodes - List.length ordered)
                    + " node(s) unreachable"
                )
            else
                let folded =
                    ordered
                    |> List.fold
                        (fun acc (node: DagNode<ExplorationOp>) ->
                            match acc with
                            | Error e -> Error e
                            | Ok(marks, record) ->
                                match Reducer.apply node.Op record with
                                | Error r -> Error("node " + node.Id + ": " + Rejection.describe r)
                                | Ok record' -> Ok(marksAfter node.Op node.Id record marks, record'))
                        (Ok(Map.empty, Exploration.empty))

                folded
                |> Result.map (fun (marks, record) ->
                    { Dag = dag
                      Marks = marks
                      Cursor =
                        (match List.rev ordered with
                         | last :: _ -> last.Id
                         | [] -> "")
                      Record = record }))
