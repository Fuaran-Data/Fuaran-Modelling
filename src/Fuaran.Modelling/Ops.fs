// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

// ============================================================================
//  The exploration op vocabulary — the closed set of moves that can change a
//  decision record. Model building is a branching exploratory search: from a
//  state you fork, each branch is informative even when abandoned, you return
//  to an earlier state and fork again, and a good idea found on one branch is
//  re-landed on another. Those four moves, plus curation and outcome
//  annotation, are the whole vocabulary.
//
//  It is closed on purpose. An exhaustive match over these cases is how a
//  policy table, a wire codec and a projection all stay total.
// ============================================================================

/// Start an exploration at its first state.
type SeedOp =
    { StateId: StateId
      Label: string
      Payload: SpecPayload }

/// Fork a new state from an existing one. The parent is untouched — forking is
/// additive, always.
type ForkOp =
    { StateId: StateId
      From: StateId
      Label: string
      Payload: SpecPayload }

/// Move the working head back to an ancestor (or to any existing state). Records the
/// intent to resume from there; destroys nothing.
type ReturnOp = { To: StateId }

/// Re-land `Source`'s payload as a new child of `Onto`. The manager copies the payload
/// verbatim — deciding what "the price control from that branch" means inside a payload
/// is dialect knowledge, so the caller supplies the rendered result.
type CherryPickOp =
    { StateId: StateId
      Source: StateId
      Onto: StateId
      Label: string
      Payload: SpecPayload }

/// Record a curation verdict — keep, kill, or note — with its reason.
type CurateOp =
    { StateId: StateId
      Record: CurationRecord }

/// Annotate a state with what an execution backend reported.
type AnnotateOp =
    { StateId: StateId
      Snapshot: OutcomeSnapshot }

/// Record a justified departure from a cross-sibling invariant, on the branch that departs.
///
/// It attaches to a STATE rather than to a forest, and that placement is the whole design.
/// A declaration recorded forest-side would be a cross-sibling op — a second place a
/// sibling's record can be changed from, and the one place per-sibling independence would
/// leak. Recorded here it is an ordinary op on one exploration: it hangs off that state's
/// own line, replays with it, and is attributable and unrewritable like every other op.
///
/// There is deliberately NO op that names two siblings. Coherence across a forest lives at
/// the process layer — one policy applied independently — and every act that changes a
/// record is an act within one sibling.
type DeclareOp =
    { StateId: StateId
      Declaration: DivergenceDeclaration }

/// Record one party class's position on a spec state: an endorsement, a challenge, or a
/// withdrawal, each with its reason.
///
/// The SECOND quality axis, recorded in the same op-DAG as the first. Statistical evidence
/// reaches a state through `Annotate`; whether the people who have to live with the state
/// stand behind it reaches it through here, and neither is derivable from the other. What
/// makes the pair worth having is precisely that they can DISAGREE — gates that hold under a
/// standing challenge, gates that fail under full endorsement — and a record that carried
/// only the first would leave the disagreement in somebody's head.
///
/// It attaches to a STATE, like a curation and like a declaration, and for the same reason: a
/// position recorded anywhere else would be a second place a branch can be spoken for from,
/// and this one replays with the branch it is about.
///
/// A challenge downgrades nothing, blocks nothing and deletes nothing. It is recorded,
/// justified and priced, exactly as a divergence declaration is — the mechanism's job is to
/// make an objection cheap to record and impossible to record invisibly, not to stop work.
type AssentOp =
    { StateId: StateId
      Record: AssentRecord }

/// Record that a replay campaign happened: which vintage, which branches, what landed,
/// what was refused, by whom and when.
///
/// A replay is an ACT and is recorded as one. The evidence it gathers is not recorded here:
/// each landed outcome reached its branch through the ordinary `Annotate` op, so there is
/// still exactly one route by which an outcome enters a decision record, and this op is the
/// receipt for the campaign that drove it. An op that carried the snapshots itself would be
/// that second route, and the second route is always the one that skips a rule — here the
/// duplicate-outcome guard, which is the only thing standing between a replay run twice and
/// a doubled evidence base.
///
/// It names one exploration, like every other op. A replay spans BRANCHES, which is ordinary;
/// nothing here spans siblings, which is the line that matters.
type ReplayOp = { Receipt: ReplayReceipt }

/// Table a branch proposal for review.
type ProposeOp = { Proposal: BranchProposal }

/// Resolve a tabled proposal. `Accepted` materialises the proposed state.
type ResolveOp =
    { ProposalId: ProposalId
      Outcome: ProposalOutcome
      ResolvedBy: string }

/// The closed exploration op vocabulary.
type ExplorationOp =
    | Seed of SeedOp
    | Fork of ForkOp
    | ReturnTo of ReturnOp
    | CherryPick of CherryPickOp
    | Curate of CurateOp
    | Annotate of AnnotateOp
    | Declare of DeclareOp
    | Assent of AssentOp
    | Replay of ReplayOp
    | Propose of ProposeOp
    | Resolve of ResolveOp

/// The op vocabulary as a flat enumeration — the axis a policy table, a coverage gate
/// and an introspection surface all quantify over.
type OpKind =
    | SeedKind
    | ForkKind
    | ReturnKind
    | CherryPickKind
    | CurateKind
    | AnnotateKind
    | DeclareKind
    | AssentKind
    | ReplayKind
    | ProposeKind
    | ResolveKind

// `Rejection` used to live here, beside the vocabulary whose refusals it enumerates. It
// moved to `Types.fs` when a replay receipt began carrying one per branch that did not
// land: the receipt is part of the folded record, so its refusal vocabulary has to be
// declared before the record is. Same name, same namespace, same cases plus two.

[<RequireQualifiedAccess>]
module OpKind =

    /// Every kind. Exhaustive by construction — the conformance suite pins that this
    /// list and the `ExplorationOp` cases stay in step.
    let all =
        [ SeedKind
          ForkKind
          ReturnKind
          CherryPickKind
          CurateKind
          AnnotateKind
          DeclareKind
          AssentKind
          ReplayKind
          ProposeKind
          ResolveKind ]

    let encode (k: OpKind) : string =
        match k with
        | SeedKind -> "seed"
        | ForkKind -> "fork"
        | ReturnKind -> "return"
        | CherryPickKind -> "cherryPick"
        | CurateKind -> "curate"
        | AnnotateKind -> "annotate"
        | DeclareKind -> "declare"
        | AssentKind -> "assent"
        | ReplayKind -> "replay"
        | ProposeKind -> "propose"
        | ResolveKind -> "resolve"

    let tryParse (s: string) : Result<OpKind, string> =
        match all |> List.tryFind (fun k -> encode k = s) with
        | Some k -> Ok k
        | None ->
            Error(
                "unknown op kind '"
                + s
                + "' (expected one of "
                + (all |> List.map encode |> String.concat " | ")
                + ")"
            )

[<RequireQualifiedAccess>]
module Op =

    let kind (op: ExplorationOp) : OpKind =
        match op with
        | Seed _ -> SeedKind
        | Fork _ -> ForkKind
        | ReturnTo _ -> ReturnKind
        | CherryPick _ -> CherryPickKind
        | Curate _ -> CurateKind
        | Annotate _ -> AnnotateKind
        | Declare _ -> DeclareKind
        | Assent _ -> AssentKind
        | Replay _ -> ReplayKind
        | Propose _ -> ProposeKind
        | Resolve _ -> ResolveKind

    let kindName (op: ExplorationOp) : string = kind op |> OpKind.encode
