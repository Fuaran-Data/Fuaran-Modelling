// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  Fuaran.Modelling — the spec-agnostic model MANAGER.
//
//  The manager owns the DECISION record: which spec states were explored, how
//  they descend from one another, what a human or agent decided about each, and
//  what an execution backend reported back. It owns none of the model: the spec
//  payload is OPAQUE here, start to finish. Nothing in this library parses a
//  payload, validates a parameter, or knows what any of it means — that is a
//  dialect's job, behind the dialect seam.
//
//  Everything below is FSharp.Core plus the Fuaran.Core substrate: no estimator,
//  no numerical library, no framework, no host.
// ============================================================================

/// The stable, author-minted identity of one spec state in an exploration.
///
/// Ids are PERMANENT: a state survives relabelling, re-specification and re-fitting,
/// and every curation record, outcome snapshot and descendant edge keys on the id
/// rather than on a position. Minting is the author's job — the manager never
/// invents one, so two hosts replaying the same op stream name the same states.
type StateId = string

/// The identity of a branch proposal — an exploration edit held unapplied until it is
/// resolved (the ops-about-ops shape).
type ProposalId = string

/// The identity of one replay campaign — one re-submission of a branch set onto one new
/// dataset vintage.
///
/// Minted by the author, like every other id here, and permanent for the same reason: the
/// receipt a campaign leaves is the only record that it happened, and a reused id would
/// silently overwrite the account of an earlier one.
type ReplayId = string

/// A rendered spec, opaque to the manager.
///
/// `Body` is whatever a dialect's projector emitted (the framework configuration that
/// crosses the execution wire); the manager stores it, hashes-by-reference through
/// `SpecHash`, and never looks inside. `Dialect` and `Format` are open registered
/// strings — the manager does not enumerate the dialects that may exist, which is the
/// whole point of the seam.
///
/// `SpecHash` is minted by the SUBMITTER over the canonical bytes of `Body`, under the
/// canonical Wire JSON discipline, and is one component of the composite key that joins
/// this decision record to an execution record. The manager treats it as given: it
/// carries the hash, it does not re-derive it, because re-deriving it would require
/// knowing the payload's canonical form — dialect knowledge, by definition.
type SpecPayload =
    { Dialect: string
      Format: string
      Body: string
      SpecHash: string }

/// Who submitted a fit. On the wire from v1 so process policy can gate automated
/// exploration harder than interactive use — retrofitting this distinction later would
/// be a breaking change to every recorded outcome.
type SubmitterClass =
    | SubmitHuman
    | SubmitScheduled
    | SubmitAgent

/// Companion for `SubmitterClass` — the pinned wire spellings.
[<RequireQualifiedAccess>]
module SubmitterClass =

    let encode (s: SubmitterClass) : string =
        match s with
        | SubmitHuman -> "human"
        | SubmitScheduled -> "scheduled"
        | SubmitAgent -> "agent"

    let tryParse (s: string) : Result<SubmitterClass, string> =
        match s with
        | "human" -> Ok SubmitHuman
        | "scheduled" -> Ok SubmitScheduled
        | "agent" -> Ok SubmitAgent
        | other -> Error("unknown submitter class '" + other + "' (expected human | scheduled | agent)")

    let all = [ SubmitHuman; SubmitScheduled; SubmitAgent ]

/// Which side of the threshold a gate passes on.
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
        | other -> Error("unknown gate direction '" + other + "' (expected atLeast | atMost)")

/// A diagnostic's unit as TEXT — the one place a unit enters or leaves as a string (D39).
///
/// A unit is valid only as the canonical text of an admitted unit: `Unit.parse` reads it and
/// `Unit.render` writes it back unchanged. A parseable spelling that is not canonical (`s-1` for
/// `/s`, `m.kg` for `kg.m`) is refused rather than normalised, because two texts for one unit
/// would give one record two encodings.
[<RequireQualifiedAccess>]
module UnitText =

    /// The unit this text is the canonical text of, or why it is not one.
    let tryParse (text: string) : Result<UnitOfMeasure, string> =
        match Unit.parse text with
        | Error refusal -> Error(sprintf "'%s' is not a unit: %A" text refusal)
        | Ok u when Unit.render u = text -> Ok u
        | Ok u ->
            Error(
                "'"
                + text
                + "' is not canonical unit text; the unit is written '"
                + Unit.render u
                + "'"
            )

/// A gate the submitter asks the backend to evaluate. Generic by construction — a
/// `(name, threshold, direction)` triple onto which any dialect's own gate vocabulary maps,
/// plus the unit the threshold is stated in, where it states one (D39).
type GateRequest =
    {
        Name: string
        Threshold: float
        /// The unit `Threshold` is in. `None` states none, and is evaluated only against a
        /// diagnostic that reports none — the path every gate took before units existed.
        Unit: UnitOfMeasure option
        Direction: GateDirection
    }

/// A gate the backend evaluated: the request, plus what was observed and whether it held.
type GateVerdict =
    {
        Name: string
        Threshold: float
        /// As requested: the unit both `Threshold` and `Observed` are in.
        Unit: UnitOfMeasure option
        Direction: GateDirection
        Observed: float
        Passed: bool
    }

/// A gate stated in one unit against a diagnostic reported in another (D39). Never converted:
/// the threshold is the submitter's, and re-reading it in another unit answers a question
/// nobody asked. `None` on either side is "states no unit", which agrees only with `None`.
type UnitMismatch =
    { Gate: string
      Stated: UnitOfMeasure option
      Reported: UnitOfMeasure option }

[<RequireQualifiedAccess>]
module GateVerdict =

    /// Evaluate a request against an observed value. The manager's ONE numerical
    /// operation, and it is a comparison — not statistics. It does not read units: a
    /// backend that knows its diagnostics' units calls `evaluateIn`.
    let evaluate (req: GateRequest) (observed: float) : GateVerdict =
        let passed =
            match req.Direction with
            | AtLeast -> observed >= req.Threshold
            | AtMost -> observed <= req.Threshold

        { Name = req.Name
          Threshold = req.Threshold
          Unit = req.Unit
          Direction = req.Direction
          Observed = observed
          Passed = passed }

    /// Evaluate a request against an observed value reported in `reported` — only where that
    /// is the unit the request states, and refused by name otherwise.
    let evaluateIn
        (reported: UnitOfMeasure option)
        (req: GateRequest)
        (observed: float)
        : Result<GateVerdict, UnitMismatch> =
        if req.Unit = reported then
            Ok(evaluate req observed)
        else
            Error
                { Gate = req.Name
                  Stated = req.Unit
                  Reported = reported }

/// Why a backend refused. A closed enumeration of data, not an error string: a caller that
/// must decide whether to wait, reduce scope, or ask a human needs to branch on the reason,
/// and it cannot branch on prose.
///
/// It lives HERE rather than beside the port it describes because a replay receipt carries
/// one per branch (`ReplayRefusal`, below), and the receipt is part of the folded record. A
/// refusal union that cannot say what actually happened is worse than a file boundary in the
/// wrong place.
type BackendRefusal =
    | BudgetDenied of quota: float * spent: float
    | GateFailed of GateVerdict list
    | UnknownProvider of known: string list
    | BackendPolicyRefused of rule: string
    /// A gate's unit is not the unit its diagnostic is reported in (D39).
    | GateUnitMismatch of UnitMismatch

[<RequireQualifiedAccess>]
module BackendRefusal =

    let describe (r: BackendRefusal) : string =
        match r with
        | BudgetDenied(quota, spent) -> sprintf "budget denied: %g of %g already spent" spent quota
        | GateFailed verdicts ->
            "gates failed: "
            + (verdicts
               |> List.filter (fun v -> not v.Passed)
               |> List.map (fun v -> v.Name)
               |> String.concat ", ")
        | UnknownProvider known -> "unknown provider (known: " + String.concat ", " known + ")"
        | BackendPolicyRefused rule -> "refused by backend rule '" + rule + "'"
        | GateUnitMismatch m ->
            let text =
                function
                | Some u -> "'" + Unit.render u + "'"
                | None -> "no unit"

            "gate '"
            + m.Gate
            + "' states "
            + text m.Stated
            + " but its diagnostic is reported in "
            + text m.Reported

/// The join between one decision-record state and one execution-record artifact.
///
/// Every component is a string, deliberately: the manager compares composite keys, it
/// never interprets them, so nothing here is typed to a producer's own model. Two
/// records naming the same fit agree on all five components with zero type sharing
/// between the two sides.
type CompositeKey =
    { SpecHash: string
      DatasetVersion: string
      Seed: string
      ProviderId: string
      ProviderVersion: string }

/// What an execution backend reported about one fit, recorded IN the decision record.
///
/// This is the self-containment rule made concrete: the analysis surface reads snapshots
/// off the exploration itself and never queries a live backend, so the evidence base is
/// offline-queryable and re-derivable rather than a runtime dependency. `Diagnostics` is
/// an OPEN name → value map — the manager enumerates no diagnostic vocabulary, because
/// every dialect has its own. `DiagnosticUnits` states what a diagnostic measures, where the
/// backend says (D39): every key names a diagnostic in `Diagnostics`, and a diagnostic with no
/// entry states no unit.
type OutcomeSnapshot =
    { Key: CompositeKey
      ArtifactRef: string
      Diagnostics: Map<string, float>
      DiagnosticUnits: Map<string, UnitOfMeasure>
      Gates: GateVerdict list
      SubmittedBy: SubmitterClass
      RecordedAt: string }

[<RequireQualifiedAccess>]
module OutcomeSnapshot =

    /// Did every gate hold? An outcome with no gates trivially passes — the absence of a
    /// gate is not a failure, it is the absence of a question.
    let gatesPassed (o: OutcomeSnapshot) : bool =
        o.Gates |> List.forall (fun g -> g.Passed)

    /// The gates that did not hold, in recorded order.
    let failedGates (o: OutcomeSnapshot) : GateVerdict list =
        o.Gates |> List.filter (fun g -> not g.Passed)

/// A curator's verdict on a spec state.
///
/// `Kill` is a VERDICT, not a deletion: an abandoned branch is informative precisely
/// because it was abandoned, and the reason for abandoning it is the most valuable thing
/// recorded about it. Nothing in this library removes a state from an exploration.
type CurationVerdict =
    | Keep
    | Kill
    | Note

[<RequireQualifiedAccess>]
module CurationVerdict =

    let encode (v: CurationVerdict) : string =
        match v with
        | Keep -> "keep"
        | Kill -> "kill"
        | Note -> "note"

    let tryParse (s: string) : Result<CurationVerdict, string> =
        match s with
        | "keep" -> Ok Keep
        | "kill" -> Ok Kill
        | "note" -> Ok Note
        | other -> Error("unknown curation verdict '" + other + "' (expected keep | kill | note)")

/// One curation act: a verdict, the reason for it, who reached it and when.
///
/// `Reason` is required by shape rather than by validation — the record cannot be
/// constructed without one. A verdict with no reason teaches nothing.
type CurationRecord =
    { Verdict: CurationVerdict
      Reason: string
      CuratedBy: string
      CuratedAt: string }

/// A recorded, justified, attributed departure from a cross-sibling invariant.
///
/// The invariant itself is not the manager's — it is declared by a dialect and evaluated
/// dialect-side (see `Forest`), so `InvariantId` is an opaque identifier here exactly as a
/// spec payload's body is opaque. What the manager owns is the DECLARATION: that somebody
/// said this branch may differ, said why, and signed it.
///
/// This is what "permitted but priced" means concretely. A declaration downgrades a finding
/// and never removes it — the finding stays in the assessment with the justification attached,
/// so a reader sees both the divergence and the argument for it. A mechanism that deleted the
/// finding would make the price zero, which is the one thing it must not be.
///
/// `Justification` is required by shape rather than by validation, for the same reason
/// `CurationRecord.Reason` is: a divergence with no argument teaches nothing, and a field that
/// can be omitted will be.
type DivergenceDeclaration =
    { InvariantId: string
      Justification: string
      DeclaredBy: string
      DeclaredAt: string }

/// The identity of one party class — WHO stands behind a spec state, as a class rather
/// than as a person.
///
/// A class rather than an individual because the question the record answers is "has the
/// client side said anything about this?", not "did this particular reader read it". The
/// individual is carried separately (`AssentRecord.AssentedBy`), so attribution survives
/// without the class dissolving into a list of names.
type PartyClassId = string

/// A declared party class, and what it is for.
///
/// DECLARED, as policy data, and that is the whole of the guard. A party minted at call
/// time would make the denominator of every consensus question a function of who happened
/// to speak — a state endorsed by the one party that bothered to appear would read exactly
/// like one every party stands behind. The declared set is what "every declared party" is
/// quantified over, so it has to exist before the first record, not accumulate from it.
///
/// `Description` is for a person, like a comparator's, and is deliberately not compared.
type PartyClass =
    { Id: PartyClassId
      Description: string }

/// What one party said about a spec state.
///
/// Three cases and no more. `Endorse` and `Challenge` are the two positions a party can
/// hold; `Withdraw` is standing down from whichever it held, and it is a RECORD of doing
/// so rather than an erasure of what was held — nothing in this library removes an assent,
/// exactly as nothing removes a curation.
type AssentVerdict =
    | Endorse
    | Challenge
    | Withdraw

[<RequireQualifiedAccess>]
module AssentVerdict =

    let encode (v: AssentVerdict) : string =
        match v with
        | Endorse -> "endorse"
        | Challenge -> "challenge"
        | Withdraw -> "withdraw"

    let tryParse (s: string) : Result<AssentVerdict, string> =
        match s with
        | "endorse" -> Ok Endorse
        | "challenge" -> Ok Challenge
        | "withdraw" -> Ok Withdraw
        | other ->
            Error(
                "unknown assent verdict '"
                + other
                + "' (expected endorse | challenge | withdraw)"
            )

    let all = [ Endorse; Challenge; Withdraw ]

/// One assent act: which party class, what it said, why, who signed it and when.
///
/// `Reason` is required by shape rather than by validation, for the third time in this file
/// and for the third form of one argument (`CurationRecord.Reason`, then
/// `DivergenceDeclaration.Justification`). An endorsement with no reason is indistinguishable
/// from a signature nobody read the thing for; a challenge with no reason is an objection
/// nobody can answer; and a withdrawal with no reason is the most expensive of the three,
/// because the record then says a party changed its position and nothing says why.
type AssentRecord =
    { Party: PartyClassId
      Verdict: AssentVerdict
      Reason: string
      AssentedBy: string
      AssentedAt: string }

/// Why an assent record was not built.
///
/// Enumerable data naming the alternatives, like every refusal here: a caller that misspelled
/// a party class can only act on being told which classes are declared.
type AssentRefusal =
    /// The record names a party class no policy declares. Refused rather than admitted,
    /// because a record standing in the name of a party nobody declared reads in the record
    /// exactly like one that counts, and counts toward nothing.
    | UndeclaredParty of party: PartyClassId * declared: PartyClassId list
    /// The record carries no reason. Refused by the gate as well as required by the record's
    /// shape, so a whitespace reason cannot satisfy the type and defeat the point.
    | UnreasonedAssent of party: PartyClassId

[<RequireQualifiedAccess>]
module AssentRefusal =

    let describe (r: AssentRefusal) : string =
        match r with
        | UndeclaredParty(party, declared) ->
            "no policy declares the party class '"
            + party
            + "' (declared: "
            + (if List.isEmpty declared then
                   "none"
               else
                   String.concat ", " declared)
            + ")"
        | UnreasonedAssent party ->
            "the assent recorded by '"
            + party
            + "' carries no reason — a position nobody argued for cannot be answered"

/// A proposed branch, held unapplied until resolved: the shape an automated explorer
/// uses when it may suggest but not act. Resolving it with `Accept` materialises exactly
/// the state described here, so what was reviewed is what lands.
type BranchProposal =
    { ProposalId: ProposalId
      From: StateId
      ProposedId: StateId
      Label: string
      Payload: SpecPayload
      Rationale: string
      ProposedBy: string }

/// How a proposal was resolved.
type ProposalOutcome =
    | Accepted
    | Declined of reason: string

/// How a spec state came to exist.
type StateOrigin =
    /// The exploration's first state.
    | Seeded
    /// A fork from an existing state.
    | ForkedFrom of StateId
    /// A `Source` state's payload re-landed onto `Onto` — the cherry-pick shape.
    | CherryPickedFrom of source: StateId * onto: StateId
    /// A proposal accepted into the exploration.
    | FromProposal of ProposalId * from: StateId

/// One spec state: its identity and lineage, the opaque payload, everything curated
/// about it, everything an execution backend reported about it, every divergence declared
/// on it, and every party's assent to it.
///
/// `Divergences` is a LOG, for the same reason `Curations` is (D3): a branch whose
/// justification was restated, or restated differently, has a history worth keeping, and the
/// current declaration is a cheap derivation off the log rather than the other way round.
/// `Assents` is a log for the same reason and one more: a party that endorsed, challenged
/// and then stood down has said three things, and the sequence is what a later reader needs
/// in order to understand a consensus that took work to reach.
type SpecState =
    { Id: StateId
      Label: string
      Parent: StateId option
      Origin: StateOrigin
      Payload: SpecPayload
      Curations: CurationRecord list
      Outcomes: OutcomeSnapshot list
      Divergences: DivergenceDeclaration list
      Assents: AssentRecord list }

/// Why an op was refused by the fold. Enumerable data, never a bare error string: a
/// refusal names the failure so a caller can enumerate what to do instead.
///
/// It sits here, ahead of the op vocabulary it describes, because a replay receipt carries
/// one per branch that did not land, and the receipt is part of the folded record below.
type Rejection =
    /// An op other than `Seed` arrived before the exploration had a root.
    | NotSeeded
    /// A second `Seed` on an exploration that already has a root.
    | AlreadySeeded of existing: StateId
    | UnknownState of StateId
    /// A state id an op would mint is already taken. Ids are permanent; reusing one
    /// would silently rewrite history.
    | DuplicateState of StateId
    /// A branch whose curation verdict is `Kill` cannot be extended. Reviving it is a
    /// deliberate act — a fresh `Keep` curation — not an accident of a later fork.
    | KilledState of StateId
    | UnknownProposal of ProposalId
    | ProposalAlreadyTabled of ProposalId
    | ProposalAlreadyResolved of ProposalId
    /// The same fit — same composite key — annotated onto the same state twice.
    | DuplicateOutcome of StateId * CompositeKey
    /// A replay id the record already carries. Campaigns are permanent, like states.
    | DuplicateReplay of ReplayId
    /// A receipt whose landed and refused branches do not partition what it says it
    /// requested. Refused rather than repaired: a branch in neither list is a fit the
    /// submitter believes was taken and nothing downstream has a record of.
    | IncompleteReplay of ReplayId
    /// The history already holds this op's content id for a different node — a content-id
    /// collision. The node held first stays and the op is refused, where the history used to
    /// replace it silently.
    | HistoryCollision of nodeId: string

[<RequireQualifiedAccess>]
module Rejection =

    /// A one-line description. Data first, prose second — callers branch on the case.
    let describe (r: Rejection) : string =
        match r with
        | NotSeeded -> "the exploration has no root yet — seed it first"
        | AlreadySeeded existing -> "the exploration is already seeded at state '" + existing + "'"
        | UnknownState id -> "no such state: '" + id + "'"
        | DuplicateState id -> "state id '" + id + "' is already taken — ids are permanent"
        | KilledState id -> "state '" + id + "' is killed — re-curate it as kept before extending it"
        | UnknownProposal id -> "no such proposal: '" + id + "'"
        | ProposalAlreadyTabled id -> "proposal '" + id + "' is already tabled"
        | ProposalAlreadyResolved id -> "proposal '" + id + "' is already resolved"
        | DuplicateOutcome(id, key) ->
            "state '"
            + id
            + "' already carries the outcome for composite key ("
            + key.SpecHash
            + ", "
            + key.DatasetVersion
            + ", "
            + key.Seed
            + ", "
            + key.ProviderId
            + ", "
            + key.ProviderVersion
            + ")"
        | DuplicateReplay id -> "replay id '" + id + "' is already recorded — ids are permanent"
        | IncompleteReplay id ->
            "replay '"
            + id
            + "' does not account for every branch it requested — a branch that is neither landed nor refused is a fit nothing has a record of"
        | HistoryCollision id ->
            "the history already holds node '"
            + id
            + "' for different content — a content-id collision, so the op is refused"

/// Why one branch of a replay did not land.
///
/// Per BRANCH, because a replay completes partially rather than aborting. A branch the
/// executor would not take is a fact about that branch; abandoning the nine beside it would
/// throw away evidence already paid for, and would make a campaign's outcome depend on which
/// item happened to fail first.
type ReplayRefusal =
    /// The branch carries no prior fit, so there is no seed to carry and no key to hold
    /// constant. A replay re-fits a decision that was fitted; a branch nobody ever fitted
    /// is not one, and inventing a seed for it would mint a fit joined to nothing.
    | NoBaselineFit
    /// The executor would not take it, or took it and registered nothing.
    | ExecutorRefused of BackendRefusal
    /// The fold would not record the outcome. The ordinary case is `DuplicateOutcome` —
    /// this branch was already replayed onto this vintage — which is the identity rule
    /// doing its job rather than anything going wrong.
    | NotRecorded of Rejection

[<RequireQualifiedAccess>]
module ReplayRefusal =

    let describe (r: ReplayRefusal) : string =
        match r with
        | NoBaselineFit -> "the branch has never been fitted, so there is no baseline fit to replay"
        | ExecutorRefused refusal -> "the executor refused it: " + BackendRefusal.describe refusal
        | NotRecorded rejection -> "the outcome could not be recorded: " + Rejection.describe rejection

/// What one replay campaign did: which vintage, which branches were asked, which landed
/// and under what key, which were refused and why.
///
/// The shape is the execution port's own receipt one layer up, and for the same reason.
/// `Landed` and `Refused` MUST partition `Requested` exactly — a branch in neither is a fit
/// the submitter believes was taken and nothing has a record of, which nothing downstream
/// can detect — so the fold refuses a receipt that does not (`IncompleteReplay`).
///
/// A receipt is a RECORD OF AN ACT, not a second route for evidence. The outcomes it names
/// reached their branches through the ordinary `Annotate` path, one at a time, each subject
/// to the duplicate-outcome guard. Nothing here writes an outcome.
type ReplayReceipt =
    {
        ReplayId: ReplayId
        /// The vintage every branch was re-submitted against — the ONE component of the
        /// composite key a replay is permitted to move.
        DatasetVersion: string
        Requested: StateId list
        Landed: (StateId * CompositeKey) list
        Refused: (StateId * ReplayRefusal) list
        ReplayedBy: string
        ReplayedAt: string
    }

[<RequireQualifiedAccess>]
module ReplayReceipt =

    /// Did the receipt account for every branch it says it requested, exactly once?
    let isTotal (r: ReplayReceipt) : bool =
        let accounted =
            (r.Landed |> List.map fst) @ (r.Refused |> List.map fst) |> List.sort

        let requested = r.Requested |> List.sort

        accounted = requested
        && List.length (List.distinct requested) = List.length requested

/// Which direction of a diagnostic a comparison reads as the leading one.
///
/// Supplied with the question, never assumed. `OutcomeSnapshot.Diagnostics` is an OPEN map
/// whose names belong to a dialect, so a manager that knew an "error" runs low would have
/// acquired exactly the model vocabulary this library exists without.
type MetricPreference =
    | PreferHigher
    | PreferLower

[<RequireQualifiedAccess>]
module MetricPreference =

    let encode (p: MetricPreference) : string =
        match p with
        | PreferHigher -> "higher"
        | PreferLower -> "lower"

    let tryParse (s: string) : Result<MetricPreference, string> =
        match s with
        | "higher" -> Ok PreferHigher
        | "lower" -> Ok PreferLower
        | other -> Error("unknown metric preference '" + other + "' (expected higher | lower)")

    let all = [ PreferHigher; PreferLower ]

/// The thresholds past which an out-of-time record may raise a question about a curation
/// decision.
///
/// DATA, never constants, and that is the whole of the slow-accumulation guard. Early
/// vintages are low-powered and regime-confounded: a decision that looks wrong after one
/// refresh may be a blip, and an agent that re-curated on it would be chasing noise — the
/// same failure the whole approach rejects, arrived at from the other side. Where the
/// numbers should sit is a house judgement that differs by unit, by cadence and by how much
/// history there is, so it is declared by whoever declares the process and read from here.
///
/// Absence is not a default. `Forest.Thresholds` is an option, and a forest that declares
/// none raises NO question at all, ever. The gate is opened deliberately or it stays shut.
type CorrectionThresholds =
    {
        /// The diagnostic the comparison reads, as the dialect spells it in its gate
        /// vocabulary and as an executor carries it in `OutcomeSnapshot.Diagnostics`.
        Metric: string
        Prefer: MetricPreference
        /// How many OUT-OF-TIME vintages must show the set-aside branch reading ahead.
        /// Read as at least one however it is declared — a threshold of zero is not a
        /// threshold, and silently honouring it would open the gate completely.
        MinVintages: int
        /// By how much it must read ahead on each of them, in `Unit`.
        MinMargin: float
        /// The unit `MinMargin` is in, and the one unit a reading must state to be compared
        /// (D40). `None` states none, and compares only readings that state none — the path
        /// every record took before units existed.
        Unit: UnitOfMeasure option
        /// How many siblings must raise the same question. This is the forest axis of the
        /// accumulation; a single exploration is ONE sibling and is counted as one, so a
        /// value above 1 silences it there. Skipping it instead would let the cheapest
        /// read bypass the gate the expensive one enforces.
        MinSiblings: int
    }

/// The folded decision record: the states, their lineage, the working head, and the
/// proposal ledger. Produced only by folding the op vocabulary — never edited directly.
type Exploration =
    {
        States: Map<StateId, SpecState>
        /// States in the order they entered the exploration — the deterministic iteration
        /// order every projection uses (a `Map` enumerates by key, which is not history).
        Order: StateId list
        Root: StateId option
        /// Where work is currently pointed. `ReturnTo` moves this and nothing else: going
        /// back to an ancestor loses no state, which is what makes abandonment cheap.
        Head: StateId option
        Proposals: Map<ProposalId, BranchProposal>
        Resolutions: Map<ProposalId, ProposalOutcome>
        /// The replay campaigns this record has seen, in the order they were recorded.
        ///
        /// A LOG, for the third time in this file and for the third form of the same
        /// argument (D3, and again for declarations): a record that kept only the latest
        /// campaign would lose the accumulation, and the accumulation is the entire signal
        /// — one replay says nothing, and the design says so in as many words.
        Replays: ReplayReceipt list
    }

[<RequireQualifiedAccess>]
module Exploration =

    let empty: Exploration =
        { States = Map.empty
          Order = []
          Root = None
          Head = None
          Proposals = Map.empty
          Resolutions = Map.empty
          Replays = [] }
