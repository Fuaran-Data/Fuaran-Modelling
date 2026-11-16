// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

// ============================================================================
//  The consensual axis — who stands behind a spec state, and why.
//
//  Model quality has two axes and only one of them is statistical. Gate verdicts
//  and outcome snapshots record what the data said; whether the modeller, the
//  client and — sometimes — a third-party reviewer stand behind a state is a
//  different question with a different kind of answer, and a record that carries
//  only the first collapses the second into whoever happened to hold the role.
//
//  So assent is party-scoped and recorded like everything else here: an ordinary
//  op on the branch it is about, attributed, reasoned, appended, never erased.
//
//  Three disciplines run through the file:
//
//    * The party classes are DECLARED, as policy data. "Every declared party
//      endorses" is only a statement if the denominator exists before the first
//      record; assembled from whoever spoke, it says nothing at all.
//    * Consensus is DERIVED and silence is not endorsement. A party that has
//      said nothing has not agreed, and a party that stood down has not either —
//      the same asymmetry `Query.keptStates` holds for curation.
//    * A challenge downgrades nothing. It does not block a fit, invalidate a
//      gate or re-curate a branch; it is recorded and priced, exactly as a
//      divergence declaration is. What it DOES is make a disagreement between
//      the two axes visible, which is what the tension surface below is for.
//
//  Nothing here changes a record. Every function takes a folded exploration and
//  returns an answer.
// ============================================================================

/// Where one spec state stands with the parties a policy declares.
///
/// The four declared-party lists PARTITION the declared set: every declared class is
/// endorsing, challenging, stood down, or silent, exactly once. That is what makes the
/// consensus question answerable by counting rather than by inference, and it is why
/// `Silent` is a field rather than an absence — "nobody asked the client" and "the client
/// endorsed" must never read the same.
type AssentStanding =
    {
        State: StateId
        /// The classes this policy declares, in declaration order — the denominator.
        Declared: PartyClassId list
        /// Declared classes whose live position is an endorsement.
        Endorsed: PartyClassId list
        /// Declared classes whose live position is a challenge.
        Challenged: PartyClassId list
        /// Declared classes that spoke and then stood down. Carried separately from `Silent`
        /// because a party that withdrew has been through the record and a party that never
        /// spoke has not — the same information, and only one of them is worth chasing.
        Withdrawn: PartyClassId list
        /// Declared classes that have said nothing at all.
        Silent: PartyClassId list
        /// Live positions held in the name of a class this policy does NOT declare — the
        /// residue of a class retired after its records were made. Named rather than dropped,
        /// and counted toward nothing: a record whose denominator quietly absorbed a retired
        /// class would answer a question nobody asked.
        Unrecognised: PartyClassId list
    }

/// Which way round the two axes disagree.
///
/// Two cases, because there are two axes and either can be the one that is out in front.
/// Neither is the other's error, and the type is deliberately not named for a fault: a
/// challenge standing over green gates may be the challenge that is right, and an endorsed
/// state whose gates failed may be a gate that asks the wrong question.
type TensionDirection =
    /// Every gate the latest evidence reports held, and a declared party challenges.
    | ChallengedWhereGatesHeld
    /// A gate did not hold, and every declared party endorses.
    | EndorsedWhereGatesFailed

/// One state on which the statistical record and the consensual record disagree.
///
/// The load-bearing query of this whole file. Both axes agreeing is the ordinary case and
/// needs no surface; the pairs that disagree are the ones somebody has to look at, and
/// without this they are implicit in two records nobody reads side by side.
type AssentTension =
    {
        State: StateId
        Direction: TensionDirection
        /// The gates in the state's latest evidence that did not hold, by name. Empty in the
        /// `ChallengedWhereGatesHeld` direction, by construction.
        FailedGates: string list
        Standing: AssentStanding
    }

/// How one decision stands with the parties, across a forest's siblings.
///
/// The forest roll-up of the above, on the same machinery as `Forest.chosenRecurrence` and
/// with the same caution: this counts where each sibling's parties stand, and a decision with
/// consensus in nine units out of ten is a decision nine sets of people signed, not a decision
/// that is better than the tenth's. The first five fields partition the forest's members.
type AssentRecurrence =
    {
        State: StateId
        Label: string
        /// Siblings in which every declared party endorses it.
        ConsensusIn: SiblingId list
        /// Siblings in which at least one declared party challenges it.
        ChallengedIn: SiblingId list
        /// Siblings in which some parties endorse, none challenges, and not all have spoken.
        PartlyEndorsedIn: SiblingId list
        /// Siblings in which no declared party has said anything about it.
        SilentIn: SiblingId list
        /// Siblings whose exploration has no such state at all.
        AbsentIn: SiblingId list
        /// Siblings in which the two axes disagree about it, either way round. Cross-cutting
        /// rather than part of the partition — a tension is a fact about a sibling's record,
        /// not a sixth place a state can stand.
        InTensionIn: SiblingId list
    }

/// The consensual axis: derived reads over the assent log, and the gate that builds an
/// assent op.
///
/// Named `PartyAssent` rather than `Assent` because `Assent` already names the op case, and a
/// fully-qualified `Fuaran.Modelling.Assent.standing` would resolve to the case constructor
/// rather than to this module. The same collision `ReplayCampaign` was named around, avoided
/// the same way and for the same reason: it only bites callers who qualify, so it is
/// discovered by somebody else, later.
[<RequireQualifiedAccess>]
module PartyAssent =

    // ---- the statistical axis, read for comparison ---------------------------

    /// The gate verdicts in a state's most recent evidence, if it has any that carry gates.
    ///
    /// MOST RECENT rather than all of them, because "do the gates hold" is a question about
    /// where the branch stands now; how its readings moved across vintages is a different
    /// question, and the vintage axis answers that one properly. Reading every outcome would
    /// leave a branch red forever on the strength of its first fit.
    ///
    /// `None` covers two cases that must not manufacture a verdict: a state nobody has fitted,
    /// and an outcome that requested no gate. Neither is green and neither is red — the
    /// absence of a gate is the absence of a question, exactly as `OutcomeSnapshot.gatesPassed`
    /// says, and a tension against a question nobody asked would be an artefact of silence.
    let latestGates (s: SpecState) : GateVerdict list option =
        match List.rev s.Outcomes with
        | latest :: _ when not (List.isEmpty latest.Gates) -> Some latest.Gates
        | _ -> None

    // ---- the consensual axis -------------------------------------------------

    /// Where one state stands with the declared classes.
    let standing (declared: PartyClassId list) (s: SpecState) : AssentStanding =
        let live = Query.currentAssents s
        let declaredSet = Set.ofList declared

        let holding (v: AssentVerdict) =
            declared
            |> List.filter (fun p -> live |> List.exists (fun a -> a.Party = p && a.Verdict = v))

        { State = s.Id
          Declared = declared
          Endorsed = holding Endorse
          Challenged = holding Challenge
          Withdrawn = holding Withdraw
          Silent =
            declared
            |> List.filter (fun p -> not (live |> List.exists (fun a -> a.Party = p)))
          Unrecognised =
            live
            |> List.map (fun a -> a.Party)
            |> List.filter (fun p -> not (Set.contains p declaredSet))
            |> List.distinct
            |> List.sort }

    /// Does every declared party endorse this state?
    ///
    /// A policy that declares NO party class reaches consensus on nothing, ever. The
    /// alternative reading — that "all of none endorse" is agreement — is vacuously true and
    /// completely wrong: it would report unanimous backing for every state in every record
    /// that never thought about parties at all. Declared or silent; there is no third state,
    /// and this is the same guard `CorrectionThresholds` needs against a threshold of zero.
    let consensus (st: AssentStanding) : bool =
        not (List.isEmpty st.Declared)
        && List.length st.Endorsed = List.length st.Declared

    /// Is any declared party standing against this state right now?
    let contested (st: AssentStanding) : bool = not (List.isEmpty st.Challenged)

    /// Every state's standing, in exploration order.
    let standings (declared: PartyClassId list) (e: Exploration) : AssentStanding list =
        Query.states e |> List.map (standing declared)

    /// States every declared party endorses.
    let consensusStates (declared: PartyClassId list) (e: Exploration) : StateId list =
        standings declared e |> List.filter consensus |> List.map (fun st -> st.State)

    /// States a declared party is standing against.
    let contestedStates (declared: PartyClassId list) (e: Exploration) : StateId list =
        standings declared e |> List.filter contested |> List.map (fun st -> st.State)

    // ---- the tension surface -------------------------------------------------

    /// Do this state's two axes disagree, and which way round?
    ///
    /// A state with no gate evidence has no statistical position to disagree with, so it is
    /// never in tension however loudly the parties disagree with each other. That is not a
    /// gap: a challenge on an unfitted branch is an ordinary recorded challenge, and calling
    /// it a tension would put it in the one report that exists to name pairs.
    let tension (declared: PartyClassId list) (s: SpecState) : AssentTension option =
        let st = standing declared s

        match latestGates s with
        | None -> None
        | Some gates ->
            let failed =
                gates |> List.filter (fun g -> not g.Passed) |> List.map (fun g -> g.Name)

            if List.isEmpty failed && contested st then
                Some
                    { State = s.Id
                      Direction = ChallengedWhereGatesHeld
                      FailedGates = []
                      Standing = st }
            elif not (List.isEmpty failed) && consensus st then
                Some
                    { State = s.Id
                      Direction = EndorsedWhereGatesFailed
                      FailedGates = failed
                      Standing = st }
            else
                None

    /// Every state on which the two axes disagree, in exploration order.
    let tensions (declared: PartyClassId list) (e: Exploration) : AssentTension list =
        Query.states e |> List.choose (tension declared)

    // ---- the rendered lines --------------------------------------------------

    /// One line per standing, in the vocabulary the types insist on.
    ///
    /// In the library rather than left to each caller, for the reason `describeRecurrence` and
    /// `describeCorrection` are: the phrasing is part of the contract. A caller rendering this
    /// itself is one edit from "three parties approved, so it is the better form" — which is a
    /// count of signatures wearing a verdict's clothes.
    let describeStanding (st: AssentStanding) : string =
        "'"
        + st.State
        + "' is endorsed by "
        + string (List.length st.Endorsed)
        + " of "
        + string (List.length st.Declared)
        + " declared parties"
        + (if List.isEmpty st.Challenged then
               ""
           else
               ", challenged by " + string (List.length st.Challenged))
        + (if List.isEmpty st.Withdrawn then
               ""
           else
               ", with " + string (List.length st.Withdrawn) + " stood down")
        + " — who stands behind it and why, not what is better"

    /// The one sentence a tension may be rendered as.
    ///
    /// It has to say that nothing is resolved, because the attractive next sentence is the one
    /// that picks a winner. Neither axis overrides the other: a challenge standing over green
    /// gates may be the reading that is right, and gates failing under full endorsement may be
    /// a gate asking the wrong question. The disagreement is the finding.
    let describeTension (t: AssentTension) : string =
        (match t.Direction with
         | ChallengedWhereGatesHeld ->
             "'"
             + t.State
             + "' — every gate held, and "
             + string (List.length t.Standing.Challenged)
             + " declared party(s) have an open challenge"
         | EndorsedWhereGatesFailed ->
             "'"
             + t.State
             + "' — every declared party endorses it, and the gate(s) "
             + String.concat ", " t.FailedGates
             + " did not hold")
        + ". The two axes disagree. Nothing here resolves them, and neither reading is the other's error."

    // ---- the forest roll-up --------------------------------------------------

    /// Every state id any supplied sibling explored, in forest-then-exploration order.
    let private allStateIds (f: Forest) (records: Map<SiblingId, Exploration>) : StateId list =
        [ for ref in f.Siblings do
              match Map.tryFind ref.Sibling records with
              | None -> ()
              | Some e -> yield! e.Order ]
        |> List.distinct

    /// The siblings a forest holds a record for, in declaration order.
    let private present (f: Forest) (records: Map<SiblingId, Exploration>) : (SiblingId * Exploration) list =
        f.Siblings
        |> List.choose (fun ref -> Map.tryFind ref.Sibling records |> Option.map (fun e -> ref.Sibling, e))

    /// How each decision stands with the parties, across the forest.
    ///
    /// The declared classes come from the forest's own policy, because that is what a forest
    /// shares: one process applied independently to each sibling. A per-sibling declaration
    /// would make each row's denominator a different number and the column meaningless.
    let recurrence (f: Forest) (records: Map<SiblingId, Exploration>) : AssentRecurrence list =
        let declared = Policy.parties f.Policy
        let rows = present f records

        allStateIds f records
        |> List.map (fun stateId ->
            let per = rows |> List.map (fun (sibling, e) -> sibling, Query.tryState stateId e)

            let label =
                per
                |> List.tryPick (fun (_, st) -> st |> Option.map (fun s -> s.Label))
                |> Option.defaultValue ""

            let where (p: SpecState -> bool) =
                per
                |> List.choose (fun (sibling, st) ->
                    match st with
                    | Some s when p s -> Some sibling
                    | _ -> None)

            let standingOf (s: SpecState) = standing declared s

            { State = stateId
              Label = label
              ConsensusIn = where (standingOf >> consensus)
              ChallengedIn = where (standingOf >> contested)
              // The remainder of the partition, in the order the cases exclude one another:
              // a sibling in which nobody has spoken is silent, and one in which somebody has
              // but not everybody endorses is partly endorsed. A forest whose policy declares
              // no party class reports NEITHER — there is no position to be in, which is the
              // same answer `consensus` gives and for the same reason.
              PartlyEndorsedIn =
                where (fun s ->
                    let st = standingOf s

                    not (List.isEmpty st.Declared)
                    && not (consensus st)
                    && not (contested st)
                    && List.length st.Silent < List.length st.Declared)
              SilentIn =
                where (fun s ->
                    let st = standingOf s

                    not (List.isEmpty st.Declared)
                    && List.length st.Silent = List.length st.Declared)
              AbsentIn =
                per
                |> List.choose (fun (sibling, st) -> if st.IsNone then Some sibling else None)
              InTensionIn = where (fun s -> (tension declared s).IsSome) })
        |> List.sortBy (fun r -> -(List.length r.ConsensusIn), r.State)

    /// Every tension anywhere in the forest, sibling by sibling.
    ///
    /// The cross-sibling read the two-axes question is actually asked with: one unit whose
    /// gates and parties disagree is a conversation, and five units disagreeing the same way
    /// about the same decision is a question about the gate.
    let forestTensions (f: Forest) (records: Map<SiblingId, Exploration>) : (SiblingId * AssentTension) list =
        let declared = Policy.parties f.Policy

        [ for sibling, e in present f records do
              for t in tensions declared e -> sibling, t ]

    // ---- the declaration gate ------------------------------------------------

    /// Build an assent op, refusing a party class no policy declares and a record with no
    /// reason.
    ///
    /// The gate is here rather than in the fold for the reason the invariant gate is outside
    /// the fold: the declared set is policy data, and the fold sees a record rather than a
    /// process. It matters for one specific reason — an assent standing in the name of an
    /// undeclared class folds perfectly and reads, in the record, exactly like one that
    /// counts, while counting toward no consensus question at all.
    ///
    /// The refusal enumerates the declared set, because a caller that misspelled a class can
    /// act on nothing else.
    let record
        (declared: PartyClassId list)
        (state: StateId)
        (assent: AssentRecord)
        : Result<ExplorationOp, AssentRefusal> =
        if not (List.contains assent.Party declared) then
            Error(UndeclaredParty(assent.Party, declared))
        elif assent.Reason.Trim() = "" then
            Error(UnreasonedAssent assent.Party)
        else
            Ok(Assent { StateId = state; Record = assent })
