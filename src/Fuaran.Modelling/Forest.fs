// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

// ============================================================================
//  Sibling forests, and the cross-sibling invariant seam.
//
//  A forest is a set of sibling explorations — one per unit — governed by ONE
//  policy applied INDEPENDENTLY to each. That is the whole idea, and the word
//  "independently" is carrying it: every sibling keeps its own record, its own
//  branches and its own decisions, and what is shared is the process, never the
//  positions it arrives at.
//
//  So there is no cross-sibling op, and there is no forest op-DAG. Every act
//  that changes a record is an act within one sibling's own history; a forest is
//  a REFERENCE structure plus a shared policy, declared rather than folded. An
//  op that named two siblings would be the one construct able to couple them,
//  and it is absent by design rather than by omission.
//
//  What crosses the boundary is a QUESTION, not a change. A dialect may supply
//  comparators over projected sibling states; the manager hosts the evaluation
//  and owns the findings; and the escape from a finding is a declaration
//  recorded on the diverging branch — permitted, but priced, and never erased.
//
//  Everything here is opaque-payload discipline, unchanged: a comparator is a
//  function the manager calls, never a spec the manager reads.
// ============================================================================

/// The identity of one sibling within a forest — the unit the exploration is about.
///
/// Distinct from `StateId`, which identifies a branch INSIDE one sibling's exploration.
/// The two together address a decision across a forest, which is what makes the
/// cross-sibling questions below answerable at all.
type SiblingId = string

/// The identity of one forest.
type ForestId = string

/// One sibling's membership of a forest.
///
/// `ExplorationRef` is an opaque locator the forest carries and never resolves — where that
/// sibling's own op stream lives, in whatever addressing the host uses. This library performs
/// no IO, so resolution belongs to the host, and the query surface below takes the folded
/// records as arguments rather than fetching them.
type SiblingRef =
    { Sibling: SiblingId
      Label: string
      ExplorationRef: string }

/// A named set of sibling explorations under one shared policy.
///
/// The policy is held here, once, and applied per sibling by the ordinary governed-submit
/// path — there is no forest-level authorisation and no forest-level submit. Sharing the
/// policy is what "the same trusted process, applied independently" means in code: the grants
/// are common, the records they govern are not.
type Forest =
    {
        Id: ForestId
        Title: string
        Siblings: SiblingRef list
        Policy: Policy
        /// The thresholds past which this forest's out-of-time evidence may raise a
        /// question about a curation decision. `None` means it raises none, ever.
        ///
        /// An OPTION rather than a defaulted record, and that is the guard rather than a
        /// nicety. A default would be a number this library invented for a judgement it
        /// cannot make — how much history a unit has, how fast its regime moves, how
        /// expensive a wrong question is — and it would apply silently to every forest
        /// that never thought about it. Declared or silent; there is no third state.
        Thresholds: CorrectionThresholds option
    }

/// Which of a sibling's states cross the invariant seam.
///
/// Closed, because a comparator's answer depends on what it was shown, and a selection
/// nobody can name is a comparison nobody can reproduce.
type StateSelection =
    /// Every state whose current verdict is `Keep` — the sibling's settled positions.
    | KeptStates
    /// The exploration's leaves, curated or not — where work currently stands.
    | FrontierStates
    /// Named states, by permanent id. The "same decision across siblings" address: ids are
    /// author-minted and permanent, so a house process that mints one id in every sibling
    /// makes one decision addressable across the whole forest.
    | NamedStates of StateId list

[<RequireQualifiedAccess>]
module StateSelection =

    let encode (s: StateSelection) : string =
        match s with
        | KeptStates -> "kept"
        | FrontierStates -> "frontier"
        | NamedStates _ -> "named"

/// One sibling state as it crosses the invariant seam.
///
/// Strings and an opaque payload — the manager hands a comparator exactly what it hands
/// anything else, and reads none of it. A comparator imports the payload into its OWN trees,
/// which is why the seam can carry an invariant about model structure without the manager
/// ever acquiring an opinion about what model structure is.
type ProjectedState =
    { Sibling: SiblingId
      State: StateId
      Payload: SpecPayload }

/// One thing a comparator found: which invariant, which siblings are implicated, and a
/// human-readable detail.
///
/// `Siblings` is the finding's subject and is compared; `Detail` is for a person and is
/// deliberately not — the same reasoning the dialect seam applies to a validator's message
/// and to a residue entry's detail. A comparator may reword freely and must not renumber.
type InvariantFinding =
    { InvariantId: string
      Siblings: SiblingId list
      Detail: string }

/// A comparator over projected sibling states, supplied by a dialect.
///
/// `Compare` runs dialect-side over the dialect's own trees, imported from the payloads it is
/// given, and answers in the manager's vocabulary. The manager calls it and stamps the
/// resulting findings with `Id`, so a comparator cannot attribute a finding to an invariant
/// that is not its own.
type InvariantComparator =
    { Id: string
      Description: string
      Compare: ProjectedState list -> InvariantFinding list }

/// Where a finding stands once the declarations on its siblings are read.
///
/// Two levels, and the boundary is load-bearing rather than editorial — the same argument the
/// dialect seam makes for `Severity`. Either something accounts for this divergence or nothing
/// does; a third, "partly accounted" level would describe the ordinary case (the divergent
/// sibling declares, the sibling it diverges FROM has nothing to declare) as a half-failure,
/// which is not what it is.
type Standing =
    /// No sibling the finding names has declared a divergence from this invariant.
    | Unaccounted
    /// At least one named sibling declared, and the justifications are attached.
    | Downgraded

/// A finding, read against the declarations standing on the siblings it names.
///
/// The finding survives whatever the declarations say. Downgrading changes how loudly it
/// reads, never whether it is there — a mechanism that removed it would set the price of a
/// divergence to nothing, and the price is the entire point.
type AssessedFinding =
    {
        Finding: InvariantFinding
        Standing: Standing
        /// Every declaration found on a named sibling for this invariant, with the sibling and
        /// state it was declared on. What was justified, by whom, and why.
        Declarations: (SiblingId * StateId * DivergenceDeclaration) list
        /// The named siblings carrying no declaration for this invariant. A comparator that
        /// wants per-pair accounting emits per-pair findings; this field is how a coarser
        /// finding still shows which siblings were left unexplained.
        Undeclared: SiblingId list
    }

/// What one pass of the invariant seam over a forest found.
type ForestAssessment =
    {
        Forest: ForestId
        Selection: StateSelection
        /// How many (sibling, state) pairs the comparators were shown. A run over an empty
        /// projection finds nothing, and "found nothing" and "was asked nothing" must not read
        /// the same.
        Projected: int
        /// Which invariants ran, in the order they were supplied.
        Invariants: string list
        Findings: AssessedFinding list
    }

/// How often one decision was CHOSEN across a forest's siblings.
///
/// The naming is the finding, and it is not decoration. A branch DAG is a human-guided walk,
/// not a designed experiment: the branches that exist are the ones somebody thought worth
/// trying, and the ones kept are the ones somebody preferred. A count over that is a record of
/// **what was chosen**, and it will not support a claim about **what is better** no matter how
/// large it gets — the sample is biased by construction, and the bias is the process itself.
///
/// So every field below names an act, never a merit, and nothing here is a score, a rank or a
/// significance. What would license a better-than claim is an out-of-time record, and this is
/// not one. `Contract.recurrenceVocabularyLaw` holds the naming to it executably, because a
/// discipline stated only in a comment is one a later reader can edit away in good faith.
type ChosenRecurrence =
    {
        State: StateId
        /// The label the siblings gave this decision, as first seen in forest order. Labels are
        /// free text and may disagree across siblings; the id is the identity.
        Label: string
        /// Siblings whose current verdict on this state is `Keep`.
        ChosenIn: SiblingId list
        /// Siblings that explicitly killed it.
        SetAsideIn: SiblingId list
        /// Siblings that explored it and curated it neither way. Silence is not endorsement.
        UncuratedIn: SiblingId list
        /// Siblings whose exploration has no such state at all.
        AbsentIn: SiblingId list
        /// Siblings carrying a divergence declaration on it.
        DivergentIn: SiblingId list
    }

/// A per-sibling read of a forest — one row per member, whether or not its record was supplied.
type SiblingSummary =
    {
        Sibling: SiblingId
        Label: string
        /// Did the host supply this sibling's folded record? A forest names siblings; it does
        /// not hold them, so a missing record is an ordinary state of affairs and must be
        /// visible rather than silently counted as an empty exploration.
        Present: bool
        States: int
        Kept: StateId list
        Killed: StateId list
        Uncurated: StateId list
        Frontier: StateId list
        Outcomes: int
        Declarations: (StateId * DivergenceDeclaration) list
    }

/// Why a divergence declaration was not built.
///
/// Enumerable data naming the alternatives, like every refusal in this library: a caller that
/// misspelled an invariant id can only act on being told which ids exist.
type DeclarationRefusal =
    /// The declaration names an invariant no supplied comparator declares.
    | UnknownInvariant of invariantId: string * known: string list
    /// The declaration carries no justification. A divergence with no argument is the thing
    /// this whole mechanism exists to prevent, so it is refused by the gate as well as being
    /// required by the record's shape.
    | UnjustifiedDivergence of invariantId: string

[<RequireQualifiedAccess>]
module DeclarationRefusal =

    let describe (r: DeclarationRefusal) : string =
        match r with
        | UnknownInvariant(id, known) ->
            "no comparator declares the invariant '"
            + id
            + "' (declared: "
            + (if List.isEmpty known then
                   "none"
               else
                   String.concat ", " known)
            + ")"
        | UnjustifiedDivergence id ->
            "the declaration on '"
            + id
            + "' carries no justification — a divergence that is not argued for is not priced"

/// Forest-level reads and the invariant seam's evaluation host.
///
/// Every function here takes the siblings' folded records as an argument. Nothing is stored, no
/// record is fetched, and no function returns a changed exploration: a forest question is a
/// question, and the only way to change a sibling is the ordinary op path into that sibling.
[<RequireQualifiedAccess>]
module Forest =

    let empty (id: ForestId) (title: string) : Forest =
        { Id = id
          Title = title
          Siblings = []
          Policy = Policy.empty
          Thresholds = None }

    /// The forest's members, in declaration order — the deterministic order every projection
    /// and every query below iterates in.
    let siblings (f: Forest) : SiblingId list =
        f.Siblings |> List.map (fun s -> s.Sibling)

    let trySibling (id: SiblingId) (f: Forest) : SiblingRef option =
        f.Siblings |> List.tryFind (fun s -> s.Sibling = id)

    /// Every declaration standing anywhere in one sibling's record, state by state.
    let declarations (e: Exploration) : (StateId * DivergenceDeclaration) list =
        Query.states e
        |> List.collect (fun s -> Query.currentDeclarations s |> List.map (fun d -> s.Id, d))

    /// One row per member. A sibling the host did not supply is reported as absent rather than
    /// as empty — those are different facts and only one of them is about the exploration.
    let summarise (f: Forest) (records: Map<SiblingId, Exploration>) : SiblingSummary list =
        f.Siblings
        |> List.map (fun ref ->
            match Map.tryFind ref.Sibling records with
            | None ->
                { Sibling = ref.Sibling
                  Label = ref.Label
                  Present = false
                  States = 0
                  Kept = []
                  Killed = []
                  Uncurated = []
                  Frontier = []
                  Outcomes = 0
                  Declarations = [] }
            | Some e ->
                { Sibling = ref.Sibling
                  Label = ref.Label
                  Present = true
                  States = List.length e.Order
                  Kept = Query.keptStates e
                  Killed = Query.killedStates e
                  Uncurated = Query.uncuratedStates e
                  Frontier = Query.leaves e
                  Outcomes = Query.outcomes e |> List.length
                  Declarations = declarations e })

    // ---- the projection ------------------------------------------------------

    /// Which states of one exploration a selection names.
    let private selected (selection: StateSelection) (e: Exploration) : StateId list =
        match selection with
        | KeptStates -> Query.keptStates e
        | FrontierStates -> Query.leaves e
        | NamedStates ids -> ids |> List.filter (fun id -> Map.containsKey id e.States)

    /// The states a selection puts in front of the comparators, in forest order then
    /// exploration order. Deterministic, because a comparator that is shown the same states in
    /// a different order may honestly answer differently, and then nothing about the run
    /// reproduces.
    let project (selection: StateSelection) (f: Forest) (records: Map<SiblingId, Exploration>) : ProjectedState list =
        [ for ref in f.Siblings do
              match Map.tryFind ref.Sibling records with
              | None -> ()
              | Some e ->
                  for id in selected selection e do
                      match Map.tryFind id e.States with
                      | None -> ()
                      | Some st ->
                          yield
                              { Sibling = ref.Sibling
                                State = id
                                Payload = st.Payload } ]

    // ---- evaluation ----------------------------------------------------------

    /// The canonical order over findings: invariant, then the siblings named, then the detail.
    /// A comparator is not forced into an emission order; the seam still compares sequences.
    let private findingKey (x: InvariantFinding) : string * string * string =
        x.InvariantId, String.concat "," x.Siblings, x.Detail

    /// Run the comparators over a projection.
    ///
    /// The manager STAMPS each finding with the comparator's own id rather than trusting the
    /// one the comparator wrote. A finding attributed to an invariant the comparator does not
    /// implement would be unanswerable — a declaration naming that invariant would downgrade
    /// findings from a comparator nobody ran — so attribution is taken from the caller's side
    /// of the seam, where it is known.
    ///
    /// Sibling lists are de-duplicated and ordered, so two hosts running one comparator over
    /// one projection compare equal findings.
    let evaluate (comparators: InvariantComparator list) (projected: ProjectedState list) : InvariantFinding list =
        comparators
        |> List.collect (fun c ->
            c.Compare projected
            |> List.map (fun x ->
                { x with
                    InvariantId = c.Id
                    Siblings = x.Siblings |> List.distinct |> List.sort }))
        |> List.sortBy findingKey

    /// Read one finding against the declarations standing on the siblings it names.
    ///
    /// A finding is downgraded when ANY named sibling declared this invariant. That is not
    /// leniency: an invariant says these siblings must agree, and a declaration says this one
    /// is permitted to differ and here is why — so one accepted departure accounts for the
    /// disagreement it caused. A comparator wanting finer accounting emits finer findings, and
    /// `Undeclared` keeps the coarser case honest by naming who said nothing.
    let assessFinding (f: Forest) (records: Map<SiblingId, Exploration>) (finding: InvariantFinding) : AssessedFinding =
        let found =
            [ for sibling in finding.Siblings do
                  match Map.tryFind sibling records with
                  | None -> ()
                  | Some e ->
                      for st in Query.states e do
                          match Query.tryDeclaration finding.InvariantId st with
                          | Some d -> yield sibling, st.Id, d
                          | None -> () ]

        let declaring = found |> List.map (fun (s, _, _) -> s) |> Set.ofList

        { Finding = finding
          Standing = (if Set.isEmpty declaring then Unaccounted else Downgraded)
          Declarations = found
          Undeclared = finding.Siblings |> List.filter (fun s -> not (Set.contains s declaring)) }

    /// One pass of the invariant seam over a forest: project, evaluate, read the declarations.
    let assess
        (selection: StateSelection)
        (comparators: InvariantComparator list)
        (f: Forest)
        (records: Map<SiblingId, Exploration>)
        : ForestAssessment =
        let projected = project selection f records

        { Forest = f.Id
          Selection = selection
          Projected = List.length projected
          Invariants = comparators |> List.map (fun c -> c.Id)
          Findings = evaluate comparators projected |> List.map (assessFinding f records) }

    /// Findings nothing accounts for — what a reviewer is actually being asked to look at.
    let unaccounted (a: ForestAssessment) : AssessedFinding list =
        a.Findings |> List.filter (fun x -> x.Standing = Unaccounted)

    /// Findings a declaration downgraded. Still findings, still listed, still carrying the
    /// argument that was made for them.
    let downgraded (a: ForestAssessment) : AssessedFinding list =
        a.Findings |> List.filter (fun x -> x.Standing = Downgraded)

    /// Did this pass leave nothing unaccounted for? Deliberately not called "passed": an
    /// assessment with twenty downgraded findings is a forest with twenty priced divergences,
    /// and "clear" says the price was paid, not that there was nothing to pay.
    let clear (a: ForestAssessment) : bool = unaccounted a |> List.isEmpty

    // ---- recurrence ----------------------------------------------------------

    /// Every state id any supplied sibling explored, in forest-then-exploration order.
    let private allStateIds (f: Forest) (records: Map<SiblingId, Exploration>) : StateId list =
        [ for ref in f.Siblings do
              match Map.tryFind ref.Sibling records with
              | None -> ()
              | Some e -> yield! e.Order ]
        |> List.distinct

    /// How often each decision was chosen across the forest.
    ///
    /// Read the type's own documentation before reading a result: this counts what curators
    /// kept, over branches they chose to explore. It is a record of choices and says nothing
    /// about which choice was better.
    ///
    /// Ordered by how many siblings chose each decision, most-chosen first, ties broken by id
    /// so the order is total and reproducible. That ordering is a convenience for reading a
    /// long list, not a ranking of merit — the same caution, in the one place a reader is most
    /// likely to forget it.
    let chosenRecurrence (f: Forest) (records: Map<SiblingId, Exploration>) : ChosenRecurrence list =
        let present =
            f.Siblings
            |> List.choose (fun ref -> Map.tryFind ref.Sibling records |> Option.map (fun e -> ref.Sibling, e))

        allStateIds f records
        |> List.map (fun stateId ->
            let rows =
                present |> List.map (fun (sibling, e) -> sibling, Query.tryState stateId e)

            let label =
                rows
                |> List.tryPick (fun (_, st) -> st |> Option.map (fun s -> s.Label))
                |> Option.defaultValue ""

            let where (p: SpecState -> bool) =
                rows
                |> List.choose (fun (sibling, st) ->
                    match st with
                    | Some s when p s -> Some sibling
                    | _ -> None)

            { State = stateId
              Label = label
              ChosenIn = where (fun s -> Query.currentVerdict s = Some Keep)
              SetAsideIn = where Query.isKilled
              UncuratedIn = where (fun s -> (Query.currentVerdict s).IsNone)
              AbsentIn =
                rows
                |> List.choose (fun (sibling, st) -> if st.IsNone then Some sibling else None)
              DivergentIn = where (fun s -> not (List.isEmpty s.Divergences)) })
        |> List.sortBy (fun r -> -(List.length r.ChosenIn), r.State)

    /// One line per decision, in the vocabulary the type insists on. Written here rather than
    /// left to each caller because the phrasing is the contract: a caller that renders this
    /// itself is one edit away from writing "best".
    let describeRecurrence (total: int) (r: ChosenRecurrence) : string =
        let n = List.length r.ChosenIn

        "'"
        + r.State
        + "' was chosen in "
        + string n
        + " of "
        + string total
        + " siblings"
        + (if List.isEmpty r.SetAsideIn then
               ""
           else
               ", set aside in " + string (List.length r.SetAsideIn))
        + (if List.isEmpty r.DivergentIn then
               ""
           else
               ", with a declared divergence in " + string (List.length r.DivergentIn))
        + " — what was chosen, not what is better"

    // ---- the declaration gate ------------------------------------------------

    /// Build a divergence declaration op, refusing an invariant no comparator declares.
    ///
    /// The gate is here rather than in the fold because the fold cannot know the invariant
    /// vocabulary — it is dialect knowledge, exactly as a spec payload's interior is. Refusing
    /// an unknown id matters for one specific reason: a declaration is the only thing that
    /// downgrades a finding, so a declaration against a misspelled invariant silently protects
    /// nothing while reading, in the record, exactly like one that does.
    let declareDivergence
        (comparators: InvariantComparator list)
        (state: StateId)
        (declaration: DivergenceDeclaration)
        : Result<ExplorationOp, DeclarationRefusal> =
        let known = comparators |> List.map (fun c -> c.Id)

        if not (List.contains declaration.InvariantId known) then
            Error(UnknownInvariant(declaration.InvariantId, known))
        elif declaration.Justification.Trim() = "" then
            Error(UnjustifiedDivergence declaration.InvariantId)
        else
            Ok(
                Declare
                    { StateId = state
                      Declaration = declaration }
            )
