// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

open Fuaran.Core

// ============================================================================
//  The vintage axis — replaying an exploration onto new data, and reading what
//  the record then says about the DECISIONS.
//
//  The move this file exists for: when a new dataset vintage arrives, re-submit
//  the branches that were kept AND the branches that were killed, and let each
//  curation decision accumulate an out-of-time track record. It is available at
//  all only because an abandoned branch is retained — a killed branch is a
//  counterfactual somebody already paid to explore, and it is still here — and
//  it is worth having only because the signal is out-of-time, which is the one
//  kind of evidence that does not reintroduce the in-sample failure the whole
//  approach rejects.
//
//  Three disciplines run through everything below, and each is structural rather
//  than advisory:
//
//    * A replay moves ONE component of the composite key. Same specification,
//      same seed, same provider, new vintage — so the replayed fit is the same
//      decision asked again of different data, and `Replay.expectedKey` is a
//      record update on one field rather than a derivation anyone could get
//      subtly wrong.
//    * The BASELINE vintage does not count. It is the evidence the curation was
//      taken on; counting it toward a correction would be an in-sample argument
//      wearing an out-of-time label.
//    * Nothing here changes anything. Every function takes a folded record and
//      returns an answer. A correction is a FINDING — a question raised for a
//      person — and this library contains no operation that acts on one.
// ============================================================================

/// Which branches a replay re-submits.
///
/// Closed, and the default is the whole point.
type ReplaySelection =
    /// Every branch carrying at least one outcome — KEPT OR KILLED, and uncurated alike.
    ///
    /// The killed ones are not an afterthought; they are the reason the campaign is worth
    /// running. A branch that was abandoned is a testable counterfactual precisely because
    /// nothing deleted it, and a replay that quietly re-fitted only the survivors would
    /// gather exactly the evidence that cannot answer the question being asked.
    | EveryFittedBranch
    /// Named branches, by permanent id. A named branch with no prior fit is planned anyway
    /// and refused per-branch: a caller that named it is owed an answer about it.
    | NamedBranches of StateId list

[<RequireQualifiedAccess>]
module ReplaySelection =

    let encode (s: ReplaySelection) : string =
        match s with
        | EveryFittedBranch -> "everyFitted"
        | NamedBranches _ -> "named"

/// One branch's place in a replay: what to re-submit, and the fit it is a replay OF.
type ReplayItem =
    {
        State: StateId
        /// The composite key of the fit being replayed. The replayed fit's key is this one
        /// with `DatasetVersion` replaced and nothing else, which is what makes the two
        /// records the same decision asked twice rather than two unrelated fits.
        Baseline: CompositeKey
        Submission: FitSubmission
    }

/// What a replay campaign will attempt, and what it already knows it cannot.
type ReplayPlan =
    {
        ReplayId: ReplayId
        DatasetVersion: string
        Items: ReplayItem list
        /// Branches refused before anything was submitted — at present only a named branch
        /// that was never fitted. Carried in the plan rather than discovered later so that
        /// the receipt can account for every branch the caller asked about, including the
        /// ones no executor was ever asked about.
        Refused: (StateId * ReplayRefusal) list
    }

/// Building a replay, and the identity rule it holds.
///
/// Named `ReplayCampaign` rather than `Replay` because `Replay` already names the op case,
/// and a fully-qualified `Fuaran.Modelling.Replay.plan` resolves to the case constructor
/// rather than to this module. A collision that only bites callers who qualify is exactly the
/// kind that is discovered by somebody else, later.
[<RequireQualifiedAccess>]
module ReplayCampaign =

    /// The fit a branch's replays are replays OF: its first recorded outcome.
    ///
    /// First rather than latest, and that is the seed rule. A replay carries the baseline's
    /// seed VERBATIM — it does not derive one, and it does not take the seed of whichever
    /// replay happened to run last — so every fit in a branch's series differs from the
    /// original in the vintage alone. Deriving a fresh seed per vintage would make each
    /// replay a different fit that happens to share a specification, and the series would
    /// then be measuring the seed as much as the data.
    let baseline (s: SpecState) : OutcomeSnapshot option = List.tryHead s.Outcomes

    /// The key a replay of `item` onto `datasetVersion` must land under.
    ///
    /// Written as a one-field record update on purpose: the rule is "everything but the
    /// vintage is held", and a rule expressed as a construction of five components is one
    /// a later edit can weaken without anybody noticing which component moved.
    let expectedKey (datasetVersion: string) (item: ReplayItem) : CompositeKey =
        { item.Baseline with
            DatasetVersion = datasetVersion }

    /// Does this key differ from the baseline in the dataset version ALONE?
    let isReplayOf (baselineKey: CompositeKey) (key: CompositeKey) : bool =
        { baselineKey with
            DatasetVersion = key.DatasetVersion } = key

    /// Which branches a selection names, in exploration order.
    let selected (selection: ReplaySelection) (e: Exploration) : StateId list =
        match selection with
        | EveryFittedBranch -> Query.fittedStates e
        | NamedBranches ids -> ids |> List.filter (fun id -> Map.containsKey id e.States)

    /// Build the campaign: one submission per selected branch, against the new vintage.
    ///
    /// `gates` and `submittedBy` are the caller's, exactly as they are for an ordinary fit.
    /// The gates are worth a word: a replay asks the same questions of the new data that the
    /// original fit asked of the old, so passing the branch's original gate set is the
    /// ordinary thing to do — but the manager does not store gate REQUESTS (an outcome
    /// records the verdicts, which is a different record), so it cannot silently reconstruct
    /// them, and a caller that wants them supplies them.
    let plan
        (replayId: ReplayId)
        (selection: ReplaySelection)
        (datasetVersion: string)
        (gates: GateRequest list)
        (submittedBy: SubmitterClass)
        (e: Exploration)
        : ReplayPlan =
        let rows =
            selected selection e
            |> List.choose (fun id -> Query.tryState id e |> Option.map (fun s -> id, s))
            |> List.map (fun (id, s) ->
                match baseline s with
                | None -> Choice2Of2(id, NoBaselineFit)
                | Some b ->
                    Choice1Of2
                        { State = id
                          Baseline = b.Key
                          Submission =
                            { StateId = id
                              Payload = s.Payload
                              DatasetVersion = datasetVersion
                              Seed = b.Key.Seed
                              Gates = gates
                              SubmittedBy = submittedBy } })

        { ReplayId = replayId
          DatasetVersion = datasetVersion
          Items =
            rows
            |> List.choose (function
                | Choice1Of2 item -> Some item
                | _ -> None)
          Refused =
            rows
            |> List.choose (function
                | Choice2Of2 refusal -> Some refusal
                | _ -> None) }

    /// Every branch a plan is answerable for — what its receipt must account for exactly.
    let requested (p: ReplayPlan) : StateId list =
        (p.Items |> List.map (fun i -> i.State)) @ (p.Refused |> List.map fst)

// ---- the evidence base ------------------------------------------------------

/// One branch's readings of one diagnostic, vintage by vintage.
type VintageSeries =
    {
        State: StateId
        Metric: string
        /// `(datasetVersion, observed)` in the exploration's own vintage order. A vintage on
        /// which this branch has no reading is ABSENT rather than zero — a missing fit and a
        /// fit that measured zero are different facts, and only one of them is evidence.
        Readings: (string * float) list
        /// The unit each reading states, keyed by its vintage; a vintage with no entry states
        /// none (D40). Beside `Readings` rather than inside it, as `DiagnosticUnits` sits beside
        /// `Diagnostics`: a series a backend stated in grams on one vintage and kilograms on the
        /// next says so here, instead of reading as a thousandfold change.
        Units: Map<string, UnitOfMeasure>
    }

/// One vintage on which both branches of a decision carried a reading and at least one of the
/// two is stated in a unit other than the record's (D40). Never compared — the two numbers are
/// not in one unit — and named with the units found, because a comparison base that shrank is
/// a fact about the evidence.
type VintageUnits =
    {
        DatasetVersion: string
        /// The unit the kept branch's reading states; `None` states none.
        Kept: UnitOfMeasure option
        /// The unit the set-aside branch's reading states; `None` states none.
        SetAside: UnitOfMeasure option
    }

/// A read whose series states a unit other than the one its margin is declared in (D40).
///
/// A series that MIXES units for one metric — two different unit texts, or a stated unit
/// beside an unstated one — always lands here, whatever unit is declared, because at most one
/// of its units can be the declared one. Units are compared by canonical text and never
/// converted, as a gate's are (D39).
type SeriesUnitMismatch =
    {
        Metric: string
        /// The unit the margin is declared in; `None` declares none.
        Declared: UnitOfMeasure option
        /// Every unit the series' readings state, each once, ordered by canonical text.
        Stated: UnitOfMeasure list
        /// Does some reading in the series state no unit at all?
        Unstated: bool
    }

[<RequireQualifiedAccess>]
module SeriesUnits =

    let private text (u: UnitOfMeasure option) : string option = u |> Option.map Unit.render

    /// Do two stated units agree? By canonical text (§3.5 of the model-execution
    /// specification, D39), and `None` agrees only with `None`.
    let agree (a: UnitOfMeasure option) (b: UnitOfMeasure option) : bool = text a = text b

    /// The mismatch a series of stated units makes against a declared one, or `None` when
    /// every reading states exactly the declared unit — which includes an empty series.
    let mismatch
        (metric: string)
        (declared: UnitOfMeasure option)
        (found: UnitOfMeasure option list)
        : SeriesUnitMismatch option =
        if found |> List.forall (agree declared) then
            None
        else
            Some
                { Metric = metric
                  Declared = declared
                  Stated =
                    found
                    |> List.choose id
                    |> List.distinctBy Unit.render
                    |> List.sortWith (fun a b -> System.String.CompareOrdinal(Unit.render a, Unit.render b))
                  Unstated = found |> List.exists Option.isNone }

    let private orNone (u: UnitOfMeasure option) : string =
        match u with
        | Some u -> "'" + Unit.render u + "'"
        | None -> "no unit"

    /// The one sentence a mismatch may be rendered as.
    let describe (m: SeriesUnitMismatch) : string =
        let found =
            (m.Stated |> List.map (fun u -> "'" + Unit.render u + "'"))
            @ (if m.Unstated then [ "no unit" ] else [])

        "'"
        + m.Metric
        + "' is read here in "
        + String.concat ", " found
        + ", and the margin is declared in "
        + orNone m.Declared
        + " — units are compared by their text and never converted, so these readings are not compared"

/// One curation decision, as the pair it is actually about: a branch that was kept, and a
/// branch that was set aside.
type CurationDecision = { Kept: StateId; SetAside: StateId }

/// What the two read on one vintage.
type VintageComparison =
    {
        DatasetVersion: string
        KeptReading: float
        SetAsideReading: float
        /// How far ahead the SET-ASIDE branch read, in the metric's declared preferred
        /// direction. Negative when the kept branch read ahead — the ordinary case, and the
        /// one a record mostly contains.
        SetAsideLead: float
        /// Did this vintage arrive after the decision could have been taken?
        ///
        /// Only an out-of-time vintage counts toward a question about the curation. The
        /// baseline is the evidence the curation was made ON, so a lead there says the
        /// curator read their own data differently, not that later data disagreed — and
        /// counting it would be the in-sample argument this signal exists to avoid, made
        /// under the name of the one that avoids it.
        OutOfTime: bool
    }

/// The out-of-time track record of one curation decision.
type DecisionRecord =
    {
        Decision: CurationDecision
        Metric: string
        Prefer: MetricPreference
        /// The unit every comparison below is in — the thresholds' (D40).
        Unit: UnitOfMeasure option
        /// The first vintage on which both branches carried a reading — the earliest the
        /// decision could have been taken on comparable evidence, and therefore the line
        /// between in-sample and out-of-time for this pair.
        Baseline: string option
        Comparisons: VintageComparison list
        /// Vintages where only one of the two has a reading. Named rather than dropped: a
        /// comparison base that shrank is a fact about the evidence and not a detail, and a
        /// record that silently narrowed would read as agreement.
        Incomparable: string list
        /// Vintages where both have a reading and at least one is stated in a unit other than
        /// `Unit` (D40). Named with the units found and never compared, for the same reason:
        /// a lead taken between grams and kilograms, or measured against a margin in another
        /// unit, is a number about nothing.
        UnitMismatched: VintageUnits list
    }

/// A question raised for a person about a curation decision.
///
/// **This is a finding, and there is no operation in this library that acts on one.** The
/// human gate is structural rather than procedural: nothing here re-curates a branch,
/// nothing here proposes a branch, and a caller that wants either emits the ordinary op
/// itself, under the ordinary policy, attributed to whoever decided.
///
/// That is not caution for its own sake. An agent that re-curated on an early, low-powered
/// run would be thrashing its own policy to chase noise — which is the same failure as
/// fitting to in-sample idiosyncrasy, reached from the other direction, and it is the failure
/// the entire approach is a rejection of. So the accumulation is slow by declaration
/// (`CorrectionThresholds`), silent below the gate by construction, and a question rather
/// than an act above it.
type CorrectionFinding =
    {
        Decision: CurationDecision
        Metric: string
        /// The OUT-OF-TIME vintages on which the set-aside branch read ahead by at least the
        /// declared margin. Listed rather than counted, so a reviewer reads the evidence
        /// rather than a number standing in for it.
        LeadingVintages: string list
        /// The smallest of those leads — the weakest link in the run, which is what a
        /// reviewer asking "is this real?" wants first.
        SmallestLead: float
        /// Which siblings raised the same question. Empty for a single exploration, which is
        /// one sibling and says so rather than implying a breadth it does not have.
        RaisedIn: SiblingId list
        /// The thresholds in force when it was raised. A finding whose gate is invisible is
        /// one a reviewer cannot weigh.
        Thresholds: CorrectionThresholds
    }

/// A decision's recurrence across a forest, with the out-of-time record beside it.
///
/// The two are DELIBERATELY separate fields rather than one blended number. `Recurrence` is
/// what curators did — a record of choices, biased by the process it measures, and unable to
/// support a claim about which choice was better however many siblings it spans.
/// `Records` is what later data said, which is a different KIND of evidence entirely. The
/// answer to a count that cannot license a verdict is not to rename the count; it is to put
/// something beside it that can, and to keep the two distinguishable at a glance.
type RecurrenceWithPower =
    {
        Recurrence: ChosenRecurrence
        /// Each supplied sibling's record for decisions in which this state is the kept side.
        Records: (SiblingId * DecisionRecord) list
        /// The siblings whose record cleared the vintage and margin thresholds — the sibling
        /// axis of the accumulation, and the breadth a single unit's short, collinear series
        /// cannot supply on its own.
        RaisedIn: SiblingId list
    }

// ---- indistinguishability ---------------------------------------------------
//
//  The modelling problem is under-determined. The solution space is unbounded, there is no
//  calibration metric that settles it, and so there is no single right specification — which
//  is a fact about the problem rather than a shortfall in anybody's process. The honest
//  response is already in the record, because a branch that was set aside is retained: what
//  the exploration holds is not one answer but a family of them, and the question worth
//  asking is how large that family is.
//
//  So: given a declared margin on a declared metric, which branches does this record fail to
//  distinguish from a reference? That is the primitive a multiplicity report is built on —
//  the spread of a business quantity across near-equivalent specifications — and it is
//  computed offline over the folded record, exactly as every other read here is.
//
//  Three disciplines, and each is structural rather than advisory:
//
//    * The declaration has NO DEFAULT, and an undeclared margin yields an explicit
//      no-declaration state rather than an empty set. A margin this library invented would
//      be a judgement it cannot make, applied silently to every record that never thought
//      about it — the argument `CorrectionThresholds` makes, made again.
//    * Membership is SYMMETRIC and unordered. There is no preferred direction here and no
//      field for one: a direction is the first step to a ranking, and a set of specifications
//      nobody can tell apart is the one surface on which a ranking is most obviously wrong.
//    * Only genuine ALTERNATIVES are eligible — `Vintage.wereAlternatives`, the same rule the
//      decision pairs use. Two branches on unrelated lines were never weighed against each
//      other, and two readings that happen to fall close together are not a choice anybody
//      faced.

/// Which branches a declaration puts in front of the comparison.
///
/// Closed, and it NARROWS rather than widens: whatever a scope names, the alternatives rule
/// applies on top of it, so no scope can admit a branch the reference was never weighed
/// against.
type IndistinguishabilityScope =
    /// Every branch that was an alternative to the reference.
    | EveryAlternative
    /// Named branches, by permanent id — still filtered to the ones that were alternatives.
    /// The shape a caller uses when the question is about a specific short list somebody drew
    /// up, rather than about everything the record holds.
    | NamedAlternatives of StateId list

[<RequireQualifiedAccess>]
module IndistinguishabilityScope =

    let encode (s: IndistinguishabilityScope) : string =
        match s with
        | EveryAlternative -> "everyAlternative"
        | NamedAlternatives _ -> "named"

/// What "indistinguishable" means for one question: a metric, a margin, and a scope.
///
/// DECLARED DATA with no default, which is the whole of the guard. `Metric` is spelled as the
/// dialect spells it in diagnostics; `Margin` is in that metric's own units; `Scope` says which
/// branches are being asked about. Every one of the three is a house judgement — how close is
/// too close to call depends on the metric, the domain and what the answer will be used for —
/// and there is no value this library could supply for any of them that would not be an
/// invention presented as a convention.
///
/// Note what is ABSENT: there is no `MetricPreference` here, and its absence is deliberate.
/// Indistinguishability is symmetric — a branch reading further ahead of the reference is as
/// far from it as one reading further behind, and both are outside the margin at the same
/// distance. A preferred direction would let the surface say which way a difference ran, which
/// is the first sentence of a ranking.
type IndistinguishabilityDeclaration =
    {
        Metric: string
        Margin: float
        Scope: IndistinguishabilityScope
        /// The unit `Margin` is in, and the one unit a reading must state to be compared (D40).
        /// `None` states none, and compares only readings that state none.
        Unit: UnitOfMeasure option
    }

/// Why a declaration was not built.
///
/// Enumerable data naming the alternatives, like every refusal here (D8): each case carries
/// what IS declarable, because a caller that misspelled a metric or named a branch that is not
/// there can act on nothing else. The metric cases matter most — a metric nothing measured
/// derives a set with no members, which reads in a report exactly like a record whose branches
/// are all far apart.
type IndistinguishabilityRefusal =
    /// The declaration names no metric at all.
    | UndeclaredMetric of declarable: string list
    /// The declaration names a metric nothing in this record measured.
    | UnmeasuredMetric of metric: string * declarable: string list
    /// A margin below zero. Zero itself is legitimate and means what it says — only readings
    /// that agree exactly are indistinguishable — which is a strict question, not an empty one.
    | NegativeMargin of margin: float
    /// A scope names a branch this record does not hold.
    | UnknownScopeState of state: StateId * known: StateId list

[<RequireQualifiedAccess>]
module IndistinguishabilityRefusal =

    let private orNone (xs: string list) =
        if List.isEmpty xs then "none" else String.concat ", " xs

    let describe (r: IndistinguishabilityRefusal) : string =
        match r with
        | UndeclaredMetric declarable -> "no metric was declared (this record carries: " + orNone declarable + ")"
        | UnmeasuredMetric(metric, declarable) ->
            "nothing in this record measured '"
            + metric
            + "' (it carries: "
            + orNone declarable
            + ")"
        | NegativeMargin margin ->
            "a margin of "
            + string margin
            + " is below zero — a distance cannot be negative, and zero already means 'only an exact agreement'"
        | UnknownScopeState(state, known) ->
            "the scope names '"
            + state
            + "', which this record does not hold (it holds: "
            + orNone known
            + ")"

/// One branch the record does not distinguish from the reference, and by how little.
type IndistinguishabilityMember =
    {
        State: StateId
        /// This branch's reading of the declared metric on the vintage the set was read on.
        Reading: float
        /// How far that reading lies from the reference's, in the metric's own units.
        ///
        /// UNSIGNED, and that is the symmetry made concrete. A signed distance would say which
        /// side of the reference this branch fell on, and a reader sorting by it would have a
        /// ranking of specifications that nothing in the evidence licenses.
        Distance: float
    }

/// Which branches this record fails to distinguish from a reference, under one declaration.
type IndistinguishabilitySet =
    {
        Reference: StateId
        Declaration: IndistinguishabilityDeclaration
        /// The vintage every reading was taken on. ONE vintage for the whole set: comparing a
        /// branch's reading on one dataset with another's on a different one would be
        /// comparing two things that were never asked the same question.
        DatasetVersion: string
        /// Is this vintage later than the reference's own first reading of the metric?
        ///
        /// Carried rather than enforced. A set is not a question about a curation, so D32's
        /// exclusion does not apply — but a set read on the evidence the branches were curated
        /// on cannot carry the weight of one read on data that arrived afterwards, and a
        /// reader who cannot tell which they are holding will use them the same way. Refusing
        /// the in-sample case would be worse: a record with one vintage would then yield no
        /// set at all, which reads exactly like a record with no near-equivalents.
        OutOfTime: bool
        ReferenceReading: float
        /// The reference and every branch within the margin of it, the reference first and the
        /// rest in exploration order.
        ///
        /// The reference is a member of its own set — it is trivially within any margin of
        /// itself — and it heads the list because every distance below is a distance FROM it.
        /// A set that excluded it would report one member for a reference plus one, and the
        /// count that a multiplicity report is built on would be short by one everywhere.
        Members: IndistinguishabilityMember list
        /// Branches in scope carrying no reading of this metric on this vintage. Named rather
        /// than dropped: a branch nobody fitted on this data is not a branch that reads
        /// differently, and a set that silently narrowed to what happened to be measured would
        /// report a smaller family than the record can actually account for.
        Unread: StateId list
    }

/// What a set query answered.
///
/// Every case but `Derived` exists so that "nobody declared a margin", "there is nothing to
/// measure from" or "these numbers are not in one unit" can never read the same as "nothing
/// lies within the declared margin". An empty member list is a real answer
/// about the evidence; a missing declaration is the absence of a question, and a surface that
/// returned an empty list for both would report a decisive finding to every caller who forgot
/// to declare anything.
type IndistinguishabilityReport =
    /// No declaration was made. The no-default idiom, surfaced rather than filled in.
    | NoDeclaration
    /// The reference carries no reading of the declared metric on the vintage asked about, so
    /// there is no anchor to measure distances from. Distinct from an empty set for the same
    /// reason as above.
    | NoReferenceReading of state: StateId * metric: string
    /// The readings the set would be built from — the reference's and its alternatives', on
    /// the vintage asked about — are not all stated in the declaration's unit (D40). Refused
    /// whole rather than narrowed: a family with its other-unit members dropped reads as a
    /// smaller family, and multiplicity is the number a reader takes from it.
    | MismatchedUnits of SeriesUnitMismatch
    | Derived of IndistinguishabilitySet

/// How often one branch was indistinguishable from the reference, across a forest's siblings.
///
/// The 774 recurrence machinery, applied to the third quality axis, with the caution the other
/// two carry and one addition of its own. The five lists PARTITION the siblings a set was
/// derived in, so a state's absence from one list is always accounted for in another.
///
/// Every field names a RELATION between two branches, never a merit of either. "Indistinguishable
/// in seven of ten units" says seven units' evidence cannot tell them apart; it says nothing
/// whatever about which of the two anybody should use, and `Contract.indistinguishabilityVocabularyLaw`
/// holds the naming to it executably.
type IndistinguishabilityRecurrence =
    {
        State: StateId
        Label: string
        /// Siblings whose evidence does not distinguish it from the reference.
        IndistinguishableIn: SiblingId list
        /// Siblings whose evidence does — it was read, and it lies outside the margin.
        SeparatedIn: SiblingId list
        /// Siblings where it is in scope but carries no reading on the vintage read.
        UnreadIn: SiblingId list
        /// Siblings where it exists but was never an alternative to the reference, or the
        /// declared scope did not name it. Not a comparison that came out one way; a
        /// comparison nobody was entitled to make.
        OutOfScopeIn: SiblingId list
        /// Siblings whose exploration has no such branch at all.
        AbsentIn: SiblingId list
    }

/// A forest's answer to one reference's question: the per-sibling reports, and the recurrence
/// over the siblings that could answer.
type IndistinguishabilityForestView =
    {
        Reference: StateId
        /// One row per supplied sibling, INCLUDING the ones that derived nothing and why —
        /// so "this unit has no evidence on the reference" never reads as "this unit finds
        /// nothing indistinguishable".
        Reports: (SiblingId * IndistinguishabilityReport) list
        /// The siblings a set was actually derived in — the denominator every recurrence row
        /// below is read against, stated rather than left to be counted.
        DerivedIn: SiblingId list
        Recurrence: IndistinguishabilityRecurrence list
    }

/// Reads over the branch × vintage evidence base. Nothing here is stored and nothing here
/// changes a record.
[<RequireQualifiedAccess>]
module Vintage =

    /// Every dataset version this exploration has evidence on, in the order the record first
    /// saw each.
    ///
    /// History rather than an alphabet, for the same reason `Exploration.Order` is: the
    /// alternative — sorting the version strings — puts `ds-10` before `ds-2` and calls the
    /// result chronology. Every series and every comparison below is presented in this order,
    /// so two branches are always read against the same sequence.
    let order (e: Exploration) : string list =
        Query.outcomes e
        |> List.map (fun (_, o) -> o.Key.DatasetVersion)
        |> List.distinct

    /// One branch's readings of one diagnostic, keyed by vintage.
    ///
    /// A vintage the branch was fitted on but whose diagnostics do not carry this metric is
    /// absent, exactly as an unfitted vintage is. The manager enumerates no diagnostic
    /// vocabulary, so a metric it was not asked for is a metric that was not measured.
    ///
    /// Each reading with the unit the SAME outcome states for it (D40) — a unit read off one
    /// fit and a value off another would be a pairing nobody reported.
    let statedReadings (metric: string) (s: SpecState) : Map<string, float * UnitOfMeasure option> =
        s.Outcomes
        |> List.fold
            (fun acc o ->
                match Map.tryFind metric o.Diagnostics with
                | Some v -> Map.add o.Key.DatasetVersion (v, Map.tryFind metric o.DiagnosticUnits) acc
                | None -> acc)
            Map.empty

    /// One branch's readings of one diagnostic, keyed by vintage, without their units.
    let readings (metric: string) (s: SpecState) : Map<string, float> =
        statedReadings metric s |> Map.map (fun _ (v, _) -> v)

    /// One branch's series, in the exploration's vintage order, each reading's unit beside it.
    let series (metric: string) (state: StateId) (e: Exploration) : VintageSeries =
        let byVintage =
            Query.tryState state e
            |> Option.map (statedReadings metric)
            |> Option.defaultValue Map.empty

        let rows =
            order e
            |> List.choose (fun v -> Map.tryFind v byVintage |> Option.map (fun r -> v, r))

        { State = state
          Metric = metric
          Readings = rows |> List.map (fun (v, (x, _)) -> v, x)
          Units =
            rows
            |> List.choose (fun (v, (_, u)) -> u |> Option.map (fun u -> v, u))
            |> Map.ofList }

    /// Every diagnostic name anything in this record was measured on, sorted.
    ///
    /// The manager enumerates no diagnostic vocabulary — a dialect's names are a dialect's —
    /// so this is not a vocabulary but a CENSUS: what this particular record happens to carry.
    /// It exists so a refusal can enumerate the alternatives (D8): a caller that declared a
    /// metric nothing measured gets an empty answer that reads exactly like a record with
    /// nothing to say, and the only thing that distinguishes the two is being told which names
    /// are there.
    let metrics (e: Exploration) : string list =
        Query.outcomes e
        |> List.collect (fun (_, o) -> o.Diagnostics |> Map.toList |> List.map fst)
        |> List.distinct
        |> List.sort

    /// One branch's reading of one metric on ONE vintage, if it has one, with the unit that
    /// fit states for it (D40).
    ///
    /// The MOST RECENT fit on that vintage, matching `readings` above (whose fold lets a later
    /// outcome overwrite an earlier one). A branch re-fitted on the same vintage has said
    /// something newer about itself, and a comparison that took the older reading would answer
    /// a question about a fit somebody already superseded.
    let statedReadingOn
        (metric: string)
        (datasetVersion: string)
        (s: SpecState)
        : (float * UnitOfMeasure option) option =
        Query.outcomesOn datasetVersion s
        |> List.rev
        |> List.tryPick (fun o ->
            Map.tryFind metric o.Diagnostics
            |> Option.map (fun v -> v, Map.tryFind metric o.DiagnosticUnits))

    /// One branch's reading of one metric on one vintage, without its unit.
    let readingOn (metric: string) (datasetVersion: string) (s: SpecState) : float option =
        statedReadingOn metric datasetVersion s |> Option.map fst

    /// The vintage of one branch's most recent reading of one metric.
    let latestVintage (metric: string) (s: SpecState) : string option =
        s.Outcomes
        |> List.rev
        |> List.tryPick (fun o ->
            if Map.containsKey metric o.Diagnostics then
                Some o.Key.DatasetVersion
            else
                None)

    /// The vintage of one branch's FIRST reading of one metric — the earliest evidence any
    /// decision about it could have been taken on, and therefore its in-sample line.
    ///
    /// The single-branch form of the baseline `record` derives per pair (D32). It is the same
    /// rule read one branch at a time, which is what a question about a SET of branches needs:
    /// a set has no pair to take a baseline over.
    let baselineVintage (metric: string) (s: SpecState) : string option =
        s.Outcomes
        |> List.tryPick (fun o ->
            if Map.containsKey metric o.Diagnostics then
                Some o.Key.DatasetVersion
            else
                None)

    /// How far ahead the set-aside reading is, in the metric's preferred direction.
    let private lead (prefer: MetricPreference) (kept: float) (setAside: float) : float =
        match prefer with
        | PreferHigher -> setAside - kept
        | PreferLower -> kept - setAside

    /// Were these two branches ever alternatives to one another?
    ///
    /// A decision is a choice somebody actually made, so the pair has to be one somebody was
    /// choosing between: either one branch descends from the other (the work continued along
    /// one line and the other was abandoned) or the two share a parent (two ways of doing the
    /// same thing, tried side by side). Two branches on unrelated lines deep in a tree were
    /// never weighed against each other, and pairing them would manufacture decisions nobody
    /// took — hundreds of them, in an exploration of any size, each able to raise a question.
    ///
    /// PUBLIC rather than private since the indistinguishability surface below is restricted by
    /// exactly this rule and must be restricted by the SAME rule rather than by a second one
    /// written to match. Two copies of a discipline are two disciplines the moment either is
    /// edited, and this one is the difference between a set of branches somebody weighed and a
    /// list of branches that happen to read alike.
    let wereAlternatives (a: StateId) (b: StateId) (e: Exploration) : bool =
        let parentOf id =
            Query.tryState id e |> Option.bind (fun s -> s.Parent)

        (Query.path b e |> List.contains a)
        || (Query.path a e |> List.contains b)
        || (match parentOf a, parentOf b with
            | Some pa, Some pb -> pa = pb
            | _ -> false)

    /// Every curation decision the record holds: a kept branch and a set-aside branch that
    /// were alternatives, in exploration order on each side.
    let decisions (e: Exploration) : CurationDecision list =
        let kept = Query.keptStates e
        let killed = Query.killedStates e

        [ for k in kept do
              for s in killed do
                  if k <> s && wereAlternatives k s e then
                      yield { Kept = k; SetAside = s } ]

    /// One decision's out-of-time record, under the metric, preference and unit declared in
    /// `thresholds`. The thresholds' gate is NOT applied here — this is the evidence, and
    /// `corrections` is the reading of it.
    ///
    /// A vintage is compared only where both readings state the thresholds' unit (D40); one
    /// that does not is named in `UnitMismatched` with the units found. So a series that mixes
    /// units is reported vintage by vintage rather than refused whole: a record spans vintages,
    /// and a backend that began stating units part-way through still has comparable evidence
    /// on every vintage stated in one unit.
    let record (thresholds: CorrectionThresholds) (decision: CurationDecision) (e: Exploration) : DecisionRecord =
        let readingsOf id =
            Query.tryState id e
            |> Option.map (statedReadings thresholds.Metric)
            |> Option.defaultValue Map.empty

        let keptReadings = readingsOf decision.Kept
        let setAsideReadings = readingsOf decision.SetAside
        let vintages = order e

        let both =
            vintages
            |> List.choose (fun v ->
                match Map.tryFind v keptReadings, Map.tryFind v setAsideReadings with
                | Some k, Some s -> Some(v, k, s)
                | _ -> None)

        // The baseline is a line in TIME — the first evidence the decision could have been
        // taken on — so it is drawn before units are read. A first vintage in another unit is
        // still the evidence the curation was made on, and moving the line past it would
        // count in-sample evidence as out-of-time.
        let baselineVintage = both |> List.tryHead |> Option.map (fun (v, _, _) -> v)

        let inUnit (_, (_, ku), (_, su)) =
            SeriesUnits.agree thresholds.Unit ku && SeriesUnits.agree thresholds.Unit su

        { Decision = decision
          Metric = thresholds.Metric
          Prefer = thresholds.Prefer
          Unit = thresholds.Unit
          Baseline = baselineVintage
          Comparisons =
            both
            |> List.filter inUnit
            |> List.map (fun (v, (k, _), (s, _)) ->
                { DatasetVersion = v
                  KeptReading = k
                  SetAsideReading = s
                  SetAsideLead = lead thresholds.Prefer k s
                  OutOfTime = Some v <> baselineVintage })
          Incomparable =
            vintages
            |> List.filter (fun v -> Map.containsKey v keptReadings <> Map.containsKey v setAsideReadings)
          UnitMismatched =
            both
            |> List.filter (inUnit >> not)
            |> List.map (fun (v, (_, ku), (_, su)) ->
                { DatasetVersion = v
                  Kept = ku
                  SetAside = su }) }

    /// Every decision's record.
    let records (thresholds: CorrectionThresholds) (e: Exploration) : DecisionRecord list =
        decisions e |> List.map (fun d -> record thresholds d e)

    /// The out-of-time vintages on which the set-aside branch read ahead by at least the
    /// declared margin.
    let leadingVintages (thresholds: CorrectionThresholds) (r: DecisionRecord) : VintageComparison list =
        r.Comparisons
        |> List.filter (fun c -> c.OutOfTime && c.SetAsideLead >= thresholds.MinMargin)

    /// Raise a question about one record, or say nothing.
    ///
    /// `siblings` is who is raising it — a single exploration passes its own id, or nothing,
    /// and gets a finding that honestly names one sibling. The sibling gate is applied here
    /// rather than only in the forest roll-up: skipping it on the cheaper read would let the
    /// cheaper read bypass a gate the expensive one enforces, which is the way a declared
    /// threshold becomes a decorative one.
    let question
        (thresholds: CorrectionThresholds)
        (siblings: SiblingId list)
        (r: DecisionRecord)
        : CorrectionFinding option =
        let leading = leadingVintages thresholds r

        // A declared threshold of zero is not a threshold. Honouring it literally would open
        // the gate completely, which is the one reading nobody can have intended.
        let vintageGate = max 1 thresholds.MinVintages
        let siblingGate = max 1 thresholds.MinSiblings
        let raisedIn = siblings |> List.distinct |> List.sort

        if
            List.length leading >= vintageGate
            && max 1 (List.length raisedIn) >= siblingGate
        then
            Some
                { Decision = r.Decision
                  Metric = r.Metric
                  LeadingVintages = leading |> List.map (fun c -> c.DatasetVersion)
                  SmallestLead = leading |> List.map (fun c -> c.SetAsideLead) |> List.min
                  RaisedIn = raisedIn
                  Thresholds = thresholds }
        else
            None

    /// Every question one exploration's evidence raises.
    ///
    /// One exploration is ONE sibling, so a `MinSiblings` above 1 silences it entirely — the
    /// honest answer, since the breadth axis of the accumulation is exactly what a single
    /// unit does not have.
    let corrections (thresholds: CorrectionThresholds) (e: Exploration) : CorrectionFinding list =
        records thresholds e |> List.choose (question thresholds [])

    /// The one sentence this finding may be rendered as.
    ///
    /// In the library rather than left to each caller, for the reason `describeRecurrence` is:
    /// the phrasing is part of the contract. A caller writing this line itself is one edit
    /// from "the curation was wrong" — which is a verdict this evidence cannot deliver and an
    /// instruction this library will not carry out.
    let describeCorrection (f: CorrectionFinding) : string =
        "the branch set aside in favour of '"
        + f.Decision.Kept
        + "' — '"
        + f.Decision.SetAside
        + "' — read ahead on "
        + string (List.length f.LeadingVintages)
        + " out-of-time vintage(s) of '"
        + f.Metric
        + "', by at least "
        + string f.SmallestLead
        + (match f.Thresholds.Unit with
           | Some u -> " " + Unit.render u
           | None -> "")
        + (match f.RaisedIn with
           | [] -> ""
           | siblings -> ", in " + string (List.length siblings) + " sibling(s)")
        + " — raised for review. No curation has been changed, and nothing here will change one."

    // ---- the forest roll-up --------------------------------------------------

    /// Every question a forest's evidence raises, across its siblings.
    ///
    /// Composes the single-sibling read: each sibling's record for a decision is gathered,
    /// the sibling raises it or does not on its own evidence, and the forest applies the
    /// sibling gate over those. Breadth buys the power depth cannot — a decision validated
    /// out-of-time across many units is a verdict no single short, collinear series can
    /// produce — and this is the shape of that, with the caution's second axis as the count.
    ///
    /// A forest that declares no thresholds raises nothing. That is not an empty result to be
    /// filled in later; it is the gate being shut.
    let forestCorrections (f: Forest) (siblingRecords: Map<SiblingId, Exploration>) : CorrectionFinding list =
        match f.Thresholds with
        | None -> []
        | Some thresholds ->
            let present =
                f.Siblings
                |> List.choose (fun ref ->
                    Map.tryFind ref.Sibling siblingRecords |> Option.map (fun e -> ref.Sibling, e))

            let perDecision =
                [ for sibling, e in present do
                      for r in records thresholds e do
                          yield r.Decision, (sibling, r) ]

            perDecision
            |> List.map fst
            |> List.distinct
            |> List.choose (fun decision ->
                let rows = perDecision |> List.filter (fun (d, _) -> d = decision) |> List.map snd

                let raising =
                    rows
                    |> List.filter (fun (_, r) ->
                        List.length (leadingVintages thresholds r) >= max 1 thresholds.MinVintages)
                    |> List.map fst

                // The question is asked once of the whole forest, naming every sibling that
                // raised it — so the sibling gate reads the breadth rather than the depth,
                // which is the axis a single unit's series cannot supply.
                match raising with
                | [] -> None
                | first :: _ ->
                    rows
                    |> List.tryPick (fun (sibling, r) -> if sibling = first then Some r else None)
                    |> Option.bind (question thresholds raising))

    /// A forest's recurrence, with each decision's out-of-time record beside it.
    let recurrenceWithPower (f: Forest) (siblingRecords: Map<SiblingId, Exploration>) : RecurrenceWithPower list =
        let present =
            f.Siblings
            |> List.choose (fun ref -> Map.tryFind ref.Sibling siblingRecords |> Option.map (fun e -> ref.Sibling, e))

        let rowsFor (state: StateId) (t: CorrectionThresholds) =
            [ for sibling, e in present do
                  for r in records t e do
                      if r.Decision.Kept = state then
                          yield sibling, r ]

        Forest.chosenRecurrence f siblingRecords
        |> List.map (fun r ->
            match f.Thresholds with
            | None ->
                { Recurrence = r
                  Records = []
                  RaisedIn = [] }
            | Some t ->
                let rows = rowsFor r.State t

                { Recurrence = r
                  Records = rows
                  RaisedIn =
                    rows
                    |> List.filter (fun (_, row) -> List.length (leadingVintages t row) >= max 1 t.MinVintages)
                    |> List.map fst
                    |> List.distinct })

/// The indistinguishability surface: which branches a record fails to tell apart, under a
/// declared margin.
///
/// Nothing here is stored, nothing here changes a record, and nothing here orders anything.
/// Every function takes a folded exploration — or a forest's folded siblings — and returns an
/// answer computed offline over the evidence the record already holds (D4/D7).
[<RequireQualifiedAccess>]
module Indistinguishability =

    // ---- the declaration gate ------------------------------------------------

    /// Build a declaration, refusing one this record cannot answer.
    ///
    /// The gate is HERE rather than at the point of use, for the reason the invariant and
    /// assent gates are outside the fold: a malformed declaration derives a set that folds
    /// perfectly, reads exactly like a real answer, and answers nothing. A metric nothing
    /// measured is the sharp case — it yields a set of one, the reference alone, which is
    /// precisely the shape a genuine "nothing else is close" answer takes.
    ///
    /// It is a `Result` rather than a total constructor because the alternatives are what a
    /// caller needs: a metric name is a dialect's, and a caller that got it wrong cannot guess
    /// its way to the right one.
    let declareIn
        (unit: UnitOfMeasure option)
        (e: Exploration)
        (metric: string)
        (margin: float)
        (scope: IndistinguishabilityScope)
        : Result<IndistinguishabilityDeclaration, IndistinguishabilityRefusal> =
        let declarable = Vintage.metrics e

        let unknownScopeState =
            match scope with
            | EveryAlternative -> None
            | NamedAlternatives ids -> ids |> List.tryFind (fun id -> not (Map.containsKey id e.States))

        if metric.Trim() = "" then
            Error(UndeclaredMetric declarable)
        elif not (List.contains metric declarable) then
            Error(UnmeasuredMetric(metric, declarable))
        elif margin < 0.0 then
            Error(NegativeMargin margin)
        else
            match unknownScopeState with
            | Some id -> Error(UnknownScopeState(id, e.Order))
            | None ->
                Ok
                    { Metric = metric
                      Margin = margin
                      Scope = scope
                      Unit = unit }

    /// `declareIn` with a margin that states no unit — the declaration every record took
    /// before units existed, and the one that compares only readings stating none (D40).
    let declare
        (e: Exploration)
        (metric: string)
        (margin: float)
        (scope: IndistinguishabilityScope)
        : Result<IndistinguishabilityDeclaration, IndistinguishabilityRefusal> =
        declareIn None e metric margin scope

    // ---- the derived set -----------------------------------------------------

    /// The branches a declaration puts in front of the comparison, in exploration order.
    ///
    /// Always exploration order, whatever order a `NamedAlternatives` scope listed them in, so
    /// two hosts asking one question of one record enumerate the answer identically. The
    /// reference is not among them: it is the anchor, and it joins its own set at the head.
    let scoped (declaration: IndistinguishabilityDeclaration) (reference: StateId) (e: Exploration) : StateId list =
        let named =
            match declaration.Scope with
            | EveryAlternative -> None
            | NamedAlternatives ids -> Some(Set.ofList ids)

        e.Order
        |> List.filter (fun id ->
            id <> reference
            && Map.containsKey id e.States
            && (match named with
                | None -> true
                | Some set -> Set.contains id set)
            && Vintage.wereAlternatives reference id e)

    /// The set read on ONE named vintage, or the reason there is none.
    ///
    /// `declaration` is an option and the `None` arm is the point of the signature: an
    /// undeclared margin returns `NoDeclaration`, which a caller must handle as its own case.
    /// Taking a bare declaration and leaving callers to decide what to do without one is how a
    /// missing declaration becomes an empty list somewhere downstream.
    ///
    /// The readings compared — the reference's and every scoped alternative's on this vintage —
    /// must all state the declaration's unit; otherwise the answer is `MismatchedUnits`, naming
    /// the metric, the declared unit and the units found (D40). An alternative with no reading
    /// states nothing, and stays `Unread`.
    let reportOn
        (declaration: IndistinguishabilityDeclaration option)
        (datasetVersion: string)
        (reference: StateId)
        (e: Exploration)
        : IndistinguishabilityReport =
        match declaration with
        | None -> NoDeclaration
        | Some d ->
            let referenceState = Query.tryState reference e

            match referenceState |> Option.bind (Vintage.statedReadingOn d.Metric datasetVersion) with
            | None -> NoReferenceReading(reference, d.Metric)
            | Some(anchor, anchorUnit) ->
                let stated =
                    scoped d reference e
                    |> List.map (fun id ->
                        id,
                        Query.tryState id e
                        |> Option.bind (Vintage.statedReadingOn d.Metric datasetVersion))

                let found = anchorUnit :: (stated |> List.choose (snd >> Option.map snd))

                match SeriesUnits.mismatch d.Metric d.Unit found with
                | Some m -> MismatchedUnits m
                | None ->
                    let rows = stated |> List.map (fun (id, r) -> id, r |> Option.map fst)

                    let within =
                        rows
                        |> List.choose (fun (id, reading) ->
                            reading
                            |> Option.bind (fun x ->
                                let distance = abs (x - anchor)

                                if distance <= d.Margin then
                                    Some
                                        { State = id
                                          Reading = x
                                          Distance = distance }
                                else
                                    None))

                    Derived
                        { Reference = reference
                          Declaration = d
                          DatasetVersion = datasetVersion
                          OutOfTime =
                            (referenceState |> Option.bind (Vintage.baselineVintage d.Metric))
                            <> Some datasetVersion
                          ReferenceReading = anchor
                          Members =
                            { State = reference
                              Reading = anchor
                              Distance = 0.0 }
                            :: within
                          Unread =
                            rows
                            |> List.choose (fun (id, reading) -> if reading.IsNone then Some id else None) }

    /// The set read on the reference's most recent vintage for the declared metric.
    ///
    /// The ordinary call: "where does this stand on the newest data anybody fitted it on". The
    /// vintage is taken from the REFERENCE rather than from the record as a whole, so the
    /// comparison is anchored to a branch that actually has a reading rather than to the
    /// newest vintage in the exploration, which the reference may never have been fitted on.
    let report
        (declaration: IndistinguishabilityDeclaration option)
        (reference: StateId)
        (e: Exploration)
        : IndistinguishabilityReport =
        match declaration with
        | None -> NoDeclaration
        | Some d ->
            match Query.tryState reference e |> Option.bind (Vintage.latestVintage d.Metric) with
            | None -> NoReferenceReading(reference, d.Metric)
            | Some v -> reportOn declaration v reference e

    /// The member ids of a report, or nothing at all.
    ///
    /// `None` for both non-derived cases, so a caller cannot reach a list without having
    /// passed through the distinction. Returning `[]` here would reintroduce, in one
    /// convenience function, exactly the collapse `IndistinguishabilityReport` exists to
    /// prevent.
    let members (r: IndistinguishabilityReport) : StateId list option =
        match r with
        | NoDeclaration
        | NoReferenceReading _
        | MismatchedUnits _ -> None
        | Derived s -> Some(s.Members |> List.map (fun m -> m.State))

    /// How many branches this record cannot tell apart, the reference included.
    let multiplicity (s: IndistinguishabilitySet) : int = List.length s.Members

    // ---- the rendered line ---------------------------------------------------

    /// The one sentence a set may be rendered as.
    ///
    /// In the library rather than left to each caller, for the reason `describeRecurrence`,
    /// `describeCorrection` and `describeStanding` are: the phrasing is part of the contract.
    /// A caller writing this line itself is one edit from "and 'b' is the best of them" — which
    /// is the single claim a set of branches nobody can tell apart is least able to support,
    /// and the most natural sentence to write next.
    let describeSet (s: IndistinguishabilitySet) : string =
        let others = List.length s.Members - 1

        "'"
        + s.Reference
        + "' and "
        + string others
        + " other branch(es) are indistinguishable under the declared margin of "
        + string s.Declaration.Margin
        + (match s.Declaration.Unit with
           | Some u -> " " + Unit.render u
           | None -> "")
        + " on '"
        + s.Declaration.Metric
        + "', read on vintage '"
        + s.DatasetVersion
        + "' ("
        + (if s.OutOfTime then
               "out-of-time"
           else
               "the reference's own baseline, so in-sample")
        + ")"
        + (if List.isEmpty s.Unread then
               ""
           else
               ", with " + string (List.length s.Unread) + " alternative(s) unread on it")
        + " — what is indistinguishable, not what is better. Nothing here orders them."

    // ---- the forest roll-up --------------------------------------------------

    /// The siblings a forest holds a record for, in declaration order.
    let private present (f: Forest) (records: Map<SiblingId, Exploration>) : (SiblingId * Exploration) list =
        f.Siblings
        |> List.choose (fun ref -> Map.tryFind ref.Sibling records |> Option.map (fun e -> ref.Sibling, e))

    /// One reference's question asked of every supplied sibling, on each sibling's own latest
    /// vintage for the metric.
    ///
    /// EACH sibling's own vintage, deliberately. Units refresh on their own cadences, and
    /// forcing one vintage across a forest would silently drop every sibling that had not been
    /// fitted on it — turning "we have not re-run that unit yet" into "that unit finds nothing
    /// indistinguishable", which is the one confusion the report type exists to prevent.
    let forestReports
        (declaration: IndistinguishabilityDeclaration option)
        (reference: StateId)
        (f: Forest)
        (records: Map<SiblingId, Exploration>)
        : (SiblingId * IndistinguishabilityReport) list =
        present f records
        |> List.map (fun (sibling, e) -> sibling, report declaration reference e)

    /// A forest's whole answer: the per-sibling reports, the siblings that could answer, and
    /// the recurring-membership view over those.
    ///
    /// The recurrence rows are in forest-then-exploration order and are DELIBERATELY NOT
    /// sorted by how often a branch was indistinguishable — which is where `chosenRecurrence`
    /// and `AssentRecurrence` do sort, and where this one must not. A list ordered by a count
    /// is read as a ranking by everyone who did not write it, and on the one surface whose
    /// entire content is "the evidence cannot tell these apart", a ranking is not merely
    /// unsupported but self-contradicting.
    let forestView
        (declaration: IndistinguishabilityDeclaration option)
        (reference: StateId)
        (f: Forest)
        (records: Map<SiblingId, Exploration>)
        : IndistinguishabilityForestView =
        let reports = forestReports declaration reference f records

        let derived =
            [ for sibling, r in reports do
                  match r with
                  | Derived s ->
                      match Map.tryFind sibling records with
                      | Some e -> yield sibling, s, e
                      | None -> ()
                  | NoDeclaration
                  | NoReferenceReading _
                  | MismatchedUnits _ -> () ]

        let stateIds = [ for _, _, e in derived -> e.Order ] |> List.concat |> List.distinct

        { Reference = reference
          Reports = reports
          DerivedIn = derived |> List.map (fun (sibling, _, _) -> sibling)
          Recurrence =
            stateIds
            |> List.map (fun stateId ->
                let label =
                    derived
                    |> List.tryPick (fun (_, _, e) -> Query.tryState stateId e |> Option.map (fun s -> s.Label))
                    |> Option.defaultValue ""

                let where (p: IndistinguishabilitySet -> Exploration -> bool) =
                    derived
                    |> List.choose (fun (sibling, s, e) -> if p s e then Some sibling else None)

                let isMember (s: IndistinguishabilitySet) =
                    s.Members |> List.exists (fun m -> m.State = stateId)

                let inScope (s: IndistinguishabilitySet) (e: Exploration) =
                    stateId <> s.Reference
                    && scoped s.Declaration s.Reference e |> List.contains stateId

                { State = stateId
                  Label = label
                  IndistinguishableIn = where (fun s _ -> isMember s)
                  SeparatedIn =
                    where (fun s e -> not (isMember s) && inScope s e && not (List.contains stateId s.Unread))
                  UnreadIn = where (fun s _ -> List.contains stateId s.Unread)
                  OutOfScopeIn =
                    where (fun s e ->
                        Query.tryState stateId e |> Option.isSome
                        && not (isMember s)
                        && not (inScope s e))
                  AbsentIn = where (fun _ e -> (Query.tryState stateId e).IsNone) }) }
