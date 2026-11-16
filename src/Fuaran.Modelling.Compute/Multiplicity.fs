// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Compute

open Fuaran.Modelling

// ============================================================================
//  Model multiplicity, as composite keys.
//
//  An indistinguishability set names the branches a record cannot tell apart on
//  one vintage under one declared metric and margin. A party that wants to show
//  the RANGE of answers across those branches — model uncertainty, beside the
//  prediction interval of the one it kept — has to re-score each member itself,
//  and what it needs for that is not the branch ids but the composite key each
//  member was fitted under: the join into whatever executed it.
//
//  The query lives with this dialect because a member is re-scorable by a Compute
//  executor only if its spec hash is a pipeline hash. A member fitted under any
//  other dialect is NAMED rather than dropped: a range taken over the members
//  that happened to be re-scorable would report a narrower family than the record
//  holds, which is exactly the understatement the set exists to prevent.
//
//  It is OFF unless a set was declared. No declaration, no question — and an
//  undeclared set answers `None`, never an empty list, for the reason
//  `Indistinguishability.members` gives.
// ============================================================================

/// One member of an indistinguishability set, as a re-scorer needs it.
type SetMemberKey =
    /// The member was fitted under this dialect: its key's spec hash is a pipeline hash, and
    /// the key names the fit whose reading put it in the set.
    | Rescorable of state: StateId * key: CompositeKey
    /// The member's specification is another dialect's, so no pipeline hash names it.
    | OtherDialect of state: StateId * dialect: string * key: CompositeKey

[<RequireQualifiedAccess>]
module SetMemberKey =

    let state (m: SetMemberKey) : StateId =
        match m with
        | Rescorable(s, _)
        | OtherDialect(s, _, _) -> s

    let key (m: SetMemberKey) : CompositeKey =
        match m with
        | Rescorable(_, k)
        | OtherDialect(_, _, k) -> k

/// The indistinguishability-set query.
[<RequireQualifiedAccess>]
module Multiplicity =

    /// The fit whose reading of the set's metric on the set's vintage put a branch in the set:
    /// the MOST RECENT outcome on that vintage carrying the metric, the same outcome
    /// `Vintage.statedReadingOn` reads, so the key and the reading are one fit's.
    let private readingFit (set: IndistinguishabilitySet) (s: SpecState) : OutcomeSnapshot option =
        Query.outcomesOn set.DatasetVersion s
        |> List.rev
        |> List.tryFind (fun o -> Map.containsKey set.Declaration.Metric o.Diagnostics)

    /// The members of a derived set as composite keys, the reference first and the rest in the
    /// set's own order.
    ///
    /// `None` exactly where `Indistinguishability.members` answers `None`: no declaration, no
    /// reference reading, or readings in more than one unit. Each of those is a reason there
    /// is no set, and a list here — even an empty one — would read as a set.
    let memberKeys (report: IndistinguishabilityReport) (e: Exploration) : SetMemberKey list option =
        match report with
        | NoDeclaration
        | NoReferenceReading _
        | MismatchedUnits _ -> None
        | Derived set ->
            set.Members
            |> List.choose (fun m ->
                Query.tryState m.State e
                |> Option.bind (fun s ->
                    readingFit set s
                    |> Option.map (fun o ->
                        if s.Payload.Dialect = ComputeDialect.dialectId then
                            Rescorable(m.State, o.Key)
                        else
                            OtherDialect(m.State, s.Payload.Dialect, o.Key))))
            |> Some

    /// The keys a Compute executor can re-score, or `None` where there is no set.
    let rescorable (report: IndistinguishabilityReport) (e: Exploration) : CompositeKey list option =
        memberKeys report e
        |> Option.map (
            List.choose (fun m ->
                match m with
                | Rescorable(_, k) -> Some k
                | OtherDialect _ -> None)
        )
