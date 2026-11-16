// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling

// ============================================================================
//  Objectives, directives and policy — the process half of governance.
//
//  DEFAULT-DENY. A subject may do exactly what some active objective grants it
//  and nothing else; there is no wildcard, no implicit owner, no "unless
//  denied" branch. That asymmetry is the point: the expensive failure is an
//  automated explorer quietly doing something nobody authorised, and it can
//  only be prevented by a table that has to be extended to say yes.
//
//  This layer governs WHICH MOVES may be made, and by whom. It does not govern
//  cost: quota and budget belong to whatever executes the fit and are enforced
//  there, on every submitter alike. A governed automated exploration passes
//  through both gates; neither implements the other's.
// ============================================================================

/// What a subject may attempt.
type Capability =
    /// Emit one kind of exploration op.
    | MayEmit of OpKind
    /// Submit a fit for execution. Separate from `MayEmit AnnotateKind` on purpose:
    /// recording an outcome someone else obtained and spending real compute to obtain a
    /// new one are different acts with different blast radii.
    | MaySubmitFit
    /// Record an assent IN THE NAME OF one declared party class. Separate from
    /// `MayEmit AssentKind` for the reason `MaySubmitFit` is separate from
    /// `MayEmit AnnotateKind`: "may record assent at all" and "may speak as the client
    /// side" are different acts, and collapsing them would let anybody trusted to file the
    /// modeller's endorsement also file the reviewer's.
    ///
    /// Per CLASS rather than per subject-and-class pair, because the pairing is what a
    /// directive already is: the subject is named by the directive that carries this, so a
    /// grant here says which voices that subject may use.
    | MayAssentAs of PartyClassId

[<RequireQualifiedAccess>]
module Capability =

    let encode (c: Capability) : string =
        match c with
        | MayEmit k -> "emit:" + OpKind.encode k
        | MaySubmitFit -> "submitFit"
        | MayAssentAs party -> "assentAs:" + party

    let tryParse (s: string) : Result<Capability, string> =
        if s = "submitFit" then
            Ok MaySubmitFit
        elif s.StartsWith "emit:" then
            OpKind.tryParse (s.Substring 5) |> Result.map MayEmit
        elif s.StartsWith "assentAs:" then
            // A party class is an open registered string, exactly as a dialect id is, so
            // there is no vocabulary to parse it against here. Whether the class is DECLARED
            // is a different question, asked by `PartyAssent.record` against the policy's own
            // declarations — a grant naming a class nobody declared is inert rather than
            // malformed, and the two failures deserve different answers.
            Ok(MayAssentAs(s.Substring 9))
        else
            Error(
                "unknown capability '"
                + s
                + "' (expected submitFit | emit:<opKind> | assentAs:<partyClass>)"
            )

/// A grant of capabilities to one named subject. `Subject` is an actor id, matched
/// exactly — there is deliberately no pattern language, because a pattern is how a
/// default-deny table quietly becomes a default-allow one.
type Directive =
    { Subject: string
      Capabilities: Set<Capability> }

/// A named goal, and the grants that hold while pursuing it. Retiring an objective
/// withdraws its grants in one act, which is the reason grants hang off objectives
/// rather than sitting loose in a table.
type Objective =
    { Id: string
      Title: string
      Active: bool
      Directives: Directive list }

/// The governing set of objectives, and the party classes this process recognises.
///
/// The parties sit HERE rather than in a record because they are process data, not decision
/// data: a forest holds one policy applied independently to each sibling (D26), so declaring
/// them here is what makes "every declared party" mean the same thing across a whole forest
/// and across every exploration governed by it. Recorded per exploration they would drift,
/// and the denominator of a consensus question would then be a fact about one branch's
/// history rather than about the process being run.
type Policy =
    {
        Objectives: Objective list
        /// Declared or absent; there is no third state. A policy that declares no party class
        /// reaches no consensus on anything, ever — the same shape as a forest declaring no
        /// correction thresholds, and for the same reason: a default here would be this
        /// library inventing whose agreement counts.
        Parties: PartyClass list
    }

/// A refusal that names what was attempted and enumerates what the subject may do
/// instead — the recovery-envelope shape. A bare "denied" tells a caller nothing it
/// can act on.
type PolicyRefusal =
    { Subject: string
      Attempted: string
      Reason: string
      Allowed: string list }

[<RequireQualifiedAccess>]
module Policy =

    /// The empty policy grants nothing to anybody and declares no party class. This is the
    /// correct starting point, not a degenerate one.
    let empty: Policy = { Objectives = []; Parties = [] }

    /// The party classes this policy declares, by id, in declaration order — the set every
    /// consensus question is quantified over, and the set a refusal enumerates.
    let parties (p: Policy) : PartyClassId list = p.Parties |> List.map (fun c -> c.Id)

    /// Declare the party classes. Replaces the declaration wholesale rather than adding to
    /// it: the declared set is the denominator of every consensus question, so a function
    /// that quietly grew it would move the answer to questions nobody re-asked.
    let declaring (classes: PartyClass list) (p: Policy) : Policy = { p with Parties = classes }

    /// Every capability granted to `subject` by the ACTIVE objectives, unioned.
    let granted (subject: string) (p: Policy) : Set<Capability> =
        p.Objectives
        |> List.filter (fun o -> o.Active)
        |> List.collect (fun o -> o.Directives)
        |> List.filter (fun d -> d.Subject = subject)
        |> List.fold (fun acc d -> Set.union acc d.Capabilities) Set.empty

    /// The granted capabilities as sorted wire strings — what a refusal enumerates.
    let allowed (subject: string) (p: Policy) : string list =
        granted subject p |> Set.toList |> List.map Capability.encode |> List.sort

    /// The gate. `Ok ()` only when an active objective explicitly granted this exact
    /// capability to this exact subject.
    let authorise (p: Policy) (subject: string) (cap: Capability) : Result<unit, PolicyRefusal> =
        if Set.contains cap (granted subject p) then
            Ok()
        else
            Error
                { Subject = subject
                  Attempted = Capability.encode cap
                  Reason = "default-deny: no active objective grants this capability to this subject"
                  Allowed = allowed subject p }

    /// Authorise an op by its kind.
    let authoriseOp (p: Policy) (subject: string) (op: ExplorationOp) : Result<unit, PolicyRefusal> =
        authorise p subject (MayEmit(Op.kind op))

    /// Authorise a fit submission.
    let authoriseFit (p: Policy) (subject: string) : Result<unit, PolicyRefusal> = authorise p subject MaySubmitFit

    /// Authorise speaking in one party class's name.
    let authoriseAssent (p: Policy) (subject: string) (party: PartyClassId) : Result<unit, PolicyRefusal> =
        authorise p subject (MayAssentAs party)

    /// Convenience: one objective granting one subject a capability set.
    let objective (id: string) (title: string) (subject: string) (caps: Capability list) : Objective =
        { Id = id
          Title = title
          Active = true
          Directives =
            [ { Subject = subject
                Capabilities = Set.ofList caps } ] }

    /// The propose-only capability set: an automated explorer may table proposals and
    /// return to earlier states to think, but may not fork, curate, cherry-pick or spend
    /// compute. The human resolves. This is the shape the autonomy curve is measured on.
    let proposeOnlyCapabilities: Capability list =
        [ MayEmit ProposeKind; MayEmit ReturnKind ]

    /// Deactivate an objective by id. Absent id — no change; withdrawing a grant that was
    /// never made is not an error.
    let retire (id: string) (p: Policy) : Policy =
        { p with
            Objectives =
                p.Objectives
                |> List.map (fun o -> if o.Id = id then { o with Active = false } else o) }
