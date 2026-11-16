// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.DialectSeam

open Fuaran.Core
open Fuaran.Modelling

// ============================================================================
//  The reference dialect.
//
//  A dialect exists to be the thing the seam does not know about, so a seam with
//  exactly one dialect is a seam nobody has tested. This is the second one, and
//  it exists ONLY to bind the contract: every obligation implemented in the
//  smallest way that is still honest, so that a change to the seam breaks
//  something here before it breaks anyone's real dialect.
//
//  It is NOT statistics. It describes a linear form — a response, some named
//  terms, an optional bound per term — and it estimates nothing, fits nothing,
//  and computes nothing about data. There is no numerical method anywhere in
//  this file, and there must never be one: the moment the reference dialect
//  becomes useful, people use it, and a contract fixture becomes a dependency.
//
//  Read it as the worked example the "writing a dialect" guide refers to.
// ============================================================================

/// How a term enters the form. A closed vocabulary — three cases, and adding a
/// fourth is a dialect-version change, not an implementation detail.
type Transform =
    | Identity
    | Log
    | Lag of periods: int

/// An inclusive range a term's coefficient is asked to stay inside. The dialect does
/// not enforce it (it enforces nothing numerically) — it carries it into the payload
/// for whatever does.
type Bound = { Lower: float; Upper: float }

/// One named term of the form.
type Term =
    { Name: string
      Transform: Transform
      Bound: Bound option }

/// The reference dialect's spec tree: a response, whether the form carries an
/// intercept, and the terms.
type ReferenceSpec =
    { Response: string
      Intercept: bool
      Terms: Term list }

/// A deliberate defect, injected into an otherwise-conforming dialect.
///
/// These ship rather than living in a test file, for the same reason the conformance
/// harness does: a dialect author has to be able to see their harness FAIL before
/// trusting it to pass. Each mutation breaks exactly one obligation and is named for
/// the code it must produce.
type Mutation =
    /// `Mint` returns the same hash for every input.
    | ConstantHash
    /// The payload's hash is not `Mint` of the payload's bytes.
    | WrongHashRule
    /// The importer drops what it cannot model and reports no residue.
    | LossyImporter
    /// The projected payload is tagged with a dialect that is not this one.
    | DriftingIdentity
    /// Two projections of one spec differ.
    | UnstableProjector
    /// A pack of an undeclared kind is applied instead of refused.
    | PermissivePacks
    /// A spec with a blocking finding projects anyway.
    | LenientProjector
    /// The dialect targets a seam this build does not implement.
    | StaleSeam
    /// A declared invariant comparator that never finds anything.
    ///
    /// The invariant-shaped defect, and the one worth having: a comparator that is silent is
    /// indistinguishable from a forest that is coherent, and everything downstream — the
    /// findings, the declarations, the whole priced-divergence mechanism — reads as though the
    /// question had been asked and answered well.
    | SilentComparator

/// The reference dialect: every seam obligation, minimally and honestly.
[<RequireQualifiedAccess>]
module ReferenceDialect =

    let dialectId = "reference-linear"
    let payloadFormat = "reference-linear/v1"

    /// The minting rule: canonicalise the rendering, digest it — the shared, registered
    /// rule, declared by its identifier rather than invented here.
    ///
    /// A dialect is free to mint however it likes, and this one deliberately does not
    /// exercise that freedom: the property that matters is that two submitters agreeing on
    /// a specification mint the same hash, and nothing downstream can check that they did.
    /// A dialect that invents its own rule for no reason interoperates with nobody, and
    /// finds out much later.
    let specHashAlgorithm = Seam.mintingAlgorithmId

    /// Total by construction — this dialect's renderings are always canonical JSON, so the
    /// `raw:` fallback in `Seam.mintTotal` is unreachable from its own projector.
    let mint (body: string) : string = Seam.mintTotal body

    let identity: DialectIdentity =
        { Id = dialectId
          Version = "1.0"
          PayloadFormat = payloadFormat
          SpecHashAlgorithm = specHashAlgorithm
          TargetsSeam = SeamVersion.current }

    /// The published term vocabulary. Every entry is exercised by the corpus, and the
    /// corpus may exercise nothing else — the coverage gate checks both directions.
    let vocabulary =
        [ "response"
          "intercept"
          "term"
          "transform:identity"
          "transform:log"
          "transform:lag"
          "bound" ]

    // ---- the document form ---------------------------------------------------

    let private encodeTransform (t: Transform) : JVal =
        match t with
        | Identity -> Canonical.typed "identity" []
        | Log -> Canonical.typed "log" []
        | Lag periods -> Canonical.typed "lag" [ "periods", JInt periods ]

    let private encodeTerm (t: Term) : JVal =
        JObj(
            [ "name", JStr t.Name; "transform", encodeTransform t.Transform ]
            @ (match t.Bound with
               | None -> []
               | Some b -> [ "bound", JObj [ "lower", JFloat b.Lower; "upper", JFloat b.Upper ] ])
        )

    let private encodeSpec (s: ReferenceSpec) : JVal =
        JObj
            [ "response", JStr s.Response
              "intercept", JBool s.Intercept
              "terms", JArr(s.Terms |> List.map encodeTerm) ]

    /// The canonical bytes of a spec: the substrate's canonical JSON discipline — keys
    /// Ordinal-sorted recursively, no whitespace, the pinned float layout, an absent
    /// optional an absent key rather than a null.
    ///
    /// This is the dialect's minting canonicalisation, and it is the dialect's to choose:
    /// the manager never re-derives a hash, precisely because the canonical form of a
    /// payload is knowledge only its dialect has.
    let render (s: ReferenceSpec) : string =
        Canonical.write Rendering.CrossHost (encodeSpec s)

    // ---- the importer --------------------------------------------------------

    let private rootKeys = [ "response"; "intercept"; "terms" ]
    let private termKeys = [ "name"; "transform"; "bound" ]

    /// The string-error readers this file is written against. Core 0.37.0 retired its own
    /// (`Decode.getProp`, `strField`, `asString` and the rest) for the typed `Decoder`; these
    /// are its migration table's one-line replacements, kept under the old names so that every
    /// call site reads exactly as it did and the error stays a string at this boundary.
    module private Decode =
        let getProp (name: string) (el: JVal) : Result<JVal, string> =
            Decoder.describing (Decoder.field name Decoder.json) el

        let strField (name: string) (el: JVal) : Result<string, string> =
            Decoder.describing (Decoder.field name Decoder.str) el

        let asString (el: JVal) : Result<string, string> = Decoder.describing Decoder.str el
        let asFloat (el: JVal) : Result<float, string> = Decoder.describing Decoder.float el
        let asBool (el: JVal) : Result<bool, string> = Decoder.describing Decoder.bool el
        let asInt (el: JVal) : Result<int, string> = Decoder.describing Decoder.int el

        let mapList (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
            Decoder.describing Decoder.items el
            |> Result.bind (fun items ->
                (items, Ok [])
                ||> List.foldBack (fun item acc ->
                    match d item, acc with
                    | Ok x, Ok xs -> Ok(x :: xs)
                    | Error e, _ -> Error e
                    | _, Error e -> Error e))

    let private objFields (el: JVal) : (string * JVal) list option =
        match el with
        | JObj fields -> Some fields
        | _ -> None

    let private tryProp (name: string) (fields: (string * JVal) list) : JVal option =
        fields |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd

    /// Import one term, collecting residue as it goes.
    let private importTerm (reportResidue: bool) (index: int) (el: JVal) : Result<Term * Residue list, ImportFailure> =
        let path = "/terms/" + string index

        match objFields el with
        | None -> Error(Unmappable(path, "a term must be an object"))
        | Some fields ->
            match tryProp "name" fields |> Option.map Decode.asString with
            | None -> Error(Unmappable(path + "/name", "a term must carry a name"))
            | Some(Error m) -> Error(Unmappable(path + "/name", m))
            | Some(Ok name) ->
                let transform, transformResidue =
                    match tryProp "transform" fields with
                    | None -> Ok Identity, []
                    | Some t ->
                        match objFields t |> Option.bind (tryProp "$type") |> Option.map Decode.asString with
                        | None -> Error(Unmappable(path + "/transform", "a transform must be a tagged object")), []
                        | Some(Error m) -> Error(Unmappable(path + "/transform", m)), []
                        | Some(Ok "identity") -> Ok Identity, []
                        | Some(Ok "log") -> Ok Log, []
                        | Some(Ok "lag") ->
                            match objFields t |> Option.bind (tryProp "periods") |> Option.map Decode.asInt with
                            | Some(Ok periods) -> Ok(Lag periods), []
                            | Some(Error m) -> Error(Unmappable(path + "/transform/periods", m)), []
                            | None -> Error(Unmappable(path + "/transform/periods", "a lag must carry its periods")), []
                        // A transform this dialect does not model. Dropped to the identity
                        // and REPORTED — the whole difference between an importer and a
                        // parser is that the caller finds out.
                        | Some(Ok other) ->
                            Ok Identity,
                            (if reportResidue then
                                 [ { Path = path + "/transform"
                                     Term = other
                                     Detail =
                                       "transform not modelled by this dialect; the term was imported untransformed" } ]
                             else
                                 [])

                let bound =
                    match tryProp "bound" fields with
                    | None -> Ok None
                    | Some b ->
                        match objFields b with
                        | None -> Error(Unmappable(path + "/bound", "a bound must be an object"))
                        | Some bf ->
                            match
                                tryProp "lower" bf |> Option.map Decode.asFloat,
                                tryProp "upper" bf |> Option.map Decode.asFloat
                            with
                            | Some(Ok lower), Some(Ok upper) -> Ok(Some { Lower = lower; Upper = upper })
                            | _ -> Error(Unmappable(path + "/bound", "a bound must carry numeric lower and upper"))

                let unknown =
                    if reportResidue then
                        fields
                        |> List.filter (fun (k, _) -> not (List.contains k termKeys))
                        |> List.map (fun (k, _) ->
                            { Path = path
                              Term = k
                              Detail = "term property not modelled by this dialect" })
                    else
                        []

                match transform, bound with
                | Error f, _ -> Error f
                | _, Error f -> Error f
                | Ok transform, Ok bound ->
                    Ok(
                        { Name = name
                          Transform = transform
                          Bound = bound },
                        transformResidue @ unknown
                    )

    let private importWith (reportResidue: bool) (text: string) : Result<ImportOutcome<ReferenceSpec>, ImportFailure> =
        match Json.parse text with
        | Error m -> Error(Malformed m)
        | Ok el ->
            match objFields el with
            | None -> Error(Malformed "the document must be an object")
            | Some fields ->
                match tryProp "response" fields |> Option.map Decode.asString with
                | None -> Error(Unmappable("/response", "the document must name a response"))
                | Some(Error m) -> Error(Unmappable("/response", m))
                | Some(Ok response) ->
                    let intercept =
                        match tryProp "intercept" fields |> Option.map Decode.asBool with
                        | None -> Ok true
                        | Some(Ok b) -> Ok b
                        | Some(Error m) -> Error(Unmappable("/intercept", m))

                    let terms =
                        match tryProp "terms" fields with
                        | None -> Ok([], [])
                        | Some(JArr items) ->
                            items
                            |> List.mapi (fun i t -> importTerm reportResidue i t)
                            |> List.fold
                                (fun acc r ->
                                    match acc, r with
                                    | Error f, _ -> Error f
                                    | _, Error f -> Error f
                                    | Ok(ts, rs), Ok(t, r) -> Ok(ts @ [ t ], rs @ r))
                                (Ok([], []))
                        | Some _ -> Error(Unmappable("/terms", "terms must be an array"))

                    let unknownRoot =
                        if reportResidue then
                            fields
                            |> List.filter (fun (k, _) -> not (List.contains k rootKeys))
                            |> List.map (fun (k, _) ->
                                { Path = "/"
                                  Term = k
                                  Detail = "document property not modelled by this dialect" })
                        else
                            []

                    match intercept, terms with
                    | Error f, _ -> Error f
                    | _, Error f -> Error f
                    | Ok intercept, Ok(terms, termResidue) ->
                        Ok
                            { Spec =
                                { Response = response
                                  Intercept = intercept
                                  Terms = terms }
                              Residue = unknownRoot @ termResidue }

    /// Obligation 4: parse a document back to the typed tree, naming everything that did
    /// not come across. Deterministic — the residue is reported in document order.
    let import (text: string) : Result<ImportOutcome<ReferenceSpec>, ImportFailure> = importWith true text

    // ---- the validators ------------------------------------------------------

    let private finding code severity path message =
        { Code = code
          Severity = severity
          Path = path
          Message = message }

    /// Obligation 2: the deterministic verify tier. Pure, total, and ordered — the same
    /// spec produces the same findings in the same sequence on every host.
    let validate (s: ReferenceSpec) : Finding list =
        let responseFindings =
            if s.Response.Trim() = "" then
                [ finding "REF-RESPONSE-EMPTY" Blocking "/response" "the form names no response" ]
            else
                []

        let termsPresent =
            if List.isEmpty s.Terms then
                [ finding "REF-NO-TERMS" Blocking "/terms" "the form has no terms" ]
            else
                []

        let duplicates =
            s.Terms
            |> List.countBy (fun t -> t.Name)
            |> List.filter (fun (_, n) -> n > 1)
            |> List.map (fun (name, _) ->
                finding "REF-DUPLICATE-TERM" Blocking "/terms" ("the term '" + name + "' appears more than once"))

        let perTerm =
            s.Terms
            |> List.indexed
            |> List.collect (fun (i, t) ->
                let path = "/terms/" + string i

                let unnamed =
                    if t.Name.Trim() = "" then
                        [ finding "REF-TERM-UNNAMED" Blocking path "a term has no name" ]
                    else
                        []

                let isResponse =
                    if t.Name = s.Response && s.Response.Trim() <> "" then
                        [ finding "REF-TERM-IS-RESPONSE" Blocking path "a term names the response" ]
                    else
                        []

                let lag =
                    match t.Transform with
                    | Lag periods when periods < 0 ->
                        [ finding "REF-LAG-NEGATIVE" Blocking (path + "/transform") "a lag cannot be negative" ]
                    | _ -> []

                let bound =
                    match t.Bound with
                    | Some b when b.Lower > b.Upper ->
                        [ finding "REF-BOUND-INVERTED" Blocking (path + "/bound") "a bound's lower exceeds its upper" ]
                    | _ -> []

                unnamed @ isResponse @ lag @ bound)

        let intercept =
            if not s.Intercept then
                [ finding
                      "REF-NO-INTERCEPT"
                      Advisory
                      "/intercept"
                      "the form has no intercept; legitimate, and rarely what was meant" ]
            else
                []

        responseFindings @ termsPresent @ duplicates @ perTerm @ intercept

    // ---- the projector -------------------------------------------------------

    let private projectWith
        (tagAs: string)
        (hashOf: ReferenceSpec -> string -> string)
        (ignoreBlocking: bool)
        (s: ReferenceSpec)
        : Result<SpecPayload, Finding list> =
        let blocking =
            validate s |> List.filter Finding.blocks |> List.sortBy Finding.sortKey

        if not ignoreBlocking && not (List.isEmpty blocking) then
            Error blocking
        else
            let body = render s

            Ok
                { Dialect = tagAs
                  Format = payloadFormat
                  Body = body
                  SpecHash = hashOf s body }

    /// Obligation 3: render the typed tree to the payload that crosses the execution wire,
    /// minting the hash over its canonical bytes.
    ///
    /// Refuses exactly when validation blocks. That equivalence is the seam's most
    /// load-bearing law: it is what lets everything downstream trust that a stored payload
    /// came from a spec its own dialect considered valid, without re-validating something
    /// it cannot read.
    let project (s: ReferenceSpec) : Result<SpecPayload, Finding list> =
        projectWith dialectId (fun _ body -> mint body) false s

    // ---- the packs -----------------------------------------------------------

    let boundsPackKind = "bounds"
    let transformsPackKind = "transforms"
    let packKinds = [ boundsPackKind; transformsPackKind ]

    let private applyBounds (body: string) (s: ReferenceSpec) : Result<ReferenceSpec, PackRefusal> =
        match Json.parse body |> Result.bind (fun el -> Decode.getProp "bounds" el) with
        | Error m -> Error(MalformedPack("a bounds pack must carry a 'bounds' object: " + m))
        | Ok boundsEl ->
            match objFields boundsEl with
            | None -> Error(MalformedPack "'bounds' must be an object keyed by term name")
            | Some entries ->
                let rec go (remaining: (string * JVal) list) (spec: ReferenceSpec) =
                    match remaining with
                    | [] -> Ok spec
                    | (name, el) :: rest ->
                        if not (spec.Terms |> List.exists (fun t -> t.Name = name)) then
                            Error(
                                PackConflicts("/terms", "the pack bounds a term the form does not have: '" + name + "'")
                            )
                        else
                            match objFields el with
                            | None -> Error(MalformedPack("the bound for '" + name + "' is not an object"))
                            | Some bf ->
                                match
                                    tryProp "lower" bf |> Option.map Decode.asFloat,
                                    tryProp "upper" bf |> Option.map Decode.asFloat
                                with
                                | Some(Ok lower), Some(Ok upper) ->
                                    let terms =
                                        spec.Terms
                                        |> List.map (fun t ->
                                            if t.Name = name then
                                                { t with
                                                    Bound = Some { Lower = lower; Upper = upper } }
                                            else
                                                t)

                                    go rest { spec with Terms = terms }
                                | _ ->
                                    Error(
                                        MalformedPack("the bound for '" + name + "' must carry numeric lower and upper")
                                    )

                go entries s

    let private applyTransforms (body: string) (s: ReferenceSpec) : Result<ReferenceSpec, PackRefusal> =
        match Json.parse body |> Result.bind (fun el -> Decode.getProp "transforms" el) with
        | Error m -> Error(MalformedPack("a transforms pack must carry a 'transforms' object: " + m))
        | Ok el ->
            match objFields el with
            | None -> Error(MalformedPack "'transforms' must be an object keyed by term name")
            | Some entries ->
                let rec go (remaining: (string * JVal) list) (spec: ReferenceSpec) =
                    match remaining with
                    | [] -> Ok spec
                    | (name, tEl) :: rest ->
                        if not (spec.Terms |> List.exists (fun t -> t.Name = name)) then
                            Error(
                                PackConflicts(
                                    "/terms",
                                    "the pack transforms a term the form does not have: '" + name + "'"
                                )
                            )
                        else
                            // A pack is authored against the same document form as a spec, so
                            // it is imported by the same code — one parser, one vocabulary.
                            let wrapped =
                                Canonical.write
                                    Rendering.CrossHost
                                    (JObj
                                        [ "response", JStr "packed"
                                          "intercept", JBool true
                                          "terms", JArr [ JObj [ "name", JStr name; "transform", tEl ] ] ])

                            match importWith false wrapped with
                            | Error f -> Error(MalformedPack(ImportFailure.describe f))
                            | Ok o ->
                                match o.Spec.Terms with
                                | [ imported ] ->
                                    let terms =
                                        spec.Terms
                                        |> List.map (fun t ->
                                            if t.Name = name then
                                                { t with
                                                    Transform = imported.Transform }
                                            else
                                                t)

                                    go rest { spec with Terms = terms }
                                | _ -> Error(MalformedPack("the transform for '" + name + "' could not be read"))

                go entries s

    /// Obligation 6: fold a parameter bundle into a spec. An undeclared kind is refused,
    /// never ignored — a pack that silently does nothing looks exactly like one that
    /// worked, and the difference surfaces much later, as a fit nobody can explain.
    let applyPack (pack: Pack) (s: ReferenceSpec) : Result<ReferenceSpec, PackRefusal> =
        if pack.Kind = boundsPackKind then
            applyBounds pack.Body s
        elif pack.Kind = transformsPackKind then
            applyTransforms pack.Body s
        else
            Error(UnknownPackKind(pack.Kind, packKinds))

    // ---- the gates -----------------------------------------------------------

    /// Obligation 5: the dialect's default gate vocabulary, expressed on the generic
    /// `(name, threshold, direction)` triple so a host, a policy or an execution backend
    /// can reason about gates from any dialect without knowing any of them.
    ///
    /// These two are illustrative. They name nothing statistical and assert nothing about
    /// what a good form looks like — the reference dialect has no opinion, because it has
    /// no subject matter.
    let defaultGates: GateSpec list =
        [ { Request =
              { Name = "termCount"
                Threshold = 1.0
                Unit = None
                Direction = AtLeast }
            Description = "the form contributes at least one term" }
          { Request =
              { Name = "residualScale"
                Threshold = 1.0
                Unit = None
                Direction = AtMost }
            Description = "a scale the executing side reports back; the dialect neither computes nor interprets it" } ]

    // ---- the invariants (the optional obligation) ----------------------------

    /// The one invariant this dialect declares: a term appearing in more than one sibling
    /// carries the same transform in all of them.
    ///
    /// It is the smallest cross-sibling question that is still a real one. It reads the
    /// dialect's OWN trees — imported back from the projected payloads it is handed, on this
    /// side of the seam — and answers in the manager's vocabulary, which is the shape every
    /// comparator has. The manager never learns what a term or a transform is.
    ///
    /// Like everything else here it asserts nothing statistical: "the same term should be
    /// transformed the same way across units unless somebody says why not" is a statement about
    /// process coherence, and the dialect has no subject matter with which to make it a
    /// statement about the world.
    let transformConsistencyId = "REF-TRANSFORM-CONSISTENT"

    let private transformName (t: Transform) : string =
        match t with
        | Identity -> "identity"
        | Log -> "log"
        | Lag periods -> "lag/" + string periods

    let private compareTransforms (projected: ProjectedState list) : InvariantFinding list =
        // Only this dialect's own payloads are comparable. A projection may legitimately carry
        // siblings from another dialect — a forest is not required to be monolingual — and a
        // comparator that tried to read those would be reading a payload it does not own.
        let mine = projected |> List.filter (fun p -> p.Payload.Dialect = dialectId)

        let terms =
            [ for p in mine do
                  match importWith false p.Payload.Body with
                  | Error _ -> ()
                  | Ok o ->
                      for t in o.Spec.Terms do
                          yield t.Name, (p.Sibling, transformName t.Transform) ]

        terms
        |> List.groupBy fst
        |> List.sortBy fst
        |> List.choose (fun (term, entries) ->
            let seen =
                entries |> List.map (fun (_, (_, transform)) -> transform) |> List.distinct

            if List.length seen < 2 then
                None
            else
                Some
                    { InvariantId = transformConsistencyId
                      Siblings = entries |> List.map (fun (_, (sibling, _)) -> sibling)
                      Detail =
                        "the term '"
                        + term
                        + "' is transformed differently across siblings ("
                        + (seen |> List.sort |> String.concat ", ")
                        + ")" })

    let invariants: InvariantComparator list =
        [ { Id = transformConsistencyId
            Description =
              "a term appearing in more than one sibling carries the same transform in all of them; a sibling that differs declares a divergence"
            Compare = compareTransforms } ]

    // ---- the dialect ---------------------------------------------------------

    /// One object implementing both the required interface and the optional one.
    ///
    /// A dialect that has nothing to say across siblings simply omits the second interface —
    /// that is what "optional" means here, and `Seam.invariants` returns an empty list for it
    /// without a type test ever being visible to its author.
    let private build
        (ident: DialectIdentity)
        (importFn: string -> Result<ImportOutcome<ReferenceSpec>, ImportFailure>)
        (projectFn: ReferenceSpec -> Result<SpecPayload, Finding list>)
        (mintFn: string -> string)
        (packFn: Pack -> ReferenceSpec -> Result<ReferenceSpec, PackRefusal>)
        (invariantFns: InvariantComparator list)
        : IDialect<ReferenceSpec> =
        { new IDialect<ReferenceSpec> with
            member _.Identity = ident
            member _.Vocabulary = vocabulary
            member _.Validate spec = validate spec
            member _.Project spec = projectFn spec
            member _.Mint body = mintFn body
            member _.Import text = importFn text
            member _.DefaultGates = defaultGates
            member _.PackKinds = packKinds
            member _.ApplyPack pack spec = packFn pack spec

          interface IInvariantComparators with
              member _.Invariants = invariantFns }

    /// The conforming reference dialect.
    let dialect: IDialect<ReferenceSpec> =
        build identity import project mint applyPack invariants

    /// The reference dialect with a comparator set of your choosing.
    ///
    /// Ships beside the mutants and for the same reason (D18): the invariant obligation is the
    /// one whose defects are most easily written by hand — a duplicated id, an empty
    /// description, a comparator that answers differently each time — and a dialect author
    /// needs to watch their harness catch each of those before trusting it to pass their own.
    let withInvariants (comparators: InvariantComparator list) : IDialect<ReferenceSpec> =
        build identity import project mint applyPack comparators

    /// The reference dialect with one obligation deliberately broken.
    ///
    /// A conformance harness that has never been seen to fail is a harness nobody has
    /// tested. Each mutation below is a go-red self-test: run the corpus against it and
    /// the run must fail, naming the code for the obligation it broke.
    let mutant (m: Mutation) : IDialect<ReferenceSpec> =
        match m with
        | ConstantHash ->
            let constant (_: string) = "00000000"

            build
                identity
                import
                (projectWith dialectId (fun _ body -> constant body) false)
                constant
                applyPack
                invariants
        | WrongHashRule ->
            // Hashes the response instead of the payload. The hash still looks like a hash,
            // is still deterministic, and names nothing.
            build identity import (projectWith dialectId (fun s _ -> mint s.Response) false) mint applyPack invariants
        | LossyImporter -> build identity (importWith false) project mint applyPack invariants
        | DriftingIdentity ->
            build identity import (projectWith "some-other-dialect" (fun _ b -> mint b) false) mint applyPack invariants
        | UnstableProjector ->
            let counter = ref 0

            let unstable (s: ReferenceSpec) =
                let n = counter.Value
                counter.Value <- n + 1

                projectWith dialectId (fun _ body -> mint (body + string n)) false s
                |> Result.map (fun p -> { p with Body = p.Body + "  " })

            build identity import unstable mint applyPack invariants
        | PermissivePacks ->
            let permissive (pack: Pack) (s: ReferenceSpec) =
                if List.contains pack.Kind packKinds then
                    applyPack pack s
                else
                    Ok s

            build identity import project mint permissive invariants
        | LenientProjector ->
            build identity import (projectWith dialectId (fun _ b -> mint b) true) mint applyPack invariants
        | StaleSeam ->
            let ahead =
                { identity with
                    TargetsSeam =
                        { Major = SeamVersion.current.Major + 1
                          Minor = 0 } }

            build ahead import project mint applyPack invariants
        | SilentComparator ->
            // Still DECLARES the invariant — an id and a description, so the shape checks pass
            // and a declaration against it is accepted. It simply never finds anything, which
            // is exactly how this defect would arrive in a real dialect: not as an absent
            // obligation, but as one that quietly agrees with everything.
            let silent = invariants |> List.map (fun c -> { c with Compare = fun _ -> [] })

            build identity import project mint applyPack silent
