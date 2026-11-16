// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.DialectSeam

open Fuaran.Core
open Fuaran.Modelling

// ============================================================================
//  The dialect conformance corpus and its harness.
//
//  The seam is a public extension point, so its contract has to be executable by
//  people who did not write this library. What ships here is the machinery a
//  dialect runs against ITSELF: six fixture families, a set of universal laws
//  that need no fixtures at all, and a report in which every failure carries a
//  stable code naming the obligation it broke.
//
//  The corpus is DATA — a JSON document in the shape below — and it is
//  per-dialect, because expected payload bytes are dialect knowledge by
//  definition. What is universal is the shape, the harness, and the codes. A
//  dialect written in another language authors the same document and implements
//  the same checks; nothing here needs .NET to be true.
//
//  Fixture families:
//
//    projector   source -> import -> project: canonical bytes + hash, pinned
//    importer    source -> import: the residue report, pinned
//    validator   source -> validate: the findings, pinned, and whether it projects
//    gates       the dialect's default gate vocabulary, pinned
//    packs       source + pack -> apply: applied bytes, or a typed refusal
//    invariants  sibling states -> compare: the findings, pinned (OPTIONAL —
//                a dialect declaring none passes visibly rather than silently)
//
//  Every fixture pins an OBSERVED value rather than describing an intention.
//  A corpus of intentions passes against an implementation that does something
//  else entirely, which is the failure this file exists to make impossible.
// ============================================================================

/// The stable codes a conformance run reports. Every check has one, passing or
/// failing, so a report is greppable and a dialect author can look up what broke
/// without reading the harness.
module Codes =

    // ---- universal (no fixtures required) ----
    let identityIncomplete = "DIALECT-IDENTITY-INCOMPLETE"
    let identityMismatch = "DIALECT-IDENTITY-MISMATCH"
    let seamSkew = "DIALECT-SEAM-SKEW"
    let vocabularyEmpty = "DIALECT-VOCABULARY-EMPTY"
    let vocabularyDuplicate = "DIALECT-VOCABULARY-DUPLICATE"
    let vocabularyUncovered = "DIALECT-VOCABULARY-UNCOVERED"
    let vocabularyUndeclared = "DIALECT-VOCABULARY-UNDECLARED"
    let mintNondeterministic = "DIALECT-MINT-NONDETERMINISTIC"
    let packUndeclaredAccepted = "DIALECT-PACK-UNDECLARED-ACCEPTED"

    // ---- projector family ----
    let importFailed = "DIALECT-IMPORT-FAILED"
    let projectRefused = "DIALECT-PROJECT-REFUSED"
    let projectBytesDrift = "DIALECT-PROJECT-BYTES-DRIFT"
    let projectHashDrift = "DIALECT-PROJECT-HASH-DRIFT"
    let projectorNondeterministic = "DIALECT-PROJECTOR-NONDETERMINISTIC"
    let hashRule = "DIALECT-HASH-RULE"
    let hashCollision = "DIALECT-HASH-COLLISION"
    let projectNotIdempotent = "DIALECT-PROJECT-NOT-IDEMPOTENT"

    // ---- importer family ----
    let residueDrift = "DIALECT-RESIDUE-DRIFT"
    let importNondeterministic = "DIALECT-IMPORT-NONDETERMINISTIC"

    // ---- validator family ----
    let validatorDrift = "DIALECT-VALIDATOR-DRIFT"
    let validatorNondeterministic = "DIALECT-VALIDATOR-NONDETERMINISTIC"
    let projectAcceptsBlocking = "DIALECT-PROJECT-ACCEPTS-BLOCKING"
    let projectRefusesValid = "DIALECT-PROJECT-REFUSES-VALID"

    // ---- gates family ----
    let gateMissing = "DIALECT-GATE-MISSING"
    let gateDrift = "DIALECT-GATE-DRIFT"
    let gateDuplicate = "DIALECT-GATE-DUPLICATE"

    // ---- packs family ----
    let packOutcomeDrift = "DIALECT-PACK-OUTCOME-DRIFT"
    let packNondeterministic = "DIALECT-PACK-NONDETERMINISTIC"

    // ---- invariants family (optional obligation, seam 1.1) ----
    let invariantDuplicate = "DIALECT-INVARIANT-DUPLICATE"
    let invariantUndescribed = "DIALECT-INVARIANT-UNDESCRIBED"
    let invariantUndeclared = "DIALECT-INVARIANT-UNDECLARED"
    let invariantDrift = "DIALECT-INVARIANT-DRIFT"
    let invariantNondeterministic = "DIALECT-INVARIANT-NONDETERMINISTIC"

/// A projector fixture: an authoring document, and the exact bytes and hash the dialect
/// must produce from it.
type ProjectorCase =
    {
        Name: string
        Source: string
        Bytes: string
        Hash: string
        /// Which vocabulary terms this fixture exercises — the coverage gate's input.
        Covers: string list
    }

/// One expected residue entry. `Detail` is deliberately not pinned: a dialect may reword
/// its explanation freely, but WHAT it dropped and WHERE are contract.
type ResidueExpectation = { Path: string; Term: string }

/// An importer fixture: a document, and everything the dialect must report as residue.
type ImporterCase =
    {
        Name: string
        Source: string
        Residue: ResidueExpectation list
        /// Whether the imported spec must re-project to the same bytes after a further
        /// import round — projection idempotence, checked where the fixture asserts it.
        RoundTrip: bool
    }

/// One expected finding. Message text is not pinned; code and severity are.
type FindingExpectation = { Code: string; Severity: Severity }

/// A validator fixture: a document, the findings it must produce, and whether it projects.
type ValidatorCase =
    { Name: string
      Source: string
      Findings: FindingExpectation list
      Projects: bool }

/// A gate fixture: one entry of the dialect's default gate vocabulary.
type GateCase =
    { Name: string
      Threshold: float
      Direction: GateDirection }

/// What applying a pack must do.
type PackExpectation =
    /// Applied, and the resulting spec projects to exactly these bytes.
    | PackApplied of bytes: string
    /// Refused, with a refusal of this kind (`unknownKind` | `malformed` | `conflicts`).
    | PackRefused of kind: string

/// A pack fixture: a document, a pack, and what applying it must do.
type PackCase =
    { Name: string
      Source: string
      Pack: Pack
      Expect: PackExpectation }

/// One sibling's contribution to an invariant fixture: which sibling it stands for, which
/// state id that sibling recorded, and the authoring document the state's payload comes from.
///
/// The sibling and state ids are the fixture's own naming, not the dialect's — a corpus
/// exercises a comparator, and a comparator's input is a set of (sibling, state, payload)
/// triples whatever the surrounding forest actually is.
type InvariantSibling =
    { Sibling: string
      State: string
      Source: string }

/// One finding an invariant fixture must produce: which invariant, and which siblings it
/// names. The detail is not pinned — it is for a person, on the same reasoning that leaves a
/// validator's message and a residue entry's detail unpinned.
type InvariantExpectation =
    { InvariantId: string
      Siblings: string list }

/// An invariant fixture: a set of sibling states, and everything comparing them must find.
///
/// Pinning the EMPTY expectation is as important as pinning a populated one. A comparator that
/// never fires satisfies every fixture that expects a finding to be absent and none that
/// expects one to be present, so a corpus with only clean cases certifies a comparator that
/// does nothing at all.
type InvariantCase =
    { Name: string
      Siblings: InvariantSibling list
      Findings: InvariantExpectation list }

/// One dialect's conformance corpus.
type DialectCorpus =
    {
        CorpusVersion: int
        SeamVersion: SeamVersion
        Dialect: string
        Projector: ProjectorCase list
        Importer: ImporterCase list
        Validator: ValidatorCase list
        Gates: GateCase list
        Packs: PackCase list
        /// The optional invariant obligation's fixtures. Absent in a corpus authored before seam
        /// 1.1, and absent in a corpus for a dialect that declares no invariants — both decode
        /// to the empty list and neither is a failure.
        Invariants: InvariantCase list
    }

/// One check: which family and fixture it ran under, which obligation it checked, and
/// whether that obligation held.
type CheckResult =
    { Family: string
      Case: string
      Code: string
      Passed: bool
      Detail: string }

/// The corpus document format, and the harness that runs it.
[<RequireQualifiedAccess>]
module Conformance =

    /// The corpus document's own version. Bumped when the document SHAPE changes, which is
    /// a different question from the seam version — a corpus can gain a field without the
    /// dialect contract moving at all.
    let corpusVersion = 1

    let private tag = "dialect-conformance"

    // ---- decoding ------------------------------------------------------------

    type private ResultBuilder() =
        member _.Bind(m: Result<'a, 'e>, f: 'a -> Result<'b, 'e>) = Result.bind f m
        member _.Return(v: 'a) : Result<'a, 'e> = Ok v
        member _.ReturnFrom(m: Result<'a, 'e>) = m

    let private res = ResultBuilder()

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

    let private listField (name: string) (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
        Decode.getProp name el |> Result.bind (Decode.mapList d)

    /// An optional list field: absent key ⇒ empty. A family a dialect has no fixtures for
    /// is an omitted key, not an empty array it had to remember to write.
    let private optListField (name: string) (d: JVal -> Result<'T, string>) (el: JVal) : Result<'T list, string> =
        match el with
        | JObj fields when not (fields |> List.exists (fun (k, _) -> k = name)) -> Ok []
        | _ -> listField name d el

    let private boolField (name: string) (el: JVal) : Result<bool, string> =
        Decode.getProp name el |> Result.bind Decode.asBool

    let private floatField (name: string) (el: JVal) : Result<float, string> =
        Decode.getProp name el |> Result.bind Decode.asFloat

    let private decodeProjectorCase (el: JVal) : Result<ProjectorCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! source = Decode.strField "source" el
            let! bytes = Decode.strField "bytes" el
            let! hash = Decode.strField "hash" el
            let! covers = listField "covers" Decode.asString el

            return
                { Name = name
                  Source = source
                  Bytes = bytes
                  Hash = hash
                  Covers = covers }
        }

    let private decodeResidue (el: JVal) : Result<ResidueExpectation, string> =
        res {
            let! path = Decode.strField "path" el
            let! term = Decode.strField "term" el
            return { Path = path; Term = term }
        }

    let private decodeImporterCase (el: JVal) : Result<ImporterCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! source = Decode.strField "source" el
            let! residue = listField "residue" decodeResidue el
            let! roundTrip = boolField "roundTrip" el

            return
                { Name = name
                  Source = source
                  Residue = residue
                  RoundTrip = roundTrip }
        }

    let private decodeFinding (el: JVal) : Result<FindingExpectation, string> =
        res {
            let! code = Decode.strField "code" el
            let! sevText = Decode.strField "severity" el
            let! severity = Severity.tryParse sevText
            return { Code = code; Severity = severity }
        }

    let private decodeValidatorCase (el: JVal) : Result<ValidatorCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! source = Decode.strField "source" el
            let! findings = listField "findings" decodeFinding el
            let! projects = boolField "projects" el

            return
                { Name = name
                  Source = source
                  Findings = findings
                  Projects = projects }
        }

    let private decodeGateCase (el: JVal) : Result<GateCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! threshold = floatField "threshold" el
            let! dirText = Decode.strField "direction" el
            let! direction = GateDirection.tryParse dirText

            return
                { Name = name
                  Threshold = threshold
                  Direction = direction }
        }

    let private decodePack (el: JVal) : Result<Pack, string> =
        res {
            let! id = Decode.strField "id" el
            let! kind = Decode.strField "kind" el
            let! version = Decode.strField "version" el
            let! body = Decode.strField "body" el

            return
                { Id = id
                  Kind = kind
                  Version = version
                  Body = body }
        }

    let private decodeExpectation (el: JVal) : Result<PackExpectation, string> =
        res {
            let! t = Decode.strField "$type" el

            match t with
            | "applied" ->
                let! bytes = Decode.strField "bytes" el
                return PackApplied bytes
            | "refused" ->
                let! kind = Decode.strField "refusal" el
                return PackRefused kind
            | other -> return! Error("unknown pack expectation '" + other + "' (expected applied | refused)")
        }

    let private decodePackCase (el: JVal) : Result<PackCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! source = Decode.strField "source" el
            let! packEl = Decode.getProp "pack" el
            let! pack = decodePack packEl
            let! expectEl = Decode.getProp "expect" el
            let! expect = decodeExpectation expectEl

            return
                { Name = name
                  Source = source
                  Pack = pack
                  Expect = expect }
        }

    let private decodeInvariantSibling (el: JVal) : Result<InvariantSibling, string> =
        res {
            let! sibling = Decode.strField "sibling" el
            let! state = Decode.strField "state" el
            let! source = Decode.strField "source" el

            return
                { Sibling = sibling
                  State = state
                  Source = source }
        }

    let private decodeInvariantExpectation (el: JVal) : Result<InvariantExpectation, string> =
        res {
            let! invariantId = Decode.strField "invariantId" el
            let! siblings = listField "siblings" Decode.asString el

            return
                { InvariantId = invariantId
                  Siblings = siblings }
        }

    let private decodeInvariantCase (el: JVal) : Result<InvariantCase, string> =
        res {
            let! name = Decode.strField "name" el
            let! siblings = listField "siblings" decodeInvariantSibling el
            let! findings = listField "findings" decodeInvariantExpectation el

            return
                { Name = name
                  Siblings = siblings
                  Findings = findings }
        }

    let decodeCorpus (el: JVal) : Result<DialectCorpus, string> =
        res {
            let! marker = Decode.strField "corpus" el

            if marker <> tag then
                return! Error("not a dialect-conformance corpus (found '" + marker + "')")
            else
                let! version = Decode.getProp "corpusVersion" el |> Result.bind Decode.asInt
                let! seamText = Decode.strField "seamVersion" el
                let! seam = SeamVersion.tryParse seamText
                let! dialect = Decode.strField "dialect" el
                let! projector = optListField "projector" decodeProjectorCase el
                let! importer = optListField "importer" decodeImporterCase el
                let! validator = optListField "validator" decodeValidatorCase el
                let! gates = optListField "gates" decodeGateCase el
                let! packs = optListField "packs" decodePackCase el
                let! invariants = optListField "invariants" decodeInvariantCase el

                return
                    { CorpusVersion = version
                      SeamVersion = seam
                      Dialect = dialect
                      Projector = projector
                      Importer = importer
                      Validator = validator
                      Gates = gates
                      Packs = packs
                      Invariants = invariants }
        }

    /// Parse a corpus document. The host reads the file; this library performs no IO, so
    /// the same harness runs from a file, a resource, or a string a test built.
    let parseCorpus (json: string) : Result<DialectCorpus, string> =
        Json.parse json |> Result.bind decodeCorpus

    // ---- encoding ------------------------------------------------------------

    let private encodeResidue (r: ResidueExpectation) : JVal =
        JObj [ "path", JStr r.Path; "term", JStr r.Term ]

    let private encodeProjectorCase (c: ProjectorCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "source", JStr c.Source
              "bytes", JStr c.Bytes
              "hash", JStr c.Hash
              "covers", JArr(c.Covers |> List.map JStr) ]

    let private encodeImporterCase (c: ImporterCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "source", JStr c.Source
              "residue", JArr(c.Residue |> List.map encodeResidue)
              "roundTrip", JBool c.RoundTrip ]

    let private encodeValidatorCase (c: ValidatorCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "source", JStr c.Source
              "findings",
              JArr(
                  c.Findings
                  |> List.map (fun f -> JObj [ "code", JStr f.Code; "severity", JStr(Severity.encode f.Severity) ])
              )
              "projects", JBool c.Projects ]

    let private encodeGateCase (c: GateCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "threshold", JFloat c.Threshold
              "direction", JStr(GateDirection.encode c.Direction) ]

    let private encodePackCase (c: PackCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "source", JStr c.Source
              "pack",
              JObj
                  [ "id", JStr c.Pack.Id
                    "kind", JStr c.Pack.Kind
                    "version", JStr c.Pack.Version
                    "body", JStr c.Pack.Body ]
              "expect",
              (match c.Expect with
               | PackApplied bytes -> Canonical.typed "applied" [ "bytes", JStr bytes ]
               | PackRefused kind -> Canonical.typed "refused" [ "refusal", JStr kind ]) ]

    let private encodeInvariantCase (c: InvariantCase) : JVal =
        JObj
            [ "name", JStr c.Name
              "siblings",
              JArr(
                  c.Siblings
                  |> List.map (fun s ->
                      JObj [ "sibling", JStr s.Sibling; "state", JStr s.State; "source", JStr s.Source ])
              )
              "findings",
              JArr(
                  c.Findings
                  |> List.map (fun f ->
                      JObj
                          [ "invariantId", JStr f.InvariantId
                            "siblings", JArr(f.Siblings |> List.map JStr) ])
              ) ]

    /// The `invariants` key is emitted only when the family is populated.
    ///
    /// That is what keeps an existing corpus byte-identical after this seam version: a dialect
    /// declaring no invariants re-emits exactly the document it had, so the corpus pin does not
    /// move for a feature it does not use.
    let encodeCorpus (c: DialectCorpus) : JVal =
        JObj(
            [ "corpus", JStr tag
              "corpusVersion", JInt c.CorpusVersion
              "seamVersion", JStr(SeamVersion.encode c.SeamVersion)
              "dialect", JStr c.Dialect
              "projector", JArr(c.Projector |> List.map encodeProjectorCase)
              "importer", JArr(c.Importer |> List.map encodeImporterCase)
              "validator", JArr(c.Validator |> List.map encodeValidatorCase)
              "gates", JArr(c.Gates |> List.map encodeGateCase)
              "packs", JArr(c.Packs |> List.map encodePackCase) ]
            @ (match c.Invariants with
               | [] -> []
               | xs -> [ "invariants", JArr(xs |> List.map encodeInvariantCase) ])
        )

    /// Render a corpus for storage: canonical key order (Ordinal, as everywhere else in
    /// this library) with indentation, because a corpus is read by dialect authors as often
    /// as it is parsed by a harness. Deterministic — re-rendering an unchanged corpus
    /// produces identical bytes, so a diff means a change.
    let renderCorpus (c: DialectCorpus) : string =
        let sb = System.Text.StringBuilder()

        let rec go (indent: string) (v: JVal) =
            match v with
            | JObj [] -> sb.Append "{}" |> ignore
            | JObj fields ->
                let inner = indent + "  "
                sb.Append "{\n" |> ignore

                fields
                |> List.sortWith (fun (a, _) (b, _) -> System.String.CompareOrdinal(a, b))
                |> List.iteri (fun i (k, v) ->
                    if i > 0 then
                        sb.Append ",\n" |> ignore

                    sb.Append(inner).Append('"').Append(Json.escape k).Append("\": ") |> ignore
                    go inner v)

                sb.Append('\n').Append(indent).Append('}') |> ignore
            | JArr [] -> sb.Append "[]" |> ignore
            | JArr items ->
                let inner = indent + "  "
                sb.Append "[\n" |> ignore

                items
                |> List.iteri (fun i v ->
                    if i > 0 then
                        sb.Append ",\n" |> ignore

                    sb.Append inner |> ignore
                    go inner v)

                sb.Append('\n').Append(indent).Append(']') |> ignore
            | scalar -> sb.Append(Canonical.write Rendering.CrossHost scalar) |> ignore

        go "" (encodeCorpus c)
        sb.Append '\n' |> ignore
        sb.ToString()

    // ---- the harness ---------------------------------------------------------

    let private check family case code passed detail =
        { Family = family
          Case = case
          Code = code
          Passed = passed
          Detail = detail }

    let private ok family case code detail = check family case code true detail
    let private fail family case code detail = check family case code false detail

    /// Import a fixture source, or report the import failure under the family's own name.
    let private tryImport
        (d: IDialect<'Spec>)
        (family: string)
        (case: string)
        (source: string)
        : Choice<'Spec * Residue list, CheckResult> =
        match d.Import source with
        | Error f -> Choice2Of2(fail family case Codes.importFailed (ImportFailure.describe f))
        | Ok outcome -> Choice1Of2(outcome.Spec, outcome.Residue)

    // ---- universal laws ------------------------------------------------------

    /// The checks that need no fixtures: identity, seam compatibility, vocabulary shape,
    /// minting determinism, and the pack-kind refusal.
    ///
    /// These are where a dialect fails FIRST if it is going to, and none of them can be
    /// satisfied by writing a fixture — which is the point of separating them.
    let universalLaws (host: SeamVersion) (d: IDialect<'Spec>) : CheckResult list =
        let family = "universal"
        let ident = d.Identity

        let identity =
            let missing =
                [ if ident.Id = "" then
                      "id"
                  if ident.Version = "" then
                      "version"
                  if ident.PayloadFormat = "" then
                      "payloadFormat"
                  if ident.SpecHashAlgorithm = "" then
                      "specHashAlgorithm" ]

            if List.isEmpty missing then
                ok family "identity" Codes.identityIncomplete "every identity field is populated"
            else
                fail family "identity" Codes.identityIncomplete ("empty: " + String.concat ", " missing)

        let skew =
            match SeamVersion.check host ident.TargetsSeam with
            | Ok() ->
                ok
                    family
                    "identity"
                    Codes.seamSkew
                    ("targets seam "
                     + SeamVersion.encode ident.TargetsSeam
                     + ", host "
                     + SeamVersion.encode host)
            | Error r -> fail family "identity" Codes.seamSkew (SeamRefusal.describe r)

        let vocabEmpty =
            if List.isEmpty d.Vocabulary then
                fail family "vocabulary" Codes.vocabularyEmpty "the dialect publishes no vocabulary"
            else
                ok family "vocabulary" Codes.vocabularyEmpty (sprintf "%d terms" (List.length d.Vocabulary))

        let vocabDuplicate =
            let dupes =
                d.Vocabulary
                |> List.countBy id
                |> List.filter (fun (_, n) -> n > 1)
                |> List.map fst

            if List.isEmpty dupes then
                ok family "vocabulary" Codes.vocabularyDuplicate "no duplicate terms"
            else
                fail family "vocabulary" Codes.vocabularyDuplicate ("duplicated: " + String.concat ", " dupes)

        let mint =
            let probes = [ ""; "{}"; "{\"a\":1}"; ident.Id ]
            let unstable = probes |> List.filter (fun p -> d.Mint p <> d.Mint p)

            if List.isEmpty unstable then
                let hashes = probes |> List.map d.Mint |> List.distinct

                if List.length hashes = List.length (List.distinct probes) then
                    ok family "mint" Codes.mintNondeterministic "stable and discriminating over the probe set"
                else
                    fail family "mint" Codes.mintNondeterministic "distinct inputs minted to the same hash"
            else
                fail family "mint" Codes.mintNondeterministic "the same input minted two different hashes"

        [ identity; skew; vocabEmpty; vocabDuplicate; mint ]

    // ---- projector family ----------------------------------------------------

    let private runProjectorCase (d: IDialect<'Spec>) (c: ProjectorCase) : CheckResult list =
        let family = "projector"

        match tryImport d family c.Name c.Source with
        | Choice2Of2 f -> [ f ]
        | Choice1Of2(spec, _) ->
            match d.Project spec with
            | Error findings ->
                [ fail
                      family
                      c.Name
                      Codes.projectRefused
                      ("projection refused: "
                       + (findings |> List.map (fun f -> f.Code) |> String.concat ", ")) ]
            | Ok payload ->
                let bytes =
                    if payload.Body = c.Bytes then
                        ok family c.Name Codes.projectBytesDrift "canonical bytes match the pin"
                    else
                        fail family c.Name Codes.projectBytesDrift ("expected " + c.Bytes + ", got " + payload.Body)

                let hash =
                    if payload.SpecHash = c.Hash then
                        ok family c.Name Codes.projectHashDrift "spec hash matches the pin"
                    else
                        fail family c.Name Codes.projectHashDrift ("expected " + c.Hash + ", got " + payload.SpecHash)

                // The hash IS the minting rule applied to the bytes. A hash that is
                // anything else names nothing, and the composite key that carries it joins
                // two records that were never the same fit.
                let rule =
                    let minted = d.Mint payload.Body

                    if minted = payload.SpecHash then
                        ok family c.Name Codes.hashRule "hash = mint(canonical bytes)"
                    else
                        fail
                            family
                            c.Name
                            Codes.hashRule
                            ("mint(bytes) = " + minted + " but the payload carries " + payload.SpecHash)

                let identity =
                    let idOk =
                        payload.Dialect = d.Identity.Id && payload.Format = d.Identity.PayloadFormat

                    if idOk then
                        ok family c.Name Codes.identityMismatch "payload tags match the dialect identity"
                    else
                        fail
                            family
                            c.Name
                            Codes.identityMismatch
                            ("payload is tagged ("
                             + payload.Dialect
                             + ", "
                             + payload.Format
                             + ") but the dialect is ("
                             + d.Identity.Id
                             + ", "
                             + d.Identity.PayloadFormat
                             + ")")

                // Determinism is checked by repetition rather than by inspection: three
                // projections of the same spec, byte-identical, or the payload's identity
                // depends on when it was produced.
                let determinism =
                    let again =
                        [ 1..3 ]
                        |> List.map (fun _ ->
                            match d.Project spec with
                            | Ok p -> Some(p.Body, p.SpecHash)
                            | Error _ -> None)

                    if again |> List.forall (fun r -> r = Some(payload.Body, payload.SpecHash)) then
                        ok family c.Name Codes.projectorNondeterministic "three projections agree byte for byte"
                    else
                        fail
                            family
                            c.Name
                            Codes.projectorNondeterministic
                            "repeated projection produced a different payload"

                // Importing the dialect's OWN projection must reproduce it exactly. This is
                // the property that makes a payload a faithful rendering of a tree rather
                // than a lossy report about one.
                let idempotence =
                    match d.Import payload.Body with
                    | Error f ->
                        fail
                            family
                            c.Name
                            Codes.projectNotIdempotent
                            ("the dialect cannot import its own projection: " + ImportFailure.describe f)
                    | Ok back ->
                        match d.Project back.Spec with
                        | Error findings ->
                            fail
                                family
                                c.Name
                                Codes.projectNotIdempotent
                                ("re-projection refused: "
                                 + (findings |> List.map (fun f -> f.Code) |> String.concat ", "))
                        | Ok p2 when p2.Body = payload.Body && List.isEmpty back.Residue ->
                            ok family c.Name Codes.projectNotIdempotent "project ∘ import ∘ project is stable"
                        | Ok p2 when p2.Body <> payload.Body ->
                            fail
                                family
                                c.Name
                                Codes.projectNotIdempotent
                                ("re-projection drifted: " + payload.Body + " -> " + p2.Body)
                        | Ok _ ->
                            fail
                                family
                                c.Name
                                Codes.projectNotIdempotent
                                ("importing its own projection reported residue: "
                                 + (back.Residue |> List.map (fun r -> r.Term) |> String.concat ", "))

                [ bytes; hash; rule; identity; determinism; idempotence ]

    /// Distinct canonical bytes must mint distinct hashes, across the whole family.
    ///
    /// Checked against what the dialect ACTUALLY produces, not against the pins: a dialect
    /// whose hash is a constant satisfies every per-fixture comparison as soon as its
    /// corpus is regenerated — the pins move with it — and fails only here.
    let private projectorDiscrimination (d: IDialect<'Spec>) (cases: ProjectorCase list) : CheckResult list =
        let projected =
            cases
            |> List.choose (fun c ->
                match d.Import c.Source with
                | Error _ -> None
                | Ok o ->
                    match d.Project o.Spec with
                    | Error _ -> None
                    | Ok p -> Some(p.Body, p.SpecHash))
            |> List.distinctBy fst

        let collisions =
            projected
            |> List.groupBy snd
            |> List.filter (fun (_, xs) -> List.length xs > 1)
            |> List.map fst

        if List.isEmpty collisions then
            [ ok
                  "projector"
                  "*"
                  Codes.hashCollision
                  (sprintf "%d distinct payloads, all with distinct hashes" (List.length projected)) ]
        else
            [ fail
                  "projector"
                  "*"
                  Codes.hashCollision
                  ("distinct payloads share a hash: " + String.concat ", " collisions) ]

    /// The coverage gate: the published vocabulary and the fixtures' `covers` tags must be
    /// the same set. An uncovered term is an untested claim; an undeclared tag is a
    /// fixture exercising something the dialect does not admit to having.
    let private coverage (d: IDialect<'Spec>) (cases: ProjectorCase list) : CheckResult list =
        let declared = Set.ofList d.Vocabulary
        let covered = cases |> List.collect (fun c -> c.Covers) |> Set.ofList
        let uncovered = Set.difference declared covered |> Set.toList
        let undeclared = Set.difference covered declared |> Set.toList

        [ (if List.isEmpty uncovered then
               ok "projector" "*" Codes.vocabularyUncovered "every vocabulary term is exercised"
           else
               fail "projector" "*" Codes.vocabularyUncovered ("never exercised: " + String.concat ", " uncovered))
          (if List.isEmpty undeclared then
               ok "projector" "*" Codes.vocabularyUndeclared "every covered term is declared"
           else
               fail
                   "projector"
                   "*"
                   Codes.vocabularyUndeclared
                   ("covered but not declared: " + String.concat ", " undeclared)) ]

    // ---- importer family -----------------------------------------------------

    let private runImporterCase (d: IDialect<'Spec>) (c: ImporterCase) : CheckResult list =
        let family = "importer"

        match tryImport d family c.Name c.Source with
        | Choice2Of2 f -> [ f ]
        | Choice1Of2(spec, residue) ->
            // Compared as sorted sets of (path, term): a dialect is not forced into an
            // arbitrary emission order, but its own order still has to be stable, which is
            // the next check.
            let actual = residue |> List.map (fun r -> r.Path, r.Term) |> List.sort
            let expected = c.Residue |> List.map (fun r -> r.Path, r.Term) |> List.sort

            let show xs =
                xs |> List.map (fun (p, t) -> p + ":" + t) |> String.concat ", "

            let drift =
                if actual = expected then
                    ok family c.Name Codes.residueDrift (sprintf "%d residue entries, as pinned" (List.length actual))
                else
                    fail
                        family
                        c.Name
                        Codes.residueDrift
                        ("expected [" + show expected + "], got [" + show actual + "]")

            let determinism =
                let again =
                    [ 1..3 ]
                    |> List.map (fun _ ->
                        match d.Import c.Source with
                        | Ok o -> Some(o.Spec, o.Residue |> List.map (fun r -> r.Path, r.Term))
                        | Error _ -> None)

                let first = Some(spec, residue |> List.map (fun r -> r.Path, r.Term))

                if again |> List.forall (fun r -> r = first) then
                    ok family c.Name Codes.importNondeterministic "three imports agree"
                else
                    fail family c.Name Codes.importNondeterministic "repeated import produced a different outcome"

            let roundTrip =
                if not c.RoundTrip then
                    []
                else
                    match d.Project spec with
                    | Error findings ->
                        [ fail
                              family
                              c.Name
                              Codes.projectNotIdempotent
                              ("projection refused: "
                               + (findings |> List.map (fun f -> f.Code) |> String.concat ", ")) ]
                    | Ok payload ->
                        match d.Import payload.Body with
                        | Error f ->
                            [ fail
                                  family
                                  c.Name
                                  Codes.projectNotIdempotent
                                  ("cannot import its own projection: " + ImportFailure.describe f) ]
                        | Ok back ->
                            if back.Spec = spec then
                                [ ok
                                      family
                                      c.Name
                                      Codes.projectNotIdempotent
                                      "import ∘ project is the identity on the tree" ]
                            else
                                [ fail
                                      family
                                      c.Name
                                      Codes.projectNotIdempotent
                                      "importing its own projection produced a different tree" ]

            [ drift; determinism ] @ roundTrip

    // ---- validator family ----------------------------------------------------

    let private runValidatorCase (d: IDialect<'Spec>) (c: ValidatorCase) : CheckResult list =
        let family = "validator"

        match tryImport d family c.Name c.Source with
        | Choice2Of2 f -> [ f ]
        | Choice1Of2(spec, _) ->
            let findings = d.Validate spec

            let actual =
                findings |> List.map (fun f -> f.Code, Severity.encode f.Severity) |> List.sort

            let expected =
                c.Findings
                |> List.map (fun f -> f.Code, Severity.encode f.Severity)
                |> List.sort

            let show xs =
                xs |> List.map (fun (c, s) -> c + "/" + s) |> String.concat ", "

            let drift =
                if actual = expected then
                    ok family c.Name Codes.validatorDrift (sprintf "%d findings, as pinned" (List.length actual))
                else
                    fail
                        family
                        c.Name
                        Codes.validatorDrift
                        ("expected [" + show expected + "], got [" + show actual + "]")

            let determinism =
                let again = [ 1..3 ] |> List.map (fun _ -> d.Validate spec)

                if again |> List.forall (fun f -> f = findings) then
                    ok family c.Name Codes.validatorNondeterministic "three validations agree, in the same order"
                else
                    fail family c.Name Codes.validatorNondeterministic "repeated validation produced different findings"

            // The projector and the validator must agree about what "valid" means. This is
            // the seam's single most load-bearing law: it is what lets a caller trust that
            // anything holding a payload holds one whose spec passed.
            let blocks = findings |> List.exists Finding.blocks

            let agreement =
                match d.Project spec, blocks, c.Projects with
                | Ok _, true, _ ->
                    fail
                        family
                        c.Name
                        Codes.projectAcceptsBlocking
                        "the spec has a blocking finding and projected anyway"
                | Ok _, false, true -> ok family c.Name Codes.projectAcceptsBlocking "valid, and projects"
                | Ok _, false, false ->
                    fail family c.Name Codes.projectRefusesValid "the fixture pins a refusal but the spec projected"
                | Error _, true, false -> ok family c.Name Codes.projectAcceptsBlocking "blocking, and refused"
                | Error _, true, true ->
                    fail
                        family
                        c.Name
                        Codes.projectRefusesValid
                        "the fixture pins a projection but the spec was refused"
                | Error findings', false, _ ->
                    fail
                        family
                        c.Name
                        Codes.projectRefusesValid
                        ("no blocking finding, but projection refused with: "
                         + (findings' |> List.map (fun f -> f.Code) |> String.concat ", "))

            [ drift; determinism; agreement ]

    // ---- gates family --------------------------------------------------------

    let private runGates (d: IDialect<'Spec>) (cases: GateCase list) : CheckResult list =
        let family = "gates"
        let declared = d.DefaultGates

        let duplicates =
            declared
            |> List.countBy (fun g -> g.Request.Name)
            |> List.filter (fun (_, n) -> n > 1)
            |> List.map fst

        let dupCheck =
            if List.isEmpty duplicates then
                ok family "*" Codes.gateDuplicate (sprintf "%d gates, names distinct" (List.length declared))
            else
                fail family "*" Codes.gateDuplicate ("duplicated gate names: " + String.concat ", " duplicates)

        let perCase =
            cases
            |> List.map (fun c ->
                match declared |> List.tryFind (fun g -> g.Request.Name = c.Name) with
                | None ->
                    fail
                        family
                        c.Name
                        Codes.gateMissing
                        ("the dialect declares no default gate named '" + c.Name + "'")
                | Some g ->
                    if g.Request.Threshold = c.Threshold && g.Request.Direction = c.Direction then
                        ok family c.Name Codes.gateDrift "threshold and direction match the pin"
                    else
                        fail
                            family
                            c.Name
                            Codes.gateDrift
                            (sprintf
                                "expected %g %s, got %g %s"
                                c.Threshold
                                (GateDirection.encode c.Direction)
                                g.Request.Threshold
                                (GateDirection.encode g.Request.Direction)))

        dupCheck :: perCase

    // ---- packs family --------------------------------------------------------

    let private refusalKind (r: PackRefusal) : string =
        match r with
        | UnknownPackKind _ -> "unknownKind"
        | MalformedPack _ -> "malformed"
        | PackConflicts _ -> "conflicts"

    let private runPackCase (d: IDialect<'Spec>) (c: PackCase) : CheckResult list =
        let family = "packs"

        match tryImport d family c.Name c.Source with
        | Choice2Of2 f -> [ f ]
        | Choice1Of2(spec, _) ->
            let applied = d.ApplyPack c.Pack spec

            let determinism =
                let again = [ 1..3 ] |> List.map (fun _ -> d.ApplyPack c.Pack spec)

                if again |> List.forall (fun r -> r = applied) then
                    ok family c.Name Codes.packNondeterministic "three applications agree"
                else
                    fail family c.Name Codes.packNondeterministic "repeated application produced a different result"

            let outcome =
                match applied, c.Expect with
                | Ok spec', PackApplied expectedBytes ->
                    match d.Project spec' with
                    | Error findings ->
                        fail
                            family
                            c.Name
                            Codes.packOutcomeDrift
                            ("the packed spec does not project: "
                             + (findings |> List.map (fun f -> f.Code) |> String.concat ", "))
                    | Ok payload when payload.Body = expectedBytes ->
                        ok family c.Name Codes.packOutcomeDrift "the packed spec projects to the pinned bytes"
                    | Ok payload ->
                        fail
                            family
                            c.Name
                            Codes.packOutcomeDrift
                            ("expected " + expectedBytes + ", got " + payload.Body)
                | Ok _, PackRefused kind ->
                    fail
                        family
                        c.Name
                        Codes.packOutcomeDrift
                        ("expected a '" + kind + "' refusal, but the pack applied")
                | Error r, PackRefused kind when refusalKind r = kind ->
                    ok family c.Name Codes.packOutcomeDrift ("refused: " + PackRefusal.describe r)
                | Error r, PackRefused kind ->
                    fail
                        family
                        c.Name
                        Codes.packOutcomeDrift
                        ("expected a '"
                         + kind
                         + "' refusal, got '"
                         + refusalKind r
                         + "': "
                         + PackRefusal.describe r)
                | Error r, PackApplied _ ->
                    fail family c.Name Codes.packOutcomeDrift ("expected the pack to apply: " + PackRefusal.describe r)

            // A kind no dialect can have declared, synthesised by the harness rather than
            // taken from a fixture — a dialect must not be able to satisfy this law by
            // declining to write the fixture that would have caught it. A pack that
            // silently does nothing is indistinguishable from one that worked.
            let undeclared =
                let probe =
                    { Id = "conformance-probe"
                      Kind = "conformance-undeclared-kind"
                      Version = "1"
                      Body = "{}" }

                match d.ApplyPack probe spec with
                | Error(UnknownPackKind _) ->
                    ok family c.Name Codes.packUndeclaredAccepted "an undeclared kind is refused"
                | Error other ->
                    fail
                        family
                        c.Name
                        Codes.packUndeclaredAccepted
                        ("refused, but not as an unknown kind: " + PackRefusal.describe other)
                | Ok _ -> fail family c.Name Codes.packUndeclaredAccepted "an undeclared pack kind was applied"

            [ determinism; outcome; undeclared ]

    // ---- invariants family (the optional obligation) -------------------------

    /// The shape checks over a dialect's declared comparators.
    ///
    /// Run for every dialect, including one declaring none — and a dialect declaring none
    /// PASSES, visibly, with a check that says so. A silently-skipped family reads in a report
    /// exactly like a family that was never authored, and the difference between "this dialect
    /// has no cross-sibling invariants" and "somebody forgot" is the whole thing a reader of
    /// an optional obligation needs to know.
    let private invariantShape (d: IDialect<'Spec>) : CheckResult list =
        let family = "invariants"
        let declared = Seam.invariants d

        if List.isEmpty declared then
            [ ok family "*" Codes.invariantDuplicate "the dialect declares no invariant comparators (optional)" ]
        else
            let dupes =
                declared
                |> List.countBy (fun c -> c.Id)
                |> List.filter (fun (_, n) -> n > 1)
                |> List.map fst

            let dupCheck =
                if List.isEmpty dupes then
                    ok
                        family
                        "*"
                        Codes.invariantDuplicate
                        (sprintf "%d invariants, ids distinct" (List.length declared))
                else
                    fail family "*" Codes.invariantDuplicate ("duplicated invariant ids: " + String.concat ", " dupes)

            // An id nobody can name cannot be declared a divergence from, and a description
            // nobody wrote leaves the person choosing invariants guessing what one means.
            let unnamed =
                declared
                |> List.filter (fun c -> c.Id.Trim() = "" || c.Description.Trim() = "")
                |> List.map (fun c -> if c.Id.Trim() = "" then "<empty id>" else c.Id)

            let describedCheck =
                if List.isEmpty unnamed then
                    ok family "*" Codes.invariantUndescribed "every invariant carries an id and a description"
                else
                    fail
                        family
                        "*"
                        Codes.invariantUndescribed
                        ("missing an id or a description: " + String.concat ", " unnamed)

            [ dupCheck; describedCheck ]

    /// Project one fixture sibling through the dialect's own projector.
    ///
    /// Through the projector rather than from a literal, deliberately: a comparator's real
    /// input is what the dialect PRODUCES, so a fixture that handed it hand-written bytes
    /// would certify the comparator against a payload the dialect might never emit.
    let private projectSibling
        (d: IDialect<'Spec>)
        (case: string)
        (s: InvariantSibling)
        : Choice<ProjectedState, CheckResult> =
        let family = "invariants"

        match d.Import s.Source with
        | Error f -> Choice2Of2(fail family case Codes.importFailed (ImportFailure.describe f))
        | Ok outcome ->
            match d.Project outcome.Spec with
            | Error findings ->
                Choice2Of2(
                    fail
                        family
                        case
                        Codes.projectRefused
                        ("projection refused: "
                         + (findings |> List.map (fun f -> f.Code) |> String.concat ", "))
                )
            | Ok payload ->
                Choice1Of2
                    { Sibling = s.Sibling
                      State = s.State
                      Payload = payload }

    let private runInvariantCase (d: IDialect<'Spec>) (c: InvariantCase) : CheckResult list =
        let family = "invariants"
        let declared = Seam.invariants d
        let declaredIds = declared |> List.map (fun x -> x.Id) |> Set.ofList

        let named = c.Findings |> List.map (fun f -> f.InvariantId) |> List.distinct

        let undeclared = named |> List.filter (fun id -> not (Set.contains id declaredIds))

        if not (List.isEmpty undeclared) then
            // A fixture naming an invariant the dialect does not declare is the optional
            // obligation's version of the coverage gate's undeclared-term failure: the corpus
            // is testing something this dialect does not admit to having.
            [ fail
                  family
                  c.Name
                  Codes.invariantUndeclared
                  ("the fixture expects findings from invariants this dialect does not declare: "
                   + String.concat ", " undeclared) ]
        else

            let projected = c.Siblings |> List.map (projectSibling d c.Name) |> List.ofSeq

            match
                projected
                |> List.tryPick (function
                    | Choice2Of2 f -> Some f
                    | _ -> None)
            with
            | Some failure -> [ failure ]
            | None ->
                let states =
                    projected
                    |> List.choose (function
                        | Choice1Of2 p -> Some p
                        | Choice2Of2 _ -> None)

                // Run through the manager's own evaluation host, exactly as a caller would — so
                // the id-stamping and the ordering are part of what is certified rather than
                // conveniences the harness reimplements.
                let findings = Forest.evaluate declared states

                let actual =
                    findings
                    |> List.map (fun f -> f.InvariantId, String.concat "," f.Siblings)
                    |> List.sort

                let expected =
                    c.Findings
                    |> List.map (fun f ->
                        f.InvariantId, (f.Siblings |> List.distinct |> List.sort |> String.concat ","))
                    |> List.sort

                let show xs =
                    if List.isEmpty xs then
                        "none"
                    else
                        xs |> List.map (fun (i, s) -> i + "@[" + s + "]") |> String.concat ", "

                let drift =
                    if actual = expected then
                        ok family c.Name Codes.invariantDrift (sprintf "%d findings, as pinned" (List.length actual))
                    else
                        fail family c.Name Codes.invariantDrift ("expected " + show expected + ", got " + show actual)

                let determinism =
                    let again = [ 1..3 ] |> List.map (fun _ -> Forest.evaluate declared states)

                    if again |> List.forall (fun f -> f = findings) then
                        ok family c.Name Codes.invariantNondeterministic "three comparisons agree, in the same order"
                    else
                        fail
                            family
                            c.Name
                            Codes.invariantNondeterministic
                            "repeated comparison produced different findings"

                [ drift; determinism ]

    // ---- the run -------------------------------------------------------------

    /// Run a dialect against a corpus, as a host implementing `host` would.
    ///
    /// Ordered universal-laws-first so a report reads top-down: a dialect whose identity or
    /// seam version is wrong has nothing to learn from thirty fixture failures underneath.
    let runWith (host: SeamVersion) (d: IDialect<'Spec>) (corpus: DialectCorpus) : CheckResult list =
        let dialectTag =
            if corpus.Dialect = d.Identity.Id then
                [ ok "universal" "corpus" Codes.identityMismatch "the corpus names this dialect" ]
            else
                [ fail
                      "universal"
                      "corpus"
                      Codes.identityMismatch
                      ("the corpus is for '"
                       + corpus.Dialect
                       + "' but the dialect is '"
                       + d.Identity.Id
                       + "'") ]

        universalLaws host d
        @ dialectTag
        @ coverage d corpus.Projector
        @ (corpus.Projector |> List.collect (runProjectorCase d))
        @ projectorDiscrimination d corpus.Projector
        @ (corpus.Importer |> List.collect (runImporterCase d))
        @ (corpus.Validator |> List.collect (runValidatorCase d))
        @ runGates d corpus.Gates
        @ (corpus.Packs |> List.collect (runPackCase d))
        @ invariantShape d
        @ (corpus.Invariants |> List.collect (runInvariantCase d))

    /// Run against the seam version this build implements.
    let run (d: IDialect<'Spec>) (corpus: DialectCorpus) : CheckResult list = runWith SeamVersion.current d corpus

    /// The whole run as one verdict, every failure named by its code.
    let certify (d: IDialect<'Spec>) (corpus: DialectCorpus) : Result<unit, string> =
        match run d corpus |> List.filter (fun r -> not r.Passed) with
        | [] -> Ok()
        | failures ->
            failures
            |> List.map (fun r -> r.Code + " @ " + r.Family + "/" + r.Case + ": " + r.Detail)
            |> String.concat "; "
            |> Error

    /// The codes a run reported as FAILING — what a go-red self-test asserts on. A harness
    /// that has never been seen to fail is a harness nobody has tested.
    let failedCodes (results: CheckResult list) : string list =
        results
        |> List.filter (fun r -> not r.Passed)
        |> List.map (fun r -> r.Code)
        |> List.distinct
        |> List.sort

    /// A one-line-per-check report, for a host that wants to print one.
    let report (results: CheckResult list) : string list =
        results
        |> List.map (fun r ->
            (if r.Passed then "pass  " else "FAIL  ")
            + r.Code
            + " @ "
            + r.Family
            + "/"
            + r.Case
            + ": "
            + r.Detail)
