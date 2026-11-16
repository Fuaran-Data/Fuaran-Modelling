// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.CorpusFixtures

open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam

// ============================================================================
//  The reference dialect's conformance corpus, authored here and emitted to
//  `conformance/reference-linear.json`.
//
//  The split between what is AUTHORED and what is COMPUTED is deliberate:
//
//    * expectations that are CLAIMS — which findings a document produces, what
//      an importer must report as residue, what a pack must do — are written
//      out by hand, because a claim generated from the implementation agrees
//      with any implementation;
//    * expectations that are TRANSCRIPTION — the exact canonical bytes and the
//      hash over them — are computed at emit time, because copying forty
//      characters of JSON by hand introduces errors that look like drift.
//
//  The committed corpus is the pin either way: a test re-emits it and compares
//  byte for byte, so a projector change shows up as a diff a human has to
//  accept rather than as a silently-regenerated expectation.
// ============================================================================

let private projected (source: string) : SpecPayload =
    match ReferenceDialect.import source with
    | Error f -> failwithf "fixture source does not import: %s" (ImportFailure.describe f)
    | Ok outcome ->
        match ReferenceDialect.project outcome.Spec with
        | Error findings -> failwithf "fixture source does not project: %A" (findings |> List.map (fun f -> f.Code))
        | Ok payload -> payload

let private projectorCase (name: string) (source: string) (covers: string list) : ProjectorCase =
    let payload = projected source

    { Name = name
      Source = source
      Bytes = payload.Body
      Hash = payload.SpecHash
      Covers = covers }

// ---- the authored sources ---------------------------------------------------

/// The smallest document that projects: one response, one term, everything else
/// defaulted.
let minimalSource = """{"response":"y","terms":[{"name":"x"}]}"""

/// The same content, authored in a different key order and with whitespace. A
/// canonicalising projector must produce identical bytes from both — this is the
/// property a content-addressed identity actually rests on, and it is the one most
/// easily lost by an emitter that preserves authoring order.
let permutedSource = """{ "terms" : [ { "name" : "x" } ] , "response" : "y" }"""

/// Every modelled term, so the coverage gate has something to be satisfied by.
let fullSource =
    """{"response":"sales","intercept":false,"terms":[{"name":"price","transform":{"$type":"log"}},{"name":"promo","transform":{"$type":"lag","periods":2},"bound":{"lower":0,"upper":1}},{"name":"trend"}]}"""

/// A document written for something else: two properties this dialect does not model,
/// a term property it does not model, and a transform it has never heard of.
let foreignSource =
    """{"response":"y","tool":"another-authoring-surface","terms":[{"name":"x","transform":{"$type":"seasonalWindow","width":7},"prior":"wide"}],"notes":"exported from elsewhere"}"""

let twoTermSource = """{"response":"y","terms":[{"name":"x1"},{"name":"x2"}]}"""

// ---- families ---------------------------------------------------------------

let projectorCases: ProjectorCase list =
    [ projectorCase "minimal" minimalSource [ "response"; "term"; "transform:identity" ]
      projectorCase "authoring-order-permuted" permutedSource [ "response"; "term" ]
      projectorCase
          "every-modelled-term"
          fullSource
          [ "response"; "intercept"; "term"; "transform:log"; "transform:lag"; "bound" ] ]

let importerCases: ImporterCase list =
    [ { Name = "clean-document"
        Source = minimalSource
        Residue = []
        RoundTrip = true }
      { Name = "foreign-document"
        Source = foreignSource
        Residue =
          [ { Path = "/"; Term = "tool" }
            { Path = "/"; Term = "notes" }
            { Path = "/terms/0/transform"
              Term = "seasonalWindow" }
            { Path = "/terms/0"; Term = "prior" } ]
        RoundTrip = true }
      { Name = "already-canonical"
        Source = (projected fullSource).Body
        Residue = []
        RoundTrip = true } ]

let validatorCases: ValidatorCase list =
    [ { Name = "clean"
        Source = minimalSource
        Findings = []
        Projects = true }
      { Name = "advisory-does-not-block"
        Source = """{"response":"y","intercept":false,"terms":[{"name":"x"}]}"""
        Findings =
          [ { Code = "REF-NO-INTERCEPT"
              Severity = Advisory } ]
        Projects = true }
      { Name = "no-terms"
        Source = """{"response":"y","terms":[]}"""
        Findings =
          [ { Code = "REF-NO-TERMS"
              Severity = Blocking } ]
        Projects = false }
      { Name = "empty-response"
        Source = """{"response":"","terms":[{"name":"x"}]}"""
        Findings =
          [ { Code = "REF-RESPONSE-EMPTY"
              Severity = Blocking } ]
        Projects = false }
      { Name = "duplicate-term"
        Source = """{"response":"y","terms":[{"name":"x"},{"name":"x"}]}"""
        Findings =
          [ { Code = "REF-DUPLICATE-TERM"
              Severity = Blocking } ]
        Projects = false }
      { Name = "term-names-the-response"
        Source = """{"response":"y","terms":[{"name":"y"}]}"""
        Findings =
          [ { Code = "REF-TERM-IS-RESPONSE"
              Severity = Blocking } ]
        Projects = false }
      { Name = "negative-lag"
        Source = """{"response":"y","terms":[{"name":"x","transform":{"$type":"lag","periods":-1}}]}"""
        Findings =
          [ { Code = "REF-LAG-NEGATIVE"
              Severity = Blocking } ]
        Projects = false }
      { Name = "inverted-bound"
        Source = """{"response":"y","terms":[{"name":"x","bound":{"lower":1,"upper":0}}]}"""
        Findings =
          [ { Code = "REF-BOUND-INVERTED"
              Severity = Blocking } ]
        Projects = false }
      { Name = "several-findings-at-once"
        Source = """{"response":"y","intercept":false,"terms":[{"name":"x"},{"name":"x"}]}"""
        Findings =
          [ { Code = "REF-DUPLICATE-TERM"
              Severity = Blocking }
            { Code = "REF-NO-INTERCEPT"
              Severity = Advisory } ]
        Projects = false } ]

let gateCases: GateCase list =
    [ { Name = "termCount"
        Threshold = 1.0
        Direction = AtLeast }
      { Name = "residualScale"
        Threshold = 1.0
        Direction = AtMost } ]

/// The bytes a packed spec projects to — computed, for the same reason the projector
/// family's bytes are.
let private packedBytes (source: string) (pack: Pack) : string =
    match ReferenceDialect.import source with
    | Error f -> failwithf "pack fixture source does not import: %s" (ImportFailure.describe f)
    | Ok outcome ->
        match ReferenceDialect.applyPack pack outcome.Spec with
        | Error r -> failwithf "pack fixture does not apply: %s" (PackRefusal.describe r)
        | Ok packed ->
            match ReferenceDialect.project packed with
            | Error findings -> failwithf "packed spec does not project: %A" (findings |> List.map (fun f -> f.Code))
            | Ok payload -> payload.Body

let private boundsPack =
    { Id = "bounds-1"
      Kind = "bounds"
      Version = "1"
      Body = """{"bounds":{"x1":{"lower":0,"upper":2}}}""" }

let private transformsPack =
    { Id = "transforms-1"
      Kind = "transforms"
      Version = "1"
      Body = """{"transforms":{"x2":{"$type":"lag","periods":3}}}""" }

let packCases: PackCase list =
    [ { Name = "bounds-applied"
        Source = twoTermSource
        Pack = boundsPack
        Expect = PackApplied(packedBytes twoTermSource boundsPack) }
      { Name = "transforms-applied"
        Source = twoTermSource
        Pack = transformsPack
        Expect = PackApplied(packedBytes twoTermSource transformsPack) }
      { Name = "undeclared-kind"
        Source = twoTermSource
        Pack =
          { Id = "hierarchy-1"
            Kind = "hierarchy"
            Version = "1"
            Body = "{}" }
        Expect = PackRefused "unknownKind" }
      { Name = "bounds-for-a-term-that-is-not-there"
        Source = twoTermSource
        Pack =
          { Id = "bounds-2"
            Kind = "bounds"
            Version = "1"
            Body = """{"bounds":{"absent":{"lower":0,"upper":1}}}""" }
        Expect = PackRefused "conflicts" }
      { Name = "malformed-body"
        Source = twoTermSource
        Pack =
          { Id = "bounds-3"
            Kind = "bounds"
            Version = "1"
            Body = """{"not-bounds":{}}""" }
        Expect = PackRefused "malformed" } ]

// ---- the invariants family --------------------------------------------------

/// Two documents that agree on how the shared term is transformed, and one that does not.
/// Neutral vocabulary throughout — this dialect has no subject matter, so its fixtures name
/// none either.
let houseSource =
    """{"response":"y","terms":[{"name":"shared","transform":{"$type":"log"}},{"name":"local-a"}]}"""

let agreeingSource =
    """{"response":"y","terms":[{"name":"shared","transform":{"$type":"log"}},{"name":"local-b"}]}"""

let divergingSource =
    """{"response":"y","terms":[{"name":"shared","transform":{"$type":"lag","periods":1}}]}"""

/// The invariant fixtures.
///
/// The clean case is not optional decoration. A comparator that never fires satisfies every
/// case expecting no finding, so a family with only clean cases certifies silence; a family
/// with only dirty ones certifies a comparator that always fires. Both are pinned, and the
/// ninth mutant is what proves the pair actually discriminates.
let invariantCases: InvariantCase list =
    [ { Name = "siblings-agree"
        Siblings =
          [ { Sibling = "unit-a"
              State = "s1"
              Source = houseSource }
            { Sibling = "unit-b"
              State = "s1"
              Source = agreeingSource } ]
        Findings = [] }
      { Name = "one-sibling-transforms-a-shared-term-differently"
        Siblings =
          [ { Sibling = "unit-a"
              State = "s1"
              Source = houseSource }
            { Sibling = "unit-b"
              State = "s1"
              Source = agreeingSource }
            { Sibling = "unit-c"
              State = "s1"
              Source = divergingSource } ]
        Findings =
          [ { InvariantId = ReferenceDialect.transformConsistencyId
              Siblings = [ "unit-a"; "unit-b"; "unit-c" ] } ] }
      { Name = "a-term-in-one-sibling-only-is-not-a-disagreement"
        Siblings =
          [ { Sibling = "unit-a"
              State = "s1"
              Source = houseSource }
            { Sibling = "unit-b"
              State = "s1"
              Source = divergingSource } ]
        Findings =
          [ { InvariantId = ReferenceDialect.transformConsistencyId
              Siblings = [ "unit-a"; "unit-b" ] } ] } ]

/// The reference dialect's corpus, whole.
let corpus: DialectCorpus =
    { CorpusVersion = Conformance.corpusVersion
      SeamVersion = SeamVersion.current
      Dialect = ReferenceDialect.dialectId
      Projector = projectorCases
      Importer = importerCases
      Validator = validatorCases
      Gates = gateCases
      Packs = packCases
      Invariants = invariantCases }

/// The corpus as it is stored, byte for byte.
let rendered () : string = Conformance.renderCorpus corpus

/// Where the corpus lives, relative to the repository root.
let corpusPath = "conformance/reference-linear.json"

/// The repository root, found by walking up from the running binary.
///
/// The corpus is a repository artefact rather than an embedded resource on purpose: it
/// is meant to be read, diffed, and consumed by dialects written in other languages, so
/// it has to exist as a file someone can open.
let repoRoot () =
    let rec up (dir: System.IO.DirectoryInfo) =
        if isNull (box dir) then
            failwith "could not locate the repository root (no Fuaran.Modelling.slnx above the running binary)"
        elif System.IO.File.Exists(System.IO.Path.Combine(dir.FullName, "Fuaran.Modelling.slnx")) then
            dir.FullName
        else
            up dir.Parent

    up (System.IO.DirectoryInfo System.AppContext.BaseDirectory)

let corpusFile () =
    System.IO.Path.Combine(repoRoot (), corpusPath)

/// Write the corpus out. Newline-exact (LF), because the file is compared byte for byte
/// by the test that keeps it honest.
let emit () : string =
    let path = corpusFile ()

    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName path: string)
    |> ignore

    System.IO.File.WriteAllText(path, rendered ())
    path
