// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ComputeCorpusFixtures

open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam
open Fuaran.Modelling.Compute
open Fuaran.Compute

// ============================================================================
//  The Compute dialect's conformance corpus, authored here and emitted to
//  `conformance/compute-fit.json` — the same split as the reference dialect's:
//  claims (findings, residue, pack outcomes) written by hand, transcription
//  (canonical bytes and hashes) computed at emit time, and the committed file the
//  pin either way.
//
//  Most sources are built as typed specifications and rendered through the
//  dialect, because the pipeline document is Compute's wire and hand-writing it
//  would test the author's spelling rather than the dialect. The two that are
//  hand-written are hand-written on purpose: one is authored in another member
//  order with whitespace, and one carries what the codec does not read.
// ============================================================================

let private col (name: string) : string * ColExpr = name, Col name

/// One response on one predictor, ordinary least squares, everything else Compute's defaults.
let minimalSpec: ComputeSpec =
    { Regressors = []
      Fit = FitSpec.ols "sales" [ col "price" ] }

/// A regressor pipeline, and every optional member of the fit: weights, an offset, a ridge
/// penalty, sandwich standard errors, grouping and a non-default output.
let fullSpec: ComputeSpec =
    { Regressors = [ Derive("net_price", Binary(Sub, Col "price", Col "discount")) ]
      Fit =
        { FitSpec.ols "sales" [ col "net_price"; col "advertising" ] with
            GroupBy = [ "region" ]
            Weights = Some "stores"
            Offset = Some(Col "baseline")
            Ridge = Some(Slot.Lit 0.5)
            StdErrors = FitStdErrors.HC1
            Output = FitOutput.Summary } }

/// A generalised linear model: a count response under the Poisson family and the log link.
let poissonSpec: ComputeSpec =
    { Regressors = []
      Fit =
        { FitSpec.ols "visits" [ col "price" ] with
            Family = FitFamily.Poisson
            Link = FitLink.Log } }

let minimalSource = ComputeDialect.render minimalSpec
let fullSource = ComputeDialect.render fullSpec
let poissonSource = ComputeDialect.render poissonSpec

/// The minimal specification, authored in another member order and with whitespace. A
/// canonicalising projector produces identical bytes from both, and the hash a registry keys on
/// rests on exactly that.
let permutedSource =
    """[ { "output" : "coefficients", "response" : "sales", "$type" : "fit",
          "predictors" : [ { "expr" : { "name" : "price", "$type" : "col" }, "name" : "price" } ],
          "link" : "identity", "intercept" : true, "groupBy" : [ ], "family" : "gaussian",
          "control" : { "tol" : 1e-8, "maxIter" : 25 } } ]"""

/// A document written for something else: a step member and a control member the codec does
/// not read, and a scoring step after the fit.
let foreignSource =
    """[{"$type":"derive","name":"net_price","expr":{"$type":"binary","op":"sub","left":{"$type":"col","name":"price"},"right":{"$type":"col","name":"discount"}},"note":"prepared upstream"},{"$type":"fit","response":"sales","predictors":[{"name":"net_price","expr":{"$type":"col","name":"net_price"}}],"intercept":true,"groupBy":[],"family":"gaussian","link":"identity","control":{"maxIter":25,"tol":1e-8,"budget":"wide"},"output":"coefficients"},{"$type":"predict","fit":{"control":{"maxIter":25,"tol":1E-08},"family":"gaussian","groupBy":[],"intercept":true,"link":"identity","output":"coefficients","predictors":[{"expr":{"$type":"col","name":"net_price"},"name":"net_price"}],"response":"sales"},"interval":{"kind":"prediction","level":0.95},"model":{"from":"training","source":{"ref":"history","schema":[]}},"scale":"response"}]"""

let private projected (spec: ComputeSpec) : SpecPayload =
    match ComputeDialect.project spec with
    | Error findings -> failwithf "fixture spec does not project: %A" (findings |> List.map (fun f -> f.Code))
    | Ok payload -> payload

let private projectorCase (name: string) (source: string) (covers: string list) : ProjectorCase =
    let spec =
        match ComputeDialect.import source with
        | Error f -> failwithf "fixture source does not import: %s" (ImportFailure.describe f)
        | Ok outcome -> outcome.Spec

    let payload = projected spec

    { Name = name
      Source = source
      Bytes = payload.Body
      Hash = payload.SpecHash
      Covers = covers }

let projectorCases: ProjectorCase list =
    [ projectorCase
          "minimal"
          minimalSource
          [ "response"; "predictor"; "intercept"; "family"; "link"; "control"; "output" ]
      projectorCase "authoring-order-permuted" permutedSource [ "response"; "predictor" ]
      projectorCase
          "every-optional-member"
          fullSource
          [ "regressors"
            "response"
            "predictor"
            "groupBy"
            "weights"
            "offset"
            "ridge"
            "stdErrors"
            "output" ]
      projectorCase "a-generalised-linear-model" poissonSource [ "family"; "link" ] ]

let importerCases: ImporterCase list =
    [ { Name = "clean-document"
        Source = minimalSource
        Residue = []
        RoundTrip = true }
      { Name = "foreign-document"
        Source = foreignSource
        Residue =
          [ { Path = "/0"; Term = "note" }
            { Path = "/1/control"; Term = "budget" }
            { Path = "/2"; Term = "predict" } ]
        RoundTrip = true }
      { Name = "already-canonical"
        Source = (projected fullSpec).Body
        Residue = []
        RoundTrip = true } ]

let private source (spec: ComputeSpec) : string = ComputeDialect.render spec

let private withFit (f: FitSpec -> FitSpec) (spec: ComputeSpec) : ComputeSpec = { spec with Fit = f spec.Fit }

let validatorCases: ValidatorCase list =
    [ { Name = "clean"
        Source = minimalSource
        Findings = []
        Projects = true }
      { Name = "advisory-does-not-block"
        Source = source (minimalSpec |> withFit (fun f -> { f with Intercept = false }))
        Findings =
          [ { Code = "CMP-NO-INTERCEPT"
              Severity = Advisory } ]
        Projects = true }
      { Name = "no-design-column"
        Source =
          source (
              minimalSpec
              |> withFit (fun f ->
                  { f with
                      Intercept = false
                      Predictors = [] })
          )
        Findings =
          [ { Code = "CMP-NO-DESIGN"
              Severity = Blocking } ]
        Projects = false }
      { Name = "empty-response"
        Source = source (minimalSpec |> withFit (fun f -> { f with Response = "" }))
        Findings =
          [ { Code = "CMP-RESPONSE-EMPTY"
              Severity = Blocking } ]
        Projects = false }
      { Name = "unnamed-predictor"
        Source =
          source (
              minimalSpec
              |> withFit (fun f ->
                  { f with
                      Predictors = [ "", Col "price" ] })
          )
        Findings =
          [ { Code = "CMP-PREDICTOR-UNNAMED"
              Severity = Blocking } ]
        Projects = false }
      { Name = "a-term-named-twice"
        Source =
          source (
              minimalSpec
              |> withFit (fun f ->
                  { f with
                      Predictors = [ col "price"; "price", Col "discount" ] })
          )
        Findings =
          [ { Code = "CMP-PREDICTOR-DUPLICATE"
              Severity = Blocking } ]
        Projects = false }
      { Name = "ridge-under-a-non-gaussian-family"
        Source = source (poissonSpec |> withFit (fun f -> { f with Ridge = Some(Slot.Lit 1.0) }))
        Findings =
          [ { Code = "CMP-RIDGE-FAMILY"
              Severity = Blocking } ]
        Projects = false }
      { Name = "a-column-the-walk-proves-absent"
        Source =
          source
              { Regressors = [ Project [ "sales", "sales"; "price", "price" ] ]
                Fit = FitSpec.ols "sales" [ col "price"; col "advertising" ] }
        Findings =
          [ { Code = "CMP-COLUMN-ABSENT"
              Severity = Blocking } ]
        Projects = false }
      { Name = "several-findings-at-once"
        Source =
          source (
              minimalSpec
              |> withFit (fun f ->
                  { f with
                      Intercept = false
                      Predictors = [ col "price"; col "price" ] })
          )
        Findings =
          [ { Code = "CMP-PREDICTOR-DUPLICATE"
              Severity = Blocking }
            { Code = "CMP-NO-INTERCEPT"
              Severity = Advisory } ]
        Projects = false } ]

let gateCases: GateCase list =
    [ { Name = ComputeDialect.rankDeficientGate
        Threshold = 0.0
        Direction = AtMost }
      { Name = ComputeDialect.notConvergedGate
        Threshold = 0.0
        Direction = AtMost } ]

let private packedBytes (spec: ComputeSpec) (pack: Pack) : string =
    match ComputeDialect.applyPack pack spec with
    | Error r -> failwithf "pack fixture does not apply: %s" (PackRefusal.describe r)
    | Ok packed -> (projected packed).Body

let private controlPack =
    { Id = "control-1"
      Kind = ComputeDialect.controlPackKind
      Version = "1"
      Body = """{"maxIter":50,"tol":1e-10}""" }

let private ridgePack =
    { Id = "ridge-1"
      Kind = ComputeDialect.ridgePackKind
      Version = "1"
      Body = """{"penalty":0.25}""" }

let packCases: PackCase list =
    [ { Name = "control-applied"
        Source = minimalSource
        Pack = controlPack
        Expect = PackApplied(packedBytes minimalSpec controlPack) }
      { Name = "ridge-applied"
        Source = minimalSource
        Pack = ridgePack
        Expect = PackApplied(packedBytes minimalSpec ridgePack) }
      { Name = "undeclared-kind"
        Source = minimalSource
        Pack =
          { Id = "priors-1"
            Kind = "priors"
            Version = "1"
            Body = "{}" }
        Expect = PackRefused "unknownKind" }
      { Name = "ridge-for-a-family-that-takes-none"
        Source = poissonSource
        Pack = ridgePack
        Expect = PackRefused "conflicts" }
      { Name = "control-pack-setting-nothing"
        Source = minimalSource
        Pack =
          { Id = "control-2"
            Kind = ComputeDialect.controlPackKind
            Version = "1"
            Body = """{"budget":3}""" }
        Expect = PackRefused "malformed" }
      { Name = "control-out-of-range"
        Source = minimalSource
        Pack =
          { Id = "control-3"
            Kind = ComputeDialect.controlPackKind
            Version = "1"
            Body = """{"maxIter":0}""" }
        Expect = PackRefused "malformed" } ]

/// The Compute dialect's corpus, whole. It declares no invariants, so it carries no invariant
/// family, and the harness reports the declined obligation as a passing check.
let corpus: DialectCorpus =
    { CorpusVersion = Conformance.corpusVersion
      SeamVersion = SeamVersion.current
      Dialect = ComputeDialect.dialectId
      Projector = projectorCases
      Importer = importerCases
      Validator = validatorCases
      Gates = gateCases
      Packs = packCases
      Invariants = [] }

let rendered () : string = Conformance.renderCorpus corpus

let corpusPath = "conformance/compute-fit.json"

let corpusFile () =
    System.IO.Path.Combine(CorpusFixtures.repoRoot (), corpusPath)

/// Write the corpus out, LF-exact, beside the reference dialect's.
let emit () : string =
    let path = corpusFile ()
    System.IO.File.WriteAllText(path, rendered ())
    path
