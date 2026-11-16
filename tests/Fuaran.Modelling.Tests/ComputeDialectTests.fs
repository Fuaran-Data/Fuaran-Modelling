// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ComputeDialectTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam
open Fuaran.Modelling.Compute
open Fuaran.Compute
open Fuaran.Modelling.Tests.Sample


// ============================================================================
//  The Compute dialect: its conformance, the one law it exists for, and the
//  pieces a deployment declares or queries.
//
//  The law is that the dialect's `specHash` IS Compute's pipeline hash, byte for
//  byte. It is asserted three ways, because each alone could pass while the
//  join silently missed: against Compute's own `pipelineHash` function, against
//  the digest the model-execution specification's corpus records for the
//  pipelines it imports, and through the execution wire's composite-key codec,
//  which is where the hash meets the registry it joins.
// ============================================================================

let private lf (s: string) = s.Replace("\r\n", "\n")

let private committedCorpus () =
    match Conformance.parseCorpus (System.IO.File.ReadAllText(ComputeCorpusFixtures.corpusFile ())) with
    | Ok c -> c
    | Error m -> failtestf "the committed Compute corpus does not parse: %s" m

let private importOrFail (text: string) : ImportOutcome<ComputeSpec> =
    match ComputeDialect.import text with
    | Ok o -> o
    | Error f -> failtestf "does not import: %s" (ImportFailure.describe f)

let private projectOrFail (spec: ComputeSpec) : SpecPayload =
    match ComputeDialect.project spec with
    | Ok p -> p
    | Error findings -> failtestf "does not project: %A" (findings |> List.map (fun f -> f.Code))

let private codes (findings: Finding list) = findings |> List.map (fun f -> f.Code)

// ---- the specification's pipeline vectors -----------------------------------

let private submissionOf (v: Fuaran.Modelling.Execution.Vector) : Fuaran.Modelling.Execution.FitSubmission =
    match Fuaran.Modelling.Execution.Codec.read (SpecCorpus.load v) with
    | Ok(Fuaran.Modelling.Execution.FitSubmissionDoc s) -> s
    | Ok other -> failtestf "%s is a %s, not a fit submission" v.Id (Fuaran.Modelling.Execution.Document.kindOf other)
    | Error refusal -> failtestf "%s does not read: %s" v.Id refusal.Detail

/// The specification's `spec-hash` vectors minted under the pipeline hash: documents imported
/// byte for byte from the pipeline specification's corpus, each with the digest that corpus
/// records for it. Read from the corpus, so a vector added there is checked here unasked.
let private pipelineVectors () =
    (SpecCorpus.manifest ()).Vectors
    |> List.filter (fun v -> v.Family = "spec-hash")
    |> List.map (fun v -> v, submissionOf v)
    |> List.filter (fun (_, s) -> s.SpecHashAlgorithm = ComputeDialect.specHashAlgorithm)

// ---- the evaluator, for the refusal gates -------------------------------------

let private floatColumn (name: string) (values: float list) : Column =
    match Column.ofCells name FloatType (values |> List.map Float) with
    | Ok c -> c
    | Error e -> failtestf "column %s: %A" name e

let private table (columns: (string * float list) list) : Table =
    { Schema = columns |> List.map (fun (n, _) -> Field.create n FloatType)
      Columns = columns |> List.map (fun (n, vs) -> floatColumn n vs) }

// ---- the multiplicity record --------------------------------------------------

let private computePayload (spec: ComputeSpec) : SpecPayload = projectOrFail spec

let private fork (id: StateId) (from: StateId) (p: SpecPayload) : ExplorationOp =
    Fork
        { StateId = id
          From = from
          Label = "branch " + id
          Payload = p }

/// A fit of `state` on `version` reading `x`, keyed as an executor would key it: the state's own
/// spec hash, under a seed that tells two fits of one branch apart.
let private fitted (e: Exploration) (state: StateId) (version: string) (seed: string) (x: float) : ExplorationOp =
    let specHash = (Query.tryState state e |> Option.get).Payload.SpecHash

    Annotate
        { StateId = state
          Snapshot =
            { outcome specHash with
                Key =
                    { SpecHash = specHash
                      DatasetVersion = version
                      Seed = seed
                      ProviderId = "compute"
                      ProviderVersion = "0.44.0" }
                Diagnostics = Map.ofList [ "holdoutError", x ]
                Gates = [] } }

[<Tests>]
let tests =
    testList
        "Compute dialect"
        [ testList
              "conformance"
              [ test "the corpus on disk is the corpus this build would emit" {
                    Expect.equal
                        (lf (System.IO.File.ReadAllText(ComputeCorpusFixtures.corpusFile ())))
                        (lf (ComputeCorpusFixtures.rendered ()))
                        "re-emit with `dotnet run --project tests/Fuaran.Modelling.Tests -- --emit-corpus` and commit the diff"
                }

                test "the dialect passes the full conformance pack" {
                    let c = committedCorpus ()
                    Expect.equal c.Dialect ComputeDialect.dialectId "the corpus names this dialect"

                    match Conformance.certify ComputeDialect.dialect c with
                    | Ok() -> ()
                    | Error report -> failtestf "the Compute dialect does not conform: %s" report
                }

                test "every family the dialect has fixtures for is populated" {
                    let c = committedCorpus ()
                    Expect.isNonEmpty c.Projector "projector fixtures"
                    Expect.isNonEmpty c.Importer "importer fixtures"
                    Expect.isNonEmpty c.Validator "validator fixtures"
                    Expect.isNonEmpty c.Gates "gate fixtures"
                    Expect.isNonEmpty c.Packs "pack fixtures"
                    Expect.isEmpty c.Invariants "it declares no invariants, so it pins none"
                }

                test "the reference dialect still conforms to its own corpus beside it" {
                    match
                        Conformance.parseCorpus (System.IO.File.ReadAllText(CorpusFixtures.corpusFile ()))
                        |> Result.bind (Conformance.certify ReferenceDialect.dialect)
                    with
                    | Ok() -> ()
                    | Error report -> failtestf "the reference dialect stopped conforming: %s" report
                }

                test "a Compute dialect minting under the other registered rule fails the pack by name" {
                    // The go-red probe for this corpus: the same dialect, its projector and Mint
                    // switched to canonical-json-sha256-v1. Every hash is still a sha256: digest,
                    // still deterministic, and names a different object than a registry keyed by
                    // the pipeline hash does.
                    let d = ComputeDialect.dialect

                    let wrongRule =
                        { new IDialect<ComputeSpec> with
                            member _.Identity = d.Identity
                            member _.Vocabulary = d.Vocabulary
                            member _.Validate s = d.Validate s

                            member _.Project s =
                                d.Project s
                                |> Result.map (fun p ->
                                    { p with
                                        SpecHash = Seam.mintTotal p.Body })

                            member _.Mint body = Seam.mintTotal body
                            member _.Import t = d.Import t
                            member _.DefaultGates = d.DefaultGates
                            member _.PackKinds = d.PackKinds
                            member _.ApplyPack p s = d.ApplyPack p s }

                    let failed =
                        Conformance.run wrongRule (committedCorpus ()) |> Conformance.failedCodes

                    Expect.contains failed Codes.projectHashDrift "the pinned hashes are pipeline hashes"
                } ]

          testList
              "specHash = pipeline hash"
              [ test "for every projector fixture, the spec hash is Compute's pipeline hash byte for byte" {
                    for case in ComputeCorpusFixtures.projectorCases do
                        let outcome = importOrFail case.Source
                        let payload = projectOrFail outcome.Spec

                        let computeHash =
                            "sha256:" + IncrementalCodec.pipelineHash (ComputeDialect.pipeline outcome.Spec)

                        Expect.equal payload.SpecHash computeHash (case.Name + ": Compute's pipeline hash")
                        Expect.equal payload.SpecHash case.Hash (case.Name + ": the pinned hash")

                        Expect.equal
                            payload.SpecHash
                            (Fuaran.Modelling.Execution.Digest.sha256 payload.Body)
                            (case.Name + ": the digest of the payload's bytes, so a carrier needs no encoder")

                        Expect.equal
                            (ComputeDialect.mint payload.Body)
                            payload.SpecHash
                            (case.Name + ": Mint over the bytes")

                        Expect.equal
                            payload.Body
                            (DataFrameCodec.encodePipeline (ComputeDialect.pipeline outcome.Spec))
                            (case.Name + ": the payload is Compute's canonical document")
                }

                test "for every pipeline the specification's corpus carries, Mint reproduces the recorded digest" {
                    // The specification imports its pipelines from the pipeline specification's
                    // corpus, which can run ahead of the Compute release pinned here. A pipeline
                    // whose steps the pinned Compute cannot decode is outside this dialect's
                    // domain: Mint must say so with a raw address rather than mint a pipeline
                    // hash it cannot vouch for. Every one it CAN decode must reproduce the digest.
                    let vectors = pipelineVectors ()
                    Expect.isNonEmpty vectors "the specification carries pipeline-hash vectors"

                    let decodable =
                        vectors
                        |> List.choose (fun (v, s) ->
                            match DataFrameCodec.decodePipeline s.SpecPayload with
                            | Ok pipeline -> Some(v, s, pipeline)
                            | Error _ ->
                                Expect.stringStarts
                                    (ComputeDialect.mint s.SpecPayload)
                                    "raw:"
                                    (v.Id + ": beyond the pinned Compute, so not minted as a pipeline hash")

                                None)

                    Expect.isTrue
                        (decodable
                         |> List.exists (fun (_, s, _) -> s.SpecPayload.Contains "\"$type\":\"fit\""))
                        "the pinned Compute decodes the specification's pipeline that carries a fit"

                    for v, submission, pipeline in decodable do
                        Expect.equal
                            (ComputeDialect.mint submission.SpecPayload)
                            submission.SpecHash
                            (v.Id + ": the digest the specification records")

                        Expect.equal
                            ("sha256:" + IncrementalCodec.pipelineHash pipeline)
                            submission.SpecHash
                            (v.Id + ": and Compute's pipeline hash of the decoded pipeline")
                }

                test "a specification's pipeline that carries a fit imports, and re-mints as its model alone" {
                    let withFit =
                        pipelineVectors ()
                        |> List.filter (fun (_, s) -> s.SpecPayload.Contains "\"$type\":\"fit\"")

                    Expect.isNonEmpty withFit "the specification carries a pipeline with a fit"

                    for v, submission in withFit do
                        let outcome = importOrFail submission.SpecPayload
                        let payload = projectOrFail outcome.Spec

                        Expect.equal
                            payload.SpecHash
                            ("sha256:" + IncrementalCodec.pipelineHash (ComputeDialect.pipeline outcome.Spec))
                            (v.Id + ": the model's own pipeline hash")

                        if not (List.isEmpty outcome.Residue) then
                            Expect.notEqual
                                payload.SpecHash
                                submission.SpecHash
                                (v.Id
                                 + ": steps after the fit are residue, so the model's hash is not the whole pipeline's")
                }

                test "the composite key built from the spec hash round-trips through the model-execution codec" {
                    for case in ComputeCorpusFixtures.projectorCases do
                        let payload = projectOrFail (importOrFail case.Source).Spec

                        let key: Fuaran.Modelling.Execution.CompositeKey =
                            { SpecHash = payload.SpecHash
                              Vintage =
                                { DatasetId = "quarterly-panel"
                                  Version = 7
                                  ContentRef = None }
                              Seed = 0L
                              ProviderId = "compute"
                              ProviderVersion = "0.44.0" }

                        let text =
                            Fuaran.Modelling.Execution.Codec.render (Fuaran.Modelling.Execution.CompositeKeyDoc key)

                        match Fuaran.Modelling.Execution.Codec.read text with
                        | Ok(Fuaran.Modelling.Execution.CompositeKeyDoc back) ->
                            Expect.equal back key (case.Name + ": the key reads back as itself")

                            Expect.equal
                                (Fuaran.Modelling.Execution.Codec.render (
                                    Fuaran.Modelling.Execution.CompositeKeyDoc back
                                ))
                                text
                                "byte for byte"

                            Expect.equal
                                (Fuaran.Modelling.Execution.Bridge.toManagerKey back).SpecHash
                                payload.SpecHash
                                (case.Name + ": and the decision record's key carries the same spec hash")
                        | Ok other -> failtestf "read back as a %s" (Fuaran.Modelling.Execution.Document.kindOf other)
                        | Error refusal -> failtestf "%s: the key does not read: %s" case.Name refusal.Detail
                }

                test "Mint refuses to call non-canonical bytes a pipeline hash" {
                    let raw = ComputeDialect.mint ComputeCorpusFixtures.permutedSource
                    Expect.stringStarts raw "raw:" "a permuted document parses as a pipeline and is not canonical"
                    Expect.stringStarts (ComputeDialect.mint "not json") "raw:" "nor is text that is not a pipeline"

                    Expect.stringStarts
                        (ComputeDialect.mint ComputeCorpusFixtures.minimalSource)
                        "sha256:"
                        "a canonical document mints its pipeline hash"
                } ]

          testList
              "import"
              [ test "a pipeline that fits nothing is not a specification of this dialect" {
                    match ComputeDialect.import "[]" with
                    | Error(Unmappable("/", _)) -> ()
                    | other -> failtestf "expected an unmappable refusal, got %A" other
                }

                test "a document that is not a pipeline is malformed, not unmappable" {
                    match ComputeDialect.import "{}", ComputeDialect.import "{" with
                    | Error(Malformed _), Error(Malformed _) -> ()
                    | other -> failtestf "expected two malformed refusals, got %A" other
                }

                test "a first-stage fit is regressor preparation, and the model is the last fit" {
                    let firstStage =
                        { FitSpec.ols "price" [ "cost", Col "cost" ] with
                            Output = FitOutput.Fitted }

                    let spec: ComputeSpec =
                        { Regressors = [ Fit firstStage ]
                          Fit = FitSpec.ols "sales" [ "fitted", Col "fitted" ] }

                    let back = importOrFail (ComputeDialect.render spec)
                    Expect.equal back.Spec spec "the regressors keep the first stage"
                    Expect.isEmpty back.Residue "and nothing is dropped"
                } ]

          testList
              "validators under a declaration"
              [ let schema =
                    [ Field.create "sales" FloatType
                      Field.create "price" FloatType
                      Field.create "region" StringType ]

                let underSchema =
                    { ComputeDeclaration.none with
                        Source = Some schema }

                test "a declared source closes the walk, so a column it lacks is refused by name" {
                    let spec: ComputeSpec =
                        { Regressors = []
                          Fit = FitSpec.ols "sales" [ "price", Col "price"; "advertising", Col "advertising" ] }

                    Expect.isEmpty (codes (ComputeDialect.validate spec)) "nothing declared: nothing refuted"

                    let findings = ComputeDialect.validateUnder underSchema spec
                    Expect.equal (codes findings) [ "CMP-COLUMN-ABSENT" ] "the absent column"
                    Expect.equal (findings |> List.map (fun f -> f.Path)) [ "/fit/predictors/1" ] "at its path"

                    match ComputeDialect.projectUnder underSchema spec with
                    | Error _ -> ()
                    | Ok _ -> failtest "and it does not project under the declaration"
                }

                test "a declared source types the response, and a string response is refused" {
                    let spec: ComputeSpec =
                        { Regressors = []
                          Fit = FitSpec.ols "region" [ "price", Col "price" ] }

                    Expect.equal
                        (codes (ComputeDialect.validateUnder underSchema spec))
                        [ "CMP-RESPONSE-TYPE" ]
                        "the walk knows the response's type"
                }

                test "a regressor step's own column is known to the walk" {
                    let spec: ComputeSpec =
                        { Regressors = [ Derive("log_price", ApplyFn(ScalarFn.Ln, [ Col "price" ])) ]
                          Fit = FitSpec.ols "sales" [ "log_price", Col "log_price" ] }

                    Expect.isEmpty (codes (ComputeDialect.validateUnder underSchema spec)) "derived, then read"
                }

                test "a family and link Compute's wire refuses cannot be projected" {
                    let spec: ComputeSpec =
                        { Regressors = []
                          Fit =
                            { FitSpec.ols "visits" [ "price", Col "price" ] with
                                Family = FitFamily.Poisson
                                Link = FitLink.Logit } }

                    Expect.contains
                        (codes (ComputeDialect.validate spec))
                        "CMP-WIRE-REFUSED"
                        "Compute's codec refuses it"

                    match ComputeDialect.project spec with
                    | Error _ -> ()
                    | Ok _ -> failtest "a payload no host could read back is not stored"
                }

                test "a control and a ridge penalty out of Compute's range are refused" {
                    let spec: ComputeSpec =
                        { Regressors = []
                          Fit =
                            { FitSpec.ols "sales" [ "price", Col "price" ] with
                                Control =
                                    { MaxIter = Slot.Lit 0
                                      Tol = Slot.Lit 1e-8 }
                                Ridge = Some(Slot.Lit -1.0) } }

                    let found = codes (ComputeDialect.validate spec)
                    Expect.contains found "CMP-CONTROL-OUT-OF-RANGE" "the control"
                    Expect.contains found "CMP-RIDGE-OUT-OF-RANGE" "the penalty"
                }

                test "a parameterised control is bound later, and is not judged now" {
                    let spec: ComputeSpec =
                        { Regressors = []
                          Fit =
                            { FitSpec.ols "sales" [ "price", Col "price" ] with
                                Control =
                                    { MaxIter = Slot.Param "maxIter"
                                      Tol = Slot.Lit 1e-8 }
                                Ridge = Some(Slot.Param "penalty") } }

                    Expect.isEmpty (codes (ComputeDialect.validate spec)) "a slot is a host's to bind"
                } ]

          testList
              "gates"
              [ test "the default gates are the Fit refusals, and no history gate is invented" {
                    Expect.equal
                        (Seam.defaultGateRequests ComputeDialect.dialect |> List.map (fun g -> g.Name))
                        [ ComputeDialect.rankDeficientGate; ComputeDialect.notConvergedGate ]
                        "two gates, both refusals"
                }

                test "a declared minimum history adds the n gate at exactly that threshold" {
                    let d =
                        ComputeDialect.declared
                            { ComputeDeclaration.none with
                                MinimumHistory = Some 24 }

                    match
                        Seam.defaultGateRequests d
                        |> List.tryFind (fun g -> g.Name = ComputeDialect.historyGate)
                    with
                    | Some g ->
                        Expect.equal g.Threshold 24.0 "the declared points"
                        Expect.equal g.Direction AtLeast "at least"
                    | None -> failtest "the history gate is missing"

                    Expect.equal d.Identity ComputeDialect.identity "a declaration never changes the identity"
                }

                test "an aliased design is fitted and observed as rank deficient, a clean one is not" {
                    // Through the evaluator rather than asserted from a description, so a change
                    // in how Compute reports aliasing turns this red rather than the gate silent.
                    // Compute 0.44.0 fits an aliased design: the aliased term stays in the
                    // Coefficients table with no estimate, and Summary states the rank reached.
                    let data =
                        table
                            [ "y", [ 1.0; 2.0; 3.1; 3.9; 5.2 ]
                              "x", [ 1.0; 2.0; 3.0; 4.0; 5.0 ]
                              "twice", [ 2.0; 4.0; 6.0; 8.0; 10.0 ] ]

                    let observed (predictors: (string * ColExpr) list) =
                        match DataFrame.evalPipeline [ Fit(FitSpec.ols "y" predictors) ] data with
                        | Ok coefficients -> ComputeDialect.Observe.rankDeficiency coefficients
                        | Error e -> failtestf "the fit was refused: %A" e

                    Expect.equal (observed [ "x", Col "x"; "twice", Col "twice" ]) (Some 1.0) "one aliased term"
                    Expect.equal (observed [ "x", Col "x" ]) (Some 0.0) "and none in a clean design"
                }

                test "a fitted Summary states its convergence and its history" {
                    let data =
                        table [ "y", [ 1.0; 2.0; 3.1; 3.9; 5.2 ]; "x", [ 1.0; 2.0; 3.0; 4.0; 5.0 ] ]

                    let fit =
                        { FitSpec.ols "y" [ "x", Col "x" ] with
                            Output = FitOutput.Summary }

                    match DataFrame.evalPipeline [ Fit fit ] data with
                    | Error e -> failtestf "the fit was refused: %A" e
                    | Ok summary ->
                        Expect.equal (ComputeDialect.Observe.notConverged summary) (Some 0.0) "converged"
                        Expect.equal (ComputeDialect.Observe.history summary) (Some 5.0) "five observations"
                        Expect.isNone (ComputeDialect.Observe.rankDeficiency summary) "and no estimate column to read"
                }

                test "Compute's own non-convergence refusal maps to the notConverged gate" {
                    let data =
                        table
                            [ "y", [ 0.0; 1.0; 3.0; 7.0; 20.0; 55.0 ]
                              "x", [ 0.0; 1.0; 2.0; 3.0; 4.0; 5.0 ] ]

                    let fit =
                        { FitSpec.ols "y" [ "x", Col "x" ] with
                            Family = FitFamily.Poisson
                            Link = FitLink.Log
                            Control =
                                { MaxIter = Slot.Lit 1
                                  Tol = Slot.Lit 1e-12 } }

                    match DataFrame.evalPipeline [ Fit fit ] data with
                    | Ok _ -> failtest "one iteration converged; the gate's premise is gone"
                    | Error e ->
                        Expect.equal
                            (ComputeDialect.Observe.refusal e)
                            (Some(ComputeDialect.notConvergedGate, 1.0))
                            (sprintf "the refusal %A" e)
                }

                test "a refusal that is not a Fit refusal is no gate's" {
                    Expect.isNone (ComputeDialect.Observe.refusal (FitError "fit: no design column")) "not a gate"
                } ]

          testList
              "indistinguishability-set query"
              [ let spec (term: string) : ComputeSpec =
                    { Regressors = []
                      Fit = FitSpec.ols "sales" [ term, Col term ] }

                let record () =
                    let seeded =
                        foldOrFail
                            [ Seed
                                  { StateId = "root"
                                    Label = "root"
                                    Payload = computePayload (spec "price") }
                              fork "a" "root" (computePayload (spec "advertising"))
                              fork "b" "root" (computePayload (spec "season"))
                              fork "c" "root" (payload "{\"v\":\"c\"}" "h-c")
                              fork "d" "root" (computePayload (spec "promotion")) ]

                    let ops =
                        [ fitted seeded "a" "ds-1" "+1" 0.40
                          fitted seeded "b" "ds-1" "+1" 0.90
                          // b refitted on the same vintage: the newer fit is the one read, so it
                          // is the key handed over.
                          fitted seeded "b" "ds-1" "+2" 0.42
                          fitted seeded "c" "ds-1" "+1" 0.41
                          fitted seeded "d" "ds-1" "+1" 0.70 ]

                    match Reducer.applyAll ops seeded with
                    | Ok e -> e
                    | Error(i, r) -> failwithf "op %d rejected: %s" i (Rejection.describe r)

                test "no declaration, no query: the answer is None, never an empty list" {
                    let e = record ()
                    Expect.isNone (Multiplicity.memberKeys (Indistinguishability.report None "a" e) e) "off by default"
                }

                test "a declared set answers each member's composite key, the reference first" {
                    let e = record ()

                    let declaration =
                        match Indistinguishability.declare e "holdoutError" 0.05 EveryAlternative with
                        | Ok d -> Some d
                        | Error r -> failtestf "%s" (IndistinguishabilityRefusal.describe r)

                    match Multiplicity.memberKeys (Indistinguishability.report declaration "a" e) e with
                    | None -> failtest "a declared set answered None"
                    | Some members ->
                        Expect.equal
                            (members |> List.map SetMemberKey.state)
                            [ "a"; "b"; "c" ]
                            "the set's members, in the set's order; d is outside the margin"

                        let specHashOf id =
                            (Query.tryState id e |> Option.get).Payload.SpecHash

                        match members with
                        | [ Rescorable("a", ka); Rescorable("b", kb); OtherDialect("c", dialect, kc) ] ->
                            Expect.equal ka.SpecHash (specHashOf "a") "a's key carries its pipeline hash"
                            Expect.equal kb.Seed "+2" "b's key is the fit whose reading put it in the set"
                            Expect.equal dialect "reference" "c is named, with the dialect it was written in"
                            Expect.equal kc.SpecHash "h-c" "and its key"
                        | other -> failtestf "unexpected members %A" other

                        Expect.equal
                            (Multiplicity.rescorable (Indistinguishability.report declaration "a" e) e
                             |> Option.map (List.map (fun k -> k.SpecHash)))
                            (Some [ specHashOf "a"; specHashOf "b" ])
                            "the re-scorable keys are the members fitted under this dialect"
                } ]

          test "the manager, the seam and the execution tier reference no Compute assembly" {
              // The dependency rule (D45), held on the built assemblies rather than on a project
              // file, so a reference that arrives transitively is caught too.
              for anchor in
                  [ typeof<Fuaran.Modelling.CompositeKey>.Assembly
                    typeof<Fuaran.Modelling.DialectSeam.Finding>.Assembly
                    typeof<Fuaran.Modelling.Execution.FitSubmission>.Assembly ] do
                  let compute =
                      anchor.GetReferencedAssemblies()
                      |> Array.map (fun a -> a.Name)
                      |> Array.filter (fun n -> n.StartsWith "Fuaran.Compute")

                  Expect.isEmpty compute (anchor.GetName().Name + " references Compute")

              // The positive control: the same read finds Compute where it is meant to be, so an
              // empty answer above is a finding and not a probe that cannot see references.
              Expect.contains
                  (typeof<ComputeSpec>.Assembly.GetReferencedAssemblies()
                   |> Array.map (fun a -> a.Name))
                  "Fuaran.Compute.DataFrame"
                  "the Compute dialect's own assembly does reference Compute"
          } ]
