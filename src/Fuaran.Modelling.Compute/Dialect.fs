// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Compute

open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam
open Fuaran.Compute

// ============================================================================
//  The Compute dialect.
//
//  A specification is a Fuaran.Compute fit: the transform pipeline that prepares
//  the regressors, ending in ONE `Fit` step. Everything this dialect knows about
//  regression is Compute's — the families, the links, the iteration control, the
//  refusals — and none of it is restated here. What the dialect adds is the seam's
//  six obligations over that vocabulary.
//
//  The projector is Compute's canonical pipeline document, so the spec hash is
//  Compute's PIPELINE HASH, registered by the model-execution specification as
//  `compute-pipeline-sha256-v1`. A registry that keys a fitted model by its
//  pipeline hash and a decision record that keys a branch state by its spec hash
//  therefore name one object with one string. That equality is the reason the
//  dialect exists, and a law test holds it byte for byte (D45).
//
//  The dependency rule this package keeps: it is the only one in the repository
//  that references Fuaran.Compute. The manager stays Core-only, and a consumer that
//  never fits a Compute model never restores Compute.
// ============================================================================

/// The Compute dialect's spec tree: the pipeline that prepares the regressors, and the fit.
///
/// `Regressors` may itself carry a `Fit` or a `Predict` — a first-stage regression whose
/// fitted values or residuals the model reads is regressor preparation like any other step.
/// The model is the LAST fit, which is why it is held apart rather than found in the list.
type ComputeSpec =
    { Regressors: Transform list
      Fit: FitSpec }

/// What a deployment declares about the specifications it fits — never what this library
/// assumes about them.
///
/// Both members are house judgements with no default. `Source` is the dataset's schema: with
/// it, Compute's static walk knows the closed column set and a column the fit reads that is
/// not there is refused; without it, the walk knows only what the regressor steps declare and
/// refutes nothing it cannot see. `MinimumHistory` is the fewest observations a fit may
/// count: with it, the dialect's default gates gain `n` at least that many; without it,
/// there is no history gate, because a number invented here would read as a standard.
type ComputeDeclaration =
    { Source: Schema option
      MinimumHistory: int option }

[<RequireQualifiedAccess>]
module ComputeDeclaration =

    /// Nothing declared: the dialect as published, and the one its conformance corpus pins.
    let none: ComputeDeclaration = { Source = None; MinimumHistory = None }

/// The dialect: every seam obligation, over Compute's own vocabulary.
[<RequireQualifiedAccess>]
module ComputeDialect =

    let dialectId = "compute-fit"

    /// The payload format: Compute's pipeline wire document, at its format version 1.
    let payloadFormat = "compute-pipeline/v1"

    /// The minting rule, by its registered identifier (model-execution specification §7.4).
    /// It names the pipeline format's major: a change there that moves an existing pipeline's
    /// bytes mints under `-v2`, and this dialect declares the new identifier when it adopts it.
    let specHashAlgorithm = "compute-pipeline-sha256-v1"

    let identity: DialectIdentity =
        { Id = dialectId
          Version = "1.0"
          PayloadFormat = payloadFormat
          SpecHashAlgorithm = specHashAlgorithm
          TargetsSeam = SeamVersion.current }

    /// The published term vocabulary: the members of the spec tree a specification can set.
    /// Compute's own closed vocabularies — its transforms, families, links and outputs — are
    /// Compute's, versioned there, and not restated here.
    let vocabulary =
        [ "regressors"
          "response"
          "predictor"
          "intercept"
          "groupBy"
          "weights"
          "offset"
          "ridge"
          "control"
          "family"
          "link"
          "output"
          "stdErrors" ]

    // ---- the projection and the hash -----------------------------------------

    /// The pipeline a specification IS: the regressor steps, then the fit.
    let pipeline (s: ComputeSpec) : Transform list = s.Regressors @ [ Fit s.Fit ]

    /// The canonical bytes of a specification: Compute's canonical pipeline document.
    ///
    /// Not this library's canonical JSON and not the execution mirror's: the pipeline format
    /// writes numbers its own way, so the same tree rendered by either would be other bytes
    /// with another hash, and the join this dialect exists for would silently miss.
    let render (s: ComputeSpec) : string =
        DataFrameCodec.encodePipeline (pipeline s)

    /// The spec hash of a specification: Compute's pipeline hash, in the content-address form
    /// the model-execution wire carries (`sha256:` and 64 lowercase hex digits).
    let specHash (s: ComputeSpec) : string =
        "sha256:" + IncrementalCodec.pipelineHash (pipeline s)

    /// The minting rule over payload bytes.
    ///
    /// Defined on Compute's canonical pipeline documents — bytes that decode as a pipeline and
    /// re-encode to themselves — where it is Compute's pipeline hash. Anything else is outside
    /// the rule's domain and is hashed as given under a distinct `raw:` address, as the seam's
    /// own totalisation does, so it can never be passed off as a pipeline hash. This is the
    /// check the execution mirror leaves to "a dialect that mints pipeline hashes" (D44): the
    /// mirror digests whatever bytes it is handed, and only something with an encoder behind it
    /// can tell a canonical pipeline from text that merely parses as one.
    let mint (body: string) : string =
        match DataFrameCodec.decodePipeline body with
        | Ok p when DataFrameCodec.encodePipeline p = body -> "sha256:" + IncrementalCodec.pipelineHash p
        | _ -> "raw:" + Fuaran.Modelling.Execution.Digest.sha256Hex body

    // ---- the importer --------------------------------------------------------

    let private pointer (segment: string) : string =
        "/" + segment.Replace("~", "~0").Replace("/", "~1")

    let private stepType (el: JVal) : string =
        match el with
        | JObj fields ->
            fields
            |> List.tryPick (fun (k, v) ->
                match k, v with
                | "$type", JStr t -> Some t
                | _ -> None)
            |> Option.defaultValue "step"
        | _ -> "step"

    /// The members a source document carries that the codec did not read.
    ///
    /// Compute's codec reads the members it knows and passes over the rest, which is right for
    /// a wire reader and wrong for an importer: a member it passed over is a term the document's
    /// author meant and the specification no longer says. So the importer compares the source
    /// with what the decoded step re-encodes to, and names every member the re-encoding lacks.
    /// A member present on both sides with another spelling (`robust` read as `hc3`, `1e-8`
    /// written `1E-08`) was read, and is not residue.
    let rec private unread (path: string) (source: JVal) (written: JVal) : Residue list =
        match source, written with
        | JObj sourceFields, JObj writtenFields ->
            sourceFields
            |> List.collect (fun (k, v) ->
                match writtenFields |> List.tryFind (fun (w, _) -> w = k) with
                | None ->
                    [ { Path = path
                        Term = k
                        Detail = "member not read by the pipeline codec; the specification does not carry it" } ]
                | Some(_, w) -> unread (path + pointer k) v w)
        | JArr sourceItems, JArr writtenItems when List.length sourceItems = List.length writtenItems ->
            List.zip sourceItems writtenItems
            |> List.mapi (fun i (s, w) -> unread (path + "/" + string i) s w)
            |> List.concat
        | _ -> []

    let private importWith (reportResidue: bool) (text: string) : Result<ImportOutcome<ComputeSpec>, ImportFailure> =
        match Json.parse text with
        | Error m -> Error(Malformed m)
        | Ok(JArr steps) ->
            match DataFrameCodec.pipelineCodec.Decode text with
            | Error m -> Error(Unmappable("/", m))
            | Ok decoded ->
                let lastFit =
                    decoded
                    |> List.indexed
                    |> List.choose (fun (i, t) ->
                        match t with
                        | Fit f -> Some(i, f)
                        | _ -> None)
                    |> List.tryLast

                match lastFit with
                | None ->
                    Error(
                        Unmappable(
                            "/",
                            "the pipeline fits nothing; a specification of this dialect ends in the fit it specifies"
                        )
                    )
                | Some(at, fit) ->
                    let kept = List.take (at + 1) decoded

                    let unreadMembers =
                        List.zip (List.take (at + 1) steps) kept
                        |> List.mapi (fun i (source, step) ->
                            unread ("/" + string i) source (DataFrameCodec.encodeTransform step))
                        |> List.concat

                    // Steps after the fit score or reshape what it produced; they are what a
                    // pipeline does WITH the model, not part of which model it is. Dropped and
                    // named, one entry per step, so importing a fit-then-predict pipeline brings
                    // the model across and says what it left behind.
                    let trailing =
                        steps
                        |> List.indexed
                        |> List.skip (at + 1)
                        |> List.map (fun (i, el) ->
                            { Path = "/" + string i
                              Term = stepType el
                              Detail = "a step after the fit is not part of the model specification" })

                    Ok
                        { Spec =
                            { Regressors = List.take at decoded
                              Fit = fit }
                          Residue = if reportResidue then unreadMembers @ trailing else [] }
        | Ok _ -> Error(Malformed "a specification is a pipeline document: a JSON array of steps")

    /// Obligation 4: a pipeline document back to the typed tree, naming what did not come across.
    /// Any Compute pipeline that contains a fit imports; the last fit is the model.
    let import (text: string) : Result<ImportOutcome<ComputeSpec>, ImportFailure> = importWith true text

    // ---- the validators ------------------------------------------------------

    let private finding code severity path message : Finding =
        { Code = code
          Severity = severity
          Path = path
          Message = message }

    let private literal (s: Slot<'T>) : 'T option =
        match s with
        | Slot.Lit v -> Some v
        | Slot.Param _ -> None

    let private isNumeric (t: ColumnType) : bool =
        match t with
        | IntType
        | FloatType
        | DecimalType -> true
        | BoolType
        | StringType
        | DateType
        | TimestampType _ -> false

    /// What Compute's static walk knows about the columns the fit reads, before it runs.
    let private beforeFit (declaration: ComputeDeclaration) (s: ComputeSpec) : SchemaKnowledge =
        let input =
            match declaration.Source with
            | Some schema -> SchemaWalk.ofSchema schema
            | None -> SchemaKnowledge.AtLeast([], "no source schema was declared")

        SchemaWalk.ofPipelineFrom SchemaWalk.noSources input s.Regressors

    /// The columns the fit names directly, each with the path it is named at. A predictor or an
    /// offset that is an expression rather than a bare column is checked by Compute when the
    /// step resolves; the dialect does not re-walk Compute's expression language.
    let private namedColumns (fit: FitSpec) : (string * string) list =
        [ yield "/fit/response", fit.Response
          for i, (_, e) in List.indexed fit.Predictors do
              match e with
              | Col c -> yield "/fit/predictors/" + string i, c
              | _ -> ()
          match fit.Offset with
          | Some(Col c) -> yield "/fit/offset", c
          | _ -> ()
          match fit.Weights with
          | Some w -> yield "/fit/weights", w
          | None -> ()
          for i, g in List.indexed fit.GroupBy do
              yield "/fit/groupBy/" + string i, g ]

    /// Obligation 2 under a declaration: the deterministic verify tier, in a fixed order.
    ///
    /// Three sources, none of them this library's opinion about regression. Compute's own wire
    /// (a specification Compute would refuse to read back cannot be stored as a payload anyone
    /// can fit), Compute's static schema walk (a column the fit reads that the walk proves
    /// absent, or a response it proves non-numeric), and the refusals Compute's `Fit` states
    /// before it reads a row (no design column, a term named twice, a control or a ridge
    /// penalty out of range, a ridge penalty under a family that is not Gaussian).
    let validateUnder (declaration: ComputeDeclaration) (s: ComputeSpec) : Finding list =
        let fit = s.Fit

        let wire =
            let written = DataFrameCodec.encodePipeline (pipeline s)

            match DataFrameCodec.pipelineCodec.Decode written with
            | Error m ->
                [ finding "CMP-WIRE-REFUSED" Blocking "/" ("Compute's codec refuses the pipeline this specifies: " + m) ]
            | Ok back when back <> pipeline s ->
                [ finding
                      "CMP-WIRE-REFUSED"
                      Blocking
                      "/"
                      "the pipeline this specifies does not read back as itself through Compute's codec" ]
            | Ok _ -> []

        let response =
            if fit.Response.Trim() = "" then
                [ finding "CMP-RESPONSE-EMPTY" Blocking "/fit/response" "the fit names no response" ]
            else
                []

        let design =
            if not fit.Intercept && List.isEmpty fit.Predictors then
                [ finding "CMP-NO-DESIGN" Blocking "/fit/predictors" "the fit has no intercept and no predictor" ]
            else
                []

        let unnamed =
            fit.Predictors
            |> List.indexed
            |> List.filter (fun (_, (name, _)) -> name.Trim() = "")
            |> List.map (fun (i, _) ->
                finding "CMP-PREDICTOR-UNNAMED" Blocking ("/fit/predictors/" + string i) "a predictor has no name")

        let duplicates =
            fit.Predictors
            |> List.countBy fst
            |> List.filter (fun (_, n) -> n > 1)
            |> List.map (fun (name, _) ->
                finding
                    "CMP-PREDICTOR-DUPLICATE"
                    Blocking
                    "/fit/predictors"
                    ("the term '" + name + "' is named more than once"))

        let control =
            match literal fit.Control.MaxIter, literal fit.Control.Tol with
            | Some maxIter, Some tol when (LinearAlgebra.GlmControl.create maxIter tol).IsNone ->
                [ finding
                      "CMP-CONTROL-OUT-OF-RANGE"
                      Blocking
                      "/fit/control"
                      ("Compute refuses the control maxIter " + string maxIter + ", tol " + string tol) ]
            | _ -> []

        let ridge =
            match fit.Ridge |> Option.bind literal with
            | Some penalty when System.Double.IsNaN penalty || System.Double.IsInfinity penalty || penalty < 0.0 ->
                [ finding "CMP-RIDGE-OUT-OF-RANGE" Blocking "/fit/ridge" "a ridge penalty is finite and not negative" ]
            | Some penalty when penalty > 0.0 && fit.Family <> FitFamily.Gaussian ->
                [ finding
                      "CMP-RIDGE-FAMILY"
                      Blocking
                      "/fit/ridge"
                      "Compute fits a positive ridge penalty under the gaussian family only" ]
            | _ -> []

        let knowledge = beforeFit declaration s

        let absent =
            if SchemaWalk.isClosed knowledge then
                namedColumns fit
                |> List.filter (fun (_, c) -> c.Trim() <> "" && not (SchemaWalk.has c knowledge))
                |> List.map (fun (path, c) ->
                    finding
                        "CMP-COLUMN-ABSENT"
                        Blocking
                        path
                        ("the fit reads '"
                         + c
                         + "', which is not among the columns reaching it: "
                         + String.concat ", " (SchemaWalk.names knowledge)))
            else
                []

        let responseType =
            match SchemaWalk.typeOf fit.Response knowledge with
            | Some t when not (isNumeric t) ->
                [ finding
                      "CMP-RESPONSE-TYPE"
                      Blocking
                      "/fit/response"
                      ("the response '"
                       + fit.Response
                       + "' is "
                       + ColumnType.tag t
                       + ", not int, float or decimal") ]
            | _ -> []

        let intercept =
            if not fit.Intercept && not (List.isEmpty fit.Predictors) then
                [ finding
                      "CMP-NO-INTERCEPT"
                      Advisory
                      "/fit/intercept"
                      "the fit has no intercept; legitimate, and rarely what was meant" ]
            else
                []

        wire
        @ response
        @ design
        @ unnamed
        @ duplicates
        @ control
        @ ridge
        @ absent
        @ responseType
        @ intercept

    /// Obligation 2 with nothing declared.
    let validate (s: ComputeSpec) : Finding list = validateUnder ComputeDeclaration.none s

    // ---- the projector -------------------------------------------------------

    /// Obligation 3 under a declaration: refuses exactly when validation blocks; otherwise the
    /// canonical pipeline document, tagged with this dialect, its hash the pipeline hash.
    let projectUnder (declaration: ComputeDeclaration) (s: ComputeSpec) : Result<SpecPayload, Finding list> =
        let blocking =
            validateUnder declaration s
            |> List.filter Finding.blocks
            |> List.sortBy Finding.sortKey

        if not (List.isEmpty blocking) then
            Error blocking
        else
            Ok
                { Dialect = dialectId
                  Format = payloadFormat
                  Body = render s
                  SpecHash = specHash s }

    let project (s: ComputeSpec) : Result<SpecPayload, Finding list> = projectUnder ComputeDeclaration.none s

    // ---- the packs -----------------------------------------------------------

    /// A control pack: `{"maxIter": <int>, "tol": <float>}`, either or both — an iteration
    /// budget calibrated elsewhere, folded in as literals.
    let controlPackKind = "control"

    /// A ridge pack: `{"penalty": <float>}` — a penalty chosen elsewhere, folded in as a literal.
    let ridgePackKind = "ridge"

    let packKinds = [ controlPackKind; ridgePackKind ]

    let private member' (name: string) (el: JVal) : JVal option =
        match el with
        | JObj fields -> fields |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd
        | _ -> None

    let private asFloat (el: JVal) : float option =
        match el with
        | JFloat v -> Some v
        | JInt v -> Some(float v)
        | _ -> None

    let private applyControl (body: string) (s: ComputeSpec) : Result<ComputeSpec, PackRefusal> =
        match Json.parse body with
        | Error m -> Error(MalformedPack("a control pack must be JSON: " + m))
        | Ok el ->
            let maxIter =
                match member' "maxIter" el with
                | None -> Ok None
                | Some(JInt n) -> Ok(Some n)
                | Some _ -> Error(MalformedPack "'maxIter' must be an integer")

            let tol =
                match member' "tol" el |> Option.map asFloat with
                | None -> Ok None
                | Some(Some t) -> Ok(Some t)
                | Some None -> Error(MalformedPack "'tol' must be a number")

            match maxIter, tol with
            | Error r, _
            | _, Error r -> Error r
            | Ok None, Ok None -> Error(MalformedPack "a control pack sets 'maxIter', 'tol', or both")
            | Ok maxIter, Ok tol ->
                let control =
                    { MaxIter = maxIter |> Option.map Slot.Lit |> Option.defaultValue s.Fit.Control.MaxIter
                      Tol = tol |> Option.map Slot.Lit |> Option.defaultValue s.Fit.Control.Tol }

                match literal control.MaxIter, literal control.Tol with
                | Some m, Some t when (LinearAlgebra.GlmControl.create m t).IsNone ->
                    Error(MalformedPack("Compute refuses the control maxIter " + string m + ", tol " + string t))
                | _ ->
                    Ok
                        { s with
                            Fit = { s.Fit with Control = control } }

    let private applyRidge (body: string) (s: ComputeSpec) : Result<ComputeSpec, PackRefusal> =
        match Json.parse body |> Result.map (member' "penalty" >> Option.bind asFloat) with
        | Error m -> Error(MalformedPack("a ridge pack must be JSON: " + m))
        | Ok None -> Error(MalformedPack "a ridge pack carries a numeric 'penalty'")
        | Ok(Some penalty) when penalty < 0.0 -> Error(MalformedPack "a ridge penalty is not negative")
        | Ok(Some penalty) when penalty > 0.0 && s.Fit.Family <> FitFamily.Gaussian ->
            Error(
                PackConflicts(
                    "/fit/family",
                    "a positive ridge penalty applies under the gaussian family only, and this fit is not gaussian"
                )
            )
        | Ok(Some penalty) ->
            Ok
                { s with
                    Fit =
                        { s.Fit with
                            Ridge = Some(Slot.Lit penalty) } }

    /// Obligation 6: fold a pack into a specification. An undeclared kind is refused.
    let applyPack (pack: Pack) (s: ComputeSpec) : Result<ComputeSpec, PackRefusal> =
        if pack.Kind = controlPackKind then applyControl pack.Body s
        elif pack.Kind = ridgePackKind then applyRidge pack.Body s
        else Error(UnknownPackKind(pack.Kind, packKinds))

    // ---- the gates -----------------------------------------------------------

    /// The gate an unidentified design fails. Compute fits an aliased design rather than
    /// refusing it: an aliased term stays in its `Coefficients` table with no estimate, and its
    /// `Summary` table states the rank the fit reached. The observation is the number of terms
    /// with no estimate (`Observe.rankDeficiency`), and a fit passes with none.
    let rankDeficientGate = "rankDeficient"

    /// The gate a fit that did not converge fails. Compute refuses an iterative fit that runs
    /// out of iterations by name ("not converged", or a negative binomial θ that "did not
    /// converge"), and its `Summary` table states `converged` for one it fitted. The observation
    /// is 1 for either, 0 for a converged fit (`Observe.notConverged`, `Observe.refusal`).
    let notConvergedGate = "notConverged"

    /// The history gate, named for the `Summary` table's `n`: the fewest observations any
    /// group's fit counted (`Observe.history`).
    let historyGate = "n"

    let private refusalGates: GateSpec list =
        [ { Request =
              { Name = rankDeficientGate
                Threshold = 0.0
                Unit = None
                Direction = GateDirection.AtMost }
            Description = "every term of the design was identified: no coefficient was left without an estimate" }
          { Request =
              { Name = notConvergedGate
                Threshold = 0.0
                Unit = None
                Direction = GateDirection.AtMost }
            Description = "the fit converged: Compute neither refused it as not converged nor stated converged false" } ]

    /// Obligation 5 under a declaration: the `Fit` refusals by name, and the declaration's
    /// minimum history where it declares one.
    let defaultGatesUnder (declaration: ComputeDeclaration) : GateSpec list =
        refusalGates
        @ (declaration.MinimumHistory
           |> Option.map (fun points ->
               { Request =
                   { Name = historyGate
                     Threshold = float points
                     Unit = None
                     Direction = GateDirection.AtLeast }
                 Description = "every group's fit counted at least the declared minimum history of observations" })
           |> Option.toList)

    let defaultGates: GateSpec list = defaultGatesUnder ComputeDeclaration.none

    /// The default gates' observations, read off what Compute answered: the tables a `Fit` step
    /// produced, or the refusal it produced instead. An executor reports these as the fit's
    /// diagnostics under the gates' names, so the gates evaluate on the generic triple with no
    /// knowledge of Compute.
    ///
    /// Each read answers `None` where the table is not the one it reads, rather than a number
    /// that would read as an observation.
    [<RequireQualifiedAccess>]
    module Observe =

        let private cells (name: string) (t: Table) : Cell list option =
            Table.tryColumn name t |> Option.map Column.toCells

        /// The terms with no estimate in a `Coefficients` table, over every group.
        let rankDeficiency (coefficients: Table) : float option =
            cells "estimate" coefficients
            |> Option.map (fun estimates ->
                estimates
                |> List.filter (fun c ->
                    match c with
                    | Null -> true
                    | _ -> false)
                |> List.length
                |> float)

        /// 1 where a `Summary` table states any group's fit did not converge, else 0.
        let notConverged (summary: Table) : float option =
            cells "converged" summary
            |> Option.map (fun flags -> if flags |> List.contains (Bool false) then 1.0 else 0.0)

        /// The fewest observations any group's fit counted, from a `Summary` table's `n`.
        let history (summary: Table) : float option =
            cells "n" summary
            |> Option.bind (fun ns ->
                ns
                |> List.choose (fun c ->
                    match c with
                    | Int n -> Some(float n)
                    | _ -> None)
                |> function
                    | [] -> None
                    | xs -> Some(List.min xs))

        /// The gate a refusal fails, with its observation, where it is a `Fit` refusal a default
        /// gate names; `None` for any other refusal, which is no gate's to report. Read off
        /// Compute's refusal text, which Compute pins as one spelling on every host.
        let refusal (e: EvalError) : (string * float) option =
            match e with
            | FitError detail when detail.Contains "not converged" || detail.Contains "did not converge" ->
                Some(notConvergedGate, 1.0)
            | _ -> None

    // ---- the dialect ---------------------------------------------------------

    /// The dialect under a declaration. Its identity is the same whatever is declared: a
    /// declaration narrows what projects and adds a gate, and never changes what a payload
    /// means or how its hash is minted.
    let declared (declaration: ComputeDeclaration) : IDialect<ComputeSpec> =
        { new IDialect<ComputeSpec> with
            member _.Identity = identity
            member _.Vocabulary = vocabulary
            member _.Validate spec = validateUnder declaration spec
            member _.Project spec = projectUnder declaration spec
            member _.Mint body = mint body
            member _.Import text = import text
            member _.DefaultGates = defaultGatesUnder declaration
            member _.PackKinds = packKinds
            member _.ApplyPack pack spec = applyPack pack spec }

    /// The dialect with nothing declared — the one its conformance corpus certifies.
    let dialect: IDialect<ComputeSpec> = declared ComputeDeclaration.none
