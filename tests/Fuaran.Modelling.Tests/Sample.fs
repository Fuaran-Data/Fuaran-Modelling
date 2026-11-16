// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.Sample

open Fuaran.Core
open Fuaran.Modelling

/// The suite's hash function. The portable substrate default — deterministic and
/// host-independent, which is what every assertion about content ids relies on.
let hashFn: HashFn = OpStream.defaultHash

let curator = Human "curator"
let explorer = Agent("reference-explorer", "1", "explorer")

/// A spec payload the manager cannot read — which is the point. The body is arbitrary
/// text; nothing in the library parses it.
let payload (body: string) (hash: string) : SpecPayload =
    { Dialect = "reference"
      Format = "json"
      Body = body
      SpecHash = hash }

let curation (verdict: CurationVerdict) (reason: string) : CurationRecord =
    { Verdict = verdict
      Reason = reason
      CuratedBy = "curator"
      CuratedAt = "2026-01-01T00:00:00Z" }

let seedOp (id: StateId) : ExplorationOp =
    Seed
        { StateId = id
          Label = "root"
          Payload = payload "{\"v\":0}" ("h-" + id) }

let forkOp (id: StateId) (from: StateId) : ExplorationOp =
    Fork
        { StateId = id
          From = from
          Label = "branch " + id
          Payload = payload ("{\"v\":\"" + id + "\"}") ("h-" + id) }

let curateOp (id: StateId) (verdict: CurationVerdict) (reason: string) : ExplorationOp =
    Curate
        { StateId = id
          Record = curation verdict reason }

let compositeKey (specHash: string) : CompositeKey =
    { SpecHash = specHash
      DatasetVersion = "ds-1"
      Seed = "7"
      ProviderId = "reference"
      ProviderVersion = "1" }

let outcome (specHash: string) : OutcomeSnapshot =
    { Key = compositeKey specHash
      ArtifactRef = "ref:" + specHash
      Diagnostics = Map.ofList [ "convergence", 0.75 ]
      DiagnosticUnits = Map.empty
      Gates =
        [ { Name = "convergence"
            Threshold = 0.5
            Unit = None
            Direction = AtLeast
            Observed = 0.75
            Passed = true } ]
      SubmittedBy = SubmitHuman
      RecordedAt = "2026-01-01T00:00:00Z" }

/// A policy granting one subject the full op vocabulary plus fit submission — the
/// "trusted human operator" shape. Written out rather than derived, so a new op kind
/// does not silently widen anybody's grant.
let fullPolicy (subject: string) : Policy =
    { Policy.empty with
        Objectives =
            [ Policy.objective
                  "everything"
                  "full manager access"
                  subject
                  ((OpKind.all |> List.map MayEmit) @ [ MaySubmitFit ]) ] }

/// Fold a sequence of ops with no policy gate, failing the test caller's expectations
/// loudly if any is rejected.
let foldOrFail (ops: ExplorationOp list) : Exploration =
    match Reducer.applyAll ops Exploration.empty with
    | Ok e -> e
    | Error(i, r) -> failwithf "op %d rejected: %s" i (Rejection.describe r)

let sessionOrFail (ops: ExplorationOp list) : Session =
    match Session.submitAll hashFn curator ops Session.empty with
    | Ok s -> s
    | Error(i, r) -> failwithf "op %d rejected: %s" i (Rejection.describe r)
