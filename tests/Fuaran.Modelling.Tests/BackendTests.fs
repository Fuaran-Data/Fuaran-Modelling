// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.BackendTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

let private backend = ReferenceBackend.create "reference" "1" "2026-01-01T00:00:00Z"

let private submission (stateId: StateId) (specHash: string) (gates: GateRequest list) : FitSubmission =
    { StateId = stateId
      Payload = payload "{\"v\":0}" specHash
      DatasetVersion = "ds-1"
      Seed = "7"
      Gates = gates
      SubmittedBy = SubmitHuman }

[<Tests>]
let tests =
    testList
        "Backend"
        [ test "the reference backend is deterministic — the same submission twice, the same outcome" {
              let sub =
                  submission
                      "a"
                      "h-a"
                      [ { Name = "convergence"
                          Threshold = 0.5
                          Unit = None
                          Direction = AtLeast } ]

              match backend.Submit sub, backend.Submit sub with
              | Ok first, Ok second -> Expect.equal second first "identical, down to the artifact reference"
              | a, b -> failtestf "expected two outcomes, got %A / %A" a b
          }

          test "the composite key names all five components of the join" {
              match backend.Submit(submission "a" "h-a" []) with
              | Ok o ->
                  Expect.equal o.Key.SpecHash "h-a" "the submitter's spec hash is carried, not re-derived"
                  Expect.equal o.Key.DatasetVersion "ds-1" "dataset version"
                  Expect.equal o.Key.Seed "7" "seed"
                  Expect.equal o.Key.ProviderId "reference" "provider id"
                  Expect.equal o.Key.ProviderVersion "1" "provider version"
              | Error r -> failtestf "unexpected refusal: %s" (BackendRefusal.describe r)
          }

          test "a different spec hash yields different diagnostics" {
              let gates =
                  [ { Name = "convergence"
                      Threshold = 0.5
                      Unit = None
                      Direction = AtLeast } ]

              match backend.Submit(submission "a" "h-a" gates), backend.Submit(submission "a" "h-b" gates) with
              | Ok x, Ok y -> Expect.notEqual x.Diagnostics y.Diagnostics "the two specs are distinguishable"
              | a, b -> failtestf "expected two outcomes, got %A / %A" a b
          }

          test "gates are evaluated in both directions and recorded as verdicts" {
              let observed = ReferenceBackend.diagnostic "h-a" "convergence"

              let gates =
                  [ { Name = "convergence"
                      Threshold = observed - 0.01
                      Unit = None
                      Direction = AtLeast } ]

              match backend.Submit(submission "a" "h-a" gates) with
              | Ok o -> Expect.isTrue (OutcomeSnapshot.gatesPassed o) "atLeast holds below the observation"
              | Error r -> failtestf "unexpected refusal: %s" (BackendRefusal.describe r)

              let tight =
                  [ { Name = "convergence"
                      Threshold = observed - 0.01
                      Unit = None
                      Direction = AtMost } ]

              match backend.Submit(submission "a" "h-a" tight) with
              | Ok o ->
                  Expect.isFalse (OutcomeSnapshot.gatesPassed o) "atMost fails above the threshold"
                  Expect.equal (OutcomeSnapshot.failedGates o |> List.map (fun g -> g.Name)) [ "convergence" ] "named"
              | Error r -> failtestf "unexpected refusal: %s" (BackendRefusal.describe r)
          }

          test "an outcome with no gates trivially passes — absence of a question is not a failure" {
              match backend.Submit(submission "a" "h-a" []) with
              | Ok o -> Expect.isTrue (OutcomeSnapshot.gatesPassed o) "no gates, no failures"
              | Error r -> failtestf "unexpected refusal: %s" (BackendRefusal.describe r)
          }

          test "a refusal is typed data a caller can branch on" {
              let refusing = ReferenceBackend.refusing "reference" "1" (BudgetDenied(100.0, 99.5))

              match refusing.Submit(submission "a" "h-a" []) with
              | Error(BudgetDenied(quota, spent)) ->
                  Expect.equal quota 100.0 "the quota is in the refusal"
                  Expect.equal spent 99.5 "and so is what was spent"
              | other -> failtestf "expected BudgetDenied, got %A" other
          }

          test "the artifact reference is content-derived, so it converges across hosts" {
              let key = compositeKey "h-a"
              Expect.equal (ReferenceBackend.artifactRef key) (ReferenceBackend.artifactRef key) "stable"

              Expect.notEqual
                  (ReferenceBackend.artifactRef key)
                  (ReferenceBackend.artifactRef { key with Seed = "8" })
                  "a different seed is a different artifact"
          } ]
