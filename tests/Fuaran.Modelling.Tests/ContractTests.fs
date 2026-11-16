// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ContractTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

[<Tests>]
let tests =
    testList
        "Contract"
        [ test "every law the substrate promises holds" {
              match Contract.certify hashFn with
              | Ok() -> ()
              | Error m -> failtestf "the contract pack does not certify: %s" m
          }

          test "each law is reported individually, and each passes" {
              for l in Contract.laws hashFn do
                  Expect.isTrue l.Passed (l.Law + ": " + l.Detail)
          }

          test "the fixture corpus covers every op kind" {
              match Contract.coverage () with
              | Ok() -> ()
              | Error m -> failtestf "coverage gate: %s" m

              Expect.equal
                  (Contract.samples |> List.map fst |> List.sort)
                  (OpKind.all |> List.sort)
                  "a new op kind needs a fixture, not just a case"
          }

          test "every fixture in the corpus passes" {
              let failures = Contract.runCorpus () |> List.filter (fun o -> not o.Passed)
              Expect.isEmpty failures (sprintf "%A" failures)
          }

          test "the corpus is not vacuous — it carries both round-trips and rejects" {
              let roundTrips =
                  Contract.cases |> List.filter (fun c -> c.Kind = Fuaran.Core.Corpus.RoundTrip)

              let rejects =
                  Contract.cases |> List.filter (fun c -> c.Kind = Fuaran.Core.Corpus.Reject)

              Expect.equal (List.length roundTrips) (List.length OpKind.all) "one round-trip per op kind"
              Expect.isNonEmpty rejects "and a decoder is only as good as what it declines"
          }

          test "the law suite would notice a broken law" {
              // Verify the probe, not just the verdict: a law harness that cannot report a
              // failure is a harness that certifies nothing. Feed the default-deny law's own
              // question a policy that DOES grant, and confirm the machinery says so.
              let granting = fullPolicy "anyone"

              let leaked =
                  (OpKind.all |> List.map MayEmit) @ [ MaySubmitFit ]
                  |> List.filter (fun c ->
                      match Policy.authorise granting "anyone" c with
                      | Ok() -> true
                      | Error _ -> false)

              Expect.isNonEmpty leaked "the same predicate the law uses does fire when a grant exists"
          }

          test "the vocabulary bans would notice a field renamed in good faith" {
              // Verify the probe, not just the verdict. A reflective ban that matched nothing
              // passes on every record it is pointed at, including one called `BestIn` — so
              // the predicate is driven over names that MUST offend, in the same shape the law
              // applies it.
              let fires (banned: string list) (name: string) =
                  let lowered = name.ToLowerInvariant()
                  banned |> List.exists lowered.Contains

              for name in [ "BestIn"; "Winner"; "RankedIn"; "PreferredIn"; "Score" ] do
                  Expect.isTrue
                      (fires Contract.bannedIndistinguishabilityVocabulary name)
                      (name + " is exactly the rename the ban exists for")

              Expect.isFalse
                  (fires Contract.bannedIndistinguishabilityVocabulary "IndistinguishableIn")
                  "and the names the surface actually uses pass — a ban that rejected everything would be no ban"
          }

          test "the byte pins are literals, not derivations" {
              // The pins must be compared against hand-written expectations; a pin computed
              // from the encoder would agree with any encoder.
              Expect.isNonEmpty Contract.bytePins "there are pins"

              for actual, pinned in Contract.bytePins do
                  Expect.equal actual pinned "the canonical bytes are the bytes they were"
          } ]
