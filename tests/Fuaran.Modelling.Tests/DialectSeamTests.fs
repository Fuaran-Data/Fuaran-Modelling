// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.DialectSeamTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam

let private importOrFail (source: string) =
    match ReferenceDialect.import source with
    | Ok o -> o
    | Error f -> failtestf "expected an import: %s" (ImportFailure.describe f)

[<Tests>]
let seamVersionTests =
    testList
        "DialectSeam / versioning"
        [ test "a dialect targeting this seam is accepted" {
              Expect.isOk (SeamVersion.check SeamVersion.current SeamVersion.current) "same version"
          }

          test "an older minor is accepted — an additive minor is additive in both directions" {
              let host = { Major = 1; Minor = 3 }
              let dialect = { Major = 1; Minor = 1 }
              Expect.isOk (SeamVersion.check host dialect) "the dialect uses fewer obligations than the host offers"
          }

          test "a newer minor is refused, naming both versions" {
              match SeamVersion.check { Major = 1; Minor = 0 } { Major = 1; Minor = 2 } with
              | Ok() -> failtest "a dialect may not rely on obligations the host has never implemented"
              | Error(NewerMinor(d, h)) ->
                  Expect.equal d { Major = 1; Minor = 2 } "the dialect's version"
                  Expect.equal h { Major = 1; Minor = 0 } "the host's version"
              | Error other -> failtestf "wrong refusal: %A" other
          }

          test "a different major is refused as a major skew" {
              match SeamVersion.check { Major = 1; Minor = 0 } { Major = 2; Minor = 0 } with
              | Error(MajorSkew _) -> ()
              | other -> failtestf "expected a major skew, got %A" other
          }

          test "skew is a refusal a caller can act on, not a prose error" {
              // The whole point of typing it: a caller branches on WHICH kind of skew,
              // because the remedies differ — a newer minor may be satisfied by upgrading
              // the host, a different major never is.
              let refusal = MajorSkew({ Major = 2; Minor = 0 }, SeamVersion.current)
              let text = SeamRefusal.describe refusal
              Expect.stringContains text "2.0" "names the dialect's version"
              Expect.stringContains text (SeamVersion.encode SeamVersion.current) "and the host's"
          }

          test "a version round-trips through its wire spelling" {
              let v = { Major = 3; Minor = 7 }
              Expect.equal (SeamVersion.tryParse (SeamVersion.encode v)) (Ok v) "encode then parse"
          }

          test "a malformed version is refused rather than defaulted" {
              Expect.isError (SeamVersion.tryParse "1") "a bare major is not a version"
              Expect.isError (SeamVersion.tryParse "1.x") "nor is a non-numeric minor"
              Expect.isError (SeamVersion.tryParse "-1.0") "nor a negative major"
          }

          test "the seam refuses to project through a skewed dialect" {
              let stale = ReferenceDialect.mutant StaleSeam
              let spec = (importOrFail CorpusFixtures.minimalSource).Spec

              match Seam.project stale spec with
              | Error(Choice1Of2(MajorSkew _)) -> ()
              | other -> failtestf "expected a skew refusal before any projection, got %A" other

              // ...and the honest dialect projects through the same path.
              Expect.isOk (Seam.project ReferenceDialect.dialect spec) "the conforming dialect is not blocked"
          } ]

module Minting = Fuaran.Modelling.Execution.Minting

[<Tests>]
let mintingTests =
    testList
        "DialectSeam / minting"
        [ test "two authoring orders of one specification mint one hash" {
              // The property the composite key rests on, stated the only way it can be
              // checked: nothing downstream may verify a hash it was handed, so agreement
              // between submitters has to come from a shared rule.
              let a = Minting.mint CorpusFixtures.minimalSource
              let b = Minting.mint CorpusFixtures.permutedSource
              Expect.equal a b "same specification, different rendering, same hash"

              Expect.notEqual
                  CorpusFixtures.minimalSource
                  CorpusFixtures.permutedSource
                  "the renderings really do differ"
          }

          test "the seam names ONE minting rule, and it is the registered one" {
              // A seam-local re-export that drifted from the mirror's identifier would be
              // exactly the defect D25 removed, reintroduced as a constant.
              Expect.equal
                  Seam.mintingAlgorithmId
                  Minting.algorithmId
                  "the seam re-exports the mirror's identifier rather than declaring a second"

              Expect.equal
                  Seam.mintingAlgorithmId
                  "canonical-json-sha256-v1"
                  "and it is the identifier registered in §7.4"
          }

          test "the seam's totalisation answers for bytes the rule declines" {
              // `mintTotal` is the one piece that is genuinely the seam's: `IDialect.Mint`
              // must be total so a conformance harness can probe it with arbitrary bytes,
              // and the wire specification has no shape for a hash over unmintable input.
              Expect.isError (Minting.mint "not a document") "the rule itself declines"

              let raw = Seam.mintTotal "not a document"
              Expect.stringStarts raw "raw:" "an unmintable rendering never masquerades as a minted one"

              match Minting.mint """{"a":1}""" with
              | Error r -> failtestf "expected a hash: %s" (Minting.describeRefusal r)
              | Ok hash ->
                  Expect.equal (Seam.mintTotal """{"a":1}""") hash "and a mintable one is minted, not wrapped"
                  Expect.stringStarts hash "sha256:" "the content-address form"
                  Expect.equal hash.Length ("sha256:".Length + 64) "a full-width hex digest"
                  Expect.equal hash (hash.ToLowerInvariant()) "lowercase"
          }

          test "the reference dialect mints under the registered rule, over its own bytes" {
              let spec = (importOrFail CorpusFixtures.fullSource).Spec

              match ReferenceDialect.project spec with
              | Error f -> failtestf "expected a projection: %A" f
              | Ok payload ->
                  Expect.equal
                      ReferenceDialect.specHashAlgorithm
                      Minting.algorithmId
                      "the dialect declares the shared algorithm"

                  Expect.equal
                      payload.SpecHash
                      (Fuaran.Modelling.Execution.Digest.sha256 payload.Body)
                      "the hash is the digest of the payload bytes"
          } ]

[<Tests>]
let referenceDialectTests =
    testList
        "DialectSeam / the reference dialect"
        [ test "the payload it projects is tagged as its own" {
              let spec = (importOrFail CorpusFixtures.minimalSource).Spec

              match ReferenceDialect.project spec with
              | Error findings -> failtestf "expected a projection: %A" findings
              | Ok payload ->
                  Expect.equal payload.Dialect ReferenceDialect.dialectId "the dialect tag"
                  Expect.equal payload.Format ReferenceDialect.payloadFormat "the format tag"
          }

          test "the canonical bytes are the bytes they were" {
              // A hand-written pin. A round-trip agrees with any encoder that is its own
              // inverse; only a literal notices the wire moved.
              let spec = (importOrFail CorpusFixtures.minimalSource).Spec

              match ReferenceDialect.project spec with
              | Error findings -> failtestf "expected a projection: %A" findings
              | Ok payload ->
                  Expect.equal
                      payload.Body
                      """{"intercept":true,"response":"y","terms":[{"name":"x","transform":{"$type":"identity"}}]}"""
                      "the canonical projection of the minimal form"
          }

          test "the hash is the minting rule applied to those bytes, and nothing else" {
              let spec = (importOrFail CorpusFixtures.fullSource).Spec

              match ReferenceDialect.project spec with
              | Error findings -> failtestf "expected a projection: %A" findings
              | Ok payload -> Expect.equal payload.SpecHash (ReferenceDialect.mint payload.Body) "hash = mint(bytes)"
          }

          test "authoring order does not change the identity" {
              let a = (importOrFail CorpusFixtures.minimalSource).Spec
              let b = (importOrFail CorpusFixtures.permutedSource).Spec
              Expect.equal (ReferenceDialect.render a) (ReferenceDialect.render b) "same canonical bytes"
              Expect.equal a b "and the same tree"
          }

          test "an unmodelled term is imported and REPORTED, never dropped silently" {
              let outcome = importOrFail CorpusFixtures.foreignSource

              let reported = outcome.Residue |> List.map (fun r -> r.Term) |> List.sort

              Expect.equal reported [ "notes"; "prior"; "seasonalWindow"; "tool" ] "everything that did not come across"

              for r in outcome.Residue do
                  Expect.isNotEmpty r.Path "residue names where it was"
                  Expect.isNotEmpty r.Detail "and says what happened to it"
          }

          test "a blocking finding refuses the projection; an advisory one does not" {
              let blocking = (importOrFail """{"response":"y","terms":[]}""").Spec

              let advisory =
                  (importOrFail """{"response":"y","intercept":false,"terms":[{"name":"x"}]}""").Spec

              match ReferenceDialect.project blocking with
              | Ok _ -> failtest "a form with no terms must not project"
              | Error findings ->
                  Expect.isTrue (findings |> List.forall Finding.blocks) "and the refusal carries the reasons"

              Expect.isOk (ReferenceDialect.project advisory) "an advisory finding is not a veto"
              Expect.isNonEmpty (ReferenceDialect.validate advisory) "but it is still reported"
          }

          test "validation is deterministic in content and in order" {
              let spec = (importOrFail CorpusFixtures.fullSource).Spec
              let once = ReferenceDialect.validate spec

              for _ in 1..5 do
                  Expect.equal (ReferenceDialect.validate spec) once "the same findings, in the same sequence"
          }

          test "a pack of an undeclared kind is refused, enumerating the declared ones" {
              let spec = (importOrFail CorpusFixtures.twoTermSource).Spec

              let pack =
                  { Id = "p"
                    Kind = "hierarchy"
                    Version = "1"
                    Body = "{}" }

              match ReferenceDialect.applyPack pack spec with
              | Error(UnknownPackKind(kind, declared)) ->
                  Expect.equal kind "hierarchy" "names what was attempted"
                  Expect.equal declared ReferenceDialect.packKinds "and what is available instead"
              | other -> failtestf "expected an unknown-kind refusal, got %A" other
          }

          test "applying a pack changes the projection, and applying it twice does not" {
              let spec = (importOrFail CorpusFixtures.twoTermSource).Spec

              let pack =
                  { Id = "p"
                    Kind = "bounds"
                    Version = "1"
                    Body = """{"bounds":{"x1":{"lower":0,"upper":2}}}""" }

              match ReferenceDialect.applyPack pack spec with
              | Error r -> failtestf "expected the pack to apply: %s" (PackRefusal.describe r)
              | Ok once ->
                  Expect.notEqual (ReferenceDialect.render once) (ReferenceDialect.render spec) "the pack did something"

                  match ReferenceDialect.applyPack pack once with
                  | Ok twice -> Expect.equal twice once "and applying it again is idempotent"
                  | Error r -> failtestf "the second application was refused: %s" (PackRefusal.describe r)
          }

          test "the default gates are expressed on the manager's generic triple" {
              // The seam publishes gates the manager can carry without knowing the dialect —
              // that is the whole reason the vocabulary is (name, threshold, direction).
              let requests = Seam.defaultGateRequests ReferenceDialect.dialect
              Expect.isNonEmpty requests "the dialect offers defaults"

              let names = requests |> List.map (fun r -> r.Name)
              Expect.equal (List.distinct names) names "with distinct names"

              // And a backend's verdict over them is the manager's own comparison.
              let verdict = GateVerdict.evaluate (List.head requests) 5.0
              Expect.isTrue verdict.Passed "at least one term, observed five"
          }

          test "the reference dialect computes nothing about data" {
              // A guard on the one thing this dialect must never become. If it ever grows a
              // numerical method, people will use it, and a contract fixture becomes a
              // dependency with users.
              let spec = (importOrFail CorpusFixtures.fullSource).Spec
              let payload = ReferenceDialect.project spec

              match payload with
              | Ok p ->
                  Expect.isFalse (p.Body.Contains "estimate") "no estimate crosses the wire"
                  Expect.isFalse (p.Body.Contains "coefficient") "and no fitted value"
              | Error f -> failtestf "expected a projection: %A" f
          } ]
