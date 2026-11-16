// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ConformanceCorpusTests

open Expecto
open Fuaran.Modelling.DialectSeam

// ============================================================================
//  The corpus, run against the dialect it was authored for — and against eight
//  dialects that are deliberately wrong.
//
//  The second half is the point. A conformance harness that has only ever been
//  seen to pass certifies nothing: every check in it might be asking a question
//  no implementation can fail. So each mutation below breaks exactly one
//  obligation, and the test asserts the run FAILS with the code that names it.
// ============================================================================

let private corpusFile () = CorpusFixtures.corpusFile ()

let private lf (s: string) = s.Replace("\r\n", "\n")

let private committedCorpus () =
    match Conformance.parseCorpus (System.IO.File.ReadAllText(corpusFile ())) with
    | Ok c -> c
    | Error m -> failtestf "the committed corpus does not parse: %s" m

/// Assert that a run fails, and that `code` is among the codes it reports.
let private expectFailure (code: string) (results: CheckResult list) =
    let failed = Conformance.failedCodes results

    Expect.isNonEmpty failed "the harness must report a failure for a dialect that has one"
    Expect.contains failed code ("expected " + code + "; the run reported: " + String.concat ", " failed)

[<Tests>]
let corpusTests =
    testList
        "DialectSeam / conformance corpus"
        [ test "the corpus on disk is the corpus this build would emit" {
              // The committed file is the pin. A projector change regenerates different
              // bytes, and this comparison turns that into a diff someone has to accept
              // rather than an expectation that quietly moved with the implementation.
              let onDisk = lf (System.IO.File.ReadAllText(corpusFile ()))
              let emitted = lf (CorpusFixtures.rendered ())

              Expect.equal
                  onDisk
                  emitted
                  "re-emit with `dotnet run --project tests/Fuaran.Modelling.Tests -- --emit-corpus` and commit the diff"
          }

          test "the committed corpus parses, and says what it is" {
              let c = committedCorpus ()
              Expect.equal c.Dialect ReferenceDialect.dialectId "names the dialect it is for"
              Expect.equal c.SeamVersion SeamVersion.current "and the seam it was authored against"
              Expect.equal c.CorpusVersion Conformance.corpusVersion "and its own document version"
          }

          test "the corpus survives a render / parse round-trip" {
              let once = CorpusFixtures.corpus

              match Conformance.parseCorpus (Conformance.renderCorpus once) with
              | Error m -> failtestf "the corpus does not round-trip: %s" m
              | Ok back -> Expect.equal back once "render then parse is the identity"
          }

          test "no family is empty — a corpus with an unpopulated family proves nothing there" {
              let c = committedCorpus ()
              Expect.isNonEmpty c.Projector "projector fixtures"
              Expect.isNonEmpty c.Importer "importer fixtures"
              Expect.isNonEmpty c.Validator "validator fixtures"
              Expect.isNonEmpty c.Gates "gate fixtures"
              Expect.isNonEmpty c.Packs "pack fixtures"
              Expect.isNonEmpty c.Invariants "invariant fixtures — this dialect declares an invariant"
          }

          test "the invariants family is OPTIONAL, and its absence is not a failure" {
              // The whole claim of the seam-1.1 minor: a corpus authored before the obligation
              // existed still certifies. Modelled by removing the family from the committed
              // corpus, which is what such a document differs by.
              let older =
                  { committedCorpus () with
                      Invariants = [] }

              let results = Conformance.run ReferenceDialect.dialect older

              Expect.isEmpty
                  (Conformance.failedCodes results)
                  "a corpus with no invariant fixtures runs clean against a dialect that has them"
          }

          test "a dialect that declines the optional obligation still conforms" {
              // The other direction, and the one that matters for a dialect written against
              // seam 1.0: it does not implement the interface at all, and the harness reports a
              // PASSING check saying so rather than skipping the family silently.
              let declining =
                  { new IDialect<ReferenceSpec> with
                      member _.Identity = ReferenceDialect.identity
                      member _.Vocabulary = ReferenceDialect.vocabulary
                      member _.Validate spec = ReferenceDialect.validate spec
                      member _.Project spec = ReferenceDialect.project spec
                      member _.Mint body = ReferenceDialect.mint body
                      member _.Import text = ReferenceDialect.import text
                      member _.DefaultGates = ReferenceDialect.defaultGates
                      member _.PackKinds = ReferenceDialect.packKinds
                      member _.ApplyPack pack spec = ReferenceDialect.applyPack pack spec }

              Expect.isEmpty (Seam.invariants declining) "declining and declaring none are the same to a caller"

              let corpusWithoutInvariants =
                  { committedCorpus () with
                      Invariants = [] }

              match Conformance.certify declining corpusWithoutInvariants with
              | Ok() -> ()
              | Error m -> failtestf "a 1.0-shaped dialect no longer conforms: %s" m

              let shape =
                  Conformance.run declining corpusWithoutInvariants
                  |> List.filter (fun r -> r.Family = "invariants")

              Expect.isNonEmpty shape "the family reports rather than vanishing"
              Expect.isTrue (shape |> List.forall (fun r -> r.Passed)) "and it reports a pass"
          }

          test "the reference dialect passes the whole corpus" {
              match Conformance.certify ReferenceDialect.dialect (committedCorpus ()) with
              | Ok() -> ()
              | Error m -> failtestf "the reference dialect does not conform: %s" m
          }

          test "and every individual check in the run passes" {
              let results = Conformance.run ReferenceDialect.dialect (committedCorpus ())

              // A run that reports nothing would "pass" vacuously; assert it did work.
              Expect.isGreaterThan (List.length results) 40 "the run exercised a substantial number of checks"

              for r in results do
                  Expect.isTrue r.Passed (r.Code + " @ " + r.Family + "/" + r.Case + ": " + r.Detail)
          }

          test "the run covers every family and every fixture" {
              let c = committedCorpus ()
              let results = Conformance.run ReferenceDialect.dialect c
              let families = results |> List.map (fun r -> r.Family) |> List.distinct |> List.sort

              Expect.equal
                  families
                  [ "gates"
                    "importer"
                    "invariants"
                    "packs"
                    "projector"
                    "universal"
                    "validator" ]
                  "every family ran"

              let cases = results |> List.map (fun r -> r.Case) |> Set.ofList

              for p in c.Projector do
                  Expect.isTrue (cases.Contains p.Name) ("projector fixture " + p.Name + " ran")

              for v in c.Validator do
                  Expect.isTrue (cases.Contains v.Name) ("validator fixture " + v.Name + " ran")

              for p in c.Packs do
                  Expect.isTrue (cases.Contains p.Name) ("pack fixture " + p.Name + " ran")
          }

          test "the vocabulary coverage gate is satisfied in both directions" {
              let c = committedCorpus ()
              let declared = Set.ofList ReferenceDialect.vocabulary
              let covered = c.Projector |> List.collect (fun p -> p.Covers) |> Set.ofList
              Expect.equal covered declared "every published term is exercised, and nothing else is claimed"
          } ]

[<Tests>]
let goRedTests =
    testList
        "DialectSeam / the harness goes red"
        [ test "a dialect whose hash is a constant is caught" {
              Conformance.run (ReferenceDialect.mutant ConstantHash) (committedCorpus ())
              |> expectFailure Codes.mintNondeterministic
          }

          test "a dialect whose hash is not over its own payload bytes is caught" {
              Conformance.run (ReferenceDialect.mutant WrongHashRule) (committedCorpus ())
              |> expectFailure Codes.hashRule
          }

          test "an importer that drops what it cannot model is caught" {
              Conformance.run (ReferenceDialect.mutant LossyImporter) (committedCorpus ())
              |> expectFailure Codes.residueDrift
          }

          test "a payload tagged with someone else's dialect is caught" {
              Conformance.run (ReferenceDialect.mutant DriftingIdentity) (committedCorpus ())
              |> expectFailure Codes.identityMismatch
          }

          test "a projector that does not produce the same bytes twice is caught" {
              Conformance.run (ReferenceDialect.mutant UnstableProjector) (committedCorpus ())
              |> expectFailure Codes.projectorNondeterministic
          }

          test "a dialect that applies an undeclared pack kind is caught" {
              Conformance.run (ReferenceDialect.mutant PermissivePacks) (committedCorpus ())
              |> expectFailure Codes.packUndeclaredAccepted
          }

          test "a projector that renders a blocking-invalid spec is caught" {
              Conformance.run (ReferenceDialect.mutant LenientProjector) (committedCorpus ())
              |> expectFailure Codes.projectAcceptsBlocking
          }

          test "a dialect targeting a seam this build does not implement is caught" {
              Conformance.run (ReferenceDialect.mutant StaleSeam) (committedCorpus ())
              |> expectFailure Codes.seamSkew
          }

          test "a comparator that never finds anything is caught" {
              // The invariant-shaped mutant. It still DECLARES the invariant — id, description,
              // shape checks all pass — and simply agrees with everything, which is exactly how
              // this defect arrives in practice and is why the clean fixtures alone would not
              // have found it.
              Conformance.run (ReferenceDialect.mutant SilentComparator) (committedCorpus ())
              |> expectFailure Codes.invariantDrift
          }

          test "two comparators sharing an id are caught" {
              let duplicated =
                  ReferenceDialect.withInvariants (ReferenceDialect.invariants @ ReferenceDialect.invariants)

              Conformance.run duplicated (committedCorpus ())
              |> expectFailure Codes.invariantDuplicate
          }

          test "a comparator with no description is caught" {
              let undescribed =
                  ReferenceDialect.withInvariants (
                      ReferenceDialect.invariants |> List.map (fun c -> { c with Description = "" })
                  )

              Conformance.run undescribed (committedCorpus ())
              |> expectFailure Codes.invariantUndescribed
          }

          test "a comparator that answers differently each time is caught" {
              let counter = ref 0

              let unstable =
                  ReferenceDialect.invariants
                  |> List.map (fun c ->
                      { c with
                          Compare =
                              fun projected ->
                                  let n = counter.Value
                                  counter.Value <- n + 1

                                  c.Compare projected
                                  |> List.map (fun f -> { f with Detail = f.Detail + string n }) })

              Conformance.run (ReferenceDialect.withInvariants unstable) (committedCorpus ())
              |> expectFailure Codes.invariantNondeterministic
          }

          test "a fixture naming an invariant the dialect does not declare is caught" {
              // The optional obligation's version of the coverage gate's undeclared-term
              // failure: a corpus testing something this dialect does not admit to having.
              let corpus = committedCorpus ()

              let renamed =
                  { corpus with
                      Invariants =
                          corpus.Invariants
                          |> List.map (fun c ->
                              { c with
                                  Findings = c.Findings |> List.map (fun f -> { f with InvariantId = "NOT-DECLARED" }) }) }

              Conformance.run ReferenceDialect.dialect renamed
              |> expectFailure Codes.invariantUndeclared
          }

          test "a corpus authored for another dialect is caught" {
              let foreign =
                  { committedCorpus () with
                      Dialect = "some-other-dialect" }

              Conformance.run ReferenceDialect.dialect foreign
              |> expectFailure Codes.identityMismatch
          }

          test "certify reports the failure rather than swallowing it" {
              match Conformance.certify (ReferenceDialect.mutant WrongHashRule) (committedCorpus ()) with
              | Ok() -> failtest "a broken dialect certified"
              | Error m -> Expect.stringContains m Codes.hashRule "the verdict names the obligation"
          }

          test "every mutation is exercised — a mutant nobody runs proves nothing" {
              let exercised =
                  [ ConstantHash
                    WrongHashRule
                    LossyImporter
                    DriftingIdentity
                    UnstableProjector
                    PermissivePacks
                    LenientProjector
                    StaleSeam
                    SilentComparator ]

              for m in exercised do
                  let results = Conformance.run (ReferenceDialect.mutant m) (committedCorpus ())
                  Expect.isNonEmpty (Conformance.failedCodes results) (sprintf "%A must fail the corpus" m)
          } ]
