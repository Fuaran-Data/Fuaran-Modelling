// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ExecutionCorpusTests

open Expecto
open Fuaran.Modelling.Execution

// ============================================================================
//  The mirror, run against the specification's own corpus — and then run again
//  against fixtures that have been deliberately broken.
//
//  The second half is the point. This suite is what stands between "the mirror
//  is a snapshot" and "the mirror is a copy of something that used to be true",
//  and a suite that has only ever been seen to pass could be asking questions no
//  implementation can fail. So each mutation below breaks exactly one thing, and
//  the test asserts the run goes red with the code that names it.
// ============================================================================

let private certifyingProfile = ExecutorProfile

let private results () =
    Conformance.run certifyingProfile (SpecCorpus.manifestText ()) (SpecCorpus.manifest ()) SpecCorpus.load

let private expectFailure (code: string) (results: CheckResult list) =
    let failed = Conformance.failedCodes results

    Expect.isNonEmpty failed "the harness must report a failure for a corpus that has one"
    Expect.contains failed code ("expected " + code + "; the run reported: " + String.concat ", " failed)

[<Tests>]
let corpusTests =
    testList
        "Execution / the specification corpus"
        [ test "the corpus is present, and is the one this mirror was pinned against" {
              // The manifest pin is the drift trigger the behavioural checks cannot be:
              // a specification can move in ways this mirror still happens to satisfy,
              // and "still passes" is not the same statement as "still current".
              let pin = Conformance.checkManifestPin (SpecCorpus.manifestText ())
              Expect.isTrue pin.Passed pin.Detail
          }

          test "the manifest enumerates the families this mirror models" {
              let m = SpecCorpus.manifest ()
              Expect.equal m.Specification "model-execution-wire" "the corpus names the specification"
              Expect.equal m.EnvelopeVersion Document.envelopeVersion "and the envelope version this mirror implements"

              Expect.equal
                  (List.sort m.Families)
                  (List.sort Conformance.implementedFamilies)
                  "every enumerated family is modelled, and nothing is claimed that is not enumerated"
          }

          test "the executor profile certifies against every vector" {
              match
                  Conformance.certify
                      certifyingProfile
                      (SpecCorpus.manifestText ())
                      (SpecCorpus.manifest ())
                      SpecCorpus.load
              with
              | Ok() -> ()
              | Error detail -> failtestf "the mirror does not conform: %s" detail
          }

          test "and every individual check in the run passes" {
              let all = results ()

              // A run that reported nothing would "pass" vacuously.
              Expect.isGreaterThan (List.length all) 60 "the run exercised a substantial number of checks"

              for r in all do
                  Expect.isTrue r.Passed (r.Code + " @ " + r.Vector + ": " + r.Detail)
          }

          test "the number of vectors executed equals the number the manifest enumerates" {
              // §9.3 asks this of a harness rather than of an emitter, because a
              // conformance suite is exactly the kind of code that passes by doing
              // nothing. Asserted here as well as inside the run so that a harness which
              // stopped counting is caught by something other than itself.
              let m = SpecCorpus.manifest ()
              let selected = Conformance.vectorsFor certifyingProfile m

              Expect.equal
                  (List.length selected)
                  (List.length m.Vectors)
                  "the executor profile runs every family, so it runs every vector"

              let executed =
                  results ()
                  |> List.map (fun r -> r.Vector)
                  |> List.filter (fun v -> v <> "manifest")
                  |> List.distinct

              Expect.equal (List.length executed) (List.length selected) "every enumerated vector ran"
          }

          test "the profiles are cumulative, and a narrower one runs strictly fewer vectors" {
              let m = SpecCorpus.manifest ()

              let observer =
                  Conformance.vectorsFor Observer m |> List.map (fun v -> v.Id) |> Set.ofList

              let submitter =
                  Conformance.vectorsFor SubmitterProfile m
                  |> List.map (fun v -> v.Id)
                  |> Set.ofList

              let executor =
                  Conformance.vectorsFor ExecutorProfile m
                  |> List.map (fun v -> v.Id)
                  |> Set.ofList

              Expect.isTrue (Set.isSubset observer submitter) "an observer's vectors are a submitter's"
              Expect.isTrue (Set.isSubset submitter executor) "a submitter's are an executor's"
              Expect.isLessThan (Set.count observer) (Set.count executor) "and the profiles genuinely differ"
          }

          test "the observer profile certifies too — a read-only consumer is conformant" {
              // Worth asserting separately: a claim of conformance names a profile, and
              // an implementation that can only certify at the widest one has made the
              // stratification meaningless.
              match
                  Conformance.certify Observer (SpecCorpus.manifestText ()) (SpecCorpus.manifest ()) SpecCorpus.load
              with
              | Ok() -> ()
              | Error detail -> failtestf "the mirror does not conform as an observer: %s" detail
          }

          test "no family is reserved — an empty list is a statement, not an omission" {
              // If this fails the specification has reserved a name, which is additive
              // and means a family is coming rather than that anything is broken. The
              // manifest pin will have fired first; this names what changed.
              Expect.isEmpty (SpecCorpus.manifest ()).ReservedFamilies "reservedFamilies"
          } ]

[<Tests>]
let goRedTests =
    testList
        "Execution / the harness goes red"
        [ test "a fixture whose bytes were edited is caught" {
              let m = SpecCorpus.manifest ()

              Conformance.run
                  certifyingProfile
                  (SpecCorpus.manifestText ())
                  m
                  (SpecCorpus.loadMutated "quarterly-panel" "quarterly-panelX")
              |> expectFailure ExecutionCodes.fixtureDrift
          }

          test "a manifest that moved is caught even when every vector still passes" {
              // The mutation is a description, which no behavioural check reads — so
              // this is the pin doing the one job the vectors cannot do for it.
              let moved =
                  (SpecCorpus.manifestText ()).Replace("The join, on its own.", "The join.")

              Expect.notEqual moved (SpecCorpus.manifestText ()) "the probe must actually change the manifest"

              Conformance.run certifyingProfile moved (SpecCorpus.manifest ()) SpecCorpus.load
              |> expectFailure ExecutionCodes.manifestDrift
          }

          test "an outcome whose stated identity was altered is caught" {
              // §4.3 consequence 3 — corrupt, and refused rather than stored. The
              // altered document is still schema-valid; only the identity rule sees it.
              let m = SpecCorpus.manifest ()

              Conformance.run
                  certifyingProfile
                  (SpecCorpus.manifestText ())
                  m
                  (SpecCorpus.loadMutated
                      "\"compositeKeyHash\":\"sha256:2ac4e6853673e53cfd662018aafa79a8ba026b40e1648e9adb5da829c5744980\""
                      "\"compositeKeyHash\":\"sha256:2ac4e6853673e53cfd662018aafa79a8ba026b40e1648e9adb5da829c5744981\"")
              |> expectFailure ExecutionCodes.fixtureDrift
          }

          test "a reject vector that the reader accepts is caught" {
              // Repair the out-of-range limit so the query becomes readable. The vector
              // still says it must be refused, so the harness must notice that it was
              // not — this is the check that catches a reader which validates nothing.
              let m = SpecCorpus.manifest ()

              let load (v: Vector) =
                  if v.Id = "registry-query/reject-limit" then
                      (SpecCorpus.load v).Replace("\"limit\":0", "\"limit\":50")
                  else
                      SpecCorpus.load v

              let results = Conformance.run certifyingProfile (SpecCorpus.manifestText ()) m load

              expectFailure ExecutionCodes.rejectAccepted results
          }

          test "a mutated spec-hash payload is caught by the minting rule" {
              // The payload changes, so its minted hash no longer equals the one the
              // submission carries. Two checks must fire: the recomputed digest and the
              // pinned canonical intermediate.
              let m = SpecCorpus.manifest ()

              let load (v: Vector) =
                  if v.Id = "spec-hash/canonical-form" then
                      (SpecCorpus.load v).Replace("\\\"scale\\\":2.5", "\\\"scale\\\":2.6")
                  else
                      SpecCorpus.load v

              let results = Conformance.run certifyingProfile (SpecCorpus.manifestText ()) m load
              expectFailure ExecutionCodes.digestDrift results
              expectFailure ExecutionCodes.canonicalPayloadDrift results
          }

          test "a round-trip vector whose member order was permuted is caught" {
              // §3.2 divergence 1 — a record's members are in declaration order, and an
              // implementation that sorts them produces a different, non-conformant
              // document. Re-encoding restores the declaration order, so the bytes no
              // longer match what was read.
              let m = SpecCorpus.manifest ()

              let load (v: Vector) =
                  if v.Id = "vintage-ref/ref-unpinned" then
                      "{\"envelopeVersion\":1,\"kind\":\"vintageRef\",\"body\":{\"contentRef\":null,\"datasetId\":\"quarterly-panel\",\"version\":7}}"
                  else
                      SpecCorpus.load v

              let results = Conformance.run certifyingProfile (SpecCorpus.manifestText ()) m load
              expectFailure ExecutionCodes.roundTripBytes results
          }

          test "certify reports the failure rather than swallowing it" {
              match
                  Conformance.certify
                      certifyingProfile
                      ((SpecCorpus.manifestText ()).Replace("\"envelopeVersion\": 1", "\"envelopeVersion\": 2"))
                      (SpecCorpus.manifest ())
                      SpecCorpus.load
              with
              | Ok() -> failtest "a moved corpus certified"
              | Error detail -> Expect.stringContains detail ExecutionCodes.manifestDrift "the verdict names the pin"
          }

          test "every probe is exercised — a mutation nobody runs proves nothing" {
              // The mutations above each break one thing. This asserts that each of them
              // genuinely changes the corpus it is applied to, so that a probe which
              // silently matched nothing cannot masquerade as a passing test.
              let probes =
                  [ "quarterly-panel", "quarterly-panelX"
                    "\"limit\":0", "\"limit\":50"
                    "\\\"scale\\\":2.5", "\\\"scale\\\":2.6" ]

              let corpus =
                  (SpecCorpus.manifest ()).Vectors
                  |> List.map SpecCorpus.load
                  |> String.concat "\n"

              for find, _ in probes do
                  Expect.stringContains corpus find ("the probe '" + find + "' must match something in the corpus")
          } ]
