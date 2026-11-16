// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ExecutionPortTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Execution

// ============================================================================
//  The port, its reference implementation, and the bridge into the decision
//  record.
//
//  The acceptance this suite is written for: the manager runs a full scripted
//  exploration against the in-memory backend, with every outcome reaching a
//  branch THROUGH THE PORT — no side door, no synthesised snapshot, no test
//  helper that annotates a branch with something the executor never produced.
// ============================================================================

let private instant = "2026-01-01T00:00:00+00:00"

let private panel = ReferenceExecutor.vintage "quarterly-panel" 7
let private holdout = ReferenceExecutor.vintage "quarterly-holdout" 2

let private config: ReferenceExecutorConfig =
    { ProviderId = "reference-linear"
      ProviderVersion = "1.4.0"
      ProviderKinds = [ "reference-linear" ]
      Vintages = [ panel; holdout ]
      Instant = instant
      Quota = Some 100000.0
      DiagnosticUnits = Map.empty }

let private executor () = ReferenceExecutor.create config

let private bridgeConfig: BridgeConfig =
    { ProviderKind = "reference-linear"
      SpecHashAlgorithm = Minting.algorithmId
      BatchPrefix = "manager" }

/// A payload and the hash a conformant submitter mints for it. Minted rather than
/// invented, so that nothing in this suite carries a hash the rule would refuse.
let private minted (body: string) : Fuaran.Modelling.SpecPayload =
    match Minting.mint body with
    | Error refusal -> failwithf "the fixture payload is not mintable: %s" (Minting.describeRefusal refusal)
    | Ok hash ->
        { Dialect = "reference"
          Format = "json"
          Body = body
          SpecHash = hash }

let private wireSubmission (body: string) (gates: Gate list) : FitSubmission =
    let payload = minted body

    { Vintage = { panel with ContentRef = None }
      SpecPayload = payload.Body
      SpecHash = payload.SpecHash
      SpecHashAlgorithm = Minting.algorithmId
      ProviderKind = "reference-linear"
      Seed = 20260720L
      Gates = gates
      SubmitterClass = SubmitterHuman }

/// A gate the reference executor's deterministic diagnostic always clears, and one it
/// never does. Both are derived from what the executor actually produces rather than
/// guessed, because a gate fixture that happens to pass teaches nothing.
let private alwaysPasses (specHash: string) (name: string) : Gate =
    { Name = name
      Threshold = ReferenceExecutor.diagnostic specHash name
      Unit = None
      Direction = AtLeast }

/// An executor whose provider reports one diagnostic in kilograms (§5.3's gate rule).
let private unitExecutor () =
    ReferenceExecutor.create
        { config with
            DiagnosticUnits = Map.ofList [ "rmse", "kg" ] }

let private neverPasses (specHash: string) (name: string) : Gate =
    { Name = name
      Threshold = ReferenceExecutor.diagnostic specHash name + 0.5
      Unit = None
      Direction = AtLeast }

[<Tests>]
let mirrorTierTests =
    testList
        "Execution / the mirror tier stays a mirror"
        [ test "no file above the bridge takes the substrate's own wire model" {
              // The pinned-snapshot posture, made checkable rather than remembered. The
              // mirror models a FOREIGN specification, and the substrate's canonical wire
              // discipline differs from it in member ordering, string escaping, real
              // formatting and the existence of null — four silent, hash-changing
              // divergences. Borrowing it would make the mirror a translation.
              let root =
                  System.IO.Path.Combine(SpecCorpus.repoRoot (), "src", "Fuaran.Modelling.Execution")

              let mirrorTier =
                  [ "Mirror.fs"
                    "Canonical.fs"
                    "Codec.fs"
                    "Port.fs"
                    "Reference.fs"
                    "Conformance.fs" ]

              for file in mirrorTier do
                  let path = System.IO.Path.Combine(root, file)
                  Expect.isTrue (System.IO.File.Exists path) ("the mirror tier still contains " + file)
                  let text = System.IO.File.ReadAllText path

                  Expect.isFalse
                      (text.Contains "open Fuaran.Core")
                      (file
                       + " opens Fuaran.Core; the mirror tier is FSharp.Core and the BCL only — the bridge is where the substrate's types enter")

                  // One qualified reference is admitted, and only one (D39): the unit algebra,
                  // which the specification's unit-text rule reads and which has no wire of its
                  // own to borrow. Anything else reached by a qualified name would be the
                  // substrate's wire model arriving by a side door.
                  let qualified =
                      System.Text.RegularExpressions.Regex.Matches(text, @"Fuaran\.Core\.[A-Za-z]+")
                      |> Seq.map (fun m -> m.Value)
                      |> Seq.distinct
                      |> List.ofSeq

                  for reference in qualified do
                      Expect.equal
                          reference
                          "Fuaran.Core.Unit"
                          (file
                           + " reaches "
                           + reference
                           + "; the mirror tier admits the unit algebra and nothing else")
          }

          test "the canonical real encoder follows the ECMAScript layout, not the .NET one" {
              // The divergence that would otherwise be invisible until a digest
              // disagreed across two parties, with nothing in either record saying why.
              Expect.equal (Canonical.real 1e-7) "1e-7" "a small value keeps its exponent"
              Expect.equal (Canonical.real 1e21) "1e+21" "and so does a large one"
              Expect.equal (Canonical.real 1e3) "1000" "an exponent that fits becomes its shortest decimal"
              Expect.equal (Canonical.real 1.50) "1.5" "a trailing zero is dropped"
              Expect.equal (Canonical.real -0.0) "0" "negative zero collapses"
              Expect.equal (Canonical.real 42.0) "42" "an integral real has no decimal point"
              Expect.equal (Canonical.real -0.0025) "-0.0025" "and a small negative keeps its digits"
          }

          test "64-bit integers carry an explicit sign, including zero" {
              Expect.equal (Canonical.int64Text 0L) "+0" "zero is signed"
              Expect.equal (Canonical.int64Text -1L) "-1" "and so is a negative"
              Expect.equal (Canonical.int64Text 9007199254740993L) "+9007199254740993" "beyond exact double range"

              Expect.equal
                  (Canonical.tryParseInt64Text "+9007199254740993")
                  (Ok 9007199254740993L)
                  "and it reads back exactly"

              Expect.isError (Canonical.tryParseInt64Text "20260720") "a bare decimal is a different encoding"
          }

          test "the minting rule refuses what it cannot mint deterministically" {
              Expect.isError (Minting.mint "{\"a\":1,\"a\":2}") "duplicate members have no deterministic ordering"
              Expect.isError (Minting.mint "not json") "a rendering that is not JSON is outside the domain"

              Expect.isError
                  (Minting.mint "{\"id\":9007199254740993}")
                  "a number beyond exact double range is rendered as a string instead"

              Expect.isOk (Minting.mint "{\"id\":\"9007199254740993\"}") "which is the shape the rule requires"
          }

          test "two authoring orders of one specification mint one hash" {
              // §4.2 rule 3, made observable — and the property the composite key rests
              // on, since nothing at the seam is permitted to verify it.
              let a =
                  "{\"family\":\"linear\",\"prior\":{\"scale\":2.5},\"terms\":[\"price\",\"promotion\"]}"

              let b =
                  "{ \"terms\" : [ \"price\", \"promotion\" ],\n  \"prior\": {\"scale\":2.5},\n\"family\":\"linear\" }"

              Expect.notEqual a b "the two renderings genuinely differ"
              Expect.equal (Minting.mint a) (Minting.mint b) "but they are one specification"

              let permuted =
                  "{\"family\":\"linear\",\"prior\":{\"scale\":2.5},\"terms\":[\"promotion\",\"price\"]}"

              Expect.notEqual
                  (Minting.mint a)
                  (Minting.mint permuted)
                  "an array's order is data, so permuting it is a different specification"
          } ]

[<Tests>]
let referenceExecutorTests =
    testList
        "Execution / the reference executor"
        [ test "a batch of fits comes back as a receipt that accounts for every item" {
              let port = executor ()
              let s = wireSubmission "{\"family\":\"linear\"}" []

              let batch =
                  { BatchId = "batch-1"
                    Submissions =
                      [ s
                        { s with Seed = -1L }
                        { s with
                            ProviderKind = "hierarchical-bayes" } ] }

              match port.SubmitFits batch with
              | Error refusal -> failtestf "the batch was refused: %s" (Refusal.describe refusal)
              | Ok receipt ->
                  Expect.equal receipt.ItemCount 3 "the receipt echoes what it was given"
                  Expect.isTrue (Port.receiptIsTotal receipt) "accepted and rejected partition the indices exactly"
                  Expect.equal (List.length receipt.Accepted) 2 "two items were taken"

                  match receipt.Rejected with
                  | [ item ] ->
                      Expect.equal item.Index 2 "the third item is the one that was refused"

                      match item.Reason with
                      | UnknownProvider(kind, known) ->
                          Expect.equal kind "hierarchical-bayes" "the refusal names what was asked for"
                          Expect.contains known "reference-linear" "and what IS available, so a submitter can re-aim"
                      | other -> failtestf "expected an unknown-provider refusal, got %s" (Refusal.describe other)
                  | other -> failtestf "expected exactly one rejection, got %d" (List.length other)
          }

          test "partial acceptance is data — a bad item never refuses the whole batch" {
              let port = executor ()
              let s = wireSubmission "{\"family\":\"linear\"}" []

              let batch =
                  { BatchId = "batch-2"
                    Submissions =
                      [ { s with
                            ProviderKind = "hierarchical-bayes" }
                        s ] }

              match port.SubmitFits batch with
              | Error refusal -> failtestf "the batch was refused whole: %s" (Refusal.describe refusal)
              | Ok receipt ->
                  Expect.equal (List.length receipt.Accepted) 1 "the good item was still taken"
                  Expect.equal (List.length receipt.Rejected) 1 "and the bad one reported"
          }

          test "an empty batch IS refused whole — a receipt for nothing says nothing" {
              let port = executor ()

              match
                  port.SubmitFits
                      { BatchId = "batch-3"
                        Submissions = [] }
              with
              | Ok _ -> failtest "an empty batch produced a receipt"
              | Error refusal ->
                  Expect.equal (Refusal.wireClass refusal) "invalidSubmission" "refused as an invalid submission"
          }

          test "gate verdicts are computed from the thresholds the submitter declared" {
              let port = executor ()
              let payload = minted "{\"family\":\"linear\",\"terms\":[\"price\"]}"

              let submission =
                  { wireSubmission payload.Body [] with
                      SpecHash = payload.SpecHash
                      Gates =
                          [ alwaysPasses payload.SpecHash "coverage"
                            neverPasses payload.SpecHash "r-squared" ] }

              match Port.submitOne port "batch-gates" submission with
              | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)
              | Ok _ ->
                  match
                      port.GetOutcomes
                          { SpecHashes = [ payload.SpecHash ]
                            Vintages = []
                            Statuses = []
                            BatchId = None
                            Page = Page.first 10 }
                  with
                  | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)
                  | Ok page ->
                      match page.Outcomes with
                      | [ outcome ] ->
                          let byName =
                              outcome.GateVerdicts |> List.map (fun v -> v.Name, v.Passed) |> Map.ofList

                          Expect.equal
                              (Map.tryFind "coverage" byName)
                              (Some true)
                              "the gate at the observed value holds"

                          Expect.equal (Map.tryFind "r-squared" byName) (Some false) "the one above it does not"

                          Expect.equal
                              outcome.Status
                              "failed"
                              "a fit that did not clear its gates is registered as failed"

                          Expect.isNone outcome.ArtifactRef "and retains no artifact — but is registered anyway"
                      | other -> failtestf "expected one outcome, got %d" (List.length other)
          }

          test "the same submission twice is the same fit, addressed identically" {
              // §4.3 consequence 1 — re-fitting with the same five values is the same
              // fit, so the registry holds one outcome and not two.
              let port = executor ()
              let s = wireSubmission "{\"family\":\"linear\"}" []

              Port.submitOne port "batch-a" s |> ignore
              Port.submitOne port "batch-b" s |> ignore

              match
                  port.GetOutcomes
                      { SpecHashes = []
                        Vintages = []
                        Statuses = []
                        BatchId = None
                        Page = Page.first 10 }
              with
              | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)
              | Ok page -> Expect.equal (List.length page.Outcomes) 1 "one fit, recorded once"
          }

          test "changing any component of the key is a different fit" {
              let port = executor ()
              let s = wireSubmission "{\"family\":\"linear\"}" []

              Port.submitOne port "batch-a" s |> ignore
              Port.submitOne port "batch-a" { s with Seed = 1L } |> ignore

              Port.submitOne
                  port
                  "batch-a"
                  { s with
                      Vintage = { holdout with ContentRef = None } }
              |> ignore

              match
                  port.GetOutcomes
                      { SpecHashes = []
                        Vintages = []
                        Statuses = []
                        BatchId = None
                        Page = Page.first 10 }
              with
              | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)
              | Ok page ->
                  Expect.equal (List.length page.Outcomes) 3 "three different fits"

                  Expect.equal
                      (page.Outcomes
                       |> List.map (fun o -> o.CompositeKeyHash)
                       |> List.distinct
                       |> List.length)
                      3
                      "with three distinct addresses"
          }

          test "paging is deterministic, ordered, and total" {
              let port = executor ()

              for i in 1..7 do
                  Port.submitOne port "batch-page" (wireSubmission (sprintf "{\"family\":\"linear\",\"i\":%d}" i) [])
                  |> ignore

              let query =
                  { SpecHashes = []
                    Vintages = []
                    Statuses = []
                    BatchId = None
                    Page = Page.first 3 }

              match Port.readAll port 10 query with
              | Error refusal -> failtestf "paging was refused: %s" (Refusal.describe refusal)
              | Ok outcomes ->
                  Expect.equal (List.length outcomes) 7 "every outcome was read"

                  Expect.equal
                      (outcomes |> List.map (fun o -> o.CompositeKeyHash))
                      (outcomes
                       |> List.map (fun o -> o.CompositeKeyHash)
                       |> List.sortWith (fun a b -> System.String.CompareOrdinal(a, b)))
                      "ordered by an address that exists before the outcome is stored"

                  Expect.equal
                      (outcomes |> List.distinctBy (fun o -> o.CompositeKeyHash) |> List.length)
                      7
                      "with nothing repeated across the page boundary"
          }

          test "an unusable cursor is refused, never restarted from the beginning" {
              // A silent restart turns a paging loop into an infinite one, and the caller
              // has no way to tell that it happened.
              let port = executor ()

              Port.submitOne port "batch-cursor" (wireSubmission "{\"family\":\"linear\"}" [])
              |> ignore

              let query =
                  { SpecHashes = []
                    Vintages = []
                    Statuses = []
                    BatchId = None
                    Page =
                      { Cursor = Some "not-a-cursor-this-executor-issued"
                        Limit = 10 } }

              match port.GetOutcomes query with
              | Ok _ -> failtest "an unrecognised cursor was silently accepted"
              | Error refusal -> Expect.equal (Refusal.wireClass refusal) "invalidQuery" "refused as an invalid query"
          }

          test "an out-of-range page limit is refused" {
              let port = executor ()

              let query =
                  { SpecHashes = []
                    Vintages = []
                    Statuses = []
                    BatchId = None
                    Page = { Cursor = None; Limit = 0 } }

              match port.GetOutcomes query with
              | Ok _ -> failtest "a zero limit was accepted"
              | Error refusal -> Expect.equal (Refusal.wireClass refusal) "invalidQuery" "refused as an invalid query"
          }

          test "resolving a vintage answers with its bytes, or says there is no such thing" {
              let port = executor ()

              match
                  port.ResolveVintage
                      { DatasetId = "quarterly-panel"
                        Version = 7
                        ContentRef = None }
              with
              | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)
              | Ok resolved ->
                  Expect.isSome resolved.Ref.ContentRef "a resolution that resolves nothing is not an answer"
                  Expect.isTrue resolved.IsLatest "and it is the highest version of that dataset"

              match
                  port.ResolveVintage
                      { DatasetId = "quarterly-panel"
                        Version = 99
                        ContentRef = None }
              with
              | Ok _ -> failtest "an unknown vintage resolved"
              | Error refusal -> Expect.equal (Refusal.wireClass refusal) "notFound" "refused as not found"
          }

          test "scoring lands predictions as a new vintage, and refuses to overwrite its own input" {
              let port = executor ()
              let payload = minted "{\"family\":\"linear\",\"terms\":[\"price\"]}"

              let submission =
                  { wireSubmission payload.Body [] with
                      SpecHash = payload.SpecHash }

              Port.submitOne port "batch-score" submission |> ignore

              let keyHash =
                  match
                      port.GetOutcomes
                          { SpecHashes = [ payload.SpecHash ]
                            Vintages = []
                            Statuses = []
                            BatchId = None
                            Page = Page.first 10 }
                  with
                  | Ok page -> (List.head page.Outcomes).CompositeKeyHash
                  | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)

              match
                  port.Score
                      { ArtifactKeyHash = keyHash
                        Input = { holdout with ContentRef = None }
                        OutputDatasetId = "quarterly-holdout-predictions" }
              with
              | Error refusal -> failtestf "scoring was refused: %s" (Refusal.describe refusal)
              | Ok resolved ->
                  Expect.equal resolved.Ref.DatasetId "quarterly-holdout-predictions" "predictions land as data"
                  Expect.isSome resolved.Ref.ContentRef "with an identity and a content hash"

              match
                  port.Score
                      { ArtifactKeyHash = keyHash
                        Input = { holdout with ContentRef = None }
                        OutputDatasetId = "quarterly-holdout" }
              with
              | Ok _ -> failtest "predictions were written back over their own input"
              | Error refusal ->
                  Expect.equal (Refusal.wireClass refusal) "invalidSubmission" "refused as an invalid submission"
          }

          test "artifacts can be queried, a page at a time" {
              let port = executor ()

              for i in 1..3 do
                  Port.submitOne port "batch-art" (wireSubmission (sprintf "{\"family\":\"linear\",\"i\":%d}" i) [])
                  |> ignore

              match
                  port.QueryArtifacts
                      { ArtifactKeyHashes = []
                        Page = Page.first 2 }
              with
              | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)
              | Ok page ->
                  Expect.equal (List.length page.Artifacts) 2 "the page is the size asked for"
                  Expect.isSome page.NextCursor "and there is more to read"
          }

          test "a re-vintage request is an opaque trigger that produces a new version" {
              let port = executor ()

              match
                  port.RequestRevintage
                      { DatasetId = "quarterly-panel"
                        AssemblyRef = "assembly-17" }
              with
              | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)
              | Ok resolved ->
                  Expect.equal resolved.Ref.Version 8 "the next version of that dataset"
                  Expect.isTrue resolved.IsLatest "and it is now the latest"
          }

          test "a budget that is spent refuses the work before it runs" {
              let port = ReferenceExecutor.create { config with Quota = Some 0.0 }

              match Port.submitOne port "batch-budget" (wireSubmission "{\"family\":\"linear\"}" []) with
              | Error refusal -> failtestf "the batch was refused whole: %s" (Refusal.describe refusal)
              | Ok receipt ->
                  match receipt.Rejected with
                  | [ { Reason = BudgetDenied _ } ] -> ()
                  | other -> failtestf "expected a budget refusal, got %A" other
          }

          test "a backend that offers nothing refuses every operation by name" {
              // Not a test double: a deployment that composes some of the interface needs
              // a shape for "not offered here" that is distinguishable from "absent".
              let port = Port.unavailable "none" "0"

              let surfaces =
                  [ (match port.SubmitFits { BatchId = "b"; Submissions = [] } with
                     | Error r -> Refusal.wireClass r
                     | Ok _ -> "accepted")
                    (match
                        port.ResolveVintage
                            { DatasetId = "d"
                              Version = 1
                              ContentRef = None }
                     with
                     | Error r -> Refusal.wireClass r
                     | Ok _ -> "accepted")
                    (match port.RequestRevintage { DatasetId = "d"; AssemblyRef = "a" } with
                     | Error r -> Refusal.wireClass r
                     | Ok _ -> "accepted") ]

              for cls in surfaces do
                  Expect.equal cls "substrateUnavailable" "every operation refuses with the surface it does not offer"
          }

          test "the executor is deterministic — two builds agree on every byte" {
              let run () =
                  let port = executor ()

                  Port.submitOne port "batch-det" (wireSubmission "{\"family\":\"linear\"}" [])
                  |> ignore

                  match
                      port.GetOutcomes
                          { SpecHashes = []
                            Vintages = []
                            Statuses = []
                            BatchId = None
                            Page = Page.first 10 }
                  with
                  | Ok page -> Codec.render (OutcomePageDoc page)
                  | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)

              Expect.equal (run ()) (run ()) "the same submission yields the same record, byte for byte"
          }

          test "everything the executor emits is itself conformant" {
              // The strongest single statement available about a reference
              // implementation: what it produces is not merely plausible, it round-trips
              // through the reader that certifies against the specification's corpus.
              let port = executor ()
              let payload = minted "{\"family\":\"linear\",\"terms\":[\"price\"]}"

              let submission =
                  { wireSubmission payload.Body [ alwaysPasses payload.SpecHash "coverage" ] with
                      SpecHash = payload.SpecHash }

              let receipt =
                  match Port.submitOne port "batch-conformant" submission with
                  | Ok r -> r
                  | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)

              let page =
                  match
                      port.GetOutcomes
                          { SpecHashes = []
                            Vintages = []
                            Statuses = []
                            BatchId = None
                            Page = Page.first 10 }
                  with
                  | Ok p -> p
                  | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)

              let resolved =
                  match
                      port.ResolveVintage
                          { DatasetId = "quarterly-panel"
                            Version = 7
                            ContentRef = None }
                  with
                  | Ok r -> r
                  | Error refusal -> failtestf "refused: %s" (Refusal.describe refusal)

              let documents =
                  [ FitSubmissionDoc submission
                    FitSubmissionBatchDoc
                        { BatchId = "batch-conformant"
                          Submissions = [ submission ] }
                    SubmissionReceiptDoc receipt
                    OutcomePageDoc page
                    ResolvedVintageDoc resolved
                    CompositeKeyDoc (List.head page.Outcomes).CompositeKey
                    FitOutcomeDoc(List.head page.Outcomes) ]

              for document in documents do
                  let bytes = Codec.render document

                  match Codec.read bytes with
                  | Error refusal ->
                      failtestf
                          "the executor emitted a %s the reader refuses as '%s': %s"
                          (Document.kindOf document)
                          refusal.RejectClass
                          refusal.Detail
                  | Ok back ->
                      Expect.equal
                          (Codec.render back)
                          bytes
                          ("the emitted " + Document.kindOf document + " round-trips")

              // And the submission it built satisfies the one obligation nothing
              // downstream is permitted to check.
              match Submitter.checkMinting submission with
              | Ok() -> ()
              | Error refusal ->
                  failtestf "the submission this suite built is not conformantly minted: %s" refusal.Detail
          } ]

[<Tests>]
let unitTests =
    testList
        "Execution / a diagnostic states its unit"
        [ test "a gate in the diagnostic's unit is evaluated, and the outcome states the unit" {
              let payload = minted "{\"family\":\"linear\",\"units\":1}"

              let gate =
                  { alwaysPasses payload.SpecHash "rmse" with
                      Unit = Some "kg" }

              let port = unitExecutor ()

              match
                  port.SubmitFits
                      { BatchId = "units-1"
                        Submissions = [ wireSubmission payload.Body [ gate ] ] }
              with
              | Ok receipt when List.isEmpty receipt.Rejected ->
                  match
                      port.GetOutcomes
                          { SpecHashes = []
                            Vintages = []
                            Statuses = []
                            BatchId = Some "units-1"
                            Page = Page.first Page.maxLimit }
                  with
                  | Ok page ->
                      let o = List.exactlyOne page.Outcomes
                      Expect.equal o.DiagnosticUnits (Map.ofList [ "rmse", "kg" ]) "the outcome says what rmse measures"
                      Expect.equal (List.exactlyOne o.GateVerdicts).Unit (Some "kg") "and the verdict carries its unit"

                      match Codec.read (Codec.render (FitOutcomeDoc o)) with
                      | Ok(FitOutcomeDoc back) -> Expect.equal back o "and it round-trips through the codec"
                      | other -> failtestf "the outcome did not read back: %A" other
                  | Error r -> failtestf "the registry refused: %s" (Refusal.describe r)
              | other -> failtestf "the submission was not taken: %A" other
          }

          test "a gate in grams against a diagnostic in kilograms is refused by name, never converted" {
              let payload = minted "{\"family\":\"linear\",\"units\":2}"

              let inGrams =
                  { alwaysPasses payload.SpecHash "rmse" with
                      Unit = Some "g" }

              let unstated = alwaysPasses payload.SpecHash "rmse"

              match
                  (unitExecutor ()).SubmitFits
                      { BatchId = "units-2"
                        Submissions =
                          [ wireSubmission payload.Body [ inGrams ]
                            wireSubmission payload.Body [ unstated ] ] }
              with
              | Ok receipt ->
                  Expect.isEmpty receipt.Accepted "neither comparison may be made"

                  match receipt.Rejected |> List.map (fun r -> r.Reason) with
                  | [ GateUnitMismatch("rmse", Some "g", Some "kg"); GateUnitMismatch("rmse", None, Some "kg") ] -> ()
                  | other -> failtestf "expected two gate-unit mismatches naming the gate, got %A" other
              | Error r -> failtestf "the batch was refused whole: %s" (Refusal.describe r)
          }

          test "the unit reader agrees with the specification's worked examples of canonical text (§3.5)" {
              for canonical in [ "kg.m/s2"; "/min"; "c[GBP]"; "mg/dL"; "1"; "kg"; "k[GBP]"; "[in_i]2"; "N.m" ] do
                  Expect.isOk (UnitText.check canonical) (canonical + " is canonical")

              for text, written in [ "s-1", "/s"; "m.kg", "kg.m"; "m.m", "m2"; "ml", "mL"; "kg/(m.s2)", "kg/m/s2" ] do
                  match UnitText.check text with
                  | Error detail -> Expect.stringContains detail ("'" + written + "'") (text + " is written " + written)
                  | Ok() -> failtestf "%s was accepted as canonical" text

              for refused in [ "kg m"; "kilogram"; "kmin"; "10*3"; "Cel"; "[CFU]"; "{beats}/min"; "" ] do
                  Expect.isError (UnitText.check refused) (refused + " is not unit text")
          }

          test "a gate whose unit is not unit text is refused before anything is scheduled" {
              let payload = minted "{\"family\":\"linear\",\"units\":3}"

              let s =
                  wireSubmission
                      payload.Body
                      [ { alwaysPasses payload.SpecHash "rmse" with
                            Unit = Some "kilogram" } ]

              match Codec.validateSubmission s with
              | Error r -> Expect.equal r.RejectClass RejectClass.submissionGateUnitInvalid "the class names the rule"
              | Ok() -> failtest "a unit nobody can read was accepted"
          }

          test "the bridge carries a unit across and maps the mismatch onto the manager's own refusal" {
              let payload = minted "{\"family\":\"linear\",\"units\":4}"
              let backend = Bridge.asExecutionBackend bridgeConfig (unitExecutor ())

              let kg =
                  match Fuaran.Modelling.UnitText.tryParse "kg" with
                  | Ok u -> u
                  | Error e -> failtest e

              let submit (stated: UnitOfMeasure option) =
                  backend.Submit
                      { StateId = "s-units"
                        Payload = payload
                        DatasetVersion = "quarterly-panel@7"
                        Seed = "20260720"
                        Gates =
                          [ { Name = "rmse"
                              Threshold = ReferenceExecutor.diagnostic payload.SpecHash "rmse"
                              Unit = stated
                              Direction = Fuaran.Modelling.AtLeast } ]
                        SubmittedBy = Fuaran.Modelling.SubmitHuman }

              match submit (Some kg) with
              | Ok snapshot ->
                  Expect.equal snapshot.DiagnosticUnits (Map.ofList [ "rmse", kg ]) "the snapshot holds the unit"
              | Error r -> failtestf "refused: %s" (Fuaran.Modelling.BackendRefusal.describe r)

              match submit None with
              | Error(Fuaran.Modelling.GateUnitMismatch m) ->
                  Expect.equal m.Gate "rmse" "the manager's refusal names the gate"
                  Expect.equal m.Reported (Some kg) "and the unit the diagnostic is reported in"
              | other -> failtestf "expected the manager's gate-unit refusal, got %A" other
          } ]

[<Tests>]
let bridgeTests =
    testList
        "Execution / the bridge into the decision record"
        [ test "a dataset version survives the round trip between the two namings" {
              let name = VintageName.encode panel
              Expect.equal name "quarterly-panel@7" "the manager's one-string form"

              match VintageName.tryParse name with
              | Error detail -> failtest detail
              | Ok back ->
                  Expect.equal back.DatasetId panel.DatasetId "the dataset survives"
                  Expect.equal back.Version panel.Version "and the ordinal"
                  Expect.isNone back.ContentRef "read back unpinned — the ordinary shape a submitter emits"

              Expect.isError (VintageName.tryParse "no-version-here") "a name with no ordinal names no vintage"
          }

          test "a seed that is not an integer is refused rather than invented" {
              let payload = minted "{\"family\":\"linear\"}"

              let submission: Fuaran.Modelling.FitSubmission =
                  { StateId = "s1"
                    Payload = payload
                    DatasetVersion = "quarterly-panel@7"
                    Seed = "not-a-number"
                    Gates = []
                    SubmittedBy = SubmitHuman }

              Expect.isError
                  (Bridge.toWireSubmission bridgeConfig submission)
                  "the seed is part of the composite key, so inventing one re-keys somebody else's fit"
          }

          test "an outcome reaches a branch through the port, and only through the port" {
              let port = executor ()
              let backend = Bridge.asExecutionBackend bridgeConfig port
              let payload = minted "{\"family\":\"linear\",\"terms\":[\"price\"]}"
              let policy = Sample.fullPolicy "curator"

              let session =
                  Sample.sessionOrFail
                      [ Seed
                            { StateId = "base"
                              Label = "baseline"
                              Payload = payload } ]

              let submission: Fuaran.Modelling.FitSubmission =
                  { StateId = "base"
                    Payload = payload
                    DatasetVersion = "quarterly-panel@7"
                    Seed = "20260720"
                    Gates =
                      [ { Name = "coverage"
                          Threshold = ReferenceExecutor.diagnostic payload.SpecHash "coverage"
                          Unit = None
                          Direction = Fuaran.Modelling.AtLeast } ]
                    SubmittedBy = SubmitHuman }

              match Session.submitFit Sample.hashFn policy "curator" Sample.curator backend submission session with
              | Error failure -> failtestf "the governed fit did not land: %A" failure
              | Ok(snapshot, _, session') ->
                  Expect.equal
                      snapshot.Key.SpecHash
                      payload.SpecHash
                      "the snapshot names the specification it came from"

                  Expect.equal snapshot.Key.DatasetVersion "quarterly-panel@7" "and the vintage"
                  Expect.equal snapshot.Key.Seed "+20260720" "carrying the seed in the wire's own form"
                  Expect.equal snapshot.Key.ProviderId "reference-linear" "and which provider ran it"
                  Expect.isTrue (OutcomeSnapshot.gatesPassed snapshot) "the gate at the observed value held"

                  match Map.tryFind "base" session'.Record.States with
                  | None -> failtest "the seeded state vanished"
                  | Some state ->
                      Expect.equal (List.length state.Outcomes) 1 "the branch carries exactly one outcome"
                      Expect.equal (List.head state.Outcomes) snapshot "and it is the one the port produced"

                  // The evidence base is self-contained: replaying the line reproduces
                  // the annotation with no backend present at all.
                  match Session.replayState "base" session' with
                  | Error detail -> failtest detail
                  | Ok replayed ->
                      let outcomes = (Map.find "base" replayed.States).Outcomes
                      Expect.equal outcomes [ snapshot ] "the record replays offline, identically"
          }

          test "policy refuses before the port is ever asked" {
              // Three gates, and they are not interchangeable. This one is the manager's:
              // an unauthorised subject never reaches the executor, so an unauthorised
              // fit spends nothing and leaves no trace.
              let port = executor ()
              let backend = Bridge.asExecutionBackend bridgeConfig port
              let payload = minted "{\"family\":\"linear\"}"

              let session =
                  Sample.sessionOrFail
                      [ Seed
                            { StateId = "base"
                              Label = "baseline"
                              Payload = payload } ]

              let submission: Fuaran.Modelling.FitSubmission =
                  { StateId = "base"
                    Payload = payload
                    DatasetVersion = "quarterly-panel@7"
                    Seed = "1"
                    Gates = []
                    SubmittedBy = SubmitAgent }

              match
                  Session.submitFit Sample.hashFn Policy.empty "explorer" Sample.explorer backend submission session
              with
              | Ok _ -> failtest "the empty policy authorised a fit"
              | Error(FitPolicyBlocked refusal) ->
                  Expect.equal refusal.Attempted "submitFit" "the refusal names what was attempted"
              | Error other -> failtestf "expected a policy refusal, got %A" other

              match
                  port.GetOutcomes
                      { SpecHashes = []
                        Vintages = []
                        Statuses = []
                        BatchId = None
                        Page = Page.first 10 }
              with
              | Error refusal -> failtestf "the query was refused: %s" (Refusal.describe refusal)
              | Ok page -> Expect.isEmpty page.Outcomes "and nothing reached the executor"
          }

          test "a backend refusal arrives as the manager's own, naming the wire class it came from" {
              let backend =
                  Bridge.asExecutionBackend
                      { bridgeConfig with
                          ProviderKind = "hierarchical-bayes" }
                      (executor ())

              let payload = minted "{\"family\":\"linear\"}"

              let submission: Fuaran.Modelling.FitSubmission =
                  { StateId = "base"
                    Payload = payload
                    DatasetVersion = "quarterly-panel@7"
                    Seed = "1"
                    Gates = []
                    SubmittedBy = SubmitHuman }

              match backend.Submit submission with
              | Ok _ -> failtest "a fit aimed at a provider this deployment does not offer succeeded"
              | Error(Fuaran.Modelling.UnknownProvider known) ->
                  Expect.contains known "reference-linear" "the manager's refusal still enumerates the alternatives"
              | Error other -> failtestf "expected an unknown-provider refusal, got %A" other
          }

          test "a full scripted exploration runs against the in-memory backend" {
              // The phase's acceptance, end to end: seed, fork twice, fit every branch
              // through the port, curate on the evidence, and replay one line offline.
              let port = executor ()
              let backend = Bridge.asExecutionBackend bridgeConfig port
              let policy = Sample.fullPolicy "curator"

              let payloadFor (terms: string) =
                  minted ("{\"family\":\"linear\",\"terms\":[" + terms + "]}")

              let basePayload = payloadFor "\"price\""
              let leftPayload = payloadFor "\"price\",\"promotion\""
              let rightPayload = payloadFor "\"price\",\"seasonality\""

              let session =
                  Sample.sessionOrFail
                      [ Seed
                            { StateId = "base"
                              Label = "baseline"
                              Payload = basePayload }
                        Fork
                            { StateId = "left"
                              From = "base"
                              Label = "with promotion"
                              Payload = leftPayload }
                        Fork
                            { StateId = "right"
                              From = "base"
                              Label = "with seasonality"
                              Payload = rightPayload } ]

              let fit
                  (stateId: string)
                  (payload: Fuaran.Modelling.SpecPayload)
                  (gate: Fuaran.Modelling.GateRequest)
                  session
                  =
                  let submission: Fuaran.Modelling.FitSubmission =
                      { StateId = stateId
                        Payload = payload
                        DatasetVersion = "quarterly-panel@7"
                        Seed = "20260720"
                        Gates = [ gate ]
                        SubmittedBy = SubmitHuman }

                  match Session.submitFit Sample.hashFn policy "curator" Sample.curator backend submission session with
                  | Error failure -> failtestf "the fit on '%s' did not land: %A" stateId failure
                  | Ok(snapshot, _, session') -> snapshot, session'

              let passing (payload: Fuaran.Modelling.SpecPayload) : Fuaran.Modelling.GateRequest =
                  { Name = "coverage"
                    Threshold = ReferenceExecutor.diagnostic payload.SpecHash "coverage"
                    Unit = None
                    Direction = Fuaran.Modelling.AtLeast }

              let failing (payload: Fuaran.Modelling.SpecPayload) : Fuaran.Modelling.GateRequest =
                  { Name = "coverage"
                    Threshold = ReferenceExecutor.diagnostic payload.SpecHash "coverage" + 0.5
                    Unit = None
                    Direction = Fuaran.Modelling.AtLeast }

              let _, session = fit "base" basePayload (passing basePayload) session
              let leftOutcome, session = fit "left" leftPayload (passing leftPayload) session
              let rightOutcome, session = fit "right" rightPayload (failing rightPayload) session

              Expect.isTrue (OutcomeSnapshot.gatesPassed leftOutcome) "the left branch cleared its gate"
              Expect.isFalse (OutcomeSnapshot.gatesPassed rightOutcome) "the right branch did not"

              // Curate on the evidence. Kill is a VERDICT: the branch stays, and the
              // reason recorded against it is the most reusable thing about it.
              let session =
                  match
                      Session.submitGoverned
                          Sample.hashFn
                          policy
                          "curator"
                          Sample.curator
                          (Sample.curateOp "right" Kill "coverage below the declared threshold")
                          session
                  with
                  | Error failure -> failtestf "the curation did not land: %A" failure
                  | Ok(_, s) -> s

              Expect.equal (List.length session.Record.Order) 3 "nothing was removed by abandoning a branch"

              // Line-local replay: the left line sees itself and its ancestor, and not
              // the branch beside it.
              match Session.replayState "left" session with
              | Error detail -> failtest detail
              | Ok replayed ->
                  Expect.equal replayed.Order [ "base"; "left" ] "the left line is its own lineage"
                  Expect.isFalse (replayed.States.ContainsKey "right") "and does not see its sibling"

                  let outcomes = (Map.find "left" replayed.States).Outcomes
                  Expect.equal outcomes [ leftOutcome ] "with the outcome the port produced, replayed offline"

              // Every fit is in the executor's registry too, addressed by the same
              // composite key the decision record carries.
              match
                  Port.readAll
                      port
                      10
                      { SpecHashes = []
                        Vintages = []
                        Statuses = []
                        BatchId = None
                        Page = Page.first 10 }
              with
              | Error refusal -> failtestf "the registry query was refused: %s" (Refusal.describe refusal)
              | Ok registered ->
                  Expect.equal (List.length registered) 3 "three fits ran"

                  let managerKeys =
                      session.Record.States
                      |> Map.toList
                      |> List.collect (fun (_, s) -> s.Outcomes |> List.map (fun o -> o.Key))
                      |> List.sort

                  let executorKeys =
                      registered
                      |> List.map (fun o -> Bridge.toManagerKey o.CompositeKey)
                      |> List.sort

                  Expect.equal
                      managerKeys
                      executorKeys
                      "the decision record and the execution record name the same fits, sharing no type"
          } ]
