// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.MintingAgreementTests

open Expecto
open Fuaran.Modelling.Execution

// ============================================================================
//  The minting agreement gate.
//
//  `canonical-json-sha256-v1` is a REGISTERED identifier (§7.4). Two parties that
//  declare it and mint differently have not disagreed about anything a reader can
//  see: §4.2 rule 2 forbids everyone downstream from checking a hash they were
//  handed, so the divergence surfaces as a join that silently misses, months
//  later, with nothing in either record saying why.
//
//  This repository has two tiers that mint — the execution mirror (the rule as the
//  specification states it) and the dialect seam (the extension point a dialect
//  author reaches for). They must be one rule, and the way to make that checkable
//  rather than asserted is to drive BOTH over the specification's own `spec-hash`
//  vectors and over documents neither of them has seen.
//
//  Three obligations, and the second is the one the corpus alone cannot carry:
//
//    1. the mirror reproduces every vector's canonical intermediate and digest,
//       byte for byte and hash for hash;
//    2. the SEAM's `Mint` obligation — reached through `IDialect`, exactly as a
//       dialect author reaches it — produces the same hash for the same payload,
//       on every vector;
//    3. both agree on documents drawn at random, so agreement is a property of
//       the rule rather than a coincidence of six fixtures.
//
//  The corpus's absence is a FAILURE here, loudly, via `SpecCorpus`. A gate that
//  quietly does nothing when it cannot find what it is checking against leaves
//  the build green, and everybody reads that as agreement.
// ============================================================================

/// The seam's minting obligation, reached the way a dialect author reaches it — through
/// the interface, not through whatever module happens to implement it. That indirection
/// is the point: this stays a real question after the two tiers are unified, because a
/// future dialect that mints its own way fails it.
let private seamMint (payload: string) : string =
    Fuaran.Modelling.DialectSeam.ReferenceDialect.dialect.Mint payload

let private payloadOf (v: Vector) : FitSubmission =
    match Codec.read (SpecCorpus.load v) with
    | Ok(FitSubmissionDoc s) -> s
    | Ok other -> failtestf "%s is a %s, not a fit submission" v.Id (Document.kindOf other)
    | Error refusal -> failtestf "%s does not read: %s" v.Id refusal.Detail

/// The family's vectors minted under the rule THIS gate checks. The family also carries
/// vectors under the other registered rule (§7.4), which neither tier here implements;
/// the corpus run recomputes those, and the corpus tests assert that it did.
let private specHashVectors () =
    (SpecCorpus.manifest ()).Vectors
    |> List.filter (fun v -> v.Family = "spec-hash")
    |> List.filter (fun v -> (payloadOf v).SpecHashAlgorithm = Minting.algorithmId)

[<Tests>]
let agreementTests =
    testList
        "Minting / the two tiers are one rule"
        [ test "the corpus enumerates a spec-hash family, and it is not empty" {
              // §9.3 asked of the harness rather than of an emitter: a gate that selected
              // nothing would pass every assertion below by never reaching one.
              let vectors = specHashVectors ()
              Expect.isNonEmpty vectors "the spec-hash family"

              Expect.contains
                  (SpecCorpus.manifest ()).Families
                  "spec-hash"
                  "the manifest enumerates the family this gate runs"

              Expect.isTrue
                  (vectors |> List.exists (fun v -> v.Kind = "reject"))
                  "including the submitter-profile reject vector — the one error the rule exists to exclude"
          }

          test "the mirror reproduces every vector's canonical intermediate, byte for byte" {
              // Every HASH vector pins the intermediate. The reject vector deliberately does
              // not — its payload is the same specification as `permuted-form`, and what it
              // pins is a wrong digest over it — so the absence is asserted rather than
              // silently tolerated, which is how a missing pin becomes a skipped check.
              let mutable compared = 0

              for v in specHashVectors () do
                  let submission = payloadOf v

                  match Minting.canonicalise submission.SpecPayload with
                  | Error refusal -> failtestf "%s: %s" v.Id (Minting.describeRefusal refusal)
                  | Ok canonical ->
                      match v.CanonicalPayload, v.Kind with
                      | Some expected, _ ->
                          Expect.equal
                              canonical
                              expected
                              (v.Id
                               + ": the §4.5 intermediate is written out precisely so a divergence says WHERE")

                          compared <- compared + 1
                      | None, "reject" -> ()
                      | None, kind -> failtestf "%s is a '%s' vector and carries no canonicalPayload" v.Id kind

              Expect.equal
                  compared
                  (specHashVectors () |> List.filter (fun v -> v.Kind = "hash") |> List.length)
                  "every hash vector's intermediate was compared"
          }

          test "the mirror reproduces every hash vector's digest" {
              let hashVectors = specHashVectors () |> List.filter (fun v -> v.Kind = "hash")
              Expect.isNonEmpty hashVectors "hash vectors"

              for v in hashVectors do
                  let submission = payloadOf v

                  match Minting.mint submission.SpecPayload with
                  | Error refusal -> failtestf "%s: %s" v.Id (Minting.describeRefusal refusal)
                  | Ok minted ->
                      Expect.equal minted (Option.defaultValue "" v.Digest) (v.Id + ": the recomputed digest")

                      Expect.equal minted submission.SpecHash (v.Id + ": and the hash the submission itself carries")
          }

          test "the seam's Mint obligation agrees with the mirror on every vector" {
              // The whole gate, in one assertion. Both tiers declare the same registered
              // identifier; a hash they do not share means the identifier names two rules.
              for v in specHashVectors () do
                  let submission = payloadOf v

                  Expect.equal
                      submission.SpecHashAlgorithm
                      Minting.algorithmId
                      (v.Id + ": the vector is minted under the rule this gate checks")

                  Expect.equal
                      Fuaran.Modelling.DialectSeam.ReferenceDialect.specHashAlgorithm
                      Minting.algorithmId
                      "and the seam declares that same registered identifier"

                  match Minting.mint submission.SpecPayload with
                  | Error refusal -> failtestf "%s: %s" v.Id (Minting.describeRefusal refusal)
                  | Ok minted ->
                      Expect.equal
                          (seamMint submission.SpecPayload)
                          minted
                          (v.Id
                           + ": the seam and the mirror declare one algorithm identifier, so they mint one hash")
          }

          test "the reject vector is caught by the rule, and by both tiers alike" {
              // §4.2 rule 2 — nothing downstream may check this, so a submitter that gets
              // it wrong is undetectable by design. The corpus's only submitter-profile
              // reject, and the reason the minting rule is a pre-emit obligation.
              let v =
                  specHashVectors ()
                  |> List.find (fun v -> v.Id = "spec-hash/reject-raw-bytes-minted")

              let submission = payloadOf v

              match Submitter.checkMinting submission with
              | Ok() -> failtest "the submitter obligation accepted a hash minted over the raw bytes"
              | Error refusal ->
                  Expect.equal
                      refusal.RejectClass
                      (Option.defaultValue "" v.Reject)
                      "refused with the class the manifest names"

              match Minting.mint submission.SpecPayload with
              | Error refusal -> failtestf "%s: %s" v.Id (Minting.describeRefusal refusal)
              | Ok minted ->
                  Expect.notEqual minted submission.SpecHash "the carried hash is over the wrong bytes"

                  Expect.equal
                      (seamMint submission.SpecPayload)
                      minted
                      "and both tiers agree about what the right bytes mint to"

              // The executor is FORBIDDEN to notice. Asserted because the obligation being
              // pre-emit is a design decision, and a reader that quietly started checking
              // would be a conformance defect that looks like diligence.
              match Codec.read (SpecCorpus.load v) with
              | Ok _ -> ()
              | Error refusal ->
                  failtestf "a reader refused a document §4.2 rule 2 forbids it to check: %s" refusal.Detail
          } ]

// ---- the property suite ------------------------------------------------------
//
//  Six fixtures pin six shapes. They cannot say that the two tiers agree on a
//  document nobody wrote — which is the case that matters, because the rule's whole
//  job is to be applied to specifications this repository has never seen.

module Generator = MintingGenerator

[<Tests>]
let propertyTests =
    testList
        "Minting / the rule, over documents nobody wrote"
        [ test "the two tiers agree on every generated document" {
              let rng = System.Random 20260805
              let mutable checked' = 0

              for _ in 1..400 do
                  let doc = Generator.document rng
                  let rendered = Generator.render doc

                  match Minting.mint rendered with
                  | Error refusal ->
                      failtestf
                          "the generator produced a rendering outside the rule's domain: %s\n%s"
                          (Minting.describeRefusal refusal)
                          rendered
                  | Ok minted ->
                      Expect.equal (seamMint rendered) minted ("the tiers disagree on: " + rendered)
                      checked' <- checked' + 1

              Expect.equal checked' 400 "every generated document was actually minted"
          }

          test "member order and whitespace do not change a hash; array order does" {
              let rng = System.Random 20260806

              for _ in 1..200 do
                  let doc = Generator.document rng
                  let a = Generator.render doc
                  let b = Generator.renderPermuted doc

                  match Minting.mint a, Minting.mint b with
                  | Ok ha, Ok hb ->
                      Expect.equal ha hb ("one specification, two renderings:\n" + a + "\n" + b)
                      Expect.equal (seamMint a) (seamMint b) "and the seam says so too"
                  | x, y -> failtestf "expected two hashes, got %A / %A" x y

              // An array's order is DATA. Stated here rather than assumed, because a rule
              // that sorted everything would satisfy every assertion above.
              let ordered = """{"terms":["price","promotion"]}"""
              let reordered = """{"terms":["promotion","price"]}"""

              Expect.notEqual
                  (Minting.mint ordered)
                  (Minting.mint reordered)
                  "permuting an array is a different specification, not a different rendering"

              Expect.notEqual (seamMint ordered) (seamMint reordered) "and the seam agrees"
          }

          test "minting is deterministic — the same rendering, twice" {
              let rng = System.Random 20260807

              for _ in 1..100 do
                  let rendered = Generator.render (Generator.document rng)
                  Expect.equal (Minting.mint rendered) (Minting.mint rendered) "the mirror"
                  Expect.equal (seamMint rendered) (seamMint rendered) "the seam"
          } ]

[<Tests>]
let goRedTests =
    testList
        "Minting / the gate goes red"
        [ test "a payload edited by one character no longer reproduces the pinned digest" {
              // The probe that proves the gate above is asking a question an
              // implementation can fail.
              let v = specHashVectors () |> List.find (fun v -> v.Id = "spec-hash/canonical-form")

              let submission = payloadOf v
              let mutated = submission.SpecPayload.Replace("\"scale\":2.5", "\"scale\":2.6")
              Expect.notEqual mutated submission.SpecPayload "the probe must actually change the payload"

              match Minting.mint mutated, Minting.mint submission.SpecPayload with
              | Ok a, Ok b -> Expect.notEqual a b "a changed specification is a changed identity"
              | x, y -> failtestf "expected two hashes, got %A / %A" x y

              match Minting.canonicalise mutated with
              | Ok canonical ->
                  Expect.notEqual
                      canonical
                      (Option.defaultValue "" v.CanonicalPayload)
                      "and the intermediate moves with it"
              | Error refusal -> failtestf "unexpected refusal: %s" (Minting.describeRefusal refusal)
          }

          test "a hash minted over the raw bytes is exactly what the reject vector pins" {
              // Re-derived rather than transcribed: the gate must fail if the corpus's
              // reject vector stops being the error it says it is.
              let v =
                  specHashVectors ()
                  |> List.find (fun v -> v.Id = "spec-hash/reject-raw-bytes-minted")

              let submission = payloadOf v

              Expect.equal
                  submission.SpecHash
                  (Digest.sha256 submission.SpecPayload)
                  "the vector's hash is the digest of the payload AS RENDERED — the error being pinned"
          }

          test "an ill-formed rendering is refused by both tiers, distinguishably" {
              Expect.isError (Minting.mint "not a document") "the mirror refuses"

              Expect.stringStarts
                  (seamMint "not a document")
                  "raw:"
                  "and the seam's total form never lets an unmintable rendering masquerade as a minted one"
          } ]

// ============================================================================
//  The canonical string rule, pinned at the character (Phase 1).
//
//  `Canonical.escape` feeds every composite-key hash and every render; the string
//  reader inside `Canonical.parse` decodes every submitted document. Both copy
//  clean runs whole, so the cases below sit on the run boundaries: an escape first,
//  last, between two runs, and none at all. The refusal table was pinned on the
//  character-at-a-time reader before the run-copying one replaced it, and is the
//  contract for message, offset and order.
// ============================================================================

/// Characters by code point: an F# literal's lone surrogate escape compiles to U+FFFD.
let private ch (code: int) = string (char code)
let private hi = ch 0xd800
let private lo = ch 0xdc00
let private pair = ch 0xd83d + ch 0xde00

/// Every control character, its escape under §3.1 rule 6.
let private controlEscapes =
    [ for c in 0..0x1f ->
          let expected =
              match c with
              | 0x08 -> "\\b"
              | 0x0c -> "\\f"
              | 0x0a -> "\\n"
              | 0x0d -> "\\r"
              | 0x09 -> "\\t"
              | _ -> "\\u" + (c.ToString "x4")

          ch c, expected ]

/// Inputs and their escaped forms, including the run boundaries.
let private escapeCases =
    [ "", ""
      "abc", "abc"
      "\"", "\\\""
      "\\", "\\\\"
      "\"abc", "\\\"abc"
      "abc\"", "abc\\\""
      "\"abc\"", "\\\"abc\\\""
      "ab\"cd\\ef" + ch 1 + "gh", "ab\\\"cd\\\\ef\\u0001gh"
      "a\n\nb", "a\\n\\nb"
      "\n\t\r", "\\n\\t\\r"
      " ~" + ch 0x7f, " ~" + ch 0x7f
      "caf" + ch 0xe9 + " " + ch 0x4e2d, "caf" + ch 0xe9 + " " + ch 0x4e2d
      pair, pair
      "x" + hi + "y", "x" + hi + "y"
      lo, lo
      hi + hi, hi + hi
      "a" + ch 0x1f + "b", "a\\u001fb" ]
    @ controlEscapes

/// Documents the reader ACCEPTS today, and the string each decodes to.
let private acceptedStrings =
    [ "\"\"", ""
      "\"abc\"", "abc"
      "\"\\nabc\"", "\nabc"
      "\"abc\\n\"", "abc\n"
      "\"a\\\"b\\\\c\\/d\"", "a\"b\\c/d"
      "\"\\b\\f\\n\\r\\t\"", "\b\f\n\r\t"
      "\"\\u00e9\\u00E9\"", ch 0xe9 + ch 0xe9
      "\"\\ud83d\\ude00\"", pair
      "\"x\\ud800y\"", "x" + hi + "y"
      "\"\\udc00\"", lo
      // A raw control character inside a string is ACCEPTED, not refused.
      "\"a" + ch 1 + "b\tc\nd\"", "a" + ch 1 + "b\tc\nd"
      "\"caf" + ch 0xe9 + "\"", "caf" + ch 0xe9 ]

/// The reader's refusals: input, and the exact message with its offset.
let private refusalTable =
    [ "\"abc", "unterminated string at offset 4"
      "\"", "unterminated string at offset 1"
      "[\"ab", "unterminated string at offset 4"
      "\"abc\\n", "unterminated string at offset 6"
      "\"ab\\", "unterminated escape at offset 4"
      "\"\\", "unterminated escape at offset 2"
      "\"\\u12", "truncated \\u escape at offset 3"
      "\"\\u", "truncated \\u escape at offset 3"
      "\"\\uZZ", "truncated \\u escape at offset 3"
      "\"\\uZZZZ\"", "bad hex digit at offset 3"
      "\"ab\\u12Z4\"", "bad hex digit at offset 5"
      "\"\\x\"", "bad escape '\\x' at offset 3"
      "\"abc\\q", "bad escape '\\q' at offset 6"
      "\"ab\\'cd\"", "bad escape '\\'' at offset 5"
      "{a:1}", "expected '\"' at offset 1"
      "{\"a\":\"b", "unterminated string at offset 7"
      "\"ab\"x", "trailing content after the document at offset 4" ]

[<Tests>]
let canonicalStringTests =
    testList
        "Canonical / the string rule"
        [ test "a string with nothing to escape is returned as it is, not copied" {
              let clean = "a clean run of text " + ch 0xe9 + pair
              Expect.isTrue (obj.ReferenceEquals(Canonical.escape clean, clean)) "the same instance"
          }

          for (k, (input, expected)) in List.indexed escapeCases do
              test (sprintf "escape %02d: %A" k input) {
                  Expect.equal (Canonical.escape input) expected "the escaped form"
              }

          for (k, (doc, expected)) in List.indexed acceptedStrings do
              test (sprintf "parse accepts %02d: %A" k doc) {
                  Expect.equal (Canonical.parse doc) (Ok(JsonString expected)) "the decoded string"
              }

          for (k, (doc, message)) in List.indexed refusalTable do
              test (sprintf "parse refuses %02d: %A" k doc) {
                  Expect.equal (Canonical.parse doc) (Error message) "the refusal, its message and its offset"
              }

          for (k, (input, _)) in List.indexed escapeCases do
              test (sprintf "escape then parse round-trips %02d: %A" k input) {
                  Expect.equal
                      (Canonical.parse ("\"" + Canonical.escape input + "\""))
                      (Ok(JsonString input))
                      "the reader inverts the writer"
              } ]

// ============================================================================
//  A repeatable timing harness for the two hot string paths. It measures only when
//  FUARAN_MODELLING_BENCH is set, because a timing is not a test verdict:
//
//    $env:FUARAN_MODELLING_BENCH = '1'
//    dotnet run -c Release --project tests/Fuaran.Modelling.Tests -- --filter "Canonical / timing"
//
//  The workload is the specification corpus itself: every composite key it holds,
//  hashed, and every vector document, parsed. The printed checksum confirms the work
//  was done, and is the same before and after a change.
// ============================================================================

let private timeIt (label: string) (work: unit -> int) =
    for _ in 1..200 do
        work () |> ignore

    let trial () =
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let mutable runs = 0

        while sw.ElapsedMilliseconds < 500L do
            work () |> ignore
            runs <- runs + 1

        sw.Elapsed.TotalMilliseconds * 1000.0 / float runs

    let best = [ for _ in 1..5 -> trial () ] |> List.min
    printfn "timing %s: %.2f us per pass (best of 5), checksum %d" label best (work ())

[<Tests>]
let canonicalTimingTests =
    testList
        "Canonical / timing"
        [ test "compositeKeyHash over the corpus keys and Canonical.parse over the corpus documents" {
              if System.Environment.GetEnvironmentVariable "FUARAN_MODELLING_BENCH" = "1" then
                  let documents =
                      (SpecCorpus.manifest ()).Vectors
                      |> List.choose (fun v ->
                          try
                              let text = SpecCorpus.load v

                              match Canonical.parse text with
                              | Ok _ -> Some text
                              | Error _ -> None
                          with _ ->
                              None)

                  let keys =
                      documents
                      |> List.choose (fun text ->
                          match Codec.read text with
                          | Ok(CompositeKeyDoc k) -> Some k
                          | Ok(FitOutcomeDoc o) -> Some o.CompositeKey
                          | _ -> None)

                  Expect.isNonEmpty documents "the corpus supplies documents"
                  Expect.isNonEmpty keys "the corpus supplies composite keys"
                  printfn "timing workload: %d documents, %d composite keys" documents.Length keys.Length

                  timeIt "compositeKeyHash" (fun () -> keys |> List.sumBy (fun k -> (Codec.compositeKeyHash k).Length))

                  timeIt "Canonical.parse" (fun () ->
                      documents
                      |> List.sumBy (fun d ->
                          match Canonical.parse d with
                          | Ok _ -> d.Length
                          | Error _ -> -1))
          } ]
