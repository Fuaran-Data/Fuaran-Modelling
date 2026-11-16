// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The DRIFT-CONFORMANCE harness.
//
//  A mirror is a copy of something that was true when it was written. What makes
//  it a snapshot rather than a stale duplicate is that something fails when the
//  original moves — and the only honest place to put that something is a build
//  gate, because a note in a document is not a mechanism.
//
//  Two failures, and they answer different questions.
//
//    * The MANIFEST PIN. This snapshot records the digest of the corpus manifest
//      it was taken against. A new family, a new vector, a changed digest,
//      anything at all — the pin no longer matches and the gate goes red with a
//      message naming the remedy. This catches the drift that behaviour cannot:
//      a specification can move in ways this mirror still happens to satisfy,
//      and "still passes" is not the same statement as "still current".
//    * The VECTORS. Every fixture in the chosen profile is decoded, re-encoded
//      byte for byte, and — where the corpus says so — has its digest recomputed
//      from the document's own content, or is refused with the class the
//      manifest names. This catches the drift the pin cannot: a mirror that
//      re-pinned without re-reading.
//
//  And two things the corpus asks of a HARNESS rather than of an emitter, both
//  implemented here because a conformance suite is exactly the kind of code that
//  passes by doing nothing:
//
//    * assert that the number of vectors executed EQUALS the number the manifest
//      enumerates for the profile — not that they all passed, that the expected
//      count ran;
//    * be provable to go red. `Conformance.failedCodes` is what a go-red test
//      asserts on, and this library's own suite mutates a fixture to watch it.
//
//  This module performs NO IO. The host reads the files and hands them over, so
//  the same harness runs from a directory, an embedded resource, or a string a
//  test built — and so that nothing here has an opinion about where a corpus
//  lives.
// ============================================================================

/// Which families a participant must run (§2). Cumulative: an executor runs
/// everything a submitter runs, and a submitter everything an observer runs.
type Profile =
    /// Read an execution record: decode outcomes, recompute the identity that names
    /// them, and understand a refusal. Emits nothing.
    | Observer
    /// The above, plus authoring work: submit, score, and page the registry.
    | SubmitterProfile
    /// The above from the answering side — and the obligation unique to this
    /// profile: refuse every reject vector with the class the manifest names.
    | ExecutorProfile

[<RequireQualifiedAccess>]
module Profile =

    let encode (p: Profile) : string =
        match p with
        | Observer -> "observer"
        | SubmitterProfile -> "submitter"
        | ExecutorProfile -> "executor"

/// One corpus vector, as the manifest enumerates it.
type Vector =
    {
        Id: string
        Family: string
        /// The lowest profile that must run this vector. Reading it as always meaning
        /// "executor" is the mistake the specification warns about: the minting rule's
        /// reject vector is a SUBMITTER's pre-emit obligation, because the executor is
        /// forbidden to check it.
        Profile: string
        /// `round-trip` | `hash` | `reject` | `accept`.
        Kind: string
        File: string
        /// The digest of the fixture's own bytes.
        Sha256: string
        /// For a `hash` vector: the value an implementation must reproduce by
        /// recomputing it from the document's own content.
        Digest: string option
        /// For a `spec-hash` vector: the §4.5 intermediate, written out so an
        /// implementation that mints the wrong digest can see WHERE it diverged rather
        /// than only that it did.
        CanonicalPayload: string option
        /// For a `reject` vector: the class the refusal must carry.
        Reject: string option
        /// For an `accept` vector: how a conformant reader must interpret it.
        Interpretation: string option
    }

/// The corpus manifest — the authoritative enumeration. Counts live here, never in
/// prose: a number written in a document drifts from the directory beside it.
type Manifest =
    {
        Specification: string
        EnvelopeVersion: int
        Families: string list
        /// Families the specification intends to add. A name here is not missing, and a
        /// certifying implementation is not failing to run it.
        ReservedFamilies: string list
        Profiles: Map<string, string list>
        Vectors: Vector list
    }

/// One check: which vector it ran under, which obligation it checked, and whether
/// that obligation held.
type CheckResult =
    { Vector: string
      Code: string
      Passed: bool
      Detail: string }

/// The stable codes a conformance run reports, so a report is greppable and a
/// failure can be looked up without reading the harness.
module ExecutionCodes =

    /// The corpus manifest is not the one this snapshot was pinned against.
    let manifestDrift = "EXEC-MANIFEST-DRIFT"
    /// A fixture's bytes are not the bytes the manifest records for it.
    let fixtureDrift = "EXEC-FIXTURE-DRIFT"
    /// Fewer (or more) vectors ran than the manifest enumerates for the profile.
    let vectorCount = "EXEC-VECTOR-COUNT"
    /// The manifest names a family this snapshot does not implement.
    let familyUnknown = "EXEC-FAMILY-UNKNOWN"
    /// A document this reader should have accepted was refused.
    let readRefused = "EXEC-READ-REFUSED"
    /// Re-encoding a decoded document did not reproduce its bytes.
    let roundTripBytes = "EXEC-ROUND-TRIP-BYTES"
    /// A recomputed identity does not equal the one the manifest records.
    let digestDrift = "EXEC-DIGEST-DRIFT"
    /// The §4.5 intermediate does not equal the one the corpus pins.
    let canonicalPayloadDrift = "EXEC-CANONICAL-PAYLOAD-DRIFT"
    /// A document that must be refused was accepted.
    let rejectAccepted = "EXEC-REJECT-ACCEPTED"
    /// A document was refused with the wrong class.
    let rejectClassDrift = "EXEC-REJECT-CLASS-DRIFT"
    /// A forward-compatibility vector was refused — the implementation will break on
    /// the next additive change.
    let acceptRefused = "EXEC-ACCEPT-REFUSED"
    /// An accepted document was not interpreted as the manifest states.
    let interpretationDrift = "EXEC-INTERPRETATION-DRIFT"
    /// The reader caught a submitter's pre-emit obligation — which it is forbidden to
    /// check (§4.2 rule 2), because a party that verifies a hash it was handed has no
    /// conformant action available in the failing case.
    let preEmitCheckedByReader = "EXEC-PRE-EMIT-CHECKED-BY-READER"

/// The harness.
[<RequireQualifiedAccess>]
module Conformance =

    /// The digest of the corpus manifest this snapshot mirrors.
    ///
    /// **When this fails**, the specification moved. The remedy is never to update the
    /// constant on its own: re-read what changed, bring the mirror to it, re-run the
    /// vectors, and re-pin in the same commit. A pin advanced by itself is a snapshot
    /// that has stopped being one.
    let pinnedManifestDigest =
        "900d1a3c5f5ebd2c2512a02c412202452aac9ad924ff734ea98752a047fc7f7c"

    /// The families this snapshot implements. A manifest naming one that is not here
    /// is a specification that grew a shape the mirror does not model.
    let implementedFamilies =
        [ "envelope"
          "vintage-ref"
          "fit-submission"
          "fit-outcome"
          "registry-query"
          "score-request"
          "refusal"
          "spec-hash" ]

    // ---- reading the manifest ------------------------------------------------

    let private prop (name: string) (v: JsonValue) =
        match v with
        | JsonObject fields -> fields |> List.tryFind (fun (k, _) -> k = name) |> Option.map snd
        | _ -> None

    let private str (name: string) (v: JsonValue) =
        match prop name v with
        | Some(JsonString s) -> Some s
        | _ -> None

    let private strList (name: string) (v: JsonValue) =
        match prop name v with
        | Some(JsonArray items) ->
            items
            |> List.choose (function
                | JsonString s -> Some s
                | _ -> None)
        | _ -> []

    /// Parse the corpus manifest. Tolerant of members it does not know — adding one is
    /// additive, and a harness that refuses an unrecognised member is a harness that
    /// breaks on the next extension rather than reporting it.
    let parseManifest (json: string) : Result<Manifest, string> =
        match Canonical.parse json with
        | Error detail -> Error("the corpus manifest does not parse: " + detail)
        | Ok root ->
            let vectors =
                match prop "vectors" root with
                | Some(JsonArray items) ->
                    items
                    |> List.map (fun v ->
                        { Id = str "id" v |> Option.defaultValue ""
                          Family = str "family" v |> Option.defaultValue ""
                          Profile = str "profile" v |> Option.defaultValue ""
                          Kind = str "kind" v |> Option.defaultValue ""
                          File = str "file" v |> Option.defaultValue ""
                          Sha256 = str "sha256" v |> Option.defaultValue ""
                          Digest = str "digest" v
                          CanonicalPayload = str "canonicalPayload" v
                          Reject = str "reject" v
                          Interpretation = str "interpretation" v })
                | _ -> []

            let profiles =
                match prop "profiles" root with
                | Some(JsonObject fields) ->
                    fields
                    |> List.map (fun (k, v) ->
                        k,
                        (match v with
                         | JsonArray items ->
                             items
                             |> List.choose (function
                                 | JsonString s -> Some s
                                 | _ -> None)
                         | _ -> []))
                    |> Map.ofList
                | _ -> Map.empty

            if List.isEmpty vectors then
                Error "the corpus manifest enumerates no vectors"
            else
                Ok
                    { Specification = str "specification" root |> Option.defaultValue ""
                      EnvelopeVersion =
                        match prop "envelopeVersion" root with
                        | Some(JsonNumber(v, _)) -> int v
                        | _ -> 0
                      Families = strList "families" root
                      ReservedFamilies = strList "reservedFamilies" root
                      Profiles = profiles
                      Vectors = vectors }

    /// The vectors a profile must run: every vector in every family the manifest maps
    /// to that profile. Certifying a subset is not certifying.
    let vectorsFor (profile: Profile) (manifest: Manifest) : Vector list =
        let families =
            manifest.Profiles
            |> Map.tryFind (Profile.encode profile)
            |> Option.defaultValue manifest.Families
            |> Set.ofList

        manifest.Vectors |> List.filter (fun v -> families.Contains v.Family)

    // ---- the checks ----------------------------------------------------------

    let private pass vector code detail =
        { Vector = vector
          Code = code
          Passed = true
          Detail = detail }

    let private fail vector code detail =
        { Vector = vector
          Code = code
          Passed = false
          Detail = detail }

    /// Is the corpus the one this snapshot was pinned against?
    let checkManifestPin (manifestText: string) : CheckResult =
        let normalised = manifestText.Replace("\r\n", "\n")
        let actual = Digest.sha256Hex normalised

        if actual = pinnedManifestDigest then
            pass "manifest" ExecutionCodes.manifestDrift "the corpus manifest is the one this mirror was pinned against"
        else
            fail
                "manifest"
                ExecutionCodes.manifestDrift
                ("the specification moved: the manifest now digests to "
                 + actual
                 + ", this mirror is pinned to "
                 + pinnedManifestDigest
                 + ". Read what changed, bring the mirror to it, re-run the vectors, and re-pin in the same commit.")

    let private checkFixtureBytes (v: Vector) (text: string) : CheckResult =
        let actual = Digest.sha256Hex (text.Replace("\r\n", "\n"))

        if actual = v.Sha256 then
            pass v.Id ExecutionCodes.fixtureDrift "the fixture's bytes are the bytes the manifest records"
        else
            fail v.Id ExecutionCodes.fixtureDrift ("expected " + v.Sha256 + ", the file digests to " + actual)

    /// Decode and re-encode. The bytes MUST be identical — this is the whole of a
    /// round-trip vector, and it is the check that catches member ordering, absent
    /// optionals, integer formatting and real-number layout all at once.
    let private checkRoundTrip (v: Vector) (text: string) : CheckResult list =
        match Codec.read text with
        | Error refusal ->
            [ fail
                  v.Id
                  ExecutionCodes.readRefused
                  ("the document was refused as '" + refusal.RejectClass + "': " + refusal.Detail) ]
        | Ok document ->
            let reEncoded = Codec.render document

            if reEncoded = text then
                [ pass v.Id ExecutionCodes.roundTripBytes "re-encoding reproduced the document byte for byte" ]
            else
                [ fail v.Id ExecutionCodes.roundTripBytes ("expected\n  " + text + "\ngot\n  " + reEncoded) ]

    /// A `hash` vector asks a DIFFERENT recomputation depending on its family, and
    /// conflating the two is the most consequential implementation error available:
    /// a `fit-outcome` vector's digest is the composite-key address recomputed from the
    /// document's own key, while a `spec-hash` vector's is the minted `specHash`
    /// recomputed from the document's own payload.
    let private checkDigest (v: Vector) (document: Document) : CheckResult list =
        match v.Digest with
        | None -> []
        | Some expected ->
            let recomputed =
                match v.Family, document with
                | "fit-outcome", CompositeKeyDoc key -> Ok(Codec.compositeKeyHash key)
                | "fit-outcome", FitOutcomeDoc outcome -> Ok(Codec.compositeKeyHash outcome.CompositeKey)
                | "spec-hash", FitSubmissionDoc submission ->
                    Minting.mint submission.SpecPayload |> Result.mapError Minting.describeRefusal
                | family, _ -> Error("no recomputation is defined for a hash vector in family '" + family + "'")

            let digestCheck =
                match recomputed with
                | Error detail -> fail v.Id ExecutionCodes.digestDrift detail
                | Ok actual when actual = expected ->
                    pass v.Id ExecutionCodes.digestDrift "the recomputed identity equals the one the corpus records"
                | Ok actual -> fail v.Id ExecutionCodes.digestDrift ("expected " + expected + ", recomputed " + actual)

            let payloadCheck =
                match v.CanonicalPayload, document with
                | Some expectedBytes, FitSubmissionDoc submission ->
                    match Minting.canonicalise submission.SpecPayload with
                    | Error refusal ->
                        [ fail v.Id ExecutionCodes.canonicalPayloadDrift (Minting.describeRefusal refusal) ]
                    | Ok actual when actual = expectedBytes ->
                        [ pass
                              v.Id
                              ExecutionCodes.canonicalPayloadDrift
                              "the §4.5 intermediate equals the one the corpus pins" ]
                    | Ok actual ->
                        [ fail
                              v.Id
                              ExecutionCodes.canonicalPayloadDrift
                              ("expected\n  " + expectedBytes + "\ngot\n  " + actual) ]
                | _ -> []

            digestCheck :: payloadCheck

    let private checkHash (v: Vector) (text: string) : CheckResult list =
        match Codec.read text with
        | Error refusal ->
            [ fail
                  v.Id
                  ExecutionCodes.readRefused
                  ("the document was refused as '" + refusal.RejectClass + "': " + refusal.Detail) ]
        | Ok document -> checkRoundTrip v text @ checkDigest v document

    /// A reject vector, routed by WHOSE obligation it is.
    ///
    /// The minting rule is the one class an executor is forbidden to check, so its
    /// vector is checked twice over: the reader must ACCEPT the document — anything
    /// else is a reader that has taken on a judgement it was not delegated — and the
    /// submitter's pre-emit check must refuse it.
    let private checkReject (v: Vector) (text: string) : CheckResult list =
        let expected = v.Reject |> Option.defaultValue ""

        if expected = RejectClass.submissionSpecHashNonCanonical then
            match Codec.read text with
            | Error refusal ->
                [ fail
                      v.Id
                      ExecutionCodes.preEmitCheckedByReader
                      ("the reader refused a pre-emit obligation as '"
                       + refusal.RejectClass
                       + "'; §4.2 rule 2 forbids it from checking this at all") ]
            | Ok(FitSubmissionDoc submission) ->
                let accepted =
                    pass
                        v.Id
                        ExecutionCodes.preEmitCheckedByReader
                        "the reader stored the hash it was handed, as §4.2 rule 2 requires"

                match Submitter.checkMinting submission with
                | Ok() ->
                    [ accepted
                      fail
                          v.Id
                          ExecutionCodes.rejectAccepted
                          "the submitter's pre-emit check did not catch a hash minted over the wrong bytes" ]
                | Error refusal when refusal.RejectClass = expected ->
                    [ accepted
                      pass v.Id ExecutionCodes.rejectClassDrift ("refused pre-emit: " + refusal.Detail) ]
                | Error refusal ->
                    [ accepted
                      fail
                          v.Id
                          ExecutionCodes.rejectClassDrift
                          ("expected '" + expected + "', got '" + refusal.RejectClass + "'") ]
            | Ok other ->
                [ fail
                      v.Id
                      ExecutionCodes.readRefused
                      ("a spec-hash vector must be a fit submission, this read as "
                       + Document.kindOf other) ]
        else
            match Codec.read text with
            | Ok document ->
                [ fail
                      v.Id
                      ExecutionCodes.rejectAccepted
                      ("the document was accepted as "
                       + Document.kindOf document
                       + "; it must be refused") ]
            | Error refusal when refusal.RejectClass = expected ->
                [ pass v.Id ExecutionCodes.rejectClassDrift ("refused as '" + expected + "': " + refusal.Detail) ]
            | Error refusal ->
                [ fail
                      v.Id
                      ExecutionCodes.rejectClassDrift
                      ("expected '"
                       + expected
                       + "', got '"
                       + refusal.RejectClass
                       + "': "
                       + refusal.Detail) ]

    /// A forward-compatibility vector: a document a conformant reader MUST accept, and
    /// must interpret as the manifest states. An implementation that refuses one is an
    /// implementation that will break on the next additive change.
    let private checkAccept (v: Vector) (text: string) : CheckResult list =
        match Codec.read text with
        | Error refusal ->
            [ fail v.Id ExecutionCodes.acceptRefused ("refused as '" + refusal.RejectClass + "': " + refusal.Detail) ]
        | Ok document ->
            let accepted =
                pass v.Id ExecutionCodes.acceptRefused "accepted, as forward compatibility requires"

            match v.Interpretation, document with
            | Some expected, RefusalDoc refusal ->
                let actual = Refusal.interpretation refusal

                if actual = expected then
                    [ accepted
                      pass v.Id ExecutionCodes.interpretationDrift ("interpreted as '" + expected + "'") ]
                else
                    [ accepted
                      fail
                          v.Id
                          ExecutionCodes.interpretationDrift
                          ("expected '" + expected + "', interpreted as '" + actual + "'") ]
            | _ -> [ accepted ]

    /// Run one vector.
    let runVector (load: Vector -> string) (v: Vector) : CheckResult list =
        let text = load v
        let bytes = checkFixtureBytes v text

        let behaviour =
            match v.Kind with
            | "round-trip" -> checkRoundTrip v text
            | "hash" -> checkHash v text
            | "reject" -> checkReject v text
            | "accept" -> checkAccept v text
            | unknown -> [ fail v.Id ExecutionCodes.familyUnknown ("unknown vector kind '" + unknown + "'") ]

        bytes :: behaviour

    /// Run a whole profile against the corpus.
    ///
    /// `load` reads a vector's bytes; it is the host's, because this library performs
    /// no IO. `manifestText` is passed as well as the parsed manifest so the pin can be
    /// taken over the bytes rather than over a re-rendering of them — a re-rendering
    /// would agree with any manifest this harness can parse, which is the one thing the
    /// pin exists not to do.
    let run (profile: Profile) (manifestText: string) (manifest: Manifest) (load: Vector -> string) : CheckResult list =
        let selected = vectorsFor profile manifest

        let unknownFamilies =
            manifest.Families
            |> List.filter (fun f -> not (List.contains f implementedFamilies))

        let familyCheck =
            if List.isEmpty unknownFamilies then
                pass
                    "manifest"
                    ExecutionCodes.familyUnknown
                    ("every enumerated family is implemented: "
                     + String.concat ", " manifest.Families)
            else
                fail
                    "manifest"
                    ExecutionCodes.familyUnknown
                    ("the specification enumerates families this mirror does not model: "
                     + String.concat ", " unknownFamilies)

        let results = selected |> List.collect (runVector load)

        // §9.3 — assert that the number of vectors EXECUTED equals the number the
        // manifest enumerates. Not that they all passed: that the expected count ran.
        // A green run that exercised nothing looks exactly like a green run that
        // exercised everything.
        let executed =
            results
            |> List.map (fun r -> r.Vector)
            |> List.filter (fun v -> v <> "manifest")
            |> List.distinct

        let countCheck =
            if List.length executed = List.length selected then
                pass
                    "manifest"
                    ExecutionCodes.vectorCount
                    (string (List.length selected)
                     + " vectors enumerated for the "
                     + Profile.encode profile
                     + " profile, "
                     + string (List.length executed)
                     + " executed")
            else
                fail
                    "manifest"
                    ExecutionCodes.vectorCount
                    ("the manifest enumerates "
                     + string (List.length selected)
                     + " vectors for the "
                     + Profile.encode profile
                     + " profile but "
                     + string (List.length executed)
                     + " ran")

        checkManifestPin manifestText :: familyCheck :: countCheck :: results

    /// The whole run as one verdict, every failure named by its code.
    let certify
        (profile: Profile)
        (manifestText: string)
        (manifest: Manifest)
        (load: Vector -> string)
        : Result<unit, string> =
        match run profile manifestText manifest load |> List.filter (fun r -> not r.Passed) with
        | [] -> Ok()
        | failures ->
            failures
            |> List.map (fun r -> r.Code + " @ " + r.Vector + ": " + r.Detail)
            |> String.concat "; "
            |> Error

    /// The codes a run reported as FAILING — what a go-red self-test asserts on. A
    /// harness that has never been seen to fail is a harness nobody has tested.
    let failedCodes (results: CheckResult list) : string list =
        results
        |> List.filter (fun r -> not r.Passed)
        |> List.map (fun r -> r.Code)
        |> List.distinct
        |> List.sort

    /// A one-line-per-check report, for a host that wants to print one.
    let report (results: CheckResult list) : string list =
        results
        |> List.map (fun r ->
            (if r.Passed then "pass  " else "FAIL  ")
            + r.Code
            + " @ "
            + r.Vector
            + ": "
            + r.Detail)
