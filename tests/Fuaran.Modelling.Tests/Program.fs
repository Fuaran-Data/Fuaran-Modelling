// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.Program

open Expecto

[<EntryPoint>]
let main argv =
    match List.ofArray argv with
    // Re-emit the dialect conformance corpora — the reference dialect's and the Compute
    // dialect's. The committed files are the pins, and a test compares each against its
    // emission byte for byte — so regenerating is a deliberate act whose result is reviewed
    // as a diff rather than an expectation that moved by itself.
    | [ "--emit-corpus" ] ->
        for path in [ CorpusFixtures.emit (); ComputeCorpusFixtures.emit () ] do
            printfn "corpus written: %s" path

        0
    // Emit the cross-host vector file: the generated documents this repository's
    // agreement suite already runs, written out WITH this implementation's canonical
    // bytes and digests so a sibling implementation in another language can be held to
    // them. Deliberately an explicit path rather than a default — the consuming
    // repository decides where its fixtures live, and a default here would encode one
    // repository's layout into another's.
    | [ "--emit-cross-host-vectors"; path ] ->
        let written = MintingGenerator.emitCrossHostVectors path
        printfn "cross-host vectors written: %s" written
        0
    // Print the execution mirror's conformance run against the model-execution wire
    // corpus, one line per check. The suite asserts on this; a human reading a drift
    // failure wants to see WHICH obligations held, not only that one did not.
    | "--execution-report" :: rest ->
        let profile =
            match rest with
            | [ "observer" ] -> Fuaran.Modelling.Execution.Observer
            | [ "submitter" ] -> Fuaran.Modelling.Execution.SubmitterProfile
            | _ -> Fuaran.Modelling.Execution.ExecutorProfile

        let results =
            Fuaran.Modelling.Execution.Conformance.run
                profile
                (SpecCorpus.manifestText ())
                (SpecCorpus.manifest ())
                SpecCorpus.load

        for line in Fuaran.Modelling.Execution.Conformance.report results do
            printfn "%s" line

        let failed = results |> List.filter (fun r -> not r.Passed) |> List.length

        printfn
            "\n%s profile: %d checks, %d failed"
            (Fuaran.Modelling.Execution.Profile.encode profile)
            (List.length results)
            failed

        if failed = 0 then 0 else 1
    | _ -> runTestsInAssemblyWithCLIArgs [] argv
