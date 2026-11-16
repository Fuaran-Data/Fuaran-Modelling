// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.SpecCorpus

open Fuaran.Modelling.Execution

// ============================================================================
//  Locating the model-execution wire specification's conformance corpus.
//
//  The corpus is the SPECIFICATION's, not this repository's. `FUARAN_MODEL_EXECUTION_SPEC`
//  names a checkout of the specification; without it the corpus is reached by the
//  relative path below, the layout this repository is developed in. Either way the
//  specification is checked out, never copied here: a copy would be a second corpus,
//  and the mirror would be certified against whichever one nobody updated.
//
//  ABSENCE IS A FAILURE, LOUDLY. There is no skip here and there must never be
//  one. A drift check that quietly does nothing when it cannot find what it is
//  checking against is worse than absent, because the build stays green and
//  everybody reads that as "the mirror is current" — which is exactly the
//  statement it is no longer making.
// ============================================================================

/// The environment variable naming a checkout of the specification.
let specVariable = "FUARAN_MODEL_EXECUTION_SPEC"

/// The corpus, relative to the repository root, when the variable is not set.
let corpusPath = "../../Fuaran-UI/fuaran-model-execution-spec/wire-fixtures"

/// The repository root, found by walking up from the running binary.
let repoRoot () =
    let rec up (dir: System.IO.DirectoryInfo) =
        if isNull (box dir) then
            failwith "could not locate the repository root (no Fuaran.Modelling.slnx above the running binary)"
        elif System.IO.File.Exists(System.IO.Path.Combine(dir.FullName, "Fuaran.Modelling.slnx")) then
            dir.FullName
        else
            up dir.Parent

    up (System.IO.DirectoryInfo System.AppContext.BaseDirectory)

let corpusRoot () =
    match System.Environment.GetEnvironmentVariable specVariable with
    | null
    | "" -> System.IO.Path.GetFullPath(System.IO.Path.Combine(repoRoot (), corpusPath))
    | spec -> System.IO.Path.GetFullPath(System.IO.Path.Combine(spec, "wire-fixtures"))

let private missing (what: string) (path: string) : 'a =
    failwith (
        "the model-execution wire corpus is not present: "
        + what
        + " was expected at '"
        + path
        + "'. This suite certifies the execution mirror against the specification's own fixtures, so it "
        + "CANNOT pass without them. Check the specification out and name the checkout in "
        + specVariable
        + " (or place it at '"
        + corpusPath
        + "') — do not disable this suite, which would leave the mirror unpinned while the build stayed green."
    )

/// The manifest's bytes. The authoritative enumeration of the corpus.
let manifestText () =
    let path = System.IO.Path.Combine(corpusRoot (), "manifest.json")

    if not (System.IO.File.Exists path) then
        missing "manifest.json" path
    else
        System.IO.File.ReadAllText path

let manifest () =
    match Conformance.parseManifest (manifestText ()) with
    | Ok m -> m
    | Error detail -> failwith ("the corpus manifest does not parse: " + detail)

/// One vector's bytes, exactly as committed.
let load (v: Vector) : string =
    let path = System.IO.Path.Combine(corpusRoot (), v.File)

    if not (System.IO.File.Exists path) then
        missing ("the fixture '" + v.Id + "'") path
    else
        System.IO.File.ReadAllText path

/// Load a vector's bytes with one substitution applied — the go-red probe. A harness
/// that has only ever been seen to pass certifies nothing.
let loadMutated (find: string) (replaceWith: string) (v: Vector) : string = (load v).Replace(find, replaceWith)
