// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.MintingGenerator

open Fuaran.Modelling.Execution

// ============================================================================
//  A tiny JSON generator, and the cross-host vector emitter built on it.
//
//  The corpus's `spec-hash` family pins a handful of shapes. It cannot say that
//  two IMPLEMENTATIONS of `canonical-json-sha256-v1` agree on a document nobody
//  wrote — which is the case that matters, because the rule's whole job is to be
//  applied to documents no repository has ever seen. Six fixtures can be
//  satisfied by an implementation that is wrong everywhere else.
//
//  So this module does two jobs from one generator:
//
//    1. the in-repo agreement suite (`MintingAgreementTests`) drives both of this
//       repository's minting entry points over hundreds of generated documents;
//    2. `emitCrossHostVectors` writes those same documents out, WITH this
//       implementation's canonical intermediate and digest for each, so a
//       sibling implementation in another language can be held to them. That file
//       is the cross-host gate: it makes a divergence a red test in the other
//       host rather than a discovery made months later by a join that missed.
//
//  The seed is fixed and the traversal is deterministic, so re-emitting an
//  unchanged implementation produces a byte-identical file — a regeneration that
//  shows a diff is a change in the RULE, which is exactly when a human should be
//  looking at one.
// ============================================================================

/// Deliberately reaches the corners the rule is written about: control characters with
/// short escapes, quotes and backslashes, non-ASCII and astral characters, negative
/// zero, exponents, and empty composites.
type Value =
    | VNull
    | VBool of bool
    | VInt of int
    | VReal of float
    | VStr of string
    | VArr of Value list
    | VObj of (string * Value) list

let private strings =
    [| "plain"
       "with \"quotes\""
       "back\\slash"
       "tab\there"
       "newline\nhere"
       "naïve — αβγ"
       "😀"
       ""
       "control" |]

let private keys =
    [| "alpha"; "Beta"; "gamma"; "𐀀"; "�"; "z"; "A"; "nested"; "_under" |]

let private reals = [| 0.0; -0.0; 1e-7; 1e3; 1.5; 0.1; -0.0025; 42.0; 1e21; -3.25 |]

let rec private value (rng: System.Random) (depth: int) : Value =
    match rng.Next(if depth >= 3 then 5 else 7) with
    | 0 -> VNull
    | 1 -> VBool(rng.Next 2 = 0)
    | 2 -> VInt(rng.Next(-1000, 1000))
    | 3 -> VReal reals[rng.Next reals.Length]
    | 4 -> VStr strings[rng.Next strings.Length]
    | 5 -> VArr [ for _ in 1 .. rng.Next 4 -> value rng (depth + 1) ]
    | _ ->
        // Distinct keys by construction: duplicates are outside the rule's domain,
        // and a generator that produced them would be testing the refusal path here
        // instead of the agreement one.
        let chosen =
            keys
            |> Array.toList
            |> List.sortBy (fun _ -> rng.Next())
            |> List.truncate (rng.Next 5)

        VObj [ for k in chosen -> k, value rng (depth + 1) ]

let document (rng: System.Random) : Value =
    VObj(
        let chosen =
            keys
            |> Array.toList
            |> List.sortBy (fun _ -> rng.Next())
            |> List.truncate (1 + rng.Next 4)

        [ for k in chosen -> k, value rng 1 ]
    )

/// Render, escaping only what JSON requires — the shape an author's own emitter would
/// produce, which is what the rule is applied to.
let rec private write (indent: string) (permute: bool) (v: Value) : string =
    match v with
    | VNull -> "null"
    | VBool b -> (if b then "true" else "false")
    | VInt i -> string i
    | VReal f ->
        // Any valid JSON rendering will do — the rule re-lays it, which is exactly the
        // property under test.
        if System.Double.IsNegative f && f = 0.0 then
            "-0"
        else
            f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
    | VStr s ->
        let sb = System.Text.StringBuilder()
        sb.Append '"' |> ignore

        for ch in s do
            match ch with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | c when c < ' ' -> sb.Append("\\u").Append((int c).ToString "x4") |> ignore
            | c -> sb.Append c |> ignore

        sb.Append('"').ToString()
    | VArr items ->
        "["
        + (items |> List.map (write indent permute) |> String.concat ("," + indent))
        + "]"
    | VObj fields ->
        let ordered = if permute then List.rev fields else fields

        "{"
        + indent
        + (ordered
           |> List.map (fun (k, child) -> write indent permute (VStr k) + ":" + indent + write indent permute child)
           |> String.concat ("," + indent))
        + indent
        + "}"

/// The author's rendering.
let render (v: Value) : string = write "" false v

/// The same document, authored the other way round and laid out with whitespace.
let renderPermuted (v: Value) : string = write " " true v

// ---- the cross-host vector file ---------------------------------------------

/// The seed this file is emitted under. Fixed, so the emission is reproducible; named,
/// so a reader can tell that it is a choice rather than a timestamp.
let seed = 20260806

/// How many generated documents the file carries.
let generatedCount = 250

/// Hand-picked renderings, ahead of the generated ones. The generator reaches the
/// corners it was written to reach, but only on average; these pin the specific cases
/// where two implementations most plausibly diverge, so a failure names the case.
let edgeCases: (string * string) list =
    [ "edge/empty-object", "{}"
      "edge/empty-nested", """{"a":{},"b":[]}"""
      "edge/negative-zero", """{"z":-0,"y":-0.0,"x":0}"""
      "edge/exponent-boundaries", """{"a":1e21,"b":1e-7,"c":1e-6,"d":1e20,"e":1.5e300}"""
      "edge/trailing-zeros", """{"a":1.50,"b":42.0,"c":0.1,"d":-2.5e-3}"""
      "edge/large-exact-integer", """{"a":9007199254740992,"b":-9007199254740992}"""
      "edge/short-escapes", "{\"a\":\"\\b\\f\\n\\r\\t\",\"b\":\"\\u0000\\u001f\"}"
      "edge/quote-and-backslash", """{"a":"a \"quoted\" term","b":"a\\b"}"""
      "edge/astral-key-ordering", "{\"\uFFFD\":1,\"\U00010000\":2,\"z\":3,\"A\":4}"
      "edge/astral-value", "{\"a\":\"\U0001F600\",\"b\":\"na\u00EFve \u2014 \u03B1\u03B2\u03B3\"}"
      "edge/array-order-is-data", """{"terms":["price","promotion","seasonality"]}"""
      "edge/array-order-reversed", """{"terms":["seasonality","promotion","price"]}"""
      "edge/nulls-and-booleans", """{"a":null,"b":true,"c":false,"d":[null,true,false]}"""
      "edge/deep-nesting", """{"a":{"b":{"c":{"d":{"e":[{"f":1},{"g":[[[2]]]}]}}}}}"""
      "edge/whitespace-and-order",
      "{\n  \"z\" : 1,\n  \"a\" : [ 1, 2 ,3 ],\n  \"m\" : { \"q\" : null , \"b\" : \"x\" }\n}"
      "edge/empty-string-key", """{"":"empty key","a":""}""" ]

/// Renderings the rule declines. Pinned alongside the agreeing ones because two
/// implementations that mint alike but refuse differently have not agreed about the
/// algorithm's DOMAIN — and the domain is where an implementation quietly starts
/// resolving what the rule says to refuse.
let refusalCases: (string * string) list =
    [ "refuse/duplicate-members", """{"a":1,"b":2,"a":3}"""
      "refuse/duplicate-members-nested", """{"outer":{"k":1,"k":2}}"""
      "refuse/integer-beyond-binary64", """{"a":9007199254740993}"""
      "refuse/integer-far-beyond-binary64", """{"a":123456789012345678901234567890}"""
      "refuse/lone-high-surrogate", "{\"a\":\"\\ud800\"}"
      "refuse/lone-low-surrogate", "{\"a\":\"\\udc00x\"}"
      "refuse/lone-surrogate-in-key", "{\"\\ud800\":1}"
      "refuse/not-json", "not a document"
      "refuse/trailing-content", """{"a":1} {"b":2}"""
      "refuse/truncated", """{"a":""" ]

/// The kebab-case name of a refusal. The two hosts' taxonomies have to line up for the
/// refusal half of the gate to mean anything, so the name is part of the wire between
/// them rather than a per-host detail.
let refusalName (r: Minting.MintRefusal) : string =
    match r with
    | Minting.NotJson _ -> "not-json"
    | Minting.DuplicateMembers _ -> "duplicate-members"
    | Minting.NumberNotRepresentable _ -> "number-not-representable"
    | Minting.IllFormedUnicode _ -> "ill-formed-unicode"

/// Every rendering the file carries, in emission order.
let renderings () : (string * string * string option) list =
    let rng = System.Random seed

    let generated =
        [ for i in 0 .. generatedCount - 1 ->
              let doc = document rng
              sprintf "generated/%04d" i, render doc, Some(renderPermuted doc) ]

    [ for id, rendered in edgeCases -> id, rendered, None ] @ generated

/// Emit the cross-host vector file to `path`.
///
/// One vector per line, deliberately: a 250-document file on a single line is a diff no
/// one can read, and the point of committing it is that a change in the rule is
/// reviewable.
let emitCrossHostVectors (path: string) : string =
    let vectorLine (id: string) (rendered: string) (permuted: string option) =
        match Minting.canonicalise rendered, Minting.mint rendered with
        | Ok canonical, Ok digest ->
            let fields =
                [ "id", DStr id
                  "rendered", DStr rendered
                  "renderedPermuted",
                  (match permuted with
                   | Some p -> DStr p
                   | None -> DNull)
                  "canonical", DStr canonical
                  "digest", DStr digest ]

            Canonical.render (DRec fields)
        | Error refusal, _
        | _, Error refusal ->
            failwith (
                "the emitter produced a rendering outside the rule's domain ("
                + id
                + "): "
                + Minting.describeRefusal refusal
            )

    let refusalLine (id: string) (rendered: string) =
        match Minting.mint rendered with
        | Ok _ -> failwith ("a refusal case was accepted by the rule (" + id + "): " + rendered)
        | Error refusal ->
            Canonical.render (
                DRec
                    [ "id", DStr id
                      "rendered", DStr rendered
                      "refusal", DStr(refusalName refusal) ]
            )

    let vectors =
        renderings ()
        |> List.map (fun (id, rendered, permuted) -> vectorLine id rendered permuted)

    let refusals =
        refusalCases |> List.map (fun (id, rendered) -> refusalLine id rendered)

    let header =
        [ "{"
          "  \"algorithm\": \"" + Minting.algorithmId + "\","
          "  \"note\": \"GENERATED — do not hand-edit. Each vector's `canonical` and `digest` are this "
          + "library's F# implementation of the algorithm named above, applied to `rendered`. A second "
          + "implementation reproduces them or is wrong. See the regeneration instructions in the "
          + "consuming repository's script beside this file.\","
          "  \"seed\": " + string seed + ","
          "  \"generatedCount\": " + string generatedCount + ","
          "  \"vectorCount\": " + string (List.length vectors) + ","
          "  \"refusalCount\": " + string (List.length refusals) + ","
          "  \"vectors\": [" ]

    let text =
        System.String.Join(
            "\n",
            header
            @ [ vectors |> List.map (fun line -> "    " + line) |> String.concat ",\n" ]
            @ [ "  ],"; "  \"refusals\": [" ]
            @ [ refusals |> List.map (fun line -> "    " + line) |> String.concat ",\n" ]
            @ [ "  ]"; "}"; "" ]
        )

    let full = System.IO.Path.GetFullPath path

    System.IO.Directory.CreateDirectory(System.IO.Path.GetDirectoryName full)
    |> ignore
    // LF, no BOM. The file crosses to a repository whose formatter and diff tooling both
    // assume LF; a CRLF emission would re-dirty on every checkout there.
    System.IO.File.WriteAllText(full, text.Replace("\r\n", "\n"), System.Text.UTF8Encoding false)
    full
