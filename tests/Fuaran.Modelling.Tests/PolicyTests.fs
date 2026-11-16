// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.PolicyTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

[<Tests>]
let tests =
    testList
        "Policy"
        [ test "the empty policy grants nothing to anyone, across the whole capability space" {
              let caps = (OpKind.all |> List.map MayEmit) @ [ MaySubmitFit ]

              for c in caps do
                  match Policy.authorise Policy.empty "anyone" c with
                  | Ok() -> failtestf "the empty policy granted %s" (Capability.encode c)
                  | Error _ -> ()
          }

          test "a grant is exact — the subject named, and no other" {
              let p = fullPolicy "curator"
              Expect.isOk (Policy.authorise p "curator" MaySubmitFit) "the named subject is granted"

              match Policy.authorise p "someone-else" MaySubmitFit with
              | Error r -> Expect.isEmpty r.Allowed "an unnamed subject has no capabilities at all"
              | Ok() -> failtest "an unnamed subject was granted a capability"
          }

          test "a refusal enumerates what the subject may do instead" {
              let p =
                  { Policy.empty with
                      Objectives =
                          [ Policy.objective "explore" "propose only" "explorer" Policy.proposeOnlyCapabilities ] }

              match Policy.authorise p "explorer" MaySubmitFit with
              | Ok() -> failtest "a propose-only subject was granted fit submission"
              | Error r ->
                  Expect.equal r.Attempted "submitFit" "the refusal names what was attempted"
                  Expect.equal r.Allowed [ "emit:propose"; "emit:return" ] "and enumerates the alternatives"
                  Expect.stringContains r.Reason "default-deny" "and says why"
          }

          test "submitting a fit is a separate capability from recording an outcome" {
              let p =
                  { Policy.empty with
                      Objectives = [ Policy.objective "record" "annotate only" "recorder" [ MayEmit AnnotateKind ] ] }

              Expect.isOk (Policy.authorise p "recorder" (MayEmit AnnotateKind)) "may record an outcome"
              Expect.isError (Policy.authorise p "recorder" MaySubmitFit) "may not spend compute to obtain one"
          }

          test "an inactive objective grants nothing" {
              let p = fullPolicy "curator"
              let retired = Policy.retire "everything" p

              Expect.isOk (Policy.authorise p "curator" MaySubmitFit) "granted while active"
              Expect.isError (Policy.authorise retired "curator" MaySubmitFit) "withdrawn once retired"
              Expect.isEmpty (Policy.allowed "curator" retired) "and the whole grant went with it"
          }

          test "grants from several active objectives union" {
              let p =
                  { Policy.empty with
                      Objectives =
                          [ Policy.objective "a" "may fork" "worker" [ MayEmit ForkKind ]
                            Policy.objective "b" "may curate" "worker" [ MayEmit CurateKind ] ] }

              Expect.equal (Policy.allowed "worker" p) [ "emit:curate"; "emit:fork" ] "both grants, sorted"
          }

          test "retiring an objective that was never made is not an error" {
              let p = Policy.retire "no-such-objective" (fullPolicy "curator")
              Expect.isOk (Policy.authorise p "curator" MaySubmitFit) "unrelated grants are untouched"
          }

          test "capability round-trips through its wire spelling" {
              let caps = (OpKind.all |> List.map MayEmit) @ [ MaySubmitFit ]

              for c in caps do
                  match Capability.tryParse (Capability.encode c) with
                  | Ok back -> Expect.equal back c "round-trip"
                  | Error m -> failtestf "%s did not parse back: %s" (Capability.encode c) m
          }

          test "an unknown capability spelling is refused, not guessed" {
              Expect.isError (Capability.tryParse "emit:teleport") "unknown op kind"
              Expect.isError (Capability.tryParse "doAnything") "unknown capability"
          } ]
