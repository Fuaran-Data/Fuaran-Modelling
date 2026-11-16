// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.WireTests

open Expecto
open Fuaran.Core
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

/// Round-trip one value through its canonical encoding and back.
let private roundTrip (encode: 'T -> JVal) (decode: JVal -> Result<'T, string>) (name: string) (value: 'T) : unit =
    let text = Canonical.write Rendering.CrossHost (encode value)

    match Json.parse text |> Result.bind decode with
    | Ok back -> Expect.equal back value (name + " round-trips: " + text)
    | Error m -> failtestf "%s failed to decode (%s): %s" name text m

[<Tests>]
let tests =
    testList
        "Wire"
        [ test "the spec payload round-trips — body included, verbatim and unread" {
              roundTrip Wire.encodeSpecPayload Wire.decodeSpecPayload "specPayload" (payload "{\"a\":[1,2]}" "h1")
              // A body that is not JSON at all still survives: the manager stores text.
              roundTrip Wire.encodeSpecPayload Wire.decodeSpecPayload "opaque body" (payload "not json {{{" "h2")
          }

          test "gate requests and verdicts round-trip in both directions" {
              for direction in [ AtLeast; AtMost ] do
                  roundTrip
                      Wire.encodeGateRequest
                      Wire.decodeGateRequest
                      "gateRequest"
                      { Name = "convergence"
                        Threshold = 1.05
                        Unit = None
                        Direction = direction }

                  roundTrip
                      Wire.encodeGateVerdict
                      Wire.decodeGateVerdict
                      "gateVerdict"
                      { Name = "convergence"
                        Threshold = 1.05
                        Unit = None
                        Direction = direction
                        Observed = 1.01
                        Passed = false }
          }

          test "the composite key round-trips, all five components" {
              roundTrip Wire.encodeCompositeKey Wire.decodeCompositeKey "compositeKey" (compositeKey "h-abc")
          }

          test "an outcome snapshot round-trips, including a whole-valued diagnostic" {
              // JSON has one number type, so a whole-valued float comes back as an integer
              // token. The decoder must read numbers through the blessed accessor or this
              // silently loses the diagnostic.
              let o =
                  { outcome "h-a" with
                      Diagnostics = Map.ofList [ "iterations", 2000.0; "holdoutError", 0.125 ] }

              roundTrip Wire.encodeOutcome Wire.decodeOutcome "outcome" o
          }

          test "an outcome that states its units round-trips, and one that states none keeps its old bytes (D39)" {
              let kg =
                  match UnitText.tryParse "kg" with
                  | Ok u -> u
                  | Error e -> failtest e

              let o = outcome "h-a"

              let stated =
                  { o with
                      Diagnostics = Map.ofList [ "holdoutError", 0.125; "r2", 0.8 ]
                      DiagnosticUnits = Map.ofList [ "holdoutError", kg ]
                      Gates = [] }

              roundTrip Wire.encodeOutcome Wire.decodeOutcome "outcome with units" stated

              let text = Canonical.write Rendering.CrossHost (Wire.encodeOutcome stated)
              Expect.stringContains text "\"diagnosticUnits\":{\"holdoutError\":\"kg\"}" "the unit crosses as its text"

              Expect.isFalse
                  ((Canonical.write Rendering.CrossHost (Wire.encodeOutcome o)).Contains "diagnosticUnits")
                  "an outcome that states no units carries no units key at all"
          }

          test "a unit is refused at the codec unless it is the canonical text of an admitted unit" {
              let o =
                  { outcome "h-a" with
                      Diagnostics = Map.ofList [ "holdoutError", 0.125 ]
                      Gates = [] }

              let withUnits (units: string) =
                  let text = Canonical.write Rendering.CrossHost (Wire.encodeOutcome o)
                  text.Replace("\"diagnostics\":", "\"diagnosticUnits\":" + units + ",\"diagnostics\":")

              let decode (text: string) =
                  Json.parse text |> Result.bind Wire.decodeOutcome

              Expect.isOk (decode (withUnits "{\"holdoutError\":\"kg\"}")) "canonical text reads"
              Expect.isError (decode (withUnits "{\"holdoutError\":\"kg m\"}")) "text that does not parse is refused"

              Expect.isError
                  (decode (withUnits "{\"holdoutError\":\"s-1\"}"))
                  "a non-canonical spelling (`/s`) is refused"

              Expect.isError
                  (decode (withUnits "{\"aic\":\"1\"}"))
                  "a unit for a diagnostic the outcome lacks is refused"
          }

          test "a gate with a unit round-trips, and a mismatch is refused by name rather than converted (D39)" {
              let unitOf text =
                  match UnitText.tryParse text with
                  | Ok u -> u
                  | Error e -> failtest e

              let gate =
                  { Name = "holdoutError"
                    Threshold = 2.0
                    Unit = Some(unitOf "kg")
                    Direction = AtMost }

              roundTrip Wire.encodeGateRequest Wire.decodeGateRequest "gate with a unit" gate

              Expect.isOk (GateVerdict.evaluateIn (Some(unitOf "kg")) gate 1.5) "same unit: evaluated"

              match GateVerdict.evaluateIn (Some(unitOf "g")) gate 1500.0 with
              | Error m ->
                  Expect.equal m.Gate "holdoutError" "the refusal names the gate"
                  Expect.equal m.Stated (Some(unitOf "kg")) "and the unit it stated"
                  Expect.equal m.Reported (Some(unitOf "g")) "and the unit reported"

                  roundTrip
                      Wire.encodeBackendRefusal
                      Wire.decodeBackendRefusal
                      "gate unit mismatch"
                      (GateUnitMismatch m)
              | Ok v -> failtestf "a threshold in kilograms was compared with a value in grams: %A" v

              Expect.isError
                  (GateVerdict.evaluateIn None gate 1.5)
                  "a gate stating a unit against a diagnostic reported with none is refused too"
          }

          test "an outcome with no gates and no diagnostics round-trips" {
              let o =
                  { outcome "h-a" with
                      Diagnostics = Map.empty
                      Gates = [] }

              roundTrip Wire.encodeOutcome Wire.decodeOutcome "empty outcome" o
          }

          test "every submitter class and curation verdict round-trips" {
              for s in SubmitterClass.all do
                  roundTrip Wire.encodeOutcome Wire.decodeOutcome "submitterClass" { outcome "h" with SubmittedBy = s }

              for v in [ Keep; Kill; Note ] do
                  roundTrip Wire.encodeCuration Wire.decodeCuration "curation" (curation v "because")
          }

          test "proposals and their outcomes round-trip" {
              let p =
                  { ProposalId = "p1"
                    From = "a"
                    ProposedId = "b"
                    Label = "suggested"
                    Payload = payload "{\"v\":1}" "h-b"
                    Rationale = "worth trying"
                    ProposedBy = "explorer" }

              roundTrip Wire.encodeProposal Wire.decodeProposal "proposal" p
              roundTrip Wire.encodeProposalOutcome Wire.decodeProposalOutcome "accepted" Accepted

              roundTrip Wire.encodeProposalOutcome Wire.decodeProposalOutcome "declined" (Declined "not identifiable")
          }

          test "every state origin round-trips" {
              for o in [ Seeded; ForkedFrom "a"; CherryPickedFrom("b", "c"); FromProposal("p1", "a") ] do
                  roundTrip Wire.encodeOrigin Wire.decodeOrigin "origin" o
          }

          test "every op kind round-trips" {
              for _, op in Contract.samples do
                  match Wire.parseOp (Wire.renderOp op) with
                  | Ok back -> Expect.equal back op (Op.kindName op + " round-trips")
                  | Error m -> failtestf "%s failed: %s" (Op.kindName op) m
          }

          test "a folded exploration round-trips, states in exploration order" {
              let e =
                  foldOrFail
                      [ seedOp "a"
                        forkOp "b" "a"
                        forkOp "c" "a"
                        curateOp "c" Kill "abandoned"
                        Annotate
                            { StateId = "b"
                              Snapshot = outcome "h-b" } ]

              match Wire.parseExploration (Wire.renderExploration e) with
              | Ok back ->
                  Expect.equal back.Order e.Order "exploration order survives"
                  Expect.equal back.States e.States "states survive"
                  Expect.equal back.Root e.Root "root survives"
                  Expect.equal back.Head e.Head "head survives"
              | Error m -> failtestf "exploration failed to decode: %s" m
          }

          test "an exploration with proposals and resolutions round-trips whole" {
              let proposal =
                  { ProposalId = "p1"
                    From = "a"
                    ProposedId = "b"
                    Label = "suggested"
                    Payload = payload "{\"v\":1}" "h-b"
                    Rationale = "worth trying"
                    ProposedBy = "explorer" }

              let e =
                  foldOrFail
                      [ seedOp "a"
                        Propose { Proposal = proposal }
                        Resolve
                            { ProposalId = "p1"
                              Outcome = Declined "not identifiable"
                              ResolvedBy = "curator" } ]

              match Wire.parseExploration (Wire.renderExploration e) with
              | Ok back -> Expect.equal back e "the whole record survives"
              | Error m -> failtestf "failed to decode: %s" m
          }

          test "policy records round-trip" {
              roundTrip Wire.encodePolicy Wire.decodePolicy "policy" (fullPolicy "curator")

              let refusal =
                  { Subject = "explorer"
                    Attempted = "submitFit"
                    Reason = "default-deny"
                    Allowed = [ "emit:propose" ] }

              roundTrip Wire.encodePolicyRefusal Wire.decodePolicyRefusal "policyRefusal" refusal
          }

          test "the execution-seam records round-trip" {
              let submission =
                  { StateId = "a"
                    Payload = payload "{\"v\":0}" "h-a"
                    DatasetVersion = "ds-1"
                    Seed = "7"
                    Gates =
                      [ { Name = "convergence"
                          Threshold = 0.5
                          Unit = None
                          Direction = AtLeast } ]
                    SubmittedBy = SubmitAgent }

              roundTrip Wire.encodeSubmission Wire.decodeSubmission "submission" submission

              for r in
                  [ BudgetDenied(100.0, 99.5)
                    GateFailed
                        [ { Name = "convergence"
                            Threshold = 0.5
                            Unit = None
                            Direction = AtLeast
                            Observed = 0.1
                            Passed = false } ]
                    UnknownProvider [ "reference" ]
                    BackendPolicyRefused "no automated fits at month end" ] do
                  roundTrip Wire.encodeBackendRefusal Wire.decodeBackendRefusal "backendRefusal" r
          }

          test "every fold rejection round-trips" {
              for r in
                  [ NotSeeded
                    AlreadySeeded "a"
                    UnknownState "x"
                    DuplicateState "b"
                    KilledState "c"
                    UnknownProposal "p"
                    ProposalAlreadyTabled "p"
                    ProposalAlreadyResolved "p"
                    DuplicateOutcome("a", compositeKey "h")
                    HistoryCollision "n1" ] do
                  roundTrip Wire.encodeRejection Wire.decodeRejection "rejection" r
          }

          test "the indistinguishability records round-trip, every scope and every refusal" {
              for scope in [ EveryAlternative; NamedAlternatives [ "a"; "b" ] ] do
                  roundTrip Wire.encodeIndistinguishabilityScope Wire.decodeIndistinguishabilityScope "scope" scope

                  roundTrip
                      Wire.encodeIndistinguishabilityDeclaration
                      Wire.decodeIndistinguishabilityDeclaration
                      "declaration"
                      { Metric = "holdoutError"
                        Margin = 0.05
                        Scope = scope
                        Unit = None }

              for r in
                  [ UndeclaredMetric [ "holdoutError" ]
                    UnmeasuredMetric("aicc", [ "holdoutError" ])
                    NegativeMargin(-0.5)
                    UnknownScopeState("ghost", [ "a"; "b" ]) ] do
                  roundTrip
                      Wire.encodeIndistinguishabilityRefusal
                      Wire.decodeIndistinguishabilityRefusal
                      "indistinguishabilityRefusal"
                      r

              roundTrip
                  Wire.encodeIndistinguishabilityMember
                  Wire.decodeIndistinguishabilityMember
                  "member"
                  { State = "b"
                    Reading = 0.42
                    Distance = 0.02 }

              roundTrip
                  Wire.encodeIndistinguishabilityRecurrence
                  Wire.decodeIndistinguishabilityRecurrence
                  "recurrence"
                  { State = "b"
                    Label = "an alternative"
                    IndistinguishableIn = [ "unit-a" ]
                    SeparatedIn = [ "unit-b" ]
                    UnreadIn = []
                    OutOfScopeIn = []
                    AbsentIn = [ "unit-c" ] }
          }

          test "a set with no members beyond the reference round-trips, and so does the whole forest view" {
              // The empty-ish shapes are the ones an encoder is most likely to drop. A set
              // whose `unread` vanished would read as "every alternative was measured", which
              // is a claim about the evidence rather than the absence of one.
              let set: IndistinguishabilitySet =
                  { Reference = "a"
                    Declaration =
                      { Metric = "holdoutError"
                        Margin = 0.0
                        Scope = EveryAlternative
                        Unit = None }
                    DatasetVersion = "ds-1"
                    OutOfTime = false
                    ReferenceReading = 0.4
                    Members =
                      [ { State = "a"
                          Reading = 0.4
                          Distance = 0.0 } ]
                    Unread = [] }

              roundTrip Wire.encodeIndistinguishabilitySet Wire.decodeIndistinguishabilitySet "set" set

              Expect.stringContains
                  (Canonical.write Rendering.CrossHost (Wire.encodeIndistinguishabilitySet set))
                  "\"unread\":[]"
                  "an empty unread list is emitted, not omitted"

              let view: IndistinguishabilityForestView =
                  { Reference = "a"
                    Reports =
                      [ "unit-a", Derived set
                        "unit-b", NoReferenceReading("a", "holdoutError")
                        "unit-c", NoDeclaration ]
                    DerivedIn = [ "unit-a" ]
                    Recurrence = [] }

              roundTrip
                  Wire.encodeIndistinguishabilityForestView
                  Wire.decodeIndistinguishabilityForestView
                  "forestView"
                  view
          }

          test "canonical bytes sort object keys and tag the type first" {
              Expect.equal
                  (Wire.renderOp (ReturnTo { To = "s1" }))
                  "{\"$type\":\"return\",\"to\":\"s1\"}"
                  "pinned bytes"

              Expect.equal
                  (Canonical.write Rendering.CrossHost (Wire.encodeSpecPayload (payload "{}" "h")))
                  "{\"body\":\"{}\",\"dialect\":\"reference\",\"format\":\"json\",\"specHash\":\"h\"}"
                  "keys are Ordinal-sorted, not author-ordered"
          }

          test "the encoding is stable — encoding twice gives the same bytes" {
              for _, op in Contract.samples do
                  Expect.equal (Wire.renderOp op) (Wire.renderOp op) "deterministic encode"
          }

          test "a malformed op is a named error, never an exception" {
              for _, json in Contract.rejectFixtures do
                  match Wire.parseOp json with
                  | Ok v -> failtestf "expected a refusal for %s, decoded %A" json v
                  | Error m -> Expect.isNotEmpty m "the refusal says something"
          } ]
