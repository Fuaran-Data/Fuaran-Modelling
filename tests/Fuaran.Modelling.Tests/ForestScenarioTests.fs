// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ForestScenarioTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.DialectSeam
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  Three siblings, one policy — the whole Rung-2 story as one scripted run.
//
//  Each unit explores INDEPENDENTLY: its own session, its own op-DAG, its own
//  branches and its own curation. What they share is the process — one policy
//  object, applied per sibling by the ordinary governed-submit path — and a set
//  of decision ids, because the house process mints the same ids everywhere.
//  Nothing about running them together changes any of their records; that is
//  what process-pooling means, and this is where it is checked rather than
//  asserted.
//
//  Then the cross-sibling question: the dialect's comparator finds a planted
//  inconsistency, and a declaration on the diverging branch downgrades the
//  finding without removing it.
// ============================================================================

let private policy = fullPolicy "curator"

/// Each sibling explores the same two decisions — the house form, and an alternative that
/// drops the shared term's transform. Same ids in every sibling, which is what makes a
/// decision addressable across the forest at all.
let private houseForm =
    """{"response":"y","terms":[{"name":"shared","transform":{"$type":"log"}},{"name":"local"}]}"""

let private alternativeForm =
    """{"response":"y","terms":[{"name":"shared"},{"name":"local"}]}"""

/// The same form, with the shared term transformed differently — the planted inconsistency.
let private divergentForm =
    """{"response":"y","terms":[{"name":"shared","transform":{"$type":"lag","periods":1}},{"name":"local"}]}"""

let private projectOrFail (source: string) : SpecPayload =
    match ReferenceDialect.import source with
    | Error f -> failwithf "the fixture does not import: %s" (ImportFailure.describe f)
    | Ok outcome ->
        match ReferenceDialect.project outcome.Spec with
        | Error findings -> failwithf "the fixture does not project: %A" (findings |> List.map (fun f -> f.Code))
        | Ok payload -> payload

let private sibling (id: SiblingId) : SiblingRef =
    { Sibling = id
      Label = "unit " + id
      ExplorationRef = "memory:" + id }

let private forest: Forest =
    { Id = "portfolio"
      Title = "three units under one curation policy"
      Siblings = [ sibling "unit-a"; sibling "unit-b"; sibling "unit-c" ]
      Policy = policy
      // This forest declares no correction thresholds, so it raises no question about a
      // curation whatever its evidence says. The Rung-3 scenario declares its own.
      Thresholds = None }

/// The house process, run against one sibling: seed the house form, fork the alternative,
/// keep the settled one and kill the other with a reason.
///
/// One function, three siblings — the process is the thing that is shared. `settled` is the
/// payload this unit settled on, which is where the units are allowed to differ.
let private runHouseProcess (settled: string) (alternative: string) : Session =
    let step op s =
        match Session.submitGoverned hashFn policy "curator" curator op s with
        | Ok(_, s') -> s'
        | Error e -> failwithf "op %s did not land: %A" (Op.kindName op) e

    let payloadOf (source: string) =
        let p = projectOrFail source

        { Dialect = p.Dialect
          Format = p.Format
          Body = p.Body
          SpecHash = p.SpecHash }

    Session.empty
    |> step (
        Seed
            { StateId = "house"
              Label = "the house form"
              Payload = payloadOf settled }
    )
    |> step (
        Fork
            { StateId = "untransformed"
              Label = "the shared term, untransformed"
              From = "house"
              Payload = payloadOf alternative }
    )
    |> step (curateOp "house" Keep "the house form held on this unit's own evidence")
    |> step (curateOp "untransformed" Kill "the shared term's shape is not a per-unit question")

/// The three sibling sessions. Two run the house form; the third settled somewhere else.
let private runForest () : Map<SiblingId, Session> =
    Map.ofList
        [ "unit-a", runHouseProcess houseForm alternativeForm
          "unit-b", runHouseProcess houseForm alternativeForm
          "unit-c", runHouseProcess divergentForm alternativeForm ]

let private recordsOf (sessions: Map<SiblingId, Session>) : Map<SiblingId, Exploration> =
    sessions |> Map.map (fun _ s -> s.Record)

let private comparators = Seam.invariants ReferenceDialect.dialect

[<Tests>]
let tests =
    testList
        "ForestScenario"
        [ test "one policy, three siblings, and each curation is that sibling's own" {
              let sessions = runForest ()
              let records = recordsOf sessions
              let rows = Forest.summarise forest records

              Expect.equal (List.length rows) 3 "three members"
              Expect.isTrue (rows |> List.forall (fun r -> r.Present)) "all supplied"

              for r in rows do
                  Expect.equal r.Kept [ "house" ] (r.Sibling + " kept its house form")
                  Expect.equal r.Killed [ "untransformed" ] (r.Sibling + " set aside the alternative")
                  Expect.equal r.States 2 (r.Sibling + " explored its own two states")

              // The shared thing is the PROCESS. The positions it arrived at are not shared:
              // unit-c settled somewhere else and its record says so, on its own.
              let hashOf sibling =
                  Query.tryState "house" (Map.find sibling records)
                  |> Option.get
                  |> fun s -> s.Payload.SpecHash

              Expect.equal (hashOf "unit-a") (hashOf "unit-b") "two units settled on the same form"
              Expect.notEqual (hashOf "unit-c") (hashOf "unit-a") "and the third did not"
          }

          test "running the siblings together changes none of them" {
              // Process-pooling's actual claim, checked the only way it can be: a sibling's
              // record folded on its own is byte-identical to the same sibling's record folded
              // beside two others. There is no cross-sibling op, so there is nothing that could
              // have made it otherwise — which is exactly why it is worth pinning.
              let together = runForest ()
              let alone = runHouseProcess divergentForm alternativeForm

              Expect.equal
                  (Session.toJsonl (Map.find "unit-c" together))
                  (Session.toJsonl alone)
                  "a sibling's history does not depend on the company it keeps"
          }

          test "the comparator finds the planted inconsistency, over the dialect's own trees" {
              let assessment =
                  Forest.assess KeptStates comparators forest (recordsOf (runForest ()))

              Expect.equal assessment.Projected 3 "one kept state per sibling was shown"
              Expect.equal assessment.Invariants [ ReferenceDialect.transformConsistencyId ] "one invariant ran"

              match assessment.Findings with
              | [ finding ] ->
                  Expect.equal
                      finding.Finding.InvariantId
                      ReferenceDialect.transformConsistencyId
                      "attributed to the invariant that ran"

                  Expect.equal
                      finding.Finding.Siblings
                      [ "unit-a"; "unit-b"; "unit-c" ]
                      "naming every sibling the disagreement involves"

                  Expect.stringContains finding.Finding.Detail "shared" "and the term it is about"
                  Expect.equal finding.Standing Unaccounted "nothing accounts for it yet"
              | other -> failtestf "expected exactly one finding, got %d" (List.length other)

              Expect.isFalse
                  (Forest.clear (Forest.assess KeptStates comparators forest (recordsOf (runForest ()))))
                  "so the pass is not clear"
          }

          test "the manager never reads a spec to get there" {
              // The comparator imported the payloads into the DIALECT's trees, on the dialect's
              // own side of the seam. Everything the manager handled was an opaque body and
              // three strings — which is what makes a cross-sibling invariant about model
              // structure possible without the manager acquiring an opinion about models.
              let projected = Forest.project KeptStates forest (recordsOf (runForest ()))

              for p in projected do
                  Expect.equal p.Payload.Dialect ReferenceDialect.dialectId "payloads cross as payloads"
                  Expect.isNotEmpty p.Payload.SpecHash "carrying the hash their submitter minted"
          }

          test "a declaration on the diverging branch downgrades the finding, and prices it" {
              let sessions = runForest ()

              // The declaration is an ordinary governed op on unit-c's own session — the same
              // path every other op takes, through the same policy, into the same history.
              let declared =
                  match
                      Session.submitDeclaration
                          hashFn
                          policy
                          "curator"
                          curator
                          comparators
                          "house"
                          { InvariantId = ReferenceDialect.transformConsistencyId
                            Justification =
                              "this unit's series is short and its own out-of-time record favours the shorter carry"
                            DeclaredBy = "curator"
                            DeclaredAt = "2026-02-01T00:00:00Z" }
                          (Map.find "unit-c" sessions)
                  with
                  | Ok(_, s) -> s
                  | Error e -> failwithf "the declaration did not land: %A" e

              let records = recordsOf (Map.add "unit-c" declared sessions)
              let assessment = Forest.assess KeptStates comparators forest records

              Expect.equal (List.length assessment.Findings) 1 "the finding is STILL THERE"
              let finding = List.head assessment.Findings
              Expect.equal finding.Standing Downgraded "downgraded, not deleted"

              Expect.equal
                  (finding.Declarations |> List.map (fun (s, st, _) -> s, st))
                  [ "unit-c", "house" ]
                  "by the sibling that departed, on the branch that departed"

              Expect.stringContains
                  (finding.Declarations |> List.head |> (fun (_, _, d) -> d.Justification))
                  "out-of-time record"
                  "with the argument carried, verbatim"

              Expect.equal finding.Undeclared [ "unit-a"; "unit-b" ] "and the siblings that said nothing named"
              Expect.isTrue (Forest.clear assessment) "the price was paid"
              Expect.equal (List.length (Forest.downgraded assessment)) 1 "and the divergence stays on the books"

              // It is unmistakably a record, not a setting: it replays with unit-c's line and
              // survives the session's own persistence.
              match Session.toJsonl declared |> Session.ofJsonl hashFn with
              | Error m -> failtestf "reload failed: %s" m
              | Ok back ->
                  let st = Query.tryState "house" back.Record |> Option.get

                  Expect.isTrue
                      (Query.hasDeclared ReferenceDialect.transformConsistencyId st)
                      "the declaration survives a round-trip through the history"
          }

          test "a declaration naming an invariant no comparator declares is refused" {
              let sessions = runForest ()

              match
                  Session.submitDeclaration
                      hashFn
                      policy
                      "curator"
                      curator
                      comparators
                      "house"
                      { InvariantId = "REF-TRANSFORM-CONSISTANT"
                        Justification = "a plausible misspelling of a real invariant"
                        DeclaredBy = "curator"
                        DeclaredAt = "2026-02-01T00:00:00Z" }
                      (Map.find "unit-c" sessions)
              with
              | Error(DeclareRefused(UnknownInvariant(id, known))) ->
                  // The failure this gate exists for: a declaration against a misspelled id
                  // reads in the record exactly like one that works, and silently protects
                  // nothing.
                  Expect.equal id "REF-TRANSFORM-CONSISTANT" "names what was attempted"
                  Expect.contains known ReferenceDialect.transformConsistencyId "and what exists"
              | other -> failtestf "expected an unknown-invariant refusal, got %A" other
          }

          test "recurrence reads the forest in chosen-not-better vocabulary" {
              let records = recordsOf (runForest ())
              let rows = Forest.chosenRecurrence forest records

              let house = rows |> List.find (fun r -> r.State = "house")
              let alternative = rows |> List.find (fun r -> r.State = "untransformed")

              Expect.equal house.ChosenIn [ "unit-a"; "unit-b"; "unit-c" ] "the house form was chosen everywhere"
              Expect.isEmpty house.SetAsideIn "and set aside nowhere"

              Expect.equal
                  alternative.SetAsideIn
                  [ "unit-a"; "unit-b"; "unit-c" ]
                  "the alternative was set aside everywhere"

              Expect.isEmpty alternative.ChosenIn "and chosen nowhere"

              let line = Forest.describeRecurrence 3 house

              Expect.stringContains line "was chosen in 3 of 3 siblings" "the count is a count of choices"

              Expect.stringContains
                  line
                  "not what is better"
                  "and the sentence says so — three curators agreeing is three choices, not evidence"
          }

          test "the scenario is deterministic, sibling by sibling" {
              let a = runForest ()
              let b = runForest ()

              for id in [ "unit-a"; "unit-b"; "unit-c" ] do
                  Expect.equal
                      (Session.toJsonl (Map.find id b))
                      (Session.toJsonl (Map.find id a))
                      (id + "'s history is a pure function of its ops")
          } ]
