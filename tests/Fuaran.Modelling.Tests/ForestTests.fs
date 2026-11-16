// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

module Fuaran.Modelling.Tests.ForestTests

open Expecto
open Fuaran.Modelling
open Fuaran.Modelling.Tests.Sample

// ============================================================================
//  The forest, in pieces: the record and its queries, the projection, the
//  evaluation host, the declaration gate, and the downgrade.
//
//  `ForestScenarioTests` runs the same machinery as one story with a real
//  dialect behind it. These are the parts, checked where a part can be wrong on
//  its own.
// ============================================================================

let private sibling (id: SiblingId) : SiblingRef =
    { Sibling = id
      Label = "unit " + id
      ExplorationRef = "memory:" + id }

let private forestOf (ids: SiblingId list) : Forest =
    { Id = "f1"
      Title = "a forest"
      Siblings = ids |> List.map sibling
      Policy = fullPolicy "curator"
      Thresholds = None }

/// A sibling record: seed `base`, fork `alt`, curate each as told.
let private record (body: string) (baseVerdict: CurationVerdict option) (altVerdict: CurationVerdict option) =
    let ops =
        [ Seed
              { StateId = "base"
                Label = "the house form"
                Payload = payload body ("h-" + body) }
          Fork
              { StateId = "alt"
                From = "base"
                Label = "the alternative"
                Payload = payload (body + "-alt") ("h-" + body + "-alt") } ]
        @ (match baseVerdict with
           | Some v -> [ curateOp "base" v "because" ]
           | None -> [])
        @ (match altVerdict with
           | Some v -> [ curateOp "alt" v "because" ]
           | None -> [])

    foldOrFail ops

let private declarationOf (invariantId: string) : DivergenceDeclaration =
    { InvariantId = invariantId
      Justification = "this unit's own history warrants the difference"
      DeclaredBy = "curator"
      DeclaredAt = "2026-01-01T00:00:00Z" }

/// A comparator that always reports one finding naming every sibling it was shown.
let private alwaysFinds (id: string) : InvariantComparator =
    { Id = id
      Description = "reports one disagreement over whatever it is shown"
      Compare =
        fun projected ->
            [ { InvariantId = "the comparator's own guess"
                Siblings = projected |> List.map (fun p -> p.Sibling)
                Detail = "they differ" } ] }

[<Tests>]
let forestShapeTests =
    testList
        "Forest / the record"
        [ test "a forest names siblings and holds one policy; it holds no exploration" {
              let f = forestOf [ "a"; "b"; "c" ]

              Expect.equal (Forest.siblings f) [ "a"; "b"; "c" ] "membership, in declaration order"

              Expect.equal
                  (Forest.trySibling "b" f |> Option.map (fun r -> r.ExplorationRef))
                  (Some "memory:b")
                  "a member carries an opaque locator this library never resolves"

              Expect.isNonEmpty f.Policy.Objectives "and one shared policy"
          }

          test "a member whose record was not supplied is reported absent, not empty" {
              // Different facts. An absent record says nothing about the exploration, and
              // counting it as an exploration with no states would say something false.
              let f = forestOf [ "a"; "b" ]
              let rows = Forest.summarise f (Map.ofList [ "a", record "{}" (Some Keep) None ])

              let a = rows |> List.find (fun r -> r.Sibling = "a")
              let b = rows |> List.find (fun r -> r.Sibling = "b")

              Expect.isTrue a.Present "supplied"
              Expect.isFalse b.Present "not supplied"
              Expect.equal a.States 2 "and the supplied one is read"
              Expect.equal a.Kept [ "base" ] "with its own curation"
              Expect.equal a.Uncurated [ "alt" ] "silence is neither kept nor killed"
          }

          test "the forest wire round-trips" {
              let f = forestOf [ "a"; "b" ]

              match Wire.parseForest (Wire.renderForest f) with
              | Ok back -> Expect.equal back f "render then parse is the identity"
              | Error m -> failtestf "the forest does not round-trip: %s" m
          }

          test "there is no op that names two siblings" {
              // Stated as a check because it is the load-bearing structural claim of the whole
              // design: coherence lives at the process layer, so every act that changes a record
              // is an act within ONE sibling. An op carrying a sibling id would be the one
              // construct able to couple two explorations, and its absence should be something
              // a reader can verify rather than take on trust.
              let opFields =
                  FSharp.Reflection.FSharpType.GetUnionCases typeof<ExplorationOp>
                  |> Array.collect (fun c -> c.GetFields())
                  |> Array.collect (fun f -> FSharp.Reflection.FSharpType.GetRecordFields f.PropertyType)
                  |> Array.map (fun p -> p.Name.ToLowerInvariant())

              Expect.isFalse
                  (opFields |> Array.exists (fun n -> n.Contains "sibling" || n.Contains "forest"))
                  "no op field names a sibling or a forest"
          } ]

[<Tests>]
let projectionTests =
    testList
        "Forest / the projection"
        [ test "the kept selection shows a comparator only what curators settled on" {
              let f = forestOf [ "a"; "b" ]

              let records =
                  Map.ofList
                      [ "a", record "{\"x\":1}" (Some Keep) (Some Kill)
                        "b", record "{\"x\":2}" None (Some Keep) ]

              let projected = Forest.project KeptStates f records

              Expect.equal
                  (projected |> List.map (fun p -> p.Sibling, p.State))
                  [ "a", "base"; "b", "alt" ]
                  "one kept state each, in forest then exploration order"
          }

          test "the named selection is how one decision is addressed across siblings" {
              // Ids are author-minted and permanent, so a house process that mints one id in
              // every sibling makes one decision addressable across the whole forest. A named
              // id a sibling never explored is simply absent from that sibling's contribution.
              let f = forestOf [ "a"; "b"; "c" ]

              let records =
                  Map.ofList
                      [ "a", record "{\"x\":1}" None None
                        "b", record "{\"x\":2}" None None
                        "c", foldOrFail [ seedOp "base" ] ]

              let projected = Forest.project (NamedStates [ "alt" ]) f records

              Expect.equal
                  (projected |> List.map (fun p -> p.Sibling))
                  [ "a"; "b" ]
                  "c never explored it, so c contributes nothing"
          }

          test "the projection carries payloads and nothing interpreted" {
              let f = forestOf [ "a" ]
              let records = Map.ofList [ "a", record "{\"x\":1}" (Some Keep) None ]

              match Forest.project KeptStates f records with
              | [ p ] ->
                  Expect.equal p.Payload.Body "{\"x\":1}" "the opaque body, verbatim"
                  Expect.equal p.Payload.SpecHash "h-{\"x\":1}" "and the hash the submitter minted"
              | other -> failtestf "expected one projected state, got %d" (List.length other)
          } ]

[<Tests>]
let evaluationTests =
    testList
        "Forest / evaluation"
        [ test "the manager stamps the invariant id; a comparator cannot misattribute" {
              // A finding attributed to an invariant its comparator does not implement would be
              // unanswerable — a declaration naming that invariant would downgrade findings from
              // a comparator nobody ran.
              let projected =
                  [ { Sibling = "a"
                      State = "s"
                      Payload = payload "{}" "h" } ]

              match Forest.evaluate [ alwaysFinds "TRUE-ID" ] projected with
              | [ f ] -> Expect.equal f.InvariantId "TRUE-ID" "stamped from the comparator, not from the finding"
              | other -> failtestf "expected one finding, got %d" (List.length other)
          }

          test "findings are ordered and their sibling lists normalised" {
              let noisy: InvariantComparator =
                  { Id = "I"
                    Description = "d"
                    Compare =
                      fun _ ->
                          [ { InvariantId = "I"
                              Siblings = [ "b"; "a"; "b" ]
                              Detail = "z" }
                            { InvariantId = "I"
                              Siblings = [ "a" ]
                              Detail = "a" } ] }

              let findings = Forest.evaluate [ noisy ] []

              Expect.equal
                  (findings |> List.map (fun f -> f.Siblings, f.Detail))
                  [ [ "a" ], "a"; [ "a"; "b" ], "z" ]
                  "de-duplicated, sorted, and in a total order two hosts agree on"
          }

          test "an assessment distinguishes 'found nothing' from 'was asked nothing'" {
              let f = forestOf [ "a" ]
              let empty = Forest.assess KeptStates [] f Map.empty
              Expect.equal empty.Projected 0 "nothing was shown"
              Expect.isEmpty empty.Invariants "and nothing ran"

              let asked =
                  Forest.assess KeptStates [ alwaysFinds "I" ] f (Map.ofList [ "a", record "{}" (Some Keep) None ])

              Expect.equal asked.Projected 1 "one state was shown"
              Expect.equal asked.Invariants [ "I" ] "and one invariant ran"
          }

          test "an assessment wire round-trips, findings and standings included" {
              let f = forestOf [ "a"; "b" ]

              let records =
                  Map.ofList
                      [ "a", record "{\"x\":1}" (Some Keep) None
                        "b", record "{\"x\":2}" (Some Keep) None ]

              let a = Forest.assess KeptStates [ alwaysFinds "I" ] f records

              match Wire.parseAssessment (Wire.renderAssessment a) with
              | Ok back -> Expect.equal back a "render then parse is the identity"
              | Error m -> failtestf "the assessment does not round-trip: %s" m
          } ]

[<Tests>]
let declarationTests =
    testList
        "Forest / the divergence declaration"
        [ test "a declaration naming an unknown invariant is refused, enumerating the known ones" {
              let comparators = [ alwaysFinds "KNOWN-A"; alwaysFinds "KNOWN-B" ]

              match Forest.declareDivergence comparators "base" (declarationOf "TYPO") with
              | Error(UnknownInvariant(id, known)) ->
                  Expect.equal id "TYPO" "names what was attempted"
                  Expect.equal known [ "KNOWN-A"; "KNOWN-B" ] "and what exists instead"
              | other -> failtestf "expected an unknown-invariant refusal, got %A" other
          }

          test "a declaration with no argument is refused — an unpriced divergence is the failure" {
              let comparators = [ alwaysFinds "I" ]

              let empty =
                  { declarationOf "I" with
                      Justification = "   " }

              match Forest.declareDivergence comparators "base" empty with
              | Error(UnjustifiedDivergence "I") -> ()
              | other -> failtestf "expected an unjustified-divergence refusal, got %A" other
          }

          test "a well-formed declaration becomes an ordinary op on one sibling" {
              match Forest.declareDivergence [ alwaysFinds "I" ] "base" (declarationOf "I") with
              | Ok(Declare d) ->
                  Expect.equal d.StateId "base" "attached to the branch that departs"
                  Expect.equal (Op.kindName (Declare d)) "declare" "and it is an op like any other"
              | other -> failtestf "expected a declare op, got %A" other
          }

          test "the fold refuses a declaration on a state that is not there" {
              match
                  Reducer.apply
                      (Declare
                          { StateId = "ghost"
                            Declaration = declarationOf "I" })
                      (record "{}" None None)
              with
              | Error(UnknownState "ghost") -> ()
              | other -> failtestf "expected an unknown-state rejection, got %A" other
          }

          test "declaration is a log; the current position is derived" {
              let first = declarationOf "I"

              let second =
                  { first with
                      Justification = "restated, on better evidence"
                      DeclaredAt = "2026-02-01T00:00:00Z" }

              let e =
                  foldOrFail
                      [ seedOp "base"
                        Declare
                            { StateId = "base"
                              Declaration = first }
                        Declare
                            { StateId = "base"
                              Declaration = second } ]

              let st = Query.tryState "base" e |> Option.get

              Expect.equal (List.length st.Divergences) 2 "both acts survive"

              Expect.equal
                  (Query.tryDeclaration "I" st |> Option.map (fun d -> d.Justification))
                  (Some "restated, on better evidence")
                  "and the current one is the latest"
          }

          test "the governed path refuses a subject with no grant, before the vocabulary gate" {
              // Default-deny is the outer gate, so an unauthorised declaration leaves no trace —
              // and gets a policy refusal rather than a lecture about invariant ids.
              let s = sessionOrFail [ seedOp "base" ]

              match
                  Session.submitDeclaration
                      hashFn
                      (fullPolicy "curator")
                      "explorer"
                      explorer
                      [ alwaysFinds "I" ]
                      "base"
                      (declarationOf "I")
                      s
              with
              | Error(DeclarePolicyBlocked _) -> ()
              | other -> failtestf "expected a policy refusal, got %A" other
          }

          test "the governed path lands, and the declaration replays with its line" {
              let s = sessionOrFail [ seedOp "base"; forkOp "alt" "base" ]

              let s =
                  match
                      Session.submitDeclaration
                          hashFn
                          (fullPolicy "curator")
                          "curator"
                          curator
                          [ alwaysFinds "I" ]
                          "alt"
                          (declarationOf "I")
                          s
                  with
                  | Ok(_, s') -> s'
                  | Error e -> failwithf "the declaration did not land: %A" e

              match Session.replayState "alt" s with
              | Error m -> failtestf "replay failed: %s" m
              | Ok line ->
                  let st = Query.tryState "alt" line |> Option.get
                  Expect.isTrue (Query.hasDeclared "I" st) "the declaration is on the line that declared it"

              match Session.replayState "base" s with
              | Error m -> failtestf "replay failed: %s" m
              | Ok line ->
                  let st = Query.tryState "base" line |> Option.get
                  Expect.isFalse (Query.hasDeclared "I" st) "and on no other"
          } ]

[<Tests>]
let downgradeTests =
    testList
        "Forest / downgrade, never deletion"
        [ test "a declaration downgrades the finding and carries the argument with it" {
              let f = forestOf [ "a"; "b" ]
              let comparator = alwaysFinds "I"

              let plain =
                  Map.ofList
                      [ "a", record "{\"x\":1}" (Some Keep) None
                        "b", record "{\"x\":2}" (Some Keep) None ]

              let before = Forest.assess KeptStates [ comparator ] f plain

              Expect.equal (List.length before.Findings) 1 "one finding"
              Expect.equal before.Findings.Head.Standing Unaccounted "and nothing accounts for it"
              Expect.isFalse (Forest.clear before) "so the pass is not clear"

              let declaredIn =
                  match
                      Reducer.apply
                          (Declare
                              { StateId = "base"
                                Declaration = declarationOf "I" })
                          (record "{\"x\":2}" (Some Keep) None)
                  with
                  | Ok e -> e
                  | Error r -> failwith (Rejection.describe r)

              let after = Forest.assess KeptStates [ comparator ] f (Map.add "b" declaredIn plain)

              Expect.equal (List.length after.Findings) 1 "the finding SURVIVES — this is the whole point"
              Expect.equal after.Findings.Head.Standing Downgraded "downgraded"

              Expect.equal
                  (after.Findings.Head.Declarations |> List.map (fun (s, st, _) -> s, st))
                  [ "b", "base" ]
                  "naming who declared it and where"

              Expect.stringContains
                  (after.Findings.Head.Declarations
                   |> List.head
                   |> fun (_, _, d) -> d.Justification)
                  "warrants"
                  "with the argument attached"

              Expect.equal after.Findings.Head.Undeclared [ "a" ] "and who said nothing"
              Expect.isTrue (Forest.clear after) "the price was paid"
              Expect.equal (List.length (Forest.downgraded after)) 1 "and the divergence is still on the books"
          }

          test "a declaration on a different invariant downgrades nothing" {
              let f = forestOf [ "a"; "b" ]
              let comparator = alwaysFinds "I"

              let elsewhere =
                  match
                      Reducer.apply
                          (Declare
                              { StateId = "base"
                                Declaration = declarationOf "SOME-OTHER-INVARIANT" })
                          (record "{\"x\":2}" (Some Keep) None)
                  with
                  | Ok e -> e
                  | Error r -> failwith (Rejection.describe r)

              let a =
                  Forest.assess
                      KeptStates
                      [ comparator ]
                      f
                      (Map.ofList [ "a", record "{\"x\":1}" (Some Keep) None; "b", elsewhere ])

              Expect.equal a.Findings.Head.Standing Unaccounted "a declaration is about ONE invariant"
          } ]

[<Tests>]
let recurrenceTests =
    testList
        "Forest / chosen recurrence"
        [ test "recurrence counts what was chosen, per decision, across the forest" {
              let f = forestOf [ "a"; "b"; "c" ]

              let records =
                  Map.ofList
                      [ "a", record "{\"x\":1}" (Some Keep) (Some Kill)
                        "b", record "{\"x\":2}" (Some Keep) None
                        "c", foldOrFail [ seedOp "base"; curateOp "base" Kill "not here" ] ]

              let rows = Forest.chosenRecurrence f records
              let baseRow = rows |> List.find (fun r -> r.State = "base")
              let altRow = rows |> List.find (fun r -> r.State = "alt")

              Expect.equal baseRow.ChosenIn [ "a"; "b" ] "kept in two"
              Expect.equal baseRow.SetAsideIn [ "c" ] "set aside in one"
              Expect.equal altRow.SetAsideIn [ "a" ] "the alternative was killed where it was tried"
              Expect.equal altRow.UncuratedIn [ "b" ] "left open where it was not"
              Expect.equal altRow.AbsentIn [ "c" ] "and never explored where it was never explored"

              Expect.equal (List.head rows).State "base" "most-chosen first — a reading convenience, not a ranking"
          }

          test "the rendered line says what the number is, and what it is not" {
              let f = forestOf [ "a"; "b" ]

              let records =
                  Map.ofList
                      [ "a", record "{\"x\":1}" (Some Keep) None
                        "b", record "{\"x\":2}" (Some Keep) None ]

              let line =
                  Forest.chosenRecurrence f records |> List.head |> Forest.describeRecurrence 2

              Expect.stringContains line "was chosen in 2 of 2 siblings" "the count, phrased as a choice"
              Expect.stringContains line "not what is better" "with the honesty rule stated in the sentence"
          }

          test "the recurrence surface names acts, never merits" {
              // The naming rule, held executably. A discipline stated only in a comment is one a
              // later reader edits away in good faith while making a field name clearer.
              match Contract.recurrenceVocabularyLaw () with
              | { Passed = true } -> ()
              | l -> failtestf "the recurrence vocabulary law failed: %s" l.Detail
          }

          test "the law goes red on a merit-flavoured name" {
              // A go-red probe on the gate itself: the law is checked by reflection over the
              // record's own fields, so the probe checks the RULE the law applies rather than
              // renaming a shipped field to watch it fail.
              let banned = Contract.bannedRecurrenceVocabulary

              Expect.isTrue
                  (banned |> List.exists ("bestForSibling".ToLowerInvariant().Contains))
                  "a field called BestForSibling would be caught"

              Expect.isFalse (banned |> List.exists ("chosenin".Contains)) "and one called ChosenIn would not"
          } ]
