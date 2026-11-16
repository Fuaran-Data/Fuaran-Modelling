# The dialect conformance corpus

A dialect is judged by what it *does*, so its contract ships as fixtures rather than prose. This
directory holds one corpus document per dialect, in the format below, and
`Fuaran.Modelling.DialectSeam` ships the harness that runs one against a dialect.

The corpus is **data**. Nothing in this format is specific to .NET or to F#: a dialect written in
another language authors the same document and implements the same checks, and the two
implementations agree or one of them is wrong.

| | |
|---|---|
| Format | JSON, one document per dialect |
| Naming | `<dialect-id>.json` |
| Current | [`reference-linear.json`](reference-linear.json) — the reference dialect's corpus |
| Harness | `Conformance.parseCorpus` → `Conformance.run` → `Conformance.certify` |

## Why the fixtures pin observed values

Every fixture records what the dialect **produced**, not what someone intended it to produce. A
corpus of intentions passes against an implementation doing something else entirely, which is the
one failure a conformance suite exists to make impossible.

That splits authoring into two halves, and the split is deliberate:

- **Claims are written by hand** — which findings a document produces, what an importer must report
  as residue, what applying a pack must do. A claim generated from the implementation agrees with
  any implementation, including a wrong one.
- **Transcription is generated** — the exact canonical bytes and the hash over them. Copying forty
  characters of JSON by hand introduces errors that look exactly like drift.

The committed file is the pin either way. Re-emitting produces a **diff a human accepts**, never an
expectation that moved on its own.

## The document

```json
{
  "corpus": "dialect-conformance",
  "corpusVersion": 1,
  "seamVersion": "1.1",
  "dialect": "<dialect id>",
  "projector":  [ ... ],
  "importer":   [ ... ],
  "validator":  [ ... ],
  "gates":      [ ... ],
  "packs":      [ ... ],
  "invariants": [ ... ]
}
```

`seamVersion` is the seam contract the corpus was authored against; `corpusVersion` is the version of
*this document format*, which moves independently — a corpus can gain a field without the dialect
contract changing at all. A family a dialect has no fixtures for may be omitted; a family that is
present and empty proves nothing, and the harness's own tests say so.

### `projector` — the typed tree rendered to payload bytes

```json
{ "name": "minimal",
  "source": "<an authoring document>",
  "bytes":  "<the exact canonical payload the dialect produces from it>",
  "hash":   "<the spec hash it mints over those bytes>",
  "covers": [ "<vocabulary terms this fixture exercises>" ] }
```

Checked per fixture: the bytes and hash match; the hash is the dialect's own minting rule applied to
those bytes; the payload's dialect and format tags match the dialect's identity; three projections
of one spec are byte-identical; and importing the projection and re-projecting reproduces it with no
residue.

Checked across the family: distinct payloads mint **distinct** hashes — against what the dialect
actually produces rather than against the pins, because a dialect whose hash is a constant satisfies
every pin as soon as its corpus is regenerated.

`covers` feeds the **coverage gate**, which runs in both directions: every term the dialect publishes
must be exercised by some fixture, and no fixture may claim a term the dialect does not publish. A
term nobody exercises is an untested claim.

### `importer` — parse-back, and the residue report

```json
{ "name": "foreign-document",
  "source": "<a document, possibly written for something else>",
  "residue": [ { "path": "/terms/0", "term": "prior" } ],
  "roundTrip": true }
```

`residue` is everything the import did not bring across, compared as a set of `(path, term)` pairs —
so a dialect is not forced into an arbitrary emission order, while its own order must still be
stable, which is checked separately by importing three times.

The `detail` a dialect attaches to a residue entry is deliberately **not** pinned: it is for a human
and may be reworded freely. *What* was dropped and *where* are contract.

`roundTrip` asks additionally that importing the dialect's own projection yields the same tree.

### `validator` — findings, and whether the spec projects

```json
{ "name": "duplicate-term",
  "source": "<a document>",
  "findings": [ { "code": "REF-DUPLICATE-TERM", "severity": "blocking" } ],
  "projects": false }
```

Codes and severities are pinned; messages are not. Findings are compared as a sorted set, and a
separate check requires the dialect's own emission order to be stable.

`projects` pins the seam's most load-bearing law: **a projector refuses exactly when validation
blocks** — not less (a blocking-invalid spec must not project) and not more (a spec with only
advisory findings must). That equivalence is what lets everything downstream trust a stored payload
came from a spec its own dialect considered valid, without re-validating something it cannot read.

### `gates` — the default gate vocabulary

```json
{ "name": "termCount", "threshold": 1, "direction": "atLeast" }
```

Every gate a dialect offers is expressed on the generic `(name, threshold, direction)` triple, so a
host or an executing side can reason about gates from any dialect without knowing any of them.
Direction is `atLeast` or `atMost`; the vocabulary is closed, and an unrecognised direction is
refused rather than defaulted.

### `packs` — parameter bundles folded into a spec

```json
{ "name": "bounds-applied",
  "source": "<a document>",
  "pack": { "id": "bounds-1", "kind": "bounds", "version": "1", "body": "<opaque to the seam>" },
  "expect": { "$type": "applied", "bytes": "<the projection after applying>" } }
```

or

```json
{ "expect": { "$type": "refused", "refusal": "unknownKind" } }
```

`refusal` is one of `unknownKind`, `malformed`, `conflicts`. Applying a pack must be deterministic,
and — checked by a probe the **harness synthesises**, not by a fixture — a pack of an undeclared kind
must be refused. That one is synthesised on purpose: a dialect must not be able to satisfy it by
declining to write the fixture that would have caught it.

### `invariants` — cross-sibling comparisons (OPTIONAL, seam 1.1)

```json
{ "name": "one-sibling-differs",
  "siblings": [ { "sibling": "unit-a", "state": "s1", "source": "<a document>" },
                { "sibling": "unit-b", "state": "s1", "source": "<another document>" } ],
  "findings": [ { "invariantId": "MY-INVARIANT", "siblings": [ "unit-a", "unit-b" ] } ] }
```

Each fixture sibling's document is imported and **projected through the dialect's own projector**,
and the resulting payloads are handed to the dialect's comparators through the manager's evaluation
host — so what is certified is what the dialect actually produces and how it is actually called,
never hand-written bytes the dialect might never emit.

The invariant id and the siblings a finding names are pinned; the `detail` is not, on the same
reasoning that leaves a validator's message unpinned. The manager stamps each finding with the id of
the comparator that produced it, so a comparator cannot misattribute one. Comparators must be
deterministic in content and in order, which is checked by running them three times.

**Pin the clean case as well as the dirty one.** A comparator that never fires satisfies every
fixture expecting no finding, so a family of only clean cases certifies silence — and silence is
indistinguishable from a coherent forest.

This family is **optional in both directions**: omit it and a dialect declaring no invariants
conforms exactly as before, while a dialect that declares them and has no fixtures is simply
untested there. A dialect that declines the obligation altogether — it does not implement
`IInvariantComparators` — is reported with a *passing* check saying so, rather than by the family
silently vanishing from the report.

### The escape: divergence declarations

An invariant finding is not a veto. A `DivergenceDeclaration` — recorded as an ordinary op on the
branch that departs, carrying the invariant id, a justification, who declared it and when —
**downgrades** the finding: it stays in the assessment, with the argument attached, and is never
removed. Divergence is permitted but priced, and a mechanism that deleted the finding would set the
price at nothing.

A declaration naming an invariant no comparator declares is refused, enumerating the ids that exist.
A declaration against a misspelled id would record perfectly, read exactly like one that works, and
protect nothing.

## Failure codes

Every check reports a stable code, passing or failing, so a report is greppable and a dialect author
can look up what broke without reading the harness.

| Code | The obligation it names |
|---|---|
| `DIALECT-IDENTITY-INCOMPLETE` | every identity field is populated |
| `DIALECT-IDENTITY-MISMATCH` | payload tags, and the corpus, name this dialect |
| `DIALECT-SEAM-SKEW` | the dialect targets a seam this host implements |
| `DIALECT-VOCABULARY-EMPTY` / `-DUPLICATE` | the published vocabulary is non-empty and has no repeats |
| `DIALECT-VOCABULARY-UNCOVERED` / `-UNDECLARED` | the coverage gate, in both directions |
| `DIALECT-MINT-NONDETERMINISTIC` | minting is stable, and distinguishes distinct inputs |
| `DIALECT-IMPORT-FAILED` | a fixture source imports at all |
| `DIALECT-IMPORT-NONDETERMINISTIC` | repeated imports agree |
| `DIALECT-RESIDUE-DRIFT` | the residue report is what the fixture pins |
| `DIALECT-PROJECT-REFUSED` | a projector fixture's source projects |
| `DIALECT-PROJECT-BYTES-DRIFT` / `-HASH-DRIFT` | the payload is what the fixture pins |
| `DIALECT-PROJECTOR-NONDETERMINISTIC` | repeated projections are byte-identical |
| `DIALECT-HASH-RULE` | the hash is the minting rule applied to the payload bytes |
| `DIALECT-HASH-COLLISION` | distinct payloads mint distinct hashes |
| `DIALECT-PROJECT-NOT-IDEMPOTENT` | project ∘ import ∘ project is stable |
| `DIALECT-VALIDATOR-DRIFT` / `-NONDETERMINISTIC` | findings are pinned, and stable |
| `DIALECT-PROJECT-ACCEPTS-BLOCKING` | a blocking-invalid spec does not project |
| `DIALECT-PROJECT-REFUSES-VALID` | a valid one does |
| `DIALECT-GATE-MISSING` / `-DRIFT` / `-DUPLICATE` | the default gate vocabulary |
| `DIALECT-PACK-OUTCOME-DRIFT` / `-NONDETERMINISTIC` | applying a pack |
| `DIALECT-PACK-UNDECLARED-ACCEPTED` | an undeclared pack kind is refused |
| `DIALECT-INVARIANT-DUPLICATE` / `-UNDESCRIBED` | declared invariants have distinct ids and are described |
| `DIALECT-INVARIANT-UNDECLARED` | a fixture only names invariants the dialect declares |
| `DIALECT-INVARIANT-DRIFT` / `-NONDETERMINISTIC` | the findings are pinned, and stable |

## Running it

```fsharp
open Fuaran.Modelling.DialectSeam

let corpus =
    System.IO.File.ReadAllText "conformance/reference-linear.json"
    |> Conformance.parseCorpus

match corpus |> Result.bind (Conformance.certify myDialect) with
| Ok () -> printfn "conformant"
| Error report -> printfn "%s" report
```

`Conformance.run` returns every check individually; `Conformance.report` formats them one per line.

The harness performs **no IO** — the host reads the file. That keeps it usable from a test, a build
step, a resource, or a string, and it is the same reason the corpus is a file in the first place:
something a person can open, diff and review.

## Seeing it fail

A conformance harness that has only ever been seen to pass certifies nothing: every check in it
might be asking a question no implementation can fail.

So the reference dialect ships **mutations** of itself — a constant hash, a hash that is not over its
own payload, an importer that drops what it cannot model, a payload tagged as someone else's, a
projector that does not repeat, a dialect that applies undeclared packs, a projector that renders a
blocking-invalid spec, a dialect targeting a seam this build does not implement, and a comparator
that declares an invariant and then never finds anything. Each breaks one obligation and must be
caught by the code that names it:

```fsharp
Conformance.run (ReferenceDialect.mutant LossyImporter) corpus
|> Conformance.failedCodes      // [ "DIALECT-RESIDUE-DRIFT" ]
```

`ReferenceDialect.withInvariants` ships beside them so you can watch the invariant checks fail on
comparator defects that are easiest to write by hand — a duplicated id, an empty description, one
that answers differently each time.

Run them against your own harness before trusting it to pass yours.

## Re-emitting

The reference dialect's corpus is generated from the fixture module beside it:

```
dotnet run --project tests/Fuaran.Modelling.Tests -- --emit-corpus
```

A test compares the committed file against a fresh emission byte for byte, so regeneration is a
deliberate act whose result is reviewed as a diff.
