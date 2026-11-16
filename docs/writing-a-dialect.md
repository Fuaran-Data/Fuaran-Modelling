# Writing a dialect

`Fuaran.Modelling` records decisions about model specs without ever reading one. Everything it
deliberately does not know — what a model is, which parameters are legal, what the payload crossing
the execution wire looks like — lives in a **dialect**, behind the dialect seam.

This page is what you need to write one. The seam is a public extension point: dialects are written
by people who did not write the library and cannot be asked to change it, so the contract is
executable rather than described, it is versioned, and version skew is a typed refusal instead of a
subtle misbehaviour.

The worked example is `ReferenceDialect` in
[`src/Fuaran.Modelling.DialectSeam/Reference.fs`](../src/Fuaran.Modelling.DialectSeam/Reference.fs).
It is deliberately trivial and deliberately not statistics — it exists to bind the contract, not to
be used. Read it alongside this page.

The worked NON-trivial example is the Compute dialect, `Fuaran.Modelling.Compute`, whose specification
is a regression fitted by Fuaran.Compute. It is what the obligations look like when the vocabulary
belongs to somebody else; [its section below](#a-worked-non-trivial-example--the-compute-dialect)
walks through it.

## What a dialect owns, and what it must not

| | |
|---|---|
| **The dialect owns** | the spec tree, its vocabulary, its validators, the rendering that crosses the wire, the parse-back path, its gate names, its packs |
| **The manager owns** | which specs were explored, how they descend from one another, what was decided about each, what came back from an execution |
| **Neither owns** | the fit itself — that is behind the execution-backend port, on the other side of an opaque payload |

Two consequences worth stating plainly, because they are the ones that surprise people:

**The manager never re-derives your spec hash.** It cannot: re-deriving it needs your payload's
canonical form, which is dialect knowledge by definition. It carries the hash you minted and compares
it. So if two of your users mint different hashes for the same specification, nothing downstream will
notice — and the composite key that joins a decision to an execution will join two records that were
never the same fit. This is why §"Obligation 3" below is stricter than it looks.

**The manager never parses your payload.** No validation, no interpretation, no peek to make an error
message nicer. The line is only load-bearing while it is absolute: the moment the manager can read a
payload it acquires an opinion about what a valid model is, and every dialect afterwards has to
satisfy that opinion.

## The interface

```fsharp
type IDialect<'Spec when 'Spec: equality> =
    abstract Identity: DialectIdentity
    abstract Vocabulary: string list
    abstract Validate: 'Spec -> Finding list
    abstract Project: 'Spec -> Result<SpecPayload, Finding list>
    abstract Mint: string -> string
    abstract Import: string -> Result<ImportOutcome<'Spec>, ImportFailure>
    abstract DefaultGates: GateSpec list
    abstract PackKinds: string list
    abstract ApplyPack: Pack -> 'Spec -> Result<'Spec, PackRefusal>
```

It is generic in `'Spec` — your own spec tree — because that type is the one thing the seam must not
specify. Everything else crossing this interface is a string, a number, or a closed enumeration the
seam declares, so a dialect couples to the seam and to nothing else.

## Obligation 1 — the spec tree

Model your specification as closed discriminated unions. Closed, not open: an exhaustive match is how
your validator, your projector and your importer all stay total, and a `Custom of string` escape
hatch quietly relocates your vocabulary into free text where nothing can check it.

Publish the vocabulary as `Vocabulary` — the term names your tree uses. This is what makes "closed"
*checkable*: the conformance corpus gate runs in both directions, so a term nobody exercises is an
untested claim and a fixture claiming a term you do not publish is a fixture testing something else.

## Obligation 2 — validators

`Validate: 'Spec -> Finding list`. Total, pure, deterministic in content **and in order** — the
harness runs it three times and compares.

A `Finding` carries a stable `Code` you own and never re-purpose, a `Path` locating it, a `Message`
for a human, and a `Severity`:

- `Blocking` — the projector **must** refuse.
- `Advisory` — the projector **must not** refuse.

There are only two levels, and the boundary is load-bearing rather than editorial: a third level
would have no answer to "does this project?", which is the only question the seam asks of a finding.

Nothing compares your messages, so reword them freely. Everything compares your codes, so treat one
like a name in a public API.

## Obligation 3 — the projector, and the hash

```fsharp
abstract Project: 'Spec -> Result<SpecPayload, Finding list>
```

The rendered projection **is** the interface payload. Nothing downstream sees your tree; the executing
side sees exactly these bytes and must not parse, normalise or re-indent them.

Four laws, and the harness checks all four:

1. **Refuse exactly when validation blocks.** Not less: a blocking-invalid spec must not project. Not
   more: a spec with only advisory findings must. This equivalence is what lets a caller trust that
   anything holding a payload holds one whose spec its own dialect considered valid — without
   re-validating something it cannot read.
2. **Be deterministic to the byte.** Same spec, same bytes, every time, on every host. A payload whose
   identity depends on when it was produced has no identity.
3. **The hash is your minting rule applied to your payload bytes, and nothing else.** `Mint` is a
   separate member precisely so the relationship between it and `Project` is checkable.
4. **Importing your own projection reproduces it**, with no residue.

### Minting

Use the shared rule unless you have a reason not to:

```fsharp
let specHashAlgorithm = Seam.mintingAlgorithmId   // "canonical-json-sha256-v1"
let mint (body: string) = Seam.mintTotal body
```

It parses the rendering, re-serialises it with every object's members ordered ordinally ascending
(recursively — by UTF-16 code unit), no insignificant whitespace, arrays in the order given, strings
escaped and real numbers laid out by the model-execution wire specification's §3.1, then digests
those bytes with SHA-256 as `sha256:{lowercase hex}`.

`canonical-json-sha256-v1` is a **registered** identifier, and there is exactly one implementation of
it here — `Fuaran.Modelling.Execution.Minting`, the tier that also carries the §3 encoder the rule is
stated in terms of. `Seam.mintTotal` is a thin totalisation over it, so that `Mint` can answer for
arbitrary bytes: a rendering outside the rule's domain comes back under a distinct `raw:` prefix,
which never arises from a projector's own output and can never be mistaken for a minted hash.

Do not write a second implementation of the registered rule, and be wary of one you get for free from
a JSON library you already have. The details above are the whole of it: escaping, number layout, and
`null` are precisely where two plausible canonicalisations disagree, and a divergence there is
undetectable by anyone downstream (see below). If you need a different rule, declare a different
identifier — that is what the registry is for.

Two properties matter more than the details:

- **The wire payload is not required to be the canonical bytes.** The canonical form is an
  intermediate that exists only to be hashed. Two payloads differing only in member order or
  whitespace therefore differ on the wire and carry the same hash — which is the point.
- **Two submitters that agree on a specification mint the same hash.** Nobody downstream can check
  that they did, so agreement has to come from a shared rule rather than from verification. A dialect
  that invents its own rule for no reason interoperates with nobody, and finds out much later.

Some renderings are outside the rule: duplicate member names in one object (no ordering of duplicates
is deterministic), and a number not exactly representable as a binary64 (render it as a string). If
your rendering is not JSON at all, you need a different rule — declare your own identifier, and say
so. That is an ordinary use of an open registry, not a failure to conform.

`SpecHashAlgorithm` is a field rather than an assumption so that the rule can rotate: identities
minted under an earlier rule stay valid and stay distinguishable, because the identifier travels with
the hash.

## Obligation 4 — the importer, and its residue report

```fsharp
abstract Import: string -> Result<ImportOutcome<'Spec>, ImportFailure>
```

This is the brownfield acquisition path and the migration path, which are the same operation: a
document written for something else, read into your tree.

**The residue report is the entire difference between an importer and a parser.** A foreign document
will contain terms you do not model. Dropping them silently produces a spec that looks complete and
is not, and the person who finds out is whoever compares the result to the original months later. So:
model what you can, and *name* the rest.

```fsharp
type Residue = { Path: string; Term: string; Detail: string }
```

`Path` and `Term` are contract — the harness pins them. `Detail` is for a human and is deliberately
not pinned.

Distinguish residue from failure by kind, not by degree: residue is a successful import that left
something behind; `ImportFailure` is no import at all (`Malformed`, `UnsupportedFormat`,
`Unmappable`).

Import must be deterministic, including the order it reports residue in.

## Obligation 5 — default gates

```fsharp
abstract DefaultGates: GateSpec list
```

Your gate vocabulary maps **onto** the generic `(name, threshold, direction)` triple rather than
replacing it. That is what lets a host, a policy, or an executing side reason about gates from any
dialect without knowing any of them. `direction` is `atLeast` or `atMost`; the vocabulary is closed,
and an unrecognised direction is refused rather than defaulted — a gate whose direction is guessed is
a gate that silently passes.

Names must be distinct. Describe each one: `Description` is what a person choosing gates reads.

## Obligation 6 — packs

```fsharp
abstract PackKinds: string list
abstract ApplyPack: Pack -> 'Spec -> Result<'Spec, PackRefusal>
```

A pack is a parameter bundle folded into a spec — priors, calibration results, a hierarchy. Its
`Body` is opaque to the seam exactly as a spec payload is opaque to the manager: the seam carries the
envelope, you read the body.

Declare your kinds, and **refuse an undeclared one** (`UnknownPackKind`, which enumerates what you do
declare). Refusing rather than ignoring is not pedantry: a pack that silently does nothing is
indistinguishable from one that worked, and the difference surfaces much later as a fit nobody can
explain. The harness synthesises this probe itself rather than taking it from a fixture, so you
cannot satisfy it by declining to write the fixture.

Applying a pack must be deterministic. Refusals are typed: `UnknownPackKind`, `MalformedPack`,
`PackConflicts`.

## Optional obligation — cross-sibling invariants (seam 1.1)

A **forest** is a set of sibling explorations — one per unit — under one shared curation policy
applied independently to each. Coherence across a forest lives at the process layer: every sibling
keeps its own DAG and its own decisions, and nothing shares parameters.

That leaves one question the manager cannot answer on its own. "These siblings should agree about
*this*" is a statement about model structure, and model structure is yours. So a dialect may supply
comparators:

```fsharp
type IInvariantComparators =
    abstract Invariants: InvariantComparator list

type InvariantComparator =
    { Id: string
      Description: string
      Compare: ProjectedState list -> InvariantFinding list }
```

**It is a separate interface, and that is deliberate.** A dialect written against seam 1.0 declines
the obligation by simply not implementing it — it keeps compiling, keeps conforming, and keeps
declaring `TargetsSeam = 1.0` honestly. A new member on `IDialect` would have broken every existing
dialect, which is a *major* change by the only test that matters, for a feature most dialects will
never want. `Seam.invariants` returns an empty list both for a dialect that declines and for one
that declares none, and a host cannot tell those apart — an optional obligation whose declining is
detectable is one a host will start treating as a defect.

**What you receive.** A `ProjectedState` is `(sibling, state, payload)` — the manager's own opaque
payload, produced by *your* projector. Import it back into your own trees and compare them there;
that is the whole reason a cross-sibling invariant about model structure is possible without the
manager acquiring an opinion about model structure. Filter to payloads tagged with your own dialect
id: a forest is not required to be monolingual, and a payload you did not project is not yours to
read.

**What you return.** `InvariantFinding` is `(invariantId, siblings, detail)`. The siblings are the
finding's subject and are compared; `detail` is for a person and is not pinned, on the same
reasoning as a validator's message. **The manager stamps the id from the comparator it called**, so
you cannot misattribute a finding — do not rely on the id you write.

**Be deterministic**, in content and in order: the harness runs your comparator three times and
compares. Sibling lists are de-duplicated and sorted for you; the rest is yours.

**Granularity is your choice, and it has a consequence.** A finding naming five siblings is
downgraded as a whole when *any one* of them has declared a divergence from your invariant. If you
want per-pair accounting, emit per-pair findings.

### Divergences — permitted, but priced

The escape from an invariant is a `DivergenceDeclaration`: an op recorded on the branch that
departs, carrying the invariant id, a justification, who declared it and when. It **downgrades** a
finding — the finding stays in the assessment, with the argument attached — and never deletes it. A
mechanism that removed the finding would set the price of a divergence to nothing.

A declaration naming an invariant no comparator declares is refused
(`Forest.declareDivergence` → `UnknownInvariant`, enumerating the ids that exist). That gate is not
pedantry: a declaration against a misspelled id records perfectly, reads exactly like one that
works, and protects nothing.

Your part is only the comparators. The forest, the declarations, the evaluation and the downgrade
are all the manager's.

### The corpus family

```json
"invariants": [
  { "name": "one-sibling-differs",
    "siblings": [ { "sibling": "unit-a", "state": "s1", "source": "<a document>" },
                  { "sibling": "unit-b", "state": "s1", "source": "<another>" } ],
    "findings": [ { "invariantId": "MY-INVARIANT", "siblings": [ "unit-a", "unit-b" ] } ] }
]
```

Omit the family entirely if you declare no invariants. Populate it and **pin the clean case too**: a
comparator that never fires satisfies every fixture expecting no finding, so a family of only clean
cases certifies silence — and silence is indistinguishable from a coherent forest, which is the
defect the ninth mutant (`SilentComparator`) exists to let you watch your harness catch.

## Seam versioning

```fsharp
{ Id = "my-dialect"
  Version = "1.0"
  PayloadFormat = "my-dialect/v1"
  SpecHashAlgorithm = Seam.mintingAlgorithmId
  TargetsSeam = SeamVersion.current }
```

A dialect declares the seam version it targets. The compatibility rule is stated rather than left to
ordering intuition:

- **same major**, or the dialect is not applicable at all (`MajorSkew`);
- **minor no newer than the host's** — an older minor is fine, since an additive minor means the
  dialect uses fewer obligations than the host implements (`NewerMinor` otherwise).

A major change removes or re-types an obligation; a minor adds one an older dialect can decline to
use. Anything that would break a conforming dialect is major, and that is the only test.

Skew is a **refusal, not a bug**: the dialect is not broken, it is not applicable, and the caller's
move is to obtain a different build rather than to fix anything. `Seam.project` checks skew *before*
projecting, so a dialect this host cannot run never produces a payload that would be stored under a
contract nobody implements.

`Id` and `PayloadFormat` are the two strings the manager already carries on every payload. A payload
whose tags disagree with its dialect's identity is unattributable the moment it is stored, so the
harness treats the disagreement as a failure rather than a style note.

## A worked non-trivial example — the Compute dialect

[`src/Fuaran.Modelling.Compute/`](../src/Fuaran.Modelling.Compute/) is a dialect over a vocabulary it
does not own. Its specification is a Fuaran.Compute fit: the transform pipeline that prepares the
regressors, ending in one `Fit` step. Compute owns the families, the links, the iteration control and
every refusal; the dialect restates none of them. Each obligation, read against it:

| Obligation | How the Compute dialect meets it |
|---|---|
| Spec tree | `ComputeSpec = { Regressors: Transform list; Fit: FitSpec }`: Compute's own types, unwrapped. The vocabulary it publishes is the tree's members (`regressors`, `response`, `predictor`, `ridge`, …), not Compute's verbs. |
| Validators | three sources, none of them the dialect's opinion: Compute's wire (a specification its codec would not read back is refused), Compute's static schema walk (a column the walk proves absent, a response it proves non-numeric), and the refusals Compute's `Fit` states before reading a row. |
| Projector and hash | Compute's canonical pipeline document, so the spec hash is Compute's pipeline hash under the registered `compute-pipeline-sha256-v1`. `Mint` is defined on canonical pipeline documents and gives anything else a `raw:` address. |
| Importer | any pipeline containing a fit; the last fit is the model. Steps after it, and members Compute's codec passed over, are residue. |
| Gates | the fit's failure modes on the generic triple: `rankDeficient` and `notConverged`, observed from what Compute answered (`ComputeDialect.Observe`). |
| Packs | `control` and `ridge`: an iteration budget or a penalty chosen elsewhere, folded in as literals. A ridge penalty for a family that takes none is a conflict. |
| Invariants | declined. A dialect with nothing to say across siblings omits the interface. |

Three things it shows that the reference dialect cannot.

**A minting rule that is not the default, chosen for a join.** The reason to mint under the pipeline
hash rather than `canonical-json-sha256-v1` is outside the dialect: a registry already keys fitted
Compute models by their pipeline hash, and a decision record keyed the same way names the same object
with the same string. Choose another rule only for a reason like that one. Hold the equality with a
law test against the other party's own function and its own corpus, because it is the one property
nothing downstream can check.

**Declared inputs instead of defaults.** Some judgements belong to the deployment: which columns the
dataset has, and how much history a fit needs. The dialect takes them as a `ComputeDeclaration`
(`ComputeDialect.declared`) and has no default for either. With a schema, the walk closes and a column
the fit reads that is not there is refused. With a minimum history, the default gates gain `n`. The
identity, the payload and the hash do not change with the declaration. The corpus certifies the
dialect with nothing declared.

**Read the other party's behaviour, not its description.** The plan this dialect was written from
called rank deficiency a refusal. The pinned Compute fits an aliased design instead: the term stays in
the coefficients with no estimate, and the summary states the rank. The gate reads that statement.
The tests run Compute's evaluator to find this out, so a change in how Compute reports it turns them
red rather than leaving the gate silent.

## Writing the corpus

Author a corpus document per the [corpus format](../conformance/README.md), then:

```fsharp
match Conformance.certify myDialect corpus with
| Ok () -> ()
| Error report -> failwith report
```

Then **make it fail**. The reference dialect ships mutations of itself, each breaking one obligation:

```fsharp
Conformance.run (ReferenceDialect.mutant LossyImporter) referenceCorpus
|> Conformance.failedCodes      // [ "DIALECT-RESIDUE-DRIFT" ]
```

Run those against your harness before trusting it to pass yours. A check that agrees with what you
expected is the least examined kind of evidence.

## A checklist

- [ ] Spec tree modelled as closed DUs; vocabulary published.
- [ ] Validators total, pure, deterministic in content and order; codes stable and owned.
- [ ] Projector refuses exactly when validation blocks.
- [ ] Projection byte-deterministic; hash = `Mint` of the payload bytes.
- [ ] Minting rule declared by identifier — the shared one unless you have a reason.
- [ ] Importer reports residue for everything it did not model; deterministic.
- [ ] Importing your own projection reproduces it, with no residue.
- [ ] Gates on the generic triple, names distinct, each described.
- [ ] Pack kinds declared; an undeclared kind refused.
- [ ] `TargetsSeam` declared honestly.
- [ ] Cross-sibling invariants declared or deliberately declined — and if declared, ids owned and
      stable, descriptions written, comparators deterministic, and the clean case pinned as well as
      the dirty one.
- [ ] Corpus authored, every family populated, every vocabulary term covered.
- [ ] Harness seen to go red before it was trusted to go green.
