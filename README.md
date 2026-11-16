# Fuaran.Modelling

**The spec-agnostic model manager.** A model exploration is a branching search, and the branching
*is* the knowledge product — which branch was tried, what came back, what a human decided about it,
and why. `Fuaran.Modelling` records that: the decision record over statistical and econometric
model specs, on a content-addressed branching op-DAG with deterministic replay.

It knows nothing about models. The spec payload is opaque from end to end — the library never
parses it, validates it, or interprets a parameter. Model vocabulary lives in a *dialect*, behind
the dialect seam; execution lives behind the execution-backend port. What is left in the middle is
the part that is the same for every model class.

Apache-2.0. `FSharp.Core` plus the `Fuaran.Core` substrate — no estimator, no numerical library, no
framework, no host.

## What it is

| | |
|---|---|
| **Owns** | the *decision* record — spec states, their lineage, curation, policy, outcome snapshots, party assent |
| **Does not own** | the *execution* record — datasets, fitting, artifacts, deployment, serving, monitoring |
| **Vocabulary** | spec, branch, curation, fit outcome, party assent. Zero estimator knowledge, zero dialect vocabulary |

It is deliberately **not** an ML-operations platform. Deployment, serving, monitoring and artifact
lifecycle belong to whatever executes the fits. The manager manages decisions.

## The spine

**An exploration op vocabulary** — a closed algebra of eight moves plus two for review:

| Op | Meaning |
|---|---|
| `Seed` | start an exploration at its first state |
| `Fork` | branch from an existing state; the parent is untouched, always |
| `ReturnTo` | move the working head back to an ancestor; destroys nothing |
| `CherryPick` | re-land one branch's payload onto another — recorded as a *merge*, so the line keeps both ancestries |
| `Curate` | record a verdict — keep, kill or note — with its reason |
| `Annotate` | record what an execution backend reported |
| `Declare` | record a justified divergence from a cross-sibling invariant, on the branch that departs |
| `Assent` | record where one declared party stands on a state — endorsing, challenging, or standing down |
| `Replay` | record that an exploration was re-submitted onto a new dataset vintage, and what became of each branch |
| `Propose` / `Resolve` | table a branch for review; accepting materialises exactly what was reviewed |

Every op names exactly one exploration. There is no op that names two, which is what keeps a forest
(below) a set of independent records rather than a coupled one.

**Kill is a verdict, not a deletion.** Nothing in this library removes a state. An abandoned branch
is informative precisely because it was abandoned, and the reason recorded against it is the most
valuable thing about it. A killed branch cannot be silently extended — reviving it is a deliberate
act.

**Outcome snapshots live in the record.** A fit outcome — diagnostics, gate verdicts, and the
composite key `(specHash, datasetVersion, seed, providerId, providerVersion)` — is annotated onto
the branch it belongs to. The evidence base is therefore self-contained and queryable offline; no
analysis path ever needs a live execution backend, and the record is still re-derivable against one.

**Path replay is line-local and deterministic.** An op attaches to the history node that last
touched the state it targets, so two forks off one state fork the history. Replaying to a line's
head yields the exploration *as it stood on that line* — not the union of everything ever tried.
Within a line the topological order is forced, so any two hosts replaying it reach the same record,
and a history that has been serialised and read back replays every line identically.

**Policy is default-deny.** A subject may do exactly what an active objective grants it: no
wildcard, no implicit owner, no "unless denied" branch. Submitting a fit is a capability distinct
from recording an outcome, because spending compute and writing down someone else's result are
different acts. A refusal is data, and it *enumerates the alternatives* — a refusal a caller cannot
act on is a bug in the refusal.

## Forests — sharing a process, not a position

A **forest** is a set of sibling explorations — one per unit — under **one policy applied
independently to each**. The alternative is to share strength through the parameter space, which
entangles the units it pools. This shares it through the *decision* space instead: every sibling
keeps its own DAG, its own branches and its own curation, and what is common is the process, never
the positions it arrives at.

That is structural rather than aspirational. A `Forest` carries sibling references and a policy and
holds no exploration; every query takes the folded records as an argument and returns an answer, not
a changed record; and no op in the vocabulary names a sibling. A test pins the consequence: a
sibling's history folded alone is byte-identical to the same history folded beside two others.

**Cross-sibling invariants** are the one question that crosses. A dialect may declare comparators
over projected sibling states; the manager hosts the evaluation and owns the findings. The
comparator runs dialect-side over the dialect's own trees, imported from the payloads it is handed,
so the manager can carry an invariant about model structure without ever reading a spec. The
obligation is **optional** — a separate interface a dialect declines by not implementing it — which
is what let it arrive as a minor seam version rather than breaking every dialect written before it.

**Divergence is permitted but priced.** The escape from a finding is a `DivergenceDeclaration`
recorded as an ordinary op on the branch that departs, carrying the invariant, the justification,
and who declared it. It **downgrades** the finding — which stays in the assessment with the argument
attached — and never deletes it. A declaration naming an invariant no comparator declares is
refused, because one that recorded silently would read exactly like one that worked and protect
nothing.

**Recurrence is what was chosen, never what is better.** `Forest.chosenRecurrence` counts, per
decision, the siblings that kept it, set it aside, left it open, or never explored it. A branch DAG
is a human-guided walk rather than a designed experiment, so the count is biased by the very process
it measures and will not support a better-than claim at any size. The naming is held to that
executably: a law in the contract pack fails the build if a field on that record acquires merit
vocabulary.

## Vintages — validating decisions out-of-time

When a new dataset vintage arrives, an exploration can be **replayed** onto it: the branches that
were kept **and** the branches that were killed, re-submitted as one batch through the execution
port. That is available at all only because an abandoned branch is retained — a killed branch is a
counterfactual somebody already paid to explore, and it is still there — and it is worth having only
because the signal is *out-of-time*, which is the one kind of evidence that does not reintroduce the
in-sample failure the whole approach rejects.

**A replay moves one component of the composite key.** Same specification, same seed, same provider,
new vintage. The seed is the baseline fit's, carried verbatim, so every fit in a branch's series is
the same decision asked again of different data rather than a new fit that happens to share a
specification.

**A replay is an act, and the act is recorded separately from its evidence.** The `Replay` op carries
a receipt — which vintage, which branches, what landed under what key, what was refused and why — and
carries no outcome. Each outcome reaches its branch by the ordinary `Annotate` path, through the
ordinary duplicate-outcome guard, so there is still exactly one route by which evidence enters a
decision record and running a campaign twice cannot double an evidence base. Refusals are per branch
and the campaign completes around them: a receipt's landed and refused branches must partition what
it says it requested, and the fold refuses one that does not.

**Reading the evidence base.** Per branch, the series of one diagnostic across vintages; per
decision — a branch that was kept and one that was set aside — the per-vintage comparison, under a
metric name and a preferred direction supplied as data (the manager enumerates no diagnostic
vocabulary and never will); and across a forest, each decision's recurrence with every sibling's
out-of-time record beside it. Those two stay separate fields rather than one blended number: the
count is what curators *did*, the record is what later data *said*, and only the second can license a
better-than claim.

**Accumulation is slow by declaration and silent below the gate.** Correction thresholds — minimum
out-of-time vintages, minimum margin, minimum siblings — are declared per forest and there is no
default: a forest that declares none raises nothing, ever. The vintage the decision was taken on is
in-sample and does not count toward a question about it. Below the gate the record accumulates and
says nothing, which is checked rather than described, because a gate that fires correctly *and* fires
early is not a gate.

**And a correction is a question, never an act.** Nothing in this library re-curates a branch or
proposes one. The finding names the decision, the vintages the set-aside branch led on, the weakest
of those leads, which siblings raised it and the thresholds it cleared; a caller that wants to act
emits the ordinary op itself, under the ordinary policy, attributed to whoever decided. The naming is
held to that executably — a law in the contract pack fails the build if a field on that record
acquires the vocabulary of something done.

## Assent — the second quality axis

A model is good in two senses and only one of them is statistical. Gate verdicts and outcome
snapshots record what the data said; whether the people who have to live with the result stand
behind it is a different question with a different kind of answer, and a record carrying only the
first collapses the second into a single approval owned by whoever holds the role.

So the record carries **party-scoped assent**: a declared party class endorses, challenges, or
stands down on a state, with a reason, as an ordinary op on the branch it is about.

**The party classes are declared, as policy data.** A class minted at call time would make the
denominator of every consensus question a function of who happened to speak — a state endorsed by
the one party that bothered to appear would read exactly like one every party stands behind. A
record naming an undeclared class is refused and the refusal enumerates the classes that exist.
Speaking *as* a class is its own capability, distinct from recording assent at all, so a subject
trusted to file one party's position is not thereby trusted to file another's.

**Consensus is derived, and silence is not endorsement.** There is no stored flag: every declared
party endorsing is read off the log against the declared set, so declaring a fourth party makes the
same unchanged record fall short, which is the correct answer and arrives without re-running
anything. A policy declaring no class reaches consensus on nothing, ever — "all of none endorse" is
vacuously true and would report unanimous backing for every record that never thought about parties.
A party that stood down has stopped objecting, which is not the same as having signed.

**A challenge is priced, not obeyed.** It fails no fit, invalidates no gate and re-curates no
branch — it is recorded, justified and attributed, exactly as a divergence declaration is, because
the job is to make an objection cheap to record and impossible to record invisibly. And a
withdrawal is an act rather than an erasure: the challenge it supersedes stays on the log, which is
the only place the difference lives between a consensus that took work and one nobody questioned.

**The load-bearing read is the tension surface.** Agreement needs no report; the states where the
two axes disagree are the ones somebody has to look at, and without a query they are implicit in two
records nobody reads side by side. Both directions count — every gate holding under an open
challenge, and a gate failing under full endorsement — and neither is the other's error: the
challenge may be the reading that is right, and the gate may be asking about a threshold nobody
agreed to. The rendered sentence says so, because the attractive next sentence is the one that picks
a winner. Across a forest the same reads roll up per sibling, on the recurrence machinery, with the
same caution: consensus in nine units is nine sets of people signing, not evidence that the tenth
was wrong.

## Indistinguishability — how many specifications the evidence cannot separate

The modelling problem is under-determined. The solution space is unbounded, there is no calibration
metric that settles it, and so there is no single right specification — a fact about the problem
rather than a shortfall in anybody's process. The record already holds the honest response, because
the branches that were set aside are still there: what an exploration contains is not one answer but
a family of them.

So the record answers a question of the form *which branches does this evidence fail to distinguish
from this one* — the primitive a multiplicity report is built on, where the spread of a business
quantity across near-equivalent specifications replaces a point estimate somebody would otherwise
read as the truth.

**The declaration has no default.** A metric, a margin in that metric's own units, and a scope,
supplied as data. How close is too close to call is a house judgement, and there is no value this
library could invent for it that would not be a convention presented as a fact. An undeclared margin
yields an explicit no-declaration state, and a reference the metric was never measured on yields its
own — never an empty list. An empty list is a *decisive finding*: nothing is close to this
specification. Returning one for a question nobody asked would report that finding to everyone who
forgot to ask.

**Membership is symmetric and unordered.** There is no preferred direction on the declaration and no
sign on the distance: a branch reading further ahead of the reference is exactly as far from it as
one reading further behind. The forest roll-up is ordered by the record rather than by how often a
branch was indistinguishable, which is where the other two recurrence surfaces do sort — a list
ordered by a count is read as a league table, and on the one surface whose whole content is *these
cannot be told apart*, that reading is self-contradicting. A law in the contract pack fails the build
if a field on any of these records acquires ranking vocabulary.

**Only genuine alternatives are eligible** — the same rule the decision pairs use, reached through
the same function. Two branches on unrelated lines were never weighed against each other, so two
readings that happen to fall close together are not a choice anybody faced. **An unread alternative
is named rather than dropped**, because a branch nobody fitted on the vintage being read is not a
branch that reads differently. And the set carries whether the vintage it was read on is the
reference's own baseline: an in-sample family is a real answer, and one a reader must be able to
tell from a family that survived new data.

**Curating the alternatives away does not shrink the set.** A killed branch is still a specification
this evidence cannot distinguish, which is what retaining it was for.

## Using it

```fsharp
open Fuaran.Core
open Fuaran.Modelling

let hashFn = OpStream.defaultHash
let actor = Human "curator"

let policy =
    { Objectives =
        [ Policy.objective "work" "full access" "curator"
              ((OpKind.all |> List.map MayEmit) @ [ MaySubmitFit ]) ] }

let payload body hash =
    { Dialect = "reference"; Format = "json"; Body = body; SpecHash = hash }

// Seed, then fork a rival branch.
let session =
    Session.empty
    |> fun s -> Session.submitGoverned hashFn policy "curator" actor
                    (Seed { StateId = "base"; Label = "baseline"; Payload = payload "{}" "h0" }) s
    |> Result.map snd
```

Fit a branch through a backend and record what came back, in one governed step:

```fsharp
let backend = ReferenceBackend.create "reference" "1" "2026-01-01T00:00:00Z"

Session.submitFit hashFn policy "curator" actor backend
    { StateId = "left"
      Payload = payload "{\"v\":1}" "h1"
      DatasetVersion = "ds-2026-01"
      Seed = "7"
      Gates = [ { Name = "convergence"; Threshold = 0.5; Direction = AtLeast } ]
      SubmittedBy = SubmitHuman }
    session
```

Then read one line back:

```fsharp
Session.replayState "left" session   // the exploration as it stood on that line
Session.toJsonl session              // the whole history, one line per node
Session.ofJsonl hashFn text          // verified on the way in — a tampered id is a named error
```

## Seams

**The dialect seam** (`Fuaran.Modelling.DialectSeam`). A dialect supplies six things, and may
optionally supply a seventh: the typed spec
tree for its model class, its validators, a projector that renders that tree to the payload crossing
the execution wire, a parse-back importer with a residue report, its own gate vocabulary mapped onto
the generic `(name, threshold, direction)` triple, and a pack schema. The manager sees only the
rendered payload, and the `SpecPayload.Dialect` / `Format` tags are open registered strings — the
manager enumerates no dialects, which is the whole point. The optional seventh is a set of
cross-sibling invariant comparators (above), carried by a separate interface so that adding it broke
no dialect written before it.

The seam is a **public extension point**, so its contract is executable rather than described: the
six obligations are one typed interface, the seam is versioned and skew is a typed refusal, and a
[conformance corpus](conformance/README.md) runs a dialect against fixtures that pin what it
*produces*. Two laws carry most of the weight — a projector refuses **exactly** when validation
blocks, and the spec hash is the declared minting rule applied to the payload bytes and nothing else
— because everything downstream trusts a payload it cannot read.

The default minting rule, `canonical-json-sha256-v1`, is a **registered identifier with exactly one
implementation here**, in the execution tier below; the seam re-exports it rather than carrying a
second. Two implementations of one identifier cannot be reconciled after the fact — nobody receiving
a spec hash is permitted to check it — so agreement is arranged by construction and then pinned by a
gate that drives both entry points over the specification's own vectors and over generated documents.

`ReferenceDialect` is the trivial second dialect that binds the contract: a linear form, explicitly
not statistics, computing nothing about data. It exists to prove the seam, not to be used. It also
ships **mutations of itself**, each breaking one obligation, so a dialect author can watch the
harness go red before trusting it to go green.

Start at [`docs/writing-a-dialect.md`](docs/writing-a-dialect.md).

**The execution-backend port** (`Fuaran.Modelling.Execution`). The manager's entire backend-facing
surface as one interface — `SubmitFits`, `GetOutcomes`, `ResolveVintage`, `Score`, `QueryArtifacts`,
`RequestRevintage` — every operation returning a typed refusal in the error channel. Nothing crossing
it is an exception or an error string: a caller deciding whether to wait, reduce scope, re-aim at
another provider or ask a human has to branch on the reason, and it cannot branch on prose.

**Batch and pagination are first-class, and single-item is the degenerate case.** The design load is
a submitter replaying many configurations across many vintages — a forest, not a model. A
single-item interface retrofitted with batching produces a batch endpoint whose semantics are a
loop, and a loop has no partial-acceptance shape, so the receipt could not be added later without
breaking every caller.

Its types are **FSharp.Core-only records mirroring a language-neutral model-execution wire
specification** field for field — the pinned-snapshot posture. That brings its own canonical codec
(records in declaration order, maps sorted, 64-bit integers as sign-prefixed strings, real numbers
by the ECMAScript rule, absent optionals as `null`) and the submitter-side minting canonicalisation
that makes a spec hash mean the same thing to two parties who share no types. What keeps the
snapshot honest is a **drift-conformance harness**: it runs the specification's own corpus — decode,
re-encode byte for byte, recompute every identity, refuse every reject vector with the class the
corpus names — and pins the corpus manifest's digest, so the build goes red when the specification
moves rather than when somebody notices. The corpus's absence is a failure, never a skip.

`ReferenceExecutor` is a deterministic in-memory implementation that fits nothing and estimates
nothing. Every number is a pure function of the composite key and a name, so the same submission
yields the same diagnostics, verdicts, artifact address and cost on every host — and it refuses in
the interface's own vocabulary, because a port whose refusal path is never exercised is a port whose
refusal path does not work. It lets the manager and a dialect develop with no executor present at
all.

`Bridge` is the only place the two halves meet: it derives the manager's one-method
`IExecutionBackend` from the port, so an outcome reaches a branch through the port and only through
the port. `ReferenceBackend` remains the manager-tier fixture the contract pack certifies against.

## The contract pack

The library ships its own conformance machinery in `Contract`, not just in its tests: the codecs, a
fixture corpus with a round-trip case per op kind and a set of inputs a decoder must refuse, byte
pins on the canonical serialisation, and the laws the substrate promises.

```fsharp
Contract.certify OpStream.defaultHash   // Ok (), or every failing law named
Contract.laws OpStream.defaultHash      // each law reported individually
```

The laws: default-deny holds across the whole capability space; a refusal enumerates alternatives;
every op kind round-trips and the corpus covers every kind; the canonical bytes are the bytes they
were; path replay is line-local and stable across persistence; a divergence declaration downgrades a
finding and never removes it; the recurrence surface names acts rather than merits; a replayed fit's
composite key differs from the fit it replays in the dataset version and nothing else; an out-of-time
record below the declared threshold raises nothing and at it raises exactly one question; the
correction surface names evidence rather than an act; consensus is derived and is never reached over
an empty declaration; the tension surface finds both directions and finds nothing where the two axes
agree; the assent surface names a position rather than a ranking; an undeclared margin derives no
indistinguishability set where a declared one derives exactly the branches within it, and only ones
that were genuine alternatives; and that surface names a relation rather than an order.

A conformance suite that lives only in this repo's tests certifies this repo. Shipping it certifies
anyone's.

## Building

```
pwsh ./run.ps1                 # publication boundary -> tool restore -> fantomas -> build -> tests
pwsh ./run.ps1 -SkipFormat     # skip the formatting pass
pwsh ./run.ps1 -SkipTests      # build only
```

Requires the .NET 10 SDK (pinned in `global.json`). Every dependency restores from nuget.org.

The suite certifies the execution mirror against the
[model-execution specification](https://github.com/Fuaran-Data/fuaran-model-execution-spec)'s own
fixtures, and fails without them. Check the specification out and name the checkout in
`FUARAN_MODEL_EXECUTION_SPEC`.

## Packages

Four packages, versioned together:

| Package | What it carries |
|---|---|
| `Fuaran.Modelling` | the manager spine — op vocabulary, path replay, curation, policy, outcome snapshots, forests, the vintage evidence axis, the consensual axis, indistinguishability sets, the contract pack |
| `Fuaran.Modelling.DialectSeam` | the dialect seam, its conformance corpus, and the reference dialect |
| `Fuaran.Modelling.Execution` | the execution-backend port, the wire mirror and its codec, the reference executor |
| `Fuaran.Modelling.Compute` (from 0.8.0) | the Compute dialect: a specification that is a Fuaran.Compute fit, whose spec hash is Compute's pipeline hash, and its conformance corpus |

A consumer takes `Fuaran.Modelling.DialectSeam` to write a dialect; it brings the manager and the
execution tier with it. `Fuaran.Modelling.Compute` is the only package that depends on
Fuaran.Compute, so a consumer that does not take it never restores Compute.
`pwsh ./pack-all.ps1` packs all four into a local folder feed for inner-loop consumption
(`-Feed <path>` to choose one). A release is a `v*` tag: the publish workflow packs all four at the
tagged version and pushes them to nuget.org. Why one version covers
four packages, and what obliges it to move, is in [`VERSIONING.md`](VERSIONING.md).

## Licence

Apache-2.0 — see [LICENSE](LICENSE) and [NOTICE](NOTICE).
