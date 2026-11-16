# Versioning

`Fuaran.Modelling`, `Fuaran.Modelling.DialectSeam` and `Fuaran.Modelling.Execution` ship as three
packages at **one version**, set once in [`Directory.Build.props`](Directory.Build.props).

## One version for three packages

They are one contract cut three ways — the manager spine, the extension point in front of it, the
port behind it — and the seam takes a reference on the execution tier so that the registered
`canonical-json-sha256-v1` minting rule has exactly one implementation. Versioning them
independently would produce combinations nobody built or tested, and the failure mode of a wrong
combination is a spec hash that means two different things to two parties who cannot check it.
So they move together, always.

## 0.x posture

Pre-1.0, the public surface may still move. What 0.x does **not** license is a silent move:

- **A change to a package's public contract advances `<Version>` in the same commit.** Adding a
  required field to a record, retyping a member, renaming a case, changing what a function refuses
  — each stops a consumer compiling, or worse, changes what it computes.
- **A folder feed keys packages by id and version.** Re-packing an unchanged version overwrites the
  slot in place, so a consumer pinned to it gets one contract or the other depending only on when
  its NuGet cache was last populated — which is not a version at all. Re-packing an unchanged
  version is fine while iterating on an unpublished one; it is never how a contract change ships.
- **Deliberate lag is legitimate, silence is not.** A consumer holding an older pin across a
  breaking change should say why in a comment beside the pin, so the next reader can tell a
  decision from an oversight.

## What counts as a contract change

Everything reachable from a consumer: the op vocabulary and its refusal enumerations, the session
and policy surfaces, the dialect seam's six obligations and its version tag, the execution port's
methods and refusal vocabulary, the mirrored wire records and their canonical encoding, and the
conformance corpora's stable failure codes. The wire mirror and the minting rule are the sharpest
of these — they are what two parties agree on without sharing types.

The seam carries its own version tag alongside the package version, and seam skew is a typed
refusal rather than a load failure. That tag says which seam contract a dialect binds; the package
version says which build it binds against. They are separate questions and both are answered.

They can move differently, and 0.2.0 was the first time they did. The seam went 1.0 → 1.1 — a minor,
because the cross-sibling invariant obligation arrived as a **separate interface** a 1.0 dialect
declines by simply not implementing it, so no conforming dialect broke. The package went 0.1.0 →
0.2.0 for a different reason entirely: the manager's own surface widened in ways a consumer feels
immediately (a new `ExplorationOp` case, a new field on `SpecState`). A dialect author reading only
the seam tag would conclude nothing changed for them, and they would be right; a consumer of the
manager reading only the seam tag would conclude the same, and be wrong. That is why both numbers
exist.

0.3.0 is the same shape read the other way. The package moved — a new `ExplorationOp` case, two new
`Rejection` cases, a new field on `Exploration` and one on `Forest`, each of which stops some
consumer compiling — and the seam did **not**, because nothing in the release asks a dialect for
anything at all. The vintage axis reads diagnostics an executor already carries, under a metric name
supplied as data; there is no seventh obligation and no new interface. A dialect author reading only
the seam tag would conclude nothing changed for them, and this time they would be right.

Two things in 0.3.0 are worth naming because they look like contract changes and are not. A file
move (`BackendRefusal` from `Backend.fs`, `Rejection` from `Ops.fs`, both into `Types.fs`) changes no
name, no namespace and no signature — F# consumers see nothing. And both wire additions are
byte-additive by construction: a record with no replay campaigns, and a forest with no declared
thresholds, encode to exactly the bytes they did at 0.2.0, which the contract pack's byte pins assert
rather than assume.

0.4.0 is 0.3.0's shape a third time, and the reading is the same both ways round. The package
moved — a new `ExplorationOp` case and a new `OpKind`, a new field on `SpecState`, a new field on
`Policy`, a new `Capability` case — and every one of those stops some consumer compiling, which is
the cost a closed algebra charges for growing. The seam did **not** move, because assent asks a
dialect for nothing: a party class is an opaque declared string, exactly as an invariant id is,
and there is no seventh obligation and no new interface. A dialect author reading only the seam tag
would conclude nothing changed for them, and would be right.

The additions to the wire are byte-additive by construction, like the two before them: a state with
no assents, and a policy declaring no party class, encode to exactly the bytes they did at 0.3.1.
The contract pack's byte pins assert it rather than assume it, including the pin on a forest — whose
encoding contains a policy, and which is therefore the place a non-additive policy change would have
surfaced.

0.5.0 breaks the pattern of the three before it, and the way it breaks it is the interesting part.
The package moved a great deal — eight new public types, a new module, thirteen new codec pairs,
five new reads, two new laws — and **nothing existing changed shape at all**: no type gained a case,
no record gained a field, no signature moved. An exhaustive match written against 0.4.0 still
compiles. So the size of a release and its cost to a consumer are independent, which is exactly what
a version number is supposed to distinguish and exactly what "additive" is supposed to mean.

The op vocabulary is untouched, and that is a design statement rather than a coincidence: an
indistinguishability set is a derived read over the folded record, and a derived read is not an act.
Storing one would be a second truth waiting to drift from the evidence it was derived from, and the
declaration that produced it is a question somebody asked rather than something that happened to the
record. Nothing in this release goes into a history.

`Vintage.wereAlternatives` moved from private to public and is the one existing name that changed at
all — a widening, so no consumer breaks. It is deliberate: the new surface is restricted by exactly
that rule, and reaching it through the same function rather than copying it is what keeps one
discipline from silently becoming two. The seam tag is unchanged at 1.1 for the third release
running, because a metric name is an opaque declared string exactly as an invariant id and a party
class are, and no obligation was added.

0.7.0 is the other kind again, and it is worth saying which kind precisely, because its wire change
is the smallest yet and its type change is not small. A diagnostic can now state its unit, and a
gate the unit of its threshold ([D39](DECISIONS.md#d39--a-diagnostic-states-its-unit-beside-its-value-and-a-gate-never-compares-across-units)).
On the wire every addition is omitted when it carries nothing, so every byte 0.6.0 wrote is a byte
0.7.0 writes, which the contract pack's byte pins assert. In the types it is the 0.2.0–0.4.0 shape:
records gained fields (`OutcomeSnapshot.DiagnosticUnits`, `GateRequest.Unit`, `GateVerdict.Unit`, and
their mirrors) and closed unions gained cases (`BackendRefusal.GateUnitMismatch`, the mirror's
`Refusal.GateUnitMismatch`, `Rejection.HistoryCollision`), so a full-literal construction and an
exhaustive match each stop compiling until they are told about them. 0.6.0 is tagged, so this
advances to a minor rather than riding a draft. The seam tag is unchanged at 1.1 for a fourth
release: a unit attaches to a name a dialect already chose, and nothing is asked of a dialect.

The same draft carries the step D39 left open ([D40](DECISIONS.md#d40--a-metric-series-is-compared-in-one-unit-and-a-margin-states-which)):
a margin states its unit, and the vintage axis and indistinguishability compare a metric only in it.
It is the same class of change — fields on `CorrectionThresholds`, `IndistinguishabilityDeclaration`,
`VintageSeries` and `DecisionRecord`, and a case on `IndistinguishabilityReport` — so it rides 0.7.0,
which was not yet tagged, rather than advancing it. Every wire addition is omitted when it carries
nothing, so a record with no units still encodes to the bytes 0.6.0 wrote.

The same release moved the `Fuaran.Core` pins from 0.20.0 to 0.37.0, because the unit algebra first
ships there and the family moves as one version. That raise crossed Core's 0.37.0 removals; each cost
one row of Core's migration table, and none moved a byte the manager writes.

## 1.0

1.0 is a statement that the surfaces above will only grow within a major version. It is not implied
by the library working.

## Publishing

`pwsh ./pack-all.ps1` packs all three at the current version into a local folder feed
(`-Feed <path>` to choose one). Release publication is a separate, deliberate act: a `v*` tag that
names the `<Version>` in `Directory.Build.props`. The publish workflow refuses a tag that disagrees,
sweeps the publication boundary over the head and the history, runs the gate, packs the three
packages as one version and pushes them to nuget.org. A push to a branch publishes nothing.
