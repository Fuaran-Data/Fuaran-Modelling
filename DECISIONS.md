# Decisions

Design decisions that shaped `Fuaran.Modelling`, and the reasoning that would have to change for
them to be revisited. A decision without its reason is a rule nobody can safely break.

## D1 — The spec payload is opaque, without exception

The manager stores `SpecPayload.Body` as text and never parses it. No validation, no
interpretation, no "just this once" peek to make an error message nicer.

The moment the manager can read a payload, it acquires an opinion about what a valid model is — and
then every dialect must satisfy that opinion, which is the coupling the seam exists to prevent. The
line is only load-bearing while it is absolute.

Consequence: `SpecHash` is carried, never re-derived. Re-deriving it would require knowing the
payload's canonical form, which is dialect knowledge by definition. The submitter mints it; the
manager compares it.

## D2 — Kill is a verdict, not a deletion

`CurationVerdict.Kill` marks a branch abandoned. Nothing in this library removes a state from an
exploration, and no operation is available that would.

An abandoned branch is informative *because* it was abandoned; the reason recorded against it is
the most reusable thing in the record. Deleting it throws away the finding and invites the next
person to rediscover it. This is also why `CurationRecord.Reason` is required by shape rather than
by validation — a verdict with no reason teaches nothing, and a field that can be omitted will be.

Consequence: a killed branch cannot be extended (`KilledState`). Reviving it is a deliberate
re-curation, which is itself recorded, so the sequence keep→kill→keep survives as history.

## D3 — Curation is a log, not a field

`SpecState.Curations` is an ordered list; `Query.currentVerdict` derives the current one.

A single mutable verdict field loses exactly the interesting part: that someone killed a branch and
then changed their mind. The current verdict is a cheap derivation; the sequence is not
recoverable once discarded.

## D4 — Outcome snapshots live in the decision record

A fit outcome is annotated onto the branch it belongs to, carrying its own composite key,
diagnostics and gate verdicts.

Two reasons, and the second is the one that matters. First: analysis is then offline — no path that
reads the evidence base needs a live execution backend, so meta-analysis over the exploration has
no runtime dependency on anything. Second: the record stays *re-derivable*. The snapshot is not the
authoritative execution record — whatever ran the fit still owns that — but the composite key names
it precisely, so the two can be reconciled at any later point without either side holding a
reference to the other's types.

Consequence: the same fit cannot be annotated twice onto one state (`DuplicateOutcome`). A silently
doubled outcome is a corrupted evidence base, and the composite key is exactly the thing that makes
the duplicate detectable.

## D5 — Every component of the composite key is a string

`CompositeKey` is five strings. None of them is typed to a producer's own model of a dataset, a
seed, or a provider.

The key exists so that two records on opposite sides of a wire can name the same fit with **zero
type sharing**. A typed component would import the producer's model into the manager and defeat
that in a single field.

## D6 — Policy is default-deny, with no wildcard

`Policy.granted` unions grants from active objectives whose directives name the subject *exactly*.
There is no pattern language, no `*` subject, and no "allow unless denied" branch.

A pattern is how a default-deny table quietly becomes a default-allow one — usually at the moment
someone is in a hurry. The expensive failure mode is an automated explorer doing something nobody
authorised, and the only structural defence is a table that must be extended to say yes.

Consequence: `Policy.empty` is the correct starting point, not a degenerate one; and the
`default-deny` law checks the *whole* capability space, not a sample, because the capability that
leaks is the one nobody thought to test.

## D7 — Submitting a fit is a separate capability from recording an outcome

`MaySubmitFit` is not `MayEmit AnnotateKind`.

Spending real compute and writing down a result someone else obtained are different acts with
different blast radii. Collapsing them means a subject trusted to file results is also trusted to
run them.

Process policy stops here. Cost and quota belong to whatever executes the fit, enforced on every
submitter alike — a governed automated exploration passes through both gates, and neither side
implements the other's.

## D8 — Refusals are enumerable data that name the alternatives

Every refusal in the library is a closed union: `Rejection`, `PolicyRefusal`, `BackendRefusal`.
`PolicyRefusal` additionally carries `Allowed` — everything the subject *may* do.

A caller deciding whether to wait, narrow its scope, or ask a human cannot branch on prose. A
refusal that does not enumerate alternatives leaves the caller with nothing but a retry, and that is
a bug in the refusal rather than in the caller.

## D9 — An op attaches to the node that last touched its target

Not to the session's cursor. This is what makes the history branch in the same shape the exploration
does: two forks off one state genuinely fork the DAG, and a curation on one branch extends that
branch's line and no other.

The payoff is line-local replay. Replaying to a line's head yields the exploration as it stood on
that line, so an abandoned branch costs the branch beside it nothing — which is the property that
makes exploration cheap, stated as a claim about replay, and therefore checkable as one.

Within a line every node has exactly one parent and one child, so the topological order is forced
and replay is deterministic without qualification.

## D10 — A cherry-pick is recorded as a merge

`CherryPick` appends a node with *two* parents: the target's line and the source's.

Recording it as a plain child of the target would leave the source a dangling label — the resulting
line could not see where the idea came from, and replaying it would fail on a state that is not in
its closure. A cherry-pick genuinely joins two lines, and the history should say so.

Consequence: `Query.path` still follows the parent lineage (target-ward), while the replayed line's
closure includes the source branch. Those are two different questions and they get two different
answers.

## D11 — The reducer is total, pure, and reads no clock

`Reducer.apply` returns `Result<Exploration, Rejection>`. It throws nothing, generates no ids, and
reads no ambient state. Timestamps and ids are supplied by the caller.

An ambient clock or id generator is the one thing that would make an otherwise-deterministic record
irreproducible — and a caller-supplied timestamp is also a timestamp a test can assert on. Every
determinism property in the contract pack rests on this.

## D12 — The conformance machinery ships in the library, not only in the tests

`Contract` is public: the codecs, the fixture corpus, the byte pins, the laws.

A conformance suite that lives in a repo's tests certifies that repo. This library is a substrate
other people build dialects and hosts on, so its contract has to be executable by *them* — which
means shipping it.

The byte pins are hand-written literals for the same reason. A round-trip test passes happily when
encoder and decoder move together; only a literal notices that the bytes on the wire are no longer
the bytes they were.

## D13 — The execution port is one method

`IExecutionBackend.Submit`, and nothing else, until something needs more.

The port's job at this stage is to prove the seam is real — that the manager can be developed,
tested and reasoned about with no executor present. A wider port speculatively shaped around one
imagined implementation would be harder to satisfy and no better understood. Batch submission is a
list of these; scoring, artifact query and dataset re-derivation grow behind the same seam.

`ReferenceBackend` is part of the decision, not a test fixture: a seam whose refusal path is never
exercised is a seam whose refusal path does not work, which is why `ReferenceBackend.refusing`
ships beside it.

## D14 — The dialect seam is versioned, and skew is a refusal rather than a bug

`DialectIdentity.TargetsSeam` declares the seam contract a dialect was written against.
`SeamVersion.check` admits the same major with a minor no newer than the host's, and refuses
anything else as `MajorSkew` or `NewerMinor`.

A seam only a handful of people implement can be iterated freely; a public one cannot be retrofitted
with a version after the fact, because the dialects that would need it are the ones already written.
The cost of declaring it on day one is one field.

Skew is deliberately not modelled as a failure: a dialect targeting a seam this build does not
implement is not broken, it is *not applicable*, and the caller's move is to obtain a different build
rather than to fix anything. That is why it is a separate refusal type from every validator finding,
and why `Seam.project` checks it before projecting — a dialect this host cannot run must never
produce a payload that is then stored under a contract nobody implements.

## D15 — A projector refuses exactly when validation blocks

Not less, not more. A spec with a blocking finding must not project; a spec with only advisory
findings must.

This equivalence is what everything downstream rests on. The manager cannot read a payload, so it
cannot check that the spec behind one was valid; what it can rely on is that a payload EXISTS only
where the dialect said the spec was valid. Weaken it in either direction and that inference is gone —
a lenient projector mints payloads for specs their own dialect rejects, and an over-strict one makes
"valid" mean something no caller can predict.

Consequence: `Severity` has two levels and will not gain a third. A third level would have no answer
to "does this project?", which is the only question the seam asks of a finding.

## D16 — Minting is a shared rule declared by identifier, not a per-dialect invention

`Minting` implements one canonicalisation — parse, re-serialise with every object's members ordered
ordinally ascending, digest with SHA-256 as a content address — and `DialectIdentity` records which
rule a dialect used.

The property that matters is not local: two submitters that agree on a specification must mint the
same hash, or the composite key joins two records that were never the same fit. Nothing downstream
can check that they did — a payload is opaque, so nobody else has a canonicalisation for its interior
— so agreement has to come from a shared rule rather than from verification. A dialect that invents
its own rule for no reason interoperates with nobody, and finds out much later.

The identifier is a field rather than an assumption so the rule can rotate: identities minted under
an earlier rule stay valid and stay distinguishable, because the identifier travels with the hash.

Consequence: `Mint` is a member of the interface in its own right, so the relationship between it and
`Project` is checkable. A hash that is not the minting rule applied to the payload bytes names
nothing.

## D17 — An importer reports residue; silence is the failure mode

`ImportOutcome` carries the typed spec AND everything the import did not bring across.

The residue report is the entire difference between an importer and a parser. A document written for
another tool contains terms a dialect does not model; dropping them silently produces a spec that
looks complete and is not, and the person who finds out is whoever compares the result to the
original months later. Model what you can, and name the rest.

Consequence: `Residue.Path` and `Residue.Term` are contract and are pinned by the corpus;
`Residue.Detail` is for a human and deliberately is not. And residue is distinguished from
`ImportFailure` by kind rather than degree — residue is a successful import that left something
behind, a failure is no import at all.

## D18 — The conformance corpus pins what a dialect PRODUCES, and ships mutants

Every fixture records an observed value. Where the expectation is a claim — which findings a document
produces, what an importer must report, what a pack must do — it is written by hand; where it is
transcription — canonical bytes, hashes — it is generated, and the committed file is compared against
a fresh emission byte for byte.

A corpus of intentions passes against an implementation doing something else entirely. A corpus
generated wholesale agrees with any implementation, including a wrong one. The split keeps each half
honest against the other's failure mode.

The mutants are the other half of the same argument. A harness that has only ever been seen to pass
certifies nothing — every check in it might be asking a question no implementation can fail. So
`ReferenceDialect.mutant` ships eight deliberate defects, each breaking one obligation and named for
the code that must catch it, and they ship in the library rather than in this repo's tests because
the people who most need to see a harness fail are the ones writing the next dialect.

Consequence: two checks are synthesised by the harness rather than taken from fixtures — the
undeclared-pack-kind probe and the hash-discrimination check over what the dialect actually produced.
A dialect must not be able to satisfy an obligation by declining to write the fixture that would have
caught it.

## D19 — The reference dialect must never become useful

It describes a linear form and computes nothing about data. There is no numerical method in it, and
there must never be one.

A reference implementation exists to keep a seam honest — to be the second dialect that proves the
first one is not the contract. The moment it becomes useful, people use it, and a contract fixture
acquires users, a compatibility surface, and a reason not to change when the seam should. Its
uselessness is the feature; a test asserts that nothing estimated crosses its wire.

## D20 — A pack of an undeclared kind is refused, never ignored

`ApplyPack` returns `UnknownPackKind`, enumerating the kinds the dialect does declare.

A pack that silently does nothing is indistinguishable from one that worked. The difference surfaces
much later, as a fit whose parameters nobody can account for — and by then the pack, the spec and the
outcome have all been recorded as if they agreed.

## D21 — The Layer-2 port absorbs the one-method backend; the narrow seam survives as its projection

`IModelExecutionBackend` is the manager's entire backend-facing surface: submit fits, read
outcomes, resolve a vintage, score, query artifacts, request a re-vintage. `IExecutionBackend`
(D13) is **not** deleted and **not** reimplemented beside it — it is DERIVED from the port by
`Bridge.asExecutionBackend`.

The alternative was a second submission path, and that is the thing worth refusing. Two ports
into one decision record means two places an outcome can be annotated from, and the second one
is always the one that skips a rule — here, the composite-key recomputation and the typed
refusal mapping. Deriving the narrow seam instead means there is exactly one route from a
manager fit to an execution record, and adding another would require deleting the sentence that
says so.

D13's reasoning is not overturned, it is completed. It said the port's job at that stage was to
prove the seam was real, and that a wider port shaped around one imagined implementation would
be harder to satisfy and no better understood. What changed is that the port is no longer
imagined: it is a pinned mirror of a published wire specification with an executable corpus, so
its width is a fact to be matched rather than a guess to be made.

Consequence: the projection requires an executor that registers synchronously, because the
manager's seam answers a submission with an outcome and the wire's answers it with a receipt. An
asynchronous executor is consumed through the wide port directly, where the receipt IS the
answer and outcomes are collected by querying. That is stated at the projection rather than
worked around, because submission is not completion and a seam that pretended otherwise would
make long-running work unrecoverable across a restart.

`ReferenceBackend` (D13) and `ReferenceExecutor` are likewise not rivals: the first is the
manager-tier fixture the contract pack certifies against, the second is the wire-tier executor.
They answer different questions, and only the second is reachable from a manager fit.

## D22 — The mirror tier is FSharp.Core and the BCL, and carries its own codec

Nothing above `Bridge.fs` opens the substrate's own wire model, and a test asserts it.

This looks like duplication and is not. The interface's canonical encoding differs from the
substrate's in four ways, each of which would have been a silent, hash-changing divergence: a
record's members are in DECLARATION order rather than sorted; JSON's short escapes are used
where they exist rather than `\u00xx` for every control character; real numbers follow the
ECMAScript `Number::toString` layout rather than the .NET round-trip one (`1e-7` against
`1E-07`); and an absent optional is REQUIRED to be `null`, which the substrate's model cannot
express at all.

Every one of those is invisible until a digest disagrees across two parties, at which point
nothing in either record says why. A mirror that borrows another wire model's conventions is not
a mirror, it is a translation, and translations drift silently. The cost is one small codec; the
alternative is a class of failure with no diagnosis.

Consequence: the minting rule of the interface is implemented here rather than reused from the
dialect seam. ~~and the two are certified independently — the dialect seam's against its own
corpus, this one's against the specification's `spec-hash` family. They implement the same
registered algorithm, and the corpus is the arbiter of whether they agree.~~ **Superseded by
[D25](#d25--one-implementation-of-a-registered-algorithm-and-a-gate-that-drives-both-tiers-over-it):
the second implementation is gone.** The struck sentence was right about the direction of reuse and
wrong about what followed from it — "certified independently against different corpora" is not a
statement that two implementations agree, and the two did not. Left visible rather than rewritten,
because the error is instructive: it reads as a plan and is actually an absence.

## D23 — Drift is a build gate, not a note

The mirror pins the digest of the corpus manifest it was taken against, and the pin is checked
on every run alongside the vectors themselves.

The two catch different things and neither subsumes the other. The vectors catch a mirror that
re-pinned without re-reading. The pin catches what the vectors structurally cannot: a
specification can move in ways this mirror still happens to satisfy, and "still passes" is not
the same statement as "still current". A new family, a new vector, a changed description — the
pin fires, and a human decides what it means.

The remedy is never to advance the constant on its own. Read what changed, bring the mirror to
it, re-run the vectors, re-pin in the same commit. A pin advanced by itself is a snapshot that
has stopped being one.

Consequence: the corpus is reached by relative path and its ABSENCE IS A FAILURE, loudly. There
is no skip and there must never be one — a drift check that quietly does nothing when it cannot
find what it is checking against leaves the build green, and everybody reads that as "the mirror
is current", which is exactly the statement it is no longer making.

## D24 — The manager's refusal vocabulary is narrowed at the bridge, not widened at the core

Four wire refusal classes map onto the manager's closed union exactly; the rest arrive as a
policy refusal whose rule NAMES the wire class they came from.

Widening `BackendRefusal` to cover all fifteen would be a breaking change to a contract that
ships, and would import the wire's vocabulary into a type whose whole purpose is to be
independent of it. Narrowing at the bridge keeps the class as data — in a string a caller can
still branch on — and keeps the loss visible at the one place that performed it.

The honest cost: a caller of the manager's seam cannot distinguish `forbidden` from `notFound`
without reading the rule string. A caller that needs to consumes the port directly, which is
where the full vocabulary lives and always was.

## D25 — One implementation of a registered algorithm, and a gate that drives both tiers over it

`canonical-json-sha256-v1` is implemented ONCE, in `Fuaran.Modelling.Execution.Minting`. The dialect
seam reaches it (`DialectSeam` takes a project reference on `Execution`) rather than carrying a rule
of its own; what remains seam-side is `Seam.mintTotal`, the totalisation that makes `IDialect.Mint`
answerable for arbitrary bytes, which the wire specification has no shape for.

The direction is one-way and deliberate. The mirror knows nothing of the seam, so the tier that owns
the wire's rules stays the tier that states them, and a dialect author who mints is reading the same
bytes the specification's own `spec-hash` vectors certify.

[D22](#d22--the-mirror-tier-is-fsharpcore-and-the-bcl-and-carries-its-own-codec) had left two
implementations standing and expected the corpus to arbitrate between them. It could not: they were
certified against *different* corpora, so nothing ever compared them, and they disagreed on three
whole classes of document. The seam's rule was built on the substrate's own canonical-JSON discipline,
which agrees with the registered one on flat documents of strings and small integers — which is
exactly what the seam's own corpus contains — and diverges on:

* **control characters** — §3.1 rule 6 uses JSON's short escapes where they exist, so a tab is
  written backslash-t; the substrate's rule writes every control character in the six-character u00xx
  form instead;
* **real numbers** — the ECMAScript `Number::toString` layout against .NET's round-trip one, so
  `1e-7` against `1E-07`, and every hash containing a real number differed;
* **`null`** — the substrate's value model cannot represent it at all, so a specification carrying a
  null member fell out of the rule's domain entirely and minted under the `raw:` fallback while
  still declaring `canonical-json-sha256-v1`.

None of that is detectable downstream. §4.2 rule 2 forbids every party that receives a `specHash`
from checking it, so a divergence between two minters does not fail anywhere — it surfaces, much
later, as a composite-key join that silently misses, with nothing in either record saying why. Two
implementations of a *registered identifier* are therefore not a redundancy to be reconciled at
leisure; the identifier is a promise that anyone declaring it mints the same bytes, and a second
implementation is a second promise nobody is watching.

Consequence: a gate, because deleting the second implementation is only true today. The tests drive
BOTH entry points — the mirror's `Minting`, and the seam's obligation reached through `IDialect.Mint`
exactly as a dialect author reaches it — over every `spec-hash` vector (canonical intermediate byte
for byte, digest hash for hash, the submitter-profile reject included) and over several hundred
randomly generated documents that reach the corners the rule is written about. The generated half
earns its place: it is what found the `null` divergence, which no fixture in either corpus contained.

Consequence: the reject vector's obligation stays where §4.2 rule 2 puts it. The gate asserts both
that `Submitter.checkMinting` catches it AND that `Codec.read` does *not* — a reader that quietly
started checking a hash it was handed would be a conformance defect that looks like diligence.

## D26 — A forest shares a process, never a position, so there is no cross-sibling op

`Forest` names sibling explorations and holds one `Policy`. It holds no exploration, and no op in
the vocabulary carries a sibling id or a forest id.

Statistical pooling shares strength through the parameter space, which entangles the units it
pools; the substitute here shares it through the *decision* space — every sibling keeps its own
DAG, its own branches and its own curation, and what is common is the process applied to each of
them independently. That distinction only survives if it is structural. A forest-level op would be
the one construct able to change two records from one place, and once it exists every later feature
has an obvious home in it; the guarantee then rests on nobody using it that way, which is not a
guarantee.

Consequence: every forest function takes the folded records as an argument and returns an answer,
never a changed record. Consequence: a divergence declaration is an ordinary op on ONE sibling
(D27). Consequence: a forest is *declared* rather than folded — there is no forest op stream,
because there is no forest op, so changing a forest is re-declaring it and the artefact a host
stores is `Wire.renderForest`.

A test pins it directly rather than trusting the prose: a sibling's history folded alone is
byte-identical to the same sibling's history folded beside two others, and no op field's name
mentions a sibling or a forest.

## D27 — A divergence is declared on the branch that departs, and downgrades a finding rather than deleting it

`Declare` attaches a `DivergenceDeclaration` to a `StateId`. `Forest.assess` reads the declarations
standing on the siblings a finding names, and marks the finding `Downgraded` — still present, still
listed, with the justification and the declarer attached.

Two halves, and each is load-bearing.

**Why on a state.** A declaration recorded forest-side would be a second place a sibling's position
can be changed from, which D26 exists to prevent. Recorded as an op it is line-local: it hangs off
that branch's own history, replays with it, is attributed to whoever emitted it, and cannot be
edited away without the op stream saying so. Consistency here is *governed, not dogmatic* — the
mechanism has to make a justified departure cheap to record and impossible to record invisibly.

**Why downgrade rather than delete.** The attractive bug is to filter a justified finding out of the
report: it makes the report shorter and every reviewer happier, and it sets the price of a
divergence to zero. "Permitted but priced" means the finding stays on the books with the argument
beside it, so a reader sees both what diverged and what was said for it, and so a forest with twenty
justified divergences reads as twenty priced divergences rather than as a coherent forest.
`Contract.divergenceDowngradeLaw` checks it executably for exactly that reason.

Consequence: `Standing` has two levels and will not gain a third. Either something accounts for a
divergence or nothing does; a "partly accounted" level would describe the ordinary case — the
divergent sibling declares, the sibling it diverges *from* has nothing to declare — as a
half-failure. `AssessedFinding.Undeclared` carries the finer information without pretending it is a
verdict.

Consequence: declaration is a LOG, like curation (D3), not a field. A restated justification is two
acts and the sequence is the interesting part.

Consequence: the unknown-invariant refusal is a gate ahead of the fold, not a rule inside it. The
fold cannot know the invariant vocabulary — it is dialect knowledge, exactly as a payload's interior
is — so `Forest.declareDivergence` checks it against the comparator set the caller supplies. It
matters because a declaration against a misspelled id folds perfectly, reads in the record exactly
like one that works, and protects nothing.

## D28 — An optional seam obligation is a separate interface, or it is not optional

The invariant comparators are `IInvariantComparators`, discovered by type test, and NOT a member on
`IDialect`. `SeamVersion.current` moved to 1.1.

D14's only test for major-versus-minor is whether a change would break a conforming dialect. A new
member on `IDialect` breaks every one of them — an interface implementation is exhaustive, so the
size of the member is irrelevant and every existing dialect stops compiling. That would have made
the invariant obligation a MAJOR seam change, for a feature most dialects will never use.

A separate interface makes the same capability additive: a 1.0 dialect keeps compiling, keeps
conforming, and keeps honestly declaring `TargetsSeam = 1.0`. `Seam.invariants` returns the empty
list both for a dialect that declines the obligation and for one that implements it and declares
nothing, and those two cases are deliberately indistinguishable to a caller — an optional obligation
a host could tell had been *declined* invites the host to treat declining as a defect, at which
point it is not optional.

Consequence: the interface is not generic in `'Spec`. A comparator reads projected payloads and
imports them into its own trees on its own side of the seam, so the spec type is not needed, and a
non-generic interface is one a host can type-test without knowing which dialect it holds.

Consequence: the corpus's `invariants` family is optional in both directions — omitted from the
document when empty, decoded as empty when absent — so an existing corpus re-emits byte-identically
and a dialect declaring no invariants sees no change at all. The harness still emits a PASSING check
naming the family, because a silently-skipped family reads exactly like one nobody authored.

## D29 — Recurrence names what was chosen, and the naming is enforced

`ChosenRecurrence` counts `ChosenIn` / `SetAsideIn` / `UncuratedIn` / `AbsentIn`. No field is a
score, a rank, or a significance, and `Contract.recurrenceVocabularyLaw` fails the contract pack if
one becomes so.

A branch DAG is a human-guided walk, not a designed experiment. The branches that exist are the ones
somebody thought worth trying and the ones kept are the ones somebody preferred, so a count over
them is a record of choices and is biased by exactly the process it is measuring. It will not
support a claim about what is better no matter how many siblings it spans — what would license that
is an out-of-time record, which this is not.

The rule is enforced rather than described because the failure is a rename made in good faith. A
later reader tidying "ChosenIn" to something "clearer" is one edit away from a field a reader will
take as a verdict, and nothing else in the build would notice. The law is reflective over the
record's own fields, so a field added later is covered without anybody remembering to extend a list.

`Forest.describeRecurrence` is in the library for the same reason: the rendered sentence states what
the number is *and* what it is not, and a caller writing that line itself is where "best" appears.
The description is therefore the one place the banned vocabulary is allowed — it has to be able to
say "not what is better".

## D30 — A replay is an op-recorded act, and its evidence enters by the ordinary annotate path

`Replay` records a `ReplayReceipt` — which vintage, which branches, what landed under what key, what
was refused and why, by whom and when. It carries no outcome. Every snapshot a campaign gathers
reaches its branch through an ordinary `Annotate` op, one at a time.

The alternative shape — one op carrying the campaign's snapshots — was the obvious one and is the
one to refuse, for the reason [D21](#d21--the-layer-2-port-absorbs-the-one-method-backend-the-narrow-seam-survives-as-its-projection)
gives about ports. A second way for an outcome to enter a decision record is always the way that
eventually skips a rule, and here the rule is the duplicate-outcome guard — the only thing standing
between a campaign run twice and a doubled evidence base. Separating the act from the evidence means
running it twice is not an error at all: the second run's annotations are refused per branch, the
receipt records that honestly, and the record is unchanged.

**Why one op names many branches.** A campaign spans branches, which is ordinary; nothing here spans
siblings, which is the line [D26](#d26--a-forest-shares-a-process-never-a-position-so-there-is-no-cross-sibling-op)
draws. The receipt therefore anchors at the session cursor and claims no branch's mark, so replaying
a branch's line yields that branch's own evidence — line-local, because each annotation is — and not
the bookkeeping of the campaign that gathered it. Anchoring it into one participant's line would put
a fact about ten branches inside the closure of one and leave it out of the other nine.

Consequence: the fold checks the ACCOUNT rather than the evidence. Every branch a receipt names must
exist, the id must be fresh, and `Landed` and `Refused` must partition `Requested` exactly
(`IncompleteReplay`) — the same failure the execution port's own receipt is shaped to prevent, one
layer up, where the fold can simply refuse it.

Consequence: `BackendRefusal` and `Rejection` moved into `Types.fs`. A per-branch `ReplayRefusal`
names one or the other, and the receipt is part of the folded record, so both vocabularies have to be
declared before the record is. Same names, same namespace, no consumer difference — recorded because
a reader diffing the tree will see the move and should not have to guess why.

Consequence: there is no manager-tier replay loop over the narrow one-fit backend, and there must not
be one. The campaign runs through the port (`Bridge.replayOnto`), where the batch shape the interface
was widened for is finally the thing being used.

## D31 — A replay moves one component of the composite key, and the seed is the baseline's

`ReplayCampaign.expectedKey` is a one-field record update: the replayed fit's key is the baseline's
with `DatasetVersion` replaced. Same spec hash, same seed, same provider, same provider version.

Written as an update rather than as a construction of five components on purpose. The rule is
"everything but the vintage is held", and a rule expressed as a construction is one a later edit can
weaken without anybody noticing which component moved — while every plausible way of weakening it
produces a record that reads exactly like a replay and is a different fit, so the series it joins is
measuring something nobody asked about.

**The seed rule is: the baseline's, verbatim, where the baseline is the branch's FIRST recorded
outcome.** Not derived per vintage, and not taken from whichever replay ran last. Deriving one would
make each replay a different fit that merely shares a specification; taking the latest would let one
stray submission re-key everything after it.

Consequence: a branch with no prior fit cannot be replayed — there is no seed to carry and no key to
hold constant — so `EveryFittedBranch` does not select it, and a caller who names it explicitly gets
`NoBaselineFit` rather than silence.

Consequence: the default selection is every FITTED branch, kept or killed alike. The killed ones are
not an afterthought, they are the reason the campaign is worth running
([D2](#d2--kill-is-a-verdict-not-a-deletion) is what makes them available), and a default that
quietly re-fitted only the survivors would gather exactly the evidence that cannot answer the
question being asked.

## D32 — The baseline vintage is in-sample, and does not count

A `DecisionRecord`'s baseline is the first vintage on which both branches carried a reading — the
earliest the decision could have been taken on comparable evidence. Every comparison carries
`OutOfTime`, and only out-of-time comparisons count toward a question about the curation.

This is the one thing that makes the signal worth having. The whole approach is a rejection of
in-sample fitting, and a correction argued from the vintage the curation was MADE on is that same
failure wearing the label of the mechanism that avoids it: it says the curator read their own data
differently, not that later data disagreed. Counting it would also be free power — one extra vintage
per decision, always available, always agreeing with whatever the record already showed.

Consequence: a two-vintage exploration has exactly one out-of-time vintage, so it cannot clear a
threshold of two however large the lead is. That is the intended reading, not a limitation to be
worked around.

## D33 — Correction thresholds are declared data with no default, and a correction is a finding nothing acts on

`CorrectionThresholds` carries the metric, its preferred direction, and three gates: minimum
out-of-time vintages, minimum margin per vintage, minimum siblings. `Forest.Thresholds` is an
**option**, and a forest that declares none raises nothing, ever.

**Why no default.** A default would be a number this library invented for a judgement it cannot make
— how much history a unit has, how fast its regime moves, how expensive a wrong question is — and it
would apply silently to every forest that never thought about it. Declared or silent; there is no
third state. The same argument makes a declared threshold of zero read as one: a gate that opens
completely is not a gate, and honouring it literally is the one reading nobody can have intended.

**Why a finding and not an act.** There is no operation in this library that acts on a correction.
Nothing re-curates a branch, nothing proposes one; a caller that wants either emits the ordinary op
itself, under the ordinary policy, attributed to whoever decided. That is not caution for its own
sake — an agent re-curating on an early, low-powered run would be thrashing its own policy to chase
noise, which is the in-sample failure reached from the other direction. Slow accumulation across
both axes, with the human gate in the loop, is the design; making the gate structural rather than
procedural is how it survives contact with an agent that is trying to be helpful.

**Why the naming is enforced.** `Contract.correctionReviewVocabularyLaw` is reflective over
`CorrectionFinding`'s own field names against a banned ACT vocabulary — `correct`, `apply`,
`recurate`, `revert`, `should`, `resolve`, and the rest. It is a different ban from
[D29](#d29--recurrence-names-what-was-chosen-and-the-naming-is-enforced)'s and for a different
reason: recurrence must not claim a merit it cannot support, whereas an out-of-time record may
legitimately say one branch read ahead of another. What this surface must never do is read as
something DONE, or to be done, to a curation — a field called `Correction` or `ShouldRecurate` would
move the human gate into the machinery by naming alone, and nothing else in the build would notice.
`Vintage.describeCorrection` is in the library for the same reason `describeRecurrence` is: the
rendered sentence has to say both that it is raised for review and that no curation has been changed,
and a caller writing that line itself is where the other sentence appears.

Consequence: the sibling gate applies to a single exploration too, which counts as one sibling. It
would have been easy to skip it there as "not applicable", and that is exactly how a declared
threshold becomes a decorative one — the cheapest read bypassing the gate the expensive one enforces.

Consequence: a decision pair is one somebody was actually choosing between — one branch descends from
the other, or the two share a parent. Pairing every kept branch with every killed one would
manufacture decisions nobody took, hundreds of them in an exploration of any size, each able to raise
a question that no curator ever faced.

## D34 — The global fold order spends its freedom on the domain's precedence, never on a content hash

`Session.ofJsonl` re-folds a stored history into the working `Record`. That fold needs a total order
over a structure that only carries a partial one, and the order it used to take was assembled
head-major: every head's ancestor closure in turn, first occurrence wins, heads taken from
`Dag.heads` — which returns them **id-sorted**. A node id is a hash over content that includes
caller-chosen state ids, so which of two genuinely concurrent nodes folded first was decided by
hash luck. A stored decision record that replays differently depending on the ids its author
happened to mint is the negation of the line-local determinism law the whole design rests on.

It was not a theoretical hazard. Two shapes reproduce it, and on freshly-minted state ids each
failed well over half the time:

* **The kill that folds first.** `Curate` claims its state's mark; `Fork` does not claim its
  *source's*. So a fork off a state and a later kill of that state both hang off whatever last
  touched it — siblings, with nothing recording which came first. Fold the kill first and the fork
  it must precede is refused `KilledState`, and the load fails outright.
* **The receipt that folds before its branches exist.** A `Replay` receipt is deliberately anchored
  on the cursor and claims no mark ([D30](#d30--a-replay-is-an-op-recorded-act-and-its-evidence-enters-by-the-ordinary-annotate-path)),
  so the branches its `Requested` names are not its DAG ancestors. Fold it before a branch on
  another line and the fold reports `UnknownState`.

Both are the same defect: **the DAG's edges are an under-approximation of the domain's
dependencies, and the remaining freedom was being spent on a hash.** The fix does not touch the
edges. It replaces the head-major concatenation with one topological sort over all nodes, draining
the eligible frontier by `(foldRank, id)` — so a node still folds only after every one of its
parents (a rank can never reorder a causally-dependent pair), and the choice among genuinely
concurrent nodes becomes a stated rule instead of an accident.

`foldRank` is two rules, one per face. **Creation dominates**: every op that demands a *live* state
also creates one (`Fork`'s `From`, `CherryPick`'s `Onto`, `Propose`'s and `Resolve`'s `From`), and
`Curate` is the only op that can take liveness away — so a creator must never be left behind a
concurrent kill. **A receipt observes, so it folds last**: `Replay` is the one op whose references
are deliberately not its ancestors, and deferring it costs nothing because no other op reads
`e.Replays`. Both are sufficient rather than lucky: a `Curate` claims its state's mark, so any
still-valid creator off that state is a sibling of the curation or earlier, and is therefore
eligible no later than it.

**Why not record the submission order instead.** The obvious alternative — stamp each node with an
ordinal and fold by it — was rejected, and not only because it is a store-format change. The
submission order is one host's accident; the DAG is the history. A record that folded by arrival
would replay differently for two hosts that reached the same history by different routes, which is
the property content addressing exists to give up. What the fix owes is determinism plus
foldability, and a stated precedence over the causal order delivers both without adding a field
that would then have to be trusted.

Consequence: no node id moves, no stored byte changes, and no anchoring changes — only the order in
which stored nodes are folded. `tests/Fuaran.Modelling.Tests/fixtures/pre-fix-session.jsonl` is a
history written by the build that carried the defect and could not read its own output back; the
suite re-folds it and re-serialises it byte-for-byte, so a future change that quietly moved the DAG's
shape would fail that comparison rather than pass it.

Consequence: the fold order is still **not** the order a live session submitted in, and
`ofJsonl`'s docstring still says so. Where exactness matters the answer is unchanged — compare PATH
replays, because within one line the topological order is forced and `replayState` agrees between a
live session and a reloaded one, always.

Consequence: a truncated sort now names itself. If the emitted order is shorter than the DAG, a
cycle left nodes unreachable, and `ofJsonl` reports that instead of folding the prefix and returning
a confident, wrong `Ok`.

## D35 — Party classes are declared policy data, and an undeclared party is refused

`Policy.Parties` declares the classes a process recognises. `PartyAssent.record` refuses a record
naming a class no policy declares, enumerating the ones that are.

The declared set is the DENOMINATOR of every consensus question, and a denominator assembled from
whoever happened to speak is not one. A state endorsed by the single party that bothered to appear
would read exactly like a state every party stands behind, and nothing in either record would say
which it was. So the set has to exist before the first record rather than accumulate from it.

**Why on the policy rather than in the record.** A forest is one policy applied independently to
each sibling ([D26](#d26--a-forest-shares-a-process-never-a-position-so-there-is-no-cross-sibling-op)),
so declaring the classes there is what makes "every declared party" mean the same thing across a
whole forest. Declared per exploration they would drift, and a cross-sibling column would be
comparing counts taken over different sets — which is worse than having no column, because it looks
like one.

**Why the refusal rather than admission.** A record standing in the name of an undeclared class
folds perfectly and reads, in the record, exactly like one that counts. It counts toward nothing.
That is the same failure the unknown-invariant gate exists for
([D27](#d27--a-divergence-is-declared-on-the-branch-that-departs-and-downgrades-a-finding-rather-than-deleting-it)),
and it is caught the same way, at the same place: a gate ahead of the fold, holding the vocabulary
the fold cannot see.

Consequence: a class RETIRED after its records were made leaves live positions behind, and they are
reported as `AssentStanding.Unrecognised` rather than dropped or absorbed. Dropping them would hide
that somebody said something; absorbing them would answer a question nobody asked.

Consequence: speaking as a class is `MayAssentAs`, a capability distinct from `MayEmit AssentKind`,
on [D7](#d7--submitting-a-fit-is-a-separate-capability-from-recording-an-outcome)'s argument exactly.
Recording assent and recording the INDEPENDENT REVIEWER'S assent are different acts, and a subject
trusted to file one party's position is not thereby trusted to file another's. Collapsing them would
make the party scoping decorative — anybody who could speak could speak for everybody.

## D36 — Consensus is derived, silence is not endorsement, and a withdrawal is a recorded act

There is no stored consensus flag. `PartyAssent.consensus` reads a standing, which is derived from
the assent log against the declared set, and a policy that declares NO class reaches consensus on
nothing, ever.

**Why derived.** A stored flag is a second truth waiting to drift, and here it would drift in the
one direction that matters: declaring a fourth party would leave every previously-consensual state
still flagged. Derived, the same unchanged record simply falls short, which is the correct answer
and arrives without anybody re-running anything.

**Why the empty declaration reaches nothing.** "All of none endorse" is vacuously true and
completely wrong — it would report unanimous backing for every state in every record that never
thought about parties at all. Declared or silent; there is no third state. That is
[D33](#d33--correction-thresholds-are-declared-data-with-no-default-and-a-correction-is-a-finding-nothing-acts-on)'s
argument about a threshold of zero, met in a different surface: a gate that opens completely is not
a gate.

**Why a withdrawal is an act and not an erasure.** `Withdraw` is a third verdict in the same log,
not the removal of the record it supersedes. Nothing here deletes an assent, exactly as nothing
deletes a curation ([D2](#d2--kill-is-a-verdict-not-a-deletion)) — and the reason is the same one,
sharper: a consensus reached after a challenge was raised, argued and answered is a very different
thing from one nobody ever questioned, and the only place that difference lives is the log.

Consequence, and it is the one that surprises: a party that withdraws its challenge has NOT thereby
endorsed. Standing down and signing are different acts, and consensus needs the second. This is
`Query.keptStates`'s asymmetry a third time — silence is not abandonment, and silence is not
endorsement either — and it is why `AssentStanding` carries `Withdrawn` and `Silent` as separate
fields rather than as one absence.

Consequence: assent is a LOG per party, and the live position is the most recent record from each
class ([D3](#d3--curation-is-a-log-not-a-field)). Endorse, then challenge, then withdraw is three
legitimate acts by one party, and the sequence is the interesting part.

## D37 — The two axes are recorded separately and their disagreement is a query

`Annotate` carries what the data said; `Assent` carries who stands behind it. Neither is derivable
from the other, neither overrides the other, and `PartyAssent.tensions` names the states where they
disagree — in BOTH directions.

A model is good in two senses and only one of them is statistical. Collapsing them loses whichever
one is not being measured that day, and the usual collapse is into a single approval transition
owned by whoever holds the role — at which point "the gates pass" and "the client will live with it"
have one answer between them and no way to differ.

**Why the disagreements are the surface.** Agreement is the ordinary case and needs no report. The
pairs that disagree are the ones somebody has to look at, and without a query they are implicit in
two records nobody reads side by side. Gates green under an open challenge, and gates red under full
endorsement, are both ordinary situations and NEITHER IS AN ERROR: a challenge over green gates may
be the reading that is right, and a failed gate under full endorsement may be a gate asking about a
threshold nobody agreed to. `describeTension` is in the library for the reason `describeRecurrence`
and `describeCorrection` are — the rendered sentence has to say that nothing is resolved, because
the attractive next sentence is the one that picks a winner.

**Why a challenge blocks nothing.** It does not fail a fit, invalidate a gate, or re-curate a
branch. It is recorded, justified and priced, exactly as a divergence declaration is
([D27](#d27--a-divergence-is-declared-on-the-branch-that-departs-and-downgrades-a-finding-rather-than-deleting-it)),
and for the same reason: the mechanism's job is to make an objection cheap to record and impossible
to record invisibly, not to stop work. An objection that halted a branch would be filed less often,
which is the opposite of what the record needs.

Consequence: the gate reading is the state's MOST RECENT evidence, not all of it. "Do the gates
hold" is a question about where the branch stands now; how its readings moved across vintages is the
vintage axis's question and it answers it properly. Reading every outcome would leave a branch red
forever on the strength of its first fit.

Consequence: a state with no gate evidence — never fitted, or fitted with no gate requested — is
never in tension, however loudly the parties disagree. There is no statistical position to disagree
with, and the absence of a gate is the absence of a question. The challenge is still an ordinary
recorded challenge; calling it a tension would put it in the one report that exists to name pairs.

Consequence: the naming is enforced, by a THIRD ban and for a third reason.
[D29](#d29--recurrence-names-what-was-chosen-and-the-naming-is-enforced)'s ban keeps a count from
claiming a merit it cannot support;
[D33](#d33--correction-thresholds-are-declared-data-with-no-default-and-a-correction-is-a-finding-nothing-acts-on)'s
keeps a finding from reading as an act; `Contract.assentVocabularyLaw` keeps assent from reading as
a RANKING. A count of endorsements looks exactly like a quality score, and one field renamed in good
faith — `Approved`, `Grade`, `Score` — turns "three people signed this" into "this is the better
form", which is a claim no number of signatures can license.

## D38 — The modelling problem is under-determined, so the record answers with a set and never with a choice

The solution space over model specifications is unbounded and there is no calibration metric that
settles it, so there is no single right spec — a fact about the problem, not a shortfall in
anybody's process. `Indistinguishability` makes the honest response queryable: given a declared
`(metric, margin, scope)`, which branches does this record **fail to distinguish** from a reference?

**Why this is available at all.** Because a killed branch is retained ([D2](#d2--kill-is-a-verdict-not-a-deletion)).
A curation is a decision somebody took; it is not a claim that the alternatives stopped being
plausible, and a record that deleted them could only ever report a family of one. Curating the
alternatives away therefore does not shrink the set, which is checked rather than described.

**Why the declaration has no default.** The same argument [D33](#d33--correction-thresholds-are-declared-data-with-no-default-and-a-correction-is-a-finding-nothing-acts-on)
makes for thresholds, with one sharper consequence. How close is too close to call depends on the
metric, the domain, and what the answer will be used for — all house judgements — so an undeclared
margin yields `NoDeclaration` and a reference with no reading yields `NoReferenceReading`, each its
own case. An empty member list is a real and decisive finding about the evidence: nothing is close
to this specification. Returning one for a question nobody asked would report that finding to every
caller who forgot to declare anything, and nothing downstream could tell the two apart.

**Why membership is symmetric and unordered.** There is no `MetricPreference` on the declaration and
no sign on `IndistinguishabilityMember.Distance`. A branch reading further ahead of the reference is
exactly as far from it as one reading further behind, and both are outside the margin at the same
distance; a preferred direction would let the surface say which way a difference ran, which is the
first sentence of a ranking. For the same reason the forest recurrence is ordered by the record
rather than by how often a branch was indistinguishable — where
[D29](#d29--recurrence-names-what-was-chosen-and-the-naming-is-enforced)'s `chosenRecurrence` and
`AssentRecurrence` both sort by count, and where this one must not: a list ordered by a count is
read as a league table by everyone who did not write it.

**Why only genuine alternatives are eligible.** `Vintage.wereAlternatives`, the same rule the
decision pairs use and reached through the same function rather than a second copy of it. Two
branches on unrelated lines were never weighed against each other, so two readings that happen to
fall close together are not a choice anybody faced. The fixture is built so that the reading
*closest* to the reference belongs to a branch that was never an alternative — a membership rule
built on distance alone passes everything else and fails that one.

**Why the baseline is labelled and not excluded.** [D32](#d32--the-baseline-vintage-is-in-sample-and-does-not-count)
excludes the baseline from a question about a *curation*, and a set is not one. But a set read on
the evidence the branches were curated on cannot carry the weight of one read on data that arrived
afterwards, and a reader who cannot tell which they are holding will use them the same way — so
`OutOfTime` rides on the set and on its rendered sentence. Excluding it would be worse than
labelling it: a record with one vintage would then yield no set at all, which reads exactly like a
record with no near-equivalents.

Consequence: the naming is enforced by a FOURTH ban, and this is the sharpest of the four.
`Contract.indistinguishabilityVocabularyLaw` is reflective over the surface's own field names against
a banned RANKING vocabulary — `best`, `win`, `rank`, `prefer`, `ahead`, and the rest. The surface's
entire content is that the evidence cannot tell these branches apart, so a ranking here does not
merely overclaim, it contradicts the finding it would be attached to. One field renamed in good
faith turns "four specifications this data cannot separate" into a recommendation, which is the one
thing an under-determined problem cannot yield.

Consequence: an unread alternative is named, never dropped. A branch nobody fitted on the vintage
being read is not a branch that reads differently, and a set that silently narrowed to whatever
happened to be measured would report a smaller family than the record can account for. The same
reasoning keeps `unread` on the wire even when it is empty, where a state's additive list fields are
omitted: an absent key there would read as "every alternative was measured", which is a claim about
the evidence rather than the absence of one.

## D39 — A diagnostic states its unit beside its value, and a gate never compares across units

An outcome reports `Diagnostics: Map<string, float>`, deliberately open: every dialect has its own
names. What the map could not say is what each number measures. An RMSE is in the target's unit, an
R² is dimensionless, a coefficient is in the target's unit per the regressor's, and a gate that
compares an RMSE in pence with a threshold in pounds is wrong by a factor of a hundred while reading
as a pass. Phase 3 lets a backend state a unit per diagnostic, and lets a gate state the unit of its
threshold. The vocabulary stays open: a unit is attached to a value a dialect already names, and
nothing is enumerated.

**The wire shape is a separate map, and it is omitted when empty.** The model-execution
specification gains `diagnosticUnits` on `fitOutcome` — a map from a diagnostic's name to its unit
text, ordinal-sorted, beside `diagnostics` — and `unit` on `gate` and `gateVerdict`, beside
`threshold`. All three are omitted when they carry nothing. That is an exception to the
specification's own rule that every declared member is present (`null` when absent), and it is taken
deliberately and written into the rule: a member that arrives after version 1 and is emitted as `{}`
or `null` would make every document emitted before it non-canonical, which is the presence change
the specification's versioning section calls breaking. Omitted when empty, a snapshot with no units
encodes byte-identically to the one it encoded before, every existing fixture is unchanged, and an
older reader ignores the member by the specification's own extension rule.

The alternative was to make each diagnostic an object carrying `value` and `unit`. It was not taken:
it retypes an existing member, so it moves every outcome's bytes and every reader's decoder, and
under the specification's versioning section that is a new envelope version for a change whose
whole content is additive. Two maps keyed by the same names cost one rule (a unit names a diagnostic
the outcome carries) where the object form costs a version.

**The F# surface follows the wire: a separate map beside `Diagnostics`.**
`OutcomeSnapshot.DiagnosticUnits: Map<string, UnitOfMeasure>`, `GateRequest.Unit` and
`GateVerdict.Unit: UnitOfMeasure option`. A value type replacing `float` was the alternative, and it
would have retyped every reader of the evidence — the vintage axis and indistinguishability read
`Diagnostics` as numbers, and none of them needs to change for a unit to be carried. Either form
adds a field, so every full-literal construction stops compiling (FS0764) whichever is chosen; that
is why the package advances to 0.7.0 rather than riding 0.6.0, which is tagged (`VERSIONING.md`).
The mirror of the specification follows the specification's own types: strings holding canonical
unit text, `Map<string, string>` and `string option`.

**A unit is `Fuaran.Core.Unit`'s, and the codec checks it.** Modelling takes `Fuaran.Core.Unit`, the
small package that gives a consumer units without columns, so the dependency rule (Modelling takes
Core and nothing else) holds. A unit text on either wire is valid only when it is the canonical text
of an admitted unit: `Unit.parse` accepts it and `Unit.render` writes it back byte for byte. A
parseable but non-canonical spelling (`s-1` for `/s`, `m.kg` for `kg.m`) is refused, because two
texts for one unit would give one outcome two encodings. The check sits at the codec boundary on
both tiers, so nothing past a decoder holds an unchecked unit. The mirror tier still takes no wire
model from the substrate: the unit algebra is a value algebra with no wire of its own, and the test
that keeps the mirror a mirror now admits exactly that one qualified reference and still refuses an
`open Fuaran.Core`.

**A gate states its threshold's unit, and a mismatch is refused by name, never converted.** A gate
is evaluated only where its unit is the unit the backend reports for the diagnostic it reads — both
absent counts as agreement, which is the path every gate took before units existed. Otherwise the
backend refuses with `GateUnitMismatch` (the wire's new `gateUnitMismatch` refusal class), naming the
gate, the unit it stated and the unit reported. Conversion was available — `Unit.conversionFactor`
is exact — and was not taken: a gate is a submitter's declared threshold, and a threshold in grams
silently re-read as kilograms answers a question nobody asked, while a refusal tells the submitter
which of the two was wrong. A gate that states no unit against a diagnostic that has one is refused
too, for the same reason a direction is never defaulted: a guessed unit is a gate that can silently
pass. The check is `GateVerdict.evaluateIn`; the reference backends report the units they are
composed with and call it.

**Not in this decision.** The vintage axis and indistinguishability compare one metric across
outcomes, and their margins are "in the metric's own units". A series that mixes units for one
metric is now detectable; refusing it is a separate question about those reads and is left to the
phase that takes it, rather than decided here by the change that made it possible.

## D40 — A metric series is compared in one unit, and a margin states which

D39 let an outcome state the unit of each diagnostic, and left one question open: the vintage axis
and indistinguishability compare one metric across many outcomes, against a margin "in the metric's
own units", and nothing stopped that series from carrying grams on one outcome and kilograms on the
next. Read as alike, 500 g beside 0.3 kg is a lead of 499.7 — a correction question raised, or a
family split, on the strength of a unit nobody converted. This decision closes it.

**A margin states its unit.** `CorrectionThresholds.Unit` and `IndistinguishabilityDeclaration.Unit`
(`UnitOfMeasure option`) name the unit `MinMargin` and `Margin` are in, beside them, for the reason
D39 gave a gate's threshold one: a margin is a declared number, and a number with no unit is a
number a reader must guess at. `None` states none. `Indistinguishability.declare` keeps its
signature and declares none; `declareIn` takes the unit, as `GateVerdict.evaluateIn` sits beside
`evaluate`. The alternative was a margin with no unit, read in whatever unit the series happened to
share. It was not taken: across a forest each sibling's series can share a unit with itself and
not with its neighbour, and one margin read in grams in one unit and kilograms in the next makes the
sibling count — the breadth the accumulation is built on — a count of different questions.

**A reading is compared only where it states the margin's unit.** Units are compared by canonical
text and never converted (D39, and §3.5 of the model-execution specification); `None` agrees only
with `None`. So the two kinds of mixed series — two different unit texts, and a stated unit
beside an unstated one — need no rule of their own: a series in more than one unit always contains a
reading that is not in the declared one, whichever that is. The same rule refuses a series that
agrees with itself in a unit the margin does not state, which is a gate's mismatch made again. The
one comparison helper is `SeriesUnits.agree`, and everything below reads through it.

**Indistinguishability refuses the set; the vintage axis reports the vintage.** The two reads have
different shapes, and each takes the answer its shape can carry honestly.

- A set is ONE vintage and one family. When the readings it would be built from — the reference's
  and every scoped alternative's on that vintage — are not all in the declared unit, the report is
  `MismatchedUnits`, a fourth case beside `NoDeclaration` and `NoReferenceReading`, carrying the
  metric, the declared unit, every unit found (ordered by canonical text) and whether some reading
  stated none. Narrowing the set instead — dropping the alternatives read in another unit, as an
  unread one is listed apart — was rejected: an unread alternative is an absence of evidence, while
  one read in kilograms is evidence that cannot be placed, and a family with it dropped reads as a
  smaller family, which is the number a multiplicity report is built on. A reading outside the
  scope is not part of the series and refuses nothing.
- A decision record spans vintages, and already names the vintages it cannot compare
  (`Incomparable`). A vintage on which both branches have a reading and either is not in the
  thresholds' unit is named in `DecisionRecord.UnitMismatched` with both units, and is not compared;
  the record states its unit in `DecisionRecord.Unit`. Refusing the whole record was rejected: a
  backend that begins stating units part-way through a record would make every decision in it
  unreadable for good, when every vintage stated in one unit is still comparable evidence. The
  baseline stays a line in time — the first vintage with both readings, read before units are —
  because a first vintage in another unit is still the evidence the curation was taken on.
- A branch's series lists readings rather than comparing them, so it reports: `VintageSeries.Units`
  states each reading's unit beside it, keyed by vintage, as `DiagnosticUnits` sits beside
  `Diagnostics`.

**Nothing moves for a record with no units.** Thresholds and declarations that state none, over
outcomes that state none, compare exactly what they compared before, and every addition is omitted
on the wire when it carries nothing, so their bytes are unchanged; the contract pack's byte pins and
its `series-units` law assert both. The F# surface is the shape D39's was — records gained fields
and a closed union gained a case — and it rides the 0.7.0 draft that D39 opened (`VERSIONING.md`).

**Not in this decision.** The public model-execution specification states no cross-outcome read:
it carries one outcome's diagnostics and their units, and §6.3 keeps judgement off the wire. The
vintage axis and indistinguishability are this library's own reads over its own record, so nothing
here moves the specification or its corpus.

## D41 — The Core pin sits on a published line, named once, and restores from nuget.org alone

The four `Fuaran.Core.*` pins moved 0.37.0 → **0.41.0** and are now one property,
`FuaranCoreVersion`. Two things chose the number. It is tagged and published, so an outside clone
restores exactly what the gate restores; and it is the line the published compute packages build
on, so a package that pins this library beside them resolves one Core rather than two.

The package sources are nuget.org and nothing else (`nuget.config` clears the rest). A folder feed
listed first shadows a released package with whatever was last packed into it, which is useful in a
development loop and is exactly what a published library must not depend on: the pin would name a
version only one machine could serve.

Crossing 0.38.0 to 0.41.0 cost two changes, both at the substrate's edge:

- **A DAG write takes a `Stream` and answers a `Written` record.** The hash function and the
  witness are declared together (`OpStream.canonical hashFn witness`) where they were two
  arguments, and `Dag.append` / `merge` answer `{ Id; Dag }` where they answered a pair.
  `Session`'s own signatures are unchanged: it still takes the hash function, and builds the
  stream where it writes, verifies and loads.
- **`Canon.render` and `Canon.typed` are `Canonical.write Rendering.CrossHost` and
  `Canonical.typed`.** The byte rule has a name now; the bytes are the ones it always wrote.

A cyclic history is reported with every node the replay could not place, where it named one.
Nothing else a caller sees moved: no type gained a case, no record gained a field, and no stored
byte or node id changes, which the contract pack's byte pins and the conformance corpus assert. So
the raise rides the 0.7.0 draft (`VERSIONING.md`).

## D42 — The Core pin moves when this library needs it to, not whenever Core releases

This library is not raised with every Core release. Its consumers are dialects and hosts on their
own release rhythms, and the version that matters to each is the one whose seam and wire it
certified against; a raise it did not need would move that version for a reason none of them can
see in their own tree. It takes Core and nothing else at runtime, so it is never the reason
another package is behind.

What stays unconditional is the producer's rule: a change to the public surface advances
`<Version>` in the same commit, and a pin names a published version. A raise is taken when a
feature here needs a newer Core, or when a package built beside this one would otherwise restore
two.

## D43 — "Written to a public standard" is a gate, and the history is swept at a publication

`gates/check-publication-boundary.ps1` sweeps every tracked file for what a reader outside this
repository could not look up: private component and product names, the tools and scripts of the
tree it is developed in, machine and actor identifiers, absolute paths, and relative paths that
leave the repository. `run.ps1` runs it first, so it is part of the ordinary gate. A claim without
a check decays one convenient reference at a time, and nothing breaks when it does.

Three choices in it are deliberate:

- **A confidential name is held as a digest.** Banning a name by spelling it in a public file
  publishes it. Those terms are the SHA-256 of their lower-cased form, and every word of every
  line is hashed and compared.
- **The copyright line is recognised by its exact shape, not exempted by file.** Each source file
  opens with it; the same name in any other sentence is a hit.
- **Exceptions are itemised and printed on every run**, a green one included, each with its file,
  its count and its reason. A count that moves in either direction fails.

A publication carries its history, so `-History` sweeps every commit's message and every line any
commit added, and the publish workflow runs it before it packs. A recorded commit message cannot be
edited in an ordinary change: the remedies are to rewrite the history or to publish with it, and
that is a decision taken at a publication, not by whoever happens to run the sweep.

**The public history begins at a cut.** At the first publication the sweep found commit messages
that named things outside this repository, so the decision was taken once: the repository was
published as a single commit of its head, and the development history before it is kept apart,
unpublished. Nothing in the tree was changed by the cut. Version 0.6.0 and everything before it
were tagged in that earlier history and were never published to a registry.

The specification's corpus is reached through `FUARAN_MODEL_EXECUTION_SPEC` for the same reason:
a relative path to a neighbouring checkout assumes a layout nothing here enforces, so the layout
is the fallback and the variable is the interface.
