# Contributing

Thanks for your interest. This is an early-stage substrate; the surface is still moving, and the
fastest way to have a change land is to open an issue describing the problem before writing code.

## Ground rules

- **The spec payload stays opaque.** No change may make this library parse, validate or interpret
  a `SpecPayload.Body`. If a feature seems to need it, it belongs in a dialect. See
  [DECISIONS.md](DECISIONS.md) D1.
- **No model vocabulary.** No estimator, no distribution, no transform, no framework name — in
  code, in documentation, or in a test fixture.
- **No new dependencies.** `FSharp.Core` plus the `Fuaran.Core` substrate. This is a hard boundary,
  not a preference.
- **Refusals are data.** A new failure mode is a case on a closed union with the information a
  caller needs to act, not a string.
- **Nothing deletes a decision.** Records accumulate; verdicts supersede.

## Before you open a pull request

```
pwsh ./run.ps1
```

That runs the whole gate: the publication-boundary sweep, `dotnet tool restore`, Fantomas over
`src` and `tests`, the build, and the Expecto suite. All four must be clean.

- **The boundary sweep reads every tracked file.** This repository names nothing a reader outside it
  cannot look up. A hit is fixed at the site; one that genuinely cannot be is an itemised entry in
  `gates/check-publication-boundary.ps1`, with its reason.
- **Fantomas is not optional.** Run it inside the repo so it picks up `.editorconfig`.
- **Adding an op kind** means adding its `OpKind` case, its fold behaviour, its codec, and a sample
  in `Contract.samples` — the coverage gate fails otherwise, which is the intended behaviour.
- **Changing the canonical encoding** breaks the byte pins in `Contract.bytePins`. That is a
  deliberate, breaking wire change, not a formatting tweak: say so in the pull request.
- **New behaviour needs a test that can fail.** Where practical, confirm the test actually goes red
  against the unfixed code before you rely on it going green against the fixed code.

## Licence

By contributing you agree that your contributions are licensed under Apache-2.0, matching the
project's [LICENSE](LICENSE).
