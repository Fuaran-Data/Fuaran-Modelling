# fuaran-modelling — "drop into the repo, run one command, the thing works".
# Stage-0 shape (library + tests only): publication boundary -> tool restore -> fantomas -> build
# -> Expecto runner.
#
#   pwsh ./run.ps1                 full pass
#   pwsh ./run.ps1 -SkipBoundary   skip the publication-boundary sweep
#   pwsh ./run.ps1 -SkipFormat     skip the Fantomas pass
#   pwsh ./run.ps1 -SkipBuild      skip the build (implies a prior build)
#   pwsh ./run.ps1 -SkipTests      skip the Expecto suite
#Requires -Version 7.0
[CmdletBinding()]
param(
    [switch] $SkipBoundary,
    [switch] $SkipFormat,
    [switch] $SkipBuild,
    [switch] $SkipTests
)

$ErrorActionPreference = 'Stop'
Set-Location $PSScriptRoot

# A stage that runs no native command must not inherit another stage's exit code, and the
# `global:` scope is what reaches the code a caller sees when this script is invoked with `&`.
$global:LASTEXITCODE = 0
if (-not $SkipBoundary) {
    # The head only: every tracked file. The history is swept with -History, at a publication.
    & (Join-Path $PSScriptRoot 'gates/check-publication-boundary.ps1')
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

$global:LASTEXITCODE = 0
if (-not $SkipFormat) {
    dotnet tool restore
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    dotnet fantomas src tests
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

$global:LASTEXITCODE = 0
if (-not $SkipBuild) {
    dotnet build Fuaran.Modelling.slnx --nologo
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}

$global:LASTEXITCODE = 0
if (-not $SkipTests) {
    # `--no-build` only when this run already built; otherwise let `dotnet run` build first.
    if ($SkipBuild) {
        dotnet run --project tests/Fuaran.Modelling.Tests
    }
    else {
        dotnet run --project tests/Fuaran.Modelling.Tests --no-build
    }
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
