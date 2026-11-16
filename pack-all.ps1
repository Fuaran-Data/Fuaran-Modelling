<#
.SYNOPSIS
    Pack the three Fuaran.Modelling packages into a local folder feed.
.DESCRIPTION
    The inner-loop distribution channel. A consumer that takes this library by
    PackageReference — rather than by project reference inside this repo —
    restores it from a local folder feed, so the packages are produced here and
    dropped there.

    Three packages, packed together at one version (see VERSIONING.md): the
    manager spine, the dialect seam, and the execution-backend port. They are
    one contract cut three ways, and the seam depends on the execution tier for
    the single implementation of the registered minting rule, so a split version
    would only ever be a lie about which pair was tested against which.

    Packing at an UNCHANGED version overwrites the feed slot in place, swapping
    the contract under every consumer pinned to it while their extracted NuGet
    caches keep serving the old bits. That is a legitimate inner-loop move while
    iterating on an unpublished version; it is not a way to ship a contract
    change. Advance <Version> in Directory.Build.props in the same commit as the
    change instead.
.PARAMETER Feed
    The folder feed to pack into. Defaults to a folder feed above this repo, the
    layout this repo is developed in; name any other folder here. Created if absent.
.PARAMETER Configuration
    Build configuration to pack. Release by default.
.EXAMPLE
    pwsh ./pack-all.ps1
    Pack all three at the current version into the default feed.
.EXAMPLE
    pwsh ./pack-all.ps1 -Feed C:\feeds\local
    Pack into an explicit folder feed.
#>

#Requires -Version 7.0
[CmdletBinding()]
param(
    [string] $Feed,
    [string] $Configuration = 'Release'
)

$ErrorActionPreference = 'Stop'
Set-Location $PSScriptRoot

if (-not $Feed) {
    $Feed = Join-Path (Split-Path (Split-Path (Split-Path $PSScriptRoot -Parent) -Parent) -Parent) 'local-nuget-feed'
}
New-Item -ItemType Directory -Force -Path $Feed | Out-Null

# Named rather than packing the solution, which would also walk the test project.
# Listed in dependency order for readability; dotnet pack resolves ProjectReferences itself.
$producers = @(
    'src\Fuaran.Modelling\Fuaran.Modelling.fsproj'
    'src\Fuaran.Modelling.Execution\Fuaran.Modelling.Execution.fsproj'
    'src\Fuaran.Modelling.DialectSeam\Fuaran.Modelling.DialectSeam.fsproj'
)

foreach ($proj in $producers) {
    Write-Host "== pack: $proj" -ForegroundColor Cyan
    dotnet pack (Join-Path $PSScriptRoot $proj) -c $Configuration -o $Feed --nologo
    if ($LASTEXITCODE -ne 0) {
        Write-Host "== pack FAILED: $proj" -ForegroundColor Red
        exit $LASTEXITCODE
    }
}

Write-Host "== pack-all: Fuaran.Modelling packages packed into $Feed" -ForegroundColor Green
exit 0
