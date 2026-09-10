[CmdletBinding()]
param([string]$RepositoryRoot = (Get-Location).Path)

& (Join-Path $PSScriptRoot "test-generated-pdca-plan.ps1") -RepositoryRoot $RepositoryRoot
