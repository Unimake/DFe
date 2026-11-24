#Requires -Version 7.6

[CmdletBinding(DefaultParameterSetName = 'Verificar')]
param(
    [Parameter(ParameterSetName = 'Verificar')]
    [switch]$Verificar,

    [Parameter(Mandatory, ParameterSetName = 'Offline')]
    [switch]$Offline,

    [Parameter(Mandatory, ParameterSetName = 'Autenticar')]
    [switch]$Autenticar,

    [Parameter(Mandatory, ParameterSetName = 'Debitos')]
    [switch]$Debitos,

    [Parameter(Mandatory, ParameterSetName = 'Autenticar')]
    [Parameter(Mandatory, ParameterSetName = 'Debitos')]
    [Parameter(ParameterSetName = 'Offline')]
    [string]$Dll,

    [Parameter(ParameterSetName = 'Verificar')]
    [Parameter(ParameterSetName = 'Autenticar')]
    [Parameter(ParameterSetName = 'Debitos')]
    [string]$RepositorioLocal,

    [Parameter(ParameterSetName = 'Debitos')]
    [switch]$NovaSolicitacao,

    [Parameter(ParameterSetName = 'Debitos')]
    [switch]$GuardarXmlFinal
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$action = $PSCmdlet.ParameterSetName
if ($PSBoundParameters.ContainsKey($action) -and -not [bool]$PSBoundParameters[$action]) {
    throw 'O modo AACBS selecionado exige switch com valor verdadeiro.'
}

Import-Module (Join-Path $PSScriptRoot 'AACBS-RoteiroLocal.psm1')

if ($action -eq 'Offline') {
    Invoke-AACBSOffline $Dll
    return
}

if (-not $RepositorioLocal) {
    $scriptRepo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
    $worktrees = @(& git -C $scriptRepo worktree list --porcelain 2>$null)
    if ($LASTEXITCODE -ne 0 -or $worktrees.Count -eq 0 -or $worktrees[0] -notmatch '^worktree (.+)$') {
        throw 'Nao foi possivel identificar o checkout principal para o acesso local AACBS.'
    }
    $RepositorioLocal = $Matches[1]
}
Invoke-AACBSLocal $RepositorioLocal $action $Dll $NovaSolicitacao.IsPresent $GuardarXmlFinal.IsPresent
