#Requires -Version 7.6

[CmdletBinding(DefaultParameterSetName = 'Offline')]
param(
    [Parameter(ParameterSetName = 'Offline')]
    [switch]$Offline,

    [Parameter(Mandatory, ParameterSetName = 'Consulta')]
    [switch]$Consultar,

    [Parameter(Mandatory, ParameterSetName = 'Consulta')]
    [ValidateSet('Creditos', 'Pagamentos', 'Recolhimentos')]
    [string]$Familia,

    [Parameter(Mandatory, ParameterSetName = 'Consulta')]
    [Parameter(ParameterSetName = 'Offline')]
    [string]$Dll,

    [Parameter(ParameterSetName = 'Consulta')]
    [string]$RepositorioLocal,

    [Parameter(ParameterSetName = 'Consulta')]
    [switch]$NovaSolicitacao,

    [Parameter(ParameterSetName = 'Consulta')]
    [switch]$GuardarXmlFinal,

    [Parameter(ParameterSetName = 'Consulta')]
    [switch]$SemAguardar
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
if (($PSBoundParameters.ContainsKey('Consultar') -and -not $Consultar) -or
    ($PSBoundParameters.ContainsKey('Offline') -and -not $Offline)) {
    throw 'O modo AACBS selecionado exige switch com valor verdadeiro.'
}

Import-Module (Join-Path $PSScriptRoot 'AACBS-RoteiroLocal.psm1')
if ($PSCmdlet.ParameterSetName -eq 'Offline') {
    Invoke-AACBSOffline $Dll
    return
}

# Não há modo de emissão de DARF: cada cenário real ainda depende do DEV.
if (-not $RepositorioLocal) {
    $scriptRepo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
    $worktrees = @(& git -C $scriptRepo worktree list --porcelain 2>$null)
    if ($LASTEXITCODE -ne 0 -or $worktrees.Count -eq 0 -or $worktrees[0] -notmatch '^worktree (.+)$') {
        throw 'Nao foi possivel identificar o checkout principal para o acesso local AACBS.'
    }
    $RepositorioLocal = $Matches[1]
}
Invoke-AACBSLocal $RepositorioLocal $Familia $Dll ([bool]$NovaSolicitacao) ([bool]$GuardarXmlFinal) ([bool]$SemAguardar)
