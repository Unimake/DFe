[CmdletBinding()]
param(
    [switch]$Verificar,
    [switch]$AtualizarCredenciais
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Este preparador local nao faz chamadas HTTP nem implementa servicos da DLL.
if ($env:OS -ne 'Windows_NT')
{
    throw 'Este preparador exige Windows para cifrar o ClientSecret com DPAPI.'
}

. (Join-Path $PSScriptRoot 'AACBS-AcessoLocal.ps1')

$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$localDirectory = Get-LocalDirectory $repositoryRoot
$configurationPath = Join-Path $localDirectory 'configuracao.local.json'
$credentialPath = Join-Path $localDirectory 'credenciais.local.clixml'

if ($Verificar)
{
    $null = Read-LocalAccess $localDirectory
    Write-Output 'Configuracao local e credencial cifrada verificadas. Nenhum token foi solicitado e nenhuma API foi chamada.'
    return
}

if (((Test-Path -LiteralPath $configurationPath) -or (Test-Path -LiteralPath $credentialPath)) -and -not $AtualizarCredenciais)
{
    throw 'Ja existe configuracao local. Use -Verificar para conferir ou -AtualizarCredenciais para substituir conscientemente.'
}

$matrixCnpj = (Read-Host 'CNPJ completo da matriz (14 digitos, sem pontuacao)').Trim()
if ($matrixCnpj -notmatch '\A[0-9]{14}\z')
{
    throw 'CNPJ da matriz deve conter 14 digitos. Nenhum arquivo foi gravado.'
}

$selectedEnvironment = (Read-Host 'Ambiente: ProducaoRestrita ou ProducaoBeta [ProducaoRestrita]').Trim()
if ([string]::IsNullOrWhiteSpace($selectedEnvironment)) { $selectedEnvironment = 'ProducaoRestrita' }
if ($selectedEnvironment -notin @('ProducaoRestrita', 'ProducaoBeta'))
{
    throw 'Ambiente invalido. Nenhum arquivo foi gravado.'
}
if ($selectedEnvironment -eq 'ProducaoRestrita') { $selectedEnvironment = 'ProducaoRestrita' }
else { $selectedEnvironment = 'ProducaoBeta' }

$accessCredential = Get-Credential -Message 'AACBS: informe o ClientId no campo Usuario e o ClientSecret no campo Senha.'
if ($null -eq $accessCredential -or [string]::IsNullOrWhiteSpace($accessCredential.UserName) -or $accessCredential.Password.Length -eq 0)
{
    throw 'ClientId e ClientSecret sao obrigatorios. Nenhum arquivo foi gravado.'
}

$configuration = [pscustomobject][ordered]@{
    cnpjMatriz = $matrixCnpj
    ambiente = $selectedEnvironment
    habilitarTestesReais = $true
    permitirProducao = ($selectedEnvironment -eq 'ProducaoBeta')
    permitirGeracaoDarf = $false
    limiteAberturasPorApiPorDia = 1
    intervaloConsultaSegundos = 120
    maximoConsultasSituacao = 120
    tempoMaximoAcompanhamentoMinutos = 240
    cenariosDarf = @()
}

Write-LocalAccess $localDirectory $configuration $accessCredential -AtualizarCredenciais:$AtualizarCredenciais
$accessCredential = $null

Write-Output 'Acesso local configurado. ClientSecret cifrado pelo Windows; arquivos ignorados pelo Git.'
Write-Output 'Geracao de DARF desabilitada. Nenhuma API foi chamada. Confira com -Verificar.'
