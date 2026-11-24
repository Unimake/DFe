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

$repositoryRoot = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$localDirectory = Join-Path $repositoryRoot '.local/aacbs'
$configurationPath = Join-Path $localDirectory 'configuracao.local.json'
$credentialPath = Join-Path $localDirectory 'credenciais.local.clixml'

foreach ($relativePath in @('.local/aacbs/configuracao.local.json', '.local/aacbs/credenciais.local.clixml'))
{
    & git -C $repositoryRoot check-ignore --quiet -- $relativePath
    if ($LASTEXITCODE -ne 0)
    {
        throw 'A pasta local AACBS precisa estar ignorada pelo Git antes de configurar as chaves.'
    }
    $trackedPaths = @(& git -C $repositoryRoot ls-files -- $relativePath)
    if ($LASTEXITCODE -ne 0 -or $trackedPaths.Count -gt 0)
    {
        throw 'Os arquivos locais AACBS nao podem estar rastreados pelo Git.'
    }
}

if ($Verificar)
{
    if (-not (Test-Path -LiteralPath $configurationPath) -or -not (Test-Path -LiteralPath $credentialPath))
    {
        throw 'Acesso local ainda nao configurado. Execute este script sem -Verificar.'
    }

    try
    {
        $localConfiguration = Get-Content -LiteralPath $configurationPath -Raw | ConvertFrom-Json
        $localCredential = Import-Clixml -LiteralPath $credentialPath
        if ($localConfiguration.cnpjMatriz -notmatch '^\d{14}$' -or
            $localConfiguration.ambiente -notin @('ProducaoRestrita', 'ProducaoBeta') -or
            $localConfiguration.habilitarTestesReais -isnot [bool] -or
            $localConfiguration.permitirProducao -isnot [bool] -or
            $localConfiguration.permitirGeracaoDarf -isnot [bool] -or
            ($localConfiguration.limiteAberturasPorApiPorDia -isnot [int] -and $localConfiguration.limiteAberturasPorApiPorDia -isnot [long]) -or
            $localConfiguration.limiteAberturasPorApiPorDia -lt 1 -or
            $localConfiguration.limiteAberturasPorApiPorDia -gt 4 -or
            ($localConfiguration.ambiente -eq 'ProducaoBeta' -and -not $localConfiguration.permitirProducao) -or
            $localCredential -isnot [Management.Automation.PSCredential] -or
            [string]::IsNullOrWhiteSpace($localCredential.UserName) -or
            $localCredential.Password.Length -eq 0)
        {
            throw 'Campos invalidos.'
        }
    }
    catch
    {
        throw 'Nao foi possivel validar o acesso local. Confira os campos e use o mesmo usuario Windows e computador da configuracao. Nenhum valor foi exibido.'
    }

    Write-Output 'Configuracao local e credencial cifrada verificadas. Nenhum token foi solicitado e nenhuma API foi chamada.'
    return
}

if (((Test-Path -LiteralPath $configurationPath) -or (Test-Path -LiteralPath $credentialPath)) -and -not $AtualizarCredenciais)
{
    throw 'Ja existe configuracao local. Use -Verificar para conferir ou -AtualizarCredenciais para substituir conscientemente.'
}

$matrixCnpj = (Read-Host 'CNPJ completo da matriz (14 digitos, sem pontuacao)').Trim()
if ($matrixCnpj -notmatch '^\d{14}$')
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

$configuration = [ordered]@{
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

if (-not (Test-Path -LiteralPath $localDirectory))
{
    New-Item -ItemType Directory -Path $localDirectory | Out-Null
}

$accessCredential | Export-Clixml -LiteralPath $credentialPath -Encoding UTF8
$configuration | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath $configurationPath -Encoding UTF8
$accessCredential = $null

Write-Output 'Acesso local configurado. ClientSecret cifrado pelo Windows; arquivos ignorados pelo Git.'
Write-Output 'Geracao de DARF desabilitada. Nenhuma API foi chamada. Confira com -Verificar.'
