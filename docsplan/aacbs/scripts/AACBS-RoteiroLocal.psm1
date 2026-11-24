Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$script:Emitter = 'https://api.receitafederal.gov.br/token'
$script:Hour = 3600000L
$script:Day = 24 * $script:Hour
$script:States = @('AberturaIniciada', 'AberturaAmbigua', 'AberturaRejeitada', 'ComTiquete', 'Pronta', 'LinkIniciado', 'LinkIndisponivel', 'DownloadIniciado', 'DownloadIndisponivel', 'Finalizada', 'ErroFiscal', 'Expirada')
$script:ProtectedTextPrefix = 'aacbs-dpapi-utf8-v1:'
$script:Utf8 = [Text.UTF8Encoding]::new($false, $true)
$script:LoadedDll = $null

function Test-Integer($Value, [long]$Min, [long]$Max) {
    return ($Value -is [int] -or $Value -is [long]) -and $Value -ge $Min -and $Value -le $Max
}

function Assert-LocalMetadata($Metadata, $Credential) {
    $fields = @('cnpjMatriz', 'ambiente', 'habilitarTestesReais', 'permitirProducao', 'permitirGeracaoDarf', 'limiteAberturasPorApiPorDia', 'intervaloConsultaSegundos', 'maximoConsultasSituacao', 'tempoMaximoAcompanhamentoMinutos', 'cenariosDarf')
    if ($null -eq $Metadata -or $Metadata -isnot [pscustomobject] -or
        @($Metadata.PSObject.Properties.Name).Count -ne $fields.Count -or
        @($Metadata.PSObject.Properties.Name | Where-Object { $_ -cnotin $fields }).Count -ne 0 -or
        $Metadata.cnpjMatriz -isnot [string] -or $Metadata.cnpjMatriz -cnotmatch '\A[0-9]{14}\z' -or
        $Metadata.ambiente -isnot [string] -or $Metadata.ambiente -cnotin @('ProducaoRestrita', 'ProducaoBeta') -or
        $Metadata.habilitarTestesReais -isnot [bool] -or $Metadata.permitirProducao -isnot [bool] -or
        $Metadata.permitirProducao -ne ($Metadata.ambiente -ceq 'ProducaoBeta') -or
        $Metadata.permitirGeracaoDarf -isnot [bool] -or $Metadata.permitirGeracaoDarf -or
        $Metadata.cenariosDarf -isnot [array] -or $Metadata.cenariosDarf.Count -ne 0 -or
        -not (Test-Integer $Metadata.limiteAberturasPorApiPorDia 1 1) -or
        -not (Test-Integer $Metadata.intervaloConsultaSegundos 120 120) -or
        -not (Test-Integer $Metadata.maximoConsultasSituacao 1 120) -or
        -not (Test-Integer $Metadata.tempoMaximoAcompanhamentoMinutos 1 240) -or
        $Credential -isnot [pscredential] -or [string]::IsNullOrWhiteSpace($Credential.UserName) -or
        $Credential.Password.Length -eq 0) {
        throw 'Metadados ou credencial AACBS invalidos para a etapa AACBS-003.'
    }
}

function Assert-RealEnvironment($Metadata) {
    if (-not $Metadata.habilitarTestesReais) {
        throw 'Os testes reais AACBS estao desabilitados na configuracao local.'
    }
    if ($Metadata.ambiente -ceq 'ProducaoRestrita') {
        throw 'Producao Restrita bloqueada. As rotas oficiais AACBS e o emissor aplicavel precisam ser qualificados antes de qualquer request.'
    }
    if ($Metadata.ambiente -cne 'ProducaoBeta' -or -not $Metadata.permitirProducao) {
        throw 'Producao exige selecao e habilitacao explicitas na configuracao local.'
    }
}

function Get-LocalDirectory([string]$Repository) {
    $root = (& git -C $Repository rev-parse --show-toplevel 2>$null)
    if ($LASTEXITCODE -ne 0 -or $root -isnot [string]) { throw 'Repositorio local AACBS invalido.' }
    foreach ($path in @('.local/aacbs/', '.local/aacbs/configuracao.local.json', '.local/aacbs/credenciais.local.clixml', '.local/aacbs/execucoes/verificacao.dpapi')) {
        & git -C $root check-ignore --quiet -- $path 2>$null
        if ($LASTEXITCODE -ne 0) { throw 'Os arquivos operacionais AACBS precisam estar ignorados pelo Git.' }
    }
    $tracked = @(& git -C $root ls-files -- '.local/aacbs/' 2>$null)
    if ($LASTEXITCODE -ne 0 -or $tracked.Count -ne 0) { throw 'A pasta operacional AACBS precisa estar fora do versionamento.' }
    return Join-Path $root '.local/aacbs'
}

function Assert-OrdinaryPath([string]$Path) {
    if ((Test-Path -LiteralPath $Path) -and
        ((Get-Item -LiteralPath $Path -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
        throw 'O acesso local AACBS exige arquivos e diretorios fisicos.'
    }
}

function Read-LocalAccess([string]$Directory) {
    try {
        $metadataPath = Join-Path $Directory 'configuracao.local.json'
        $credentialPath = Join-Path $Directory 'credenciais.local.clixml'
        foreach ($path in @((Split-Path -Parent $Directory), $Directory, $metadataPath, $credentialPath)) {
            Assert-OrdinaryPath $path
        }
        $json = [IO.File]::ReadAllText($metadataPath)
        $options = [System.Text.Json.JsonDocumentOptions]::new()
        $options.AllowDuplicateProperties = $false
        $document = [System.Text.Json.JsonDocument]::Parse($json, $options)
        try {
            if ($document.RootElement.ValueKind -ne [System.Text.Json.JsonValueKind]::Object) {
                throw 'Estrutura local invalida.'
            }
            $metadata = ConvertFrom-Json -InputObject $json -DateKind String
        }
        finally { $document.Dispose() }
        $credential = Import-Clixml -LiteralPath $credentialPath
        Assert-LocalMetadata $metadata $credential
        return @{ Metadata = $metadata; Credential = $credential }
    }
    catch {
        throw 'Falha ao verificar o acesso AACBS. Confira a configuracao local e a conta Windows que preparou a credencial.'
    }
}

function Get-Identity([string[]]$Values) {
    $bytes = [Text.Encoding]::UTF8.GetBytes((ConvertTo-Json -InputObject $Values -Compress))
    $sha = [Security.Cryptography.SHA256]::Create()
    try { return [BitConverter]::ToString($sha.ComputeHash($bytes)).Replace('-', '').ToLowerInvariant() }
    finally {
        [Array]::Clear($bytes, 0, $bytes.Length)
        $sha.Dispose()
    }
}

function Get-Now { return [DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds() }

function New-LocalState {
    return [pscustomobject]@{
        Version = 1; AuthAttempts = @(); AuthBlockedUntil = 0L; Tokens = @(); Runs = @()
    }
}

function Replace-ProtectedFile([string]$Temporary, [string]$Path,
    [scriptblock]$Replace = { param($source, $destination) [IO.File]::Replace($source, $destination, [NullString]::Value) }) {
    for ($attempt = 0; $attempt -lt 5; $attempt++) {
        try { & $Replace $Temporary $Path; return }
        catch {
            $failure = $_.Exception
            while ($failure -isnot [IO.IOException] -and $null -ne $failure.InnerException) { $failure = $failure.InnerException }
            $code = $failure.HResult -band 0xffff
            # Bloqueios transitórios do Windows não exigem repetir nenhuma operação remota.
            if ($failure -isnot [IO.IOException] -or $code -notin @(32, 33, 1175) -or
                $attempt -eq 4 -or -not [IO.File]::Exists($Temporary)) { throw }
            Start-Sleep -Milliseconds 50
        }
    }
}

function Write-ProtectedText([string]$Path, [string]$Text) {
    Assert-OrdinaryPath $Path
    $temporary = $Path + '.' + [Guid]::NewGuid().ToString('N') + '.tmp'
    $clear = $script:Utf8.GetBytes($Text)
    try {
        $cipher = [Security.Cryptography.ProtectedData]::Protect($clear, $null, [Security.Cryptography.DataProtectionScope]::CurrentUser)
        $bytes = [Text.Encoding]::ASCII.GetBytes($script:ProtectedTextPrefix + [Convert]::ToBase64String($cipher))
        $stream = [IO.File]::Open($temporary, [IO.FileMode]::CreateNew, [IO.FileAccess]::Write, [IO.FileShare]::None)
        try {
            $stream.Write($bytes, 0, $bytes.Length)
            $stream.Flush($true)
        }
        finally { $stream.Dispose() }
        if ([IO.File]::Exists($Path)) { Replace-ProtectedFile $temporary $Path }
        else { [IO.File]::Move($temporary, $Path) }
    }
    finally {
        [Array]::Clear($clear, 0, $clear.Length)
        [IO.File]::Delete($temporary)
    }
}

function Read-ProtectedText([string]$Path) {
    Assert-OrdinaryPath $Path
    $encoded = [IO.File]::ReadAllText($Path)
    if ($encoded.StartsWith($script:ProtectedTextPrefix, [StringComparison]::Ordinal)) {
        $cipher = [Convert]::FromBase64String($encoded.Substring($script:ProtectedTextPrefix.Length))
        $encoding = $script:Utf8
    }
    else {
        # O formato anterior usava ConvertFrom-SecureString com DPAPI e hex.
        $cipher = [Convert]::FromHexString($encoded)
        $encoding = [Text.UnicodeEncoding]::new($false, $false, $true)
    }
    $clear = [Security.Cryptography.ProtectedData]::Unprotect($cipher, $null, [Security.Cryptography.DataProtectionScope]::CurrentUser)
    try { return $encoding.GetString($clear) }
    finally { [Array]::Clear($clear, 0, $clear.Length) }
}

function Save-LocalState($Context) {
    Write-ProtectedText $Context.Path (ConvertTo-Json -InputObject $Context.State -Depth 6 -Compress)
}

function Read-LocalState([string]$Path) {
    Assert-OrdinaryPath $Path
    if (-not (Test-Path -LiteralPath $Path)) { return New-LocalState }
    $state = ConvertFrom-Json -InputObject (Read-ProtectedText $Path) -DateKind String
    # A integridade DPAPI e verificada na leitura. Um formato desconhecido bloqueia
    # a retomada e nao pode zerar silenciosamente os contadores de tentativas.
    $now = Get-Now
    if (-not (Test-Integer $state.Version 1 1) -or $state.AuthAttempts -isnot [array] -or $state.AuthAttempts.Count -gt 2 -or
        $state.Tokens -isnot [array] -or $state.Runs -isnot [array] -or
        -not (Test-Integer $state.AuthBlockedUntil 0 ($now + $script:Hour)) -or
        @($state.AuthAttempts | Where-Object { -not (Test-Integer $_ 1 $now) }).Count -gt 0) {
        throw 'Estado local AACBS invalido ou relogio alterado.'
    }
    foreach ($token in $state.Tokens) {
        if ($token.Key -cnotmatch '\A[0-9a-f]{64}\z' -or $token.Value -isnot [string] -or [string]::IsNullOrWhiteSpace($token.Value) -or
            -not (Test-Integer $token.UsableUntil 1 ($now + $script:Hour))) {
            throw 'Cache de autenticacao AACBS invalido.'
        }
    }
    foreach ($run in $state.Runs) {
        if ($run.Id -cnotmatch '\A[0-9a-f]{32}\z' -or $run.Scope -cnotmatch '\A[0-9a-f]{64}\z' -or
            $run.Credential -cnotmatch '\A[0-9a-f]{64}\z' -or $run.Stage -cne 'AACBS-003' -or
            $run.Api -cne 'Debitos' -or $run.Environment -cne 'ProducaoBeta' -or
            $run.Phase -cnotin $script:States -or -not (Test-Integer $run.Opened 1 $now) -or
            -not (Test-Integer $run.Polls 0 120) -or -not (Test-Integer $run.LastPoll 0 $now) -or
            -not (Test-Integer $run.LinkAt 0 $now) -or -not (Test-Integer $run.DownloadAt 0 $now) -or
            -not (Test-Integer $run.LastHttp 0 599) -or
            $run.Ticket -isnot [string] -or
            ($run.Phase -notin @('AberturaIniciada', 'AberturaAmbigua', 'AberturaRejeitada') -and $run.Ticket.Length -eq 0)) {
            throw 'Journal AACBS invalido.'
        }
    }
    return $state
}

function Write-Observation($Context, [string]$Operation, [string]$Status, [int]$Http = 0) {
    $when = [DateTimeOffset]::UtcNow.ToString('o')
    $origin = if ($Context.ContainsKey('Synthetic')) { ' origem=simulada' } else { '' }
    $correlation = if ($Context.ContainsKey('Correlation')) { " correlacao=$($Context.Correlation)" } else { '' }
    $response = if ($Http -gt 0) { " HTTP=$Http" } else { '' }
    Write-Information "$when AACBS-003 ambiente=$($Context.Metadata.ambiente) operacao=$Operation estado=$Status$response$origin$correlation" -InformationAction Continue
}

function Get-LocalToken($Context, [long]$Now, [scriptblock]$Authenticate) {
    $state = $Context.State
    $state.Tokens = @($state.Tokens | Where-Object { $_.UsableUntil -gt $Now })
    $cached = @($state.Tokens | Where-Object { $_.Key -ceq $Context.TokenScope })
    if ($cached.Count -eq 1) { return $cached[0].Value }
    if ($cached.Count -gt 1) { throw 'Cache de autenticacao AACBS inconsistente.' }

    $state.AuthAttempts = @($state.AuthAttempts | Where-Object { $_ -gt ($Now - $script:Hour) })
    if ($state.AuthBlockedUntil -gt $Now -or $state.AuthAttempts.Count -ge 2) {
        throw 'Orcamento local de autenticacao AACBS esgotado. Aguarde a janela movel.'
    }
    $state.AuthAttempts += $Now
    Save-LocalState $Context

    $issued = & $Authenticate $Context
    if ($issued.Http -eq 429) {
        $state.AuthBlockedUntil = $Now + $script:Hour
        Save-LocalState $Context
    }
    if ($issued.Http -ne 200 -or $null -eq $issued.Result -or
        [string]::IsNullOrWhiteSpace($issued.Result.AccessToken) -or $issued.Result.TokenType -ine 'Bearer') {
        throw 'A autenticacao AACBS nao forneceu token utilizavel.'
    }
    $seconds = 3600
    if ($null -ne $issued.Result.ExpiresIn) { $seconds = [Math]::Min(3600, [int]$issued.Result.ExpiresIn) }
    if ($seconds -le 0) { throw 'O token AACBS foi emitido sem prazo utilizavel.' }
    $usableUntil = $Now + ([long]$seconds * 1000) - [long][Math]::Min(60000, ($seconds * 500))
    if ($usableUntil -le (Get-Now)) { throw 'O token AACBS expirou durante a autenticacao.' }
    $state.Tokens += [pscustomobject]@{ Key = $Context.TokenScope; Value = $issued.Result.AccessToken; UsableUntil = $usableUntil }
    Save-LocalState $Context
    return $issued.Result.AccessToken
}

function Remove-LocalToken($Context, [string]$Token) {
    $Context.State.Tokens = @($Context.State.Tokens | Where-Object { $_.Key -cne $Context.TokenScope -or $_.Value -cne $Token })
    Save-LocalState $Context
}

function Get-Run($Context, [bool]$NewRun, [long]$Now) {
    $runs = @($Context.State.Runs | Where-Object { $_.Scope -ceq $Context.RunScope })
    if ($runs.Count -gt 0) {
        $run = $runs[-1]
        $Context.Correlation = $run.Id
        if ($run.Phase -eq 'AberturaIniciada') {
            $run.Phase = 'AberturaAmbigua'
            Save-LocalState $Context
        }
        if ($run.Phase -eq 'AberturaAmbigua') { throw 'Abertura de debitos sem resultado confirmado. Reconcilie a solicitacao antes de prosseguir.' }
        if (-not $NewRun) { return $run }
        if ($run.Phase -notin @('AberturaRejeitada', 'Finalizada', 'ErroFiscal', 'Expirada')) {
            throw 'Existe uma solicitacao de debitos que precisa ser retomada ou reconciliada.'
        }
    }
    # Janela movel para nao presumir o fuso adotado pela Receita. O limite e por
    # credencial e API, inclusive para outra empresa representada.
    $recent = @($Context.State.Runs | Where-Object { $_.Api -eq 'Debitos' -and $_.Opened -gt ($Now - $script:Day) })
    if ($recent.Count -ge $Context.Metadata.limiteAberturasPorApiPorDia) {
        throw 'Orcamento local diario de abertura de debitos esgotado.'
    }
    return $null
}

function Start-Run($Context, [long]$Now) {
    $run = [pscustomobject]@{
        Id = [Guid]::NewGuid().ToString('N'); Scope = $Context.RunScope; Credential = $Context.TokenScope
        Stage = 'AACBS-003'; Api = 'Debitos'; Environment = $Context.Metadata.ambiente
        Phase = 'AberturaIniciada'; Opened = $Now; LastPoll = 0L; Polls = 0; Ticket = ''
        LinkAt = 0L; DownloadAt = 0L; LastHttp = 0
    }
    $Context.State.Runs += $run
    $Context.Correlation = $run.Id
    Save-LocalState $Context
    return $run
}

function Get-PollDelay($Metadata, $Run, [long]$Now) {
    $deadline = $Run.Opened + ($Metadata.tempoMaximoAcompanhamentoMinutos * 60000L)
    $last = [Math]::Max($Run.LastPoll, $Run.Opened)
    $next = [Math]::Max($Now, $last + ($Metadata.intervaloConsultaSegundos * 1000L))
    if ($Run.Polls -ge $Metadata.maximoConsultasSituacao -or $next -ge $deadline) { return -1L }
    return $next - $Now
}

function Import-AACBSAssembly([string]$Path) {
    try {
        $dll = Get-Item -LiteralPath $Path
        if ($dll.Name -cne 'Unimake.Business.DFe.dll') { throw 'DLL invalida.' }
        if ($null -ne $script:LoadedDll) {
            if ($script:LoadedDll.Path -ine $dll.FullName) { throw 'Outra DLL ja carregada.' }
            return $script:LoadedDll
        }

        if (-not ('Unimake.AACBS.LocalAssemblyLoadContext' -as [type])) {
            Add-Type -TypeDefinition @'
using System.IO;
using System.Reflection;
using System.Runtime.Loader;

namespace Unimake.AACBS
{
    public sealed class LocalAssemblyLoadContext : AssemblyLoadContext
    {
        private readonly string directory;

        public LocalAssemblyLoadContext(string directory) : base("Unimake.AACBS.Local", isCollectible: false)
        {
            this.directory = directory;
        }

        protected override Assembly Load(AssemblyName name)
        {
            var candidate = Path.Combine(directory, name.Name + ".dll");
            return File.Exists(candidate) ? LoadFromAssemblyPath(candidate) : null;
        }
    }
}
'@
        }

        # As dependencias do build ficam isoladas das versoes usadas pelo PowerShell.
        $loadContext = [Unimake.AACBS.LocalAssemblyLoadContext]::new($dll.DirectoryName)
        $assembly = $loadContext.LoadFromAssemblyPath($dll.FullName)
        $types = @{}
        $configurationType = $assembly.GetType('Unimake.Business.DFe.Servicos.Configuracao', $true)
        if ($null -eq $configurationType.GetProperty('AACBSToken')) { throw 'Contrato AACBS ausente.' }
        foreach ($operation in @('Autenticar', 'ConsultarDebitos', 'ConsultarSituacaoSolicitacao', 'ConsultarLinkDownload', 'BaixarArquivoRetorno')) {
            $requestType = $assembly.GetType("Unimake.Business.DFe.Xml.AACBS.$operation", $true)
            $serviceType = $assembly.GetType("Unimake.Business.DFe.Servicos.AACBS.$operation", $true)
            if ($null -eq $serviceType.GetConstructor([type[]]@($requestType, $configurationType)) -or
                $null -eq $serviceType.GetMethod('Executar', [type[]]@()) -or
                $null -eq $serviceType.GetProperty('Result')) {
                throw 'Contrato de servico AACBS ausente.'
            }
            $types.Add($operation, @{ Request = $requestType; Service = $serviceType })
        }
        $script:LoadedDll = @{ Types = $types; Configuration = $configurationType; Context = $loadContext; Path = $dll.FullName }
        return $script:LoadedDll
    }
    catch {
        throw 'Falha ao carregar a DLL AACBS e suas dependencias. Use a saida completa do build da etapa em um novo processo PowerShell.'
    }
}

function Invoke-DllCall($Context, [string]$Operation, [hashtable]$Fields, [string]$Token = '') {
    $service = $null
    try {
        $configuration = [Activator]::CreateInstance($Context.Dll.Configuration)
        $configuration.TipoAmbiente = 'Producao'
        $configuration.HasProxy = $false
        $configuration.UsaCertificadoDigital = $false
        if ($Operation -eq 'Autenticar') {
            $configuration.ClientID = $Context.Credential.UserName
            $configuration.ClientSecret = $Context.Credential.GetNetworkCredential().Password
        }
        elseif ($Token.Length -gt 0) { $configuration.AACBSToken = $Token }

        $types = $Context.Dll.Types[$Operation]
        $request = [Activator]::CreateInstance($types.Request)
        foreach ($name in $Fields.Keys) { $request.$name = $Fields[$name] }
        $service = [Activator]::CreateInstance($types.Service, [object[]]@($request, $configuration))
        $service.Executar()
        $http = [int]$service.HttpStatusCode
        $result = $service.Result
        Write-Observation $Context $Operation 'RespostaInterpretada' $http
        $xml = if ($Operation -eq 'BaixarArquivoRetorno') { $service.RetornoWSString } else { $null }
        return @{ Http = $http; Result = $result; Xml = $xml }
    }
    catch {
        $http = if ($null -ne $service) { [int]$service.HttpStatusCode } else { 0 }
        Write-Observation $Context $Operation 'FalhaTecnica' $http
        return @{ Http = $http; Result = $null }
    }
    finally {
        # A configuracao e descartada ao final de cada chamada explicita.
        $configuration = $null
        $service = $null
    }
}

function Get-ExecutionToken($Context) {
    return Get-LocalToken $Context (Get-Now) { param($context) & $context.Dispatch $context 'Autenticar' @{} '' }
}

function Test-AuthorizationFailure($Context, $Response, [string]$Token) {
    if ($Response.Http -eq 401) { Remove-LocalToken $Context $Token }
    return $Response.Http -in @(401, 403, 429)
}

function Assert-DownloadWindow($Context, $Run, [long]$Now) {
    # Sem webhook, o inicio da abertura e a referencia conservadora para as 48h.
    if ($Now -ge ($Run.Opened + 2 * $script:Day)) {
        $Run.Phase = 'Expirada'
        Save-LocalState $Context
        throw 'A janela local para recuperar o arquivo de debitos expirou.'
    }
}

function Invoke-Debits($Context, [bool]$NewRun, [bool]$KeepFinalXml) {
    $run = Get-Run $Context $NewRun (Get-Now)
    if ($null -eq $run) {
        $token = Get-ExecutionToken $Context
        $run = Start-Run $Context (Get-Now)
        $opened = & $Context.Dispatch $Context 'ConsultarDebitos' @{ Cnpj = $Context.Cnpj } $token
        $run.LastHttp = $opened.Http
        if ($opened.Http -eq 201 -and $null -ne $opened.Result -and $opened.Result.TiqueteSolicitacao) {
            $run.Ticket = $opened.Result.TiqueteSolicitacao
            $run.Phase = 'ComTiquete'
        }
        elseif ($opened.Http -in @(400, 401, 403, 404, 422, 429) -and $null -ne $opened.Result -and $opened.Result.CodigoErro) {
            $run.Phase = 'AberturaRejeitada'
        }
        else { $run.Phase = 'AberturaAmbigua' }
        Save-LocalState $Context
        $rejected = Test-AuthorizationFailure $Context $opened $token
        if ($rejected -or $run.Phase -ne 'ComTiquete') {
            Write-Observation $Context 'ConsultarDebitos' $run.Phase $opened.Http
            throw 'Abertura de debitos interrompida. O resultado e o orcamento permanecem no journal cifrado.'
        }
    }

    while ($run.Phase -eq 'ComTiquete') {
        $delay = Get-PollDelay $Context.Metadata $run (Get-Now)
        if ($delay -lt 0) { throw 'Limite local de acompanhamento atingido. O tiquete permanece no journal cifrado.' }
        if ($delay -gt 0) { Start-Sleep -Milliseconds ([int]$delay) }
        $token = Get-ExecutionToken $Context
        $now = Get-Now
        if ((Get-PollDelay $Context.Metadata $run $now) -ne 0) {
            throw 'Janela da consulta de situacao indisponivel. O tiquete permanece no journal cifrado.'
        }
        $run.Polls += 1
        $run.LastPoll = $now
        $run.LastHttp = 0
        Save-LocalState $Context
        $status = & $Context.Dispatch $Context 'ConsultarSituacaoSolicitacao' @{ TiqueteSolicitacao = $run.Ticket } $token
        $run.LastHttp = $status.Http
        Save-LocalState $Context
        if ((Test-AuthorizationFailure $Context $status $token) -or $null -eq $status.Result) {
            throw 'Consulta de situacao interrompida. O tiquete permanece no journal cifrado.'
        }
        $situation = [string]$status.Result.Situacao
        if ($situation -in @('Erro', 'Expirada')) {
            $run.Phase = if ($situation -eq 'Expirada') { 'Expirada' } else { 'ErroFiscal' }
            Save-LocalState $Context
            Write-Observation $Context 'ConsultarSituacaoSolicitacao' $run.Phase $status.Http
            throw 'A Receita encerrou a solicitacao de debitos. O estado permanece no journal cifrado.'
        }
        if ($status.Http -ne 200 -or $situation -cnotin @('Pendente', 'EmProcessamento', 'Concluida')) {
            throw 'Consulta de situacao sem resultado utilizavel. O tiquete permanece no journal cifrado.'
        }
        if ($situation -eq 'Concluida') {
            $run.Phase = 'Pronta'
            Save-LocalState $Context
        }
        Write-Observation $Context 'ConsultarSituacaoSolicitacao' $situation $status.Http
    }

    if ($run.Phase -eq 'Finalizada') {
        Write-Observation $Context 'ConsultarDebitos' 'JaFinalizada'
        return
    }
    if ($run.Phase -ne 'Pronta') {
        Write-Observation $Context 'ConsultarDebitos' $run.Phase
        throw 'A solicitacao existente exige reconciliacao ou decisao explicita do executor.'
    }

    Assert-DownloadWindow $Context $run (Get-Now)
    $token = Get-ExecutionToken $Context
    $now = Get-Now
    Assert-DownloadWindow $Context $run $now
    $run.Phase = 'LinkIniciado'
    $run.LinkAt = $now
    $run.LastHttp = 0
    Save-LocalState $Context
    $link = & $Context.Dispatch $Context 'ConsultarLinkDownload' @{ TiqueteSolicitacao = $run.Ticket } $token
    $run.LastHttp = $link.Http
    $authorizationFailure = Test-AuthorizationFailure $Context $link $token
    if ($authorizationFailure -or $link.Http -ne 200 -or $null -eq $link.Result -or
        [string]::IsNullOrWhiteSpace($link.Result.UrlAssinada)) {
        $run.Phase = if ($link.Http -in @(401, 403, 429)) { 'Pronta' } else { 'LinkIndisponivel' }
        Save-LocalState $Context
        throw 'Link de debitos indisponivel. A recuperacao exige revisar a operacao registrada.'
    }

    try {
        $expiration = $link.Result.UrlAssinadaExpiraEm
        if ((Get-Now) -ge ($run.Opened + 2 * $script:Day) -or
            ($null -ne $expiration -and $expiration -le [DateTimeOffset]::UtcNow)) {
            $run.Phase = 'Expirada'
            Save-LocalState $Context
            throw 'O link do arquivo de debitos expirou.'
        }
        $run.Phase = 'DownloadIniciado'
        $run.DownloadAt = Get-Now
        $run.LastHttp = 0
        Save-LocalState $Context
        $downloaded = & $Context.Dispatch $Context 'BaixarArquivoRetorno' @{
            TipoConsulta = 'Debitos'; Cnpj = $Context.Cnpj; TiqueteSolicitacao = $run.Ticket
            UrlAssinada = $link.Result.UrlAssinada; UrlAssinadaExpiraEmField = $link.Result.UrlAssinadaExpiraEmField
        } ''
    }
    finally { $link = $null }
    $run.LastHttp = $downloaded.Http
    if ($downloaded.Http -ne 200 -or $null -eq $downloaded.Result -or $null -eq $downloaded.Result.ArquivoDebitos) {
        $run.Phase = 'DownloadIndisponivel'
        Save-LocalState $Context
        throw 'Download de debitos sem arquivo valido. O link foi descartado.'
    }
    if ($KeepFinalXml) {
        $finalPath = Join-Path (Split-Path -Parent $Context.Path) ("arquivo-debitos-" + $run.Id + '.dpapi')
        if ([string]::IsNullOrEmpty($downloaded.Xml)) { throw 'O arquivo validado nao tem XML para gravacao local.' }
        Write-ProtectedText $finalPath $downloaded.Xml
    }
    $run.Phase = 'Finalizada'
    Save-LocalState $Context
    Write-Observation $Context 'BaixarArquivoRetorno' 'Finalizada' $downloaded.Http
}

function Invoke-AACBSLocal([string]$Repository, [ValidateSet('Verificar', 'Autenticar', 'Debitos')][string]$Action, [string]$Dll, [bool]$NewRun, [bool]$KeepFinalXml) {
    if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) { throw 'O roteiro AACBS exige Windows para usar DPAPI.' }
    $local = Get-LocalDirectory $Repository
    $access = Read-LocalAccess $local
    if ($Action -eq 'Verificar') {
        Write-Observation $access 'AcessoLocal' 'Verificado'
        if ($access.Metadata.ambiente -eq 'ProducaoRestrita') {
            Write-Observation $access 'AcessoReal' 'RotaOficialPendente'
        }
        return
    }
    Assert-RealEnvironment $access.Metadata
    $assembly = Import-AACBSAssembly $Dll
    $access.Cnpj = $access.Metadata.cnpjMatriz.Substring(0, 8)
    $access.Dll = $assembly
    $access.Dispatch = { param($context, $operation, $fields, $token) Invoke-DllCall $context $operation $fields $token }

    try {
        $credentialId = $access.Credential.UserName
        $quota = Get-Identity @($script:Emitter, $credentialId)
        $access.TokenScope = Get-Identity @($script:Emitter, $access.Metadata.ambiente, $credentialId, $access.Credential.GetNetworkCredential().Password)
        $access.RunScope = Get-Identity @($quota, $access.Metadata.ambiente, 'Debitos', $access.Cnpj)
        $sid = [Security.Principal.WindowsIdentity]::GetCurrent().User.Value
        $mutex = [Threading.Mutex]::new($false, ('Global\UnimakeAACBS-' + (Get-Identity @($sid, $quota))))
        $locked = $false
        try {
            try { $locked = $mutex.WaitOne(0) }
            catch [Threading.AbandonedMutexException] { $locked = $true }
            if (-not $locked) { throw 'Ja existe um executor AACBS para esta credencial.' }

            $directory = Join-Path $local 'execucoes'
            Assert-OrdinaryPath $directory
            [void][IO.Directory]::CreateDirectory($directory)
            $access.Path = Join-Path $directory ("execucao-" + $quota + '.dpapi')
            $access.State = Read-LocalState $access.Path
            if ($Action -eq 'Autenticar') {
                $null = Get-ExecutionToken $access
                Write-Observation $access 'Autenticar' 'TokenDisponivel'
                return
            }
            Invoke-Debits $access $NewRun $KeepFinalXml
        }
        finally {
            if ($locked) { $mutex.ReleaseMutex() }
            $mutex.Dispose()
        }
    }
    catch {
        # Somente mensagens fixas emitidas pelo roteiro atravessam a fronteira.
        if ($_.Exception -is [System.Management.Automation.RuntimeException] -and
            $_.FullyQualifiedErrorId -ceq $_.Exception.Message -and
            $_.ScriptStackTrace -match 'AACBS-RoteiroLocal\.psm1') {
            throw $_.Exception.Message
        }
        throw 'Falha local no roteiro AACBS. Confira a DLL, o estado cifrado e as observacoes sanitizadas.'
    }
}

. (Join-Path $PSScriptRoot 'Testar-RoteiroAACBS.ps1')
Export-ModuleMember -Function Invoke-AACBSLocal, Invoke-AACBSOffline
