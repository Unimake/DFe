function Assert-OfflineCondition([bool]$Condition, [string]$Case) {
    if (-not $Condition) { throw "Falha no cenario offline AACBS $Case." }
}

function Assert-OfflineFailure([scriptblock]$Action, [string]$Message, [string]$Case) {
    try { $null = & $Action }
    catch {
        if ($_.Exception.Message -notlike $Message) {
            throw "Falha no cenario offline AACBS $Case. Esperado: $Message Obtido: $($_.Exception.Message)"
        }
        return
    }
    throw "Falha no cenario offline AACBS $Case. A operacao deveria ser recusada."
}

function Test-OfflineDll([string]$Dll) {
    $hostJson = [System.Text.Json.JsonDocument].Assembly
    $loaded = Import-AACBSAssembly $Dll
    try {
        $directory = Split-Path -Parent $loaded.Path
        $testPath = Join-Path $directory 'Unimake.DFe.Test.dll'
        $testAssembly = $loaded.Context.LoadFromAssemblyPath($testPath)
        $testType = $testAssembly.GetType('Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest', $true)
        $test = [Activator]::CreateInstance($testType)
        $method = $testType.GetMethod('ExecutarCicloExplicitoValidaContratosEIsolaDownload', [type[]]@([bool], [bool]))
        [void]$method.Invoke($test, [object[]]@($false, $false))

        $queryType = $testAssembly.GetType('Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoConsultasTest', $true)
        $queryTest = [Activator]::CreateInstance($queryType)
        $queryMethod = $queryType.GetMethod('ExecutarCicloPreservaContratoEContextoDaFamilia')
        $familyType = $queryMethod.GetParameters()[0].ParameterType
        foreach ($family in @('Creditos', 'Pagamentos', 'Recolhimentos')) {
            $value = [Enum]::Parse($familyType, $family)
            $webhook = if ($family -ceq 'Creditos') { 'https://example.com/a%2Fb?teste=a%2Bb&v=2' } else { '' }
            $estimate = switch ($family) { 'Creditos' { -1 } 'Pagamentos' { 0 } 'Recolhimentos' { 60 } }
            [void]$queryMethod.Invoke($queryTest, [object[]]@($value, $family.ToLowerInvariant(), $webhook, $estimate))
        }

        $localJsonPath = Join-Path $directory 'System.Text.Json.dll'
        if (Test-Path -LiteralPath $localJsonPath) {
            $localJson = @($loaded.Context.Assemblies | Where-Object { $_.GetName().Name -ceq 'System.Text.Json' })
            if ($localJson.Count -ne 1 -or $localJson[0].Location -ine $localJsonPath -or
                [object]::ReferenceEquals($hostJson, $localJson[0]) -or
                -not [object]::ReferenceEquals($hostJson, [System.Text.Json.JsonDocument].Assembly)) {
                throw 'As dependencias JSON da DLL e do PowerShell precisam permanecer isoladas.'
            }
            Write-Output ('System.Text.Json do PowerShell {0}; System.Text.Json da DLL {1}.' -f $hostJson.GetName().Version, $localJson[0].GetName().Version)
        }
    }
    catch {
        throw 'A prova offline AACBS na DLL falhou. Confira a saida completa do build dos testes e suas dependencias.'
    }
    Write-Output 'A DLL executou o ciclo AACBS sintetico no mesmo contexto isolado do roteiro, com transporte controlado.'
}

function Invoke-AACBSOffline([string]$Dll) {
    if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) { throw 'A prova offline AACBS de persistencia exige Windows com DPAPI.' }
    $entry = Join-Path $PSScriptRoot 'Executar-AACBS003.ps1'
    $missingRepository = Join-Path ([IO.Path]::GetTempPath()) ('aacbs-inexistente-' + [Guid]::NewGuid().ToString('N'))
    foreach ($mode in @('Autenticar', 'Debitos')) {
        $arguments = @{ Dll = 'dll-sintetica-inexistente.dll'; RepositorioLocal = $missingRepository }
        $arguments[$mode] = $false
        Assert-OfflineFailure { & $entry @arguments } 'O modo AACBS selecionado exige switch com valor verdadeiro.' ('switch-falso-splat-' + $mode)
    }
    Assert-OfflineFailure { & $entry -Autenticar:$false -Dll 'dll-sintetica-inexistente.dll' -RepositorioLocal $missingRepository } 'O modo AACBS selecionado exige switch com valor verdadeiro.' 'switch-autenticar-falso-direto'
    Assert-OfflineFailure { & $entry -Debitos:$false -Dll 'dll-sintetica-inexistente.dll' -RepositorioLocal $missingRepository } 'O modo AACBS selecionado exige switch com valor verdadeiro.' 'switch-debitos-falso-direto'

    $replacementDirectory = Join-Path ([IO.Path]::GetTempPath()) ('aacbs-replace-' + [Guid]::NewGuid().ToString('N'))
    [void][IO.Directory]::CreateDirectory($replacementDirectory)
    $replacementSource = Join-Path $replacementDirectory 'novo.tmp'
    $replacementTarget = Join-Path $replacementDirectory 'anterior.dpapi'
    try {
        foreach ($case in @(
            @{ Until = 1; Code = -2147023721; Attempts = 2; Success = $true; Missing = $false },
            @{ Until = 99; Code = -2147023721; Attempts = 5; Success = $false; Missing = $false },
            @{ Until = 99; Code = -2147024891; Attempts = 1; Success = $false; Missing = $false },
            @{ Until = 99; Code = -2147023721; Attempts = 1; Success = $false; Missing = $true }
        )) {
            [IO.File]::WriteAllText($replacementSource, 'novo-sintetico')
            [IO.File]::WriteAllText($replacementTarget, 'anterior-sintetico')
            $control = @{ Count = 0; Until = $case.Until; Code = $case.Code; Missing = $case.Missing }
            $replace = {
                param($source, $destination)
                $control.Count++
                if ($control.Missing) { [IO.File]::Delete($source) }
                if ($control.Count -le $control.Until) { throw [IO.IOException]::new('Bloqueio sintetico de arquivo.', $control.Code) }
                [IO.File]::WriteAllText($destination, [IO.File]::ReadAllText($source))
                [IO.File]::Delete($source)
            }.GetNewClosure()
            if ($case.Success) { Replace-ProtectedFile $replacementSource $replacementTarget $replace }
            else { Assert-OfflineFailure { Replace-ProtectedFile $replacementSource $replacementTarget $replace } '*Bloqueio sintetico de arquivo.*' 'replace-sem-loop-ilimitado' }
            Assert-OfflineCondition ($control.Count -eq $case.Attempts) 'replace-tentativas-limitadas'
            $expectedText = if ($case.Success) { 'novo-sintetico' } else { 'anterior-sintetico' }
            Assert-OfflineCondition ([IO.File]::ReadAllText($replacementTarget) -ceq $expectedText) 'replace-preserva-destino'
        }
    }
    finally {
        foreach ($file in [IO.Directory]::GetFiles($replacementDirectory)) { [IO.File]::Delete($file) }
        [IO.Directory]::Delete($replacementDirectory)
    }
    $metadata = [pscustomobject]@{
        cnpjMatriz = '00000000000100'; ambiente = 'ProducaoRestrita'; habilitarTestesReais = $true
        permitirProducao = $false; permitirGeracaoDarf = $false; cenariosDarf = @()
        limiteAberturasPorApiPorDia = 1; intervaloConsultaSegundos = 120
        maximoConsultasSituacao = 120; tempoMaximoAcompanhamentoMinutos = 240
    }
    $credential = [pscredential]::new('client-sintetico', (ConvertTo-SecureString 'segredo-sintetico +:%' -AsPlainText -Force))
    Assert-LocalMetadata $metadata $credential
    Assert-OfflineFailure { Assert-RealEnvironment $metadata } 'Producao Restrita bloqueada.*' 'rota-prr'
    foreach ($case in @(
        @{ Field = 'cnpjMatriz'; Value = ('0000000000010' + [char]0x0660) },
        @{ Field = 'ambiente'; Value = 'producaoRestrita' },
        @{ Field = 'habilitarTestesReais'; Value = 'true' },
        @{ Field = 'permitirProducao'; Value = $true },
        @{ Field = 'permitirGeracaoDarf'; Value = $true },
        @{ Field = 'cenariosDarf'; Value = @('sintetico') },
        @{ Field = 'limiteAberturasPorApiPorDia'; Value = 2 },
        @{ Field = 'intervaloConsultaSegundos'; Value = 119 },
        @{ Field = 'maximoConsultasSituacao'; Value = 121 },
        @{ Field = 'tempoMaximoAcompanhamentoMinutos'; Value = 241 }
    )) {
        $field = $case.Field
        $value = $metadata.$field
        $metadata.$field = $case.Value
        Assert-OfflineFailure { Assert-LocalMetadata $metadata $credential } 'Metadados ou credencial*' $field
        $metadata.$field = $value
    }
    $metadata.ambiente = 'ProducaoBeta'
    Assert-OfflineFailure { Assert-LocalMetadata $metadata $credential } 'Metadados ou credencial*' 'producao-sem-permissao'
    $metadata.permitirProducao = $true
    Assert-LocalMetadata $metadata $credential
    Assert-RealEnvironment $metadata
    $metadata.habilitarTestesReais = $false
    Assert-OfflineFailure { Assert-RealEnvironment $metadata } 'Os testes reais AACBS estao desabilitados*' 'testes-desabilitados'
    $metadata.habilitarTestesReais = $true

    $directory = Join-Path ([IO.Path]::GetTempPath()) ('aacbs-offline-' + [Guid]::NewGuid().ToString('N'))
    [void][IO.Directory]::CreateDirectory($directory)
    $context = @{
        Metadata = $metadata; State = New-LocalState; Cnpj = '00000000'; Calls = @(); Synthetic = $true
        Path = Join-Path $directory 'execucao.dpapi'
        TokenScope = Get-Identity @('emissor-sintetico', 'cliente', 'revisao-1')
        RunScope = Get-Identity @('empresa-sintetica', 'Debitos')
    }
    try {
        $metadataPath = Join-Path $directory 'configuracao.local.json'
        $metadataJson = ConvertTo-Json -InputObject $metadata -Compress
        [IO.File]::WriteAllText($metadataPath, $metadataJson)
        Export-Clixml -InputObject $credential -LiteralPath (Join-Path $directory 'credenciais.local.clixml')
        $imported = Read-LocalAccess $directory
        Assert-OfflineCondition ($imported.Credential -is [pscredential]) 'importacao-clixml'
        foreach ($invalid in @(
            @{ Json = '{"permitirProducao":false,' + $metadataJson.Substring(1); Case = 'campo-duplicado' },
            @{ Json = '{"\u0070ermitirProducao":false,' + $metadataJson.Substring(1); Case = 'campo-duplicado-unicode' },
            @{ Json = '[' + $metadataJson + ']'; Case = 'raiz-nao-objeto' }
        )) {
            [IO.File]::WriteAllText($metadataPath, $invalid.Json)
            Assert-OfflineFailure { Read-LocalAccess $directory } 'Falha ao verificar o acesso AACBS.*' $invalid.Case
        }

        $issued = @{ Http = 200; Result = @{ AccessToken = 'token-sintetico'; TokenType = 'Bearer'; ExpiresIn = 3600 } }
        $now = Get-Now
        $null = Get-LocalToken $context $now { param($context) return $issued }
        $cipher = [IO.File]::ReadAllText($context.Path)
        Assert-OfflineCondition (-not $cipher.Contains($issued.Result.AccessToken)) 'cache-cifrado'
        $context.State = Read-LocalState $context.Path
        $token = Get-LocalToken $context $now { throw 'Emissao indevida no teste offline.' }
        Assert-OfflineCondition ($token -ceq $issued.Result.AccessToken) 'reutilizacao-em-reinicio'
        Remove-LocalToken $context 'token-antigo-sintetico'
        Assert-OfflineCondition ($context.State.Tokens.Count -eq 1) '401-do-token-antigo'
        $context.TokenScope = Get-Identity @('emissor-sintetico', 'cliente', 'revisao-2')
        Assert-OfflineFailure { Get-LocalToken $context $now { throw 'Timeout controlado.' } } 'Timeout controlado.' 'segunda-tentativa-reservada'
        $context.State = Read-LocalState $context.Path
        Assert-OfflineFailure { Get-LocalToken $context $now { throw 'Emissao indevida no teste offline.' } } 'Orcamento local de autenticacao AACBS esgotado.*' 'limite-apos-reinicio-e-rotacao'
        $windowLater = $now + $script:Hour
        Assert-OfflineFailure { Get-LocalToken $context $windowLater { return @{ Http = 429; Result = $null } } } 'A autenticacao AACBS nao forneceu token*' 'http-429'
        Assert-OfflineFailure { Get-LocalToken $context $windowLater { throw 'Emissao indevida no teste offline.' } } 'Orcamento local de autenticacao AACBS esgotado.*' 'http-429-bloqueia-emissao'

        $context.State = New-LocalState
        $longToken = 'x' * 70000
        $context.State.Tokens += [pscustomobject]@{ Key = $context.TokenScope; Value = $longToken; UsableUntil = $now + 10000 }
        Save-LocalState $context
        $context.State = Read-LocalState $context.Path
        Assert-OfflineCondition ($context.State.Tokens[0].Value -ceq $longToken) 'journal-acima-64-kib'

        $legacyJson = ConvertTo-Json -InputObject (New-LocalState) -Compress
        $legacy = ConvertTo-SecureString -String $legacyJson -AsPlainText -Force
        try { [IO.File]::WriteAllText($context.Path, (ConvertFrom-SecureString -SecureString $legacy)) }
        finally { $legacy.Dispose() }
        $context.State = Read-LocalState $context.Path
        Assert-OfflineCondition ($context.State.Version -eq 1 -and $context.State.Runs.Count -eq 0) 'leitura-dpapi-legada'

        $context.State = New-LocalState
        $run = Start-Run $context $now
        $context.State = Read-LocalState $context.Path
        Assert-OfflineFailure { Get-Run $context $false $now } 'Abertura de debitos sem resultado confirmado.*' 'post-interrompido'
        $context.State = Read-LocalState $context.Path
        Assert-OfflineFailure { Get-Run $context $true ($now + 2 * $script:Day) } 'Abertura de debitos sem resultado confirmado.*' 'post-ambiguo-apos-virada'
        $context.RunScope = Get-Identity @('outra-empresa-sintetica', 'Debitos')
        Assert-OfflineFailure { Get-Run $context $false $now } 'Orcamento local diario de abertura de debitos esgotado.' 'limite-diario-compartilhado'

        $context.State = New-LocalState
        $run = Start-Run $context $now
        $run.Phase = 'ComTiquete'
        $run.Ticket = '2026-11-24T16:01:02+02:00'
        Save-LocalState $context
        $cipher = [IO.File]::ReadAllText($context.Path)
        Assert-OfflineCondition (-not $cipher.Contains($run.Ticket)) 'journal-cifrado'
        $context.State = Read-LocalState $context.Path
        $resumed = Get-Run $context $false $now
        Assert-OfflineCondition ($resumed.Id -ceq $run.Id -and $resumed.Ticket -is [string] -and $resumed.Ticket -ceq $run.Ticket) 'retomada-de-tiquete-em-formato-iso'
        Assert-OfflineCondition ((Get-PollDelay $metadata $resumed $now) -eq 120000) 'intervalo-primeira-consulta'
        $resumed.LastPoll = $now + 120000
        Assert-OfflineCondition ((Get-PollDelay $metadata $resumed ($resumed.LastPoll + 1)) -eq 119999) 'intervalo-consultas'
        Assert-OfflineCondition ((Get-PollDelay $metadata $resumed ($now + 240 * 60000)) -eq -1) 'limite-acompanhamento'
        $resumed.Polls = 120
        Assert-OfflineCondition ((Get-PollDelay $metadata $resumed $now) -eq -1) 'limite-consultas'

        $context.Dispatch = {
            param($context, $operation, $fields, $token)
            $context.Calls += $operation
            switch ($operation) {
                'Autenticar' { return $issued }
                'ConsultarDebitos' { return @{ Http = 0; Result = $null } }
                'ConsultarSituacaoSolicitacao' {
                    Assert-OfflineCondition ($fields.TiqueteSolicitacao -ceq 'tiquete-sintetico') 'situacao-reusa-tiquete'
                    return @{ Http = 200; Result = @{ Situacao = 'Concluida' } }
                }
                'ConsultarLinkDownload' {
                    return @{ Http = 200; Result = @{
                        UrlAssinada = 'https://example.invalid/aacbs?sig=sintetica'
                        UrlAssinadaExpiraEm = $context.LinkExpires
                        UrlAssinadaExpiraEmField = $null
                    } }
                }
                'BaixarArquivoRetorno' {
                    Assert-OfflineCondition ($token.Length -eq 0) 'download-sem-bearer'
                    Assert-OfflineCondition ($fields.UrlAssinada -ceq 'https://example.invalid/aacbs?sig=sintetica') 'download-preserva-url'
                    return @{ Http = 200; Result = @{ ArquivoDebitos = @{} }; Xml = $context.FinalXml }
                }
            }
        }
        $context.State = New-LocalState
        Assert-OfflineFailure { Invoke-Query $context $false $false } 'Abertura de debitos interrompida.*' 'post-timeout-controlado'
        $context.State = Read-LocalState $context.Path
        Assert-OfflineFailure { Invoke-Query $context $false $false } 'Abertura de debitos sem resultado confirmado.*' 'roteiro-sem-replay'
        Assert-OfflineCondition (($context.Calls -join ',') -ceq 'Autenticar,ConsultarDebitos') 'contagem-sem-replay'

        foreach ($expired in @($false, $true)) {
            $context.State = New-LocalState
            $context.Calls = @()
            $context.LinkExpires = if ($expired) { [DateTimeOffset]::UtcNow.AddMinutes(-1) } else { $null }
            $run = Start-Run $context ($now - 121000)
            $run.Phase = 'ComTiquete'
            $run.Ticket = 'tiquete-sintetico'
            Save-LocalState $context
            $context.State = Read-LocalState $context.Path
            if ($expired) {
                Assert-OfflineFailure { Invoke-Query $context $false $false } 'O link do arquivo de debitos expirou.' 'link-vencido'
                Assert-OfflineCondition (($context.Calls -join ',') -ceq 'Autenticar,ConsultarSituacaoSolicitacao,ConsultarLinkDownload') 'sem-download-vencido'
            }
            else {
                $largeText = 'x' * 70000
                $context.FinalXml = '<retBaixarArquivoRetorno><texto>CR&#xD;CRLF&#xD;&#xA;' + $largeText + '</texto></retBaixarArquivoRetorno>'
                Invoke-Query $context $false $true
                $finalPath = Join-Path $directory ('arquivo-debitos-' + $run.Id + '.dpapi')
                $finalXml = Read-ProtectedText $finalPath
                Assert-OfflineCondition ($finalXml -ceq $context.FinalXml) 'xml-final-acima-64-kib-fiel'
                $document = [Xml.XmlDocument]::new()
                $document.LoadXml($finalXml)
                Assert-OfflineCondition ($document.DocumentElement.InnerText -ceq ('CR' + "`r" + 'CRLF' + "`r`n" + $largeText)) 'xml-final-preserva-cr-e-crlf'
                $context.State = Read-LocalState $context.Path
                Invoke-Query $context $false $false
                Assert-OfflineCondition (($context.Calls -join ',') -ceq 'Autenticar,ConsultarSituacaoSolicitacao,ConsultarLinkDownload,BaixarArquivoRetorno') 'retomada-debito-sem-post-extra'
            }
        }

        [IO.File]::WriteAllText($context.Path, 'estado-sintetico-corrompido')
        Assert-OfflineFailure { Read-LocalState $context.Path } '*' 'journal-corrompido'
    }
    finally {
        foreach ($file in [IO.Directory]::GetFiles($directory)) { [IO.File]::Delete($file) }
        [IO.Directory]::Delete($directory)
    }
    Test-OfflineAACBS004 $metadata
    if ($Dll) { Test-OfflineDll $Dll }
    Write-Output 'Verificacoes sinteticas AACBS concluidas. Nenhuma API chamada e nenhum acesso local real importado.'
}
