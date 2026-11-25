function Test-OfflineAACBS004Repository([string]$Entry) {
    $directory = Join-Path ([IO.Path]::GetTempPath()) ('aacbs-004-worktree-' + [Guid]::NewGuid().ToString('N'))
    $main = Join-Path $directory 'checkout principal'
    $worktree = Join-Path $directory 'worktree da etapa'
    $docs = Join-Path $worktree 'docsplan/aacbs/scripts'
    $override = Join-Path $directory 'checkout explicito'
    $entryCopy = Join-Path $docs 'Executar-AACBS004.ps1'
    $stubModule = Join-Path $docs 'AACBS-RoteiroLocal.psm1'
    $isolated = [PowerShell]::Create()
    try {
        foreach ($path in @($main, $docs, $override)) { [void][IO.Directory]::CreateDirectory($path) }
        [IO.File]::Copy($Entry, $entryCopy)
        [IO.File]::WriteAllText($stubModule, @'
function Invoke-AACBSLocal($Repository, $Action, $Dll, $NewRun, $KeepFinalXml, $NoWait) {
    return $Repository
}
Export-ModuleMember -Function Invoke-AACBSLocal
'@)
        [void]$isolated.AddScript({
            param($entry, $main, $worktree, $override)
            $ErrorActionPreference = 'Stop'
            $gitCalls = [Collections.Generic.List[string]]::new()
            function git {
                if ($args.Count -ne 5 -or $args[0] -cne '-C' -or $args[1] -cne $worktree -or
                    ($args[2..4] -join ' ') -cne 'worktree list --porcelain') {
                    throw 'Invocacao inesperada do Git no teste offline AACBS 004.'
                }
                $gitCalls.Add($args[1])
                $global:LASTEXITCODE = 0
                "worktree $main"
                ''
                "worktree $worktree"
            }
            $defaultRepository = & $entry -Consultar -Familia Creditos -Dll 'dll-sintetica-inexistente.dll'
            $callsAfterDefault = $gitCalls.Count
            $explicitRepository = & $entry -Consultar -Familia Creditos -Dll 'dll-sintetica-inexistente.dll' -RepositorioLocal $override
            [pscustomobject]@{
                Default = $defaultRepository; Explicit = $explicitRepository
                CallsAfterDefault = $callsAfterDefault; TotalCalls = $gitCalls.Count
            }
        }.ToString()).AddArgument($entryCopy).AddArgument($main).AddArgument($worktree).AddArgument($override)
        $result = $isolated.Invoke()
        Assert-OfflineCondition (-not $isolated.HadErrors -and $result.Count -eq 1) '004-worktree-execucao-isolada'
        Assert-OfflineCondition ($result[0].Default -ceq $main -and $result[0].CallsAfterDefault -eq 1) '004-worktree-usa-checkout-principal'
        Assert-OfflineCondition ($result[0].Explicit -ceq $override -and $result[0].TotalCalls -eq 1) '004-worktree-preserva-repositorio-explicito'
    }
    finally {
        $isolated.Dispose()
        foreach ($file in @($entryCopy, $stubModule)) { if ([IO.File]::Exists($file)) { [IO.File]::Delete($file) } }
        foreach ($path in @($docs, (Join-Path $worktree 'docsplan/aacbs'), (Join-Path $worktree 'docsplan'), $worktree, $main, $override, $directory)) {
            if ([IO.Directory]::Exists($path)) { [IO.Directory]::Delete($path) }
        }
    }
}

function Test-OfflineAACBS004($Metadata) {
    $contract = Get-ContractFailure ([FormatException]::new('AACBS BaixarArquivoRetorno (CREDITOS): resposta incompatível com o contrato em $.ni.')) 'BaixarArquivoRetorno'
    Assert-OfflineCondition ($null -ne $contract -and $contract.Path -ceq '$.ni') '004-caminho-sanitizado-de-contrato'
    $observationContext = @{ Metadata = $Metadata; Stage = 'AACBS-004'; Synthetic = $true }
    foreach ($field in @('scope', 'access_token', 'token_type', 'expires_in')) {
        $path = '$.' + $field
        $contract = Get-ContractFailure ([FormatException]::new("AACBS Autenticar: resposta incompatível com o contrato em $path.")) 'ConsultarCreditos'
        Assert-OfflineCondition ($null -ne $contract -and $contract.Operation -ceq 'Autenticar' -and $contract.Path -ceq $path) "004-contrato-oauth-$field"
        $observation = Write-Observation $observationContext $contract.Operation 'ContratoIncompativel' 200 $contract.Path 6>&1
        Assert-OfflineCondition ($observation.MessageData -clike "* operacao=Autenticar estado=ContratoIncompativel HTTP=200 * campo=$path") "004-observacao-oauth-$field"
    }
    foreach ($unsafeMessage in @(
        'SENTINELA-CORPO-PRIVADO',
        'AACBS BaixarArquivoRetorno (CREDITOS): resposta incompatível com o contrato em $.ni?token=SENTINELA.',
        'AACBS BaixarArquivoRetorno (CREDITOS): resposta incompatível com o contrato em $.ni. SENTINELA',
        'AACBS Autenticar: resposta incompatível com o contrato em $.access_token?token=SENTINELA.'
    )) {
        Assert-OfflineCondition ($null -eq (Get-ContractFailure ([FormatException]::new($unsafeMessage)) 'BaixarArquivoRetorno')) '004-nao-expoe-mensagem-remota'
    }
    $observation = Write-Observation $observationContext 'Autenticar' 'ContratoIncompativel' 200 '$.access_token?token=SENTINELA' 6>&1
    Assert-OfflineCondition ($observation.MessageData -cnotmatch 'campo=|SENTINELA') '004-nao-registra-caminho-oauth-invalido'
    $entry = Join-Path $PSScriptRoot 'Executar-AACBS004.ps1'
    Assert-OfflineFailure { & $entry -Consultar:$false -Familia Creditos -Dll 'inexistente.dll' -RepositorioLocal 'inexistente' } 'O modo AACBS selecionado exige switch com valor verdadeiro.' '004-switch-falso'
    Assert-OfflineFailure { & $entry -Offline:$false } 'O modo AACBS selecionado exige switch com valor verdadeiro.' '004-offline-falso'
    Test-OfflineAACBS004Repository $entry
    $directory = Join-Path ([IO.Path]::GetTempPath()) ('aacbs-004-offline-' + [Guid]::NewGuid().ToString('N'))
    [void][IO.Directory]::CreateDirectory($directory)
    $context = @{
        Metadata = $Metadata; State = New-LocalState; Cnpj = '00000000'; Calls = @(); Synthetic = $true
        Path = Join-Path $directory 'execucao.dpapi'; TokenScope = Get-Identity @('004-emissor', '004-cliente')
    }
    $context.Dispatch = {
        param($context, $operation, $fields, $token)
        $context.Calls += $operation
        $family = Get-QueryFamily $context
        switch -Regex ($operation) {
            '^Autenticar$' { return @{ Http = 200; Result = @{ AccessToken = '004-token-sintetico'; TokenType = 'Bearer'; ExpiresIn = 3600 } } }
            '^Consultar(Debitos|Creditos|Pagamentos|Recolhimentos)$' {
                Assert-OfflineCondition ($operation -ceq ('Consultar' + $family)) '004-operacao-da-familia'
                Assert-OfflineCondition ($fields.Cnpj -ceq $context.Cnpj) '004-cnpj-da-familia'
                return @{ Http = 201; Result = @{ TiqueteSolicitacao = 'tiquete-' + $family; TEASegundos = 0 } }
            }
            '^ConsultarSituacaoSolicitacao$' {
                Assert-OfflineCondition ($fields.TiqueteSolicitacao -ceq ('tiquete-' + $family)) '004-retomada-mesmo-tiquete'
                return @{ Http = 200; Result = @{ Situacao = 'Concluida' } }
            }
            '^ConsultarLinkDownload$' {
                return @{ Http = 200; Result = @{ UrlAssinada = 'https://example.invalid/004'; UrlAssinadaExpiraEm = $null; UrlAssinadaExpiraEmField = $null } }
            }
            '^BaixarArquivoRetorno$' {
                Assert-OfflineCondition ($fields.TipoConsulta -ceq $family) '004-download-familia-explicita'
                Assert-OfflineCondition ($fields.TiqueteSolicitacao -ceq ('tiquete-' + $family)) '004-download-tiquete'
                Assert-OfflineCondition ($token.Length -eq 0) '004-download-sem-token'
                $result = @{}
                $result['Arquivo' + $family] = @{}
                return @{ Http = 200; Result = $result; Xml = '<xml-sintetico />' }
            }
            default { throw 'Operacao imprevista no teste offline 004.' }
        }
    }
    try {
        foreach ($family in @('Debitos', 'Creditos', 'Pagamentos', 'Recolhimentos')) {
            $context.Family = $family
            $context.Stage = if ($family -ceq 'Debitos') { 'AACBS-003' } else { 'AACBS-004' }
            $context.RunScope = Get-Identity @('004-empresa', $family)
            Invoke-Query $context $false $false $true
            $run = $context.State.Runs[-1]
            Assert-OfflineCondition ($run.Phase -ceq 'ComTiquete' -and $run.Polls -eq 0) '004-sem-aguardar-nao-consulta-antes-da-janela'
            Assert-OfflineCondition ($run.Api -ceq $family -and $run.Stage -ceq $context.Stage) '004-etapa-e-api-no-journal'
            $run.Opened = (Get-Now) - 121000L
            Save-LocalState $context
            $context.State = Read-LocalState $context.Path
            Invoke-Query $context $false $false $true
            Assert-OfflineCondition ($context.State.Runs[-1].Phase -ceq 'Finalizada') '004-ciclo-finalizado'
            $before = $context.Calls.Count
            Invoke-Query $context $false $false $true
            Assert-OfflineCondition ($context.Calls.Count -eq $before) '004-finalizada-sem-request-extra'
            Assert-OfflineFailure { Get-Run $context $true (Get-Now) } 'Orcamento local diario de abertura*esgotado.' '004-cota-por-api'
        }
        Assert-OfflineCondition ($context.State.Runs.Count -eq 4) '004-journal-preserva-familias-e-legado'
        Assert-OfflineCondition (@($context.Calls | Where-Object { $_ -ceq 'Autenticar' }).Count -eq 1) '004-cache-comum-sem-reemissao'
        Assert-OfflineCondition ($context.Calls.Count -eq 17) '004-contagem-exata-de-requests'
        $context.State.Runs[0].Stage = 'AACBS-004'
        Save-LocalState $context
        Assert-OfflineFailure { Read-LocalState $context.Path } 'Journal AACBS invalido.' '004-rejeita-etapa-api-cruzadas'
    }
    finally {
        foreach ($file in [IO.Directory]::GetFiles($directory)) { [IO.File]::Delete($file) }
        [IO.Directory]::Delete($directory)
    }
}
