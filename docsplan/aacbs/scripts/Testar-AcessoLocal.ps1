[CmdletBinding()]
param()

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if ([Environment]::OSVersion.Platform -ne [PlatformID]::Win32NT) {
    throw 'Este teste offline exige Windows para usar DPAPI.'
}

$temporaryRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath())
$repository = Join-Path $temporaryRoot ('aacbs-acesso-offline-' + [Guid]::NewGuid().ToString('N'))
$scripts = Join-Path $repository 'docsplan/aacbs/scripts'
$runner = [PowerShell]::Create()

try {
    [void][IO.Directory]::CreateDirectory($scripts)
    foreach ($name in @('Configurar-AcessoLocal.ps1', 'AACBS-AcessoLocal.ps1', 'AACBS-RoteiroLocal.psm1',
        'Testar-RoteiroAACBS.ps1', 'Testar-ConsultasAACBS004.ps1')) {
        [IO.File]::Copy((Join-Path $PSScriptRoot $name), (Join-Path $scripts $name))
    }

    [void]$runner.AddScript({
        param($Repository, $Scripts)

        Set-StrictMode -Version Latest
        $ErrorActionPreference = 'Stop'
        $entry = Join-Path $Scripts 'Configurar-AcessoLocal.ps1'
        $local = Join-Path $Repository '.local/aacbs'
        $configurationPath = Join-Path $local 'configuracao.local.json'
        $credentialPath = Join-Path $local 'credenciais.local.clixml'
        $global:accessTestInputs = @{
            Cnpj = '00000000000001'; Environment = 'ProducaoRestrita'
            ClientId = 'cliente-aacbs-sintetico-1'; Secret = 'segredo-aacbs-sintetico-1'
            PromptCalls = 0; Repository = $Repository
        }
        $inputs = $global:accessTestInputs

        function global:git {
            if ($args.Count -lt 4 -or $args[0] -cne '-C' -or
                [IO.Path]::GetFullPath($args[1]) -ine $global:accessTestInputs.Repository) {
                throw 'Repositorio inesperado no teste offline AACBS.'
            }
            $global:LASTEXITCODE = 0
            switch ($args[2]) {
                'rev-parse' { $global:accessTestInputs.Repository }
                'check-ignore' { }
                'ls-files' { }
                default { throw 'Comando Git inesperado no teste offline AACBS.' }
            }
        }

        function global:Read-Host([string]$Prompt) {
            $global:accessTestInputs.PromptCalls += 1
            if ($Prompt -like 'CNPJ*') { return $global:accessTestInputs.Cnpj }
            if ($Prompt -like 'Ambiente*') { return $global:accessTestInputs.Environment }
            throw 'Prompt inesperado no teste offline AACBS.'
        }

        function global:Get-Credential([string]$Message) {
            $global:accessTestInputs.PromptCalls += 1
            $password = ConvertTo-SecureString -String $global:accessTestInputs.Secret -AsPlainText -Force
            return [pscredential]::new($global:accessTestInputs.ClientId, $password)
        }

        function Assert-Case([bool]$Condition, [string]$Case) {
            if (-not $Condition) { throw "Falha no cenario offline de acesso AACBS $Case." }
        }

        function Assert-Rejected([scriptblock]$Action, [string]$Message, [string]$Case) {
            try { $null = & $Action }
            catch {
                Assert-Case ($_.Exception.Message -like $Message) $Case
                return
            }
            throw "Operacao deveria falhar no cenario offline de acesso AACBS $Case."
        }

        function Assert-NoSecrets($Result, $Expected, [string]$Case) {
            $output = $Result | Out-String
            foreach ($value in @($Expected.Cnpj, $Expected.ClientId, $Expected.Secret)) {
                Assert-Case (-not $output.Contains($value)) $Case
            }
        }

        function Assert-PreparedAccess($Access, $Expected, [string]$Case) {
            Assert-Case ($Access.Metadata.cnpjMatriz -ceq $Expected.Cnpj -and
                $Access.Metadata.ambiente -ceq $Expected.Environment -and
                $Access.Credential.UserName -ceq $Expected.ClientId -and
                $Access.Credential.GetNetworkCredential().Password -ceq $Expected.Secret) $Case
        }

        $module = Import-Module (Join-Path $Scripts 'AACBS-RoteiroLocal.psm1') -PassThru
        $originalInputs = $inputs.Clone()
        $output = & $entry *>&1
        Assert-NoSecrets $output $inputs 'criacao-sem-expor-valores'
        Assert-PreparedAccess (& $module { param($Directory) Read-LocalAccess $Directory } $local) $inputs 'criacao-leitura-pelo-executor'
        Assert-Case (-not [IO.File]::ReadAllText($credentialPath).Contains($inputs.Secret)) 'segredo-cifrado-dpapi'
        Assert-Case ($inputs.PromptCalls -eq 3) 'criacao-prompts'
        Assert-NoSecrets (& $entry -Verificar *>&1) $inputs 'verificacao-sem-expor-valores'
        $executorOutput = Invoke-AACBSLocal -Repository $Repository -Action Verificar *>&1
        Assert-NoSecrets $executorOutput $inputs 'executor-verifica-sem-expor-valores'
        Assert-Case (($executorOutput | Out-String).Contains('estado=Verificado')) 'executor-verifica-entrada-publica'
        Assert-Case ($inputs.PromptCalls -eq 3) 'verificacao-sem-prompts'

        $originalJson = [IO.File]::ReadAllText($configurationPath)
        $originalCredential = [IO.File]::ReadAllBytes($credentialPath)
        Assert-Rejected { & $entry } 'Ja existe configuracao local*' 'substituicao-exige-switch'
        Assert-Case ($inputs.PromptCalls -eq 3 -and [IO.File]::ReadAllText($configurationPath) -ceq $originalJson -and
            [Convert]::ToBase64String([IO.File]::ReadAllBytes($credentialPath)) -ceq [Convert]::ToBase64String($originalCredential)) 'sem-substituicao-implicita'

        foreach ($invalid in @(
            @{ Field = 'limiteAberturasPorApiPorDia'; Value = 2 },
            @{ Field = 'permitirGeracaoDarf'; Value = $true },
            @{ Field = 'cenariosDarf'; Value = @('cenario-sintetico') },
            @{ Field = 'intervaloConsultaSegundos'; Value = 119 },
            @{ Field = 'maximoConsultasSituacao'; Value = 121 },
            @{ Field = 'tempoMaximoAcompanhamentoMinutos'; Value = 0 },
            @{ Field = 'permitirProducao'; Value = $true }
        )) {
            $metadata = ConvertFrom-Json -InputObject $originalJson
            $metadata.($invalid.Field) = $invalid.Value
            [IO.File]::WriteAllText($configurationPath, (ConvertTo-Json -InputObject $metadata -Depth 5))
            Assert-Rejected { & $entry -Verificar } '*verificar o acesso*' ('preparador-rejeita-' + $invalid.Field)
            Assert-Rejected { Invoke-AACBSLocal -Repository $Repository -Action Verificar } '*verificar o acesso*' ('executor-rejeita-' + $invalid.Field)
        }

        foreach ($invalid in @(
            @{ Case = 'chave-duplicada'; Json = $originalJson.Insert(1, '"permitirProducao":false,') },
            @{ Case = 'chave-duplicada-escapada'; Json = $originalJson.Insert(1, '"\u0070ermitirProducao":false,') },
            @{ Case = 'raiz-em-array'; Json = '[' + $originalJson + ']' }
        )) {
            [IO.File]::WriteAllText($configurationPath, $invalid.Json)
            Assert-Rejected { & $entry -Verificar } '*verificar o acesso*' ('preparador-rejeita-' + $invalid.Case)
            Assert-Rejected { Invoke-AACBSLocal -Repository $Repository -Action Verificar } '*verificar o acesso*' ('executor-rejeita-' + $invalid.Case)
        }

        [IO.File]::WriteAllText($configurationPath, $originalJson)
        Assert-NoSecrets (& $entry -Verificar *>&1) $inputs 'verifica-configuracao-restaurada'
        Assert-Case ($inputs.PromptCalls -eq 3) 'verificacoes-invalidas-sem-prompts'

        $inputs.Cnpj = '00000000000002'
        $inputs.Environment = 'ProducaoBeta'
        $inputs.ClientId = 'cliente-aacbs-sintetico-2'
        $inputs.Secret = 'segredo-aacbs-sintetico-2'
        $interleave = @{ Entry = $entry; CredentialPath = $credentialPath; Imports = 0; WriterFailed = $false }
        $readDuringUpdate = & $module {
            param($Directory, $Interleave)

            $script:accessInterleave = $Interleave
            function script:Import-Clixml {
                [CmdletBinding()]
                param([string]$LiteralPath)

                # Tenta publicar a segunda revisao entre a leitura do JSON e a importacao da credencial.
                if ($LiteralPath -ieq $script:accessInterleave.CredentialPath) {
                    $script:accessInterleave.Imports += 1
                    try { $null = & $script:accessInterleave.Entry -AtualizarCredenciais }
                    catch { $script:accessInterleave.WriterFailed = $true }
                }
                return Microsoft.PowerShell.Utility\Import-Clixml @PSBoundParameters
            }
            try { Read-LocalAccess $Directory }
            finally {
                Remove-Item -LiteralPath Function:Import-Clixml
                Remove-Variable -Name accessInterleave -Scope Script
            }
        } $local $interleave
        Assert-Case ($interleave.Imports -eq 1 -and $interleave.WriterFailed -and $inputs.PromptCalls -eq 6) 'leitor-bloqueia-rotacao-intercalada'
        Assert-PreparedAccess $readDuringUpdate $originalInputs 'leitor-intercalado-preserva-par-antigo'
        Assert-PreparedAccess (& $module { param($Directory) Read-LocalAccess $Directory } $local) $originalInputs 'tentativa-intercalada-preserva-par-publicado'

        # Impede a publicacao da credencial apos a retirada do JSON ativo.
        $credentialLock = [IO.File]::Open($credentialPath, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read)
        try {
            Assert-Rejected { & $entry -AtualizarCredenciais } '*' 'falha-ao-publicar-credencial'
            Assert-Case ($inputs.PromptCalls -eq 9) 'rotacao-interrompida-apos-prompts'
            Assert-Case (-not [IO.File]::Exists($configurationPath)) 'rotacao-interrompida-sem-json-ativo'
            Assert-Rejected { & $entry -Verificar } '*verificar o acesso*' 'preparador-rejeita-publicacao-incompleta'
            Assert-Rejected { & $module { param($Directory) Read-LocalAccess $Directory } $local } '*verificar o acesso*' 'executor-rejeita-publicacao-incompleta'
        }
        finally { $credentialLock.Dispose() }

        Assert-NoSecrets (& $entry -AtualizarCredenciais *>&1) $inputs 'rotacao-sem-expor-valores'
        $updated = & $module { param($Directory) Read-LocalAccess $Directory } $local
        Assert-PreparedAccess $updated $inputs 'rotacao-publica-revisao-consistente'
        Assert-Case $updated.Metadata.permitirProducao 'beta-autorizado-explicitamente'
        Assert-NoSecrets (& $entry -Verificar *>&1) $inputs 'verifica-configuracao-atualizada'
        Assert-Case ($inputs.PromptCalls -eq 12) 'somente-criacao-e-atualizacao-pedem-valores'
    }.ToString()).AddArgument($repository).AddArgument($scripts)

    $null = $runner.Invoke()
    if ($runner.HadErrors) {
        throw 'Falha na execucao isolada do teste offline de acesso AACBS.'
    }
    Write-Output 'Verificacoes sinteticas de acesso local AACBS concluidas.'
}
finally {
    $runner.Dispose()
    if ($repository.StartsWith($temporaryRoot, [StringComparison]::OrdinalIgnoreCase) -and [IO.Directory]::Exists($repository)) {
        [IO.Directory]::Delete($repository, $true)
    }
}
