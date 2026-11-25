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
        throw 'Metadados ou credencial AACBS invalidos para o roteiro de consultas.'
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
    try { $attributes = [IO.File]::GetAttributes($Path) }
    catch [IO.FileNotFoundException] { return }
    catch [IO.DirectoryNotFoundException] { return }
    if ($attributes -band [IO.FileAttributes]::ReparsePoint) {
        throw 'O acesso local AACBS exige arquivos e diretorios fisicos.'
    }
}

function ConvertFrom-AccessJson([string]$Json) {
    if ($PSVersionTable.PSVersion -ge [version]'7.6') {
        $options = [System.Text.Json.JsonDocumentOptions]::new()
        $options.AllowDuplicateProperties = $false
        $document = [System.Text.Json.JsonDocument]::Parse($Json, $options)
        try {
            if ($document.RootElement.ValueKind -ne [System.Text.Json.JsonValueKind]::Object) {
                throw 'Estrutura local invalida.'
            }
            return ConvertFrom-Json -InputObject $Json -DateKind String
        }
        finally { $document.Dispose() }
    }

    # O Windows PowerShell 5.1 nao traz System.Text.Json. O leitor nativo do
    # .NET Framework preserva cada chave, inclusive as escritas com escape.
    Add-Type -AssemblyName System.Runtime.Serialization -ErrorAction Stop
    $bytes = [Text.Encoding]::UTF8.GetBytes($Json)
    $reader = [Runtime.Serialization.Json.JsonReaderWriterFactory]::CreateJsonReader($bytes, [Xml.XmlDictionaryReaderQuotas]::Max)
    try {
        $xml = [Xml.XmlDocument]::new()
        $xml.Load($reader)
        $root = $xml.DocumentElement
        if ($root.GetAttribute('type') -cne 'object') { throw 'Estrutura local invalida.' }
        $keys = [Collections.Generic.HashSet[string]]::new([StringComparer]::OrdinalIgnoreCase)
        foreach ($property in $root.ChildNodes) {
            # A representacao XML usa <a:item item="nome"> para chaves com
            # escape; o atributo ja contem o nome JSON decodificado.
            $name = if ($property.HasAttribute('item')) { $property.GetAttribute('item') } else { $property.LocalName }
            if (-not $keys.Add($name)) { throw 'Campo local duplicado.' }
        }
    }
    finally { $reader.Dispose() }
    return ConvertFrom-Json -InputObject $Json
}

function Read-LocalAccessPair([string]$MetadataPath, [string]$CredentialPath) {
    Assert-OrdinaryPath $MetadataPath
    Assert-OrdinaryPath $CredentialPath
    # No Windows, FileShare.Read impede a retirada do JSON durante a leitura
    # da credencial. Cada leitor recebe um unico par publicado.
    $stream = [IO.File]::Open($MetadataPath, [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::Read)
    try {
        $reader = [IO.StreamReader]::new($stream)
        try {
            $metadata = ConvertFrom-AccessJson $reader.ReadToEnd()
            $credential = Import-Clixml -LiteralPath $CredentialPath -ErrorAction Stop
            Assert-LocalMetadata $metadata $credential
            return @{ Metadata = $metadata; Credential = $credential }
        }
        finally { $reader.Dispose() }
    }
    finally { $stream.Dispose() }
}

function Read-LocalAccess([string]$Directory) {
    try {
        Assert-OrdinaryPath (Split-Path -Parent $Directory)
        Assert-OrdinaryPath $Directory
        return Read-LocalAccessPair (Join-Path $Directory 'configuracao.local.json') (Join-Path $Directory 'credenciais.local.clixml')
    }
    catch {
        throw 'Falha ao verificar o acesso AACBS. Confira a configuracao local e a conta Windows que preparou a credencial.'
    }
}

function Write-LocalAccess([string]$Directory, $Metadata, [pscredential]$Credential, [switch]$AtualizarCredenciais) {
    $metadataPath = Join-Path $Directory 'configuracao.local.json'
    $credentialPath = Join-Path $Directory 'credenciais.local.clixml'
    $lockPath = Join-Path $Directory 'acesso.local.lock'
    foreach ($path in @((Split-Path -Parent $Directory), $Directory, $metadataPath, $credentialPath, $lockPath)) {
        Assert-OrdinaryPath $path
    }
    Assert-LocalMetadata $Metadata $Credential
    [void][IO.Directory]::CreateDirectory($Directory)
    $lock = [IO.FileStream]::new($lockPath, [IO.FileMode]::OpenOrCreate, [IO.FileAccess]::ReadWrite,
        [IO.FileShare]::None, 4096, [IO.FileOptions]::DeleteOnClose)
    try {
        if (([IO.File]::Exists($metadataPath) -or [IO.File]::Exists($credentialPath)) -and -not $AtualizarCredenciais) {
            throw 'Ja existe configuracao local. Use -Verificar para conferir ou -AtualizarCredenciais para substituir conscientemente.'
        }
        $suffix = '.' + [Guid]::NewGuid().ToString('N') + '.tmp'
        $metadataTemp = $metadataPath + $suffix
        $credentialTemp = $credentialPath + $suffix
        try {
            Export-Clixml -InputObject $Credential -LiteralPath $credentialTemp -Encoding UTF8 -NoClobber -ErrorAction Stop
            [IO.File]::WriteAllText($metadataTemp, (ConvertTo-Json -InputObject $Metadata -Depth 5), [Text.UTF8Encoding]::new($false))
            $null = Read-LocalAccessPair $metadataTemp $credentialTemp

            # O JSON presente autoriza a leitura do par. Uma interrupcao depois
            # da retirada mantem o acesso bloqueado ate uma atualizacao explicita.
            [IO.File]::Delete($metadataPath)
            if ([IO.File]::Exists($credentialPath)) {
                [IO.File]::Replace($credentialTemp, $credentialPath, [NullString]::Value)
            }
            else { [IO.File]::Move($credentialTemp, $credentialPath) }
            [IO.File]::Move($metadataTemp, $metadataPath)
        }
        finally {
            [IO.File]::Delete($metadataTemp)
            [IO.File]::Delete($credentialTemp)
        }
    }
    finally { $lock.Dispose() }
}
