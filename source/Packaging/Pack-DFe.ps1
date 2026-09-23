[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('Offline', 'Public')]
    [string]$Mode,

    [string]$StartDate,

    [string]$EndDate,

    [switch]$DryRun
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.IO.Compression.FileSystem

$script:PackageId = 'Unimake.DFe'
$script:PackageDescription = 'Pacote para geração e envio dos XML de documentos fiscais eletrônicos (NFe, NFCe, CTe, CTeOS, CTeSimp, MDFe, GNRe, NFSe, eSocial, EFD-Reinf, DARE-SP, NF3e, NFCom, DCe, NFGas, NFe-ABI, etc.) para a SEFAZ, Receita Federal e Municípios.'
$script:OfflineFeed = 'C:\projetos\NuGetOffline'
$script:OfflineSourceName = 'Unimake Offline'
$script:NuGetSource = 'https://api.nuget.org/v3/index.json'
$script:GitHubRepository = 'Unimake/DFe'
$script:SourceRoot = Split-Path -Parent $PSScriptRoot
$script:RepositoryRoot = Split-Path -Parent $script:SourceRoot
$script:ProjectPath = Join-Path $script:SourceRoot '.NET Standard\Unimake.Business.DFe\Unimake.Business.DFe.csproj'
$script:TemplatePath = Join-Path $PSScriptRoot 'Unimake.DFe.nuspec.template'
$script:TrackedPackagePath = Join-Path $script:SourceRoot 'Unimake.DFe\Unimake.DFe.nupkg'
$script:CiotSourcePath = Join-Path $script:SourceRoot '.NET Standard\Unimake.Business.DFe\ThirdParty\GeradorCIOTShared\GeradorCIOTShared.dll'
$script:DateFormat = 'dd/MM/yyyy HH:mm:ss'

function Invoke-NativeCommand {
    param(
        [Parameter(Mandatory = $true)]
        [string]$Command,

        [Parameter(Mandatory = $true)]
        [string[]]$Arguments,

        [switch]$CaptureOutput
    )

    Write-Host "Executando: $Command $($Arguments -join ' ')" -ForegroundColor DarkGray

    $previousErrorActionPreference = $ErrorActionPreference
    $ErrorActionPreference = 'Continue'
    try {
        $output = & $Command @Arguments 2>&1
        $exitCode = $LASTEXITCODE
    }
    finally {
        $ErrorActionPreference = $previousErrorActionPreference
    }

    if ($exitCode -ne 0) {
        throw "Falha ao executar '$Command $($Arguments -join ' ')' (código $exitCode).`n$($output -join [Environment]::NewLine)"
    }

    if ($CaptureOutput) {
        return $output
    }

    foreach ($line in @($output)) {
        if ($line -is [System.Management.Automation.ErrorRecord]) {
            Write-Warning $line.Exception.Message
        }
        else {
            Write-Host $line
        }
    }
}

function Get-SaoPauloTimeZone {
    try {
        return [TimeZoneInfo]::FindSystemTimeZoneById('E. South America Standard Time')
    }
    catch {
        return [TimeZoneInfo]::FindSystemTimeZoneById('America/Sao_Paulo')
    }
}

function Invoke-JsonRequest {
    param([Parameter(Mandatory = $true)][string]$Uri)

    $headers = @{
        Accept = 'application/vnd.github+json'
        'User-Agent' = 'Unimake-DFe-Packager'
    }
    if (-not [string]::IsNullOrWhiteSpace($env:GITHUB_TOKEN)) {
        $headers.Authorization = "Bearer $($env:GITHUB_TOKEN)"
    }

    return Invoke-RestMethod -Uri $Uri -Headers $headers -Method Get
}

function Get-LatestPublishedDate {
    $uri = 'https://api.nuget.org/v3/registration5-semver1/unimake.dfe/index.json'
    $index = Invoke-RestMethod -Uri $uri -Method Get
    $entries = @()

    foreach ($page in $index.items) {
        $itemsProperty = $page.PSObject.Properties['items']
        if ($null -eq $itemsProperty) {
            $pageData = Invoke-RestMethod -Uri $page.'@id' -Method Get
            $itemsProperty = $pageData.PSObject.Properties['items']
        }
        $entries += $itemsProperty.Value
    }

    $latest = $entries |
        Where-Object { $_.catalogEntry.version -match '^\d{8}\.\d{3,4}\.\d{1,2}$' } |
        Sort-Object { [DateTimeOffset]($_.catalogEntry.published) } -Descending |
        Select-Object -First 1

    if ($null -eq $latest) {
        throw 'Não foi possível determinar a última publicação da Unimake.DFe no nuget.org.'
    }

    $published = [DateTimeOffset]($latest.catalogEntry.published)
    return [TimeZoneInfo]::ConvertTime($published, (Get-SaoPauloTimeZone)).DateTime
}

function Read-DateValue {
    param(
        [Parameter(Mandatory = $true)][string]$Label,
        [Parameter(Mandatory = $true)][DateTime]$DefaultValue,
        [string]$ProvidedValue
    )

    $value = $ProvidedValue
    if ([string]::IsNullOrWhiteSpace($value)) {
        $defaultText = $DefaultValue.ToString($script:DateFormat, [Globalization.CultureInfo]::InvariantCulture)
        $value = Read-Host "$Label [$defaultText]"
        if ([string]::IsNullOrWhiteSpace($value)) {
            $value = $defaultText
        }
    }

    $parsed = [DateTime]::MinValue
    $styles = [Globalization.DateTimeStyles]::AllowWhiteSpaces
    if (-not [DateTime]::TryParseExact($value, $script:DateFormat, [Globalization.CultureInfo]::InvariantCulture, $styles, [ref]$parsed)) {
        throw "Data inválida: '$value'. Use o formato $($script:DateFormat)."
    }

    return [DateTime]::SpecifyKind($parsed, [DateTimeKind]::Unspecified)
}

function Get-ReleaseInterval {
    $timeZone = Get-SaoPauloTimeZone
    $now = [TimeZoneInfo]::ConvertTime([DateTimeOffset]::UtcNow, $timeZone).DateTime
    $latestPublished = Get-LatestPublishedDate
    $start = Read-DateValue -Label 'Data/hora inicial' -DefaultValue $latestPublished -ProvidedValue $StartDate
    $end = Read-DateValue -Label 'Data/hora final' -DefaultValue $now -ProvidedValue $EndDate

    if ($start -ge $end) {
        throw 'A data/hora inicial deve ser anterior à data/hora final.'
    }

    return [pscustomobject]@{
        StartLocal = $start
        EndLocal = $end
        StartUtc = [TimeZoneInfo]::ConvertTimeToUtc($start, $timeZone)
        EndUtc = [TimeZoneInfo]::ConvertTimeToUtc($end, $timeZone)
    }
}

function Get-ReleaseNotes {
    param([Parameter(Mandatory = $true)]$Interval)

    $since = [Uri]::EscapeDataString($Interval.StartUtc.ToString('o'))
    $until = [Uri]::EscapeDataString($Interval.EndUtc.ToString('o'))
    $commits = @()
    $page = 1

    do {
        $uri = "https://api.github.com/repos/$($script:GitHubRepository)/commits?sha=main&since=$since&until=$until&per_page=100&page=$page"
        $response = Invoke-JsonRequest -Uri $uri
        $batch = @($response)
        $commits += $batch
        $page++
    } while ($batch.Count -eq 100)

    $subjects = foreach ($item in $commits) {
        $commitDate = [DateTimeOffset]($item.commit.committer.date)
        if ($commitDate.UtcDateTime -le $Interval.StartUtc -or $commitDate.UtcDateTime -gt $Interval.EndUtc) {
            continue
        }

        $subject = ([string]$item.commit.message -split "`r?`n", 2)[0].Trim()
        if ($subject -notmatch '^(?i:feat|fix|config)(\([^)]+\))?:\s+.+') {
            continue
        }

        $subject = $subject -replace '\s+ID\s*#\d+\s*$', ''
        if (-not [string]::IsNullOrWhiteSpace($subject)) {
            $subject
        }
    }

    $subjects = @($subjects | Select-Object -Unique)
    if ($subjects.Count -eq 0) {
        throw 'Nenhum commit feat, fix ou config foi encontrado no intervalo informado.'
    }

    return ($subjects | ForEach-Object { "- $_" }) -join ([Environment]::NewLine + [Environment]::NewLine)
}

function Get-DependencyVersions {
    [xml]$project = Get-Content -Raw -LiteralPath $script:ProjectPath
    $references = @{}
    foreach ($reference in $project.SelectNodes('/Project/ItemGroup/PackageReference')) {
        if ($null -ne $reference.Include -and $null -ne $reference.Version) {
            $references[[string]$reference.Include] = [string]$reference.Version
        }
    }

    $required = @(
        'Microsoft.CSharp',
        'Unimake.Utils',
        'Unimake.Extensions',
        'Unimake.Cryptography',
        'System.Net.Http.WinHttpHandler',
        'System.Security.Cryptography.Xml'
    )
    foreach ($id in $required) {
        if (-not $references.ContainsKey($id)) {
            throw "A dependência obrigatória '$id' não foi encontrada no projeto."
        }
    }

    $selectedReferences = @{}
    foreach ($id in $required) {
        $selectedReferences[$id] = $references[$id]
    }
    return $selectedReferences
}

function ConvertTo-XmlText {
    param([AllowEmptyString()][string]$Value)
    return [Security.SecurityElement]::Escape($Value)
}

function New-Nuspec {
    param(
        [Parameter(Mandatory = $true)][string]$PackageVersion,
        [Parameter(Mandatory = $true)][string]$ReleaseNotes,
        [Parameter(Mandatory = $true)][string]$OutputPath,
        [Parameter(Mandatory = $true)][hashtable]$Dependencies,
        [Parameter(Mandatory = $true)][hashtable]$Files
    )

    $content = [IO.File]::ReadAllText($script:TemplatePath, [Text.Encoding]::UTF8)
    $replacements = [ordered]@{
        '{{PACKAGE_VERSION}}' = $PackageVersion
        '{{DESCRIPTION}}' = ConvertTo-XmlText $script:PackageDescription
        '{{RELEASE_NOTES}}' = ConvertTo-XmlText $ReleaseNotes
        '{{DEPENDENCY_MICROSOFT_CSHARP}}' = $Dependencies['Microsoft.CSharp']
        '{{DEPENDENCY_UNIMAKE_UTILS}}' = $Dependencies['Unimake.Utils']
        '{{DEPENDENCY_UNIMAKE_EXTENSIONS}}' = $Dependencies['Unimake.Extensions']
        '{{DEPENDENCY_UNIMAKE_CRYPTOGRAPHY}}' = $Dependencies['Unimake.Cryptography']
        '{{DEPENDENCY_WINHTTPHANDLER}}' = $Dependencies['System.Net.Http.WinHttpHandler']
        '{{DEPENDENCY_CRYPTOGRAPHY_XML}}' = $Dependencies['System.Security.Cryptography.Xml']
        '{{DFe_DLL}}' = ConvertTo-XmlText $Files.DFeDll
        '{{DFe_PDB}}' = ConvertTo-XmlText $Files.DFePdb
        '{{DFe_XML}}' = ConvertTo-XmlText $Files.DFeXml
        '{{CIOT_DLL}}' = ConvertTo-XmlText $Files.CiotDll
    }

    foreach ($entry in $replacements.GetEnumerator()) {
        $content = $content.Replace($entry.Key, [string]$entry.Value)
    }

    if ($content -match '\{\{[^}]+\}\}') {
        throw 'O template do pacote contém marcadores que não foram substituídos.'
    }

    [IO.File]::WriteAllText($OutputPath, $content, [Text.UTF8Encoding]::new($false))
}

function Test-PackageDoesNotExist {
    param([Parameter(Mandatory = $true)][string]$PackageVersion)

    if ($Mode -eq 'Offline') {
        if (Test-Path -LiteralPath $script:OfflineFeed) {
            $existing = Get-ChildItem -LiteralPath $script:OfflineFeed -Recurse -File -Filter "$($script:PackageId).$PackageVersion.nupkg" -ErrorAction SilentlyContinue
            if ($null -ne $existing) {
                throw "A versão $PackageVersion já existe no feed offline. Pacotes são imutáveis."
            }
        }
        return
    }

    $index = Invoke-RestMethod -Uri 'https://api.nuget.org/v3-flatcontainer/unimake.dfe/index.json' -Method Get
    if (@($index.versions) -contains $PackageVersion.ToLowerInvariant()) {
        throw "A versão $PackageVersion já existe no nuget.org."
    }
}

function Test-PublicationPreconditions {
    if ($DryRun) {
        Write-Host 'DryRun: validações de branch limpa, sincronismo e NUGET_API_KEY foram ignoradas.' -ForegroundColor Yellow
        return
    }

    $branch = (Invoke-NativeCommand -Command git -Arguments @('-C', $script:RepositoryRoot, 'branch', '--show-current') -CaptureOutput | Select-Object -First 1).Trim()
    if ($branch -ne 'main') {
        throw "A publicação oficial exige a branch main. Branch atual: $branch."
    }

    $status = @(Invoke-NativeCommand -Command git -Arguments @('-C', $script:RepositoryRoot, 'status', '--porcelain') -CaptureOutput)
    if ($status.Count -gt 0) {
        throw 'A publicação oficial exige uma árvore Git limpa.'
    }

    Invoke-NativeCommand -Command git -Arguments @('-C', $script:RepositoryRoot, 'fetch', 'origin', 'main', '--quiet')
    $head = (Invoke-NativeCommand -Command git -Arguments @('-C', $script:RepositoryRoot, 'rev-parse', 'HEAD') -CaptureOutput | Select-Object -First 1).Trim()
    $originHead = (Invoke-NativeCommand -Command git -Arguments @('-C', $script:RepositoryRoot, 'rev-parse', 'origin/main') -CaptureOutput | Select-Object -First 1).Trim()
    if ($head -ne $originHead) {
        throw 'A branch main local não está sincronizada com origin/main.'
    }

    if ([string]::IsNullOrWhiteSpace($env:NUGET_API_KEY)) {
        throw 'Defina a variável de ambiente NUGET_API_KEY antes de publicar.'
    }
}

function Build-Package {
    param(
        [Parameter(Mandatory = $true)][string]$PackageVersion,
        [Parameter(Mandatory = $true)][string]$AssemblyVersion,
        [Parameter(Mandatory = $true)][string]$ReleaseNotes,
        [Parameter(Mandatory = $true)][string]$TemporaryPath
    )

    $cleanArguments = @(
        'clean', $script:ProjectPath,
        '--configuration', 'Release',
        "-p:SolutionDir=$($script:SourceRoot)\"
    )
    Invoke-NativeCommand -Command dotnet -Arguments $cleanArguments

    $buildArguments = @(
        'build', $script:ProjectPath,
        '--configuration', 'Release',
        '--no-incremental',
        '--disable-build-servers',
        "-p:Version=$PackageVersion",
        "-p:PackageVersion=$PackageVersion",
        "-p:AssemblyVersion=$AssemblyVersion",
        "-p:FileVersion=$AssemblyVersion",
        "-p:SolutionDir=$($script:SourceRoot)\"
    )
    Invoke-NativeCommand -Command dotnet -Arguments $buildArguments

    $outputPath = Join-Path $script:SourceRoot 'Unimake.DFe\Compilacao\Release\netstandard2.0'
    $files = @{
        DFeDll = Join-Path $outputPath 'Unimake.Business.DFe.dll'
        DFePdb = Join-Path $outputPath 'Unimake.Business.DFe.pdb'
        DFeXml = Join-Path $outputPath 'Unimake.Business.DFe.xml'
        CiotDll = $script:CiotSourcePath
    }
    foreach ($file in $files.Values) {
        if (-not (Test-Path -LiteralPath $file -PathType Leaf)) {
            throw "Arquivo obrigatório não encontrado após o build: $file"
        }
    }

    $dependencies = Get-DependencyVersions
    $nuspecPath = Join-Path $TemporaryPath 'Unimake.DFe.nuspec'
    New-Nuspec -PackageVersion $PackageVersion -ReleaseNotes $ReleaseNotes -OutputPath $nuspecPath -Dependencies $dependencies -Files $files

    Invoke-NativeCommand -Command dotnet -Arguments @('pack', $nuspecPath, '--output', $TemporaryPath)
    $packagePath = Join-Path $TemporaryPath "$($script:PackageId).$PackageVersion.nupkg"
    if (-not (Test-Path -LiteralPath $packagePath -PathType Leaf)) {
        throw "O pacote esperado não foi criado: $packagePath"
    }

    return [pscustomobject]@{
        PackagePath = $packagePath
        Dependencies = $dependencies
        SourceFiles = $files
    }
}

function Assert-EqualHash {
    param(
        [Parameter(Mandatory = $true)][string]$First,
        [Parameter(Mandatory = $true)][string]$Second,
        [Parameter(Mandatory = $true)][string]$Description
    )

    $firstHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $First).Hash
    $secondHash = (Get-FileHash -Algorithm SHA256 -LiteralPath $Second).Hash
    if ($firstHash -ne $secondHash) {
        throw "Conteúdo divergente: $Description."
    }
}

function Test-GeneratedPackage {
    param(
        [Parameter(Mandatory = $true)]$BuildResult,
        [Parameter(Mandatory = $true)][string]$PackageVersion,
        [Parameter(Mandatory = $true)][string]$AssemblyVersion,
        [Parameter(Mandatory = $true)][string]$ReleaseNotes,
        [Parameter(Mandatory = $true)][string]$TemporaryPath
    )

    $extractPath = Join-Path $TemporaryPath 'validated-package'
    [IO.Compression.ZipFile]::ExtractToDirectory($BuildResult.PackagePath, $extractPath)
    $required = @(
        'lib\netstandard2.0\Unimake.Business.DFe.dll',
        'lib\netstandard2.0\Unimake.Business.DFe.pdb',
        'lib\netstandard2.0\Unimake.Business.DFe.xml',
        'lib\netstandard2.0\GeradorCIOTShared.dll'
    )
    foreach ($relativePath in $required) {
        if (-not (Test-Path -LiteralPath (Join-Path $extractPath $relativePath) -PathType Leaf)) {
            throw "Arquivo ausente no pacote: $relativePath"
        }
    }

    $netStandardDll = Join-Path $extractPath 'lib\netstandard2.0\Unimake.Business.DFe.dll'
    $netStandardCiot = Join-Path $extractPath 'lib\netstandard2.0\GeradorCIOTShared.dll'
    Assert-EqualHash $netStandardCiot $script:CiotSourcePath 'GeradorCIOTShared.dll contra a origem'

    $actualAssemblyVersion = [Reflection.AssemblyName]::GetAssemblyName($netStandardDll).Version
    if ($actualAssemblyVersion -ne [Version]$AssemblyVersion) {
        throw "AssemblyVersion inválida. Esperado: $AssemblyVersion. Encontrado: $actualAssemblyVersion."
    }
    $actualFileVersion = [Diagnostics.FileVersionInfo]::GetVersionInfo($netStandardDll).FileVersion
    if ([Version]$actualFileVersion -ne [Version]$AssemblyVersion) {
        throw "FileVersion inválida. Esperado: $AssemblyVersion. Encontrado: $actualFileVersion."
    }

    $nuspecPath = Get-ChildItem -LiteralPath $extractPath -Filter '*.nuspec' -File | Select-Object -First 1 -ExpandProperty FullName
    [xml]$nuspec = Get-Content -Raw -LiteralPath $nuspecPath
    $namespace = New-Object Xml.XmlNamespaceManager($nuspec.NameTable)
    $namespace.AddNamespace('n', $nuspec.DocumentElement.NamespaceURI)
    $id = $nuspec.SelectSingleNode('/n:package/n:metadata/n:id', $namespace).InnerText
    $version = $nuspec.SelectSingleNode('/n:package/n:metadata/n:version', $namespace).InnerText
    $description = $nuspec.SelectSingleNode('/n:package/n:metadata/n:description', $namespace).InnerText
    $notes = $nuspec.SelectSingleNode('/n:package/n:metadata/n:releaseNotes', $namespace).InnerText
    if ($id -ne $script:PackageId) {
        throw "ID inválido no pacote. Esperado: $($script:PackageId). Encontrado: $id."
    }
    if ($version -ne $PackageVersion) {
        throw "Versão inválida no pacote. Esperado: $PackageVersion. Encontrado: $version."
    }
    if ($description -ne $script:PackageDescription) {
        throw 'A descrição institucional do pacote é inválida ou apresenta problema de codificação.'
    }

    $normalizedNotes = ($notes -replace "`r`n", "`n").Trim()
    $normalizedExpectedNotes = ($ReleaseNotes -replace "`r`n", "`n").Trim()
    if ($normalizedNotes -ne $normalizedExpectedNotes) {
        throw 'As release notes do pacote são diferentes das alterações obtidas no GitHub.'
    }

    $groups = @($nuspec.SelectNodes('/n:package/n:metadata/n:dependencies/n:group', $namespace))
    if ($groups.Count -ne 1) {
        throw 'O pacote deve conter somente o grupo de dependências .NETStandard2.0.'
    }
    foreach ($group in $groups) {
        if ([string]$group.targetFramework -ne '.NETStandard2.0') {
            throw "Framework inválido no grupo de dependências: $($group.targetFramework)."
        }
        $actualDependencies = @($group.SelectNodes('n:dependency', $namespace))
        if ($actualDependencies.Count -ne $BuildResult.Dependencies.Count) {
            throw "Quantidade de dependências inválida no grupo $($group.targetFramework)."
        }
        foreach ($dependency in $actualDependencies) {
            if ($BuildResult.Dependencies[[string]$dependency.id] -ne [string]$dependency.version) {
                throw "Versão inválida para a dependência $($dependency.id)."
            }
        }
    }
}

function Ensure-OfflineSource {
    if (-not (Test-Path -LiteralPath $script:OfflineFeed)) {
        New-Item -ItemType Directory -Path $script:OfflineFeed | Out-Null
    }

    $sources = @(Invoke-NativeCommand -Command dotnet -Arguments @('nuget', 'list', 'source') -CaptureOutput)
    $text = $sources -join [Environment]::NewLine
    if ($text -match [regex]::Escape($script:OfflineFeed)) {
        return
    }
    if ($text -match [regex]::Escape($script:OfflineSourceName)) {
        throw "Já existe uma fonte chamada '$($script:OfflineSourceName)' apontando para outro local."
    }

    Invoke-NativeCommand -Command dotnet -Arguments @('nuget', 'add', 'source', $script:OfflineFeed, '--name', $script:OfflineSourceName)
}

function Update-TrackedVersion {
    param([Parameter(Mandatory = $true)][string]$AssemblyVersion)

    $content = Get-Content -Raw -LiteralPath $script:ProjectPath
    $content = [regex]::Replace($content, '<AssemblyVersion>[^<]+</AssemblyVersion>', "<AssemblyVersion>$AssemblyVersion</AssemblyVersion>")
    $content = [regex]::Replace($content, '<FileVersion>[^<]+</FileVersion>', "<FileVersion>$AssemblyVersion</FileVersion>")
    $temporaryProject = "$($script:ProjectPath).packaging.tmp"
    [IO.File]::WriteAllText($temporaryProject, $content, [Text.UTF8Encoding]::new($false))
    Move-Item -LiteralPath $temporaryProject -Destination $script:ProjectPath -Force
}

function Update-TrackedPackage {
    param([Parameter(Mandatory = $true)][string]$PackagePath)

    $temporaryPackage = "$($script:TrackedPackagePath).packaging.tmp"
    Copy-Item -LiteralPath $PackagePath -Destination $temporaryPackage -Force
    Move-Item -LiteralPath $temporaryPackage -Destination $script:TrackedPackagePath -Force
}

function Show-Summary {
    param(
        [Parameter(Mandatory = $true)][string]$PackageVersion,
        [Parameter(Mandatory = $true)][string]$AssemblyVersion,
        [Parameter(Mandatory = $true)]$Interval,
        [Parameter(Mandatory = $true)][string]$ReleaseNotes
    )

    Write-Host ''
    Write-Host 'Pacote validado com sucesso.' -ForegroundColor Green
    Write-Host "Modo             : $Mode"
    Write-Host "PackageVersion   : $PackageVersion"
    Write-Host "AssemblyVersion  : $AssemblyVersion"
    Write-Host "Início           : $($Interval.StartLocal.ToString($script:DateFormat))"
    Write-Host "Fim              : $($Interval.EndLocal.ToString($script:DateFormat))"
    Write-Host 'Release notes:'
    Write-Host $ReleaseNotes
    Write-Host ''
}

try {
    if ($Mode -eq 'Public') {
        Test-PublicationPreconditions
    }

    $timeZone = Get-SaoPauloTimeZone
    $timeBeforeVersionCapture = [TimeZoneInfo]::ConvertTime([DateTimeOffset]::UtcNow, $timeZone).DateTime
    if ($timeBeforeVersionCapture.Second -lt 10) {
        # O NuGet remove zeros à esquerda dos segmentos numéricos (por exemplo, .05 vira .5).
        # Aguarde a faixa canônica para preservar exatamente o formato yyyyMMdd.HHmm.ss.
        Start-Sleep -Seconds (10 - $timeBeforeVersionCapture.Second)
    }

    $versionTime = [TimeZoneInfo]::ConvertTime([DateTimeOffset]::UtcNow, $timeZone).DateTime
    $packageVersion = $versionTime.ToString('yyyyMMdd.HHmm.ss', [Globalization.CultureInfo]::InvariantCulture)
    $assemblyVersion = $versionTime.ToString('yyyy.MM.dd.HHmm', [Globalization.CultureInfo]::InvariantCulture)
    Test-PackageDoesNotExist -PackageVersion $packageVersion

    $interval = Get-ReleaseInterval
    $releaseNotes = Get-ReleaseNotes -Interval $interval
    $temporaryPath = Join-Path ([IO.Path]::GetTempPath()) ("Unimake.DFe.Packaging." + [Guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $temporaryPath | Out-Null

    try {
        $buildResult = Build-Package -PackageVersion $packageVersion -AssemblyVersion $assemblyVersion -ReleaseNotes $releaseNotes -TemporaryPath $temporaryPath
        Test-GeneratedPackage -BuildResult $buildResult -PackageVersion $packageVersion -AssemblyVersion $assemblyVersion -ReleaseNotes $releaseNotes -TemporaryPath $temporaryPath
        Show-Summary -PackageVersion $packageVersion -AssemblyVersion $assemblyVersion -Interval $interval -ReleaseNotes $releaseNotes

        if ($DryRun) {
            Write-Host 'DryRun concluído: nenhum pacote foi enviado e nenhum arquivo rastreado foi alterado.' -ForegroundColor Yellow
            exit 0
        }

        if ($Mode -eq 'Offline') {
            Ensure-OfflineSource
            Invoke-NativeCommand -Command dotnet -Arguments @('nuget', 'push', $buildResult.PackagePath, '--source', $script:OfflineFeed)
            Write-Host "Pacote enviado para $($script:OfflineFeed)." -ForegroundColor Green
            Write-Host 'Atualização manual no Package Manager Console:'
            Write-Host "Update-Package Unimake.DFe -Version $packageVersion -Source `"$($script:OfflineFeed)`""
            Write-Host 'Não faça commit das referências enquanto esta versão existir somente no feed offline.' -ForegroundColor Yellow
            exit 0
        }

        $confirmation = Read-Host 'Publicar esta versão no nuget.org? Digite S para confirmar'
        if ($confirmation -notmatch '^(?i:s|sim)$') {
            Write-Host 'Publicação cancelada. Nenhum arquivo rastreado foi alterado.' -ForegroundColor Yellow
            exit 0
        }

        Update-TrackedVersion -AssemblyVersion $assemblyVersion
        Update-TrackedPackage -PackagePath $buildResult.PackagePath
        Invoke-NativeCommand -Command dotnet -Arguments @('nuget', 'push', $buildResult.PackagePath, '--source', $script:NuGetSource)
        Write-Host 'Pacote publicado com sucesso. Revise e faça commit das alterações locais.' -ForegroundColor Green
    }
    finally {
        if (Test-Path -LiteralPath $temporaryPath) {
            Remove-Item -LiteralPath $temporaryPath -Recurse -Force
        }
    }
}
catch {
    Write-Error "$($_.Exception.Message)`n$($_.ScriptStackTrace)"
    exit 1
}
