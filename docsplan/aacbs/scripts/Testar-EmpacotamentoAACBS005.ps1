#Requires -Version 7.6
# Regressão do manifesto oficial; não cria pacote, restaura dependências ou publica.
Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$repository = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$packaging = Join-Path $repository 'source/Packaging/Pack-DFe.ps1'
$script:ProjectPath = Join-Path $repository 'source/.NET Standard/Unimake.Business.DFe/Unimake.Business.DFe.csproj'
$script:TemplatePath = Join-Path $repository 'source/Packaging/Unimake.DFe.nuspec.template'
$script:PackageDescription = 'Descrição sintética para conferir o manifesto oficial.'
$errors = $null; $tokens = $null
$ast = [Management.Automation.Language.Parser]::ParseFile($packaging, [ref]$tokens, [ref]$errors)
if ($errors.Count) { throw 'Sintaxe invalida no empacotamento oficial.' }
foreach ($name in @('Get-DependencyVersions', 'ConvertTo-XmlText', 'New-Nuspec', 'Test-GeneratedPackage')) {
    $functions = @($ast.FindAll({param($node) $node -is [Management.Automation.Language.FunctionDefinitionAst] -and $node.Name -ceq $name}, $true))
    if ($functions.Count -ne 1) { throw 'Funcao oficial de manifesto ausente ou ambigua.' }
    . ([scriptblock]::Create($functions[0].Extent.Text))
}
$dependencies = Get-DependencyVersions
[xml]$project = Get-Content -LiteralPath $script:ProjectPath -Raw
$reference = $project.SelectSingleNode('/Project/ItemGroup/PackageReference[@Include="System.Net.Http.Json"]')
if ($null -eq $reference -or -not $dependencies.ContainsKey('System.Net.Http.Json') -or
    $dependencies['System.Net.Http.Json'] -cne [string]$reference.Version) {
    throw 'O manifesto precisa exportar System.Net.Http.Json na mesma versao compilada.'
}
$temporary = Join-Path ([IO.Path]::GetTempPath()) ('aacbs005-manifesto-' + [Guid]::NewGuid().ToString('N') + '.nuspec')
try {
    $files = @{DFeDll='exemplo.dll';DFePdb='exemplo.pdb';DFeXml='exemplo.xml';CiotDll='ciot.dll'}
    New-Nuspec -PackageVersion '0.0.0' -ReleaseNotes 'Teste sintetico do manifesto.' -OutputPath $temporary -Dependencies $dependencies -Files $files
    [xml]$manifest = Get-Content -LiteralPath $temporary -Raw
    $ns = [Xml.XmlNamespaceManager]::new($manifest.NameTable)
    $ns.AddNamespace('n', 'http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd')
    $exported = @($manifest.SelectNodes('/n:package/n:metadata/n:dependencies/n:group/n:dependency', $ns))
    $httpJson = @($exported | Where-Object { $_.id -ceq 'System.Net.Http.Json' })
    if ($httpJson.Count -ne 1 -or $httpJson[0].version -cne [string]$reference.Version -or
        $exported.Count -ne $dependencies.Count) { throw 'Dependencias divergentes no template oficial.' }
    if ($manifest.OuterXml.Contains('{{')) { throw 'Marcador de template nao resolvido.' }
    Write-Output 'Regressao de empacotamento aprovada: dependencia HTTP JSON exportada na versao compilada; nenhum pacote criado ou publicado.'
}
finally { if ([IO.File]::Exists($temporary)) { [IO.File]::Delete($temporary) } }

# Arquivos ZIP sintéticos exercitam somente a rejeição de assets extras,
# antes de ler assemblies. Não são pacotes distribuíveis nem usam outro fluxo de pack.
Add-Type -AssemblyName System.IO.Compression.FileSystem
foreach ($extra in @('ref/net472/Unimake.Business.DFe.dll', 'runtimes/win/lib/net472/Unimake.Business.DFe.dll')) {
    $testRoot = Join-Path ([IO.Path]::GetTempPath()) ('aacbs005-assets-' + [Guid]::NewGuid().ToString('N'))
    [void][IO.Directory]::CreateDirectory($testRoot)
    try {
        $zipPath = Join-Path $testRoot 'sintetico.zip'
        $zip = [IO.Compression.ZipFile]::Open($zipPath, [IO.Compression.ZipArchiveMode]::Create)
        try {
            foreach ($entry in @('lib/netstandard2.0/Unimake.Business.DFe.dll', 'lib/netstandard2.0/Unimake.Business.DFe.pdb',
                'lib/netstandard2.0/Unimake.Business.DFe.xml', 'lib/netstandard2.0/GeradorCIOTShared.dll', $extra)) {
                [void]$zip.CreateEntry($entry)
            }
        }
        finally { $zip.Dispose() }
        $rejected = $false
        try {
            Test-GeneratedPackage -BuildResult @{PackagePath=$zipPath} -PackageVersion '0.0.0' -AssemblyVersion '0.0.0.0' -ReleaseNotes 'Sintetico' -TemporaryPath $testRoot
        }
        catch {
            if ($_.Exception.Message -cne 'O pacote contém arquivos fora dos quatro assets e metadados NuGet previstos.') { throw }
            $rejected = $true
        }
        if (-not $rejected) { throw 'Asset extra de compilacao ou runtime aceito indevidamente.' }
    }
    finally {
        $resolvedTestRoot = [IO.Path]::GetFullPath($testRoot)
        $temporaryRoot = [IO.Path]::GetFullPath([IO.Path]::GetTempPath()).TrimEnd('\') + '\'
        if (-not $resolvedTestRoot.StartsWith($temporaryRoot, [StringComparison]::OrdinalIgnoreCase) -or
            [IO.Path]::GetFileName($resolvedTestRoot) -notmatch '^aacbs005-assets-[a-f0-9]{32}$') {
            throw 'Diretorio temporario de teste fora do limite esperado.'
        }
        Remove-Item -LiteralPath $resolvedTestRoot -Recurse -Force
    }
}
Write-Output 'Regressao de assets aprovada: ref e runtimes externos aos quatro arquivos foram rejeitados.'
