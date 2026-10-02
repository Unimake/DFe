param(
    [switch]$Check
)

$ErrorActionPreference = 'Stop'

$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..\..\..\..'))
$catalogPath = Join-Path $repoRoot 'source/.NET Standard/Unimake.Business.DFe/Xml/NFe/Txt/NFeTxtLayoutCatalog.cs'
$outputPath = Join-Path $repoRoot 'LAYOUT-TXT-NFE-NFCE.md'

if (-not [System.IO.File]::Exists($catalogPath)) {
    throw "Catálogo não encontrado: $catalogPath"
}

$source = [System.IO.File]::ReadAllText($catalogPath)
$entryPattern = 'layouts\.Add\("(?<key>[^"]+)",\s*prefix\s*\+\s*"(?<layout>[^"]+)"\);'
$entries = [System.Text.RegularExpressions.Regex]::Matches($source, $entryPattern)
$declarations = [System.Text.RegularExpressions.Regex]::Matches($source, 'layouts\.Add\s*\(')

if ($entries.Count -eq 0 -or $entries.Count -ne $declarations.Count) {
    throw "Catálogo não interpretado por completo: $($entries.Count) entradas reconhecidas de $($declarations.Count) declarações."
}

$groups = [ordered]@{}
foreach ($entry in $entries) {
    $key = $entry.Groups['key'].Value
    $layout = $entry.Groups['layout'].Value
    if ($layout -notmatch '^[A-Za-z][A-Za-z0-9]*\|.*\|$') {
        throw "Layout inválido na chave ${key}: $layout"
    }

    $group = $layout.Substring(0, 1).ToUpperInvariant()
    if (-not $groups.Contains($group)) {
        $groups[$group] = New-Object 'System.Collections.Generic.List[string]'
    }

    $segment = $layout.Substring(0, $layout.IndexOf('|'))
    if ($key -cne $segment.ToUpperInvariant()) {
        $groups[$group].Add("# $key")
    }
    $groups[$group].Add($layout)
}

$lines = New-Object 'System.Collections.Generic.List[string]'
$lines.Add('# Layout TXT da NF-e/NFC-e')
$lines.Add('')
$lines.Add('Fonte: catálogo atual `NFeTxtLayoutCatalog` da DLL `Unimake.DFe`. Cada linha mostra o segmento seguido dos campos na ordem aceita pelo conversor; o marcador interno `§` foi omitido. Nas variantes, o identificador após `#` corresponde à chave usada pelo resolvedor do layout.')
$lines.Add('')

foreach ($group in $groups.Keys) {
    $lines.Add("## $group")
    $lines.Add('')
    $lines.Add('```text')
    foreach ($line in $groups[$group]) {
        $lines.Add($line)
    }
    $lines.Add('```')
    $lines.Add('')
}

$expected = [string]::Join("`n", $lines) + "`n"
$utf8 = New-Object System.Text.UTF8Encoding($false)

if ($Check) {
    if (-not [System.IO.File]::Exists($outputPath)) {
        throw "Markdown não encontrado: $outputPath"
    }
    $actual = [System.IO.File]::ReadAllText($outputPath).Replace("`r`n", "`n")
    if ($actual -cne $expected) {
        throw "O Markdown diverge do catálogo: $outputPath"
    }
    Write-Output "Conferidos $($entries.Count) layouts: $outputPath"
    return
}

[System.IO.File]::WriteAllText($outputPath, $expected, $utf8)
Write-Output "Atualizados $($entries.Count) layouts: $outputPath"
