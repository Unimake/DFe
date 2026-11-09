[CmdletBinding()]
param()
$ErrorActionPreference = 'Stop'
$repo = [IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../..'))
$base = Join-Path $repo 'source/.NET Standard/Unimake.Business.DFe/Xml/Schemas/NFSe'
$saida = Join-Path $repo 'source/.NET Standard/Unimake.Business.DFe/Utility/Disponibilidade/CatalogoRespostasNFSe.cs'
$docs = @{}
foreach ($file in Get-ChildItem $base -Recurse -Filter '*.xsd' | Sort-Object FullName) {
    [xml]$xml = [IO.File]::ReadAllText($file.FullName)
    $relativo = [IO.Path]::GetRelativePath($base, $file.FullName).Replace('\', '/')
    $docs[$relativo] = $xml
}
function Campos($node, $xml, $padrao, $nivel = 0) {
    if ($nivel -gt 8) { return }
    $tipo = $node.GetAttribute('type')
    if ($tipo) {
        $local = ($tipo -split ':')[-1]
        $declaracao = $xml.DocumentElement.SelectSingleNode("*[local-name()='complexType' and @name='$local']")
        if ($declaracao) { Campos $declaracao $xml $padrao ($nivel + 1) }
    }
    foreach ($child in $node.ChildNodes) {
        if ($child.LocalName -eq 'element') {
            $nome = $child.GetAttribute('name')
            if (!$nome) { $nome = ($child.GetAttribute('ref') -split ':')[-1] }
            if ($nome -and $nome -ne 'Signature') { $nome }
        } elseif ($child.LocalName -in @('complexType', 'sequence', 'choice', 'all', 'complexContent', 'extension')) {
            Campos $child $xml $padrao ($nivel + 1)
        }
    }
}
$regras = @{}
$origens = [Collections.Generic.List[object]]::new()
foreach ($relativo in $docs.Keys | Sort-Object) {
    $xml = $docs[$relativo]
    $padrao = ($relativo -split '/')[0]
    if ($padrao -eq 'NACIONAL') { continue } # Contratos JSON/XML nacionais têm reconhecimento próprio.
    foreach ($node in $xml.DocumentElement.ChildNodes) {
        if ($node.LocalName -ne 'element') { continue }
        $nome = $node.GetAttribute('name')
        if ($nome -notmatch '(Resposta$|Response$|^Retorno)' -or $nome -match '^Lista') { continue }
        $campos = @(Campos $node $xml $padrao | Sort-Object -Unique)
        if (!$campos.Count) { continue }
        $ns = $xml.DocumentElement.GetAttribute('targetNamespace')
        $chave = "$padrao|$ns|$nome"
        $regras[$chave] = @($regras[$chave]) + $campos | Where-Object { $_ } | Sort-Object -Unique
        $origens.Add([pscustomobject]@{Padrao=$padrao;Namespace=$ns;Raiz=$nome;Campos=($campos -join ';');Schema=$relativo;SHA256=(Get-FileHash (Join-Path $base $relativo) -Algorithm SHA256).Hash})
    }
}
$linhas = [Collections.Generic.List[string]]::new()
$linhas.Add('// Gerado por docsplan/diagnostico/Gerar-CatalogoRespostasNFSe.ps1 a partir dos XSDs versionados.')
$linhas.Add('// Somente nomes de contratos; não altera nem valida o XML fiscal, não lê schemas no caminho da emissão.')
$linhas.Add('using System.Collections.Generic;')
$linhas.Add('using System.Xml;')
$linhas.Add('using Unimake.Business.DFe.Servicos;')
$linhas.Add('namespace Unimake.Business.DFe.Utility')
$linhas.Add('{')
$linhas.Add('    internal static class CatalogoRespostasNFSe')
$linhas.Add('    {')
$linhas.Add('        private static readonly Dictionary<string, string> Campos = new Dictionary<string, string>')
$linhas.Add('        {')
foreach ($chave in $regras.Keys | Sort-Object) {
    $linhas.Add('            { "' + $chave + '", "|' + ($regras[$chave] -join '|') + '|" },')
}
$linhas.Add('        };')
$linhas.Add('        private static readonly Dictionary<string, string> SemNamespace = CriarSemNamespace();')
$linhas.Add('        private static Dictionary<string, string> CriarSemNamespace()')
$linhas.Add('        {')
$linhas.Add('            var resultado = new Dictionary<string, string>();')
$linhas.Add('            foreach (var item in Campos)')
$linhas.Add('            {')
$linhas.Add('                var partes = item.Key.Split(''|'');')
$linhas.Add('                var chave = partes[0] + "||" + partes[2];')
$linhas.Add('                string anterior;')
$linhas.Add('                resultado.TryGetValue(chave, out anterior);')
$linhas.Add('                resultado[chave] = anterior + item.Value;')
$linhas.Add('            }')
$linhas.Add('            return resultado;')
$linhas.Add('        }')
$linhas.Add('        internal static bool Aceita(PadraoNFSe padrao, XmlElement raiz)')
$linhas.Add('        {')
$linhas.Add('            string campos;')
$linhas.Add('            if (raiz == null) return false;')
$linhas.Add('            var contratos = raiz.NamespaceURI.Length == 0 ? SemNamespace : Campos;')
$linhas.Add('            if (!contratos.TryGetValue(padrao + "|" + raiz.NamespaceURI + "|" + raiz.LocalName, out campos)) return false;')
$linhas.Add('            foreach (XmlNode child in raiz.ChildNodes)')
$linhas.Add('                if (child is XmlElement && campos.Contains("|" + child.LocalName + "|")) return true;')
$linhas.Add('            return false;')
$linhas.Add('        }')
$linhas.Add('    }')
$linhas.Add('}')
[IO.File]::WriteAllLines($saida, $linhas, [Text.UTF8Encoding]::new($false))
$origens | Export-Csv (Join-Path $PSScriptRoot 'CONTRATOS-NFSE.csv') -Encoding utf8 -NoTypeInformation
Write-Output ('Contratos municipais distintos: ' + $regras.Count + '; declarações XSD: ' + $origens.Count)
