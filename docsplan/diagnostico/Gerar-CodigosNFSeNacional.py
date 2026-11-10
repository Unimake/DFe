"""Gera somente IDs de regras oficiais. Uso: python Gerar-CodigosNFSeNacional.py arquivo.xlsx.

Não baixa documentos nem altera schemas/modelos fiscais. Exige a fonte auditada;
uma atualização oficial requer revisar o hash e as políticas de certificado.
"""
import hashlib
from pathlib import Path
import re
import sys
import xml.etree.ElementTree as ET
from zipfile import ZipFile

fonte = Path(sys.argv[1])
esperado = "DE5BC492959EADC8BFA7540E16939995924F2188F743648EAF84D3B31E9EEB7C"
if hashlib.sha256(fonte.read_bytes()).hexdigest().upper() != esperado:
    raise SystemExit("Fonte diferente da auditada: revisar regras antes de atualizar o catálogo.")
ns = {"s": "http://schemas.openxmlformats.org/spreadsheetml/2006/main"}
codigos = set()
with ZipFile(fonte) as z:
    strings = ["".join(n.itertext()) for n in ET.fromstring(z.read("xl/sharedStrings.xml")).findall("s:si", ns)]
    for folha in (3, 5):  # RN_RECEPCAO_DPS e RN DPS_NFS-e do Anexo I v1.01.
        xml = ET.fromstring(z.read(f"xl/worksheets/sheet{folha}.xml"))
        for celula in xml.findall(".//s:c", ns):
            valor = celula.find("s:v", ns)
            if valor is None:
                continue
            texto = strings[int(valor.text)] if celula.get("t") == "s" else valor.text
            if re.fullmatch(r"E\d{4}", texto or ""):
                codigos.add(texto)
destino = Path(__file__).resolve().parents[2] / "source/.NET Standard/Unimake.Business.DFe/Utility/Disponibilidade/CodigosRespostaNFSeNacional.cs"
linhas = ["// Gerado por docsplan/diagnostico/Gerar-CodigosNFSeNacional.py; somente IDs de regras oficiais.",
          "// Anexo I v1.01 de 09/02/2026. SHA256 XLSX: " + esperado,
          "using System.Collections.Generic;", "namespace Unimake.Business.DFe.Utility", "{",
          "    internal static class CodigosRespostaNFSeNacional", "    {",
          "        internal static readonly HashSet<string> Conhecidos = new HashSet<string>", "        {"]
ordenados = sorted(codigos)
for inicio in range(0, len(ordenados), 12):
    linhas.append("            " + ", ".join('"' + c + '"' for c in ordenados[inicio:inicio + 12]) + ",")
linhas.extend(["        };", "    }", "}", ""])
destino.write_text("\n".join(linhas), encoding="utf-8")
print(f"IDs distintos: {len(codigos)}")
