import os
import xml.etree.ElementTree as ET
from pathlib import Path
import locale

# Configurações
BASE_DIR = Path(__file__).parent
CONFIG_XML = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "Config.xml").resolve()
NFSE_DIR = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "NFSe").resolve()
OUTPUT_FILE = BASE_DIR / "generated" / "lista_municipios_nfse.txt"

try:
    locale.setlocale(locale.LC_COLLATE, 'pt_BR.UTF-8')
except:
    locale.setlocale(locale.LC_COLLATE, '')

def escape_wiki(t): return str(t).replace("|", "{{!}}")
def is_estado(id): return len(id) == 2 and id.isdigit()

def get_dados_xml(caminho):
    try:
        root = ET.parse(caminho).getroot()
        versoes = set()
        
        servicos = root.find("Servicos")
        if servicos is not None:
            for s in servicos:
                v = s.get("versao") or s.findtext("SchemaVersao", "")
                if v:
                    versoes.add(v)
        
        if not versoes: 
            return "", False, False

        versao_final = ", ".join(sorted(list(versoes)))

        prod = any(root.findtext(f".//{t}") for t in ["WebEnderecoProducao", "RequestURIProducao"])
        homo = any(root.findtext(f".//{t}") for t in ["WebEnderecoHomologacao", "RequestURIHomologacao"])
        
        return versao_final, prod, homo
    except:
        return "", False, False

def main():

    if not os.path.exists(CONFIG_XML):
        print("Config.xml não encontrado!")
        return

    entradas = []
    tree = ET.parse(CONFIG_XML)

    for arq in tree.findall(".//Arquivo"):
        id_arq = arq.get("ID", "")
        nome = arq.findtext("Nome", "").strip()
        uf = arq.findtext("UF", "").strip()
        arq_config = arq.findtext("ArqConfig", "").strip()
        padrao = arq.findtext("PadraoNFSe", "").strip()

        if not padrao or is_estado(id_arq):
            continue

        if uf != "BR" and f" - {uf}" in nome:
            nome = nome.replace(f" - {uf}", "").strip()

        ibge = id_arq.zfill(7)
        xml_path = os.path.join(NFSE_DIR, arq_config)
        if not os.path.exists(xml_path):
            continue

        versao, prod, homo = get_dados_xml(xml_path)
        if not versao:
            continue

        padrao_exibido = "E&L" if padrao == "EL" else padrao

        entradas.append((nome, uf, ibge, padrao, padrao_exibido, versao, prod, homo))

    entradas.sort(key=lambda x: locale.strxfrm(x[0]))

    header_text = """__NOTITLE__
==<big><font color="#008000">'''Municípios implementados no UniNFe e DLL'''</font></big>==

Unificamos as antigas listas de municípios implementados no UniNFe e na DLL Unimake.DFe. Confira abaixo como ficou o novo formato:

====<font color="#008000">Observações</font>====
* Em caso de dúvidas sobre o padrão do município, clique no nome do padrão para visualizar os serviços disponíveis e exemplos de XML de envio.

===<font color="#008000">'''ATENÇÃO aos padrões BETHA, BETHA_CLOUD, GINFES, MEMORY, ABASE, WEBFISCO, EQUIPLANO, FISCO e NACIONAL'''</font>===

* No '''UniNFe''':
** Veja como configurar a [[Manuais:UniNFe/MunicipiosMonitor/ConfigTodosPadroesUnico|NFSe para padrões com um único WSDL para todos os municípios]] no monitor UniNFe.
* Na '''DLL Unimake.DFe''':
** Veja como configurar a [[Manuais:Unimake.DFe/MunicipiosDLL/ConfigTodosPadroesUnico|NFSe para padrões com um único WSDL para todos os municípios]] na DLL Unimake.DFe.
"""

    lines = [header_text]

    lines += [
        '{| class="wikitable" style="width: 100%;"',
        "|-",
        "! Município",
        "! UF",
        "! Código IBGE",
        "! Padrão",
        "! Versão SCHEMA",
        "! Ambiente Produção",
        "! Ambiente Homologação",
        "|-"
    ]

    for n, u, i, p, p_exib, v, pr, h in entradas:
        link = f"[[Manuais:UniNFe/MunicipiosDesenvolvidos/PadroesDesenvolvidos#Padr.C3.A3o_{p}|{p_exib}]]"
        lines += [
            f"||{escape_wiki(n)}||{u}||{i}||{link}||"
            f"<div style=\"text-align: center;\">{v}</div>||"
            f"<div style=\"text-align: center;\">{'Sim' if pr else 'Não'}</div>||"
            f"<div style=\"text-align: center;\">{'Sim' if h else 'Não'}</div>",
            "|-"
        ]

    lines.append("|}")

    footer_text = """
* [[Manuais:UniNFe|📘 UniNFe]]
* [[Manuais:Unimake.DFe|⚙️ Unimake.DFe]]
"""
    lines.append(footer_text)

    OUTPUT_FILE.parent.mkdir(parents=True, exist_ok=True)
    OUTPUT_FILE.write_text("\n".join(lines), encoding="utf-8")
    
    print(f"Gerado: {OUTPUT_FILE} ({len(entradas)} entradas)")

if __name__ == "__main__":
    main()