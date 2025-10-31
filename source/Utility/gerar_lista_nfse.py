import os
import xml.etree.ElementTree as ET
from pathlib import Path
import locale

# Configurações
BASE_DIR = Path(__file__).parent
CONFIG_XML = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "Config.xml").resolve()
NFSE_DIR = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "NFSe").resolve()
OUTPUT_FILE = "ListaMunicipiosNFSe.txt"

try:
    locale.setlocale(locale.LC_COLLATE, 'pt_BR.UTF-8')
except:
    locale.setlocale(locale.LC_COLLATE, '')

def escape_wiki(t): return str(t).replace("|", "{{!}}")
def is_estado(id): return len(id) == 2 and id.isdigit()

def get_dados_xml(caminho):
    try:
        root = ET.parse(caminho).getroot()
        versao = ""
        servicos = root.find("Servicos")
        if servicos is not None:
            for s in servicos:
                versao = s.get("versao") or s.findtext("SchemaVersao", "")
                if versao: break
        if not versao: return "", False, False

        prod = any(root.findtext(f".//{t}") for t in ["WebEnderecoProducao", "RequestURIProducao"])
        homo = any(root.findtext(f".//{t}") for t in ["WebEnderecoHomologacao", "RequestURIHomologacao"])
        return versao, prod, homo
    except:
        return "", False, False

def main():
    # print("Caminho esperado para Config.xml:")
    # print(CONFIG_XML)
    # print("Existe?", CONFIG_XML.exists())

    # print("Caminho esperado para CidadeBR.xml:")
    # print(NFSE_DIR)
    # print("Existe?", NFSE_DIR.exists())


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

        # === SUBSTITUI EL POR E&L NO TEXTO ===
        padrao_exibido = "E&L" if padrao == "EL" else padrao

        entradas.append((nome, uf, ibge, padrao, padrao_exibido, versao, prod, homo))

    # Ordena com acentos
    entradas.sort(key=lambda x: locale.strxfrm(x[0]))

    # Gera Wiki
    lines = [
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
        # Usa p_exib no texto, mas p no link (mantém compatibilidade)
        link = f"[[Manuais:UniNFe/MunicipiosMonitor/{p}|{p_exib}]]"
        lines += [
            f"||{escape_wiki(n)}||{u}||{i}||{link}||"
            f"<div style=\"text-align: center;\">{v}</div>||"
            f"<div style=\"text-align: center;\">{'Sim' if pr else 'Não'}</div>||"
            f"<div style=\"text-align: center;\">{'Sim' if h else 'Não'}</div>",
            "|-"
        ]
    lines.append("|}")

    Path(OUTPUT_FILE).write_text("\n".join(lines), encoding="utf-8")
    print(f"Gerado: {OUTPUT_FILE} ({len(entradas)} entradas)")

if __name__ == "__main__":
    main()