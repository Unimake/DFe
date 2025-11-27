import xml.etree.ElementTree as ET
from pathlib import Path
import locale
import re
from collections import defaultdict

# Importação do módudo de config
from config_nfse import (
    ENUMS_CS,
    CONFIG_XML,
    NFSE_DIR,
    OUTPUT_DIR,
    EXCEPTION_NAME_MAP,
    EXCEPTION_NAME_FILE,
    DEFAULT_CONFIG,
    PADROES_CONFIG,
)

# Ordenação com acentos
try:
    locale.setlocale(locale.LC_COLLATE, 'pt_BR.UTF-8')
except Exception:
    locale.setlocale(locale.LC_COLLATE, '')

def escape_wiki(t: str) -> str:
    return str(t).replace("|", "{{!}}").replace("'", "&#39;")

# Leitura de enums
def ler_enums_valor() -> dict[str, int]:
    if not ENUMS_CS.exists():
        raise FileNotFoundError(f"Enums.cs não encontrado: {ENUMS_CS}")
    enum_valor: dict[str, int] = {}
    with open(ENUMS_CS, encoding="utf-8") as f:
        txt = f.read()
    for m in re.finditer(r'NFSe(\w+)\s*=\s*(\d+)', txt):
        enum_valor[m.group(1)] = int(m.group(2))
    return enum_valor

# Leitura de cidades
def ler_cidades() -> list[dict]:
    if not CONFIG_XML.exists():
        raise FileNotFoundError(f"Config.xml não encontrado: {CONFIG_XML}")
    tree = ET.parse(CONFIG_XML)
    cidades = []
    for arq in tree.findall(".//Arquivo"):
        id_arq = arq.get("ID", "")
        padrao = arq.findtext("PadraoNFSe", "").strip()
        arq_cfg = arq.findtext("ArqConfig", "").strip()
        if not padrao or (len(id_arq) == 2 and id_arq.isdigit()):
            continue
        xml_path = NFSE_DIR / arq_cfg
        if not xml_path.exists():
            continue
        cidades.append({"padrao": padrao, "xml_path": xml_path})
    return cidades

# Extração de serviços
def extrair_servicos_xml(xml_path: Path) -> dict[str, dict]:
    try:
        root = ET.parse(xml_path).getroot()
        servicos_elem = root.find(".//Servicos[@ID='NFSe']")
        if servicos_elem is None:
            return {}
        dados: dict[str, dict] = {}
        for elem in servicos_elem:
            tag = elem.tag
            versao = elem.get("versao", "")
            desc = elem.findtext("Descricao", tag).strip()
            if versao:
                dados[tag] = {"versao": versao, "descricao": desc}
        return dados
    except Exception as e:
        print(f"Erro ao ler {xml_path}: {e}")
        return {}

# Nome do arquivo de envio
def gerar_nome_exemplo_envio(tag_servico: str) -> str:
    excecoes = EXCEPTION_NAME_FILE
    if tag_servico in excecoes:
        return excecoes[tag_servico]
    base = re.sub(r'Nfse$', '', tag_servico)
    palavras = re.findall(r'[A-Z]?[a-z]+', base)
    if not palavras:
        return "-"
    sigla = ''.join(p[0].lower() for p in palavras if p)
    sufixo = "nfse.xml" if 'Nfse' in tag_servico else ".xml"
    return f"{tag_servico}Envio-ped-{sigla}{sufixo}"

# Construção das árvores
def construir_arvores(enum_valor: dict[str, int], cidades: list[dict]) -> tuple[dict, dict]:
    arvore_dll = defaultdict(lambda: defaultdict(dict))
    arvore_uninfe = defaultdict(lambda: defaultdict(dict))
    for cidade in cidades:
        padrao = cidade["padrao"]
        xml_path = cidade["xml_path"]
        servicos_xml = extrair_servicos_xml(xml_path)
        for tag, info in servicos_xml.items():
            versao = info["versao"]
            desc = info["descricao"]
            if tag not in arvore_dll[padrao][versao]:
                arvore_dll[padrao][versao][tag] = {
                    "descricao": desc,
                    "enum_valor": enum_valor.get(tag)
                }
            exemplo = gerar_nome_exemplo_envio(tag)
            arvore_uninfe[padrao][versao][tag] = {"exemplo": exemplo}
    return arvore_dll, arvore_uninfe

# Tabela UniNFe
def gerar_tabela_uninfe_wiki(padrao: str, versoes_dict: dict, uninfe_dict: dict, servico_info: dict) -> str:
    if not versoes_dict:
        return ""
    versoes = sorted(versoes_dict.keys(), key=lambda v: tuple(map(int, v.split('.'))))
    servicos_ordenados = sorted(
        servico_info.keys(),
        key=lambda s: locale.strxfrm(servico_info[s]["descricao"])
    )
    lines = ['{| class="wikitable" style="width:80%; border:1px solid #ccc; border-collapse:collapse; font-size:95%;"']
    header = '!scope="row"|\'\'\'Descrição do serviço\'\'\'||'
    for v in versoes:
        link = f"[https://www.unimake.com.br/uninfe/modelos.php?p=NFSe%2F{padrao.upper()}%2F{v} Exemplos versão {v}]"
        header += f'<div style="text-align: center;">\'\'\'{link}\'\'\'</div>||'
    header = header.rstrip('|')
    lines.append(header)
    lines.append('|-')
    verde = False
    for servico in servicos_ordenados:
        descricao = escape_wiki(servico_info[servico]["descricao"])
        linha = f"||<font color=#008000>{descricao}</font>||" if verde else f"||{descricao}||"
        for v in versoes:
            exemplo = uninfe_dict.get(v, {}).get(servico, {}).get("exemplo", "-")
            txt = escape_wiki(exemplo)
            if verde:
                linha += f"<font color=#008000><div style=\"text-align: center;\">{txt}</div></font>||"
            else:
                linha += f"<div style=\"text-align: center;\">{txt}</div>||"
        linha = linha.rstrip('|')
        lines.append(linha)
        lines.append('|-')
        verde = not verde
    lines.append('|}')
    return "\n".join(lines)

# Tabela DLL
def gerar_tabela_dll_wiki(padrao: str, versoes_dict: dict, enum_valor: dict[str, int]) -> str:
    if not versoes_dict:
        return ""
    versoes = sorted(versoes_dict.keys(), key=lambda v: tuple(map(int, v.split('.'))))
    servico_info: dict[str, dict] = {}
    todos = set()
    for versao, servicos in versoes_dict.items():
        for servico, info in servicos.items():
            todos.add(servico)
            if servico not in servico_info or len(info["descricao"]) > len(servico_info[servico]["descricao"]):
                servico_info[servico] = {
                    "descricao": info["descricao"],
                    "enum_valor": info["enum_valor"]
                }
    servicos_ordenados = sorted(todos, key=lambda s: locale.strxfrm(servico_info[s]["descricao"]))
    lines = ['{| class="wikitable" style="width:80%; border:1px solid #ccc; border-collapse:collapse; font-size:95%;"']
    header = "! '''Descrição do serviço''' ||"
    for v in versoes:
        link = f"[https://www.unimake.com.br/uninfe/modelos.php?p=nfse%2F{padrao.upper()}%2F{v} Exemplos versão {v}]"
        header += f'<div style="text-align: center;">\'\'\'{link}\'\'\'</div> ||'
    header += '<div style="text-align: center;">\'\'\'[https://www.unimake.com.br/uninfe/docdll/api/Unimake.Business.DFe.Servicos.Servico.html Enum do serviço]\'\'\'</div>'
    lines.append(header)
    lines.append('|-')
    verde = False
    for servico in servicos_ordenados:
        info = servico_info[servico]
        descricao = escape_wiki(info["descricao"])
        enum_val = info["enum_valor"]
        linha = f"||<font color=#008000>{descricao}</font>||" if verde else f"||{descricao}||"
        for v in versoes:
            if servico in versoes_dict[v]:
                txt = escape_wiki(servico)
                if verde:
                    linha += f"<font color=#008000><div style=\"text-align: center;\">{txt}</div></font>||"
                else:
                    linha += f"<div style=\"text-align: center;\">{txt}</div>||"
            else:
                linha += "<div style=\"text-align: center;\">-</div>||"
        if enum_val is not None:
            if verde:
                linha += f"<font color=#008000><div style=\"text-align: center;\">{enum_val}</div></font>"
            else:
                linha += f"<div style=\"text-align: center;\">{enum_val}</div>"
        else:
            linha += "<div style=\"text-align: center;\">-</div>"
        lines.append(linha)
        lines.append('|-')
        verde = not verde
    lines.append('|}')
    return "\n".join(lines)

# Geração da página de padrão completa
def gerar_pagina_completa(padrao: str, arvore_dll: dict, arvore_uninfe: dict, enum_valor: dict) -> str:
    config = PADROES_CONFIG.get(padrao, DEFAULT_CONFIG.copy())
    versoes = sorted(arvore_dll.keys(), key=lambda v: tuple(map(int, v.split('.'))))
    versoes_count = len(versoes)
    
    def obter_padrao_titulo(padrao_atual) -> str:
        for chave, valor in EXCEPTION_NAME_MAP.items():
            if padrao_atual.upper() == chave.upper():
                return valor
        return padrao_atual.upper()
    padrao_titulo = obter_padrao_titulo(padrao.upper())
    
    linhas = [f"== <span style=\"color:#008000;\">Padrão {padrao_titulo}</span> =="]

    # --- Notas únicas ---
    for nota in config.get("notas_unicas", []):
        linhas.append(nota)

    # --- WSDL ---
    if config.get("wsdl_unico", True):
        linhas.append(f"* Municípios no padrão {padrao_titulo} utilizam o mesmo WSDL para emissão de NFSe;")
    elif config.get("wsdl_diferente", False):
        linhas.append(f"* Municípios no padrão {padrao_titulo} utilizam WSDL's diferentes para emissão de NFSe, logo cada prefeitura pode ou não implementar os serviços listados abaixo;")

    # --- API ---
    if config.get("api", False):
        linhas.append(f"* Municípios no padrão {padrao_titulo} utilizam-se de API para os serviços relacionados a NFSe, logo cada prefeitura tem seu link de produção e homologação para emissão;")

    # --- Quantidade de versões ---
    if versoes_count == 1:
        linhas.append("* No UniNFe e DLL, tem uma versão de SCHEMA disponível.")
    else:
        linhas.append(f"* No UniNFe e DLL, tem {versoes_count} versões de SCHEMA disponíveis.")

    # --- Observações destacadas ---
    for obs in config.get("obs_unicas", []):
        linhas.append('<div style="background-color:#e3a8a8; border-left:4px solid red; padding:6px;">')
        linhas.append(f"Observação: {obs}")
        linhas.append('</div>')

    linhas.append("<br>")

    # --- Tabela UniNFe ---
    servico_info = {}
    for versao, servicos in arvore_dll.items():
        for servico, info in servicos.items():
            if servico not in servico_info or len(info["descricao"]) > len(servico_info[servico]["descricao"]):
                servico_info[servico] = {"descricao": info["descricao"], "enum_valor": info["enum_valor"]}

    linhas.append("=== <span style=\"color=#008000;\">Para uso no UniNFe</span> ===")
    tabela_uninfe = gerar_tabela_uninfe_wiki(padrao.upper(), arvore_dll, arvore_uninfe, servico_info)  # ← padrao_atual
    linhas.append(tabela_uninfe)
    linhas.append("<br>")

    # --- Tabela DLL ---
    linhas.append("=== <span style=\"color=#008000;\">Para uso na DLL</span> ===")
    tabela_dll = gerar_tabela_dll_wiki(padrao.upper(), arvore_dll, enum_valor)  # ← padrao_atual
    linhas.append(tabela_dll)
    linhas.append("<br>")

    # --- Exemplo na DLL ---
    if config.get("exemplo_dll_padrao", False):
        if versoes and len(versoes) == 1:
            versao_exemplo = versoes[0]
        else:
            versao_exemplo = "X.XX"

        # Inicializa com valor padrão
        mun_user_pass = ""
        if config["exemplo_user_pass"]:
            mun_user_pass = """    MunicipioUsuario = "12345678901234",
    MunicipioSenha = "123456","""

        servico_classe = config.get("exemplo_servico", "GerarNfse")
        codigo_municipio = config.get("codigo_mun", "codigo_ibge_do_municipio")

        exemplo_codigo = f"""==== <font color="#008000">Exemplo de uso na DLL em C#:</font> ====
<font color="#008000">Uso para todas versões</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    CodigoMunicipio = {codigo_municipio},
    Servico = Servico.NFSe{servico_classe},
    SchemaVersao = "{versao_exemplo}",
{mun_user_pass}
}};

var servico = new {servico_classe}(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>
<br>"""
        linhas.append(exemplo_codigo)

    # --- Exemplo na DLL (único/customizado) ---
    exemplo_unico = config.get("exemplo_dll_unico", "")
    if exemplo_unico:
        linhas.append(exemplo_unico)
        linhas.append("<br>")

    return "\n".join(map(str, linhas))

# Gera à página principal
def gerar_pagina_principal(todas_paginas: list[tuple[str, str]]) -> None:
    cabecalho = """__NOTITLE__
=<big><font color=#008000>'''Padrões de NFSe Desenvolvidos'''</font></big>=
Guia apresentando todos os padrões de NFSe desenvolvidos no UniNFe e na DLL Unimake.DFe. Confira abaixo:

<div style="background-color:#c5e6c5; border-left:4px solid #008000; padding:10px;">
💡 <b>Observação:</b> Em caso de dúvidas acerca de um padrão específico, clique no link da versão no cabeçalho das tabelas para visualizar exemplos de XML de envio disponíveis.
</div>

<br>
"""

    paginas_ordenadas = sorted(todas_paginas, key=lambda x: x[0].lower())
    conteudo = "\n\n".join([f"{texto}" for nome, texto in paginas_ordenadas])

    rodape = """
= <span style="color:#008000;">Veja também</span> =
* [[Manuais:UniNFe|📘 UniNFe]]
* [[Manuais:Unimake.DFe|⚙️ Unimake.DFe]]
"""

    pagina_completa = f"{cabecalho}\n{conteudo}\n{rodape}"

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    out_file = OUTPUT_DIR / "NFSe_Todos.wiki.txt"
    out_file.write_text(pagina_completa, encoding="utf-8")
    print(f" → {out_file.name} (página geral consolidada)")

# Main
def main() -> None:
    print("Lendo Enums.cs (valor do enum)…")
    enum_valor = ler_enums_valor()
    print(f" → {len(enum_valor)} enums encontrados")

    print("Lendo Config.xml…")
    cidades = ler_cidades()
    print(f" → {len(cidades)} arquivos de configuração")

    print("Construindo árvores (dll + uninfe)…")
    arvore_dll, arvore_uninfe = construir_arvores(enum_valor, cidades)

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    print(f"Gerando páginas completas em: {OUTPUT_DIR}")
    gerados = 0
    todas_paginas = []

    for padrao, versoes_dll in sorted(arvore_dll.items()):
        if not versoes_dll:
            continue

        pagina = gerar_pagina_completa(padrao, versoes_dll, arvore_uninfe[padrao], enum_valor)
        out_file = OUTPUT_DIR / f"NFSe_{padrao}.wiki.txt"
        out_file.write_text(pagina, encoding="utf-8")
        todas_paginas.append((padrao, pagina))
        gerados += 1
        print(f" → {out_file.name} ({len(versoes_dll)} versões)")

    if gerados:
        print("Gerando página principal consolidada…")
        gerar_pagina_principal(todas_paginas)

    print(f"\nConcluído! {gerados} páginas geradas com sucesso.")

if __name__ == "__main__":
    main()