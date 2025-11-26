# config_nfse.py
from pathlib import Path
from collections import defaultdict
from typing import Dict

# ====================== CONFIGURAÇÕES GLOBAIS ======================
BASE_DIR = Path(__file__).parent
ENUMS_CS = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Enums" / "Enums.cs").resolve()
CONFIG_XML = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "Config.xml").resolve()
NFSE_DIR = (BASE_DIR / ".." / ".NET Standard" / "Unimake.Business.DFe" / "Servicos" / "Config" / "NFSe").resolve()
OUTPUT_DIR = BASE_DIR / "Wikis_NFSe"
OUTPUT_DIR.mkdir(exist_ok=True)

# ------------------- Mapeamento de nomes de padrões com exceção -------------------
EXCEPTION_NAME_MAP: Dict[str, str] = {
    "EL": "E&L"
}

EXCEPTION_NAME_FILE = {
    "GerarNfse": "GerarNfseEnvio-env-loterps.xml",
    "RecepcionarLoteRps": "RecepcionarLoteRpsEnvio-env-loterps.xml",
    "RecepcionarLoteRpsSincrono": "RecepcionarLoteRpsSincronoEnvio-env-loterps.xml",
    "CancelarNfse": "CancelarNfseEnvio-ped-cannfse.xml",
    "ConsultarLoteRps": "ConsultarLoteRpsEnvio-ped-loterps.xml",
    "ConsultarNfseFaixa": "ConsultarNfseFaixaEnvio-ped-sitnfse.xml",
    "ConsultarNfseRps": "ConsultarNfseRpsEnvio-ped-sitnfserps.xml",
    "ConsultarNfseServicoPrestado": "ConsultarNfseServicoPrestadoEnvio-ped-sitnfse.xml",
    "ConsultarNfseServicoTomado": "ConsultarNfseServicoTomadoEnvio-ped-sitnfsetom.xml",
    "SubstituirNfse": "SubstituirNfseEnvio-ped-substnfse.xml",
}

# ------------------- Default -------------------
DEFAULT_CONFIG: Dict = {
    "wsdl_unico": False,
    "wsdl_diferente": True,
    "api": False,
    "notas_unicas": [],
    "obs_unicas": [],
    "exemplo_dll_padrao": False,
    "exemplo_user_pass": False,
    "exemplo_servico": "",
    "codigo_mun": "codigo_ibge_do_municipio",
    "exemplo_dll_unico": ""
}

# ------------------- Configurações de exceção -------------------
PADROES_CONFIG: Dict[str, Dict] = {
    "ABASE": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "9999902",
        "exemplo_dll_unico": ""
    },
    "ADM_SISTEMAS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": True,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "AGILI": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Municípios no padrão AGILI utilizam URLs diferentes para emissão de NFSe;"
            ],
        "obs_unicas": [
            "Padrão AGILI é trabalhado por API, logo cada contribuinte deve ter um login e senha para uso desse padrão."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "AVMB": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente o município de <b>Pelotas - RS</b> utiliza esse padrão;"
        ],
        "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "BAUHAUS": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [
            "Esse padrão exige um TOKEN para se comunicar com a API: No UniNFe, basta informar esse <b>TOKEN</b> no campo <b>senha</b>; Na DLL, basta informar esse <b>TOKEN</b> na propriedade <b>MunicipioToken</b> no objeto <b>Configuracao</b>."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": 
            """==== <font color="#008000">Exemplo de uso na DLL em C#:</font> ====
<font color="#008000">Versão 1.00</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    codigo_municipio = codigo_ibge_do_municipio,
    Servico = Servico.NFSeGerarNfse,
    SchemaVersao = "1.00"
    MunicipioToken = "TOKEN_AQUI"
}};

var servico = new GerarNfse(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>"""
    },
    "BETHA_CLOUD": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "9999907",
        "exemplo_dll_unico": ""
    },
    "BETHA": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "9999903",
        "exemplo_dll_unico": ""
    },
    "BSITBR": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Esse padrão não disponibiliza WSDL de homologação."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "CARIOCA": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Rio de Janeiro - RJ</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"],
        "obs_unicas": [
            "Veja como é feito o [https://wiki.unimake.com.br/images/f/fd/Montagem_link_impressao_carioca.png link de impressão] da NFSe em PDF."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "CENTI": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "CONAM": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "COPLAN": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "DBSELLER": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "SubstituirNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "DESENVOLVECIDADE": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "DSF": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Veja como é [[Manuais:UniNFe/MunicipiosMonitor/DSF/ConstruindoTagAssinatura|construída a tag <Assinatura>]] que é diferente da tag <Signature>.", 
            "Sobre São José dos Campos - SP: Aceita tanto versão 3.00 quanto versão 2.03."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "E_RECEITA": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "EGOVERNE": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Curitiba - PR</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
        "obs_unicas": [
            "Veja como é feito o [[Manuais:UniNFe/MunicipiosMonitor/EGOVERNE/LinkImpressao|link de impressão]] da NFSe em PDF."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "EGOVERNEISS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "WSDL de homologação é o mesmo de produção. Para emitir em homologação, deve-se alterar a tag <Homologacao> para <b>TRUE</b>."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "EnvioLoteRps",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "EL": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Esse padrão não disponibiliza WSDL de homologação, cada prefeitura tem que ter um usuário de teste no WSDL de produção."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico":
            """==== <font color="#008000">Exemplo de uso na DLL em C#:</font> ====
<font color="#008000">Versão 1.00</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    codigo_municipio = codigo_ibge_do_municipio,
    Servico = Servico.NFSeRecepcionarLoteRps,
    SchemaVersao = "1.00"
    MunicipioUsuario = "12345678901234",
    MunicipioSenha = "123456"
}};

var servico = new RecepcionarLoteRps(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>
<font color="#008000">Versão 2.04</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    codigo_municipio = codigo_ibge_do_municipio,
    Servico = Servico.NFSeGerarNfse,
    SchemaVersao = "2.04"
}};

var servico = new GerarNfse(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>"""
    },
    "ELOTECH": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Endereço de homologação é o mesmo de produção, mudando somente a tag <Homologa> para <b>TRUE</b> ou <b>FALSE</b>."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "EQUIPLANO": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "9999905",
        "exemplo_dll_unico": ""
    },
    "FINTEL": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "FIORILLI": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Cada contribuinte deve ter um usuário e senha no WSDL de produção e homologação."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": True,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "FISCO": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Não possui ambiente de homologação."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "9999906",
        "exemplo_dll_unico": ""
    },
    "GIAP": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [
            "No ambiente de homologação, o padrão GIAP oferece apenas o serviço de NotaFiscal.",
            "Para realizar o acesso da API, deve-se informar os campos usuário e senha, fornecidos pela prefeitura do município, no UniNFe, ao qual será composto por: Campo usuário: <b>Inscrição Municipal</b>; Campo senha: <b>Token</b>"
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "EnviarLoteNotas",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "GIAP": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_user_pass": False,
        "exemplo_dll_unico": ""
    },
    "GINFES": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Os municípios <b>Fortaleza - CE</b> e <b>Contagem - MG</b> utilizam a versão <b>2.00</b> para o serviço de cancelamento.",
           "Município de <b>São José dos Pinhais - PR</b> (Cód. IBGE 4125506) possui XMLs modificados que utilizam versão <b>3.00</b> disponibilizado nesse [https://www.unimake.com.br/uninfe/modelos.php?p=nfse%2FGINFES%2F3.00+-+Sao+Jose+dos+Pinhais+-+PR link.]"
           ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "9999900",
        "exemplo_dll_unico": ""
    },
    "GISSONLINE": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Veja como montar o [[Manuais:UniNFe/MunicipiosMonitor/GISSONLINE/LinkPDF|link do PDF para o padrão GISSONLINE.]]",
            "Sobre município de <b>Ribeirão Preto - SP</b>, para o ambiente de produção: XML de CancelarNfse não deve ter ID na tag ns2:InfPedidoCancelamento."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "HM2SOLUCOES": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [
            "Cada contribuinte deve ter um usuário e senha no WSDL de produção e homologação."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": True,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "IIBRASIL": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Cada contribuinte deve ter um usuário e senha no WSDL de produção e homologação."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "INDAIATUBA_SP": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Indaiatuba - SP</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "INTERSOL": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Municípios no padrão INTERSOL utilizam o mesmo WSDL de homologação e diferentes WSDL's de produção para emissão de NFSe, logo cada prefeitura pode ou não implementar os serviços listados abaixo."
            ],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "IPM": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "<b>Versão 1.00</b>: Municípios dessa versão utilizam uma única URL para os envios; Para enviar em homologação, é necessário acrescentar a tag <nfse_teste>1</nfse_teste> antes de <nf> para testes e retirar essa tag para enviar em produção.",
            "<b>Versão 1.20</b>: Municípios dessa versão utilizam uma URL própria de produção e homologação; Municípios que não possuem ambiente de homologação, é necessário acrescentar a tag <nfse_teste>1</nfse_teste> antes de <nf> para testes e retirar essa tag para enviar em produção."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": True,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "ISSNET": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "MANAUS_AM": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Manaus - AM</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "MEGASOFT": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "WSDL de homologação é o mesmo endereço de produção. Para colocar em homologação ou produção, tem que fazer contato para a prefeitura para que eles façam a mudança de um ambiente para outro."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "MEMORY": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "9999901",
        "exemplo_dll_unico": ""
    },
    "MODERNIZACAO_PUBLICA": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "NACIONAL": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "1001058",
        "exemplo_dll_unico": ""
    },
    "NATALENSE": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Natal - RN</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "NOBESISTEMAS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Os XMLs tem diferença na URL de envio para cada ambiente: produção e homologação."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "PAULISTANA": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>São Paulo - SP</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
        "obs_unicas": [
            "Esse padrão não disponibiliza WSDL de homologação. Para fazer testes de emissão, deve-se usar o XML com o nome <b>TesteEnvioLoteRps-env-loterps</b> disponibilizada na pasta de exemplos do padrão.",
            "Veja como é [[Manuais:UniNFe/MunicipiosMonitor/PAULISTANA/ConstruindoTagAssinatura|construída a tag <Assinatura>]] que é diferente da tag <Signature>.",
            "Veja como é feito o [https://wiki.unimake.com.br/images/b/b4/Montagem_link_impressão_pdf.png link de impressão] da NFSe em PDF."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "EnvioLoteRps",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "PORTAL_FACIL": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Os municípios desse padrão contém particularidades em cada XML logo, cada município tem sua pasta de exemplos. É importante analisar qual o município desejado e selecionar a pasta correta."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "PRIMAX": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "EmissaoNota",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "PRONIM": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "A partir de Janeiro de 2022 a PRONIM desativou alguns WSDL de homologação."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "PROPRIOBARUERISP": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Barueri - SP</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
        "obs_unicas": [
            "No uso do XML de GerarNfseEnvio: O ERP deve informar no campo ArquivoRPSBase64 o conteúdo sem ser no formato base64 pois o software utilizado (UniNFe ou DLL) irá fazer a conversão para base64."
            ],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "QUASAR": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Endereço de homologação é o mesmo de produção, mudando somente a tag <Producao> para 0 ou 1."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SALVADOR_BA": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Salvador - BA</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SH3": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "[https://wiki.unimake.com.br/images/0/02/Link_impressao_nfse_SH3.png Clique aqui] para saber como montar o link para impressão da NFSe."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SIGCORP": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "<b>Versão 1.03</b>: Somente Londrina - PR (Cód. IBGE 4113700) utiliza essa versão.",
            "<b>Versão 3.00</b>: Município de Rio Grande - RS (Cód. IBGE 4315602) possui XMLs modificados que utilizam versão 3.00 disponibilizado nesse [https://www.unimake.com.br/uninfe/modelos.php?p=nfse%2FSIGCORP-SIGISS%2F3.00+-+RioGrande+-+RS link]"
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SIGISSWEB": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [
            "Para realizar a conexão com a API deve-se informar no campo <b>usuário</b> e <b>senha</b> do UniNFe as credencias que são disponibilizadas pela prefeitura do município, ao qual o usuário será o <b>CNPJ</b> do contribuinte e a <b>senha precisa ser gerada no site da prefeitura.</b>",
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SIMPLISS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico":
            """==== <font color="#008000">Exemplo de uso na DLL em C#:</font> ====
<font color="#008000">Versão 2.03</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    codigo_municipio = codigo_ibge_do_municipio,
    Servico = Servico.NFSeGerarNfse,
    SchemaVersao = "2.03"
}};

var servico = new GerarNfse(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>
<font color="#008000">Versão 3.00</font>
<pre style="width: 80%;background-color:#c5e6c5;">
var configuracao = new Configuracao
{{
    TipoDFe = TipoDFe.NFSe,
    CertificadoDigital = CertificadoSelecionado,
    TipoAmbiente = TipoAmbiente.Producao,
    codigo_municipio = codigo_ibge_do_municipio,
    Servico = Servico.NFSeGerarNfse,
    SchemaVersao = "3.00"
    MunicipioUsuario = "12345678901234",
    MunicipioSenha = "123456"
}};

var servico = new GerarNfse(conteudoXML, configuracao);
servico.Executar();

var retornoPrefeitura = servico.RetornoWSString;
</pre>"""
    },
    "SINSOFT": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": True,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "SOFTPLAN": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": True,
        "notas_unicas": [],
        "obs_unicas": [
            "Cada contribuinte deve ter um usuário e senha, fornecidos pela prefeitura do município, para acessar a API e utilizar os serviços.",
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "TECNOSISTEMAS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "ConsultarSequenciaLoteNotaRPS",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "THEMA": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRps",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "TINUS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Município <b>Ipojuca - PE</b> possui XMLs modificados dos demais municípios, podem ser conferidos [https://www.unimake.com.br/uninfe/modelos.php?p=nfse%2FTINUS%2FIpojucaPE nesse link].",
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "TIPLAN": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "<b>Versão 2.01</b>: Municípios dessa versão não possuem WSDL de homologação, somente produção.",
            "<b>Versão 2.03</b>: Municípios dessa versão possuem WSDL de produção e homologação."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "TRIBUTUS": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Municípios no padrão TRIBUTUS utilizam o mesmo WSDL para produção e homologação diferenciando-se pelo TOKEN."
            ],
        "obs_unicas": [
            "Esse padrão exige um TOKEN para usar o ambiente de homologação e um TOKEN para o ambiente de produção para cada contribuinte.",
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "VITORIA_ES": {
        "wsdl_unico": False,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [
            "* Somente <b>Vitória - ES</b> utiliza esse padrão, pois foi feito especialmente para essa prefeitura;"
            ],
            "obs_unicas": [],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "WEBFISCO": {
        "wsdl_unico": True,
        "wsdl_diferente": False,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "RecepcionarLoteRpsSincrono",
        "codigo_mun": "9999904",
        "exemplo_dll_unico": ""
    },
    "WEBISS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [
            "Municípios de versão 1.00, utilizam WSDL de produção e homologação diferentes.",
            "Municípios de versão 2.02, utilizam WSDL de produção diferente e o mesmo WSDL de homologação."
            ],
        "exemplo_dll_padrao": False,
        "exemplo_user_pass": False,
        "exemplo_servico": "",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    },
    "WEBISS": {
        "wsdl_unico": False,
        "wsdl_diferente": True,
        "api": False,
        "notas_unicas": [],
        "obs_unicas": [],
        "exemplo_dll_padrao": True,
        "exemplo_user_pass": False,
        "exemplo_servico": "GerarNfse",
        "codigo_mun": "codigo_ibge_do_municipio",
        "exemplo_dll_unico": ""
    }
}