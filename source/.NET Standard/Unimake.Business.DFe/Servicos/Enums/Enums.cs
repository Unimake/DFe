using System.ComponentModel;
using System.Xml.Serialization;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Servicos
{
    #region Servico

    /// <summary>
    /// Serviços disponíveis na DLL para integração com os documentos fiscais eletrônicos
    /// </summary>
    public enum Servico
    {
        #region Serviço

        /// <summary>
        /// 0 - Consulta status serviço NFe/NFCe
        /// </summary>
        [Description("Consulta status do serviço da NFe/NFCe")]
        NFeStatusServico = 0,

        /// <summary>
        /// 1 - Consulta protocolo da NFe/NFCe
        /// </summary>
        [Description("Consulta situação da NFe/NFCe")]
        NFeConsultaProtocolo = 1,

        /// <summary>
        /// 2 - Consulta recibo NFe/NFCe
        /// </summary>
        [Description("Consulta recibo da NFe/NFCe")]
        NFeConsultaRecibo = 2,

        /// <summary>
        /// 3 - Inutilização de números da nota fiscal eletrônica
        /// </summary>
        [Description("Inutilização de números da NFe/NFCE")]
        NFeInutilizacao = 3,

        /// <summary>
        /// 4 - Consulta cadastro do contribuinte
        /// </summary>
        [Description("Consulta cadastro de contribuinte")]
        NFeConsultaCadastro = 4,

        /// <summary>
        /// 5 - Envio de Eventos (Cancelamento, CCe, EPEC, etc...)
        /// </summary>
        [Description("Envio de eventos da NFe/NFCe")]
        NFeRecepcaoEvento = 5,

        /// <summary>
        /// 6 - Envio do XML de lote de NFe/NFCe
        /// </summary>
        [Description("Autorização da NFe/NFCe")]
        NFeAutorizacao = 6,

        /// <summary>
        /// 7 - Envio do XML de consulta dos documentos fiscais eletrônicos destinados - NFe
        /// </summary>
        [Description("Distribuição de documentos fiscais eletrônicos da NFe")]
        NFeDistribuicaoDFe = 7,

        /// <summary>
        /// 8 - Consulta status serviço CTe
        /// </summary>
        [Description("Consulta status do serviço do CTe")]
        CTeStatusServico = 8,

        /// <summary>
        /// 9 - Consulta protocolo do CTe
        /// </summary>
        [Description("Consulta situação do CTe")]
        CTeConsultaProtocolo = 9,

        /// <summary>
        /// 11 - Envio do XML de consulta dos documentos fiscais eletrônicos destinados - CTe
        /// </summary>
        [Description("Distribuição de documentos fiscais eletrônicos do CTe")]
        CTeDistribuicaoDFe = 11,

        /// <summary>
        /// 12 - Consulta recibo CTe
        /// </summary>
        [Description("Consulta recibo do CTe")]
        CTeConsultaRecibo = 12,

        /// <summary>
        /// 13 - Envio do lote de XML de CTe - Assíncrono
        /// </summary>
        [Description("Autorização do CTe")]
        CTeAutorizacao = 13,

        /// <summary>
        /// 14 - Envio do XML de CTeOS
        /// </summary>
        [Description("Autorização do CTeOS")]
        CTeAutorizacaoOS = 14,

        /// <summary>
        /// 15 - Consulta status serviço MDFe
        /// </summary>
        [Description("Consulta status do serviço do MDFe")]
        MDFeStatusServico = 15,

        /// <summary>
        /// 16 - Consulta protocolo do MDFe
        /// </summary>
        [Description("Consulta situação do MDFe")]
        MDFeConsultaProtocolo = 16,

        /// <summary>
        /// 17 - Consulta recibo MDFe
        /// </summary>
        [Description("Consulta recibo do MDFe")]
        MDFeConsultaRecibo = 17,

        /// <summary>
        /// 18 - Consulta MDFe não encerrado
        /// </summary>
        [Description("Consulta de MDFe´s não encerrados")]
        MDFeConsultaNaoEnc = 18,

        /// <summary>
        /// 19 - Envio do XML de MDFe em lote no modo assíncrono
        /// </summary>
        [Description("Autorização do MDFe")]
        MDFeAutorizacao = 19,

        /// <summary>
        /// 20 - Envio do XML de MDFe no modo síncrono
        /// </summary>
        [Description("Autorização do MDFe")]
        MDFeAutorizacaoSinc = 20,

        /// <summary>
        /// 21 - GNRE - Consultar Configurações da UF
        /// </summary>
        [Description("GNRE - Consultar Configurações da UF")]
        GNREConsultaConfigUF = 21,

        /// <summary>
        /// 22 - GNRE - Consultar Resultado do Lote de GNRE enviado
        /// </summary>
        [Description("Consultar Resultado do Lote de GNRE enviado")]
        GNREConsultaResultadoLote = 22,

        /// <summary>
        /// 23 - GNRE - Enviar Lote de GNRE
        /// </summary>
        [Description("Enviar Lote de GNRE")]
        GNRELoteRecepcao = 23,

        /// <summary>
        /// 24 - Enviar o XML de cancelamento da NFS-e para prefeitura
        /// </summary>
        [Description("Cancelamento da NFS-e")]
        NFSeCancelarNfse = 24,

        /// <summary>
        /// 25 - Enviar o XML de consulta NFS-e do prestador para a prefeitura
        /// </summary>
        [Description("Consulta NFS-e do prestador")]
        NFSeConsultarNotaPrestador = 25,

        /// <summary>
        /// 26 - Enviar o XML de consulta NFS-e valida para a prefeitura
        /// </summary>
        [Description("Consulta NFS-e valida")]
        NFSeConsultarNotaValida = 26,

        /// <summary>
        /// 27 - Enviar o XML da NFS-e para a prefeitura
        /// </summary>
        [Description("Envio da NFS-e")]
        NFSeGerarNfse = 27,

        /// <summary>
        /// 28 - Enviar o XML do Lote de RPS para gerar NFSe para a prefeitura
        /// </summary>
        [Description("Envio do lote RPS para gerar NFSe")]
        NFSeRecepcionarLoteRps = 28,

        /// <summary>
        /// 29 - Enviar o XML do Lote de RPS no modo síncrono para gerar NFSe para a prefeitura
        /// </summary>
        [Description("Envio do lote RPS no modo síncrono para gerar NFSe")]
        NFSeRecepcionarLoteRpsSincrono = 29,

        /// <summary>
        /// 30 - Enviar o XML para Substituir NFSe
        /// </summary>
        [Description("Envio da substituição da NFS-e")]
        NFSeSubstituirNfse = 30,

        /// <summary>
        /// 31 - Enviar o XML de consulta lote RPS para a prefeitura
        /// </summary>
        [Description("Consulta lote RPS")]
        NFSeConsultarLoteRps = 31,

        /// <summary>
        /// 32 - Enviar o XML de consulta NFSe para a prefeitura
        /// </summary>
        [Description("Consulta NFSe")]
        NFSeConsultarNfse = 32,

        /// <summary>
        /// 33 - Enviar o XML de consulta NFSe serviço prestado para a prefeitura
        /// </summary>
        [Description("Consulta NFSe serviço prestado")]
        NFSeConsultarNfseServicoPrestado = 33,

        /// <summary>
        /// 34 - Enviar o XML de consulta NFSe serviço tomado para a prefeitura
        /// </summary>
        [Description("Consulta NFSe serviço tomado")]
        NFSeConsultarNfseServicoTomado = 34,

        /// <summary>
        /// 35 - Enviar o XML de consulta NFSe por faixa para a prefeitura
        /// </summary>
        [Description("Consulta NFSe por Faixa")]
        NFSeConsultarNfseFaixa = 35,

        /// <summary>
        /// 36 - Enviar o XML de consulta NFSe por RPS para a prefeitura
        /// </summary>
        [Description("Consulta NFSe por RPS")]
        NFSeConsultarNfsePorRps = 36,

        /// <summary>
        /// 37 - Enviar o XML de consulta/download do PDF NFSe para a prefeitura
        /// </summary>
        [Description("Consulta/Download do PDF da NFSe")]
        NFSeConsultarNfsePDF = 37,

        /// <summary>
        /// 38 - GNRE - Consultar Lote Recepção Consulta
        /// </summary>
        [Description("Consultar Lote Recepção Consulta")]
        GNREConsultaLoteRecepcao = 38,

        /// <summary>
        /// 39 - GNRE - Consultar Resultado do Lote da Consulta de GNRE enviado
        /// </summary>
        [Description("Consultar Resultado do Lote da Consulta de GNRE")]
        GNREConsultaResultadoLoteConsulta = 39,

        /// <summary>
        /// 40 - Enviar o XML de consulta situação lote RPS para a prefeitura
        /// </summary>
        [Description("Consultar Situação Lote Rps")]
        NFSeConsultarSituacaoLoteRps = 40,

        /// <summary>
        /// 41 - Enviar o XML de consulta NFS-e recebidas para a prefeitura
        /// </summary>
        [Description("Consulta NFSe Recebidas")]
        NFSeConsultaNFeRecebidas = 41,

        /// <summary>
        /// 42 - Enviar o XML de consulta NFS-e emitidas para a prefeitura
        /// </summary>
        [Description("Consulta NFSe Emitidas")]
        NFSeConsultaNFeEmitidas = 42,

        /// <summary>
        /// 43 - Enviar o XML de teste envio lote rps para a prefeitura
        /// </summary>
        [Description("Teste Envio Lote RPS")]
        NFSeTesteEnvioLoteRps = 43,

        /// <summary>
        /// 44 - Enviar o XML de envio lote rps para a prefeitura
        /// </summary>
        [Description("Envio Lote RPS")]
        NFSeEnvioLoteRps = 44,

        /// <summary>
        /// 45 - Enviar o XML de envio rps para a prefeitura
        /// </summary>
        [Description("Envio RPS")]
        NFSeEnvioRps = 45,

        /// <summary>
        /// 46 - Enviar o XML de cancelamento NFS-e para a prefeitura
        /// </summary>
        [Description("Cancelamento NFSe")]
        NFSeCancelamentoNfe = 46,

        /// <summary>
        /// 47 - Enviar o XML de consulta informações lote para a prefeitura
        /// </summary>
        [Description("Consulta Informações Lote")]
        NFSeConsultaInformacoesLote = 47,

        /// <summary>
        /// 48 - Enviar o XML de consulta lote para a prefeitura
        /// </summary>
        [Description("Consulta Lote")]
        NFSeConsultaLote = 48,

        /// <summary>
        /// 49 - Consulta Centralizada de Código GTIN (CCG)
        /// </summary>
        [Description("Consulta Centralizada de Código GTIN (CCG)")]
        CCGConsGTIN = 49,

        /// <summary>
        /// 50 - Enviar o XML de cancelamento da NFS-e para prefeitura
        /// </summary>
        [Description("Cancela NFS-e")]
        NFSeCancelaNota = 50,

        /// <summary>
        /// 51 - Enviar o XML de emissão da NFS-e para prefeitura
        /// </summary>
        [Description("Emissão NFS-e")]
        NFSeEmissaoNota = 51,

        /// <summary>
        /// 52 - Cancela o XML de emissão da NFS-e para prefeitura
        /// </summary>
        [Description("Cancela NFS-e")]
        NFSeCancelarNotaFiscal = 52,

        /// <summary>
        /// 53 - Consulta o XML de emissão da NFS-e para prefeitura
        /// </summary>
        [Description("Consulta NFS-e")]
        NFSeConsultaNotaFiscal = 53,

        /// <summary>
        /// 54 - Envia o lote de XML da NFS-e para prefeitura
        /// </summary>
        [Description("Envia o lote NFS-e")]
        NFSeEnviarLoteNotas = 54,

        /// <summary>
        /// 55 - Consultar Rps Serviço Prestado da NFS-e para prefeitura
        /// </summary>
        [Description("Consultar Rps Serviço Prestado")]
        NFSeConsultarRpsServicoPrestado = 55,

        /// <summary>
        /// 56 - Envio do XML de CTe - Síncrono
        /// </summary>
        [Description("Autorização do CTe")]
        CTeAutorizacaoSinc = 56,

        /// <summary>
        /// 57 - Obter Critica Lote de XML da NFS-e para prefeitura
        /// </summary>
        [Description("Obter Critica Lote")]
        NFSeObterCriticaLote = 57,

        /// <summary>
        /// 58 - Consultar URL da NFS-e para prefeitura
        /// </summary>
        [Description("Consultar Url Nfse")]
        NFSeConsultarUrlNfse = 58,

        /// <summary>
        /// 59 - Consultar dados cadastrais da NFS-e
        /// </summary>
        [Description("Consultar Dados Cadastrais")]
        NFSeConsultarDadosCadastrais = 59,

        /// <summary>
        /// 60 - Consultar RPS Disponivel
        /// </summary>
        [Description("Consulta RPS Disponivel")]
        NFSeConsultarRpsDisponivel = 60,

        /// <summary>
        /// 61 - Consultar sequencia lote nota RPS
        /// </summary>
        [Description("Consultar sequencia lote nota RPS")]
        NFSeConsultarSequenciaLoteNotaRPS = 61,

        /// <summary>
        /// 62 - Obter Nota Fiscal Xml da NFS-e
        /// </summary>
        [Description("Obter Nota Fiscal Xml")]
        NFSeObterNotaFiscalXml = 62,

        /// <summary>
        /// 63 - Inutilizacao NFSe
        /// </summary>
        [Description("Solicitar Inutilizacao NFSe")]
        NFSeSolicitacaoInutilizacao = 63,

        /// <summary>
        /// 64 - Consulta Requerimento de Cancelamento da NFSe
        /// </summary>
        [Description("Consulta Requerimento de Cancelamento NFSe")]
        NFSeConsultarRequerimentoCancelamento= 64,
        #endregion

        #region Gerais

        /// <summary>
        /// 9999 - Serviço não definido
        /// </summary>
        Nulo = 9999

        #endregion
    }

    #endregion

    #region TipoDFe

    /// <summary>
    /// Tipos de DFe´s existentes
    /// </summary>
    public enum TipoDFe
    {
        /// <summary>
        /// -1 - Evento desconhecido
        /// </summary>
        Desconhecido = -1,
        /// <summary>
        /// 0 - NF-e - Nota Fiscal Eletrônica 
        /// </summary>
        NFe = 0,
        /// <summary>
        /// 1 - NFC-e - Nota Fiscal de Venda a Consumidor Eletrônica
        /// </summary>
        NFCe = 1,
        /// <summary>
        /// 2 - CT-e - Conhecimento de Transporte Eletrônico
        /// </summary>
        CTe = 2,
        /// <summary>
        /// 3 - CT-e - Conhecimento de Transporte Eletrônico para Outros Serviços
        /// </summary>
        CTeOS = 3,
        /// <summary>
        /// 4 - MDF-e - Manifesto Eletrônico de Documentos Fiscais
        /// </summary>
        MDFe = 4,
        /// <summary>
        /// 5 - Nota Fiscal de Serviço Eletrônica
        /// </summary>
        NFSe = 5,
        /// <summary>
        /// 6 - CFe-SAT - Sistema Autenticador e Transmissor de Cupons Fiscais Eletrônicos
        /// </summary>
        SAT = 6,
        /// <summary>
        /// 7 - Cupom Fiscal Eletrônico
        /// </summary>
        CFe = 7,
        /// <summary>
        /// 8 - GNRE - Guia Nacional de Recolhimento de Tributos Estaduais
        /// </summary>
        GNRE = 8,
        /// <summary>
        /// 9 - SNCM - Sistema Nacional de Controle de Medicamentos
        /// </summary>
        SNCM = 9,
        /// <summary>
        /// 10 - CCG - Consulta Centralizada de GTIN
        /// </summary>
        CCG = 10,
        /// <summary>
        /// 11 - EFDReinf - Escrituração Fiscal Digital de Retenções e Outras Informações Fiscais
        /// </summary>
        EFDReinf = 11

    }

    #endregion       

    #region UF

    /// <summary>
    /// Unidades Federativas do Brasil (Tem como XmlEnum o nome abreviado da UF)
    /// </summary>
    public enum UFBrasil
    {
        /// <summary>
        /// 12 - Acre - AC
        /// </summary>
        AC = 12,

        /// <summary>
        /// 27 - Alagoas - AL
        /// </summary>
        AL = 27,

        /// <summary>
        /// 16 - Amapá - AP
        /// </summary>
        AP = 16,

        /// <summary>
        /// 13 - Amazonas - AM
        /// </summary>
        AM = 13,

        /// <summary>
        /// 29 - Bahia - BA
        /// </summary>
        BA = 29,

        /// <summary>
        /// 23 - Ceará - CE
        /// </summary>
        CE = 23,

        /// <summary>
        /// 53 - Distrito Federal - DF
        /// </summary>
        DF = 53,

        /// <summary>
        /// 32 - Espírito Santo - ES
        /// </summary>
        ES = 32,

        /// <summary>
        /// 52 - Goiás - GO
        /// </summary>
        GO = 52,

        /// <summary>
        /// 21 - Maranhão - MA
        /// </summary>
        MA = 21,

        /// <summary>
        /// 51 - Mato Grosso - MT
        /// </summary>
        MT = 51,

        /// <summary>
        /// 50 - Mato Grosso do Sul - MS
        /// </summary>
        MS = 50,

        /// <summary>
        /// 31 - Minas Gerais - MG
        /// </summary>
        MG = 31,

        /// <summary>
        /// 15 - Pará - PA
        /// </summary>
        PA = 15,

        /// <summary>
        /// 25 - Paraíba - PB
        /// </summary>
        PB = 25,

        /// <summary>
        /// 41 - Paraná - PR
        /// </summary>
        PR = 41,

        /// <summary>
        /// 26 - Pernambuco - PE
        /// </summary>
        PE = 26,

        /// <summary>
        /// 22 - Piauí - PI
        /// </summary>
        PI = 22,

        /// <summary>
        /// 33 - Rio de Janeiro - RJ
        /// </summary>
        RJ = 33,

        /// <summary>
        /// 24 - Rio Grande do Norte - RN
        /// </summary>
        RN = 24,

        /// <summary>
        /// 43 - Rio Grande do Sul - RS
        /// </summary>
        RS = 43,

        /// <summary>
        /// 11 - Rondônia - RO
        /// </summary>
        RO = 11,

        /// <summary>
        /// 14 - Roraima - RR
        /// </summary>
        RR = 14,

        /// <summary>
        /// 42 - Santa Catarina - SC
        /// </summary>
        SC = 42,

        /// <summary>
        /// 35 - São Paulo - SP
        /// </summary>
        SP = 35,

        /// <summary>
        /// 28 - Sergipe - SE
        /// </summary>
        SE = 28,

        /// <summary>
        /// 17 - Tocantins - TO
        /// </summary>
        TO = 17,

        ///<summary>
        /// 90 - SUFRAMA
        /// </summary>
        SUFRAMA = 90,

        /// <summary>
        /// 91 - RFB - Ambiente Nacional - AN
        /// </summary>
        AN = 91,

        //Existe um código 92, mas como não sei o que é e não encontrei nada na internet que identifique, não criei ainda este código no ENUM. Wandrey 30/01/2023

        /// <summary>
        /// 94 - SVCRS - Serviço Virtual de Contingência do Rio Grande do Sul
        /// </summary>
        SVCRS = 94,

        /// <summary>
        /// 95 - SVCRS - Serviço Virtual de Contingência de São Paulo
        /// </summary>
        SVCSP = 95,

        /// <summary>
        /// 96 - Sincronização de chaves do estado do Rio Grande do Sul com o Sistema Virtual de São Paulo
        /// </summary>
        SincChavesRSparaSVSP = 96,

        /// <summary>
        /// 99 - Exportação
        /// </summary>
        EX = 99,

        /// <summary>
        /// 0 - Não definido
        /// </summary>
        NaoDefinido = 0
    }

    #endregion

    #region TipoAmbiente

    /// <summary>
    /// Tipo ambiente DFe (NFe, CTe, MDFe, NFCe, etc...)
    /// </summary>
    public enum TipoAmbiente
    {
        /// <summary>
        /// 1 - Ambiente de Produção
        /// </summary>
        [XmlEnum("1")]
        Producao = 1,
        /// <summary>
        /// 2 - Ambiente de Homologação / Produção Restrita
        /// </summary>
        [XmlEnum("2")]
        Homologacao = 2
    }

    #endregion

    #region ModeloDFe

    /// <summary>
    /// Modelos dos DFes (NFe, CTe, MDFe, NFCe, etc...)
    /// </summary>
    public enum ModeloDFe
    {
        /// <summary>
        /// NF-e (Modelo: 55)
        /// </summary>
        [XmlEnum("55")]
        NFe = 55,

        /// <summary>
        /// NFC-e (Modelo: 65)
        /// </summary>
        [XmlEnum("65")]
        NFCe = 65,

        /// <summary>
        /// CT-e (Modelo: 57)
        /// </summary>
        [XmlEnum("57")]
        CTe = 57,

        /// <summary>
        /// MDF-e (Modelo: 58)
        /// </summary>
        [XmlEnum("58")]
        MDFe = 58,

        /// <summary>
        /// CTeOS (Modelo: 67)
        /// </summary>
        [XmlEnum("67")]
        CTeOS = 67
    }

    #endregion

    #region TipoEventoNFe

    /// <summary>
    /// Tipos de eventos da NFe e NFCe
    /// </summary>
    public enum TipoEventoNFe
    {
        /// <summary>
        /// 0 - Evento desconhecido
        /// </summary>
        [XmlEnum("0")]
        Desconhecido = 0,

        /// <summary>
        /// 110110 - Carta de correção eletrônica
        /// </summary>
        [XmlEnum("110110")]
        CartaCorrecao = 110110,

        /// <summary>
        /// 110111 - Cancelamento NFe
        /// </summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>
        /// 110112 - Cancelamento da NFCe sendo substituída por outra NFCe
        /// </summary>
        [XmlEnum("110112")]
        CancelamentoPorSubstituicao = 110112,

        /// <summary>
        /// 110130 - Comprovante de Entrega da NF-e
        /// </summary>
        [XmlEnum("110130")]
        ComprovanteEntregaNFe = 110130,

        /// <summary>
        /// 110131 - Cancelamento do Comprovante de Entrega da NF-e
        /// </summary>
        [XmlEnum("110131")]
        CancelamentoComprovanteEntregaNFe = 110131,

        /// <summary>
        /// 110140 - EPEC - Evento Prévio de Emissão em Contingência
        /// </summary>
        [XmlEnum("110140")]
        EPEC = 110140,

        /// <summary>
        /// 111500 - Pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 1o Prazo
        /// </summary>
        [XmlEnum("111500")]
        PedidoProrrogacaoPrazo1 = 111500,

        /// <summary>
        /// 111501 - Pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 2o Prazo
        /// </summary>
        [XmlEnum("111501")]
        PedidoProrrogacaoPrazo2 = 111501,

        /// <summary>
        /// 111502 - Cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 1o Prazo
        /// </summary>
        [XmlEnum("111502")]
        CancelamentoPedidoProrrogacaoPrazo1 = 111502,

        /// <summary>
        /// 111503 - Cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 2o Prazo
        /// </summary>
        [XmlEnum("111503")]
        CancelamentoPedidoProrrogacaoPrazo2 = 111503,

        /// <summary>
        /// 210200 - Manifestação do Destinatário - Confirmação da Operação
        /// </summary>
        [XmlEnum("210200")]
        ManifestacaoConfirmacaoOperacao = 210200,

        /// <summary>
        /// 210210 - Manifestação do Destinatário - Ciência da Operação
        /// </summary>
        [XmlEnum("210210")]
        ManifestacaoCienciaOperacao = 210210,

        /// <summary>
        /// 210220 - Manifestação do Destinatário - Desconhecimento da Operação
        /// </summary>
        [XmlEnum("210220")]
        ManifestacaoDesconhecimentoOperacao = 210220,

        /// <summary>
        /// 210240 - Manifestação do Destinatário - Operação não realizada
        /// </summary>
        [XmlEnum("210240")]
        ManifestacaoOperacaoNaoRealizada = 210240,

        /// <summary>
        /// 400200 - SEFAZ do emitente declara que NF-e é um "Documento Fiscal Inidôneo".
        /// </summary>
        [XmlEnum("400200")]
        DocumentoFiscalInidoneo = 400200,

        /// <summary>
        /// 400201 - Cancelamento do evento 400200
        /// </summary>
        [XmlEnum("400201")]
        CancelamentoEventoFisco400200 = 400201,

        /// <summary>
        /// 400300 - Possibilita que a SEFAZ marque uma NF-e emitida em função de uma situação específica prevista em legislação, ex.: transferência de crédito, ressarcimento.
        /// </summary>
        [XmlEnum("400300")]
        VistoEletronicoDoFisco = 400300,

        /// <summary>
        /// 410300 - O evento da Nota Fiscal Referenciada é gerado sempre que uma nova NF-e referenciar uma ou mais outras Notas Fiscais Eletrônicas. Não serão gerados eventos de "NF-e Referenciada" para os documentos diferentes do Modelo 55.
        /// </summary>
        [XmlEnum("410300")]
        NFeReferenciada = 410300,

        /// <summary>
        /// 411500 - Resposta ao pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 1o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411500")]
        RespostaPedidoProrrogacaoPrazo1 = 411500,

        /// <summary>
        /// 411501 - Resposta ao pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 2o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411501")]
        RespostaPedidoProrrogacaoPrazo2 = 411501,

        /// <summary>
        /// 411502 - Resposta ao cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 1o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411502")]
        RespostaCancelamentoPedidoProrrogacaoPrazo1 = 411502,

        /// <summary>
        /// 411503 - Resposta ao cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização - 2o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411503")]
        RespostaCancelamentoPedidoProrrogacaoPrazo2 = 411503,

        /// <summary>
        /// 610130 - Comprovante de entrega do CTe
        /// </summary>
        [XmlEnum("610130")]
        ComprovantedeEntregaCTe = 610130,

        /// <summary>
        /// 610131 - Cancelamento Comprovante de Entrega do CTe
        /// </summary>
        [XmlEnum("610131")]
        CancelamentoComprovantedeEntregaCTe = 610131,

        /// <summary>
        /// 610500 - Registro de Passagem da NF-e no Posto Fiscal
        /// </summary>
        [XmlEnum("610500")]
        RegistroPassagemNFe = 610500,

        /// <summary>
        /// 610501 - Cancelamento do evento 610500
        /// </summary>
        [XmlEnum("610501")]
        CancelamentoRegistroPassagemNFe = 610501,

        /// <summary>
        /// 610510 - Registro de Passagem do MDF-e no Posto Fiscal, propagado pelo Sistema MDF-e.
        /// </summary>
        [XmlEnum("610510")]
        RegistroDePassagemMDFe = 610510,

        /// <summary>
        /// 610511 - Cancelamento do evento 610511
        /// </summary>
        [XmlEnum("610511")]
        CancelamentoRegistroDePassagemMDFe = 610511,

        /// <summary>
        /// 610514 - Registro de Passagem do MDF-e no Posto Fiscal, propagado pelo Ambiente Nacional. Nota: A Chave de Acesso da NF-e está vinculada a um CT-e citado no MDF-e.
        /// </summary>
        [XmlEnum("610514")]
        RegistroDePassagemMDFeComCTe = 610514,

        /// <summary>
        /// 610515 - Cancelamento do evento 610514.
        /// </summary>
        [XmlEnum("610515")]
        CancelamentoRegistroDePassagemMDFeComCTe = 610515,

        /// <summary>
        /// 610550 - Registro de Passagem do MDF-e, capturado por antenas do Projeto Brasil ID. Evento eliminado (BT 2017.002), substituído pelo Registro de Passagem Automático MDF-e.
        /// </summary>
        [XmlEnum("610550")]
        RegistroPassagemNFeBRId = 610550,

        /// <summary>
        /// 610552 - Registro de Passagem do MDF-e capturado de forma automática (antena, leitura de placa por OCR, etc.), propagado pelo Sistema MDFe. Nota: A Chave de Acesso da NF-e está citada no MDF-e.
        /// </summary>
        [XmlEnum("610552")]
        RegistroDePassagemAutomaticoMDFe = 610552,

        /// <summary>
        /// 610554 - Cancelamento do evento 610552
        /// </summary>
        [XmlEnum("610554")]
        RegistroDePassagemAutomaticoMDFeComCTe = 610554,

        /// <summary>
        /// 610600 - Documenta na NF-e a ocorrência de CT-e autorizado, no momento do compartilhamento do CT-e com o Ambiente Nacional. Nota: A Chave de Acesso da NF-e está citada no CTe.
        /// </summary>
        [XmlEnum("610600")]
        CTeAutorizado = 610600,

        /// <summary>
        /// 610601 - Documenta na NF-e a ocorrência de cancelamento de CT-e autorizado, no momento do compartilhamento do evento com o Ambiente Nacional. Nota: A Chave de Acesso da NF-e está citada no CT-e.
        /// </summary>
        [XmlEnum("610601")]
        CTeCancelado = 610601,

        /// <summary>
        /// 610610 - Evento que documenta na NF-e a ocorrência de MDF-e autorizado.Nota: A Chave de Acesso da NF-e está citada no MDF-e.
        /// </summary>
        [XmlEnum("610610")]
        MDFeAutorizado = 610610,

        /// <summary>
        /// 610611 - Cancelamento do MDF-e
        /// </summary>
        [XmlEnum("610611")]
        MDFeCancelado = 610611,

        /// <summary>
        /// 610614 - Evento que documenta na NF-e a ocorrência de MDF-e autorizado. Nota: A Chave de Acesso da NF-e está vinculada a um CT-e citado no MDF-e.
        /// </summary>
        [XmlEnum("610614")]
        MDFeAutorizadoComCTe = 610614,

        /// <summary>
        /// 610615 - Cancelamento do evento 610614.
        /// </summary>
        [XmlEnum("610615")]
        CancelamentoDoMDFeAutorizadoComCTe = 610615,

        /// <summary>
        /// 790700 - Evento que indica a quantidade de mercadoria na unidade tributável que foi efetivamente embarcada para o exterior referente a um certo item de uma NF-e. Gerado e enviado pelo sistema Portal Único do Comércio Exterior (PUCOMEX) Receita Federal do Brasil (RFB) para o Ambiente Nacional da NF-e
        /// </summary>
        [XmlEnum("790700")]
        AverbacaoDeExportacao = 790700,

        /// <summary>
        /// 630690 - Registro da ocorrência da Vistoria do processo de internalização de produtos industrializados de origem nacional com isenção de ICMS nas áreas sob controle da SUFRAMA.
        /// </summary>
        [XmlEnum("630690")]
        VistoriaSUFRAMASEFAZ = 630690,

        /// <summary>
        /// 990900 - Registro da ocorrência da Vistoria do processo de internalização de produtos industrializados de origem nacional com isenção de ICMS nas áreas sob controle da SUFRAMA.
        /// </summary>
        [XmlEnum("990900")]
        VistoriaSUFRAMA = 990900,

        /// <summary>
        /// 990910 - Confirmação da internalização de produtos industrializados de origem nacional com isenção de ICMS nas áreas sob controle da SUFRAMA.
        /// </summary>
        [XmlEnum("990910")]
        InternalizacaoSUFRAMA = 990910
    }

    #endregion

    #region TipoEventoCTe

    /// <summary>
    /// Tipos de eventos do CTe
    /// </summary>
    public enum TipoEventoCTe
    {
        /// <summary>
        /// 0 - Evento desconhecido
        /// </summary>
        [XmlEnum("0")]
        Desconhecido = 0,

        /// <summary>
        /// 110110 - Carta de Correção CTe
        /// </summary>
        [XmlEnum("110110")]
        CartaCorrecao = 110110,

        /// <summary>
        /// 110111 - Cancelamento CTe
        /// </summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>
        /// 110113 - EPEC = Evento Prévio da Emissão em Contingência do CTe
        /// </summary>
        [XmlEnum("110113")]
        EPEC = 110113,

        /// <summary>
        /// Comprovante de Entrega do CTe (110180)
        /// </summary>
        [XmlEnum("110180")]
        ComprovanteEntrega = 110180,

        /// <summary>
        /// 110181 - Cancelamento Comprovante de Entrega do CTe 
        /// </summary>
        [XmlEnum("110181")]
        CancelamentoComprovanteEntrega = 110181,

        /// <summary>
        /// 110190 - Evento de Insucesso na Entrega do CTe
        /// </summary>
        [XmlEnum("110190")]
        InsucessoEntrega = 110190,

        /// <summary>
        /// 110191 - Evento de Insucesso na Entrega do CTe
        /// </summary>
        [XmlEnum("110191")]
        CancelamentoInsucessoEntrega = 110191,

        /// <summary>
        /// 610110 - Evento de prestação de serviço em desacordo CTe
        /// </summary>
        [XmlEnum("610110")]
        PrestDesacordo = 610110,

        /// <summary>
        /// 610111 - Cancelamento evento prestação de serviço em desacordo CTe
        /// </summary>
        [XmlEnum("610111")]
        CancelamentoPrestDesacordo = 610111,

        /// <summary>
        /// 310610 - MDFe Autorizado (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("310610")]
        MDFeAutorizado = 310610,

        /// <summary>
        /// 310620 - Registro de Passagem CT-e (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("310620")]
        RegistroPassagem = 310620,

        /// <summary>
        /// 310611 - MDFe cancelado (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("310611")]
        MDFeCancelado = 310611,

        /// <summary>
        /// 510620 - Registro de Passagem Automático CTe (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("510620")]
        RegistroPassagemAutomatico = 510620
    }

    #endregion

    #region TipoEventoMDFe

    /// <summary>
    /// Tipos de eventos do MDFe
    /// </summary>
    public enum TipoEventoMDFe
    {
        /// <summary>
        /// Evento desconhecido (0)
        /// </summary>
        [XmlEnum("0")]
        Desconhecido = 0,
        /// <summary>
        /// Cancelamento de MDFe (110111)
        /// </summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>
        /// Encerramento de MDFe (110112)
        /// </summary>
        [XmlEnum("110112")]
        Encerramento = 110112,

        /// <summary>
        /// Inclusão de Condutor no MDFe (110114)
        /// </summary>
        [XmlEnum("110114")]
        InclusaoCondutor = 110114,

        /// <summary>
        /// Inclusão de DFe no MDFe (110115)
        /// </summary>
        [XmlEnum("110115")]
        InclusaoDFe = 110115,

        /// <summary>
        /// Pagamento da Operação de Transporte (MDFe) (110116)
        /// </summary>
        [XmlEnum("110116")]
        PagamentoOperacao = 110116,

        /// <summary>
        /// Confirmação do Serviço de Transporte (110117)
        /// </summary>
        [XmlEnum("110117")]
        ConfirmacaoServicoTransporte = 110117,

        /// <summary>
        /// Alteração Pagamento Serviço MDFe = 110118
        /// </summary>
        [XmlEnum("110118")]
        AlteracaoPagamentoServico = 110118,

        /// <summary>
        /// EncerramentoFisco (Evento exclusivo do fisco) (310112)
        /// </summary>
        [XmlEnum("310112")]
        EncerramentoFisco = 310112,

        /// <summary>
        /// Registro de passagem (Evento exclusivo do fisco) (310620)
        /// </summary>
        [XmlEnum("310620")]
        RegistroPassagem = 310620,

        /// <summary>
        /// Registro de passagem automático (Evento exclusivo outros (ONE)) (310620)
        /// </summary>
        [XmlEnum("510620")]
        RegistroPassagemBRId = 510620,

        /// <summary>
        /// Liberação Prazo Cancelamento  (Registro exclusivo do fisco emitente) (240170)
        /// </summary>
        [XmlEnum("240170")]
        LiberacaoPrazoCancelamento = 240170
    }

    #endregion

    #region SimNao

    /// <summary>
    /// Sim ou Não (1 ou 0)
    /// </summary>
    public enum SimNao
    {
        /// <summary>
        /// 0 - Não
        /// </summary>
        [XmlEnum("0")]
        Nao = 0,

        /// <summary>
        /// 1 - Sim
        /// </summary>
        [XmlEnum("1")]
        Sim = 1
    }

    #endregion

    #region SimNao12

    /// <summary>
    /// Sim ou Não (1 ou 2)
    /// </summary>
    public enum SimNao12
    {
        /// <summary>
        /// 1 - Sim
        /// </summary>
        [XmlEnum("1")]
        Sim = 1,

        /// <summary>
        /// 2 - Não
        /// </summary>
        [XmlEnum("2")]
        Nao = 2
    }

    #endregion

    #region TipoOperacao

    /// <summary>
    /// Tipo de operação (Entrada ou Saída)
    /// </summary>
    public enum TipoOperacao
    {
        /// <summary>
        /// 0 - Operação de entrada
        /// </summary>
        [XmlEnum("0")]
        Entrada = 0,

        /// <summary>
        /// 1 - Operação de saída
        /// </summary>
        [XmlEnum("1")]
        Saida = 1
    }

    #endregion

    #region DestinoOperacao

    /// <summary>
    /// Identificador do Destino da Operação
    /// </summary>
    public enum DestinoOperacao
    {
        /// <summary>
        /// 1 - Operação interna, ou seja, dentro do estado de origem
        /// </summary>
        [XmlEnum("1")]
        OperacaoInterna = 1,

        /// <summary>
        /// 2 - Operação interestadual, ou seja, estado diferente do de origem
        /// </summary>
        [XmlEnum("2")]
        OperacaoInterestadual = 2,

        /// <summary>
        /// 3 - Operação com o exterior, ou seja, fora do país de origem
        /// </summary>
        [XmlEnum("3")]
        OperacaoExterior = 3
    }

    #endregion

    #region FormatoImpressaoDANFE

    /// <summary>
    /// Formato de impressão do DANFE
    /// </summary>
    public enum FormatoImpressaoDANFE
    {
        /// <summary>
        /// 0 - Sem geração de DANFE
        /// </summary>
        [XmlEnum("0")]
        SemGeracao = 0,

        /// <summary>
        /// 1 - DANFE normal, Retrato
        /// </summary>
        [XmlEnum("1")]
        NormalRetrato = 1,

        /// <summary>
        /// 2 - DANFE normal, Paisagem
        /// </summary>
        [XmlEnum("2")]
        NormalPaisagem = 2,

        /// <summary>
        /// 3 - DANFE Simplificado
        /// </summary>
        [XmlEnum("3")]
        Simplificado = 3,

        /// <summary>
        /// 4 - DANFE NFC-e
        /// </summary>
        [XmlEnum("4")]
        NFCe = 4,

        /// <summary>
        /// 5 - DANFE NFC-e em mensagem eletrônica
        /// </summary>
        [XmlEnum("5")]
        NFCeMensagemEletronica = 5
    }

    #endregion

    #region FormatoImpressaoDACTE

    /// <summary>
    /// Formato de impressão do DACTE
    /// </summary>
    public enum FormatoImpressaoDACTE
    {
        /// <summary>
        /// 1 - Retrato
        /// </summary>
        [XmlEnum("1")]
        NormalRetrato = 1,

        /// <summary>
        /// 2 - Paisagem
        /// </summary>
        [XmlEnum("2")]
        NormalPaisagem = 2
    }

    #endregion

    #region TipoEmissao

    /// <summary>
    /// Tipo de emissão do DF-e (NFe, NFCe, CTe, MDFe, etc...)
    /// </summary>
    public enum TipoEmissao
    {
        /// <summary>
        /// 1 - Emissão normal (não em contingência)
        /// </summary>
        [XmlEnum("1")]
        Normal = 1,

        /// <summary>
        /// 2 - Contingência FS-IA, com impressão do DANFE em formulário de segurança ou Para MDFe é impressão em formulário branco (sulfite)
        /// </summary>
        [XmlEnum("2")]
        ContingenciaFSIA = 2,

        /// <summary>
        /// 3 - Regime Especial NFF (Nota Fiscal Fácil)
        /// </summary>
        [XmlEnum("3")]
        RegimeEspecialNFF,

        /// <summary>
        /// 4 - Contingência EPEC (Evento Prévio de Emissão em Contingência)
        /// </summary>
        [XmlEnum("4")]
        ContingenciaEPEC = 4,

        /// <summary>
        /// 5 - Contingência FS-DA, com impressão do DANFE em formulário de segurança;
        /// </summary>
        [XmlEnum("5")]
        ContingenciaFSDA = 5,

        /// <summary>
        /// 6 - Contingência SVC-AN (SEFAZ Virtual de Contingência do AN);
        /// </summary>
        [XmlEnum("6")]
        ContingenciaSVCAN = 6,

        /// <summary>
        /// 7 - Contingência SVC-RS (SEFAZ Virtual de Contingência do RS);
        /// </summary>
        [XmlEnum("7")]
        ContingenciaSVCRS = 7,

        /// <summary>
        /// 8 - Contingência SVC-SP (SEFAZ Virtual de Contingência de SP);
        /// </summary>
        [XmlEnum("8")]
        ContingenciaSVCSP = 8,

        /// <summary>
        /// 9 - Contingência off-line da NFC-e
        /// </summary>
        [XmlEnum("9")]
        ContingenciaOffLine = 9
    }

    #endregion

    #region FinalidadeNFe

    /// <summary>
    /// Finalidades da NFe/NFCe
    /// </summary>
    public enum FinalidadeNFe
    {
        /// <summary>
        /// 1 - NF-e normal
        /// </summary>
        [XmlEnum("1")]
        Normal = 1,

        /// <summary>
        /// 2 - NF-e complementar
        /// </summary>
        [XmlEnum("2")]
        Complementar = 2,

        /// <summary>
        /// 3 - NF-e de ajuste
        /// </summary>
        [XmlEnum("3")]
        Auste = 3,

        /// <summary>
        /// 4 - Devolução de mercadoria
        /// </summary>
        [XmlEnum("4")]
        Devolucao = 4
    }

    #endregion

    #region IndicadorPresenca

    /// <summary>
    /// Indicador de presença do comprador no estabelecimento comercial no momento da operação
    /// </summary>
    public enum IndicadorPresenca
    {
        /// <summary>
        /// 0 - Não se aplica (por exemplo, Nota Fiscal complementar ou de ajuste)
        /// </summary>
        [XmlEnum("0")]
        NaoSeAplica = 0,

        /// <summary>
        /// 1 - Operação presencial
        /// </summary>
        [XmlEnum("1")]
        OperacaoPresencial = 1,

        /// <summary>
        /// 2 - Operação não presencial, pela Internet
        /// </summary>
        [XmlEnum("2")]
        OperacaoInternet = 2,

        /// <summary>
        /// 3 - Operação não presencial, Teleatendimento
        /// </summary>
        [XmlEnum("3")]
        OperacaoTeleAtendimento = 3,

        /// <summary>
        /// 4 - NFC-e em operação com entrega a domicílio
        /// </summary>
        [XmlEnum("4")]
        NFCeEntregaDomicilio = 4,

        /// <summary>
        /// 5 - Operação presencial, fora do estabelecimento
        /// </summary>
        [XmlEnum("5")]
        [Description("Operação presencial, fora do estabelecimento")]
        PresencialForaEstabelecimento = 5,

        /// <summary>
        /// 9 - Operação não presencial, outros
        /// </summary>
        [XmlEnum("9")]
        OperacaoOutros = 9
    }

    #endregion

    #region ProcessoEmissao

    /// <summary>
    /// Processo de emissão do DFe (NFe, NFCe, CTe, etc...)
    /// </summary>
    public enum ProcessoEmissao
    {
        /// <summary>
        /// 0 - Emissão de NF-e com aplicativo do contribuinte
        /// </summary>
        [XmlEnum("0")]
        AplicativoContribuinte = 0,

        /// <summary>
        /// 1 - Emissão de NF-e avulsa pelo Fisco;
        /// </summary>
        [XmlEnum("1")]
        AvulsaPeloFisco = 1,

        /// <summary>
        /// 2 - Emissão de NF-e avulsa, pelo contribuinte com seu certificado digital, através do site do Fisco;
        /// </summary>
        [XmlEnum("2")]
        AvulsaPeloContribuinteSiteFisco = 2,

        /// <summary>
        /// 3 - Emissão NF-e pelo contribuinte com aplicativo fornecido pelo Fisco.
        /// </summary>
        [XmlEnum("3")]
        AplicativoFisco = 3
    }

    #endregion

    #region CRT

    /// <summary>
    /// Códigos de regimes tributários
    /// </summary>
    public enum CRT
    {
        /// <summary>
        /// 1 - Simples Nacional
        /// </summary>
        [XmlEnum("1")]
        SimplesNacional = 1,

        /// <summary>
        /// 2 - Simples Nacional, excesso sublimite de receita bruta
        /// </summary>
        [XmlEnum("2")]
        SimplesNacionalExcessoSublimite = 2,

        /// <summary>
        /// 3 - Regime Normal
        /// </summary>
        [XmlEnum("3")]
        RegimeNormal = 3,

        /// <summary>
        /// 4 - Simples Nacional - Microempreendedor Individual – MEI
        /// </summary>
        [XmlEnum("4")]
        SimplesNacionalMEI = 4
    }

    #endregion

    #region IndicadorIEDestinatario

    /// <summary>
    /// No caso de NFe e NFCe = Indicador da IE do Destinatário
    /// No caso de CTe = Indicador da IE do toma3 ou toma4
    /// </summary>
    public enum IndicadorIEDestinatario
    {
        /// <summary>
        /// 1 - Contribuinte ICMS (informar a IE do destinatário)
        /// </summary>
        [XmlEnum("1")]
        ContribuinteICMS = 1,

        /// <summary>
        /// 2 - Contribuinte isento de Inscrição no cadastro de Contribuintes do ICMS
        /// </summary>
        [XmlEnum("2")]
        ContribuinteIsento = 2,

        /// <summary>
        /// 9 - Não Contribuinte, que pode ou não possuir Inscrição Estadual no Cadastro de Contribuintes do ICMS
        /// </summary>
        [XmlEnum("9")]
        NaoContribuinte = 9
    }

    #endregion

    #region IndicadorEscalaRelevante

    /// <summary>
    /// Indicador de Escala de Relevante
    /// </summary>
    public enum IndicadorEscalaRelevante
    {
        /// <summary>
        /// S - Produzido em Escala Relevante
        /// </summary>
        [XmlEnum("S")]
        Sim,

        /// <summary>
        /// N – Produzido em Escala NÃO Relevante
        /// </summary>
        [XmlEnum("N")]
        Nao
    }

    #endregion

    #region ViaTransporteInternacional

    /// <summary>
    /// Via Transporte Internacional Informada na Declaração de Importação
    /// </summary>
    public enum ViaTransporteInternacional
    {
        /// <summary>
        /// 1 - Marítima
        /// </summary>
        [XmlEnum("1")]
        Maritima = 1,

        /// <summary>
        /// 2 - Fluvial
        /// </summary>
        [XmlEnum("2")]
        Fluvial = 2,

        /// <summary>
        /// 3 - Lacustre
        /// </summary>
        [XmlEnum("3")]
        Lacustre = 3,

        /// <summary>
        /// 4 - Aérea
        /// </summary>
        [XmlEnum("4")]
        Aerea = 4,

        /// <summary>
        /// 5 - Postal
        /// </summary>
        [XmlEnum("5")]
        Postal = 5,

        /// <summary>
        /// 6=Ferroviária
        /// </summary>
        [XmlEnum("6")]
        Ferroviaria = 6,

        /// <summary>
        /// 7 - Rodoviária
        /// </summary>
        [XmlEnum("7")]
        Rodoviaria = 7,

        /// <summary>
        /// 8 - Conduto / Rede Transmissão
        /// </summary>
        [XmlEnum("8")]
        CondutoRedeTransmissao = 8,

        /// <summary>
        /// 9 - Meios Próprios
        /// </summary>
        [XmlEnum("9")]
        MeiosProprios = 9,

        /// <summary>
        /// 10 - Entrada / Saída ficta
        /// </summary>
        [XmlEnum("10")]
        EntradaSaidaFicta = 10,

        /// <summary>
        /// 11 - Courier
        /// </summary>
        [XmlEnum("11")]
        Courier = 11,

        /// <summary>
        /// 12 - Em Mãos
        /// </summary>
        [XmlEnum("12")]
        EmMaos = 12,

        /// <summary>
        /// 13 - Por Reboque
        /// </summary>
        [XmlEnum("13")]
        PorReboque = 13
    }

    #endregion

    #region FormaImportacaoIntermediacao

    /// <summary>
    /// Forma de importação quanto a intermediação
    /// </summary>
    public enum FormaImportacaoIntermediacao
    {
        /// <summary>
        /// 1 - Importação por conta própria
        /// </summary>
        [XmlEnum("1")]
        ImportacaoPorContaPropria = 1,

        /// <summary>
        /// 2 - Importação por conta e ordem
        /// </summary>
        [XmlEnum("2")]
        ImportacaoPorContaOrdem = 2,

        /// <summary>
        /// 3 - Importação por encomenda
        /// </summary>
        [XmlEnum("3")]
        ImportacaoPorEncomenda = 3
    }

    #endregion

    #region OrigemMercadoria

    /// <summary>
    /// Origens das mercadorias
    /// </summary>
    public enum OrigemMercadoria
    {
        /// <summary>
        /// 0 - Nacional, exceto as indicadas nos códigos 3, 4, 5 e 8;
        /// </summary>
        [XmlEnum("0")]
        Nacional = 0,

        /// <summary>
        /// 1 - Estrangeira - Importação direta, exceto a indicada no código 6;
        /// </summary>
        [XmlEnum("1")]
        Estrangeira = 1,

        /// <summary>
        /// 2 - Estrangeira - Adquirida no mercado interno, exceto a indicada no código 7;
        /// </summary>
        [XmlEnum("2")]
        Estrangeira2 = 2,

        /// <summary>
        /// 3 - Nacional, mercadoria ou bem com Conteúdo de Importação superior a 40% e inferior ou igual a 70%;
        /// </summary>
        [XmlEnum("3")]
        Nacional3 = 3,

        /// <summary>
        /// 4 - Nacional, cuja produção tenha sido feita em conformidade com os processos produtivos básicos de que tratam as legislações citadas nos Ajustes;
        /// </summary>
        [XmlEnum("4")]
        Nacional4 = 4,

        /// <summary>
        /// 5 - Nacional, mercadoria ou bem com Conteúdo de Importação inferior ou igual a 40%;
        /// </summary>
        [XmlEnum("5")]
        Nacional5 = 5,

        /// <summary>
        /// 6 - Estrangeira - Importação direta, sem similar nacional, constante em lista da CAMEX e gás natural;
        /// </summary>
        [XmlEnum("6")]
        Estrangeira6 = 6,

        /// <summary>
        /// 7 - Estrangeira - Adquirida no mercado interno, sem similar nacional, constante lista CAMEX e gás natural.
        /// </summary>
        [XmlEnum("7")]
        Estrangeira7 = 7,

        /// <summary>
        /// 8 - Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70%;
        /// </summary>
        [XmlEnum("8")]
        Nacional8 = 8
    }

    #endregion

    #region ModalidadeBaseCalculoICMS

    /// <summary>
    /// Modalidades de determinação da Base de Cálculo do ICMS
    /// </summary>
    public enum ModalidadeBaseCalculoICMS
    {
        /// <summary>
        /// 0 - Margem Valor Agregado (%)
        /// </summary>
        [XmlEnum("0")]
        MargemValorAgregado = 0,

        /// <summary>
        /// 1 - Pauta (Valor)
        /// </summary>
        [XmlEnum("1")]
        Pauta = 1,

        /// <summary>
        /// 2 - Preço Tabelado Máx. (valor)
        /// </summary>
        [XmlEnum("2")]
        PrecoTabeladoMaximo = 2,

        /// <summary>
        /// 3 - Valor da operação
        /// </summary>
        [XmlEnum("3")]
        ValorOperacao = 3
    }

    #endregion

    #region ModalidadeBaseCalculoICMSST

    /// <summary>
    /// Modalidade de determinação da Basse de Cálculo do ICMS ST
    /// </summary>
    public enum ModalidadeBaseCalculoICMSST
    {
        /// <summary>
        /// 0 - Preço tabelado ou máximo sugerido
        /// </summary>
        [XmlEnum("0")]
        PrecoTabeladoMaximoSugerido = 0,

        /// <summary>
        /// 1 - Lista Negativa (valor)
        /// </summary>
        [XmlEnum("1")]
        ListaNegativa = 1,

        /// <summary>
        /// 2 - Lista Positiva (valor)
        /// </summary>
        [XmlEnum("2")]
        ListaPositiva = 2,

        /// <summary>
        /// 3 - Lista Neutra (valor)
        /// </summary>
        [XmlEnum("3")]
        ListaNeutra = 3,

        /// <summary>
        /// 4 - Margem Valor Agregado (%)
        /// </summary>
        [XmlEnum("4")]
        MargemValorAgregado = 4,

        /// <summary>
        /// 5 - Pauta (valor)
        /// </summary>
        [XmlEnum("5")]
        Pauta = 5,

        /// <summary>
        /// 6 - Valor da Operação
        /// </summary>
        [XmlEnum("6")]
        ValorOperacao = 6
    }

    #endregion

    #region IndicadorOrigemProcesso

    /// <summary>
    /// Indicador da Origem do Processo
    /// </summary>
    public enum IndicadorOrigemProcesso
    {
        /// <summary>
        /// 0 - SEFAZ
        /// </summary>
        [XmlEnum("0")]
        SEFAZ = 0,

        /// <summary>
        /// 1 - Justiça Federal
        /// </summary>
        [XmlEnum("1")]
        JusticaFederal = 1,

        /// <summary>
        /// 2 - Justiça Estadual
        /// </summary>
        [XmlEnum("2")]
        JusticaEstadual = 2,

        /// <summary>
        /// 3 - Secex/RFB
        /// </summary>
        [XmlEnum("3")]
        SecexRFB = 3,

        /// <summary>
        /// 9 - Outros
        /// </summary>
        [XmlEnum("9")]
        Outros = 9
    }

    #endregion

    #region MotivoDesoneracaoICMS

    /// <summary>
    /// Motivos para Desoneração do ICMS
    /// </summary>
    public enum MotivoDesoneracaoICMS
    {
        /// <summary>
        /// 1=Táxi
        /// </summary>
        [XmlEnum("1")]
        Taxi = 1,

        /// <summary>
        /// 3=Uso na agropecuária
        /// </summary>
        [XmlEnum("3")]
        UsoAgropecuaria = 3,

        /// <summary>
        /// 4=Frotista/Locadora
        /// </summary>
        [XmlEnum("4")]
        FrotistaLocadora = 4,

        /// <summary>
        /// 5=Diplomático/Consular
        /// </summary>
        [XmlEnum("5")]
        DiplomaticoConsular = 5,

        /// <summary>
        /// 6=Utilitários e Motocicletas da Amazônia Ocidental e Áreas de Livre Comércio (Resolução 714/88 e 790/94 – CONTRAN e suas alterações)
        /// </summary>
        [XmlEnum("6")]
        UtilitariosMotocicletas = 6,

        /// <summary>
        /// 7=SUFRAMA
        /// </summary>
        [XmlEnum("7")]
        SUFRAMA = 7,

        /// <summary>
        /// 8=Venda a órgão Público
        /// </summary>
        [XmlEnum("8")]
        VendaOrgaoPublico = 8,

        /// <summary>
        /// 9=Outros
        /// </summary>
        [XmlEnum("9")]
        Outro = 9,

        /// <summary>
        /// 10=Deficiente Condutor
        /// </summary>
        [XmlEnum("10")]
        DeficienteCondutor = 10,

        /// <summary>
        /// 11=Deficiente não condutor
        /// </summary>
        [XmlEnum("11")]
        DeficienteNaoCondutor = 11,

        /// <summary>
        /// 12=Órgão de fomento e desenvolvimento agropecuário
        /// </summary>
        [XmlEnum("12")]
        OrgaoFomentoDesenvolvimentoAgropecuario = 12,

        /// <summary>
        /// 16=Olimpíadas Rio 2016
        /// </summary>
        [XmlEnum("16")]
        OlimpiadasRio2016 = 16,

        /// <summary>
        /// 90=Solicitado pelo Fisco
        /// </summary>
        [XmlEnum("90")]
        SolicitadoPeloFisco = 90
    }

    #endregion

    #region ModalidadeFrete

    /// <summary>
    /// Modalidades de Frete
    /// </summary>
    public enum ModalidadeFrete
    {
        /// <summary>
        /// 0=Contratação do Frete por conta do Remetente (CIF); 
        /// </summary>
        [XmlEnum("0")]
        ContratacaoFretePorContaRemetente_CIF = 0,

        /// <summary>
        /// 1=Contratação do Frete por conta do Destinatário (FOB)
        /// </summary>
        [XmlEnum("1")]
        ContratacaoFretePorContaDestinatário_FOB = 1,

        /// <summary>
        /// 2=Contratação do Frete por conta de Terceiros
        /// </summary>
        [XmlEnum("2")]
        ContratacaoFretePorContaTerceiros = 2,

        /// <summary>
        /// 3=Transporte Próprio por conta do Remetente
        /// </summary>
        [XmlEnum("3")]
        TransporteProprioPorContaRemetente = 3,

        /// <summary>
        /// 4=Transporte Próprio por conta do Destinatário
        /// </summary>
        [XmlEnum("4")]
        TransporteProprioPorContaDestinatário = 4,

        /// <summary>
        /// 9=Sem Ocorrência de Transporte
        /// </summary>
        [XmlEnum("9")]
        SemOcorrenciaTransporte = 9
    }

    #endregion

    #region TipoIntegracaoPagamento

    /// <summary>
    /// Tipo de Integração do processo de pagamento com o sistema de automação da empresa
    /// </summary>
    public enum TipoIntegracaoPagamento
    {
        /// <summary>
        /// 1=Pagamento integrado com o sistema de automação da empresa (Ex.: equipamento TEF, Comércio Eletrônico)
        /// </summary>
        [XmlEnum("1")]
        PagamentoIntegrado = 1,
        /// <summary>
        /// 2= Pagamento não integrado com o sistema de automação da empresa (Ex.: equipamento POS)
        /// </summary>
        [XmlEnum("2")]
        PagamentoNaoIntegrado = 2
    }

    #endregion

    #region BandeiraOperadoraCartao

    /// <summary>
    /// Bandeira da operadora de cartão de crédito e/ou débito
    /// </summary>    
    public enum BandeiraOperadoraCartao
    {
        /// <summary>
        /// 01=Visa
        /// </summary>
        [XmlEnum("01")]
        Visa = 1,

        /// <summary>
        /// 02=Mastercard
        /// </summary>
        [XmlEnum("02")]
        Mastercard = 2,

        /// <summary>
        /// 03=American Express
        /// </summary>
        [XmlEnum("03")]
        AmericanExpress = 3,

        /// <summary>
        /// 04=Sorocred
        /// </summary>
        [XmlEnum("04")]
        Sorocred = 4,

        /// <summary>
        /// 05=Diners Club
        /// </summary>
        [XmlEnum("05")]
        DinersClub = 5,

        /// <summary>
        /// 06=Elo
        /// </summary>
        [XmlEnum("06")]
        Elo = 6,

        /// <summary>
        /// 07=Hipercard
        /// </summary>
        [XmlEnum("07")]
        Hipercard = 7,

        /// <summary>
        /// 08=Aura
        /// </summary>
        [XmlEnum("08")]
        Aura = 8,

        /// <summary>
        /// 09=Cabal
        /// </summary>
        [XmlEnum("09")]
        Cabal = 9,

        /// <summary>
        /// 10=Alelo
        /// </summary>
        [XmlEnum("10")]
        Alelo = 10,

        /// <summary>
        /// 11=Banes Card
        /// </summary>
        [XmlEnum("11")]
        BanesCard = 11,

        /// <summary>
        /// 12=CalCard
        /// </summary>
        [XmlEnum("12")]
        CalCard = 12,

        /// <summary>
        /// 13=Credz
        /// </summary>
        [XmlEnum("13")]
        Credz = 13,

        /// <summary>
        /// 14=Discover
        /// </summary>
        [XmlEnum("14")]
        Discover = 14,

        /// <summary>
        /// 15=GoodCard
        /// </summary>
        [XmlEnum("15")]
        GoodCard = 15,

        /// <summary>
        /// 16=GreenCard
        /// </summary>
        [XmlEnum("16")]
        GreenCard = 16,

        /// <summary>
        /// 17=Hiper
        /// </summary>
        [XmlEnum("17")]
        Hiper = 17,

        /// <summary>
        /// 18=JcB
        /// </summary>
        [XmlEnum("18")]
        JcB = 18,

        /// <summary>
        /// 19=Mais
        /// </summary>
        [XmlEnum("19")]
        Mais = 19,

        /// <summary>
        /// 20=MaxVan
        /// </summary>
        [XmlEnum("20")]
        MaxVan = 20,

        /// <summary>
        /// 21=Policard
        /// </summary>
        [XmlEnum("21")]
        Policard = 21,

        /// <summary>
        /// 22=RedeCompras
        /// </summary>
        [XmlEnum("22")]
        RedeCompras = 22,

        /// <summary>
        /// 23=Sodexo
        /// </summary>
        [XmlEnum("23")]
        Sodexo = 23,

        /// <summary>
        /// 24=ValeCard
        /// </summary>
        [XmlEnum("24")]
        ValeCard = 24,

        /// <summary>
        /// 25=Verocheque
        /// </summary>
        [XmlEnum("25")]
        Verocheque = 25,

        /// <summary>
        /// 26=VR
        /// </summary>
        [XmlEnum("26")]
        VR = 26,

        /// <summary>
        /// 27=Ticket
        /// </summary>
        [XmlEnum("27")]
        Ticket = 27,

        /// <summary>
        /// 99=Outros
        /// </summary>
        [XmlEnum("99")]
        Outros = 99
    }

    #endregion

    #region MeioPagamento

    /// <summary>
    /// Meios de pagamentos
    /// </summary>
    public enum MeioPagamento
    {
        /// <summary>
        /// 01=Dinheiro
        /// </summary>
        [XmlEnum("01")]
        Dinheiro = 1,

        /// <summary>
        /// 02=Cheque
        /// </summary>
        [XmlEnum("02")]
        Cheque = 2,

        /// <summary>
        /// 03=Cartão de Crédito
        /// </summary>
        [XmlEnum("03")]
        CartaoCredito = 3,

        /// <summary>
        /// 04=Cartão de Débito
        /// </summary>
        [XmlEnum("04")]
        CartaoDebito = 4,

        /// <summary>
        /// 05=Crédito Loja
        /// </summary>
        [XmlEnum("05")]
        CreditoLoja = 5,

        /// <summary>
        /// 10=Vale Alimentação
        /// </summary>
        [XmlEnum("10")]
        ValeAlimentacao = 10,

        /// <summary>
        /// 11=Vale Refeição
        /// </summary>
        [XmlEnum("11")]
        ValeRefeicao = 11,

        /// <summary>
        /// 12=Vale Presente
        /// </summary>
        [XmlEnum("12")]
        ValePresente = 12,

        /// <summary>
        /// 13=Vale Combustível
        /// </summary>
        [XmlEnum("13")]
        ValeCombustivel = 13,

        /// <summary>
        /// 14=Duplicata Mercantil (Não existe mais este numerador no padrão da SEFAZ, foi retirado na Nota Técnica 2016.002 - v 1.61. Mantemos no enum para manter compatilidade em casos de importação de XMLs antigos (B2B) que possuem este valor na tag tPag.)
        /// </summary>
        [XmlEnum("14")]
        DuplicataMercantil = 14,

        /// <summary>
        /// 15=Boleto Bancário
        /// </summary> 
        [XmlEnum("15")]
        BoletoBancario = 15,

        /// <summary>
        /// 16=Depósito Bancário
        /// </summary> 
        [XmlEnum("16")]
        DepositoBancario = 16,

        /// <summary>
        /// 17=Pagamento Instantâneo (PIX)
        /// </summary> 
        [XmlEnum("17")]
        PagamentoInstantaneo = 17,

        /// <summary>
        /// 18=Transferência bancária, Carteira Digital
        /// </summary> 
        [XmlEnum("18")]
        TransferenciaBancaria = 18,

        /// <summary>
        /// 19=Programa de fidelidade, Cashback, Crédito Virtual
        /// </summary> 
        [XmlEnum("19")]
        ProgramaFidelidade = 19,

        /// <summary>
        /// 90=Sem pagamento
        /// </summary>
        [XmlEnum("90")]
        SemPagamento = 90,

        /// <summary>
        /// 99=Outros
        /// </summary>
        [XmlEnum("99")]
        Outros = 99
    }

    #endregion

    #region Indicador de Forma de Pagamento

    /// <summary>
    /// Indicador de Forma de Pagamento
    /// </summary>
    public enum IndicadorPagamento
    {
        /// <summary>
        /// 0=Pagamento à Vista
        /// </summary>
        [XmlEnum("0")]
        PagamentoVista = 0,

        /// <summary>
        /// 1=Pagamento à Prazo
        /// </summary>
        [XmlEnum("1")]
        PagamentoPrazo = 1
    }

    #endregion

    #region Lista de serviços

    /// <summary>
    /// Lista de Serviços do ISSQN
    /// </summary>
    public enum ListaServicoISSQN
    {
        /// <summary>
        /// Serviço 01.01
        /// </summary>
        [XmlEnum("01.01")]
        Servico0101 = 0101,

        /// <summary>
        /// Serviço 01.02
        /// </summary>
        [XmlEnum("01.02")]
        Servico0102 = 0102,

        /// <summary>
        /// Serviço 01.03
        /// </summary>
        [XmlEnum("01.03")]
        Servico0103 = 0103,

        /// <summary>
        /// Serviço 01.04
        /// </summary>
        [XmlEnum("01.04")]
        Servico0104 = 0104,

        /// <summary>
        /// Serviço 01.05
        /// </summary>
        [XmlEnum("01.05")]
        Servico0105 = 0105,

        /// <summary>
        /// Serviço 01.06
        /// </summary>
        [XmlEnum("01.06")]
        Servico0106 = 0106,

        /// <summary>
        /// Serviço 01.07
        /// </summary>
        [XmlEnum("01.07")]
        Servico0107 = 0107,

        /// <summary>
        /// Serviço 01.08
        /// </summary>
        [XmlEnum("01.08")]
        Servico0108 = 0108,

        /// <summary>
        /// Serviço 01.09
        /// </summary>
        [XmlEnum("01.09")]
        Servico0109 = 0109,

        /// <summary>
        /// Serviço 02.01
        /// </summary>
        [XmlEnum("02.01")]
        Servico0201 = 0201,

        /// <summary>
        /// Serviço 03.02
        /// </summary>
        [XmlEnum("03.02")]
        Servico0302 = 0302,

        /// <summary>
        /// Serviço 03.03
        /// </summary>
        [XmlEnum("03.03")]
        Servico0303 = 0303,

        /// <summary>
        /// Serviço 03.04
        /// </summary>
        [XmlEnum("03.04")]
        Servico0304 = 0304,

        /// <summary>
        /// Serviço 03.05
        /// </summary>
        [XmlEnum("03.05")]
        Servico0305 = 0305,

        /// <summary>
        /// Serviço 04.01
        /// </summary>
        [XmlEnum("04.01")]
        Servico0401 = 0401,

        /// <summary>
        /// Serviço 04.02
        /// </summary>
        [XmlEnum("04.02")]
        Servico0402 = 0402,

        /// <summary>
        /// Serviço 04.03
        /// </summary>
        [XmlEnum("04.03")]
        Servico0403 = 0403,

        /// <summary>
        /// Serviço 04.04
        /// </summary>
        [XmlEnum("04.04")]
        Servico0404 = 0404,

        /// <summary>
        /// Serviço 04.05
        /// </summary>
        [XmlEnum("04.05")]
        Servico0405 = 0405,

        /// <summary>
        /// Serviço 04.06
        /// </summary>
        [XmlEnum("04.06")]
        Servico0406 = 0406,

        /// <summary>
        /// Serviço 04.07
        /// </summary>
        [XmlEnum("04.07")]
        Servico0407 = 0407,

        /// <summary>
        /// Serviço 04.08
        /// </summary>
        [XmlEnum("04.08")]
        Servico0408 = 0408,

        /// <summary>
        /// Serviço 04.09
        /// </summary>
        [XmlEnum("04.09")]
        Servico0409 = 0409,

        /// <summary>
        /// Serviço 04.10
        /// </summary>
        [XmlEnum("04.10")]
        Servico0410 = 0410,

        /// <summary>
        /// Serviço 04.11
        /// </summary>
        [XmlEnum("04.11")]
        Servico0411 = 0411,

        /// <summary>
        /// Serviço 04.12
        /// </summary>
        [XmlEnum("04.12")]
        Servico0412 = 0412,

        /// <summary>
        /// Serviço 04.13
        /// </summary>
        [XmlEnum("04.13")]
        Servico0413 = 0413,

        /// <summary>
        /// Serviço 04.14
        /// </summary>
        [XmlEnum("04.14")]
        Servico0414 = 0414,

        /// <summary>
        /// Serviço 04.15
        /// </summary>
        [XmlEnum("04.15")]
        Servico0415 = 0415,

        /// <summary>
        /// Serviço 04.16
        /// </summary>
        [XmlEnum("04.16")]
        Servico0416 = 0416,

        /// <summary>
        /// Serviço 04.17
        /// </summary>
        [XmlEnum("04.17")]
        Servico0417 = 0417,

        /// <summary>
        /// Serviço 04.18
        /// </summary>
        [XmlEnum("04.18")]
        Servico0418 = 0418,

        /// <summary>
        /// Serviço 04.19
        /// </summary>
        [XmlEnum("04.19")]
        Servico0419 = 0419,

        /// <summary>
        /// Serviço 04.20
        /// </summary>
        [XmlEnum("04.20")]
        Servico0420 = 0420,

        /// <summary>
        /// Serviço 04.21
        /// </summary>
        [XmlEnum("04.21")]
        Servico0421 = 0421,

        /// <summary>
        /// Serviço 04.22
        /// </summary>
        [XmlEnum("04.22")]
        Servico0422 = 0422,

        /// <summary>
        /// Serviço 04.23
        /// </summary>
        [XmlEnum("04.23")]
        Servico0423 = 0423,

        /// <summary>
        /// Serviço 05.01
        /// </summary>
        [XmlEnum("05.01")]
        Servico0501 = 0501,

        /// <summary>
        /// Serviço 05.02
        /// </summary>
        [XmlEnum("05.02")]
        Servico0502 = 0502,

        /// <summary>
        /// Serviço 05.03
        /// </summary>
        [XmlEnum("05.03")]
        Servico0503 = 0503,

        /// <summary>
        /// Serviço 05.04
        /// </summary>
        [XmlEnum("05.04")]
        Servico0504 = 0504,

        /// <summary>
        /// Serviço 05.05
        /// </summary>
        [XmlEnum("05.05")]
        Servico0505 = 0505,

        /// <summary>
        /// Serviço 05.06
        /// </summary>
        [XmlEnum("05.06")]
        Servico0506 = 0506,

        /// <summary>
        /// Serviço 05.07
        /// </summary>
        [XmlEnum("05.07")]
        Servico0507 = 0507,

        /// <summary>
        /// Serviço 05.08
        /// </summary>
        [XmlEnum("05.08")]
        Servico0508 = 0508,

        /// <summary>
        /// Serviço 05.09
        /// </summary>
        [XmlEnum("05.09")]
        Servico0509 = 0509,

        /// <summary>
        /// Serviço 06.01
        /// </summary>
        [XmlEnum("06.01")]
        Servico0601 = 0601,

        /// <summary>
        /// Serviço 06.02
        /// </summary>
        [XmlEnum("06.02")]
        Servico0602 = 0602,

        /// <summary>
        /// Serviço 06.03
        /// </summary>
        [XmlEnum("06.03")]
        Servico0603 = 0603,

        /// <summary>
        /// Serviço 06.04
        /// </summary>
        [XmlEnum("06.04")]
        Servico0604 = 0604,

        /// <summary>
        /// Serviço 06.05
        /// </summary>
        [XmlEnum("06.05")]
        Servico0605 = 0605,

        /// <summary>
        /// Serviço 06.06
        /// </summary>
        [XmlEnum("06.06")]
        Servico0606 = 0606,

        /// <summary>
        /// Serviço 07.01
        /// </summary>
        [XmlEnum("07.01")]
        Servico0701 = 0701,

        /// <summary>
        /// Serviço 07.02
        /// </summary>
        [XmlEnum("07.02")]
        Servico0702 = 0702,

        /// <summary>
        /// Serviço 07.03
        /// </summary>
        [XmlEnum("07.03")]
        Servico0703 = 0703,

        /// <summary>
        /// Serviço 07.04
        /// </summary>
        [XmlEnum("07.04")]
        Servico0704 = 0704,

        /// <summary>
        /// Serviço 07.05
        /// </summary>
        [XmlEnum("07.05")]
        Servico0705 = 0705,

        /// <summary>
        /// Serviço 07.06
        /// </summary>
        [XmlEnum("07.06")]
        Servico0706 = 0706,

        /// <summary>
        /// Serviço 07.07
        /// </summary>
        [XmlEnum("07.07")]
        Servico0707 = 0707,

        /// <summary>
        /// Serviço 07.08
        /// </summary>
        [XmlEnum("07.08")]
        Servico0708 = 0708,

        /// <summary>
        /// Serviço 07.09
        /// </summary>
        [XmlEnum("07.09")]
        Servico0709 = 0709,

        /// <summary>
        /// Serviço 07.10
        /// </summary>
        [XmlEnum("07.10")]
        Servico0710 = 0710,

        /// <summary>
        /// Serviço 07.11
        /// </summary>
        [XmlEnum("07.11")]
        Servico0711 = 0711,

        /// <summary>
        /// Serviço 07.12
        /// </summary>
        [XmlEnum("07.12")]
        Servico0712 = 0712,

        /// <summary>
        /// Serviço 07.13
        /// </summary>
        [XmlEnum("07.13")]
        Servico0713 = 0713,

        /// <summary>
        /// Serviço 07.16
        /// </summary>
        [XmlEnum("07.16")]
        Servico0716 = 0716,

        /// <summary>
        /// Serviço 07.17
        /// </summary>
        [XmlEnum("07.17")]
        Servico0717 = 0717,

        /// <summary>
        /// Serviço 07.18
        /// </summary>
        [XmlEnum("07.18")]
        Servico0718 = 0718,

        /// <summary>
        /// Serviço 07.19
        /// </summary>
        [XmlEnum("07.19")]
        Servico0719 = 0719,

        /// <summary>
        /// Serviço 07.20
        /// </summary>
        [XmlEnum("07.20")]
        Servico0720 = 0720,

        /// <summary>
        /// Serviço 07.21
        /// </summary>
        [XmlEnum("07.21")]
        Servico0721 = 0721,

        /// <summary>
        /// Serviço 07.22
        /// </summary>
        [XmlEnum("07.22")]
        Servico0722 = 0722,

        /// <summary>
        /// Serviço 08.01
        /// </summary>
        [XmlEnum("08.01")]
        Servico0801 = 0801,

        /// <summary>
        /// Serviço 08.02
        /// </summary>
        [XmlEnum("08.02")]
        Servico0802 = 0802,

        /// <summary>
        /// Serviço 09.01
        /// </summary>
        [XmlEnum("09.01")]
        Servico0901 = 0901,

        /// <summary>
        /// Serviço 09.02
        /// </summary>
        [XmlEnum("09.02")]
        Servico0902 = 0902,

        /// <summary>
        /// Serviço 09.03
        /// </summary>
        [XmlEnum("09.03")]
        Servico0903 = 0903,

        /// <summary>
        /// Serviço 10.01
        /// </summary>
        [XmlEnum("10.01")]
        Servico1001 = 1001,

        /// <summary>
        /// Serviço 10.02
        /// </summary>
        [XmlEnum("10.02")]
        Servico1002 = 1002,

        /// <summary>
        /// Serviço 10.03
        /// </summary>
        [XmlEnum("10.03")]
        Servico1003 = 1003,

        /// <summary>
        /// Serviço 10.04
        /// </summary>
        [XmlEnum("10.04")]
        Servico1004 = 1004,

        /// <summary>
        /// Serviço 10.05
        /// </summary>
        [XmlEnum("10.05")]
        Servico1005 = 1005,

        /// <summary>
        /// Serviço 10.06
        /// </summary>
        [XmlEnum("10.06")]
        Servico1006 = 1006,

        /// <summary>
        /// Serviço 10.07
        /// </summary>
        [XmlEnum("10.07")]
        Servico1007 = 1007,

        /// <summary>
        /// Serviço 10.08
        /// </summary>
        [XmlEnum("10.08")]
        Servico1008 = 1008,

        /// <summary>
        /// Serviço 10.09
        /// </summary>
        [XmlEnum("10.09")]
        Servico1009 = 1009,

        /// <summary>
        /// Serviço 10.10
        /// </summary>
        [XmlEnum("10.10")]
        Servico1010 = 1010,

        /// <summary>
        /// Serviço 11.01
        /// </summary>
        [XmlEnum("11.01")]
        Servico1101 = 1101,

        /// <summary>
        /// Serviço 11.02
        /// </summary>
        [XmlEnum("11.02")]
        Servico1102 = 1102,

        /// <summary>
        /// Serviço 11.03
        /// </summary>
        [XmlEnum("11.03")]
        Servico1103 = 1103,

        /// <summary>
        /// Serviço 11.04
        /// </summary>
        [XmlEnum("11.04")]
        Servico1104 = 1104,

        /// <summary>
        /// Serviço 11.05
        /// </summary>
        [XmlEnum("11.05")]
        Servico1105 = 1105,

        /// <summary>
        /// Serviço 12.01
        /// </summary>
        [XmlEnum("12.01")]
        Servico1201 = 1201,

        /// <summary>
        /// Serviço 12.02
        /// </summary>
        [XmlEnum("12.02")]
        Servico1202 = 1202,

        /// <summary>
        /// Serviço 12.03
        /// </summary>
        [XmlEnum("12.03")]
        Servico1203 = 1203,

        /// <summary>
        /// Serviço 12.04
        /// </summary>
        [XmlEnum("12.04")]
        Servico1204 = 1204,

        /// <summary>
        /// Serviço 12.05
        /// </summary>
        [XmlEnum("12.05")]
        Servico1205 = 1205,

        /// <summary>
        /// Serviço 12.06
        /// </summary>
        [XmlEnum("12.06")]
        Servico1206 = 1206,

        /// <summary>
        /// Serviço 12.07
        /// </summary>
        [XmlEnum("12.07")]
        Servico1207 = 1207,

        /// <summary>
        /// Serviço 12.08
        /// </summary>
        [XmlEnum("12.08")]
        Servico1208 = 1208,

        /// <summary>
        /// Serviço 12.09
        /// </summary>
        [XmlEnum("12.09")]
        Servico1209 = 1209,

        /// <summary>
        /// Serviço 12.10
        /// </summary>
        [XmlEnum("12.10")]
        Servico1210 = 1210,

        /// <summary>
        /// Serviço 12.11
        /// </summary>
        [XmlEnum("12.11")]
        Servico1211 = 1211,

        /// <summary>
        /// Serviço 12.12
        /// </summary>
        [XmlEnum("12.12")]
        Servico1212 = 1212,

        /// <summary>
        /// Serviço 12.13
        /// </summary>
        [XmlEnum("12.13")]
        Servico1213 = 1213,

        /// <summary>
        /// Serviço 12.14
        /// </summary>
        [XmlEnum("12.14")]
        Servico1214 = 1214,

        /// <summary>
        /// Serviço 12.15
        /// </summary>
        [XmlEnum("12.15")]
        Servico1215 = 1215,

        /// <summary>
        /// Serviço 12.16
        /// </summary>
        [XmlEnum("12.16")]
        Servico1216 = 1216,

        /// <summary>
        /// Serviço 12.17
        /// </summary>
        [XmlEnum("12.17")]
        Servico1217 = 1217,

        /// <summary>
        /// Serviço 13.02
        /// </summary>
        [XmlEnum("13.02")]
        Servico1302 = 1302,

        /// <summary>
        /// Serviço 13.03
        /// </summary>
        [XmlEnum("13.03")]
        Servico1303 = 1303,

        /// <summary>
        /// Serviço 13.04
        /// </summary>
        [XmlEnum("13.04")]
        Servico1304 = 1304,

        /// <summary>
        /// Serviço 13.05
        /// </summary>
        [XmlEnum("13.05")]
        Servico1305 = 1305,

        /// <summary>
        /// Serviço 14.01
        /// </summary>
        [XmlEnum("14.01")]
        Servico1401 = 1401,

        /// <summary>
        /// Serviço 14.02
        /// </summary>
        [XmlEnum("14.02")]
        Servico1402 = 1402,

        /// <summary>
        /// Serviço 14.03
        /// </summary>
        [XmlEnum("14.03")]
        Servico1403 = 1403,

        /// <summary>
        /// Serviço 14.04
        /// </summary>
        [XmlEnum("14.04")]
        Servico1404 = 1404,

        /// <summary>
        /// Serviço 14.05
        /// </summary>
        [XmlEnum("14.05")]
        Servico1405 = 1405,

        /// <summary>
        /// Serviço 14.06
        /// </summary>
        [XmlEnum("14.06")]
        Servico1406 = 1406,

        /// <summary>
        /// Serviço 14.07
        /// </summary>
        [XmlEnum("14.07")]
        Servico1407 = 1407,

        /// <summary>
        /// Serviço 14.08
        /// </summary>
        [XmlEnum("14.08")]
        Servico1408 = 1408,

        /// <summary>
        /// Serviço 14.09
        /// </summary>
        [XmlEnum("14.09")]
        Servico1409 = 1409,

        /// <summary>
        /// Serviço 14.10
        /// </summary>
        [XmlEnum("14.10")]
        Servico1410 = 1410,

        /// <summary>
        /// Serviço 14.11
        /// </summary>
        [XmlEnum("14.11")]
        Servico1411 = 1411,

        /// <summary>
        /// Serviço 14.12
        /// </summary>
        [XmlEnum("14.12")]
        Servico1412 = 1412,

        /// <summary>
        /// Serviço 14.13
        /// </summary>
        [XmlEnum("14.13")]
        Servico1413 = 1413,

        /// <summary>
        /// Serviço 14.14
        /// </summary>
        [XmlEnum("14.14")]
        Servico1414 = 1414,

        /// <summary>
        /// Serviço 15.01
        /// </summary>
        [XmlEnum("15.01")]
        Servico1501 = 1501,

        /// <summary>
        /// Serviço 15.02
        /// </summary>
        [XmlEnum("15.02")]
        Servico1502 = 1502,

        /// <summary>
        /// Serviço 15.03
        /// </summary>
        [XmlEnum("15.03")]
        Servico1503 = 1503,

        /// <summary>
        /// Serviço 15.04
        /// </summary>
        [XmlEnum("15.04")]
        Servico1504 = 1504,

        /// <summary>
        /// Serviço 15.05
        /// </summary>
        [XmlEnum("15.05")]
        Servico1505 = 1505,

        /// <summary>
        /// Serviço 15.06
        /// </summary>
        [XmlEnum("15.06")]
        Servico1506 = 1506,

        /// <summary>
        /// Serviço 15.07
        /// </summary>
        [XmlEnum("15.07")]
        Servico1507 = 1507,

        /// <summary>
        /// Serviço 15.08
        /// </summary>
        [XmlEnum("15.08")]
        Servico1508 = 1508,

        /// <summary>
        /// Serviço 15.09
        /// </summary>
        [XmlEnum("15.09")]
        Servico1509 = 1509,

        /// <summary>
        /// Serviço 15.10
        /// </summary>
        [XmlEnum("15.10")]
        Servico1510 = 1510,

        /// <summary>
        /// Serviço 15.11
        /// </summary>
        [XmlEnum("15.11")]
        Servico1511 = 1511,

        /// <summary>
        /// Serviço 15.12
        /// </summary>
        [XmlEnum("15.12")]
        Servico1512 = 1512,

        /// <summary>
        /// Serviço 15.13
        /// </summary>
        [XmlEnum("15.13")]
        Servico1513 = 1513,

        /// <summary>
        /// Serviço 15.14
        /// </summary>
        [XmlEnum("15.14")]
        Servico1514 = 1514,

        /// <summary>
        /// Serviço 15.15
        /// </summary>
        [XmlEnum("15.15")]
        Servico1515 = 1515,

        /// <summary>
        /// Serviço 15.16
        /// </summary>
        [XmlEnum("15.16")]
        Servico1516 = 1516,

        /// <summary>
        /// Serviço 15.17
        /// </summary>
        [XmlEnum("15.17")]
        Servico1517 = 1517,

        /// <summary>
        /// Serviço 15.18
        /// </summary>
        [XmlEnum("15.18")]
        Servico1518 = 1518,

        /// <summary>
        /// Serviço 16.01
        /// </summary>
        [XmlEnum("16.01")]
        Servico1601 = 1601,

        /// <summary>
        /// Serviço 16.02
        /// </summary>
        [XmlEnum("16.02")]
        Servico1602 = 1602,

        /// <summary>
        /// Serviço 17.01
        /// </summary>
        [XmlEnum("17.01")]
        Servico1701 = 1701,

        /// <summary>
        /// Serviço 17.02
        /// </summary>
        [XmlEnum("17.02")]
        Servico1702 = 1702,

        /// <summary>
        /// Serviço 17.03
        /// </summary>
        [XmlEnum("17.03")]
        Servico1703 = 1703,

        /// <summary>
        /// Serviço 17.04
        /// </summary>
        [XmlEnum("17.04")]
        Servico1704 = 1704,

        /// <summary>
        /// Serviço 17.05
        /// </summary>
        [XmlEnum("17.05")]
        Servico1705 = 1705,

        /// <summary>
        /// Serviço 17.06
        /// </summary>
        [XmlEnum("17.06")]
        Servico1706 = 1706,

        /// <summary>
        /// Serviço 17.08
        /// </summary>
        [XmlEnum("17.08")]
        Servico1708 = 1708,

        /// <summary>
        /// Serviço 17.09
        /// </summary>
        [XmlEnum("17.09")]
        Servico1709 = 1709,

        /// <summary>
        /// Serviço 17.10
        /// </summary>
        [XmlEnum("17.10")]
        Servico1710 = 1710,

        /// <summary>
        /// Serviço 17.11
        /// </summary>
        [XmlEnum("17.11")]
        Servico1711 = 1711,

        /// <summary>
        /// Serviço 17.12
        /// </summary>
        [XmlEnum("17.12")]
        Servico1712 = 1712,

        /// <summary>
        /// Serviço 17.13
        /// </summary>
        [XmlEnum("17.13")]
        Servico1713 = 1713,

        /// <summary>
        /// Serviço 17.14
        /// </summary>
        [XmlEnum("17.14")]
        Servico1714 = 1714,

        /// <summary>
        /// Serviço 17.15
        /// </summary>
        [XmlEnum("17.15")]
        Servico1715 = 1715,

        /// <summary>
        /// Serviço 17.16
        /// </summary>
        [XmlEnum("17.16")]
        Servico1716 = 1716,

        /// <summary>
        /// Serviço 17.17
        /// </summary>
        [XmlEnum("17.17")]
        Servico1717 = 1717,

        /// <summary>
        /// Serviço 17.18
        /// </summary>
        [XmlEnum("17.18")]
        Servico1718 = 1718,

        /// <summary>
        /// Serviço 17.19
        /// </summary>
        [XmlEnum("17.19")]
        Servico1719 = 1719,

        /// <summary>
        /// Serviço 17.20
        /// </summary>
        [XmlEnum("17.20")]
        Servico1720 = 1720,

        /// <summary>
        /// Serviço 17.21
        /// </summary>
        [XmlEnum("17.21")]
        Servico1721 = 1721,

        /// <summary>
        /// Serviço 17.22
        /// </summary>
        [XmlEnum("17.22")]
        Servico1722 = 1722,

        /// <summary>
        /// Serviço 17.23
        /// </summary>
        [XmlEnum("17.23")]
        Servico1723 = 1723,

        /// <summary>
        /// Serviço 17.24
        /// </summary>
        [XmlEnum("17.24")]
        Servico1724 = 1724,

        /// <summary>
        /// Serviço 17.25
        /// </summary>
        [XmlEnum("17.25")]
        Servico1725 = 1725,

        /// <summary>
        /// Serviço 18.01
        /// </summary>
        [XmlEnum("18.01")]
        Servico1801 = 1801,

        /// <summary>
        /// Serviço 19.01
        /// </summary>
        [XmlEnum("19.01")]
        Servico1901 = 1901,

        /// <summary>
        /// Serviço 20.01
        /// </summary>
        [XmlEnum("20.01")]
        Servico2001 = 2001,

        /// <summary>
        /// Serviço 20.02
        /// </summary>
        [XmlEnum("20.02")]
        Servico2002 = 2002,

        /// <summary>
        /// Serviço 20.03
        /// </summary>
        [XmlEnum("20.03")]
        Servico2003 = 2003,

        /// <summary>
        /// Serviço 21.01
        /// </summary>
        [XmlEnum("21.01")]
        Servico2101 = 2101,

        /// <summary>
        /// Serviço 22.01
        /// </summary>
        [XmlEnum("22.01")]
        Servico2201 = 2201,

        /// <summary>
        /// Serviço 23.01
        /// </summary>
        [XmlEnum("23.01")]
        Servico2301 = 2301,

        /// <summary>
        /// Serviço 24.01
        /// </summary>
        [XmlEnum("24.01")]
        Servico2401 = 2401,

        /// <summary>
        /// Serviço 25.01
        /// </summary>
        [XmlEnum("25.01")]
        Servico2501 = 2501,

        /// <summary>
        /// Serviço 25.02
        /// </summary>
        [XmlEnum("25.02")]
        Servico2502 = 2502,

        /// <summary>
        /// Serviço 25.03
        /// </summary>
        [XmlEnum("25.03")]
        Servico2503 = 2503,

        /// <summary>
        /// Serviço 25.04
        /// </summary>
        [XmlEnum("25.04")]
        Servico2504 = 2504,

        /// <summary>
        /// Serviço 25.05
        /// </summary>
        [XmlEnum("25.05")]
        Servico2505 = 2505,

        /// <summary>
        /// Serviço 26.01
        /// </summary>
        [XmlEnum("26.01")]
        Servico2601 = 2601,

        /// <summary>
        /// Serviço 27.01
        /// </summary>
        [XmlEnum("27.01")]
        Servico2701 = 2701,

        /// <summary>
        /// Serviço 28.01
        /// </summary>
        [XmlEnum("28.01")]
        Servico2801 = 2801,

        /// <summary>
        /// Serviço 29.01
        /// </summary>
        [XmlEnum("29.01")]
        Servico2901 = 2901,

        /// <summary>
        /// Serviço 30.01
        /// </summary>
        [XmlEnum("30.01")]
        Servico3001 = 3001,

        /// <summary>
        /// Serviço 31.01
        /// </summary>
        [XmlEnum("31.01")]
        Servico3101 = 3101,

        /// <summary>
        /// Serviço 32.01
        /// </summary>
        [XmlEnum("32.01")]
        Servico3201 = 3201,

        /// <summary>
        /// Serviço 33.01
        /// </summary>
        [XmlEnum("33.01")]
        Servico3301 = 3301,

        /// <summary>
        /// Serviço 34.01
        /// </summary>
        [XmlEnum("34.01")]
        Servico3401 = 3401,

        /// <summary>
        /// Serviço 35.01
        /// </summary>
        [XmlEnum("35.01")]
        Servico3501 = 3501,

        /// <summary>
        /// Serviço 36.01
        /// </summary>
        [XmlEnum("36.01")]
        Servico3601 = 3601,

        /// <summary>
        /// Serviço 37.01
        /// </summary>
        [XmlEnum("37.01")]
        Servico3701 = 3701,

        /// <summary>
        /// Serviço 38.01
        /// </summary>
        [XmlEnum("38.01")]
        Servico3801 = 3801,

        /// <summary>
        /// Serviço 39.01
        /// </summary>
        [XmlEnum("39.01")]
        Servico3901 = 3901,

        /// <summary>
        /// Serviço 40.01
        /// </summary>
        [XmlEnum("40.01")]
        Servico4001 = 4001
    }

    #endregion

    #region Indicador da exigibilidade do ISS 

    /// <summary>
    /// Indicador de Exigibilidade do ISSQN
    /// </summary>
    public enum IndicadorExigibilidadeISSQN
    {
        /// <summary>
        /// 1=Exigível
        /// </summary>
        [XmlEnum("1")]
        Exigivel = 1,

        /// <summary>
        /// 2=Não incidência
        /// </summary>
        [XmlEnum("2")]
        NaoIncidencia = 2,

        /// <summary>
        /// 3=Isenção
        /// </summary>
        [XmlEnum("3")]
        Isencao = 3,

        /// <summary>
        /// 4=Exportação
        /// </summary>
        [XmlEnum("4")]
        Exportacao = 4,

        /// <summary>
        /// 5=Imunidade
        /// </summary>
        [XmlEnum("5")]
        Imunidade = 5,

        /// <summary>
        /// 6=Exigibilidade Suspensa por Decisão Judicial
        /// </summary>
        [XmlEnum("6")]
        SuspensaDecisaoJudicial = 6,

        /// <summary>
        /// 7=Exigibilidade Suspensa por Processo Administrativo
        /// </summary>
        [XmlEnum("7")]
        SuspensaProcessoAdministrativo = 7
    }

    #endregion

    #region Código do Regime Especial de Tributação

    /// <summary>
    /// Códigos de Regime Especial de Tributação - Utilizado na prestação de serviços (ISSQN)
    /// </summary>
    public enum CodigoRegimeEspecialTributacao
    {
        /// <summary>
        /// 1=Microempresa Municipal,
        /// </summary>
        [XmlEnum("1")]
        MicroempresaMunicipal = 1,

        /// <summary>
        /// 2=Estimativa,
        /// </summary>
        [XmlEnum("2")]
        Estimativa = 2,

        /// <summary>
        /// 3=Sociedade de Profissionais,
        /// </summary>
        [XmlEnum("3")]
        SociedadeProfissionais = 3,

        /// <summary>
        /// 4=Cooperativa,
        /// </summary>
        [XmlEnum("4")]
        Cooperativa = 4,

        /// <summary>
        /// 5=Microempresário Individual (MEI),
        /// </summary>
        [XmlEnum("5")]
        MicroEmpresarioIndividual = 5,

        /// <summary>
        /// 6=Microempresário e Empresa de Pequeno Porte (ME/EPP)
        /// </summary>
        [XmlEnum("6")]
        MicroEmpresarioEmpresaPequenoPorte = 6
    }

    #endregion

    #region TipoOperacaoVeicNovo

    /// <summary>
    /// Tipo de operação - Detalhamento de veículos novos
    /// </summary>
    public enum TipoOperacaoVeicNovo
    {
        /// <summary>
        /// 1=Venda concessionária,
        /// </summary>
        [XmlEnum("1")]
        VendaConcessionaria = 1,

        /// <summary>
        /// 2=Faturamento direto para consumidor final
        /// </summary>
        [XmlEnum("2")]
        FaturamentoDiretoConsumidorfinal = 2,

        /// <summary>
        /// 3=Venda direta para grandes consumidores (frotista, governo, etc...)
        /// </summary>
        [XmlEnum("3")]
        VendaDiretaGrandesConsumidores = 3,

        /// <summary>
        /// 0=Outros
        /// </summary>
        [XmlEnum("0")]
        Outros = 0
    }

    #endregion

    #region CondicaoVIN

    /// <summary>
    /// Condição do VIN, chassi do veículo, remarcado ou normal
    /// </summary>
    public enum CondicaoVIN
    {
        /// <summary>
        /// R = Veículo com VIN (chassi) remarcado
        /// </summary>
        [XmlEnum("R")]
        Remarcado = 0,

        /// <summary>
        /// R = Veículo com VIN (chassi) remarcado
        /// </summary>
        [XmlEnum("N")]
        Normal = 1
    }

    #endregion

    #region CondicaoVeiculo

    /// <summary>
    /// Condição do veículo
    /// </summary>
    public enum CondicaoVeiculo
    {
        /// <summary>
        /// 1=Acabado
        /// </summary>
        [XmlEnum("1")]
        Acabado = 1,

        /// <summary>
        /// 2=Inacabado
        /// </summary>
        [XmlEnum("2")]
        Inacabado = 2,

        /// <summary>
        /// 3=Semiacabado
        /// </summary>
        [XmlEnum("3")]
        Semiacabado = 3
    }

    #endregion

    #region TipoRestricaoVeiculo

    /// <summary>
    /// Tipo de Restrição do Veículo
    /// </summary>
    public enum TipoRestricaoVeiculo
    {
        /// <summary>
        /// 0=Não há
        /// </summary>
        [XmlEnum("0")]
        NaoHa = 0,

        /// <summary>
        /// 1=Alienação Fiduciária
        /// </summary>
        [XmlEnum("1")]
        AlienacaoFiduciaria = 1,

        /// <summary>
        /// 2=Arrendamento Mercantil
        /// </summary>
        [XmlEnum("2")]
        ArrendamentoMercantil = 2,

        /// <summary>
        /// 3=Reserva de Domínio
        /// </summary>
        [XmlEnum("3")]
        ReservaDominio = 3,

        /// <summary>
        /// 4=Penhor de Veículos
        /// </summary>
        [XmlEnum("4")]
        PenhorVeiculos = 4,

        /// <summary>
        /// 9=Outras
        /// </summary>
        [XmlEnum("9")]
        Outras = 9
    }

    #endregion

    #region TipoArma

    /// <summary>
    /// Indicador do tipo de arma de fogo
    /// </summary>
    public enum TipoArma
    {
        /// <summary>
        /// 0 = Uso Permitido
        /// </summary>
        [XmlEnum("0")]
        UsoPermitido = 0,

        /// <summary>
        /// Uso Restrito
        /// </summary>
        [XmlEnum("1")]
        UsoRestrito = 1
    }

    #endregion

    #region Tipo de CTe

    /// <summary>
    /// Tipo de CTe
    /// </summary>
    public enum TipoCTe
    {
        /// <summary>
        /// 1=CTe Normal
        /// </summary>
        [XmlEnum("0")]
        Normal = 0,

        /// <summary>
        /// 2=CTe de complemento de valores
        /// </summary>
        [XmlEnum("1")]
        Complementar = 1,

        /// <summary>
        /// 3=CTe de Anulação (Tipo obsoleto, só funciona para versão 3.00 do CTe)
        /// </summary>
        [XmlEnum("2")]
        Anulacao = 2,

        /// <summary>
        /// 3=CTe de Substituição
        /// </summary>
        [XmlEnum("3")]
        Substituicao = 3
    }

    #endregion

    #region Modalidades de transportes do CTe

    /// <summary>
    /// Modalidade de transportes do CTe
    /// </summary>
    public enum ModalidadeTransporteCTe
    {
        /// <summary>
        /// 01 - Rodoviário
        /// </summary>
        [XmlEnum("01")]
        Rodoviario = 01,

        /// <summary>
        /// 02 - Aéreo
        /// </summary>
        [XmlEnum("02")]
        Aereo = 02,

        /// <summary>
        /// 03 - Aquaviário
        /// </summary>
        [XmlEnum("03")]
        Aquaviario = 03,

        /// <summary>
        /// 04 - Ferroviário
        /// </summary>
        [XmlEnum("04")]
        Ferroviario = 04,

        /// <summary>
        /// 05 - Dutoviário
        /// </summary>
        [XmlEnum("05")]
        Dutoviario = 05,

        /// <summary>
        /// 06 - Multimodal
        /// </summary>
        [XmlEnum("06")]
        Multimodal = 06
    }
    #endregion

    #region Tipo Serviço CTe

    /// <summary>
    /// Tipos de serviços do CTe
    /// </summary>
    public enum TipoServicoCTe
    {
        /// <summary>
        /// 0 = Normal
        /// </summary>
        [XmlEnum("0")]
        Normal = 0,

        /// <summary>
        /// 1 = Subcontratação
        /// </summary>
        [XmlEnum("1")]
        Subcontratacao = 1,

        /// <summary>
        /// 2 = Redespacho
        /// </summary>
        [XmlEnum("2")]
        Redespacho = 2,

        /// <summary>
        /// 3 = Redespacho Intermediário
        /// </summary>
        [XmlEnum("3")]
        RedespachoIntermediario = 3,

        /// <summary>
        /// 4 = ServicoVinculadoMultimodal
        /// </summary>
        [XmlEnum("4")]
        ServicoVinculadoMultimodal = 4
    }

    #endregion

    #region Tipo Serviço CTeOS

    /// <summary>
    /// Tipos de serviços do CTeOS
    /// </summary>
    public enum TipoServicoCTeOS
    {
        /// <summary>
        /// 6 = Transporte de Pessoas
        /// </summary>
        [XmlEnum("6")]
        TransportePessoas = 6,

        /// <summary>
        /// 7 = Transporte de Valores
        /// </summary>
        [XmlEnum("7")]
        TransporteValores = 7,

        /// <summary>
        /// 8 = Excesso de Bagagem
        /// </summary>
        [XmlEnum("8")]
        ExcessoBagagem = 8
    }

    #endregion

    #region TomadorServicoCTe

    /// <summary>
    /// Tomador do serviço do CTe
    /// </summary>
    public enum TomadorServicoCTe
    {
        /// <summary>
        /// 0 - Remetente
        /// </summary>
        [XmlEnum("0")]
        Remetente = 0,

        /// <summary>
        /// 1 - Expedidor
        /// </summary>
        [XmlEnum("1")]
        Expedidor = 1,

        /// <summary>
        /// 2 - Recebedor
        /// </summary>
        [XmlEnum("2")]
        Recebedor = 2,

        /// <summary>
        /// 3 - Destinatário
        /// </summary>
        [XmlEnum("3")]
        Destinatario = 3,

        /// <summary>
        /// 4 - Outros
        /// </summary>
        [XmlEnum("4")]
        Outros = 4
    }

    #endregion

    #region Tipo Período Entrega (CTe)

    /// <summary>
    /// Tipo Período Entrega (CTe)
    /// </summary>
    public enum TipoPeriodoEntregaCTe
    {
        /// <summary>
        /// 0 - Sem data definida
        /// </summary>
        [XmlEnum("0")]
        SemDataDefinida = 0,

        /// <summary>
        /// 1 - Na data
        /// </summary>
        [XmlEnum("1")]
        NaData = 1,

        /// <summary>
        /// 2 - Até a data
        /// </summary>
        [XmlEnum("2")]
        AteAData = 2,

        /// <summary>
        /// 3 - A partir da data
        /// </summary>
        [XmlEnum("3")]
        APartirDaData = 3,

        /// <summary>
        /// 4 - No período
        /// </summary>
        [XmlEnum("4")]
        NoPeriodo = 4
    }

    #endregion

    #region Tipo de Hora de Entrega (CTe)

    /// <summary>
    /// Tipo de Hora de Entrega (CTe)
    /// </summary>
    public enum TipoHoraEntregaCTe
    {
        /// <summary>
        /// 0 - Sem hora definida
        /// </summary>
        [XmlEnum("0")]
        SemHoraDefinida = 0,

        /// <summary>
        /// 1 - No horário
        /// </summary>
        [XmlEnum("1")]
        NoHorario = 1,

        /// <summary>
        /// 2 - Até o horário
        /// </summary>
        [XmlEnum("2")]
        AteOHorario = 2,

        /// <summary>
        /// 3 - A partir do horário
        /// </summary>
        [XmlEnum("3")]
        APartirDoHorario = 3,

        /// <summary>
        /// 4 - No intervalo de tempo
        /// </summary>
        [XmlEnum("4")]
        NoIntervaloTempo = 4
    }

    #endregion

    #region Código da Unidade de Medida - CTe

    /// <summary>
    /// Código da Unidade de Medida - CTe
    /// </summary>
    public enum CodigoUnidadeMedidaCTe
    {
        /// <summary>
        /// 00-M3
        /// </summary>
        [XmlEnum("00")]
        M3 = 0,

        /// <summary>
        /// 01-KG
        /// </summary>
        [XmlEnum("01")]
        KG = 1,

        /// <summary>
        /// 02-TON
        /// </summary>
        [XmlEnum("02")]
        TON = 2,

        /// <summary>
        /// 03-UNIDADE
        /// </summary>
        [XmlEnum("03")]
        UNIDADE = 3,

        /// <summary>
        /// 04-LITROS
        /// </summary>
        [XmlEnum("04")]
        LITROS = 4,

        /// <summary>
        /// 05-MMBTU
        /// </summary>
        [XmlEnum("05")]
        MMBTU = 5
    }

    #endregion

    #region ModeloNF

    /// <summary>
    /// Modelos da Nota Fiscal
    /// </summary>
    public enum ModeloNF
    {
        /// <summary>
        /// 01 - NF Modelo 01/1A e Avulsa
        /// </summary>
        [XmlEnum("01")]
        NF = 1,

        /// <summary>
        /// 02 - NF de Produtor
        /// </summary>
        [XmlEnum("04")]
        NFProdutor = 4
    }

    #endregion

    #region TipoUnidadeCarga

    /// <summary>
    /// Tipo da Unidade de Carga - CTe e MDFe
    /// </summary>
    public enum TipoUnidadeCarga
    {
        /// <summary>
        /// 1 - Container
        /// </summary>
        [XmlEnum("1")]
        Container = 1,
        /// <summary>
        /// 2 - ULD
        /// </summary>
        [XmlEnum("2")]
        ULD = 2,
        /// <summary>
        /// 3 - Pallet
        /// </summary>
        [XmlEnum("3")]
        Pallet = 3,
        /// <summary>
        /// 4 - Outros
        /// </summary>
        [XmlEnum("4")]
        Outros = 4
    }

    #endregion

    #region TipoUnidadeTransporte

    /// <summary>
    /// Tipo da Unidade de Carta - CTe e MDFe
    /// </summary>
    public enum TipoUnidadeTransporte
    {
        /// <summary>
        /// 1 - Rodoviário Tração
        /// </summary>
        [XmlEnum("1")]
        RodoviarioTracao = 1,

        /// <summary>
        /// 2 - Rodoviário Reboque
        /// </summary>
        [XmlEnum("2")]
        RodoviarioReboque = 2,

        /// <summary>
        /// 3 - Navio
        /// </summary>
        [XmlEnum("3")]
        Navio = 3,

        /// <summary>
        /// 4 - Balsa
        /// </summary>
        [XmlEnum("4")]
        Balsa = 4,

        /// <summary>
        /// 5 - Aeronave
        /// </summary>
        [XmlEnum("5")]
        Aeronave = 5,

        /// <summary>
        /// 6 - Vagão
        /// </summary>
        [XmlEnum("6")]
        Vagao = 6,

        /// <summary>
        /// 7 - Outros
        /// </summary>
        [XmlEnum("7")]
        Outros = 7
    }

    #endregion

    #region TipoDocumentoOriginarioCTe

    /// <summary>
    /// Tipo de Documento Originário - CTe
    /// </summary>
    public enum TipoDocumentoOriginarioCTe
    {
        /// <summary>
        /// 00 - Declaração
        /// </summary>
        [XmlEnum("00")]
        Declaracao = 0,

        /// <summary>
        /// 10 - Dutoviário
        /// </summary>
        [XmlEnum("10")]
        Dutoviario = 10,

        /// <summary>
        /// 59 - CF-e SAT
        /// </summary>
        [XmlEnum("59")]
        CFeSAT = 59,

        /// <summary>
        /// 65 - NFC-e
        /// </summary>
        [XmlEnum("65")]
        NFCe = 65,

        /// <summary>
        /// 99 - Outros
        /// </summary>
        [XmlEnum("99")]
        Outros = 99
    }

    #endregion

    #region TipoDocumentoTransporteAnteriorCTe

    /// <summary>
    /// Tipo do documento de Transporte Anterior - CTe
    /// </summary>
    public enum TipoDocumentoTransporteAnteriorCTe
    {
        /// <summary>
        /// 07-ATRE
        /// </summary>
        [XmlEnum("07")]
        ATRE = 07,

        /// <summary>
        /// 08-DTA (Despacho de Transito Aduaneiro)
        /// </summary>
        [XmlEnum("08")]
        DTA = 08,

        /// <summary>
        /// 09-Conhecimento Aéreo Internacional
        /// </summary>
        [XmlEnum("09")]
        ConhecimentoAereoInternacional = 09,

        /// <summary>
        /// 10 – Conhecimento - Carta de Porte Internacional
        /// </summary>
        [XmlEnum("10")]
        ConhecimentoCartaDePorteInternacional = 10,

        /// <summary>
        /// 11 – Conhecimento Avulso
        /// </summary>
        [XmlEnum("11")]
        ConhecimentoAvulso = 11,

        /// <summary>
        /// 12-TIF (Transporte Internacional Ferroviário)
        /// </summary>
        [XmlEnum("12")]
        TIF = 12,

        /// <summary>
        /// 13-BL (Bill of Lading)
        /// </summary>
        [XmlEnum("13")]
        BL = 13
    }

    #endregion

    #region Indicador Negociável

    /// <summary>
    /// Indicador Negociável - CTe
    /// </summary>
    public enum IndicadorNegociavelCTe
    {
        /// <summary>
        /// 0 - Não negociável
        /// </summary>
        [XmlEnum("0")]
        NaoNegociavel = 0,

        /// <summary>
        /// 1 - Negociável
        /// </summary>
        [XmlEnum("1")]
        Negociavel = 1
    }

    #endregion

    #region Tipo Autor do Cancelamento por Substituição da NFCe

    /// <summary>
    /// Tipo Autor para Cancelamento por Substituição da NFCe e EPEC
    /// </summary>
    public enum TipoAutor
    {
        /// <summary>
        /// 1 - Empresa Emitente/Pessoa Física
        /// </summary>
        [XmlEnum("1")]
        EmpresaEmitente = 1,

        /// <summary>
        /// 2 - Empresa destinatária
        /// </summary>
        [XmlEnum("2")]
        EmpresaDestinataria = 2,

        /// <summary>
        /// 3 - Empresa
        /// </summary>
        [XmlEnum("3")]
        Empresa = 3,

        /// <summary>
        /// 5 - Fisco,
        /// </summary>
        [XmlEnum("5")]
        Fisco = 5,

        /// <summary>
        /// 6 - RFB,
        /// </summary>
        [XmlEnum("6")]
        RFB = 6,

        /// <summary>
        /// 9 - Outros Órgãos
        /// </summary>
        [XmlEnum("9")]
        OutrosOrgaos = 9
    }

    #endregion

    #region Tipo de Emitente do MDFe

    /// <summary>
    /// Tipo de emitente do MDFe
    /// </summary>
    public enum TipoEmitenteMDFe
    {
        /// <summary>
        /// 1 = Prestador de serviço de transporte
        /// </summary>
        [XmlEnum("1")]
        PrestadorServicoTransporte = 1,

        /// <summary>
        /// Trasportador de Carga Própria
        /// </summary>
        [XmlEnum("2")]
        TransportadorCargaPropria = 2,

        /// <summary>
        /// Prestador de serviço de transporte que emitirá CT-e Globalizado.
        /// </summary>
        [XmlEnum("3")]
        PrestadorServicoTransporteCteGlobalizado = 3
    }

    #endregion

    #region Tipo do Transportador para o MDFe

    /// <summary>
    /// Tipo de transportador para o MDFe
    /// </summary>
    public enum TipoTransportadorMDFe
    {
        /// <summary>
        /// 1 = ETC (Empresas de Transporte de Cargas)
        /// </summary>
        [XmlEnum("1")]
        ETC = 1,

        /// <summary>
        /// 2 = TAC (Transportador Autônomo de Carga)
        /// </summary>
        [XmlEnum("2")]
        TAC = 2,

        /// <summary>
        /// 3 = CTC (Cooperativa de Transporte de Cargas)
        /// </summary>
        [XmlEnum("3")]
        CTC = 3
    }

    #endregion

    #region Modalidades de transportes do MDFe

    /// <summary>
    /// Modalidade de transportes do MDFe
    /// </summary>
    public enum ModalidadeTransporteMDFe
    {
        /// <summary>
        /// 1 - Rodoviário
        /// </summary>
        [XmlEnum("1")]
        Rodoviario = 1,

        /// <summary>
        /// 2 - Aéreo
        /// </summary>
        [XmlEnum("2")]
        Aereo = 2,

        /// <summary>
        /// 3 - Aquaviário
        /// </summary>
        [XmlEnum("3")]
        Aquaviario = 3,

        /// <summary>
        /// 4 - Ferroviário
        /// </summary>
        [XmlEnum("4")]
        Ferroviario = 4
    }

    #endregion

    #region Responsavel pelo Seguro - MDFe

    /// <summary>
    /// Responsável pelo Seguro do MDFe
    /// </summary>
    public enum ResponsavelSeguroMDFe
    {
        /// <summary>
        /// 1 - Emitente do MDF-e
        /// </summary>
        [XmlEnum("1")]
        EmitenteMDFe = 1,

        /// <summary>
        /// 2 - Responsável pela contratação do serviço de transporte (contratante)
        /// </summary>
        [XmlEnum("2")]
        ContratanteServicoTransporte = 2
    }

    #endregion

    #region Responsavel pelo Seguro - CTeOS

    /// <summary>
    /// Responsável pelo Seguro do CTeOS
    /// </summary>
    public enum ResponsavelSeguroCTeOS
    {
        /// <summary>
        /// 4 - Emitente do CT-e OS
        /// </summary>
        [XmlEnum("4")]
        EmitenteCTeOS = 4,

        /// <summary>
        /// 5 - Tomador de Serviço
        /// </summary>
        [XmlEnum("5")]
        TomadorServico = 5
    }

    #endregion

    #region Tipo da Carga MDFe

    /// <summary>
    /// Tipo de Carga para o MDFe
    /// </summary>
    public enum TipoCargaMDFe
    {
        /// <summary>
        /// 01 - Granel sólido
        /// </summary>
        [XmlEnum("01")]
        GranelSolido = 1,

        /// <summary>
        /// 02 - Granel líquido
        /// </summary>
        [XmlEnum("02")]
        GranelLiquido = 2,

        /// <summary>
        /// 03 - Frigorificada
        /// </summary>
        [XmlEnum("03")]
        Frigorificada = 3,

        /// <summary>
        /// 04 - Conteinerizada
        /// </summary>
        [XmlEnum("04")]
        Conteinerizada = 4,

        /// <summary>
        /// 05 - Carga Geral
        /// </summary>
        [XmlEnum("05")]
        CargaGeral = 5,

        /// <summary>
        /// 06 - Neogranel
        /// </summary>
        [XmlEnum("06")]
        Neogranel = 6,

        /// <summary>
        /// 07 - Perigosa (granel sólido)
        /// </summary>
        [XmlEnum("07")]
        PerigosaGranelSolido = 7,

        /// <summary>
        /// 08 - Perigosa (granel líquido)
        /// </summary>
        [XmlEnum("08")]
        PerigosaGranelLiquido = 8,

        /// <summary>
        /// 09 - Perigosa (carga frigorificada)
        /// </summary>
        [XmlEnum("09")]
        PerigosaFrigorificada = 9,

        /// <summary>
        /// 10 - Perigosa (conteinerizada)
        /// </summary>
        [XmlEnum("10")]
        PerigosaConteinerizada = 10,

        /// <summary>
        /// 11 - Perigosa (carga geral)
        /// </summary>
        [XmlEnum("11")]
        PerigosaCargaGeral = 11
    }

    #endregion

    #region Código da Unidade de Medida - MDFe

    /// <summary>
    /// Código da Unidade de Medida - MDFe
    /// </summary>
    public enum CodigoUnidadeMedidaMDFe
    {
        /// <summary>
        /// 01-KG
        /// </summary>
        [XmlEnum("01")]
        KG = 1,

        /// <summary>
        /// 02-TON
        /// </summary>
        [XmlEnum("02")]
        TON = 2
    }

    #endregion

    #region Tipo do Componente do MDFe

    /// <summary>
    /// Tipo do componente para o MDFe
    /// </summary>
    public enum TipoComponenteMDFe
    {
        /// <summary>
        /// 01 = Vale Pedágio
        /// </summary>
        [XmlEnum("01")]
        ValePedagio = 1,

        /// <summary>
        /// 02 = Impostos, Taxas e Contribuições
        /// </summary>
        [XmlEnum("02")]
        ImpostosTaxasContribuicoes = 2,

        /// <summary>
        /// 03 = Despesas (Bancárias, Meios de pagamento, Outras)
        /// </summary>
        [XmlEnum("03")]
        Despesas = 3,

        /// <summary>
        /// 99 = Outras
        /// </summary>
        [XmlEnum("99")]
        Outros = 99
    }

    #endregion

    #region Tipo Proprietário para MDFe

    /// <summary>
    /// Tipo do proprietário para o MDFe
    /// </summary>
    public enum TipoProprietarioMDFe
    {
        /// <summary>
        /// 0 = TAC - Agregado
        /// </summary>
        [XmlEnum("0")]
        TACAgregado = 0,

        /// <summary>
        /// 1 = TAC - Independente
        /// </summary>
        [XmlEnum("1")]
        TACIndependente = 1,

        /// <summary>
        /// 2 = Outros
        /// </summary>
        [XmlEnum("2")]
        Outros = 2,

        /// <summary>
        /// 99999 = Não definido, utilizar nos casos que não deseja enviar o valor na tag.
        /// </summary>
        NaoDefinido = 99999
    }

    #endregion

    #region Tipo de Rodado para o MDFe

    /// <summary>
    /// Tipo de Rodado para o MDFe
    /// </summary>
    public enum TipoRodado
    {
        /// <summary>
        /// 01 - Truck
        /// </summary>
        [XmlEnum("01")]
        Truck = 1,

        /// <summary>
        /// 02 - Toco
        /// </summary>
        [XmlEnum("02")]
        Toco = 2,

        /// <summary>
        /// 03 - Cavalo Mecânico
        /// </summary>
        [XmlEnum("03")]
        CavaloMecanico = 3,

        /// <summary>
        /// 04 - VAN
        /// </summary>
        [XmlEnum("04")]
        VAN = 4,

        /// <summary>
        /// 05 - Utilitário
        /// </summary>
        [XmlEnum("05")]
        Utilitario = 5,

        /// <summary>
        /// 06 - Outros
        /// </summary>
        [XmlEnum("06")]
        Outros = 6
    }

    #endregion

    #region Tipo de Carroceira para o MDFe

    /// <summary>
    /// Tipo de Carroceria para o MDFe
    /// </summary>
    public enum TipoCarroceriaMDFe
    {
        /// <summary>
        /// 00 - Não aplicável
        /// </summary>
        [XmlEnum("00")]
        NaoAplicavel = 0,

        /// <summary>
        /// 01 - Aberta
        /// </summary>
        [XmlEnum("01")]
        Aberta = 1,

        /// <summary>
        /// 02 - Fechada/Bau
        /// </summary>
        [XmlEnum("02")]
        FechadaBau = 2,

        /// <summary>
        /// 03 - Granelera
        /// </summary>
        [XmlEnum("03")]
        Granelera = 3,

        /// <summary>
        /// 04 - Porta Container
        /// </summary>
        [XmlEnum("04")]
        PortaContainer = 4,

        /// <summary>
        /// 05 - Sider
        /// </summary>
        [XmlEnum("05")]
        Sider = 5
    }

    #endregion

    #region Tipo de Navegação para o CTe e MDFe

    /// <summary>
    /// Tipo de Navegação para o CTe e MDFe
    /// </summary>
    public enum TipoNavegacao
    {
        /// <summary>
        /// 0 - Interior
        /// </summary>
        [XmlEnum("0")]
        Interior = 0,

        /// <summary>
        /// 1 - Cabotagem
        /// </summary>
        [XmlEnum("1")]
        Cabotagem = 1,

        /// <summary>
        /// 99999 = Não definido, utilizar nos casos que não deseja enviar o valor na tag.
        /// </summary>
        NaoDefinido = 99999
    }

    #endregion

    #region Tipo de Fretamento para o CTeOS

    /// <summary>
    /// Tipo de fretamento para o CTeOS
    /// </summary>
    public enum TipoFretamentoCTeOS
    {
        /// <summary>
        /// 1 - Eventual
        /// </summary>
        [XmlEnum("1")]
        Eventual = 1,

        /// <summary>
        /// 2 - Contínuo
        /// </summary>
        [XmlEnum("2")]
        Continuo = 2
    }

    #endregion

    #region 

    /// <summary>
    /// Informações de manuseio CTe - Modal Aereo
    /// </summary>
    public enum InformacaoManuseioCTe
    {
        /// <summary>
        /// 01 - certificado do expedidor para embarque de animal vivo
        /// </summary>
        [XmlEnum("01")]
        CertificadoExpedidorEmbarqueAnimalVivo = 01,

        /// <summary>
        /// 02 - artigo perigoso conforme Declaração do Expedidor anexa
        /// </summary>
        [XmlEnum("02")]
        ArtigoPerigosoConformeDeclaracaoExpedidor = 02,

        /// <summary>
        /// 03 - somente em aeronave cargueira
        /// </summary>
        [XmlEnum("03")]
        SomenteAeronaveCargueira = 03,

        /// <summary>
        /// 04 - artigo perigoso - declaração do expedidor não requerida
        /// </summary>
        [XmlEnum("04")]
        ArtigoPerigosoDeclaracaoExpedidorNaoRequerida = 04,

        /// <summary>
        /// 05 - artigo perigoso em quantidade isenta
        /// </summary>
        [XmlEnum("05")]
        ArtigoPerigosoQuantidadeIsenta = 05,

        /// <summary>
        /// 06 - gelo seco para refrigeração (especificar no campo observações a quantidade)
        /// </summary>
        [XmlEnum("06")]
        GeloSecoRefrigeracao = 06,

        /// <summary>
        /// 07 - não restrito (especificar a Disposição Especial no campo observações)
        /// </summary>
        [XmlEnum("07")]
        NaoRestrito = 07,

        /// <summary>
        /// 08 - artigo perigoso em carga consolidada (especificar a quantidade no campo observações)
        /// </summary>
        [XmlEnum("08")]
        ArtigoPerigosoCargaConsolidada = 08,

        /// <summary>
        /// 09 - autorização da autoridade governamental anexa (especificar no campo observações)
        /// </summary>
        [XmlEnum("09")]
        AutorizacaoAutoridadeGovernamental = 09,

        /// <summary>
        /// 10 – baterias de íons de lítio em conformidade com a Seção II da PI965 – CAO
        /// </summary>
        [XmlEnum("10")]
        BateriasIonsLitioSecaoIIdaPI965 = 10,

        /// <summary>
        /// 11 - baterias de íons de lítio em conformidade com a Seção II da PI966
        /// </summary>
        [XmlEnum("11")]
        BateriasIonsLitioSecaoIIdaPI966 = 11,

        /// <summary>
        /// 12 - baterias de íons de lítio em conformidade com a Seção II da PI967
        /// </summary>
        [XmlEnum("12")]
        BateriasIonsLitioSecaoIIdaPI967 = 12,

        /// <summary>
        /// 13 – baterias de metal lítio em conformidade com a Seção II da PI968 — CAO
        /// </summary>
        [XmlEnum("13")]
        BateriasMetalLitioSecaoIIdaPI968 = 13,

        /// <summary>
        /// 14 - baterias de metal lítio em conformidade com a Seção II da PI969
        /// </summary>
        [XmlEnum("14")]
        BateriasMetalLitioSecaoIIdaPI969 = 14,

        /// <summary>
        /// 15 - baterias de metal lítio em conformidade com a Seção II da PI970
        /// </summary>
        [XmlEnum("15")]
        BateriasMetalLitioSecaoIIdaPI970 = 15,

        /// <summary>
        /// 99 - outro (especificar no campo observações) 
        /// </summary>
        [XmlEnum("99")]
        Outro = 99
    }

    #endregion

    #region Unidade de Medida de Artigos Perigosos - CTe

    /// <summary>
    /// Unidade de Medida de Artigos Perigosos - CTe
    /// </summary>
    public enum UnidadeMedidaArtigoPerigoso
    {
        /// <summary>
        /// 1-KG
        /// </summary>
        [XmlEnum("1")]
        KG = 1,

        /// <summary>
        /// 2-KG G (quilograma bruto)
        /// </summary>
        [XmlEnum("2")]
        KGG = 2,

        /// <summary>
        /// 3-Litros
        /// </summary>
        [XmlEnum("3")]
        Litros = 3,

        /// <summary>
        /// 4-TI (Índice de transporte para radioativos)
        /// </summary>
        [XmlEnum("4")]
        TI = 4,

        /// <summary>
        /// 5- Unidades (apenas para artigos perigosos medidos em unidades que não se enquadram nos itens acima. Exemplo: baterias, celulares, equipamentos, veículos, dentre outros)
        /// </summary>
        [XmlEnum("5")]
        Unidades = 5
    }

    #endregion

    #region Direção

    /// <summary>
    /// Direção - CTe
    /// </summary>
    public enum DirecaoCTe
    {
        /// <summary>
        /// N - Norte
        /// </summary>
        [XmlEnum("N")]
        Norte = 0,

        /// <summary>
        /// L - Leste
        /// </summary>
        [XmlEnum("L")]
        Leste = 1,

        /// <summary>
        /// S - Sul
        /// </summary>
        [XmlEnum("S")]
        Sul = 2,

        /// <summary>
        /// O - Oeste
        /// </summary>
        [XmlEnum("O")]
        Oeste = 3
    }

    #endregion

    #region Tipo de Tráfego - CTe

    /// <summary>
    /// Tipo de tráfego para o CTe
    /// </summary>
    public enum TipoTrafegoCTe
    {
        /// <summary>
        /// 0-Próprio
        /// </summary>
        [XmlEnum("0")]
        Proprio = 0,

        /// <summary>
        /// 1-Mútuo
        /// </summary>
        [XmlEnum("1")]
        Mutuo = 1,

        /// <summary>
        /// 2-Rodoferrovio
        /// </summary>
        [XmlEnum("2")]
        Rodoferroviario = 2,

        /// <summary>
        /// 3-Rodoviário
        /// </summary>
        [XmlEnum("3")]
        Rodoviario = 3
    }

    #endregion

    #region Responsável pelo Faturamento CTe

    /// <summary>
    /// "Responsável pelo Faturamento" ou "Ferrovia Emitente do CTe"
    /// </summary>
    public enum FerroviaCTe
    {
        /// <summary>
        /// 1 - Ferrovia de Origem
        /// </summary>
        [XmlEnum("1")]
        FerroviaOrigem = 1,

        /// <summary>
        /// Ferrovia de Destino
        /// </summary>
        [XmlEnum("2")]
        FerroviaDestino = 2
    }

    #endregion

    #region Campos GNRE

    /// <summary>
    /// Campos GNRE
    /// </summary>
    public enum CamposGNRE
    {
        /// <summary>
        /// c01_UfFavorecida
        /// </summary>
        [XmlEnum("c01_UfFavorecida")]
        c01_UfFavorecida,

        /// <summary>
        /// c02_receita
        /// </summary>
        [XmlEnum("c02_receita")]
        c02_receita,

        /// <summary>
        /// c25_detalhamentoReceita
        /// </summary>
        [XmlEnum("c25_detalhamentoReceita")]
        c25_detalhamentoReceita,

        /// <summary>
        /// c26_produto
        /// </summary>
        [XmlEnum("c26_produto")]
        c26_produto,

        /// <summary>
        /// c27_tipoIdentificacaoEmitente
        /// </summary>
        [XmlEnum("c27_tipoIdentificacaoEmitente")]
        c27_tipoIdentificacaoEmitente,

        /// <summary>
        /// c03_idContribuinteEmitente
        /// </summary>
        [XmlEnum("c03_idContribuinteEmitente")]
        c03_idContribuinteEmitente,

        /// <summary>
        /// c28_tipoDocOrigem
        /// </summary>
        [XmlEnum("c28_tipoDocOrigem")]
        c28_tipoDocOrigem,

        /// <summary>
        /// c04_docOrigem
        /// </summary>
        [XmlEnum("c04_docOrigem")]
        c04_docOrigem,

        /// <summary>
        /// c06_valorPrincipal
        /// </summary>
        [XmlEnum("c06_valorPrincipal")]
        c06_valorPrincipal,

        /// <summary>
        /// c07_atualizacaoMonetaria
        /// </summary>
        [XmlEnum("c07_atualizacaoMonetaria")]
        c07_atualizacaoMonetaria,

        /// <summary>
        /// c08_juros
        /// </summary>
        [XmlEnum("c08_juros")]
        c08_juros,

        /// <summary>
        /// c09_multa
        /// </summary>
        [XmlEnum("c09_multa")]
        c09_multa,

        /// <summary>
        /// c10_valorTotal
        /// </summary>
        [XmlEnum("c10_valorTotal")]
        c10_valorTotal,

        /// <summary>
        /// c14_dataVencimento
        /// </summary>
        [XmlEnum("c14_dataVencimento")]
        c14_dataVencimento,

        /// <summary>
        /// c33_dataPagamento
        /// </summary>
        [XmlEnum("c33_dataPagamento")]
        c33_dataPagamento,

        /// <summary>
        /// c29_dataLimitePagamento
        /// </summary>
        [XmlEnum("c29_dataLimitePagamento")]
        c29_dataLimitePagamento,

        /// <summary>
        /// c15_convenio
        /// </summary>
        [XmlEnum("c15_convenio")]
        c15_convenio,

        /// <summary>
        /// c16_razaoSocialEmitente
        /// </summary>
        [XmlEnum("c16_razaoSocialEmitente")]
        c16_razaoSocialEmitente,

        /// <summary>
        /// c17_inscricaoEstadualEmitente
        /// </summary>
        [XmlEnum("c17_inscricaoEstadualEmitente")]
        c17_inscricaoEstadualEmitente,

        /// <summary>
        /// c18_enderecoEmitente
        /// </summary>
        [XmlEnum("c18_enderecoEmitente")]
        c18_enderecoEmitente,

        /// <summary>
        /// c19_municipioEmitente
        /// </summary>
        [XmlEnum("c19_municipioEmitente")]
        c19_municipioEmitente,

        /// <summary>
        /// c20_ufEnderecoEmitente
        /// </summary>
        [XmlEnum("c20_ufEnderecoEmitente")]
        c20_ufEnderecoEmitente,

        /// <summary>
        /// c21_cepEmitente
        /// </summary>
        [XmlEnum("c21_cepEmitente")]
        c21_cepEmitente,

        /// <summary>
        /// c22_telefoneEmitente
        /// </summary>
        [XmlEnum("c22_telefoneEmitente")]
        c22_telefoneEmitente,

        /// <summary>
        /// c34_tipoIdentificacaoDestinatario
        /// </summary>
        [XmlEnum("c34_tipoIdentificacaoDestinatario")]
        c34_tipoIdentificacaoDestinatario,

        /// <summary>
        /// c35_idContribuinteDestinatario
        /// </summary>
        [XmlEnum("c35_idContribuinteDestinatario")]
        c35_idContribuinteDestinatario,

        /// <summary>
        /// c36_inscricaoEstadualDestinatario
        /// </summary>
        [XmlEnum("c36_inscricaoEstadualDestinatario")]
        c36_inscricaoEstadualDestinatario,

        /// <summary>
        /// c37_razaoSocialDestinatario
        /// </summary>
        [XmlEnum("c37_razaoSocialDestinatario")]
        c37_razaoSocialDestinatario,

        /// <summary>
        /// c38_municipioDestinatario
        /// </summary>
        [XmlEnum("c38_municipioDestinatario")]
        c38_municipioDestinatario,

        /// <summary>
        /// c30_nossoNumero
        /// </summary>
        [XmlEnum("c30_nossoNumero")]
        c30_nossoNumero,

        /// <summary>
        /// c05_referencia
        /// </summary>
        [XmlEnum("c05_referencia")]
        c05_referencia,

        /// <summary>
        /// ufFavorecida
        /// </summary>
        [XmlEnum("ufFavorecida")]
        ufFavorecida,

        /// <summary>
        /// receita
        /// </summary>
        [XmlEnum("receita")]
        receita,

        /// <summary>
        /// detalhamentoReceita
        /// </summary>
        [XmlEnum("detalhamentoReceita")]
        detalhamentoReceita,

        /// <summary>
        /// periodo
        /// </summary>
        [XmlEnum("periodo")]
        periodo,

        /// <summary>
        /// produto
        /// </summary>
        [XmlEnum("produto")]
        produto,

        /// <summary>
        /// referencia
        /// </summary>
        [XmlEnum("referencia")]
        referencia,

        /// <summary>
        /// mes
        /// </summary>
        [XmlEnum("mes")]
        mes,

        /// <summary>
        /// ano
        /// </summary>
        [XmlEnum("ano")]
        ano,

        /// <summary>
        /// parcela
        /// </summary>
        [XmlEnum("parcela")]
        parcela,

        /// <summary>
        /// documentoOrigem
        /// </summary>
        [XmlEnum("documentoOrigem")]
        documentoOrigem,

        /// <summary>
        /// dataVencimento
        /// </summary>
        [XmlEnum("dataVencimento")]
        dataVencimento,

        /// <summary>
        /// dataPagamento
        /// </summary>
        [XmlEnum("dataPagamento")]
        dataPagamento,

        /// <summary>
        /// convenio
        /// </summary>
        [XmlEnum("convenio")]
        convenio,

        /// <summary>
        /// camposExtras
        /// </summary>
        [XmlEnum("camposExtras")]
        camposExtras,

        /// <summary>
        /// c39_campoExtra1
        /// </summary>
        [XmlEnum("c39_campoExtra1")]
        c39_campoExtra1,

        /// <summary>
        /// c40_campoExtra2
        /// </summary>
        [XmlEnum("c40_campoExtra2")]
        c40_campoExtra2,

        /// <summary>
        /// c41_campoExtra3
        /// </summary>
        [XmlEnum("c41_campoExtra3")]
        c41_campoExtra3,

        /// <summary>
        /// c39_camposExtras
        /// </summary>
        [XmlEnum("c39_camposExtras")]
        c39_camposExtras,

        /// <summary>
        /// campoExtra
        /// </summary>
        [XmlEnum("campoExtra")]
        campoExtra,

        /// <summary>
        /// codigo
        /// </summary>
        [XmlEnum("codigo")]
        codigo,

        /// <summary>
        /// tipo
        /// </summary>
        [XmlEnum("tipo")]
        tipo,

        /// <summary>
        /// valor
        /// </summary>
        [XmlEnum("valor")]
        valor,

        /// <summary>
        /// c42_identificadorGui
        /// </summary>
        [XmlEnum("c42_identificadorGuia")]
        c42_identificadorGuia
    }

    #endregion

    #region Campos GNRE 2

    /// <summary>
    /// Campos GNRE 2
    /// </summary>
    public enum CamposGNRE2
    {
        /// <summary>
        /// ufFavorecida
        /// </summary>        
        ufFavorecida,
        /// <summary>
        /// contribuinteEmitente
        /// </summary>        
        contribuinteEmitente,
        /// <summary>
        /// contribuinteEmitente_identificacao
        /// </summary>        
        contribuinteEmitente_identificacao,
        /// <summary>
        /// contribuinteEmitente_identificacao_CPF
        /// </summary>        
        contribuinteEmitente_identificacao_CPF,
        /// <summary>
        /// contribuinteEmitente_identificacao_CNPJ
        /// </summary>        
        contribuinteEmitente_identificacao_CNPJ,
        /// <summary>
        /// contribuinteEmitente_identificacao_IE
        /// </summary>        
        contribuinteEmitente_identificacao_IE,
        /// <summary>
        /// contribuinteEmitente_razaoSocial
        /// </summary>        
        contribuinteEmitente_razaoSocial,
        /// <summary>
        /// contribuinteEmitente_endereco
        /// </summary>        
        contribuinteEmitente_endereco,
        /// <summary>
        /// contribuinteEmitente_municipio
        /// </summary>        
        contribuinteEmitente_municipio,
        /// <summary>
        /// contribuinteEmitente_uf
        /// </summary>        
        contribuinteEmitente_uf,
        /// <summary>
        /// contribuinteEmitente_cep
        /// </summary>        
        contribuinteEmitente_cep,
        /// <summary>
        /// contribuinteEmitente_telefone
        /// </summary>        
        contribuinteEmitente_telefone,
        /// <summary>
        /// itensGNRE
        /// </summary>        
        itensGNRE,
        /// <summary>
        /// item
        /// </summary>        
        item,
        /// <summary>
        /// item_receita
        /// </summary>        
        item_receita,
        /// <summary>
        /// item_detalhamentoReceita
        /// </summary>        
        item_detalhamentoReceita,
        /// <summary>
        /// item_documentoOrigem
        /// </summary>        
        item_documentoOrigem,
        /// <summary>
        /// item_produto
        /// </summary>        
        item_produto,
        /// <summary>
        /// item_referencia
        /// </summary>        
        item_referencia,
        /// <summary>
        /// item_referencia_periodo
        /// </summary>        
        item_referencia_periodo,
        /// <summary>
        /// item_referencia_mes
        /// </summary>        
        item_referencia_mes,
        /// <summary>
        /// item_referencia_ano
        /// </summary>        
        item_referencia_ano,
        /// <summary>
        /// item_referencia_parcela
        /// </summary>        
        item_referencia_parcela,
        /// <summary>
        /// item_dataVencimento
        /// </summary>        
        item_dataVencimento,
        /// <summary>
        /// item_valorPrincipal
        /// </summary>        
        item_valorPrincipal,
        /// <summary>
        /// item_valorPrincipalFecp
        /// </summary>        
        item_valorPrincipalFecp,
        /// <summary>
        /// item_valorAtualizacaoMonetaria
        /// </summary>        
        item_valorAtualizacaoMonetaria,
        /// <summary>
        /// item_valorAtualizacaoMonetariaFecp
        /// </summary>        
        item_valorAtualizacaoMonetariaFecp,
        /// <summary>
        /// item_valorJuros
        /// </summary>        
        item_valorJuros,
        /// <summary>
        /// item_valorJurosFecp
        /// </summary>        
        item_valorJurosFecp,
        /// <summary>
        /// item_valorMulta
        /// </summary>        
        item_valorMulta,
        /// <summary>
        /// item_valorMultaFecp
        /// </summary>        
        item_valorMultaFecp,
        /// <summary>
        /// item_valorTotal
        /// </summary>        
        item_valorTotal,
        /// <summary>
        /// item_valorTotalFecp
        /// </summary>        
        item_valorTotalFecp,
        /// <summary>
        /// item_convenio
        /// </summary>        
        item_convenio,
        /// <summary>
        /// item_contribuinteDestinatario
        /// </summary>        
        item_contribuinteDestinatario,
        /// <summary>
        /// item_contribuinteDestinatario_identificacao
        /// </summary>        
        item_contribuinteDestinatario_identificacao,
        /// <summary>
        /// item_contribuinteDestinatario_identificacao_CNPJ
        /// </summary>        
        item_contribuinteDestinatario_identificacao_CNPJ,
        /// <summary>
        /// item_contribuinteDestinatario_identificacao_CPF
        /// </summary>        
        item_contribuinteDestinatario_identificacao_CPF,
        /// <summary>
        /// item_contribuinteDestinatario_identificacao_IE
        /// </summary>        
        item_contribuinteDestinatario_identificacao_IE,
        /// <summary>
        /// item_contribuinteDestinatario_razaoSocial
        /// </summary>        
        item_contribuinteDestinatario_razaoSocial,
        /// <summary>
        /// item_contribuinteDestinatario_municipio
        /// </summary>        
        item_contribuinteDestinatario_municipio,
        /// <summary>
        /// item_camposExtras
        /// </summary>        
        item_camposExtras,
        /// <summary>
        /// item_camposExtras_campoExtra
        /// </summary>        
        item_camposExtras_campoExtra,
        /// <summary>
        /// item_camposExtras_campoExtra_codigo
        /// </summary>        
        item_camposExtras_campoExtra_codigo,
        /// <summary>
        /// item_camposExtras_campoExtra_valor
        /// </summary>        
        item_camposExtras_campoExtra_valor,
        /// <summary>
        /// valorGNRE
        /// </summary>        
        valorGNRE,
        /// <summary>
        /// dataPagamento
        /// </summary>        
        dataPagamento,
        /// <summary>
        /// identificadorGuia
        /// </summary>        
        identificadorGuia,
        /// <summary>
        /// dataLimitePagamento
        /// </summary>        
        dataLimitePagamento,
        /// <summary>
        /// informacoesComplementares
        /// </summary>        
        informacoesComplementares,
        /// <summary>
        /// informacoesComplementares_informacao
        /// </summary>        
        informacoesComplementares_informacao,
        /// <summary>
        /// nossoNumero
        /// </summary>        
        nossoNumero,
        /// <summary>
        /// dadosPagamento
        /// </summary>        
        dadosPagamento,
        /// <summary>
        /// dadosPagamento_data
        /// </summary>        
        dadosPagamento_data,
        /// <summary>
        /// dadosPagamento_autenticacao
        /// </summary>        
        dadosPagamento_autenticacao,
        /// <summary>
        /// dadosPagamento_banco
        /// </summary>        
        dadosPagamento_banco,
        /// <summary>
        /// dadosPagamento_agencia
        /// </summary>        
        dadosPagamento_agencia,
        /// <summary>
        /// linhaDigitavel
        /// </summary>        
        linhaDigitavel,
        /// <summary>
        /// valor
        /// </summary>
        valor
    }

    #endregion

    #region SimNaoLetra

    /// <summary>
    /// Sim ou Não (S ou N)
    /// </summary>
    public enum SimNaoLetra
    {
        /// <summary>
        /// Não (0 ou N)
        /// </summary>
        [XmlEnum("N")]
        Nao = 0,

        /// <summary>
        /// Sim (1 ou N)
        /// </summary>
        [XmlEnum("S")]
        Sim = 1
    }

    #endregion

    #region SimNaoOpcionalLetra

    /// <summary>
    /// Sim ou Não (S ou N)
    /// </summary>
    public enum SimNaoOpcionalLetra
    {
        /// <summary>
        /// Não (0 ou N)
        /// </summary>
        [XmlEnum("N")]
        Nao = 0,

        /// <summary>
        /// Sim (1 ou N)
        /// </summary>
        [XmlEnum("S")]
        Sim = 1,

        /// <summary>
        /// Opcional (2 ou O)
        /// </summary>
        [XmlEnum("O")]
        Opcional = 2
    }

    #endregion

    #region TipoCampoExtraGNRE

    /// <summary>
    /// Tipo dos campos extras da GNRE
    /// </summary>
    public enum TipoCampoExtraGNRE
    {
        /// <summary>
        /// T - Texto
        /// </summary>
        [XmlEnum("T")]
        Texto = 0,

        /// <summary>
        /// N - Numérico
        /// </summary>
        [XmlEnum("N")]
        Numerico = 1,

        /// <summary>
        /// D - Data
        /// </summary>
        [XmlEnum("D")]
        Data = 2
    }

    #endregion

    #region Meses do Ano

    /// <summary>
    /// Meses do ano
    /// </summary>
    public enum Meses
    {
        /// <summary>
        /// 01 - Janeiro
        /// </summary>
        [XmlEnum("01")]
        Janeiro = 1,

        /// <summary>
        /// 02 - Fevereiro
        /// </summary>
        [XmlEnum("02")]
        Fevereiro = 2,

        /// <summary>
        /// 03 - Março
        /// </summary>
        [XmlEnum("03")]
        Marco = 3,

        /// <summary>
        /// 04 - Abril
        /// </summary>
        [XmlEnum("04")]
        Abril = 4,

        /// <summary>
        /// 05 - Maio
        /// </summary>
        [XmlEnum("05")]
        Maio = 5,

        /// <summary>
        /// 06 - Junho
        /// </summary>
        [XmlEnum("06")]
        Junho = 6,

        /// <summary>
        /// 07 - Julho
        /// </summary>
        [XmlEnum("07")]
        Julho = 7,

        /// <summary>
        /// 08 - Agosto
        /// </summary>
        [XmlEnum("08")]
        Agosto = 8,

        /// <summary>
        /// 09 - Setembro
        /// </summary>
        [XmlEnum("09")]
        Setembro = 9,

        /// <summary>
        /// 10 - Outubro
        /// </summary>
        [XmlEnum("10")]
        Outubro = 10,

        /// <summary>
        /// 11 - Novembro
        /// </summary>
        [XmlEnum("11")]
        Novembro = 11,

        /// <summary>
        /// 12 - Dezembro
        /// </summary>
        [XmlEnum("12")]
        Dezembro = 12
    }

    #endregion

    #region Tipos de GNRE

    /// <summary>
    /// Tipos de Guias da GNRE
    /// </summary>
    public enum TipoGuiaGNRE
    {
        /// <summary>
        /// 0 - Guia Simples
        /// </summary>
        [XmlEnum("0")]
        Simples = 0,

        /// <summary>
        /// 1 - Guia com Múltiplos Documentos de Origem
        /// </summary>
        [XmlEnum("1")]
        MultiplosDocumentosOrigem = 1,

        /// <summary>
        /// 2 - Guia com Múltiplas Receitas
        /// </summary>
        [XmlEnum("2")]
        MultiplasReceitas = 2
    }

    #endregion

    #region Situação Guia GNRE

    /// <summary>
    /// Situação da Guia GNRE
    /// </summary>
    public enum SituacaoGuiaGNRE
    {
        /// <summary>
        /// 0 - Processada com Sucesso
        /// </summary>
        [XmlEnum("0")]
        ProcessadaComSucesso = 0,

        /// <summary>
        /// 1 - Invalidada pelo Portal
        /// </summary>
        [XmlEnum("1")]
        InvalidadaPeloPortal = 1,

        /// <summary>
        /// 2 - Invalidada pela UF
        /// </summary>
        [XmlEnum("2")]
        InvalidadaPelaUF = 2,

        /// <summary>
        /// 3 - Erro de comunicação
        /// </summary>
        [XmlEnum("3")]
        ErroDeComunicacao = 3
    }

    #endregion

    #region Indicador do Intermediário/Marketplace

    /// <summary>
    /// Indicador do Intermediário/Marketplace
    /// </summary>
    public enum IndicadorIntermediario
    {

        /// <summary>
        /// 0 = Operação sem intermediador (em site ou plataforma própria)
        /// </summary>
        [XmlEnum("0")]
        OperacaoSemIntermediador = 0,

        /// <summary>
        /// 1 = Operação em site ou plataforma de terceiros (intermediadores/marketplace)
        /// </summary>
        [XmlEnum("1")]
        OperacaoSitePlataformaTerceiro = 1
    }

    #endregion

    #region Indica se o valor do PISST compõe o valor total da NF-e

    /// <summary>
    /// Indica se o valor do PISST compõe o valor total da NF-e
    /// </summary>
    public enum IndicaSomaPISST
    {
        /// <summary>
        /// 0=Valor do PISST não compõe o valor total da NF-e,
        /// </summary>
        [XmlEnum("0")]
        ValorPISSTNaoCompoeValorTotalNFe = 0,

        /// <summary>
        /// 1=Valor do PISST compõe o valor total da NF-e
        /// </summary>
        [XmlEnum("1")]
        ValorPISSTCompoeValorTotalNFe = 1
    }

    #endregion

    #region Indica se o valor do COFINSST compõe o valor total da NF-e

    /// <summary>
    /// Indica se o valor do COFINSST compõe o valor total da NF-e
    /// </summary>
    public enum IndicaSomaCOFINSST
    {
        /// <summary>
        /// 0=Valor do COFINSST não compõe o valor total da NF-e,
        /// </summary>
        [XmlEnum("0")]
        ValorCOFINSSTNaoCompoeValorTotalNFe = 0,

        /// <summary>
        /// 1=Valor do COFINSST compõe o valor total da NF-e
        /// </summary>
        [XmlEnum("1")]
        ValorCOFINSSTCompoeValorTotalNFe = 1
    }

    #endregion

    #region Tipo de Vale Pedágio

    /// <summary>
    /// Tipo de vale pedágio
    /// </summary>
    public enum TipoValePedagio
    {
        /// <summary>
        /// TAG = 01
        /// </summary>
        [XmlEnum("01")]
        TAG = 1,

        /// <summary>
        /// Cupom = 01
        /// </summary>
        [XmlEnum("02")]
        Cupom = 2,

        /// <summary>
        /// Cartão = 01
        /// </summary>
        [XmlEnum("03")]
        Cartao = 3
    }

    #endregion

    #region Categoria de Combinação Veicular

    /// <summary>
    /// Categorias de combinação veicular
    /// </summary>
    public enum CategoriaCombinacaoVeicular
    {
        /// <summary>
        /// Veículo Comercial 2 Eixos = 02
        /// </summary>
        [XmlEnum("02")]
        VeiculoComercial2Eixos = 02,

        /// <summary>
        /// Veículo Comercial 3 Eixos = 04
        /// </summary>
        [XmlEnum("04")]
        VeiculoComercial3Eixos = 04,

        /// <summary>
        /// Veículo Comercial 4 Eixos = 06
        /// </summary>
        [XmlEnum("06")]
        VeiculoComercial4Eixos = 06,

        /// <summary>
        /// Veículo Comercial 5 Eixos = 07
        /// </summary>
        [XmlEnum("07")]
        VeiculoComercial5Eixos = 07,

        /// <summary>
        /// Veículo Comercial 6 Eixos = 08
        /// </summary>
        [XmlEnum("08")]
        VeiculoComercial6Eixos = 08,

        /// <summary>
        /// Veículo Comercial 7 Eixos = 10
        /// </summary>
        [XmlEnum("10")]
        VeiculoComercial7Eixos = 10,

        /// <summary>
        /// Veículo Comercial 8 eixos = 11
        /// </summary>
        [XmlEnum("11")]
        VeiculoComercial8Eixos = 11,

        /// <summary>
        /// Veículo Comercial 9 Eixos = 12
        /// </summary>
        [XmlEnum("12")]
        VeiculoComercial9Eixos = 12,

        /// <summary>
        /// Veículo Comercial 10 Eixos = 13
        /// </summary>
        [XmlEnum("13")]
        VeiculoComercial10Eixos = 13,

        /// <summary>
        /// Veículo Comercial Acima de 10 Eixos = 14
        /// </summary>
        [XmlEnum("14")]
        VeiculoComercialAcima10Eixos = 14
    }

    #endregion

    #region Padrões de NFSe

    /// <summary>
    /// Padrões de NFSe
    /// </summary>
    public enum PadraoNFSe
    {
        /// <summary>
        /// Não definido / Nenhum
        /// </summary>
        [Description("Nenhum")]
        None = 0,

        /// <summary>
        /// BETHA Sistemas
        /// </summary>
        [Description("BETHA Sistemas")]
        BETHA = 1,

        /// <summary>
        /// SIGCORP - Tecnologia da informação
        /// </summary>
        [Description("SIGCORP - Tecnologia da informação")]
        SIGCORP = 2,

        /// <summary>
        /// PRODATA - Gestão Estratégica
        /// </summary>
        [Description("PRODATA - Gestão Estratégica")]
        PRODATA = 3,

        /// <summary>
        /// EL - Produções de Software
        /// </summary>
        [Description("EL - Produções de Software")]
        EL = 4,

        /// <summary>
        /// NOTAINTELIGENTE
        /// </summary>
        [Description("Nota Inteligente")]
        NOTAINTELIGENTE = 5,

        /// <summary>
        /// AVMB Soluções em TI
        /// </summary>
        [Description("AVMB Soluções em TI")]
        AVMB = 6,

        /// <summary>
        /// WebISS
        /// </summary>
        [Description("WebISS")]
        WEBISS = 7,

        /// <summary>
        /// SIMPLISS Sistema de Informação LTDA
        /// </summary>
        [Description("SIMPLISS Sistema de Informação LTDA")]
        SIMPLISS = 8,

        /// <summary>
        /// COPLAN
        /// </summary>
        [Description("Coplan")]
        COPLAN = 9,

        /// <summary>
        /// Próprio Joinville SC 
        /// </summary>
        [Description("Próprio Joinville SC")]
        PROPRIOJOINVILLESC = 10,

        /// <summary>
        /// Sonner - Sistemas integrados, governos inteligentes (Antigo GOVDIGITAL)
        /// </summary>
        [Description("SONNER - Sistemas integrados")]
        SONNER = 11,

        /// <summary>
        /// SMARAPD
        /// </summary>
        [Description("SMARAPD")]
        SMARAPD = 12,

        /// <summary>
        /// NobeSistemas - Software de gestão integrada
        /// </summary>
        [Description("NOBESISTEMAS")]
        NOBESISTEMAS = 13,

        /// <summary>
        /// TRIBUTUS - Gestão integrada
        /// </summary>
        [Description("TRIBUTUS")]
        TRIBUTUS = 14,

        /// <summary>
        /// BHISS Digital
        /// </summary>
        [Description("BHISS")]
        BHISS = 15,

        /// <summary>
        /// PAULISTANA
        /// </summary>
        [Description("PAULISTANA")]
        PAULISTANA = 16,

        /// <summary>
        /// DSF - Desenvolvimento de Sistemas Fiscais Ltda
        /// </summary>
        [Description("DSF")]
        DSF = 17,

        /// <summary>
        /// Digifred - Soluções em Tecnologia para a Gestão Pública
        /// </summary>
        [Description("DIGIFRED")]
        DIGIFRED = 18,

        /// <summary>
        /// VersaTEC – Tecnologia, Educação e Comunicação
        /// </summary>
        [Description("VERSATEC")]
        VERSATEC = 19,

        /// <summary>
        /// GINFES – Gestão Inteligente da Nota Fiscal de Serviço Eletrônica
        /// </summary>
        [Description("GINFES")]
        GINFES = 20,

        /// <summary>
        /// QUASAR - Sistemas Inteligentes de Gestão
        /// </summary>
        [Description("QUASAR")]
        QUASAR = 21,

        /// <summary>
        /// Próprio Goiânia GO
        /// </summary>
        [Description("Próprio Goiânia GO")]
        PROPRIOGOIANIA = 22,

        /// <summary>
        /// OBARATEC
        /// </summary>
        [Description("OBARATEC")]
        OBARATEC = 23,

        /// <summary>
        /// GIF
        /// </summary>
        [Description("GIF")]
        GIF = 24,

        /// <summary>
        /// ISSNET
        /// </summary>
        [Description("ISSNET")]
        ISSNET = 25,

        /// <summary>
        /// CENTI
        /// </summary>
        [Description("CENTI")]
        CENTI = 26,

        /// <summary>
        /// NACIONAL
        /// </summary>
        [Description("NACIONAL")]
        NACIONAL = 27,

        /// <summary>
        /// GIAP
        /// </summary>
        [Description("GIAP")]
        GIAP = 28,

        /// <summary>
        /// EQUIPLANO 
        /// </summary>
        [Description("EQUIPLANO")]
        EQUIPLANO = 29,

        /// <summary>
        /// MEMORY 
        /// </summary>
        [Description("MEMORY")]
        MEMORY = 30,

        /// <summary>
        /// ABASE Sistemas
        /// </summary>
        [Description("ABASE")]
        ABASE = 31,

        /// <summary>
        /// FIORILLI
        /// </summary>
        [Description("FIORILLI")]
        FIORILLI = 32,

        /// <summary>
        /// IPM
        /// </summary>
        [Description("IPM")]
        IPM = 33,

        /// <summary>
        /// TECNOSISTEMAS
        /// </summary>
        [Description("TECNOSISTEMAS")]
        TECNOSISTEMAS = 34,

        /// <summary>
        /// BAUHAUS - Prefeitura Moderna
        /// </summary>
        [Description("BAUHAUS")]
        BAUHAUS = 35,

        /// <summary>
        /// TINUS
        /// </summary>
        [Description("TINUS")]
        TINUS = 36,

        /// <summary>
        /// EMBRAS
        /// </summary>
        [Description("EMBRAS")]
        EMBRAS = 37,

        /// <summary>
        /// BSITBR
        /// </summary>
        [Description("BSITBR")]
        BSITBR = 38,

        /// <summary>
        /// SIMPLE
        /// </summary>
        [Description("SIMPLE")]
        SIMPLE = 39,

        /// <summary>
        /// PRONIM
        /// </summary>
        [Description("PRONIM")]
        PRONIM = 40,

        /// <summary>
        /// PROPRIOBARUERISP
        /// </summary>
        [Description("PROPRIOBARUERISP")]
        PROPRIOBARUERISP = 41,

        /// <summary>
        /// THEMA
        /// </summary>
        [Description("THEMA")]
        THEMA = 42,

        /// <summary>
        /// AGILI
        /// </summary>
        [Description("AGILI")]
        AGILI = 43,

        /// <summary>
        /// WEBFISCO_TECNOLOGIA
        /// </summary>
        [Description("WEBFISCO")]
        WEBFISCO = 44,

        /// <summary>
        /// IIBRASIL
        /// </summary>
        [Description("IIBRASIL")]
        IIBRASIL = 45,
    }

    #endregion

    #region Tipos de Compoenentes da GTVe para CTeOS

    /// <summary>
    /// Tipos de componentes da GTVe para CTeOS
    /// </summary>
    public enum TipoComponenteGTVe
    {
        /// <summary>
        /// 1 - Custódia
        /// </summary>
        [XmlEnum("1")]
        Custodia = 1,

        /// <summary>
        /// 2 - Embarque
        /// </summary>
        [XmlEnum("2")]
        Embarque = 2,

        /// <summary>
        /// 3 - Tempo de espera
        /// </summary>
        [XmlEnum("3")]
        TempoEspera = 3,

        /// <summary>
        /// 4 - Malote
        /// </summary>
        [XmlEnum("4")]
        Malote = 4,

        /// <summary>
        /// 5 - Ad Valorem
        /// </summary>
        [XmlEnum("5")]
        AdValorem = 5,

        /// <summary>
        /// 6 - Outros
        /// </summary>
        [XmlEnum("6")]
        Outros = 6
    }

    #endregion

    #region TipoXML

    /// <summary>
    /// Tipos de XML
    /// </summary>
    public enum TipoXML
    {
        #region NFe/NFCe

        /// <summary>
        /// XML de consulta status do serviço da NFe/NFCe
        /// </summary>
        [Description("XML de consulta status do serviço da NFe/NFCe")]
        NFeStatusServico = 0,

        /// <summary>
        /// XML de consulta situação da NFe/NFCe
        /// </summary>
        [Description("XML de consulta situação da NFe/NFCe")]
        NFeConsultaSituacao = 1,

        /// <summary>
        /// XML de consulta do recibo do lote da NFe/NFCe
        /// </summary>
        [Description("XML de consulta do recibo do lote da NFe/NFCe")]
        NFeConsultaRecibo = 2,

        /// <summary>
        /// XML de consulta cadastro do contribuinte da NFe/NFCe
        /// </summary>
        [Description("XML de consulta cadastro do contribuinte da NFe/NFCe")]
        NFeConsultaCadastro = 3,

        /// <summary>
        /// XML de consulta dos documentos fiscais eletrônicos distribuídos da NFe/NFCe
        /// </summary>
        [Description("XML de consulta dos documentos fiscais eletrônicos distribuídos da NFe/NFCe")]
        NFeDistribuicaoDFe = 4,

        /// <summary>
        /// XML de envio de evento da NFe/NFCe
        /// </summary>
        [Description("XML de envio de evento da NFe/NFCe")]
        NFeEnvioEvento = 5,

        /// <summary>
        /// XML de Inutilização da NFe/NFCe
        /// </summary>
        [Description("XML de Inutilização da NFe/NFCe")]
        NFeInutilizacao = 6,

        /// <summary>
        /// XML individual da NFe/NFCe
        /// </summary>
        [Description("XML individual da NFe/NFCe")]
        NFe = 7,

        /// <summary>
        /// XML de envio em lote da NFe/NFCe
        /// </summary>
        [Description("XML de envio em lote da NFe/NFCe")]
        NFeEnvioEmLote = 8,

        /// <summary>
        /// XML de distribuição da NFe com protocolo de autorização anexado
        /// </summary>
        [Description("XML de distribuição da NFe com protocolo de autorização anexado")]
        NFeDistribuicao = 9,

        #endregion 

        #region CTe

        /// <summary>
        /// XML de consulta status do serviço do CTe
        /// </summary>
        [Description("XML de consulta status do serviço do CTe")]
        CTeStatusServico = 10,

        /// <summary>
        /// XML de consulta situação do CTe
        /// </summary>
        [Description("XML de consulta situação do CTe")]
        CTeConsultaSituacao = 11,

        /// <summary>
        /// XML de consulta do recibo do lote do CTe
        /// </summary>
        [Description("XML de consulta do recibo do lote do CTe")]
        CTeConsultaRecibo = 12,

        /// <summary>
        /// XML de envio de evento do CTe
        /// </summary>
        [Description("XML de envio de evento do CTe")]
        CTeEnvioEvento = 13,

        /// <summary>
        /// XML individual do CTe
        /// </summary>
        [Description("XML individual do CTe")]
        CTe = 15,

        /// <summary>
        /// XML de envio em lote do CTe
        /// </summary>
        [Description("XML de envio em lote do CTe")]
        CTeEnvioEmLote = 16,

        /// <summary>
        /// XML do CTeOS
        /// </summary>
        [Description("XML do CTeOS")]
        CTeOS = 17,

        /// <summary>
        /// XML de consulta dos documentos fiscais eletrônicos distribuídos do CTe
        /// </summary>
        [Description("XML de consulta dos documentos fiscais eletrônicos distribuídos do CTe")]
        CTeDistribuicaoDFe = 18,

        /// <summary>
        /// XML de distribuição da NFe com protocolo de autorização anexado
        /// </summary>
        [Description("XML de distribuição da NFe com protocolo de autorização anexado")]
        CTeDistribuicao = 19,

        #endregion 

        #region MDFe

        /// <summary>
        /// XML de consulta status do serviço do MDFe
        /// </summary>
        [Description("XML de consulta status do serviço do MDFe")]
        MDFeStatusServico = 20,

        /// <summary>
        /// XML de consulta situação do MDFe
        /// </summary>
        [Description("XML de consulta situação do MDFe")]
        MDFeConsultaSituacao = 21,

        /// <summary>
        /// XML de consulta do recibo do lote do MDFe
        /// </summary>
        [Description("XML de consulta do recibo do lote do MDFe")]
        MDFeConsultaRecibo = 22,

        /// <summary>
        /// XML de envio de evento do MDFe
        /// </summary>
        [Description("XML de envio de evento do MDFe")]
        MDFeEnvioEvento = 23,

        /// <summary>
        /// XML individual do MDFe
        /// </summary>
        [Description("XML individual do MDFe")]
        MDFe = 24,

        /// <summary>
        /// XML de envio em lote do MDFe
        /// </summary>
        [Description("XML de envio em lote do MDFe")]
        MDFeEnvioEmLote = 25,

        /// <summary>
        /// XML de consulta dos MDFe´s não encerrados
        /// </summary>
        [Description("XML de consulta dos MDFe´s não encerrados")]
        MDFeConsultaNaoEncerrado = 26,

        /// <summary>
        /// XML de distribuição da NFe com protocolo de autorização anexado
        /// </summary>
        [Description("XML de distribuição da NFe com protocolo de autorização anexado")]
        MDFeDistribuicao = 27,

        #endregion 

        /// <summary>
        /// Não foi possível identificar o tipo do XML
        /// </summary>
        [Description("Não foi possível identificar o tipo do XML")]
        NaoIdentificado = 9999
    }

    #endregion

    #region Tipos de Consultas da GNRE

    /// <summary>
    /// Tipos de Consultas da GNRE
    /// </summary>
    public enum TipoConsultaGNRE
    {
        /// <summary>
        /// C - Consulta por código de barras.
        /// </summary>
        [XmlEnum("C")]
        ConsultaPorCodigoBarra = 0,

        /// <summary>
        /// N - Consulta por Número de Controle da GNRE.
        /// </summary>
        [XmlEnum("N")]
        ConsultaPorNumeroControleGNRE = 1,

        /// <summary>
        /// D - Consulta por Documento de Origem.
        /// </summary>
        [XmlEnum("D")]
        ConsultaPorDocumentOrigem = 2,
    }

    #endregion

    #region Tipos de ato concessório

    /// <summary>
    /// Tipos do ato concessório
    /// </summary>
    public enum TipoAtoConcessorio
    {
        /// <summary>
        /// 08 - Termo de Acordo
        /// </summary>
        [XmlEnum("08")]
        TermoDeAcordo = 08,

        /// <summary>
        /// 10 - Regime Especial
        /// </summary>
        [XmlEnum("10")]
        RegimeEspecial = 10,

        /// <summary>
        /// 12 = Autorização Específica
        /// </summary>
        [XmlEnum("12")]
        AutorizacaoEspecifica = 12
    }

    #endregion

    #region IdentificacaoAmbiente

    /// <summary>
    /// Identificação do ambiente (Produção ou Teste) - SNCM
    /// </summary>
    public enum IdentificacaoAmbiente
    {
        /// <summary>
        /// 0-Produção
        /// </summary>
        [XmlEnum("0")]
        Producao = 0,

        /// <summary>
        /// 1-Teste
        /// </summary>
        [XmlEnum("1")]
        Teste = 1
    }

    #endregion

    #region Tipo de permissão em relação a antecipação das parcelas referente ao pagamento de transportes - MDFe

    /// <summary>
    /// Tipo de permissão em relação a antecipação das parcelas referente a pagamento de transportes
    /// </summary>
    public enum TipoPermissaoAtencipacaoParcela
    {
        /// <summary>
        /// 0 - Não permite antecipar parcelas
        /// </summary>
        [XmlEnum("0")]
        NaoPermiteAnteciparParcelas = 0,

        /// <summary>
        /// 1 - Permite antecipar Parcelas
        /// </summary>
        [XmlEnum("1")]
        PermiteAnteciparParcelas = 1,

        /// <summary>
        /// 2 - Permite antecipar as parcelas mediante confirmação
        /// </summary>
        [XmlEnum("2")]
        PermiteAnteciparParcelasMedianteConfirmacao = 2
    }

    #endregion

    #region Tipos de códigos GTIN

    /// <summary>
    /// Tipos de códigos GTIN
    /// </summary>
    public enum TipoCodigoGTIN
    {
        /// <summary>
        /// 8 - GTIN8 (é codificado no código de barras EAN-8)
        /// </summary>
        [XmlEnum("8")]
        GTIN8 = 8,

        /// <summary>
        /// 12 - GTIN12 (é mais utilizado no código de barras UPC-A)
        /// </summary>
        [XmlEnum("12")]
        GTIN12 = 12,

        /// <summary>
        /// 13 - GTIN13 (é codificado no EAN-13)
        /// </summary>
        [XmlEnum("13")]
        GTIN13 = 13,

        /// <summary>
        /// 14 - GTIN14 (é codificado no ITF-14)
        /// </summary>
        [XmlEnum("14")]
        GTIN14 = 14
    }

    #endregion

    #region Tipos de GNRE

    /// <summary>
    /// Tipos de GNRE
    /// </summary>
    public enum TiposDeGNRE
    {
        /// <summary>
        /// 0 - GNRE Simples
        /// </summary>
        [XmlEnum("0")]
        Simples = 0,

        /// <summary>
        /// 1 - GNRE Múltiplos Documentos
        /// </summary>
        [XmlEnum("1")]
        MultiplosDocumentos = 1,

        /// <summary>
        /// 2 - GNRE Múltiplas Receitas
        /// </summary>
        [XmlEnum("2")]
        MultiplasReceitas = 2
    }

    #endregion

    #region Indicador de Importação

    /// <summary>
    /// Indicador de Importação
    /// </summary>
    public enum IndicadorImportacao
    {
        /// <summary>
        /// 0 - Nacional
        /// </summary>
        [XmlEnum("0")]
        Nacional = 0,

        /// <summary>
        /// 1 - Importado
        /// </summary>
        [XmlEnum("1")]
        Importado = 1
    }

    #endregion

    #region Motivos de redução do adrem do ICMS

    /// <summary>
    /// Motivos da redução do adrem do ICMS
    /// </summary>
    public enum MotivoReducaoAdRem
    {
        /// <summary>
        /// 1 - Transporte coletivo de passageiros
        /// </summary>
        [XmlEnum("1")]
        TransporteColetivoDePassageiros = 1,
        /// <summary>
        /// 9 = Outros
        /// </summary>
        [XmlEnum("9")]
        Outros = 9
    }

    #endregion

    #region Tipos dos XML retornados no DocZip

    /// <summary>
    /// Tipos de XML retornados na consulta distribuição de DFe (NFe/CTe) na tag docZip
    /// </summary>
    public enum TipoXMLDocZip
    {
        /// <summary>
        /// XML de resumo de eventos
        /// </summary>
        ResEvento = 1,

        /// <summary>
        /// XML de resumo da NFe
        /// </summary>
        ResNFe = 2,

        /// <summary>
        /// XML de distribuição de eventos da NFe (XML completo do evento)
        /// </summary>
        ProcEventoNFe = 3,

        /// <summary>
        /// XML de distribuição da NFe (XML completo da NFe)
        /// </summary>
        ProcNFe = 4,

        /// <summary>
        /// XML de distribuição de eventos da CTe (XML completo do evento)
        /// </summary>
        ProcEventoCTe = 5,

        /// <summary>
        /// XML de distribuição do CTe (XML completo do CTe)
        /// </summary>
        ProcCTe = 6,

        /// <summary>
        /// XML desconhecido
        /// </summary>
        Desconhecido = 0
    }

    #endregion

    #region Motivos de insucesso na entrega de mercadorias

    /// <summary>
    /// Motivos de insucesso na entrega da mercadoria
    /// </summary>
    public enum TipoMotivoInsucessoEntrega
    {
        /// <summary>
        /// 1 - Recebedor não foi encontrado 
        /// </summary>
        [XmlEnum("1")]
        RecebedorNaoEncontrado = 1,

        /// <summary>
        /// 2 - Recusa do recebedor
        /// </summary>
        [XmlEnum("2")]
        RecusaDoRecebedor = 2,

        /// <summary>
        /// 3 - Endereço inexistente
        /// </summary>
        [XmlEnum("3")]
        EnderecoInexistente = 3,

        /// <summary>
        /// 4 - Outros (exige informar justificativa)
        /// </summary>
        [XmlEnum("4")]
        Outros = 4
    }

    #endregion

    #region ProcessoEmissao

    /// <summary>
    /// Processo de emissão do evento do EFDReinf
    /// </summary>
    public enum ProcessoEmissaoReinf
    {
        /// <summary>
        /// 1 - Aplicativo do contribuinte
        /// </summary>
        [XmlEnum("1")]
        AplicativoContribuinte = 1,

        /// <summary>
        /// 2 - Aplicativo governamental
        /// </summary>
        [XmlEnum("2")]
        AplicativoGovernamental = 2,
    }

    #endregion

    #region Tipos De Inscrição

    /// <summary>
    /// Tipos de Inscrição
    /// </summary>
    public enum TiposInscricao
    {
        /// <summary>
        /// 1 - CNPJ
        /// </summary>
        [XmlEnum("1")]
        CNPJ = 1,

        /// <summary>
        /// 2 - CPF
        /// </summary>
        [XmlEnum("2")]
        CPF = 2,
    }

    #endregion

    #region Indicativo escrituração

    /// <summary>
    /// Indicativo da obrigatoriedade do contribuinte em fazer a sua escrituração contábil através da ECD Escrituração Contábil Digital
    /// </summary>
    public enum IndicativoEscrituracao
    {
        /// <summary>
        /// 0 - Empresa NÃO obrigada à ECD.
        /// </summary>
        [XmlEnum("0")]
        NaoObrigada = 0,

        /// <summary>
        /// 1 - Empresa obrigada à ECD.
        /// </summary>
        [XmlEnum("1")]
        Obrigada = 1,
    }

    #endregion

    #region Indicativo de desoneração

    /// <summary>
    /// Indicativo de desoneração da folha de pagamento
    /// </summary>
    public enum IndicativoDesoneracao
    {
        /// <summary>
        /// 0 - Não aplicável.
        /// </summary>
        [XmlEnum("0")]
        NaoAplicavel = 0,

        /// <summary>
        /// 1 - Empresa enquadrada nos artigos 7° a 9° da Lei 12.546/2011. Validação: Pode ser igual a [1] apenas se a classificação tributária for igual a [02, 03, 99]. Nos demais casos deve ser igual a [0].
        /// </summary>
        [XmlEnum("1")]
        Aplicavel = 1,
    }

    #endregion

    #region Indicativo acordo isenção de multa

    /// <summary>
    /// Indicativo da existência de acordo internacional para isenção de multa
    /// Validação: Só pode ser igual a [1] se {classTrib} for igual a [60].
    /// </summary>
    public enum IndicativoIsencaoMulta
    {
        /// <summary>
        /// 0 - Sem acordo.
        /// </summary>
        [XmlEnum("0")]
        SemAcordo = 0,

        /// <summary>
        /// 1 - Com acordo.
        /// </summary>
        [XmlEnum("1")]
        ComAcordo = 1,
    }

    #endregion

    #region Indicativo situação pessoa jurídica

    /// <summary>
    /// Indicativo da situação da pessoa jurídica
    /// Validação: Informação obrigatória e exclusiva para pessoa jurídica.
    /// </summary>
    public enum IndicativoSituacaoPJ
    {
        /// <summary>
        /// 0 - Situação normal.
        /// </summary>
        [XmlEnum("0")]
        Normal = 0,

        /// <summary>
        /// 1 - Extinção.
        /// </summary>
        [XmlEnum("1")]
        Extincao = 1,

        /// <summary>
        /// 2 - Fusão.
        /// </summary>
        [XmlEnum("2")]
        Fusao = 2,

        /// <summary>
        /// 3 - Cisão.
        /// </summary>
        [XmlEnum("3")]
        Cisao = 3,

        /// <summary>
        /// 4 - Incorporação.
        /// </summary>
        [XmlEnum("4")]
        Incorporacao = 4,
    }

    #endregion

    #region Indicativo de entidade vinculada a União

    /// <summary>
    /// Indicativo de entidade vinculada a União
    /// </summary>
    public enum IndicativoUniao
    {
        /// <summary>
        /// 0 - Não aplicável.
        /// </summary>
        [XmlEnum("0")]
        NaoAplicavel = 0,

        /// <summary>
        /// 1 - Órgão da Administração Pública Federal Direta, autarquias e fundações da Administração Pública Federal, empresas públicas, sociedades de economia mista, ou demais entidades em que a que União detenha maioria do capital social sujeito a voto, recebe recursos do Tesouro Nacional e está obrigada a registrar a execução orçamentária no Siafi. Validação: Informação obrigatória e exclusiva se a natureza jurídica do declarante for igual a: [101-5,104-0,107-4,110-4, 113-9, 116-3, 121-0, 122-8, 125-2, 126-0, 128-7, 131-7, 201-1, 203-8].
        /// </summary>
        [XmlEnum("1")]
        Aplicavel = 1,
    }

    #endregion

    #region Classificação tributária

    /// <summary>
    /// Classificação tributária
    /// </summary>
    public enum ClassificacaoTributaria
    {
        /// <summary>
        /// 01 - Empresa enquadrada no regime de tributação Simples Nacional com tributação previdenciária substituída.
        /// </summary>
        [XmlEnum("01")]
        SimplesNacionalTributacaoPrevidenciariaSubstituida = 1,

        /// <summary>
        /// 02 - Empresa enquadrada no regime de tributação Simples Nacional com tributação previdenciária não substituída
        /// </summary>
        [XmlEnum("02")]
        SimplesNacionalTributacaoPrevidenciariaNaoSubstituida = 2,

        /// <summary>
        /// 03 - Empresa enquadrada no regime de tributação Simples Nacional com tributação previdenciária substituída e não substituída
        /// </summary>
        [XmlEnum("03")]
        SimplesNacionalTributacaoPrevidenciariaSubstituidaNaoSubstituida = 3,

        /// <summary>
        /// 04 - MEI - Micro Empreendedor Individual.
        /// </summary>
        [XmlEnum("04")]
        MEI = 4,

        /// <summary>
        /// 06 - Agroindústria.
        /// </summary>
        [XmlEnum("06")]
        Agroindustria = 6,

        /// <summary>
        /// 07 - Produtor rural pessoa jurídica.
        /// </summary>
        [XmlEnum("07")]
        ProdutorRuralPessoaJuridica = 7,

        /// <summary>
        /// 08 - Consórcio simplificado de produtores rurais.
        /// </summary>
        [XmlEnum("08")]
        ConsorcioSimplificadoDeProdutoresRurais = 8,

        /// <summary>
        /// 09 - Órgão gestor de mão de obra – OGMO
        /// </summary>
        [XmlEnum("09")]
        OGMO = 9,

        /// <summary>
        /// 10 - Entidade sindical a que se refere a Lei 12.023/2009
        /// </summary>
        [XmlEnum("10")]
        EntidadeSindical = 10,

        /// <summary>
        /// 11 - Associação desportiva que mantém clube de futebol profissional
        /// </summary>
        [XmlEnum("11")]
        AssociacaoDesportiva = 11,

        /// <summary>
        /// 13 - Banco, caixa econômica, sociedade de crédito, financiamento e investimento e demais empresas relacionadas no parágrafo 1º do art. 22 da Lei 8.212./91
        /// </summary>
        [XmlEnum("13")]
        InstituicaoFinanceira = 13,

        /// <summary>
        /// 14 - Sindicatos em geral, exceto aquele classificado no código [10]
        /// </summary>
        [XmlEnum("14")]
        SindicatosGeral = 14,

        /// <summary>
        /// 21 - Pessoa física, exceto segurado especial
        /// </summary>
        [XmlEnum("21")]
        PessoaFisica = 21,

        /// <summary>
        /// 22 - Segurado especial
        /// </summary>
        [XmlEnum("22")]
        SeguradoEspecial = 22,

        /// <summary>
        /// 60 - Missão diplomática ou repartição consular de carreira estrangeira
        /// </summary>
        [XmlEnum("60")]
        MissaoDiplomatica = 60,

        /// <summary>
        /// 70 - Empresa de que trata o Decreto 5.436/2005
        /// </summary>
        [XmlEnum("70")]
        Decreto54362005 = 70,

        /// <summary>
        /// 80 - Entidade beneficente de assistência social isenta de contribuições sociais
        /// </summary>
        [XmlEnum("80")]
        EntidadeBeneficente = 80,

        /// <summary>
        /// 85 - Administração direta da União, Estados, Distrito Federal e Municípios; Autarquias e fundações públicas
        /// </summary>
        [XmlEnum("85")]
        AdministracaoDiretaUniao = 85,

        /// <summary>
        /// 99 - Pessoas jurídicas em geral
        /// </summary>
        [XmlEnum("99")]
        PessoaJuridica = 99,
    }
    #endregion

    #region Tipo da entidadade ligada

    /// <summary>
    /// Tipo da entidadade ligada
    /// Valores validos: 1, 2, 3, 4
    /// </summary>
    public enum TipoEntidadeLigada
    {
        /// <summary>
        /// 01 - Fundo de investimento.
        /// </summary>
        [XmlEnum("01")]
        FundoDeInvestimento = 1,

        /// <summary>
        /// 02 - Fundo de investimento imobiliário.
        /// </summary>
        [XmlEnum("02")]
        FundoDeInvestimentoImobiliario = 2,

        /// <summary>
        /// 03 - Clube de Investimento.
        /// </summary>
        [XmlEnum("03")]
        ClubeDeInvestimento = 3,

        /// <summary>
        /// 04 - Sociedade em conta de participação.
        /// </summary>
        [XmlEnum("04")]
        SociedadeEmContaDeParticipacao = 4,
    }
    #endregion

    #region Tipo do Processo

    /// <summary>
    /// Preencher com o código correspondente ao tipo de processo
    /// </summary>
    public enum TipoProcesso
    {
        /// <summary>
        /// 01 - Administrativo.
        /// </summary>
        [XmlEnum("1")]
        Administrativo = 1,

        /// <summary>
        /// 02 - Judicial.
        /// </summary>
        [XmlEnum("2")]
        Judicial = 2,
    }
    #endregion

    #region Indicativo da autoria da ação judicial

    /// <summary>
    /// Indicativo da autoria da ação judicial:
    /// </summary>
    public enum IndicativoAutoria
    {
        /// <summary>
        /// 01 - Próprio contribuinte.
        /// </summary>
        [XmlEnum("1")]
        ProprioContribuinte = 1,

        /// <summary>
        /// 02 - Terceiro (outra entidade, empresa ou pessoa física).
        /// </summary>
        [XmlEnum("2")]
        Terceiro = 2,
    }
    #endregion

    #region Indicativo de suspensão da exigibilidade

    /// <summary>
    /// Indicativo de suspensão da exigibilidade
    /// </summary>
    public enum IndicativoSuspensao
    {
        /// <summary>
        /// 01 - Liminar em mandado de segurança.
        /// </summary>
        [XmlEnum("01")]
        MandadoDeSeguranca = 1,

        /// <summary>
        /// 02 - Depósito judicial do montante integral.
        /// </summary>
        [XmlEnum("02")]
        DepositoJudicial = 2,

        /// <summary>
        /// 03 - Depósito administrativo do montante integral.
        /// </summary>
        [XmlEnum("03")]
        DepositoAdministrativo = 3,

        /// <summary>
        /// 04 - Antecipação de tutela.
        /// </summary>
        [XmlEnum("04")]
        AntecipacaoDeTutela = 4,

        /// <summary>
        /// 05 - Liminar em medida cautelar.
        /// </summary>
        [XmlEnum("05")]
        MedidaCautelar = 5,

        /// <summary>
        /// 08 - Sentença em mandado de segurança favorável ao contribuinte.
        /// </summary>
        [XmlEnum("08")]
        SentencaEmMandado = 8,

        /// <summary>
        /// 09 - Sentença em ação ordinária favorável ao contribuinte e confirmada pelo TRF.
        /// </summary>
        [XmlEnum("09")]
        SentencaEmAcao = 9,

        /// <summary>
        /// 10 - Acórdão do TRF favorável ao contribuinte.
        /// </summary>
        [XmlEnum("10")]
        AcordaoTRF = 10,

        /// <summary>
        /// 11 - Acórdão do STJ em recurso especial favorável ao contribuinte.
        /// </summary>
        [XmlEnum("11")]
        AcordaoSTJ = 11,

        /// <summary>
        /// 12 - Acórdão do STF em recurso extraordinário favorável ao contribuinte.
        /// </summary>
        [XmlEnum("12")]
        AcordaoSTF = 12,

        /// <summary>
        /// 13 - Sentença 1ª instância não transitada em julgado com efeito suspensivo.
        /// </summary>
        [XmlEnum("13")]
        SentencaNaoTransitada = 13,

        /// <summary>
        /// 90 - Decisão definitiva a favor do contribuinte.
        /// </summary>
        [XmlEnum("90")]
        DecisaoDefinitiva = 90,

        /// <summary>
        /// 92 - Sem suspensão da exigibilidade.
        /// </summary>
        [XmlEnum("92")]
        SemSuspensao = 92,
    }
    #endregion


    #region Indicativo Retificação

    /// <summary>
    /// Indicativo de retificação.
    /// Valores válidos: 1, 2.
    /// </summary>
    public enum IndicativoRetificacao
    {
        /// <summary>
        /// 1 - Arquivo original.
        /// </summary>
        [XmlEnum("1")]
        ArquivoOriginal = 1,

        /// <summary>
        /// 2 - Arquivo de retificação.
        /// </summary>
        [XmlEnum("2")]
        ArquivoRetificacao = 2
    }
    #endregion

    #region Tipo de Inscricao do Estabelecimento

    /// <summary>
    /// Preencher com o código correspondente ao tipo de inscrição do estabelecimento contratante dos serviços:
    /// 1 - CNPJ;
    /// 4 - CNO - Cadastro Nacional de Obras
    /// </summary>
    public enum TipoInscricaoEstabelecimento
    {
        /// <summary>
        /// 1 - CNPJ.
        /// </summary>
        [XmlEnum("1")]
        CNPJ = 1,

        /// <summary>
        /// 3 - CAEPF.
        /// </summary>
        [XmlEnum("3")]
        CAEPF = 3,

        /// <summary>
        /// 4 - Cadastro Nacional de Obras.
        /// </summary>
        [XmlEnum("4")]
        CNO = 4
    }

    #region Indicativo Obra

    /// <summary>
    /// Indicativo de prestação de serviços em obra de construção civil
    /// </summary>
    public enum IndicativoObra
    {
        /// <summary>
        /// 0 - Não é obra de construção civil ou não está sujeita a matrícula de obra.
        /// </summary>
        [XmlEnum("0")]
        NaoSujeitaMatriculaDeObra = 0,

        /// <summary>
        /// 1 - É obra de construção civil, modalidade empreitada total.
        /// </summary>
        [XmlEnum("1")]
        EmpreitadaTotal = 1,

        /// <summary>
        /// 2 - É obra de construção civil, modalidade empreitada parcial.
        /// </summary>
        [XmlEnum("2")]
        EmpreitadaParcial = 2
    }
    #endregion

    #region IndicativoCPRB
    /// <summary>
    /// Indicativo se o prestador é contribuinte da contribuição previdenciária sobre a receita bruta(CPRB), a qual reduz a alíquota de 11% para 3,5% na retenção de contribuição previdenciária
    /// </summary>
    public enum IndicativoCPRB
    {
        /// <summary>
        /// 0 - Não é contribuinte da CPRB - retenção de 11%.
        /// </summary>
        [XmlEnum("0")]
        NaoContribuinte = 0,

        /// <summary>
        /// 1 - É contribuinte da CPRB - retenção de 3,5%.
        /// </summary>
        [XmlEnum("1")]
        Contribuinte = 1,

    }
    #endregion

    #region Tipo Repasse
    /// <summary>
    /// Tipo de repasse, conforme tabela:
    /// 1-Patrocinio, 2-Licenciamento de marcas e símbolos, 3 - Publicidade, 4 - Propaganda, 5 - Transmissão de espetáculos. 
    /// </summary>
    public enum TipoRepasse
    {
        /// <summary>
        /// 1 - Patrocinio.
        /// </summary>
        [XmlEnum("1")]
        Patrocinio = 1,

        /// <summary>
        /// 2 - Licenciamento de marcas e símbolos.
        /// </summary>
        [XmlEnum("2")]
        LicenciamentoMarcas = 2,

        /// <summary>
        /// 3 - Publicidade.
        /// </summary>
        [XmlEnum("3")]
        Publicidade = 3,

        /// <summary>
        /// 4 - Propaganda.
        /// </summary>
        [XmlEnum("4")]
        Propaganda = 4,

        /// <summary>
        /// 5 - Transmissão de espetáculos.
        /// </summary>
        [XmlEnum("5")]
        TransmissaoDeEspetaculos = 5,

    }
    #endregion

    #region Indicativo de Comercialização
    /// <summary>
    /// Indicativo de comercialização.
    /// </summary>
    public enum IndicativoComercializacao
    {
        /// <summary>
        /// 1 - Comercialização da produção por produtor rural PJ/agroindústria, exceto para entidade executora do Programa de Aquisição de Alimentos - PAA
        /// </summary>
        [XmlEnum("1")]
        ProdutorRuralPJ = 1,

        /// <summary>
        /// 7 - Comercialização da produção com isenção de contribuição previdenciária, de acordo com a Lei n° 13.606/2018
        /// </summary>
        [XmlEnum("7")]
        IsencaoDeContribuicao = 7,

        /// <summary>
        /// 8 - Comercialização da produção para entidade executora do PAA
        /// </summary>
        [XmlEnum("8")]
        ParaEntidadePAA = 8,

        /// <summary>
        /// 9 - Comercialização da produção para entidade executora do PAA
        /// </summary>
        [XmlEnum("9")]
        MercadoExterno = 9,

    }
    #endregion

    #region Tipo de Inscrição do Estabelecimento Adquirente da Produção
    /// <summary>
    /// Tipo de inscrição do estabelecimento adquirente da produção.
    /// </summary>
    public enum TipoInscricaoAdquirente
    {
        /// <summary>
        /// 1 - CNPJ.
        /// </summary>
        [XmlEnum("1")]
        CNPJ = 1,

        /// <summary>
        /// 3 - CAEPF.
        /// </summary>
        [XmlEnum("3")]
        CAEPF = 3,
    }
    #endregion

    #region Indicativo da aquisição
    /// <summary>
    /// Indicativo da aquisição.
    /// </summary>
    public enum IndicativoDaAquisicao
    {
        /// <summary>
        /// 1 - Aquisição de produção de produtor rural pessoa física ou segurado especial em geral;
        /// </summary>
        [XmlEnum("1")]
        AquisicaoGeralPfSe = 1,

        /// <summary>
        /// 2 - Aquisição de produção de produtor rural pessoa física ou segurado especial em geral por entidade executora do Programa de Aquisição de Alimentos - PAA;
        /// </summary>
        [XmlEnum("2")]
        AquisicaoPfSePaa = 2,

        /// <summary>
        /// 3 - Aquisição de produção de produtor rural pessoa jurídica por entidade executora do PAA;
        /// </summary>
        [XmlEnum("3")]
        AquisicaoPjPaa = 3,

        /// <summary>
        /// 4 - Aquisição de produção de produtor rural pessoa física ou segurado especial em geral - Produção isenta (Lei 13.606/2018);
        /// </summary>
        [XmlEnum("4")]
        AquisicaoIsentaPfSe = 4,

        /// <summary>
        /// 5 - Aquisição de produção de produtor rural pessoa física ou segurado especial em geral por entidade executora do PAA - Produção isenta (Lei 13.606/2018);
        /// </summary>
        [XmlEnum("5")]
        AquisicaoIsentaPfSePaa = 5,

        /// <summary>
        /// 6 -  Aquisição de produção de produtor rural pessoa jurídica por entidade executora do PAA - Produção isenta (Lei 13.606/2018);
        /// </summary>
        [XmlEnum("6")]
        AquisicaoIsentaPjPaa = 6,

        /// <summary>
        /// 7 -  Aquisição de produção de produtor rural pessoa física ou segurado especial para fins de exportação.
        /// </summary>
        [XmlEnum("7")]
        AquisicaoExportacaoPfSe = 7,
    }
    #endregion

    #region Tipo Ajuste
    /// <summary>
    /// Preencher com o código correspondente ao tipo de ajuste.
    /// </summary>
    public enum TipoAjusteReinf
    {
        /// <summary>
        /// 0 - Ajuste de Redução.
        /// </summary>
        [XmlEnum("0")]
        AjusteDeReducao = 0,

        /// <summary>
        /// 1 - Ajuste de Acréscimo.
        /// </summary>
        [XmlEnum("1")]
        AjusteDeAcrescimo = 1,
    }
    #endregion

    #region Codigo Ajuste
    /// <summary>
    /// Preencher com o código de ajuste.
    /// </summary>
    public enum CodigoAjuste
    {
        /// <summary>
        /// 1 - Ajuste da CPRB: Adoção do regime de caixa.
        /// </summary>
        [XmlEnum("1")]
        AjusteAdocaoCaixa = 1,

        /// <summary>
        /// 2 - Ajuste da CPRB: Diferimento de valores a recolher no período.
        /// </summary>
        [XmlEnum("2")]
        AjusteDiferimento = 2,

        /// <summary>
        /// 3 - Adição de valores diferidos em período(s) anteriores(es).
        /// </summary>
        [XmlEnum("3")]
        AdicaoValoresDiferidos = 3,

        /// <summary>
        /// 4 - Exportações diretas.
        /// </summary>
        [XmlEnum("4")]
        ExportacaoDireta = 4,

        /// <summary>
        /// 5 - Transporte internacional de cargas.
        /// </summary>
        [XmlEnum("5")]
        TransporteInternacional = 5,

        /// <summary>
        /// 6 - Vendas canceladas e os descontos incondicionais concedidos.
        /// </summary>
        [XmlEnum("6")]
        VendaCanceladaDesconto = 6,

        /// <summary>
        /// 7 - IPI, se incluído na receita bruta.
        /// </summary>
        [XmlEnum("7")]
        IpiIncluidoReceitaBruta = 7,

        /// <summary>
        /// 8 - ICMS, quando cobrado pelo vendedor dos bens ou prestador dos serviços na condição de substituto tributário.
        /// </summary>
        [XmlEnum("8")]
        IcmsCobrancaSubstituto = 8,

        /// <summary>
        /// 9 - Receita bruta reconhecida pela construção, recuperação, reforma, ampliação ou melhoramento da infraestrutura, cuja contrapartida seja ativo intangível representativo de direito de exploração, no caso de contratos de concessão de serviços públicos.
        /// </summary>
        [XmlEnum("9")]
        ReceitaBrutaInfraestrutura = 9,

        /// <summary>
        /// 10 - O valor do aporte de recursos realizado nos termos do art 6 §3 inciso III da Lei 11.079/2004.
        /// </summary>
        [XmlEnum("10")]
        AporteRecursos = 10,

        /// <summary>
        /// 11 - Demais ajustes oriundos da legislação tributária, estorno ou outras situações.
        /// </summary>
        [XmlEnum("11")]
        DemaisAjustes = 11,
    }
    #endregion

    #region Tipo Competicao. Reinf
    /// <summary>
    /// Tipo de competição.
    /// </summary>
    public enum TipoCompeticao
    {
        /// <summary>
        /// 1 - Oficial.
        /// </summary>
        [XmlEnum("1")]
        Oficial = 1,

        /// <summary>
        /// 2 - Não oficial.
        /// </summary>
        [XmlEnum("2")]
        NaoOficial = 2,
    }
    #endregion

    #region Categoria do evento esportivo. Reinf
    /// <summary>
    /// Categoria do evento esportivo.
    /// </summary>
    public enum CategoriaEventoReinf
    {
        /// <summary>
        /// 1 - Internacional.
        /// </summary>
        [XmlEnum("1")]
        Internacional = 1,

        /// <summary>
        /// 2 - Interestadual.
        /// </summary>
        [XmlEnum("2")]
        Interestadual = 2,

        /// <summary>
        /// 3 - Estadual.
        /// </summary>
        [XmlEnum("3")]
        Estadual = 3,

        /// <summary>
        /// 4 - Local.
        /// </summary>
        [XmlEnum("4")]
        Local = 4,
    }
    #endregion

    #region Tipo de Ingresso.
    /// <summary>
    /// Tipo de Ingresso. 
    /// </summary>
    public enum TipoDeIngresso
    {
        /// <summary>
        /// 1 - Arquibancada.
        /// </summary>
        [XmlEnum("1")]
        Arquibancada = 1,

        /// <summary>
        /// 2 - Geral.
        /// </summary>
        [XmlEnum("2")]
        Geral = 2,

        /// <summary>
        /// 3 - Cadeiras.
        /// </summary>
        [XmlEnum("3")]
        Cadeiras = 3,

        /// <summary>
        /// 4 - Camarote.
        /// </summary>
        [XmlEnum("4")]
        Camarote = 4,
    }
    #endregion

    #region Tipo de Receita.
    /// <summary>
    /// Tipo de Receita. 
    /// </summary>
    public enum TipoReceita
    {
        /// <summary>
        /// 1 - Transmissão.
        /// </summary>
        [XmlEnum("1")]
        Transmissao = 1,

        /// <summary>
        /// 2 - Propaganda.
        /// </summary>
        [XmlEnum("2")]
        Propaganda = 2,

        /// <summary>
        /// 3 - Publicidade.
        /// </summary>
        [XmlEnum("3")]
        Publicidade = 3,

        /// <summary>
        /// 4 - Sorteio.
        /// </summary>
        [XmlEnum("4")]
        Sorteio = 4,

        /// <summary>
        /// 5 - Outros.
        /// </summary>
        [XmlEnum("5")]
        Outros = 5,
    }
    #endregion

    #region Relacao De Dependencia

    /// <summary>
    /// Relação de dependência;
    /// </summary>
    public enum RelacaoDeDependencia
    {
        /// <summary>
        /// 1 - Conjuge
        /// </summary>
        [XmlEnum("1")]
        Conjuge = 1,

        /// <summary>
        /// 2 - Companheiro(a) com o(a) qual tenha filho ou viva há mais de 5 (cinco) anos ou possua declaração de união estável
        /// </summary>
        [XmlEnum("2")]
        UniaoEstavel = 2,

        /// <summary>
        /// 3 - Filho(a) ou enteado(a)
        /// </summary>
        [XmlEnum("3")]
        FilhoOuEnteado = 3,

        /// <summary>
        /// 6 - Irmão(ã), neto(a) ou bisneto(a) sem arrimo dos pais, do(a) qual detenha a guarda judicial
        /// </summary>
        [XmlEnum("6")]
        IrmaoNetoBisneto = 6,

        /// <summary>
        /// 9 - Pais, avós e bisavós;
        /// </summary>
        [XmlEnum("9")]
        PaisAvosBisavos = 9,

        /// <summary>
        /// 10 - Menor pobre do qual detenha a guarda judicial;
        /// </summary>
        [XmlEnum("10")]
        MenorPobreJudicial = 10,

        /// <summary>
        /// 11 - A pessoa absolutamente incapaz, da qual seja tutor ou curador;
        /// </summary>
        [XmlEnum("11")]
        PessoaAbsolutamenteIncapaz = 11,

        /// <summary>
        /// 12 - Ex-cônjuge;
        /// </summary>
        [XmlEnum("12")]
        ExConjuge = 12,

        /// <summary>
        /// 99 - Agregado / Outros;
        /// </summary>
        [XmlEnum("99")]
        AgregadoOutros = 99,
    }

    #endregion

    #region Tipo Competicao. Reinf
    /// <summary>
    /// Tipo de competição.
    /// </summary>
    public enum IndicativoFundoDeInvestimento
    {
        /// <summary>
        /// 1 - Fundo ou clube de investimento.
        /// </summary>
        [XmlEnum("1")]
        FCI = 1,

        /// <summary>
        /// 2 - Sociedade em conta de participação.
        /// </summary>
        [XmlEnum("2")]
        SCP = 2,
    }
    #endregion

    #region Indicativo do tipo de dedução
    /// <summary>
    /// Indicativo do tipo de dedução.
    /// </summary>
    public enum IndicativoTipoDeducao
    {
        /// <summary>
        /// 1 - Previdência oficial.
        /// </summary>
        [XmlEnum("1")]
        PrevidenciaOficial = 1,

        /// <summary>
        /// 2 - Previdência privada.
        /// </summary>
        [XmlEnum("2")]
        PrevidenciaPrivada = 2,

        /// <summary>
        /// 3 - Fundo de aposentadoria programada individual - Fapi.
        /// </summary>
        [XmlEnum("3")]
        FundoAposentadoriaIndividual = 3,

        /// <summary>
        /// 4 - Fundação de previdência complementar do servidor público - Funpresp.
        /// </summary>
        [XmlEnum("4")]
        FundacaoPrevidenciaComplementar = 4,

        /// <summary>
        /// 5 - Pensão alimentícia.
        /// </summary>
        [XmlEnum("5")]
        PensaoAlimenticia = 5,

        /// <summary>
        /// 7 - Dependentes.
        /// </summary>
        [XmlEnum("7")]
        Dependentes = 7,
    }
    #endregion

    #region Tipo de Isenção
    /// <summary>
    /// Tipo de Isenção.
    /// </summary>
    public enum TipoIsencao
    {
        /// <summary>
        /// 1 - Parcela isenta 65 anos.
        /// </summary>
        [XmlEnum("1")]
        ParcelaIsenta = 1,

        /// <summary>
        /// 2 - Diária de viagem.
        /// </summary>
        [XmlEnum("2")]
        DiariaViagem = 2,

        /// <summary>
        /// 3 - Indenização e rescisão de contrato, inclusive a título de PDV e acidentes de trabalho
        /// </summary>
        [XmlEnum("3")]
        IndenizacaoRescisao = 3,

        /// <summary>
        /// 4 - Abono pecuniário
        /// </summary>
        [XmlEnum("4")]
        AbonoPecuniario = 4,

        /// <summary>
        /// 5 - Valores pagos a titular ou sócio de microempresa ou empresa de pequeno porte, exceto pró-labore, alugueis e serviços prestados
        /// </summary>
        [XmlEnum("5")]
        TitularOuSocioMicroempresa = 5,

        /// <summary>
        /// 7 - Complementação de aposentadoria, correspondente às contribuições efetuadas no período de 01/01/1989 a 31/12/1995.
        /// </summary>
        [XmlEnum("7")]
        ComplementacaoAposentadoria = 7,

        /// <summary>
        /// 8 - Ajuda de custo
        /// </summary>
        [XmlEnum("8")]
        AjudaDeCusto = 8,

        /// <summary>
        /// 9 - Rendimentos pagos sem retenção do IR na fonte - Lei 10.833/2003;
        /// </summary>
        [XmlEnum("9")]
        RendimentoSemRetencao = 9,

        /// <summary>
        /// 99 - Outros (especificar).
        /// </summary>
        [XmlEnum("99")]
        Outros = 99,
    }
    #endregion

    #region Indicativo da origem dos recursos
    /// <summary>
    /// Indicativo da origem dos recursos.
    /// </summary>
    public enum IndicativoOrigemRecursos
    {
        /// <summary>
        /// 1 - Recursos do próprio declarante.
        /// </summary>
        [XmlEnum("1")]
        ProprioDeclarante = 1,

        /// <summary>
        /// 2 - Recursos de terceiros - Declarante é a instituição financeira responsável pelo repasse dos valores.
        /// </summary>
        [XmlEnum("2")]
        Terceiros = 2,
    }
    #endregion

    #region Indicativo do Número de Identificação Fiscal - NIF
    /// <summary>
    /// Indicativo do Número de Identificação Fiscal - NIF.
    /// </summary>
    public enum IndicativoNIF
    {
        /// <summary>
        /// 1 - Beneficiário com NIF
        /// </summary>
        [XmlEnum("1")]
        BeneficiarioComNIF = 1,

        /// <summary>
        /// 2 - Beneficiário dispensado do NIF
        /// </summary>
        [XmlEnum("2")]
        DispensadoDoNIF = 2,

        /// <summary>
        /// 3 - País não exige NIF
        /// </summary>
        [XmlEnum("3")]
        PaisNaoExigeNIF = 3,
    }
    #endregion

    #region Informações sobre isenção e imunidade
    /// <summary>
    /// Informações sobre isenção e imunidade.
    /// </summary>
    public enum IsencaoEImunidade
    {
        /// <summary>
        /// 2 - Instituição de educação e de assistência social sem fins lucrativos, a que se refere o art. 12 da Lei nº 9.532, de 10 de dezembro de 1997;
        /// </summary>
        [XmlEnum("2")]
        InstituicaoDeEducacao = 2,

        /// <summary>
        /// 3 - Instituição de caráter filantrópico, recreativo, cultural, científico e às associações civis, a que se refere o art. 15 da Lei nº 9.532, de 1997. 
        /// Informação a ser prestada exclusivamente por órgãos da Administração Pública Federal Direta, autarquias e fundações da Administração Pública Federal, empresas públicas, sociedades de economia mista, ou demais entidades em que a que União detenha maioria do capital social sujeito a voto, recebe recursos do Tesouro Nacional e está obrigada a registrar a execução orçamentária no Siafi
        /// </summary>
        [XmlEnum("3")]
        InstituicaoFilantropica = 3,
    }
    #endregion

    #region Relação da fonte pagadora com o beneficiário
    /// <summary>
    /// Relação da fonte pagadora com o beneficiário.
    /// </summary>
    public enum RelacaoFontePagadora
    {
        /// <summary>
        /// 500 - A fonte pagadora é matriz da beneficiária no exterior.
        /// </summary>
        [XmlEnum("500")]
        MatrizNoExterior = 500,

        /// <summary>
        /// 510 - A fonte pagadora é filial, sucursal ou agência de beneficiária no exterior.
        /// </summary>
        [XmlEnum("510")]
        FilialNoExterior = 510,

        /// <summary>
        /// 520 - A fonte pagadora é controlada ou coligada da beneficiária no exterior, na forma dos §§ 1º e 2º do art. 243 da Lei nº 6.404, de 15 de dezembro de 19
        /// </summary>
        [XmlEnum("520")]
        ControladaOuColigada = 520,

        /// <summary>
        /// 530 - A fonte pagadora é controladora ou coligada da beneficiária no exterior, na forma dos §§ 1º e 2º do art. 243 da Lei nº 6.404, de 197
        /// </summary>
        [XmlEnum("530")]
        ControladoraOuColigada = 530,

        /// <summary>
        /// 540 - A fonte pagadora e a beneficiária no exterior estão sob controle societário ou administrativo comum ou quando pelo menos 10% do capital de cada uma, pertencer a uma mesma pessoa física ou jurídica
        /// </summary>
        [XmlEnum("540")]
        ControleSocietario = 540,

        /// <summary>
        /// 550 - A fonte pagadora e a beneficiária no exterior têm participação societária no capital de uma terceira pessoa jurídica, cuja soma as caracterize como controladoras ou coligadas na forma dos §§ 1º e 2º do art. 243 da Lei nº 6.404, de 1976.
        /// </summary>
        [XmlEnum("550")]
        ParticipacaoSocietaria = 550,

        /// <summary>
        /// 560 - A fonte pagadora ou a beneficiária no exterior mantenha contrato de exclusividade como agente, como distribuidor ou como concessionário nas operações com bens, serviços e direitos.
        /// </summary>
        [XmlEnum("560")]
        ContratoDeExclusividade = 560,

        /// <summary>
        /// 570 - A fonte pagadora e a beneficiária mantêm acordo de atuação conjunta
        /// </summary>
        [XmlEnum("570")]
        AcordoAtuacaoConjunta = 570,

        /// <summary>
        /// 900 - Não há relação entre a fonte pagadora e a beneficiária no exterior
        /// </summary>
        [XmlEnum("900")]
        SemRelacao = 900,
    }
    #endregion

    #region Forma de tributação sobre rendimentos de beneficiários domiciliados no exterior
    /// <summary>
    /// Forma de tributação sobre rendimentos de beneficiários domiciliados no exterior
    /// </summary>
    public enum FormaDeTributacao
    {
        /// <summary>
        /// 10 - Retenção do IRRF – alíquota padrã
        /// </summary>
        [XmlEnum("10")]
        AliquotaPadrao = 10,

        /// <summary>
        /// 11 - Retenção do IRRF – alíquota da tabela progressiva
        /// </summary>
        [XmlEnum("11")]
        AliquotaProgressiva = 11,

        /// <summary>
        /// 12 - Retenção do IRRF – alíquota diferenciada (países com tributação favorecida)
        /// </summary>
        [XmlEnum("12")]
        AliquotaDiferenciada = 12,

        /// <summary>
        /// 13 - Retenção do IRRF – alíquota limitada conforme cláusula em convênio
        /// </summary>
        [XmlEnum("13")]
        AliquotaLimitada = 13,

        /// <summary>
        /// 30 - Retenção do IRRF – outras hipóteses
        /// </summary>
        [XmlEnum("30")]
        RetencaoOutrasHipoteses = 30,

        /// <summary>
        /// 40 - Não retenção do IRRF – isenção estabelecida em convênio
        /// </summary>
        [XmlEnum("40")]
        IsencaoConvenio = 40,

        /// <summary>
        /// 41 - Não retenção do IRRF – isenção prevista em lei interna
        /// </summary>
        [XmlEnum("41")]
        IsencaoLeiInterna = 41,

        /// <summary>
        /// 42 - Não retenção do IRRF – alíquota zero prevista em lei interna
        /// </summary>
        [XmlEnum("42")]
        AliquotaZeroLeiInterna = 42,

        /// <summary>
        /// 43 - Não retenção do IRRF – pagamento antecipado do imposto
        /// </summary>
        [XmlEnum("43")]
        PagamentoAntecipado = 43,

        /// <summary>
        /// 44 - Não retenção do IRRF – medida judicial
        /// </summary>
        [XmlEnum("44")]
        MedidaJudicial = 44,

        /// <summary>
        /// 50 - Não retenção do IRRF – outras hipóteses
        /// </summary>
        [XmlEnum("50")]
        NaoRetencaoOutrasHipoteses = 50,
    }
    #endregion

    #region FechamentoRetencao

    /// <summary>
    /// Indicativo de fechamento ou reabertura de movimento relativo aos eventos de retenções na fonte(IR, CSLL, Pis/Pasep e Cofins)
    /// </summary>
    public enum FechamentoRetencao
    {
        /// <summary>
        /// 0 - Fechamento (fecha o movimento, caso esteja aberto)
        /// </summary>
        [XmlEnum("0")]
        Fechamento = 0,

        /// <summary>
        /// 1 - Reabertura (reabre o movimento, caso esteja fechado).
        /// </summary>
        [XmlEnum("1")]
        Reabertura = 1,
    }
    #endregion

    #region Eventos da EFD-Reinf

    /// <summary>
    /// Eventos da EFD-Reinf
    /// </summary>
    public enum EventosEFDReinf
    {
        /// <summary>
        /// R-1000 - Informações do contribuinte
        /// </summary>
        [XmlEnum("R-1000")]
        R1000 = 1000,

        /// <summary>
        /// R-1050 - Tabela de entidades ligadas
        /// </summary>
        [XmlEnum("R-1050")]
        R1050 = 1050,

        /// <summary>
        /// R-1070 - Tabela de processos administrativos/judiciais
        /// </summary>
        [XmlEnum("R-1070")]
        R1070 = 1070,

        /// <summary>
        /// R-2010 - Retenção de contribuição previdenciária - serviços tomados
        /// </summary>
        [XmlEnum("R-2010")]
        R2010 = 2010,

        /// <summary>
        /// R-2020 - Retenção de contribuição previdenciária - serviços prestados
        /// </summary>
        [XmlEnum("R-2020")]
        R2020 = 2020,

        /// <summary>
        /// R-2030 - Recursos recebidos por associação desportiva
        /// </summary>
        [XmlEnum("R-2030")]
        R2030 = 2030,

        /// <summary>
        /// R-2040 - Recursos repassados para associação desportiva
        /// </summary>
        [XmlEnum("R-2040")]
        R2040 = 2040,

        /// <summary>
        /// R-2050 - Comercialização de produção
        /// </summary>
        [XmlEnum("R-2050")]
        R2050 = 2050,

        /// <summary>
        /// R-2055 - Aquisição de produção rural
        /// </summary>
        [XmlEnum("R-2055")]
        R2055 = 2055,

        /// <summary>
        /// R-2060 - Contribuição previdenciária sobre a receita bruta - CPRB
        /// </summary>
        [XmlEnum("R-2060")]
        R2060 = 2060,

        /// <summary>
        /// R-2098 - Reabertura dos eventos da série R-2000
        /// </summary>
        [XmlEnum("R-2098")]
        R2098 = 2098,

        /// <summary>
        /// R-2099 - Fechamento dos eventos da série R-2000
        /// </summary>
        [XmlEnum("R-2099")]
        R2099 = 2099,

        /// <summary>
        /// R-3010 - Receita de espetáculos desportivos
        /// </summary>
        [XmlEnum("R-3010")]
        R3010 = 3010,

        /// <summary>
        /// R-4010 - Pagamentos/créditos a beneficiário pessoa física
        /// </summary>
        [XmlEnum("R-4010")]
        R4010 = 4010,

        /// <summary>
        /// R-4020 - Pagamentos/créditos a beneficiário pessoa jurídica
        /// </summary>
        [XmlEnum("R-4020")]
        R4020 = 4020,

        /// <summary>
        /// R-4040 - Pagamentos/créditos a beneficiários não identificados
        /// </summary>
        [XmlEnum("R-4040")]
        R4040 = 4040,

        /// <summary>
        /// R-4080 - Retenção no recebimento
        /// </summary>
        [XmlEnum("R-4080")]
        R4080 = 4080,

        /// <summary>
        /// R-4099 - Fechamento/reabertura dos eventos da série R-4000
        /// </summary>
        [XmlEnum("R-4099")]
        R4099 = 4099,

        /// <summary>
        /// R-9000 - Exclusão de eventos
        /// </summary>
        [XmlEnum("R-9000")]
        R9000 = 9000,

        /// <summary>
        /// R-9001 - Bases e tributos - contribuição previdenciária
        /// </summary>
        [XmlEnum("R-9001")]
        R9001 = 9001,

        /// <summary>
        /// R-9005 - Bases e tributos - retenções na fonte
        /// </summary>
        [XmlEnum("R-9005")]
        R9005 = 9005,

        /// <summary>
        /// R-9011 - Consolidação de bases e tributos - contribuição previdenciária
        /// </summary>
        [XmlEnum("R-9011")]
        R9011 = 9011,

        /// <summary>
        /// R-9015 - Consolidação das retenções na fonte
        /// </summary>
        [XmlEnum("R-9015")]
        R9015 = 9015,
    }
    #endregion

    #region Código do Retorno

    /// <summary>
    /// Código do Retorno
    /// </summary>
    public enum CodigoDoRetorno
    {
        /// <summary>
        /// 0 - Sucesso
        /// </summary>
        [XmlEnum("0")]
        Sucesso = 0,

        /// <summary>
        /// 1 - Erro
        /// </summary>
        [XmlEnum("1")]
        Erro = 1,

        /// <summary>
        /// 2 - Em Processamento
        /// </summary>
        [XmlEnum("2")]
        EmProcessamento = 2,
    }
    #endregion

    #region Tipo da ocorrência

    /// <summary>
    /// Tipo da ocorrência
    /// </summary>
    public enum TipoDaOcorrencia
    {
        /// <summary>
        /// 1 - Erro
        /// </summary>
        [XmlEnum("1")]
        Erro = 1,

        /// <summary>
        /// 2 - Aviso
        /// </summary>
        [XmlEnum("2")]
        Aviso = 2,

        /// <summary>
        /// 3 - Informação
        /// </summary>
        [XmlEnum("3")]
        Informacao = 3,
    }
    #endregion

    #region Indicativo de existência de valores de bases ou de tributos.

    /// <summary>
    /// Indicativo de existência de valores de bases ou de tributos.
    /// </summary>
    public enum IndicativoExistenciaTributos
    {
        /// <summary>
        /// 1 - Há informações de bases e/ou de tributos.
        /// </summary>
        [XmlEnum("1")]
        Tributo = 1,

        /// <summary>
        /// 2 - Há movimento, porém não há informações de bases ou de tributos.
        /// </summary>
        [XmlEnum("2")]
        SemInformacoes = 2,

        /// <summary>
        /// 3 - Não há movimento na competência.
        /// </summary>
        [XmlEnum("3")]
        SemMovimento = 3,
    }
    #endregion
}
#endregion