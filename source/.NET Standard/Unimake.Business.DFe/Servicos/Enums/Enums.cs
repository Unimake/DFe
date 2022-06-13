using System.ComponentModel;
using System.Xml.Serialization;

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
        /// Consulta status serviço NFe/NFCe
        /// </summary>
        [Description("Consulta status do serviço da NFe/NFCe")]
        NFeStatusServico = 0,

        /// <summary>
        /// Consulta protocolo da NFe/NFCe
        /// </summary>
        [Description("Consulta situação da NFe/NFCe")]
        NFeConsultaProtocolo = 1,

        /// <summary>
        /// Consulta recibo NFe/NFCe
        /// </summary>
        [Description("Consulta recibo da NFe/NFCe")]
        NFeConsultaRecibo = 2,

        /// <summary>
        /// Inutilização de números da nota fiscal eletrônica
        /// </summary>
        [Description("Inutilização de números da NFe/NFCE")]
        NFeInutilizacao = 3,

        /// <summary>
        /// Consulta cadastro do contribuinte
        /// </summary>
        [Description("Consulta cadastro de contribuinte")]
        NFeConsultaCadastro = 4,

        /// <summary>
        /// Envio de Eventos (Cancelamento, CCe, EPEC, etc...)
        /// </summary>
        [Description("Envio de eventos da NFe/NFCe")]
        NFeRecepcaoEvento = 5,

        /// <summary>
        /// Envio do XML de lote de NFe/NFCe
        /// </summary>
        [Description("Autorização da NFe/NFCe")]
        NFeAutorizacao = 6,

        /// <summary>
        /// Envio do XML de consulta dos documentos fiscais eletrônicos destinados - NFe
        /// </summary>
        [Description("Distribuição de documentos fiscais eletrônicos da NFe")]
        NFeDistribuicaoDFe = 7,

        /// <summary>
        /// Consulta status serviço CTe
        /// </summary>
        [Description("Consulta status do serviço do CTe")]
        CTeStatusServico = 8,

        /// <summary>
        /// Consulta protocolo do CTe
        /// </summary>
        [Description("Consulta situação do CTe")]
        CTeConsultaProtocolo = 9,

        /// <summary>
        /// Inutilização de números do Conhecimetno de Transporte Eletrônico (CTe)
        /// </summary>
        [Description("Inutilização de números do CTe")]
        CTeInutilizacao = 10,

        /// <summary>
        /// Envio do XML de consulta dos documentos fiscais eletrônicos destinados - CTe
        /// </summary>
        [Description("Distribuição de documentos fiscais eletrônicos do CTe")]
        CTeDistribuicaoDFe = 11,

        /// <summary>
        /// Consulta recibo CTe
        /// </summary>
        [Description("Consulta recibo do CTe")]
        CTeConsultaRecibo = 12,

        /// <summary>
        /// Envio do XML de CTe
        /// </summary>
        [Description("Autorização do CTe")]
        CTeAutorizacao = 13,

        /// <summary>
        /// Envio do XML de CTeOS
        /// </summary>
        [Description("Autorização do CTeOS")]
        CTeAutorizacaoOS = 14,

        /// <summary>
        /// Consulta status serviço MDFe
        /// </summary>
        [Description("Consulta status do serviço do MDFe")]
        MDFeStatusServico = 15,

        /// <summary>
        /// Consulta protocolo do MDFe
        /// </summary>
        [Description("Consulta situação do MDFe")]
        MDFeConsultaProtocolo = 16,

        /// <summary>
        /// Consulta recibo MDFe
        /// </summary>
        [Description("Consulta recibo do MDFe")]
        MDFeConsultaRecibo = 17,

        /// <summary>
        /// Consulta MDFe não encerrado
        /// </summary>
        [Description("Consulta de MDFe´s não encerrados")]
        MDFeConsultaNaoEnc = 18,

        /// <summary>
        /// Envio do XML de MDFe em lote no modo assíncrono
        /// </summary>
        [Description("Autorização do MDFe")]
        MDFeAutorizacao = 19,

        /// <summary>
        /// Envio do XML de MDFe no modo síncrono
        /// </summary>
        [Description("Autorização do MDFe")]
        MDFeAutorizacaoSinc = 20,

        /// <summary>
        /// GNRE - Consultar Configurações da UF
        /// </summary>
        [Description("GNRE - Consultar Configurações da UF")]
        GNREConsultaConfigUF = 21,

        /// <summary>
        /// GNRE - Consultar Resultado do Lote de GNRE enviado
        /// </summary>
        [Description("Consultar Resultado do Lote de GNRE enviado")]
        GNREConsultaResultadoLote = 22,

        /// <summary>
        /// GNRE - Enviar Lote de GNRE
        /// </summary>
        [Description("Enviar Lote de GNRE")]
        GNRELoteRecepcao = 23,

        /// <summary>
        /// Enviar o XML de cancelamento da NFS-e para prefeiutura
        /// </summary>
        [Description("Cancelamento da NFS-e")]
        NFSeCancelarNfse = 24,

        /// <summary>
        /// Enviar o XML de consulta NFS-e do prestador para a prefeitura
        /// </summary>
        [Description("Consulta NFS-e do prestador")]
        NFSeConsultarNotaPrestador = 25,

        /// <summary>
        /// Enviar o XML de consulta NFS-e valida para a prefeitura
        /// </summary>
        [Description("Consulta NFS-e valida")]
        NFSeConsultarNotaValida = 26,

        /// <summary>
        /// Enviar o XML da NFS-e para a prefeitura
        /// </summary>
        [Description("Envio da NFS-e")]
        NFSeGerarNfse = 27,

        /// <summary>
        /// Enviar o XML do Lote de RPS para gerar NFSe para a prefeitura
        /// </summary>
        [Description("Envio do lote RPS para gerar NFSe")]
        NFSeRecepcionarLoteRps = 28,

        /// <summary>
        /// Enviar o XML do Lote de RPS no modo síncrono para gerar NFSe para a prefeitura
        /// </summary>
        [Description("Envio do lote RPS no modo síncrono para gerar NFSe")]
        NFSeRecepcionarLoteRpsSincrono = 29,

        /// <summary>
        /// Enviar o XML para Substituir NFSe
        /// </summary>
        [Description("Envio da substituição da NFS-e")]
        NFSeSubstituirNfse = 30,

        /// <summary>
        /// Enviar o XML de consulta lote RPS para a prefeitura
        /// </summary>
        [Description("Consulta lote RPS")]
        NFSeConsultarLoteRps = 31,

        /// <summary>
        /// Enviar o XML de consulta NFSe para a prefeitura
        /// </summary>
        [Description("Consulta NFSe")]
        NFSeConsultarNfse = 32,

        /// <summary>
        /// Enviar o XML de consulta NFSe serviço prestado para a prefeitura
        /// </summary>
        [Description("Consulta NFSe serviço prestado")]
        NFSeConsultarNfseServicoPrestado = 33,

        /// <summary>
        /// Enviar o XML de consulta NFSe serviço tomado para a prefeitura
        /// </summary>
        [Description("Consulta NFSe serviço tomado")]
        NFSeConsultarNfseServicoTomado = 34,

        /// <summary>
        /// Enviar o XML de consulta NFSe por faixa para a prefeitura
        /// </summary>
        [Description("Consulta NFSe por Faixa")]
        NFSeConsultarNfseFaixa = 35,

        /// <summary>
        /// Enviar o XML de consulta NFSe por RPS para a prefeitura
        /// </summary>
        [Description("Consulta NFSe por RPS")]
        NFSeConsultarNfsePorRps = 36,

        /// <summary>
        /// Enviar o XML de consulta/download do PDF NFSe para a prefeitura
        /// </summary>
        [Description("Consulta/Download do PDF da NFSe")]
        NFSeConsultarNfsePDF = 37,

        /// <summary>
        /// GNRE - Consultar Lote Recepção Consulta
        /// </summary>
        [Description("Consultar Lote Recepção Consulta")]
        GNREConsultaLoteRecepcao = 38,

        /// <summary>
        /// GNRE - Consultar Resultado do Lote da Consulta de GNRE enviado
        /// </summary>
        [Description("Consultar Resultado do Lote da Consulta de GNRE")]
        GNREConsultaResultadoLoteConsulta = 39,

        /// <summary>
        /// Enviar o XML de consulta situação lote RPS para a prefeitura
        /// </summary>
        [Description("Consultar Situacao Lote Rps")]
        NFSeConsultarSituacaoLoteRps = 40,

        /// <summary>
        /// Enviar o XML de consulta nfse recebidas para a prefeitura
        /// </summary>
        [Description("Consulta NFSe Recebidas")]
        NFSeConsultaNFeRecebidas = 41,

        /// <summary>
        /// Enviar o XML de consulta nfse emitidas para a prefeitura
        /// </summary>
        [Description("Consulta NFSe Emitidas")]
        NFSeConsultaNFeEmitidas = 42,

        /// <summary>
        /// Enviar o XML de teste envio lote rps para a prefeitura
        /// </summary>
        [Description("Teste Envio Lote RPS")]
        NFSeTesteEnvioLoteRps = 43,

        /// <summary>
        /// Enviar o XML de envio lote rps para a prefeitura
        /// </summary>
        [Description("Envio Lote RPS")]
        NFSeEnvioLoteRps = 44,

        /// <summary>
        /// Enviar o XML deenvio rps para a prefeitura
        /// </summary>
        [Description("Envio RPS")]
        NFSeEnvioRps = 45,

        /// <summary>
        /// Enviar o XML de cancelamento nfse para a prefeitura
        /// </summary>
        [Description("Cancelamento NFSe")]
        NFSeCancelamentoNfe = 46,

        /// <summary>
        /// Enviar o XML de consulta informações lote para a prefeitura
        /// </summary>
        [Description("Consulta Informações Lote")]
        NFSeConsultaInformacoesLote = 47,

        /// <summary>
        /// Enviar o XML de consulta lote para a prefeitura
        /// </summary>
        [Description("Consulta Lote")]
        NFSeConsultaLote = 48,

        #endregion

        #region Gerais

        /// <summary>
        /// Serviço não definido
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
        SNCM = 9
    }

    #endregion       

    #region UF

    /// <summary>
    /// Unidades Federativas do Brasil (Tem como XmlEnum o nome abreviado da UF)
    /// </summary>
    public enum UFBrasil
    {
        /// <summary>
        /// Acre - AC (12)
        /// </summary>
        AC = 12,

        /// <summary>
        /// Alagoas - AL (27)
        /// </summary>
        AL = 27,

        /// <summary>
        /// Amapá - AP (16)
        /// </summary>
        AP = 16,

        /// <summary>
        /// Amazonas - AM (13)
        /// </summary>
        AM = 13,

        /// <summary>
        /// Bahia - BA (29)
        /// </summary>
        BA = 29,

        /// <summary>
        /// Ceará - CE (23)
        /// </summary>
        CE = 23,

        /// <summary>
        /// Distrito Federal - DF (53)
        /// </summary>
        DF = 53,

        /// <summary>
        /// Espírito Santo - ES (32)
        /// </summary>
        ES = 32,

        /// <summary>
        /// Goiás - GO (52)
        /// </summary>
        GO = 52,

        /// <summary>
        /// Maranhão - MA (21)
        /// </summary>
        MA = 21,

        /// <summary>
        /// Mato Grosso - MT (51)
        /// </summary>
        MT = 51,

        /// <summary>
        /// Mato Grosso do Sul - MS (50)
        /// </summary>
        MS = 50,

        /// <summary>
        /// Minas Gerais - MG (31)
        /// </summary>
        MG = 31,

        /// <summary>
        /// Pará - PA (15)
        /// </summary>
        PA = 15,

        /// <summary>
        /// Paraíba - PB (25)
        /// </summary>
        PB = 25,

        /// <summary>
        /// Paraná - PR (41)
        /// </summary>
        PR = 41,

        /// <summary>
        /// Pernambuco - PE (26)
        /// </summary>
        PE = 26,

        /// <summary>
        /// Piauí - PI (22)
        /// </summary>
        PI = 22,

        /// <summary>
        /// Rio de Janeiro - RJ (33)
        /// </summary>
        RJ = 33,

        /// <summary>
        /// Rio Grande do Norte - RN (24)
        /// </summary>
        RN = 24,

        /// <summary>
        /// Rio Grande do Sul - RS (43)
        /// </summary>
        RS = 43,

        /// <summary>
        /// Rondônia - RO (11)
        /// </summary>
        RO = 11,

        /// <summary>
        /// Roraima - RR (14)
        /// </summary>
        RR = 14,

        /// <summary>
        /// Santa Catarina - SC (42)
        /// </summary>
        SC = 42,

        /// <summary>
        /// São Paulo - SP (35)
        /// </summary>
        SP = 35,

        /// <summary>
        /// Sergipe - SE (28)
        /// </summary>
        SE = 28,

        /// <summary>
        /// Tocantins - TO (17)
        /// </summary>
        TO = 17,

        ///<summary>
        /// SUFRAMA (90)
        /// </summary>
        SUFRAMA = 90,

        /// <summary>
        /// RFB - Ambiente Nacional - AN (91)
        /// </summary>
        AN = 91,

        /// <summary>
        /// SVCRS - Servico Virtual de Contingência do Rio Grande do Sul (94)
        /// </summary>
        SVCRS = 94,

        /// <summary>
        /// SVCRS - Servico Virtual de Contingência de São Paulo (95)
        /// </summary>
        SVCSP = 95,

        /// <summary>
        /// Sincronização de chaves do estado do Rio Grande do Sul com o Sistema Virtual de São Paulo (96)
        /// </summary>
        SincChavesRSparaSVSP = 96,

        /// <summary>
        /// Exportação
        /// </summary>
        EX = 99,

        /// <summary>
        /// Não definido (0)
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
        /// Ambiente de Produção
        /// </summary>
        [XmlEnum("1")]
        Producao = 1,
        /// <summary>
        /// Ambiente de Homologação
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
        /// Evento desconhecido
        /// </summary>
        [XmlEnum("0")]
        Desconhecido = 0,

        /// <summary>
        /// Carta de correção eletrônica (110110)
        /// </summary>
        [XmlEnum("110110")]
        CartaCorrecao = 110110,

        /// <summary>
        /// Cancelamento NFe (110111)
        /// </summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>
        /// Cancelamento da NFCe sendo substituída por outra NFCe (110112)
        /// </summary>
        [XmlEnum("110112")]
        CancelamentoPorSubstituicao = 110112,

        /// <summary>
        /// Comprovante de Entrega da NF-e
        /// </summary>
        [XmlEnum("110130")]
        ComprovanteEntregaNFe = 110130,

        /// <summary>
        /// Cancelamento do Comprovante de Entrega da NF-e
        /// </summary>
        [XmlEnum("110131")]
        CancelamentoComprovanteEntregaNFe = 110131,

        /// <summary>
        /// EPEC - Evento Prévio de Emissão em Contingência (110140)
        /// </summary>
        [XmlEnum("110140")]
        EPEC = 110140,

        /// <summary>
        /// Pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (111500) - 1o Prazo
        /// </summary>
        [XmlEnum("111500")]
        PedidoProrrogacaoPrazo1 = 111500,

        /// <summary>
        /// Pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (111501) - 2o Prazo
        /// </summary>
        [XmlEnum("111501")]
        PedidoProrrogacaoPrazo2 = 111501,

        /// <summary>
        /// Cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (111502) - 1o Prazo
        /// </summary>
        [XmlEnum("111502")]
        CancelamentoPedidoProrrogacaoPrazo1 = 111502,

        /// <summary>
        /// Cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (111503) - 2o Prazo
        /// </summary>
        [XmlEnum("111503")]
        CancelamentoPedidoProrrogacaoPrazo2 = 111503,

        /// <summary>
        /// Manifestação do Destinatário - Confirmação da Operação (210200)
        /// </summary>
        [XmlEnum("210200")]
        ManifestacaoConfirmacaoOperacao = 210200,

        /// <summary>
        /// Manifestação do Destinatário - Ciência da Operação (210210)
        /// </summary>
        [XmlEnum("210210")]
        ManifestacaoCienciaOperacao = 210210,

        /// <summary>
        /// Manifestação do Destinatário - Desconhecimento da Operação (210220)
        /// </summary>
        [XmlEnum("210220")]
        ManifestacaoDesconhecimentoOperacao = 210220,

        /// <summary>
        /// Manifestação do Destinatário - Operação não realizada (210240)
        /// </summary>
        [XmlEnum("210240")]
        ManifestacaoOperacaoNaoRealizada = 210240,

        /// <summary>
        /// SEFAZ do emitente declara que NF-e é um "Documento Fiscal Inidôneo". (400200)
        /// </summary>
        [XmlEnum("400200")]
        DocumentoFiscalInidoneo = 400200,

        /// <summary>
        /// Cancelamento do evento 400200 (400201)
        /// </summary>
        [XmlEnum("400201")]
        CancelamentoEventoFisco400200 = 400201,

        /// <summary>
        /// Possibilita que a SEFAZ marque uma NF-e emitida em função de uma situação específica prevista em legislação, ex.: transferência de crédito, ressarcimento. (400300)
        /// </summary>
        [XmlEnum("400300")]
        VistoEletronicoDoFisco = 400300,

        /// <summary>
        /// O evento da Nota Fiscal Referenciada é gerado sempre que uma nova NF-e referenciar uma ou mais outras Notas Fiscais Eletrônicas. Não serão gerados eventos de "NF-e Referenciada" para os documentos diferentes do Modelo 55. (410300)
        /// </summary>
        [XmlEnum("410300")]
        NFeReferenciada = 410300,

        /// <summary>
        /// Resposta ao pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (411500) - 1o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411500")]
        RespostaPedidoProrrogacaoPrazo1 = 411500,

        /// <summary>
        /// Resposta ao pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (411501) - 2o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411501")]
        RespostaPedidoProrrogacaoPrazo2 = 411501,

        /// <summary>
        /// Resposta ao cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (411502) - 1o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411502")]
        RespostaCancelamentoPedidoProrrogacaoPrazo1 = 411502,

        /// <summary>
        /// Resposta ao cancelamento do pedido de prorrogação do prazo de ICMS no caso de remessa para industrialização (411503) - 2o Prazo (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("411503")]
        RespostaCancelamentoPedidoProrrogacaoPrazo2 = 411503,

        /// <summary>
        /// Registro de Passagem da NF-e no Posto Fiscal (610500)
        /// </summary>
        [XmlEnum("610500")]
        RegistroPassagemNFe = 610500,

        /// <summary>
        /// Cancelamento do evento 610500 (610501)
        /// </summary>
        [XmlEnum("610501")]
        CancelamentoRegistroPassagemNFe = 610501,

        /// <summary>
        /// Registro de Passagem do MDF-e no Posto Fiscal, propagado pelo Sistema MDF-e. (610510)
        /// </summary>
        [XmlEnum("610510")]
        RegistroDePassagemMDFe = 610510,

        /// <summary>
        /// Cancelamento do evento 610511 (610511)
        /// </summary>
        [XmlEnum("610511")]
        CancelamentoRegistroDePassagemMDFe = 610511,

        /// <summary>
        /// Registro de Passagem do MDF-e no Posto Fiscal, propagado pelo Ambiente Nacional. Nota: A Chave de Acesso da NF-e está vinculada a um CT-e citado no MDF-e. (610514)
        /// </summary>
        [XmlEnum("610514")]
        RegistroDePassagemMDFeComCTe = 610514,

        /// <summary>
        /// Cancelamento do evento 610514. (610515)
        /// </summary>
        [XmlEnum("610515")]
        CancelamentoRegistroDePassagemMDFeComCTe = 610515,

        /// <summary>
        /// Registro de Passagem do MDF-e, capturado por antenas do Projeto Brasil ID. Evento eliminado (BT 2017.002), substituído pelo Registro de Passagem Automático MDF-e. (610550)
        /// </summary>
        [XmlEnum("610550")]
        RegistroPassagemNFeBRId = 610550,

        /// <summary>
        /// Registro de Passagem do MDF-e capturado de forma automática (antena, leitura de placa por OCR, etc.), propagado pelo Sistema MDFe. Nota: A Chave de Acesso da NF-e está citada no MDF-e. (610552)
        /// </summary>
        [XmlEnum("610552")]
        RegistroDePassagemAutomaticoMDFe = 610552,

        /// <summary>
        /// Cancelamento do evento 610552 (610554)
        /// </summary>
        [XmlEnum("610554")]
        RegistroDePassagemAutomaticoMDFeComCTe = 610554,

        /// <summary>
        /// Documenta na NF-e a ocorrência de CT-e autorizado, no momento do compartilhamento do CT-e com o Ambiente Nacional. Nota: A Chave de Acesso da NF-e está citada no CTe. (610600)
        /// </summary>
        [XmlEnum("610600")]
        CTeAutorizado = 610600,

        /// <summary>
        /// Documenta na NF-e a ocorrência de cancelamento de CT-e autorizado, no momento do compartilhamento do evento com o Ambiente Nacional. Nota: A Chave de Acesso da NF-e está citada no CT-e. (610601)
        /// </summary>
        [XmlEnum("610601")]
        CTeCancelado = 610601,

        /// <summary>
        /// Evento que documenta na NF-e a ocorrência de MDF-e autorizado.Nota: A Chave de Acesso da NF-e está citada no MDF-e. (610610)
        /// </summary>
        [XmlEnum("610610")]
        MDFeAutorizado = 610610,

        /// <summary>
        /// Cancelamento do MDF-e (610611)
        /// </summary>
        [XmlEnum("610611")]
        MDFeCancelado = 610611,

        /// <summary>
        /// Evento que documenta na NF-e a ocorrência de MDF-e autorizado. Nota: A Chave de Acesso da NF-e está vinculada a um CT-e citado no MDF-e. (610614)
        /// </summary>
        [XmlEnum("610614")]
        MDFeAutorizadoComCTe = 610614,

        /// <summary>
        /// Cancelamento do evento 610614. (610615)
        /// </summary>
        [XmlEnum("610615")]
        CancelamentoDoMDFeAutorizadoComCTe = 610615,

        /// <summary>
        /// Evento que indica a quantidade de mercadoria na unidade tributável que foi efetivamente embarcada para o exterior referente a um certo item de uma NF-e. Gerado e enviado pelo sistema Portal Único do Comércio Exterior (PUCOMEX) Receita Federal do Brasil (RFB) para o Ambiente Nacional da NF-e (790700)
        /// </summary>
        [XmlEnum("790700")]
        AverbacaoDeExportacao = 790700,

        /// <summary>
        /// Registro da ocorrência da Vistoria do processo de internalização de produtos industrializados de origem nacional com isenção de ICMS nas áreas sob controle da SUFRAMA. (990900)
        /// </summary>
        [XmlEnum("990900")]
        VistoriaSUFRAMA = 990900,

        /// <summary>
        /// Confirmação da internalização de produtos industrializados de origem nacional com isenção de ICMS nas áreas sob controle da SUFRAMA. (990910)
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
        /// Evento desconhecido
        /// </summary>
        [XmlEnum("0")]
        Desconhecido = 0,
        /// <summary>
        /// Carta de Correção CTe (110110)
        /// </summary>
        [XmlEnum("110110")]
        CartaCorrecao = 110110,

        /// <summary>
        /// Cancelamento CTe (110111)
        /// </summary>
        [XmlEnum("110111")]
        Cancelamento = 110111,

        /// <summary>
        /// EPEC = Evento Previo da Emissão em Contingência do CTe (110113)
        /// </summary>
        [XmlEnum("110113")]
        EPEC = 110113,

        /// <summary>
        /// Comprovante de Entrega do CTe (110180)
        /// </summary>
        [XmlEnum("110180")]
        ComprovanteEntrega = 110180,

        /// <summary>
        /// Cancelamento Comprovante de Entrega do CTe (110181)
        /// </summary>
        [XmlEnum("110181")]
        CancelamentoComprovanteEntrega = 110181,

        /// <summary>
        /// Prestação de serviço em desacordo CTe (610110)
        /// </summary>
        [XmlEnum("610110")]
        PrestDesacordo = 610110,

        /// <summary>
        /// Registro de Passagem CT-e (310620)
        /// </summary>
        [XmlEnum("310620")]
        RegistroPassagem = 310620,

        /// <summary>
        /// MDFe cancelado (310611) (Evento exclusivo do fisco)
        /// </summary>
        [XmlEnum("310611")]
        MDFeCancelado = 310611
    }

    #endregion

    #region TipoEventoMDFe

    /// <summary>
    /// Tipos de eventos do MDFe
    /// </summary>
    public enum TipoEventoMDFe
    {
        /// <summary>
        /// Evento desconhecido
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
        /// Não (0)
        /// </summary>
        [XmlEnum("0")]
        Nao = 0,

        /// <summary>
        /// Sim (1)
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
        /// Sim (1)
        /// </summary>
        [XmlEnum("1")]
        Sim = 1,

        /// <summary>
        /// Não (2)
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
        /// Operação de entrada
        /// </summary>
        [XmlEnum("0")]
        Entrada = 0,
        /// <summary>
        /// Operação de saída
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
        /// Operação interna, ou seja, dentro do estado de origem
        /// </summary>
        [XmlEnum("1")]
        OperacaoInterna = 1,

        /// <summary>
        /// Operação interestadual, ou seja, estado diferente do de origem
        /// </summary>
        [XmlEnum("2")]
        OperacaoInterestadual = 2,

        /// <summary>
        /// Operação com o exterior, ou seja, fora do país de origem
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
        /// 0=Sem geração de DANFE
        /// </summary>
        [XmlEnum("0")]
        SemGeracao = 0,

        /// <summary>
        /// 1=DANFE normal, Retrato
        /// </summary>
        [XmlEnum("1")]
        NormalRetrato = 1,

        /// <summary>
        /// 2=DANFE normal, Paisagem
        /// </summary>
        [XmlEnum("2")]
        NormalPaisagem = 2,

        /// <summary>
        /// 3=DANFE Simplificado
        /// </summary>
        [XmlEnum("3")]
        Simplificado = 3,

        /// <summary>
        /// 4=DANFE NFC-e
        /// </summary>
        [XmlEnum("4")]
        NFCe = 4,

        /// <summary>
        /// 5=DANFE NFC-e em mensagem eletrônica
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
        /// 1=Retrato
        /// </summary>
        [XmlEnum("1")]
        NormalRetrato = 1,

        /// <summary>
        /// 2=Paisagem
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
        /// 1=Emissão normal (não em contingência)
        /// </summary>
        [XmlEnum("1")]
        Normal = 1,

        /// <summary>
        /// 2=Contingência FS-IA, com impressão do DANFE em formulário de segurança ou Para MDFe é impressão em formulário branco (sulfite)
        /// </summary>
        [XmlEnum("2")]
        ContingenciaFSIA = 2,

        /// <summary>
        /// 3=Regime Especial NFF (Nota Fiscal Fácil)
        /// </summary>
        [XmlEnum("3")]
        RegimeEspecialNFF,

        /// <summary>
        /// 4=Contingência EPEC (Evento Prévio de Emissão em Contingência)
        /// </summary>
        [XmlEnum("4")]
        ContingenciaEPEC = 4,

        /// <summary>
        /// 5=Contingência FS-DA, com impressão do DANFE em formulário de segurança;
        /// </summary>
        [XmlEnum("5")]
        ContingenciaFSDA = 5,

        /// <summary>
        /// 6=Contingência SVC-AN (SEFAZ Virtual de Contingência do AN);
        /// </summary>
        [XmlEnum("6")]
        ContingenciaSVCAN = 6,

        /// <summary>
        /// 7=Contingência SVC-RS (SEFAZ Virtual de Contingência do RS);
        /// </summary>
        [XmlEnum("7")]
        ContingenciaSVCRS = 7,

        /// <summary>
        /// 8=Contingência SVC-SP (SEFAZ Virtual de Contingência de SP);
        /// </summary>
        [XmlEnum("8")]
        ContingenciaSVCSP = 8,

        /// <summary>
        /// 9=Contingência off-line da NFC-e
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
        /// 1=NF-e normal
        /// </summary>
        [XmlEnum("1")]
        Normal = 1,

        /// <summary>
        /// 2=NF-e complementar
        /// </summary>
        [XmlEnum("2")]
        Complementar = 2,

        /// <summary>
        /// 3=NF-e de ajuste
        /// </summary>
        [XmlEnum("3")]
        Auste = 3,

        /// <summary>
        /// 4=Devolução de mercadoria
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
        /// 0=Não se aplica (por exemplo, Nota Fiscal complementar ou de ajuste)
        /// </summary>
        [XmlEnum("0")]
        NaoSeAplica = 0,

        /// <summary>
        /// 1=Operação presencial
        /// </summary>
        [XmlEnum("1")]
        OperacaoPresencial = 1,

        /// <summary>
        /// 2=Operação não presencial, pela Internet
        /// </summary>
        [XmlEnum("2")]
        OperacaoInternet = 2,

        /// <summary>
        /// 3=Operação não presencial, Teleatendimento
        /// </summary>
        [XmlEnum("3")]
        OperacaoTeleAtendimento = 3,

        /// <summary>
        /// 4=NFC-e em operação com entrega a domicílio
        /// </summary>
        [XmlEnum("4")]
        NFCeEntregaDomicilio = 4,

        /// <summary>
        /// Operação presencial, fora do estabelecimento
        /// </summary>
        [XmlEnum("5")]
        [Description("Operação presencial, fora do estabelecimento")]
        PresencialForaEstabelecimento = 5,

        /// <summary>
        /// 9=Operação não presencial, outros
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
        /// 0=Emissão de NF-e com aplicativo do contribuinte
        /// </summary>
        [XmlEnum("0")]
        AplicativoContribuinte = 0,

        /// <summary>
        /// 1=Emissão de NF-e avulsa pelo Fisco;
        /// </summary>
        [XmlEnum("1")]
        AvulsaPeloFisco = 1,

        /// <summary>
        /// 2=Emissão de NF-e avulsa, pelo contribuinte com seu certificado digital, através do site do Fisco;
        /// </summary>
        [XmlEnum("2")]
        AvulsaPeloContribuinteSiteFisco = 2,

        /// <summary>
        /// 3=Emissão NF-e pelo contribuinte com aplicativo fornecido pelo Fisco.
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
        /// 1=Simples Nacional
        /// </summary>
        [XmlEnum("1")]
        SimplesNacional = 1,

        /// <summary>
        /// 2=Simples Nacional, excesso sublimite de receita bruta
        /// </summary>
        [XmlEnum("2")]
        SimplesNacionalExcessoSublimite = 2,

        /// <summary>
        /// 3=Regime Normal
        /// </summary>
        [XmlEnum("3")]
        RegimeNormal = 3
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
        /// 1=Contribuinte ICMS (informar a IE do destinatário)
        /// </summary>
        [XmlEnum("1")]
        ContribuinteICMS = 1,

        /// <summary>
        /// 2=Contribuinte isento de Inscrição no cadastro de Contribuintes do ICMS
        /// </summary>
        [XmlEnum("2")]
        ContribuinteIsento = 2,

        /// <summary>
        /// 9=Não Contribuinte, que pode ou não possuir Inscrição Estadual no Cadastro de Contribuintes do ICMS
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
        /// 1=Marítima
        /// </summary>
        [XmlEnum("1")]
        Maritima = 1,

        /// <summary>
        /// 2=Fluvial
        /// </summary>
        [XmlEnum("2")]
        Fluvial = 2,

        /// <summary>
        /// 3=Lacustre
        /// </summary>
        [XmlEnum("3")]
        Lacustre = 3,

        /// <summary>
        /// 4=Aérea
        /// </summary>
        [XmlEnum("4")]
        Aerea = 4,

        /// <summary>
        /// 5=Postal
        /// </summary>
        [XmlEnum("5")]
        Postal = 5,

        /// <summary>
        /// 6=Ferroviária
        /// </summary>
        [XmlEnum("6")]
        Ferroviaria = 6,

        /// <summary>
        /// 7=Rodoviária
        /// </summary>
        [XmlEnum("7")]
        Rodoviaria = 7,

        /// <summary>
        /// 8=Conduto / Rede Transmissão
        /// </summary>
        [XmlEnum("8")]
        CondutoRedeTransmissao = 8,

        /// <summary>
        /// 9=Meios Próprios
        /// </summary>
        [XmlEnum("9")]
        MeiosProprios = 9,

        /// <summary>
        /// 10=Entrada / Saída ficta
        /// </summary>
        [XmlEnum("10")]
        EntradaSaidaFicta = 10,

        /// <summary>
        /// 11=Courier
        /// </summary>
        [XmlEnum("11")]
        Courier = 11,

        /// <summary>
        /// 12=Em Mãos
        /// </summary>
        [XmlEnum("12")]
        EmMaos = 12,

        /// <summary>
        /// 13=Por Reboque
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
        /// 1=Importação por conta própria
        /// </summary>
        [XmlEnum("1")]
        ImportacaoPorContaPropria = 1,

        /// <summary>
        /// 2=Importação por conta e ordem
        /// </summary>
        [XmlEnum("2")]
        ImportacaoPorContaOrdem = 2,

        /// <summary>
        /// 3=Importação por encomenda
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
        /// 0=Margem Valor Agregado (%)
        /// </summary>
        [XmlEnum("0")]
        MargemValorAgregado = 0,

        /// <summary>
        /// 1=Pauta (Valor)
        /// </summary>
        [XmlEnum("1")]
        Pauta = 1,

        /// <summary>
        /// 2=Preço Tabelado Máx. (valor)
        /// </summary>
        [XmlEnum("2")]
        PrecoTabeladoMaximo = 2,

        /// <summary>
        /// 3=Valor da operação
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
        /// 0=Preço tabelado ou máximo sugerido
        /// </summary>
        [XmlEnum("0")]
        PrecoTabeladoMaximoSugerido = 0,

        /// <summary>
        /// 1=Lista Negativa (valor)
        /// </summary>
        [XmlEnum("1")]
        ListaNegativa = 1,

        /// <summary>
        /// 2=Lista Positiva (valor)
        /// </summary>
        [XmlEnum("2")]
        ListaPositiva = 2,

        /// <summary>
        /// 3=Lista Neutra (valor)
        /// </summary>
        [XmlEnum("3")]
        ListaNeutra = 3,

        /// <summary>
        /// 4=Margem Valor Agregado (%)
        /// </summary>
        [XmlEnum("4")]
        MargemValorAgregado = 4,

        /// <summary>
        /// 5=Pauta (valor)
        /// </summary>
        [XmlEnum("5")]
        Pauta = 5,

        /// <summary>
        /// 6=Valor da Operação
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
        /// 0=SEFAZ
        /// </summary>
        [XmlEnum("0")]
        SEFAZ = 0,

        /// <summary>
        /// 1=Justiça Federal
        /// </summary>
        [XmlEnum("1")]
        JusticaFederal = 1,

        /// <summary>
        /// 2=Justiça Estadual
        /// </summary>
        [XmlEnum("2")]
        JusticaEstadual = 2,

        /// <summary>
        /// 3=Secex/RFB
        /// </summary>
        [XmlEnum("3")]
        SecexRFB = 3,

        /// <summary>
        /// 9=Outros
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
        /// 3=CTe de Anulação
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
        NFeStatusServico,

        /// <summary>
        /// XML de consulta situação da NFe/NFCe
        /// </summary>
        [Description("XML de consulta situação da NFe/NFCe")]
        NFeConsultaSituacao,

        /// <summary>
        /// XML de consulta do recibo do lote da NFe/NFCe
        /// </summary>
        [Description("XML de consulta do recibo do lote da NFe/NFCe")]
        NFeConsultaRecibo,

        /// <summary>
        /// XML de consulta cadastro do contribuinte da NFe/NFCe
        /// </summary>
        [Description("XML de consulta cadastro do contribuinte da NFe/NFCe")]
        NFeConsultaCadastro,

        /// <summary>
        /// XML de consulta dos documentos fiscais eletrônicos distribuídos da NFe/NFCe
        /// </summary>
        [Description("XML de consulta dos documentos fiscais eletrônicos distribuídos da NFe/NFCe")]
        NFeDistribuicaoDFe,

        /// <summary>
        /// XML de envio de evento da NFe/NFCe
        /// </summary>
        [Description("XML de envio de evento da NFe/NFCe")]
        NFeEnvioEvento,

        /// <summary>
        /// XML de Inutilização da NFe/NFCe
        /// </summary>
        [Description("XML de Inutilização da NFe/NFCe")]
        NFeInutilizacao,

        /// <summary>
        /// XML individual da NFe/NFCe
        /// </summary>
        [Description("XML individual da NFe/NFCe")]
        NFe,

        /// <summary>
        /// XML de envio em lote da NFe/NFCe
        /// </summary>
        [Description("XML de envio em lote da NFe/NFCe")]
        NFeEnvioEmLote,

        #endregion 

        #region CTe

        /// <summary>
        /// XML de consulta status do serviço do CTe
        /// </summary>
        [Description("XML de consulta status do serviço do CTe")]
        CTeStatusServico,

        /// <summary>
        /// XML de consulta situação do CTe
        /// </summary>
        [Description("XML de consulta situação do CTe")]
        CTeConsultaSituacao,

        /// <summary>
        /// XML de consulta do recibo do lote do CTe
        /// </summary>
        [Description("XML de consulta do recibo do lote do CTe")]
        CTeConsultaRecibo,

        /// <summary>
        /// XML de envio de evento do CTe
        /// </summary>
        [Description("XML de envio de evento do CTe")]
        CTeEnvioEvento,

        /// <summary>
        /// XML de Inutilização do CTe
        /// </summary>
        [Description("XML de Inutilização do CTe")]
        CTeInutilizacao,

        /// <summary>
        /// XML individual do CTe
        /// </summary>
        [Description("XML individual do CTe")]
        CTe,

        /// <summary>
        /// XML de envio em lote do CTe
        /// </summary>
        [Description("XML de envio em lote do CTe")]
        CTeEnvioEmLote,

        /// <summary>
        /// XML do CTeOS
        /// </summary>
        [Description("XML do CTeOS")]
        CTeOS,

        /// <summary>
        /// XML de consulta dos documentos fiscais eletrônicos distribuídos do CTe
        /// </summary>
        [Description("XML de consulta dos documentos fiscais eletrônicos distribuídos do CTe")]
        CTeDistribuicaoDFe,

        #endregion 

        #region MDFe

        /// <summary>
        /// XML de consulta status do serviço do MDFe
        /// </summary>
        [Description("XML de consulta status do serviço do MDFe")]
        MDFeStatusServico,

        /// <summary>
        /// XML de consulta situação do MDFe
        /// </summary>
        [Description("XML de consulta situação do MDFe")]
        MDFeConsultaSituacao,

        /// <summary>
        /// XML de consulta do recibo do lote do MDFe
        /// </summary>
        [Description("XML de consulta do recibo do lote do MDFe")]
        MDFeConsultaRecibo,

        /// <summary>
        /// XML de envio de evento do MDFe
        /// </summary>
        [Description("XML de envio de evento do MDFe")]
        MDFeEnvioEvento,

        /// <summary>
        /// XML individual do MDFe
        /// </summary>
        [Description("XML individual do MDFe")]
        MDFe,

        /// <summary>
        /// XML de envio em lote do MDFe
        /// </summary>
        [Description("XML de envio em lote do MDFe")]
        MDFeEnvioEmLote,

        /// <summary>
        /// XML de consulta dos MDFe´s não encerrados
        /// </summary>
        [Description("XML de consulta dos MDFe´s não encerrados")]
        MDFeConsultaNaoEncerrado,

        #endregion 

        /// <summary>
        /// Não foi possível identificar o tipo do XML
        /// </summary>
        [Description("Não foi possível identificar o tipo do XML")]
        NaoIdentificado
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
}