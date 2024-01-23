Attribute VB_Name = "ConstAndEnuns"
Option Explicit

Public Const TpAmb = 2

Public Enum UFBrasil
        AC = 12
        AL = 27
        AP = 16
        AM = 13
        BA = 29
        CE = 23
        DF = 53
        ES = 32
        GO = 52
        MA = 21
        MT = 51
        MS = 50
        MG = 31
        PA = 15
        PB = 25
        PR = 41
        PE = 26
        PI = 22
        RJ = 33
        RN = 24
        RS = 43
        RO = 11
        RR = 14
        SC = 42
        SP = 35
        SE = 28
        [TO] = 17
        SUFRAMA = 90
        AN = 91
        SVCRS = 94
        SVCSP = 95
        SincChavesRSparaSVSP = 96
        EX = 99
        NaoDefinido = 0
End Enum

Public Enum TipoEventoMDFe
        Desconhecido = 0
        Cancelamento = 110111
        Encerramento = 110112
        InclusaoCondutor = 110114
        InclusaoDFe = 110115
        EncerramentoFisco = 310112
        RegistroPassagem = 310620
        RegistroPassagemBRId = 510620
        LiberacaoPrazoCancelamento = 240170
End Enum

Public Enum TipoEmitenteMDFe
        PrestadorServicoTransporte = 1
        TransportadorCargaPropria = 2
        PrestadorServicoTransporteCteGlobalizado = 3
End Enum

Public Enum ModeloDFe
    NFe = 55
    NFCe = 65
    CTe = 57
    MDFe = 58
    CTeOS = 67
End Enum

Public Enum ModalidadeTransporteMDFe
    Rodoviario = 1
    Aereo = 2
    Aquaviario = 3
    Ferroviario = 4
End Enum

Public Enum TipoEmissao
    Normal = 1
    ContingenciaFSIA = 2
    RegimeEspecialNFF
    ContingenciaEPEC = 4
    ContingenciaFSDA = 5
    ContingenciaSVCAN = 6
    ContingenciaSVCRS = 7
    ContingenciaSVCSP = 8
    ContingenciaOffLine = 9
End Enum

Public Enum ProcessoEmissao
    AplicativoContribuinte = 0
    AvulsaPeloFisco = 1
    AvulsaPeloContribuinteSiteFisco = 2
    AplicativoFisco = 3
End Enum

Public Enum TipoProprietarioMDFe
    TACAgregado = 0
    TACIndependente = 1
    Outros = 2
    NaoDefinido = 99999
End Enum

Public Enum TipoRodado
    Truck = 1
    Toco = 2
    CavaloMecanico = 3
    VAN = 4
    Utilitario = 5
    Outros = 6
End Enum

Public Enum TipoCarroceriaMDFe
    NaoAplicavel = 0
    Aberta = 1
    FechadaBau = 2
    Granelera = 3
    PortaContainer = 4
    Sider = 5
End Enum

Public Enum TipoCargaMDFe
    GranelSolido = 1
    GranelLiquido = 2
    Frigorificada = 3
    Conteinerizada = 4
    CargaGeral = 5
    Neogranel = 6
    PerigosaGranelSolido = 7
    PerigosaGranelLiquido = 8
    PerigosaFrigorificada = 9
    PerigosaConteinerizada = 10
    PerigosaCargaGeral = 11
End Enum

Public Enum ResponsavelSeguroMDFe
    EmitenteMDFe = 1
    ContratanteServicoTransporte = 2
End Enum

Public Enum CodigoUnidadeMedidaMDFe
    KG = 1
    TON = 2
End Enum


Public Enum TipoDFe
    Desconhecido = -1
    NFe = 0
    NFCe = 1
    CTe = 2
    CTeOS = 3
    MDFe = 4
    NFSe = 5
    SAT = 6
    CFe = 7
    GNRE = 8
End Enum

Public Enum IndicadorPresenca
        NaoSeAplica = 0
        OperacaoPresencial = 1
        OperacaoInternet = 2
        OperacaoTeleAtendimento = 3
        NFCeEntregaDomicilio = 4
        PresencialForaEstabelecimento = 5
        OperacaoOutros = 9
End Enum

Public Enum IndicadorIntermediario
    OperacaoSemIntermediador = 0
    OperacaoSitePlataformaTerceiro = 1
End Enum
