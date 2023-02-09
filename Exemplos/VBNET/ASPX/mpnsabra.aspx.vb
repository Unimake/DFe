
Imports System
Imports System.Data 
Imports System.Data.OleDb
Imports System.Net.Mail                                                      
Imports System.IO

Imports System.Printing
Imports System.Drawing.Printing 
'<%@ Import Namespace="System.Runtime.InteropServices" 
'<%@ Import Namespace="System.Runtime.InteropServices.CallingConvention" 

IMPORTS SYSTEM.NET
IMPORTS SYSTEM.SECURITY
IMPORTS SYSTEM.SECURITY.CRYPTOGRAPHY
IMPORTS SYSTEM.SECURITY.CRYPTOGRAPHY.X509CERTIFICATES
IMPORTS SYSTEM.SECURITY.CRYPTOGRAPHY.XML
IMPORTS SYSTEM.COLLECTIONS.GENERIC
IMPORTS SYSTEM.TEXT
IMPORTS SYSTEM.XML
IMPORTS SYSTEM.WINDOWS.FORMS
IMPORTS MICROSOFT.WIN32
IMPORTS UNIMAKE.BUSINESS.DFE
IMPORTS UNIMAKE.BUSINESS.DFE.SERVICOS

IMPORTS UNIMAKE.CRIPTOGRAPHY
IMPORTS UNIMAKE.SECURITY.PLATFORM
IMPORTS UNIMAKE.SECURITY.EXCEPTIONS
IMPORTS UNIMAKE.UTILS


Public Class _MPNSABRA
    Inherits System.Web.UI.Page
'
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
    Public Enum SimNao
        Sim = 1
        Nao = 0
    End Enum
    Public Enum TipoEventoMDFe
        Desconhecido = 0
        CartaCorrecao = 110110
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
 
    Public TextoErro as string = ""
               
    PUBLIC CONFIGURACAO = NEW UNIMAKE.BUSINESS.DFE.SERVICOS.CONFIGURACAO
  

    
    PUBLIC OX509CERT AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()
    PUBLIC OCERTIFICADO = NEW X509CERTIFICATE2()
    PUBLIC SEMCERTIFICADO AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()

    PUBLIC DATAHOJE AS STRING
    PUBLIC HOJE AS STRING
    PUBLIC AUTORIZACAO
 
Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    
   If (Not Page.IsPostBack) Then

    mostramensagem.visible = false


  feito.text = 0
  MPNSabra_Shown()

   End If

 End Sub

Public Sub EnviarCamposNFe()
        ' Autorizar a NFe (ou NFCe ) preenchendo os campos necess�rios do Schema

        Configurar()

        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe


        Dim Det = New Unimake.Business.DFe.Xml.NFe.Det
        Dim Prod = New Unimake.Business.DFe.Xml.NFe.Prod
        Dim ICMS = New Unimake.Business.DFe.Xml.NFe.ICMS
        Dim Pis = New Unimake.Business.DFe.Xml.NFe.PIS
        Dim PISOutr = New Unimake.Business.DFe.Xml.NFe.PISOutr
        Dim COFINS = New Unimake.Business.DFe.Xml.NFe.COFINS
        Dim COFINSOutr = New Unimake.Business.DFe.Xml.NFe.COFINSOutr
        Dim Imposto = New Unimake.Business.DFe.Xml.NFe.Imposto
        Dim Total = New Unimake.Business.DFe.Xml.NFe.Total
        Dim ICMSTot = New Unimake.Business.DFe.Xml.NFe.ICMSTot
        Dim Transp = New Unimake.Business.DFe.Xml.NFe.Transp
        Dim Vol = New Unimake.Business.DFe.Xml.NFe.Vol
        Dim Cobr = New Unimake.Business.DFe.Xml.NFe.Cobr
        Dim Fat = New Unimake.Business.DFe.Xml.NFe.Fat
        Dim Dup = New Unimake.Business.DFe.Xml.NFe.Dup
        Dim Pag = New Unimake.Business.DFe.Xml.NFe.Pag
        Dim DetPag = New Unimake.Business.DFe.Xml.NFe.DetPag
        Dim InfAdic = New Unimake.Business.DFe.Xml.NFe.InfAdic
        Dim InfRespTec = New Unimake.Business.DFe.Xml.NFe.InfRespTec
        Dim ICMSSN101 = New Unimake.Business.DFe.Xml.NFe.ICMSSN101

        With InfAdic
            .InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;"
        End With

        With InfRespTec

            .CNPJ = Preencher(cnpj.text, "14")
            .XContato = "Wandrey Mundin Ferreira"
            .Email = "wandrey@unimake.com.br"
            .Fone = "04431414900"
        End With



        With Vol

            .QVol = 1
            .Esp = "LU"
            .Marca = "UNIMAKE"
            .PesoL = 0.000
            .PesoB = 0.000

        End With

        Dim Jura As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Vol)
        Jura.Add(Vol)

        With Transp
            .ModFrete = 9 'ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
            .Vol = Jura
        End With


        With Fat
            .NFat = "057910"
            .VOrig = 169.8
            .VDesc = 0
            .VLiq = 169.8
        End With

        With Dup
            .NDup = "001"
            .DVenc = DateTime.Now
            .VDup = 169.8
        End With


        With DetPag
            .IndPag = 0 'IndicadorPagamento.PagamentoVista
            .TPag = 10 'MeioPagamento.Dinheiro
            .VPag = 169.8
        End With
        Dim Jura1 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.DetPag)
        Jura1.Add(DetPag)

        With Pag
            .DetPag = Jura1
        End With


        '      Dim Jura2 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Fat)
        '      Jura2.Add(Fat)
        Dim Jura3 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Dup)
        Jura3.Add(Dup)
        Dim Jura4 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Pag)
        Jura4.Add(Pag)
        With Cobr

            .Fat = Fat
            .Dup = Jura3

        End With


        With ICMSTot
            .VBC = 0
            .VICMS = 0
            .VICMSDeson = 0
            .VFCP = 0
            .VBCST = 0
            .VST = 0
            .VFCPST = 0
            .VFCPSTRet = 0
            .VProd = 169.8
            .VFrete = 0
            .VSeg = 0
            .VDesc = 0
            .VII = 0
            .VIPI = 0
            .VIPIDevol = 0
            .VPIS = 0
            .VCOFINS = 0
            .VOutro = 0
            .VNF = 169.8
            .VTotTrib = 25.26
        End With

        With Total
            '			
            .ICMSTot = ICMSTot

        End With




        With PISOutr
            .CST = "99"
            .VBC = 0.00
            .PPIS = 0.00
            .VPIS = 0.00
        End With

        With Pis
            .PISOutr = PISOutr
        End With



        With COFINSOutr

            .CST = "99"
            .VBC = 0.00
            .PCOFINS = 0.00
            .VCOFINS = 0.00

        End With

        With COFINS
            .cofinsoutr = COFINSOutr
        End With


        With ICMSSN101
            .Orig = 0 'OrigemMercadoria.Nacional
            .PCredSN = 2.8255
            .VCredICMSSN = 2.4
        End With

        With Prod
            .CProd = "01042"
            .CEAN = "SEM GTIN"
            .XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
            .NCM = "84714900"
            .CFOP = "6101"
            .UCom = "LU"
            .QCom = 1.0
            .VUnCom = 84.9
            .VProd = 84.9
            .CEANTrib = "SEM GTIN"
            .UTrib = "LU"
            .QTrib = 1.0
            .VUnTrib = 84.9
            .IndTot = 1 'SimNao.Sim
            .XPed = "300474"
            .NItemPed = 1
        End With



        With ICMS
            .icmssn101 = ICMSSN101
        End With



        Dim Jura5 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.ICMS)
        Jura5.Add(ICMS)



        With Imposto
            .VTotTrib = 12.63
            .ICMS = Jura5
            .PIS = Pis
            '            .PISOutr = PISOutr
            .COFINS = COFINS
            '           .COFINSOutr = COFINSOutr
        End With




        Dim Emit As New Unimake.Business.DFe.Xml.NFe.Emit
        Dim EnderEmit As New Unimake.Business.DFe.Xml.NFe.EnderEmit
        Dim Dest As New Unimake.Business.DFe.Xml.NFe.Dest
        Dim EnderDest As New Unimake.Business.DFe.Xml.NFe.EnderDest
        Dim Ide As New Unimake.Business.DFe.Xml.NFe.Ide
        With EnderDest
            .XLgr = "AVENIDA DA SAUDADE"
            .Nro = "1555"
            .XBairro = "CAMPOS ELISEOS"
            .CMun = 3543402
            .XMun = "RIBEIRAO PRETO"
            .UF = 35
            .CEP = "14080000"
            .Fone = "01639611500"
        End With


        With Dest
            .CNPJ = "06117473000150"
            .XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
            .EnderDest = EnderDest

            .IndIEDest = 1 'IndicadorIEDestinatario.ContribuinteICMS

            .IE = "582614838110"
            .Email = "janelaorp@janelaorp.com.br"
        End With

        With EnderEmit
            .XLgr = "RUA ANTONIO FELIPE"
            .Nro = "1500"
            .XBairro = "CENTRO"
            .CMun = 4118402
            .XMun = "PARANAVAI"
            .UF = 41
            .CEP = "21931340"
            .Fone = "02124622094"
        End With

        With Emit
            .CNPJ = Preencher(cnpj.text, "14")
            .XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
            .XFant = "UNIMAKE - PARANAVAI"
            .EnderEmit = EnderEmit

            .IE = "9032000301"
            .IM = "14018"
            .CNAE = "6202300"
            .CRT = 1 'CRT.SimplesNacional
        End With

        With Det
            .NITEM = 1
            .Prod = Prod
            .Imposto = Imposto
        End With


        Dim Jura2 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Det)
        Jura2.Add(Det)

        'Dim NFe = New List < NFe >

        '     New NFe
        '
        '       Dim InfNFe = New List < InfNFe >
        '              New InfNFe



        '
        With Ide
            .CNF = 94595994
            .CUF = 33
            .NatOp = "VENDA PRODUC.DO ESTABELEC"
            .Mod = 65
            .Serie = 1
            .NNF = 57972
            .DhEmi = DateTime.Now
            .DhSaiEnt = DateTime.Now
            .TpNF = 1 'TipoOperacao.Saida
            .IdDest = 1 'DestinoOperacao.OperacaoInterestadual
            .CMunFG = 4118402
            If VAL(TipoNF.TEXT) = "55" Then
                .TpImp = 1 'FormatoImpressaoDANFE.NormalRetrato
            Else
                .TpImp = 4
            End If
            .TpEmis = 2
                .TpAmb = TPAmb.text
            .FinNFe = 1 'FinalidadeNFe.Normal
            .IndFinal = 1 'SimNao.Sim
            .IndPres = 1
            .ProcEmi = 1
            .VerProc = "TESTE 1.00"

        End With


        With InfNFe

            InfNFe.Versao = Versao.TEXT
            InfNFe.Ide = Ide

            .Emit = Emit
            .Dest = Dest


            .Det = Jura2

            '				New Det
            '				{
            'NItem = 2,
            'Prod = New Prod
            '					{
            'CProd = "11111",
            'CEAN = "SEM GTIN",
            'XProd = "TESTE DO PRODUTO DO ITEM 2",
            'NCM = "84714900",
            'CFOP = "6101",
            'UCom = "LU",
            'QCom = 1.0,
            'VUnCom = 84.9,
            'VProd = 84.9,
            'CEANTrib = "SEM GTIN",
            'UTrib = "LU",
            'QTrib = 1.0,
            'VUnTrib = 84.9,
            'IndTot = SimNao.Sim,
            'XPed = "300474",
            'NItemPed = 1
            '					},
            'Imposto = New Imposto
            '					{
            'VTotTrib = 12.63,
            'ICMS = New List < ICMS >
            '                       {
            '                          New ICMS
            '						{
            'ICMSSN101 = New ICMSSN101
            '								{
            'Orig = OrigemMercadoria.Nacional,
            'PCredSN = 2.8255,
            'VCredICMSSN = 2.4
            '								}
            '							}
            '						},
            'PIS = New PIS
            '						{                                    
            'PISOutr = New PISOutr
            '							{
            'CST = "99",
            'VBC = 0.00,
            'PPIS = 0.00,
            'VPIS = 0.00
            '							}
            '						},
            'COFINS = New COFINS
            '						{
            'COFINSOutr = New COFINSOutr
            '							{
            'CST = "99",
            'VBC = 0.00,
            'PCOFINS = 0.00,
            'VCOFINS = 0.00
            '							}
            '						}
            '					}
            '				}
            '			},
            .Total = Total


            .Transp = Transp

            .Cobr = Cobr
            .Pag = Pag


            .InfAdic = InfAdic

            .InfRespTec = InfRespTec

            '

        End With

        '        Dim Jura As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.InfNFe)
        '        Jura.Add(Vol)

        Dim Jura6 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.InfNFe)
        Jura6.Add(InfNFe)

        With NFe
            .InfNFe = Jura6
        End With

        Dim Jura7 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.NFe)
        Jura7.Add(NFe)


        Xml.Versao = Versao.TEXT
        Xml.IdLote = "000000000000001"
        Xml.IndSinc = 1

        Xml.nfe = Jura7
        '      Xml.nfe.add(NFe)



        If VAL(TipoNF.TEXT) = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Autorizacao(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Autorizacao(Xml, Configuracao)
        End If

        Autorizacao.Executar
       
        '    MsgBox(Autorizacao.RetornoWSString)
        '    MsgBox(Autorizacao.Result.Cstat & " - " & Autorizacao.Result.XMotivo)




        'Processo Sincrono


       If Autorizacao.result.CStat = 104 Then ''104 = Lote de evento processado com sucesso

                Dim Cstato As String = Autorizacao.Result.ProtNFe.InfProt.CStat.ToString()

        If CStato =  100 Or CStato = 150 then     '//Autorizado o uso da NFe
  '                CStato = 110 Or    '//Uso Denegado
  '              CStato = 150 Or    '//Autorizado o uso da NF-e, autoriza��o fora de prazo
  '            CStato = 205 Or     '//NF-e est� denegada na base de dados da SEFAZ [nRec:999999999999999]
  '          CStato = 301 Or     '//Uso Denegado: Irregularidade fiscal do emitente
  '        CStato = 302 Or      '//Uso Denegado: Irregularidade fiscal do destinat�rio
  '      CStato = 303 Then      '//Uso Denegado: Destinat�rio n�o habilitado a operar na UF

            ' gravar o arquivo autorizado
            Autorizacao.GravarXmlDistribuicao(trim(Certo.text))

            ' Gerar o Objeto para pegar a string e gravar em banco de dados

            Dim docProcNFe As System.Xml.XmlDocument
            docProcNFe = Autorizacao.NfeProcResult.GerarXML()

            ' mostra o nome do arquivo gerado
            Mensagem(Autorizacao.NfeProcResult.NomeArquivoDistribuicao)
            Autorizacao.Result.ProtNFe.InfProt.XMotivo
            exit sub
        Else
            Mensagem("Nota Rejeitada : " & CStato & " - " & Autorizacao.RetornoWSString)
            Exit sub
        End If

       Else
            metext.text = Autorizacao.result.CStat & " - " & Autorizacao.result.xmotivo            
            mensagem("Lote n�o Processado. Poss�vel Problema na SEFAZ.")
            Exit Sub
           
         End If 

    End Sub

    
    Public Sub EnviarXMLNFe()

        'Enviar NFE. Usando o .XML previamente gerado.

        SelecionarCertificado()
  
       If CertificadoValido.Text = False  then
          Mensagem("Certificado Não Valido. Verifique !")
          Exit Sub  
       End If

        Configurar()

       Dim ArquivoOrigem as string = TRIM(PATHE.text) & "envio\" & Trim(ChaveNota.text) & "-NFE.xml" 


        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe
        '
        Dim Doc = New System.Xml.XmlDocument

        Doc.load(TRIM(ArquivoOrigem))

        With Xml
            .IdLote = "000000000000021"
            .IndSinc = 1
            .Versao = "4.00"

        End With

        Dim Jura6 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.NFe)
        Xml.NFe = Jura6


        Try

            Xml.NFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.NFe)(Doc))

        Catch ex As Exception
            GravarArquivoSequencial(trim(certo.text) & "NFe" & trim(ChaveNota.text) & ".err" , ex.ToString)    
            MENSAGEM("Erro ao Deserializar o XML da NFe.")
            EXIT SUB
        End Try


        Try
            
            If val(TipoNF.text) = 55 Then
                Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Autorizacao(Xml, Configuracao)
            Else
                Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Autorizacao(Xml, Configuracao)
            End If
     
            Autorizacao.Executar
          
            Catch ex As Exception
         
               GravarArquivoSequencial(trim(certo.text) & "NFe" & trim(ChaveNota.text) & ".err" , ex.ToString)
               Mensagem("Erro na Autorizacao ou Possivel Problema na SEFAZ")
               Exit Sub

        End Try
            
        If Autorizacao.result.CStat <> 104 Then ''104 = Lote de evento processado com sucesso
           
             metext.text = Autorizacao.result.CStat & " - " & Autorizacao.result.xmotivo
             mensagem("Lote Nao Processado. Possivel Problema na SEFAZ.")
             Exit Sub
         End If

            Dim Cstato As String = Autorizacao.Result.ProtNFe.InfProt.CStat.ToString()

            If CStato =  100 Or CStato = 150  then     '//Autorizado o uso da NFe
               ' 150 /Autorizado o uso da NF-e, autoriza��o fora de prazo
               ' 110    '//Uso Denegado
               ' 205 Or     '//NF-e est� denegada na base de dados da SEFAZ [nRec:999999999999999]
               ' 301 Or     '//Uso Denegado: Irregularidade fiscal do emitente
               ' 302 Or      '//Uso Denegado: Irregularidade fiscal do destinat�rio
               ' 303      '//Uso Denegado: Destinat�rio n�o habilitado a operar na UF
   
                ' gravar o arquivo autorizado
                Autorizacao.GravarXmlDistribuicao(trim(Certo.text))
                ContaGrava.Text = "1"
                metext.text = CStato & " - " & Autorizacao.Result.XMotivo 
                Exit Sub           
           End IF
 

               ' ' Gerar o Objeto para pegar a string com o nome do arquivo e gravar em banco de dados

               ' Dim docProcNFe As System.Xml.XmlDocument
               '  docProcNFe = Autorizacao.NfeProcResult.GerarXML()
               ' mostra o nome do arquivo gerado.  Tirar quando Producao
               'Mensagem(Autorizacao.NfeProcResult.NomeArquivoDistribuicao)
               'Exit Sub
             
                       
              Autorizacao.GravarXmlDistribuicao(trim(Errado.text)) 
    
              Mensagem("Nota Rejeitada : " & CStato & " - " &  Autorizacao.Result.ProtNFe.InfProt.XMotivo)
              
    End Sub
  
 ' Enviar Evento Carta de Corre��o

    Public Sub EnviarEventoCCe()
        SelecionarCertificado()
        Configurar()

        Dim EnvEvento = New Unimake.Business.DFe.Xml.NFe.EnvEvento
        EnvEvento.Versao = "1.00"
        EnvEvento.IdLote = "000000000000001"

        Dim Evento = New Unimake.Business.DFe.Xml.NFe.Evento
        Evento.Versao = "1.00"

        Dim DetEventoCanc = New Unimake.Business.DFe.Xml.NFe.DetEventoCCE

        With DetEventoCanc
            .Versao = "1.00"
            .XCorrecao = "CFOP errada, segue CFOP correta."
        End With

        Dim InfEvento = New Unimake.Business.DFe.Xml.NFe.InfEvento
        InfEvento.DetEvento = DetEventoCanc

        With InfEvento

            .COrgao = CUF.text
            .ChNFe = Trim(chavenota.text)
            .CNPJ = Preencher(cnpj.text, "14")
            .DhEvento = DateTime.Now
            .TpEvento = 110110 
            .NSeqEvento = 1
            .VerEvento = "1.00"
            .TpAmb = TPAmb.text

        End With

        Evento.InfEvento = InfEvento

        EnvEvento.Evento.Add(Evento)
        
        If val(TipoNF.text) = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(EnvEvento, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento(EnvEvento, Configuracao)
        End If


        Autorizacao.Executar

 '       MsgBox(Autorizacao.RetornoWSString)
 '       MsgBox(Autorizacao.Result.RetEvento(0).InfEvento.CStat & " - " & Autorizacao.Result.RetEvento(0).InfEvento.XMotivo)

        '  Gravar o XML de distribui��o se a inutiliza��o foi homologada
        If Autorizacao.result.CStat = 128 Then ''128 = Lote de evento processado com sucesso
                Dim CStatr As String = Autorizacao.Result.RetEvento(0).InfEvento.CStat

                '' 134: Evento homologado
                '' 135: Evento homologado com vincula��o da respectiva NFe
                '' 136: Evento homologado sem vincula��o com a respectiva NFe (SEFAZ n�o encontrou a NFe na base dela)
                '' 155: Evento de Cancelamento homologado fora do prazo permitido para cancelamento


                If CStatr = 134 Or CStatr = 135 Or CStatr = 136 Or CStatr = 155 Then

                    Autorizacao.GravarXmlDistribuicao(trim(Certo.text))

                Else ''Evento rejeitado

                    Autorizacao.GravarXmlDistribuicao(trim(Errado.text))

                End If
           Mensagem(Autorizacao.Result.RetEvento(0).InfEvento.CStat & " - " & Autorizacao.Result.RetEvento(0).InfEvento.XMotivo)
            Exit Sub
            Else
                Mensagem("Lote nao processado. Stat = " & Autorizacao.result.CStat)
                Exit Sub            
            End If

     

    End Sub



 Public Sub ConsultarSituacaoNF()
        
        SelecionarCertificado()

If CertificadoValido.text = true then
        Dim Xml = New Unimake.Business.DFe.Xml.NFe.ConsSitNFe

        Xml.versao = Versao.text
        Xml.TpAmb = TPAmb.text
        Xml.ChNfe = Trim(ChaveNota.text)
        
               
        If val(TipoNF.text) = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo(Xml, Configuracao)
    '         Autorizacao = New Unimake.Business.DFe.XML.NFe.ProtNFe(Xml, Configuracao)
        End If

   Try
        Autorizacao.Executar
    

'        MessageBox.Show(Autorizacao.RetornoWSString)
        Protoca.text = Autorizacao.Result.ProtNFE.InfProt.nProt
        Mensagem(Autorizacao.result.cstat & " - " & Autorizacao.Result.XMotivo)
        Exit Sub 
        
        
        Catch Ex as Exception 
        mensagem("Erro na Consulta. Possível Problema na SEFAZ.   " & Ex.Tostring) 
       
   End Try
End IF

End Sub


 PUBLIC SUB CONSULTARSTATUSSERVICO()
        

 '       CONFIGURACAO.CERTIFICADOARQUIVO = ""

'        CONFIGURACAO.CERTIFICADODIGITAL = SEMCERTIFICADO


        SELECIONARCERTIFICADO()


        IF CertificadoValido.text = TRUE THEN

            DIM XML = NEW UNIMAKE.BUSINESS.DFE.XML.NFE.CONSSTATSERV

            XML.VERSAO = VERSAO.text
            XML.CUF = CUF.text

            XML.TPAMB = TPAMB.text
            ' XML.TPEMISS = "1"
            XML.XSERV = "STATUS"

            CONFIGURACAO.SERVICO = 0

            ' PARA CERTIFICADOS INSTALADOS , A SENHA NãO SERá TESTADA.
            CONFIGURACAO.CERTIFICADOSENHA = SENHAPFX.TEXT

            CONFIGURACAO.CERTIFICADODIGITAL = OCERTIFICADO
      
  TRY    
            IF val(TIPONF.text) = 55 THEN
                AUTORIZACAO = NEW UNIMAKE.BUSINESS.DFE.SERVICOS.NFE.STATUSSERVICO(XML, CONFIGURACAO)
            ELSE
                AUTORIZACAO = NEW UNIMAKE.BUSINESS.DFE.SERVICOS.NFCE.STATUSSERVICO(XML, CONFIGURACAO)
            END IF
 
            AUTORIZACAO.EXECUTAR
 Catch EX As Exception
  '          Mensagem(EX.ToString)
             GravarArquivoSequencial(trim(Certo.text) & "EstadoServico.err", ex.tostring)
             Mensagem("Problema na Autorizacao. Possivel Erro na SEFAZ.")
           EXIT SUB
END TRY     
         
'            IF FEITO.text = 2 THEN
'             MENSAGEM(AUTORIZACAO.RETORNOWSSTRING)
              MENSAGEM(AUTORIZACAO.RESULT.CSTAT & " - " & AUTORIZACAO.RESULT.XMOTIVO)
              exit sub
 '           END IF
             

        END IF

    END SUB
Public Sub GravarArquivoSequencial(Local as string , Mensagem as string)

         Dim Hfile
         Hfile = FreeFile()
         FileOpen(Hfile, trim(Local) , OpenMode.Output)
         Print(Hfile, Trim(Mensagem))
         FileClose(Hfile) 

End Sub 
   
    PUBLIC SUB CERTIFICADOINSTALADO()

'  Nao testado.  Nao se consegue selecionar o Certificado. A funcao de Selecionar "roda" apenas na Console.
     
       CertificadoValido.text = false
  

'dim certificado = new CertificadoDigital();
'dim certificadoSelecionado = certificado.AbrirTelaSelecao()

 DIM STORE AS X509STORE = NEW X509STORE("MY", STORELOCATION.CURRENTUSER)

        STORE.OPEN(OPENFLAGS.READONLY OR OPENFLAGS.OPENEXISTINGONLY)

        DIM COLLECTION AS X509CERTIFICATE2COLLECTION = NEW X509CERTIFICATE2COLLECTION(STORE.CERTIFICATES)
        ' SELECIONA OS CERTIFICADOS DENTRO DAS DATAS DE VALIDADE
        DIM COLLECTION1 AS X509CERTIFICATE2COLLECTION = NEW X509CERTIFICATE2COLLECTION(COLLECTION.FIND(X509FINDTYPE.FINDBYTIMEVALID, DATETIME.NOW, FALSE))

        '  DIM COLLECTION2 AS X509CERTIFICATE2COLLECTION = NEW X509CERTIFICATE2COLLECTION(COLLECTION.FIND(X509FINDTYPE.FINDBYKEYUSAGE, X509KEYUSAGEFLAGS.DIGITALSIGNATURE, FALSE))
        ' DIM COLLECTION3 AS X509CERTIFICATE2COLLECTION = NEW X509CERTIFICATE2COLLECTION(COLLECTION.FIND(X509FINDTYPE.FINDBYISSUERNAME, "MASIMO IND E COM DE ART DE VESTUARIO LTDA", FALSE))
        DIM SCOLLECTION AS X509CERTIFICATE2COLLECTION
        IF FEITO.text = 2 THEN
            SCOLLECTION = X509CERTIFICATE2UI.SELECTFROMCOLLECTION(COLLECTION1, "CERTIFICADO(S) DIGITAL(IS) DISPONíVEL(IS)", "SELECIONE O CERTIFICADO DIGITAL PARA USO NO APLICATIVO", X509SELECTIONFLAG.SINGLESELECTION)
            '  DIM SCOLLECTION AS X509CERTIFICATE2COLLECTION = X509CERTIFICATE2UI.SELECTFROMCOLLECTION(COLLECTION1, "CERTIFICADO(S) DIGITAL(IS) DISPONíVEL(IS)", "SELECIONE O CERTIFICADO DIGITAL PARA USO NO APLICATIVO", X509SELECTIONFLAG.SINGLESELECTION)
        ELSE
            SCOLLECTION = COLLECTION1
        END IF
        IF (SCOLLECTION.COUNT = 0) THEN

            DIM MSGRESULTADO AS STRING = "NENHUM CERTIFICADO DIGITAL FOI SELECIONADO OU O CERTIFICADO SELECIONADO ESTÁ COM PROBLEMAS."
            metext.text = msgresultado     
            MEnsagem(MSGRESULTADO)
            Exit Sub
        ELSE

            OX509CERT = SCOLLECTION(0)
            OCERTIFICADO = OX509CERT


            DIM DATAINICIAL AS STRING = OCERTIFICADO.NOTBEFORE
            DIM DATAFINAL AS STRING = OCERTIFICADO.NOTAFTER



            metext.text = "CERTIFICADO INSTALADO  INICIO : " & DATAINICIAL & "  TÉRMINO : " & DATAFINAL

            '  EXIBIR DADOS DO CERTIFICADO SELECIONADO



            '  DIM CAMPO AS STRING = "DATA INICIAL : " & OCERTIFICADO.NOTBEFORE & " DATA FINAL : " & OCERTIFICADO.NOTAFTER & ENVIRONMENT.NEWLINE
            ' CAMPO = CAMPO & "CERTIFICATE VERIFIED ?  " & OCERTIFICADO.VERIFY() & ENVIRONMENT.NEWLINE
            'CAMPO = CAMPO & "SIMPLE NAME :  " & OCERTIFICADO.GETNAMEINFO(X509NAMETYPE.SIMPLENAME, TRUE) & ENVIRONMENT.NEWLINE
            'CAMPO = CAMPO & "SIGNATURE ALGORITHM : " & OCERTIFICADO.SIGNATUREALGORITHM.FRIENDLYNAME & ENVIRONMENT.NEWLINE
            'CAMPO = CAMPO & "PUBLIC KEY: " & OCERTIFICADO.PUBLICKEY.KEY.TOXMLSTRING(FALSE) & ENVIRONMENT.NEWLINE
            'CAMPO = CAMPO & "CERTIFICATE ARCHIVED ?  " & OCERTIFICADO.ARCHIVED & ENVIRONMENT.NEWLINE
            'CAMPO = CAMPO & "LENGTH OF RAW DATA : " & OCERTIFICADO.RAWDATA.LENGTH & ENVIRONMENT.NEWLINE
            '  Mensagem(CAMPO)
            '      X509CERTIFICATE2UI.DISPLAYCERTIFICATE(OCERTIFICADO)


            CertificadoValido.text = TRUE
            STORE.CLOSE()
        END IF

    END SUB

 
    PUBLIC SUB CERTIFICADOPFX()
            ' Acessar Certificado A1 , direto do arquivo

        DIM HFILE
        HFILE = FREEFILE()

      '  Pathe.text = "C:\inetpub\vhosts\sabra.com.br\httpdocs\mpnsabra\"

        CERTIFICADOVALIDO.text = FALSE

        IF  trim(LOCALPFX.text) = "" THEN
            Metext.text = "Informe o Nome do Arquivo do Certificado a Ser Utilizado."
            Mensagem("Informe o Nome do Arquivo do Certificado a Ser Utilizado.")
            EXIT SUB
        END IF

        DIM Arquivado as String = trim(pathe.text) & trim(LocalPFX.Text)

        DIM XX AS STRING = DIR(trim(arquivado))
        IF TRIM(XX) = "" THEN
            Metext.text = "Arquivo " & arquivado & " Não Encontrado. Verifique."
            Mensagem("Arquivo " & arquivado & " Não Encontrado. Verifique.")
            EXIT SUB
        END IF

        
        DIM SELCERTIFICADO = new UNIMAKE.SECURITY.PLATFORM.CERTIFICADODIGITAL

'        DIM CertificadoByte() as Byte = SelCertificado.ToByteArray(trim(Arquivado))
'        DIM StringCertificado as System.Security.Cryptography.X509Certificates.X509Certificate2 = SelCertificado.ToByteArray(trim(Arquivado))

   

        DIM STRINGCERTIFICADO = SELCERTIFICADO.CARREGARCERTIFICADODIGITALA1(Trim(Arquivado), TRIM(SENHAPFX.text),X509KeyStorageFlags.MachineKeySet)
  
  
        ' FIZ DESTA FORMA PARA TESTAR A LEITURA DOS DADOS DO CERTIFICADO
        ' PODE SER FEITO LENDO DIRETO O CERTIFICADO

        '  DIM DATAINICIAL AS STRING =  STRINGCERTIFICADO.NOTBEFORE
        '  DIM DATAFINAL AS STRING =  STRINGCERTIFICADO.NOTAFTER

        DIM INICIO AS DOUBLE = STRINGS.INSTR(UCASE(STRINGCERTIFICADO.TOSTRING), "[NOT BEFORE]")

        IF INICIO = 0 THEN
            Metext.text = "NâO ENCONTRADA A DATA DE VALIDADE INICIAL DO CERTIFICADO."
            EXIT SUB
        END IF

        WHILE MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "]"
            INICIO = INICIO + 1
        END WHILE

        INICIO = INICIO + 1

        WHILE TRIM(MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)) <> "0" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "1" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "2" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "3" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "4" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "5" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "6" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "7" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "8" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "9"

            INICIO = INICIO + 1
        END WHILE



        DIM DATAINICIO AS STRING = ""

        WHILE TRIM(MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)) <> ""
            DATAINICIO = DATAINICIO & MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)
            INICIO = INICIO + 1
        END WHILE


        INICIO = STRINGS.INSTR(UCASE(STRINGCERTIFICADO.TOSTRING), "[NOT AFTER]")

        IF INICIO = 0 THEN
            Metext.text = "NãO ENCONTRADA A DATA FINAL DE VALIDADE DO CERTIFICADO"
            EXIT SUB
        END IF


        WHILE MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "]"
            INICIO = INICIO + 1
        END WHILE

        INICIO = INICIO + 1

        WHILE TRIM(MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)) <> "0" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "1" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "2" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "3" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "4" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "5" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "6" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "7" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "8" _
            AND MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1) <> "9"

            INICIO = INICIO + 1
        END WHILE



        DIM DATAFINAL AS STRING = ""

        WHILE TRIM(MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)) <> ""
            DATAFINAL = DATAFINAL & MID(STRINGCERTIFICADO.TOSTRING, INICIO, 1)
            INICIO = INICIO + 1
        END WHILE

        IF TROCADATA(DATAINICIO) > TROCADATA(DATADOMICRO) THEN
            metext.text = "CERTIFICADO AINDA NãO VáLIDO."
            EXIT SUB
        END IF
        IF TROCADATA(DATAFINAL) < TROCADATA(DATADOMICRO) THEN
            Metext.text = "DATA DE VALIDADE DO CERTIFICADO EXPIRADA."
            EXIT SUB
        END IF


        OCERTIFICADO = STRINGCERTIFICADO


        DIM DATAINICIAL AS STRING = OCERTIFICADO.NOTBEFORE
        DIM DFINAL AS STRING = OCERTIFICADO.NOTAFTER

        metext.text = "CERTIFICADO .PFX  INICIO : " & DATAINICIAL & "  TERMINO : " & DFINAL

        CERTIFICADOVALIDO.text = TRUE

    END SUB

Public Sub InutilizarNumero()

        '    * ---------------------------------------------------------------------------------
        '* Consumindo o serviço de inutilização de números da NFe
        '* ---------------------------------------------------------------------------------

        SelecionarCertificado()

        Dim InutNFe = New Unimake.Business.DFe.Xml.NFe.InutNFe
        Dim InutNFeInfInut = New Unimake.Business.DFe.Xml.NFe.InutNFeInfInut

        InutNFeInfInut.Ano = "22"
        InutNFeInfInut.CNPJ = Preencher(cnpj.text, "14")
        InutNFeInfInut.CUF = CUF.text
        InutNFeInfInut.Mod = TipoNF.text
        InutNFeInfInut.NNFIni = "10503"
        InutNFeInfInut.NNFFin = "10503"
        InutNFeInfInut.Serie = Serie.text
        InutNFeInfInut.TpAmb = TPAmb.text
        InutNFeInfInut.XJust = "Justificativa da inutilizacao de teste"
        InutNFe.Versao = Versao.text

        InutNFe.InfInut = InutNFeInfInut

        '* Consumir o serviço

        Try        

        If val(TipoNF.text) = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Inutilizacao(InutNFe, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Inutilizacao(InutNFe, Configuracao)
        End If

        Autorizacao.Executar()

        Catch EX As Exception
        Mensagem("Problema na Autorização. Possível Erro na SEFAZ.")
           ' Mensagem(EX.ToString)
            EXIT SUB
       End Try 


       
        '* 102 Inutilizacao homologada

        If Autorizacao.result.InfInut.CStat = 102 Then
            Autorizacao.GravarXmlDistribuicao(Trim(Certo.text))
        Else
            ' Erro na Inutilização
            Autorizacao.GravarXmlDistribuicao(Trim(Errado.text))
        End If

 'Mensagem(Autorizacao.RetornoWSString)
        Mensagem(Autorizacao.result.InfInut.CStat & " - " & Autorizacao.result.InfInut.XMotivo)


    End Sub

 Public Sub CancelarNFe()


       SELECIONARCERTIFICADO()
       configurar

if CertificadoValido.text = true then

' Consulta Situacao da NF para pegar o Protocolo
    
 Dim Xml = New Unimake.Business.DFe.Xml.NFe.ConsSitNFe

        Xml.versao = Versao.text
        Xml.TpAmb = TPAmb.text
        Xml.ChNfe = Trim(ChaveNota.text)
        
        If val(TipoNF.text) = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo(Xml, Configuracao)
        End If
   Try
        Autorizacao.Executar

       If Autorizacao.result.cstat = 100 then   
        Protoca.text = Autorizacao.Result.ProtNFE.InfProt.nProt
        else
        Mensagem("Nota Fiscal Nao Autorizada ou Ja Cancelada. Cancelamento Nao Efetuado.")
        Exit Sub
       End If
       Catch ex as exception
        mensagem("Erro na Consulta do Protocolo. " & Autorizacao.result.cstat & " - " & Autorizacao.Result.XMotivo) 
      Exit Sub
   
End Try


    Dim EnvEvento = New Unimake.Business.DFe.Xml.NFe.EnvEvento
        EnvEvento.Versao = "1.00"
        EnvEvento.IdLote = "000000000000001"

        Dim Evento = New Unimake.Business.DFe.Xml.NFe.Evento
        Evento.Versao = "1.00"

        Dim DetEventoCanc = New Unimake.Business.DFe.Xml.NFe.DetEventoCanc

        With DetEventoCanc
            .Versao = "1.00"
            .NProt = Trim(Protoca.text)
            .XJust = "Justificativa para cancelamento da NFe de teste"
            .DescEvento = "Cancelamento"
        End With

        Dim InfEvento = New Unimake.Business.DFe.Xml.NFe.InfEvento
        InfEvento.DetEvento = DetEventoCanc

        With InfEvento

            .COrgao = 33
            .ChNFe = trim(ChaveNota.text)
            .CNPJ = Preencher(cnpj.text, "14")
            .DhEvento = DateTime.Now
            .TpEvento = 110111
            .NSeqEvento = 1
            .VerEvento = "1.00"
            .TpAmb = TPAmb.text

        End With

        Evento.InfEvento = InfEvento


        ' 060422
        ' Dim Jura As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Evento)
        ' Jura.Add(Evento)
        ' EnvEvento.Evento = Jura

        EnvEvento.Evento.Add(Evento)

  
        Try
           
            If val(TipoNF.text) = 55 Then
                Autorizacao = New Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(EnvEvento, Configuracao)
            Else
                Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento(EnvEvento, Configuracao)
            End If

            Autorizacao.Executar

            '  Gravar o XML de distribuição se a inutilização foi homologada
            If Autorizacao.result.CStat = 128 Then ''128 = Lote de evento processado com sucesso
                Dim CStatr As String = Autorizacao.Result.RetEvento(0).InfEvento.CStat


                '' 135: Evento homologado com vinculação da respectiva NFe
                '' 136: Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
                '' 155: Evento de Cancelamento homologado fora do prazo permitido para cancelamento


                If CStatr = 135 Or CStatr = 136 Or CStatr = 155 Then
                    Autorizacao.GravarXmlDistribuicao(Trim(Certo.text))
                Else ''Evento rejeitado
                    Autorizacao.GravarXmlDistribuicao(trim(Errado.text))
                 End If

           
            Mensagem(Autorizacao.Result.RetEvento(0).InfEvento.CStat & " - " & Autorizacao.Result.RetEvento(0).InfEvento.XMotivo)
            Exit Sub

            Else
                Mensagem("Lote não processado. Poss�vel Problema na SEFAZ.")
            Exit Sub
            End If

        Catch EX As Exception
            'Mensagem(EX.ToString)
            
             gravararquivosequencial(trim(certo.text) & "ID" & trim(chavenota.text) & ".err" , ex.tostring)
 
             Mensagem("Problema na Autorização. Possivel Erro na SEFAZ.")        
             Exit Sub
        End Try
    End If

    End Sub


Public SUB BUTTON1_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON1.CLICK
       CONSULTARSTATUSSERVICO()
   END SUB

Public SUB BUTTON2_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON2.CLICK
        InutilizarNumero()
    END SUB

Public SUB BUTTON3_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON3.CLICK
       CANCELARNFE()
END SUB

Public Sub MPNSabra_Shown()
        IF FEITO.text = 0 THEN
      
            FEITO.text = 1

            LERARQUIVOINI("MPNSABRA.INI")

       Certo.text = trim(pathe.text) & "\retorno\"
       Errado.text = trim(pathe.text) & "\erro\"    

            SelecionarCertificado()
        end if
End Sub

Public Sub SelecionarCertificado()

        ' INICIALIZA A CONFIGURAçãO

        IF TIPONF.text = 55 THEN
            CONFIGURACAO.TIPODFE = TIPODFE.NFE
            
        ELSE
            CONFIGURACAO.TIPODFE = TIPODFE.NFCE
            
        END IF

        CONFIGURACAO.CODIGOUF = CUF.text
        CONFIGURACAO.SERVICO = 0

        CONFIGURACAO.CERTIFICADOSENHA = TRIM(SENHAPFX.text)


            IF TRIM(LOCALPFX.text) = "" THEN
            CERTIFICADOINSTALADO()
        ELSE
            ' LER CERTIFICADO EM ARQUIVO .PFX
            CONFIGURACAO.CERTIFICADOARQUIVO = Server.MapPath(TRIM(LOCALPFX.text))
            CERTIFICADOPFX()
            CONFIGURACAO.CERTIFICADODIGITAL = OCERTIFICADO
            CONFIGURACAO.CERTIFICADOARQUIVO = Server.MapPath(TRIM(LOCALPFX.text))

        END IF

        '  DIM DATAINICIAL AS STRING =  STRINGCERTIFICADO.NOTBEFORE
        '  DIM DATAFINAL AS STRING =  STRINGCERTIFICADO.NOTAFTER
        feito.text = "2"

    END SUB
Public SUB BUTTON4_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON4.CLICK
Dim Texto as string = "mpnsabra.autoriza.aspx?CertificadoValido=" & CertificadoValido.text & "&Pathe=" & trim(Pathe.text)
Texto = Texto & "&LocalPFX=" & trim(LocalPfx.text)
Texto = texto & "&SenhaPFX=" & SenhaPFX.text
Texto = Texto & "&TokenCSC=" & ToKenCSC.text 
Texto = Texto & "&IdentCSC=" & IdentCSC.Text
Texto = Texto & "&TipoNF=" & TipoNF.text
Texto = Texto &  "&ChaveNota=" & trim(ChaveNota.text) & "&Versao=" & Versao.text & "&CodigoOperador=" & CodigoOperador.text & "&Qual=Sim"	
Response.Redirect (Texto)
END SUB

Public SUB BUTTON5_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON5.CLICK
       ContaGrava.text = "0"       
       EnviarCamposNFE()
END SUB

Public SUB BUTTON6_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON6.CLICK
        ENVIAREVENTOCCE()
    END SUB


Public SUB BUTTON7_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON7.CLICK
      CONSULTARSITUACAONF()
END SUB

Public SUB BUTTON8_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON8.CLICK
Dim Texto as string = "mpnsabra.consultarGTIN.aspx?Pathe=" & trim(Pathe.text)
Texto = Texto & "&LocalPFX=" & trim(LocalPfx.text)
Texto = texto & "&SenhaPFX=" & SenhaPFX.text
Texto = Texto & "&TokenCSC=" & ToKenCSC.text 
Texto = Texto & "&IdentCSC=" & IdentCSC.Text
Texto = Texto & "&TipoNF=" & TipoNF.text
Texto = Texto &  "&Versao=" & Versao.text & "&CodigoOperador=" & CodigoOperador.text & "&Qual=Sim"	
Response.Redirect (Texto)
END SUB																											

Public SUB BUTTON9_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON9.CLICK
	Dim Texto as string = "mpnsabra.ConsultarDistribuicao.aspx?Pathe=" & trim(Pathe.text)
Texto = Texto & "&LocalPFX=" & trim(LocalPfx.text)
Texto = texto & "&SenhaPFX=" & SenhaPFX.text
Texto = Texto & "&TokenCSC=" & ToKenCSC.text 
Texto = Texto & "&IdentCSC=" & IdentCSC.Text
Texto = Texto & "&CNPJ=" & CNPJ.text
Texto = Texto & "&TPAmb=" & TPAmb.text																										
Texto = Texto &  "&CUF=" & CUF.text & "&CodigoOperador=" & CodigoOperador.text & "&Qual=Sim"	
Response.Redirect (Texto)
END SUB	
																											
Public SUB BUTTON10_CLICK(SENDER AS OBJECT, E AS EVENTARGS) HANDLES BUTTON10.CLICK
Dim Texto as string = "mpnsabra.Manifestacao.aspx?Pathe=" & trim(Pathe.text)
Texto = Texto & "&LocalPFX=" & trim(LocalPfx.text)
Texto = texto & "&SenhaPFX=" & SenhaPFX.text
Texto = Texto & "&CHNota=" & ChaveNota.text 
Texto = Texto & "&CNPJ=" & CNPJ.text
Texto = Texto & "&TPAmb=" & TPAmb.text																										
Texto = Texto &  "&CUF=" & CUF.text & "&CodigoOperador=" & CodigoOperador.text & "&Qual=Sim"	
Response.Redirect (Texto)
END SUB	

  
       Public Sub MostrandoMensagem(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs)
       MostraMensagem.visible = false
   
       End Sub

    Sub Mensagem(Byval Texto as string)
        TextoErro = Texto
        if trim(natal.text) = "" then

        MensagemBox
        else
        Mostrou.text = textoerro
        MostraMensagem.visible = true
        End If
        
    End Sub
    Sub MensagemBox()
        'termina o processamento do servidor e retorna ao cliente
        Dim strScript As String = "<script language=JavaScript>"
        strScript += "alert(""" & TextoErro & """);"
        strScript += "</script>"

        If (Not ClientScript.IsStartupScriptRegistered("clientScript")) Then
            ClientScript.RegisterClientScriptBlock(Me.GetType(), "clientScript", strScript)
        End If

    End Sub

    

    PUBLIC SUB LERARQUIVOINI(BYVAL ARQUIVO AS STRING)

        ' LER O ARQUIVO DE INICIALIZACAO E PARAMETROS DO SISTEMA
        ' ATENTAR PARA O FORMATO DO ARQUIVO , BEM COMO POR SEUS CAMPOS.
        ' ' MODELO DEFAULT DA NF FISCAL   55 OU 65           '
        ' *[TIPONF  ]55                                      *
        ' TIPO DE IMPRESSAO EMISSAO DA NF                    '
        ' 0-SEM  1-RETRATO 2 - PAISAGEM  3 -                 '
        ' 4 NFC  5-EMAIL   6 -           7 -                 '
        ' [TIPOIMP ]1                                        *

        DIM HFILE = FREEFILE()
        If (File.Exists(Server.MapPath(trim(arquivo)))) = false Then
            Mensagem("N?o Encontrado o arquivo " & trim(arquivo) & ". Programa Encerrado.") 
            Exit Sub
        End If
 
        FILEOPEN(HFILE, server.MapPath(TRIM(ARQUIVO)), OPENMODE.INPUT)

        DIM BUFFER AS STRING = ""

        WHILE NOT EOF(HFILE)
            BUFFER = LINEINPUT(HFILE)

            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CUF" THEN
                CUF.text = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "SERIE" THEN
                SERIE.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPFX" THEN
                LOCALPFX.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPF1" THEN
                LOCALPFX.text = TRIM(LOCALPFX.TEXT) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPF2" THEN
                LOCALPFX.text = TRIM(LOCALPFX.TEXT) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE" THEN
                PATHE.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE1" THEN
                PATHE.text = TRIM(PATHE.TEXT) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE2" THEN
                PATHE.text = TRIM(PATHE.TEXT) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "SENHAPFX" THEN
                SENHAPFX.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CNPJ" THEN
                CNPJ.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TIPONF" THEN
               TIPONF.text = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TPAMB" THEN
                TPAMB.text = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "VERSAO" THEN
                VERSAO.text = TRIM(MID(BUFFER, 12, 40))
            END IF
           
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "IDENTCSC" THEN
                IDENTCSC.text = TRIM(MID(BUFFER, 12, 40))
            END IF
            
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TOKENCSC" THEN
                TOKENCSC.text = TRIM(MID(BUFFER, 12, 40))
           END IF
           IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CHAVENOT" THEN
                CHAVENOTA.text = TRIM(MID(BUFFER, 12, 40))
           END IF
           IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CHAVENO1" THEN
                CHAVENOTA.text = TRIM(CHAVENOTA.TEXT) & TRIM(MID(BUFFER, 12, 40))
           END IF
        END WHILE
        FILECLOSE(HFILE)


    END SUB


    PUBLIC SUB CONFIGURAR()


        CONFIGURACAO.CODIGOUF = CUF.text
        CONFIGURACAO.SERVICO = 0
        CONFIGURACAO.CERTIFICADOSENHA = TRIM(SENHAPFX.text)
        CONFIGURACAO.CERTIFICADODIGITAL = OCERTIFICADO
        CONFIGURACAO.CERTIFICADOARQUIVO = Server.MapPath(TRIM(LOCALPFX.text))
        CONFIGURACAO.TIPOEMISSAO = TIPOEMISSAO.NORMAL
        CONFIGURACAO.TIPOAMBIENTE = TPAMB.text
        CONFIGURACAO.SCHEMAVERSAO = VERSAO.text
        IF TIPONF.text = 55 THEN
            CONFIGURACAO.MODELO = MODELODFE.NFE
            CONFIGURACAO.TIPODFE = TIPODFE.NFE
        ELSE
            CONFIGURACAO.MODELO = MODELODFE.NFCE
            CONFIGURACAO.TIPODFE = TIPODFE.NFCE
            CONFIGURACAO.CSC = TOKENCSC.TEXT
            CONFIGURACAO.CSCIDTOKEN = IDENTCSC.TEXT
        END IF
        ' CONFIGURACAO.TPEMISS = "1"
        '  CONFIGURACAO.XSERV = "STATUS"
    END SUB
Function Preencher(ByVal Campo As String, ByVal DIGITOS As String) As String
        Dim x As Short
        Dim Z As Short
        Dim y As String = ""
        Dim Linha1 As String
        x = 1
        Linha1 = Trim(Campo)
        Z = Len(Trim(Linha1))
        While x <= Z
            If Mid(Linha1, x, 1) = "0" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "1" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "2" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "3" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "4" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "5" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "6" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "7" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "8" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "9" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            x = x + 1
        End While
        x = Val(DIGITOS)
        Z = Len(Trim(y))
        If x > Z Then
            Z = x
        End If
        Linha1 = Mid(y, Z - x + 1, x)
        Linha1 = New String("0", x - Len(Trim(Linha1))) & Trim(Linha1)
   '     TextoLinha1 = Trim(Linha1)
        Preencher = Trim(Linha1)

    End Function
PUBLIC FUNCTION TROCADATA(BYVAL TEXTE AS STRING) AS STRING
        ' INVERTE A DATA DESEJADA  DE  DDMMAA PARA AAMMDD E VICE-VERSA
        DIM TEXTO AS STRING
        DIM POSSO AS STRING
        TEXTO = PREENCHER(TRIM(TEXTE), "06")
        POSSO = MID(TEXTO, 5, 2) & MID(TEXTO, 3, 2) & MID(TEXTO, 1, 2)
        TROCADATA = POSSO
    END FUNCTION
Function DataDoMicro() As String
        Dim Dia As String
        Dim Mes As String
        Dim Ano As String
        Dim Ana As String

        Dia = Preencher(Trim(CStr(Day(Today))), "02")
        Mes = Preencher(Trim(CStr(Month(Today))), "02")
        Ana = CStr(Year(Today))

        If Len(Trim(Ana)) > 2 Then
            Ano = Preencher(Mid(Ana, 3, 2), "02")
        Else
            Ano = Preencher(Mid(Ana, 1, 2), "02")
        End If
        DataHoje = Dia & "/" & Mes & "/" & Ano
        HOJE = Dia & Mes & Ano
        DataDoMicro = Dia & Mes & Ano



    End Function

End Class
