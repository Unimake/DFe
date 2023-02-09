<%@ Page  Language="VB" Debug="true" %>
<%@ Import Namespace="System" %>
<%@ Import Namespace="System.Data" %>
<%@ Import Namespace="System.Data.OleDb" %>
<%@ Import NameSpace="System.Web.Mail"%>                                                      
<%@ Import Namespace="System.Net.Mail" %>                                                      
<%@ Import Namespace="System.IO" %>
<%@ IMPORT Namespace="SYSTEM.NET" %>
<%@ IMPORT Namespace="SYSTEM.SECURITY" %>
<%@ IMPORT Namespace="SYSTEM.SECURITY.CRYPTOGRAPHY" %>
<%@ IMPORT Namespace="SYSTEM.SECURITY.CRYPTOGRAPHY.X509CERTIFICATES" %>
<%@ IMPORT Namespace="SYSTEM.SECURITY.CRYPTOGRAPHY.XML" %>
<%@ IMPORT Namespace="SYSTEM.COLLECTIONS.GENERIC" %>
<%@ IMPORT Namespace="SYSTEM.TEXT" %>
<%@ IMPORT Namespace="SYSTEM.XML" %>
<%@ IMPORT Namespace="SYSTEM.WINDOWS.FORMS" %>
<%@ IMPORT Namespace="MICROSOFT.WIN32" %>
<%@ IMPORT Namespace="UNIMAKE.BUSINESS.DFE" %>
<%@ IMPORT Namespace="UNIMAKE.BUSINESS.DFE.SERVICOS" %>

<%@ IMPORT Namespace="UNIMAKE.CRIPTOGRAPHY" %>
<%@ IMPORT Namespace="UNIMAKE.SECURITY.PLATFORM" %>
<%@ IMPORT Namespace="UNIMAKE.SECURITY.EXCEPTIONS" %>
<%@ IMPORT Namespace="UNIMAKE.UTILS" %>

<html>
<head>
<title>Autorizacao de NFe e NFCe</title>
</head>


<script language="VB" runat="server">

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
                  
    PUBLIC CONFIGURACAO = NEW UNIMAKE.BUSINESS.DFE.SERVICOS.CONFIGURACAO
    
	Public Testando = New X509Certificate2()
    PUBLIC OX509CERT AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()
    PUBLIC OCERTIFICADO AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()
    PUBLIC SEMCERTIFICADO AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()

    PUBLIC DATAHOJE AS STRING
    PUBLIC HOJE AS STRING
    PUBLIC AUTORIZACAO
    Public ArquivoOrigem as string
    Public Versao as string
    Public Pathe as string
    Public ChaveNota as string
    Public CertificadoValido as Boolean
    Public LocalPFX as string
    Public SenhaPFX as string
    Public CUF as string
    Public TipoNF as string
    Public TextoErro as string
    Public Natal as string
    Public TokenCSC as string
    Public IdentCSC as string
	Public Serie as string
	Public CNPJ as string
	Public TpAmb as string
	Public Retorna as string = "mpnsabra.aspx"
Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
    
If (Not Page.IsPostBack) Then
 
  EnviarXMLNFe()

End If

 End Sub
    
    Public Sub EnviarXMLNFe()

        'Enviar NFE. Usando o .XML previamente gerado.


Versao = Request.QueryString("Versao")
If trim(Versao) = "" then
   Versao  = Request.form("Versao")
End If
Pathe = Request.QueryString("Pathe")
If trim(Pathe) = "" then
   Pathe  = Request.form("Pathe")
End If
ChaveNota = Request.QueryString("ChaveNota")
If trim(ChaveNota) = "" then
   ChaveNota  = Request.form("ChaveNota")
End If
'CertificadoValido = Request.QueryString("CertificadoValido")
'If CertificadoValido <> False And CertificadoValido <> true then
'   CertificadoValido  = Request.form("CertificadoValido")
'End If
LocalPFX = Request.QueryString("LocalPFX")
If trim(LocalPFX) = "" then
   LocalPFX  = Request.form("LocalPFX")
End If

SenhaPFX = Request.QueryString("SenhaPFX")
If trim(SenhaPFX) = "" then
   SenhaPFX  = Request.form("SenhaPFX")
End If
'Configuracao = Request.QueryString("Configuracao")
'If trim(Configuracao) = "" then
'   Configuracao  = Request.form("Configuracao")
'End If
'Testando = Request.QueryString("oCertificado")
'If trim(Testando) = "" then
'   Testando  = Request.form("oCertificado")
'End If

TipoNF = Request.QueryString("TipoNF")
If trim(TipoNF) = "" then
   TipoNF  = Request.form("TipoNF")
End If


TokenCSC = Request.QueryString("TokenCSC")
If trim(TokenCSC) = "" then
   TokenCSC  = Request.form("TokenCSC")
End If

IdentCSC = Request.QueryString("IdentCSC")
If trim(IdentCSC) = "" then
   IdentCSC  = Request.form("IdentCSC")
End If

        SelecionarCertificado()
  
       If CertificadoValido = False  then
          Metext.text = "Certificado Não Valido. Verifique !" 
          Exit Sub  
       End If

        Configurar()

Dim Certo as string = trim(Pathe) & "Retorno\"
Dim Errado as string = trim(Pathe) & "Erro\"

       ArquivoOrigem = TRIM(PATHE) & "envio\" & Trim(ChaveNota) & "-NFE.xml" 


        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe
        '
        Dim Doc = New System.Xml.XmlDocument

        Doc.load(TRIM(ArquivoOrigem))

        With Xml
            .IdLote = "000000000000021"
            .IndSinc = 1
            .Versao = Versao

        End With

        Dim Jura6 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.NFe)
        Xml.NFe = Jura6


        Try

            Xml.NFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.NFe)(Doc))

        Catch ex As Exception
            GravarArquivoSequencial(trim(certo) & "NFe" & trim(ChaveNota) & ".err" , ex.ToString)    
            MEtext.text ="Erro ao Deserializar o XML da NFe."
            EXIT SUB
        End Try


        Try
            
            If val(TipoNF) = 55 Then
                Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Autorizacao(Xml, Configuracao)
            Else
                Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Autorizacao(Xml, Configuracao)
            End If
     
            Autorizacao.Executar
          
            Catch ex As Exception
         
               GravarArquivoSequencial(trim(certo) & "NFe" & trim(ChaveNota) & ".err" , ex.ToString)
               Metext.text ="Erro na Autorizacao ou Possivel Problema na SEFAZ"
               Exit Sub

        End Try
            
        If Autorizacao.result.CStat <> 104 Then ''104 = Lote de evento processado com sucesso
           
             metext.text = Autorizacao.result.CStat & " - " & Autorizacao.result.xmotivo
       '      mensagem("Lote Nao Processado. Possivel Problema na SEFAZ.")
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
                Autorizacao.GravarXmlDistribuicao(trim(Certo))
               ' metext.text = CStato & " - " & Autorizacao.Result.XMotivo 
	            Response.Redirect (Retorna)
	            Exit Sub           
           End IF
 
               ' ' Gerar o Objeto para pegar a string com o nome do arquivo e gravar em banco de dados

               ' Dim docProcNFe As System.Xml.XmlDocument
               '  docProcNFe = Autorizacao.NfeProcResult.GerarXML()
               ' mostra o nome do arquivo gerado.  Tirar quando Producao
               'Mensagem(Autorizacao.NfeProcResult.NomeArquivoDistribuicao)
               'Exit Sub
             
                       
              Autorizacao.GravarXmlDistribuicao(trim(Errado)) 
    
              MeText.text ="Nota Rejeitada : " & CStato & " - " &  Autorizacao.Result.ProtNFe.InfProt.XMotivo
              
    End Sub
  
 
   
    PUBLIC SUB CERTIFICADOINSTALADO()

'  Nao testado.  Nao se consegue selecionar o Certificado. A funcao de Selecionar "roda" apenas na Console.
     
       CertificadoValido = false
  

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
   '     IF FEITO.text = 2 THEN
            SCOLLECTION = X509CERTIFICATE2UI.SELECTFROMCOLLECTION(COLLECTION1, "CERTIFICADO(S) DIGITAL(IS) DISPONíVEL(IS)", "SELECIONE O CERTIFICADO DIGITAL PARA USO NO APLICATIVO", X509SELECTIONFLAG.SINGLESELECTION)
            '  DIM SCOLLECTION AS X509CERTIFICATE2COLLECTION = X509CERTIFICATE2UI.SELECTFROMCOLLECTION(COLLECTION1, "CERTIFICADO(S) DIGITAL(IS) DISPONíVEL(IS)", "SELECIONE O CERTIFICADO DIGITAL PARA USO NO APLICATIVO", X509SELECTIONFLAG.SINGLESELECTION)
    '    ELSE
     '       SCOLLECTION = COLLECTION1
      '  END IF
        IF (SCOLLECTION.COUNT = 0) THEN

            DIM MSGRESULTADO AS STRING = "NENHUM CERTIFICADO DIGITAL FOI SELECIONADO OU O CERTIFICADO SELECIONADO ESTÁ COM PROBLEMAS."
            metext.text = msgresultado     
          
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


            CertificadoValido = TRUE
            STORE.CLOSE()
        END IF

    END SUB

 
    PUBLIC SUB CERTIFICADOPFX()
            ' Acessar Certificado A1 , direto do arquivo

        DIM HFILE
        HFILE = FREEFILE()

        CERTIFICADOVALIDO = FALSE

        IF  trim(LOCALPFX) = "" THEN
            Metext.text = "Informe o Nome do Arquivo do Certificado a Ser Utilizado."
            EXIT SUB
        END IF

        DIM Arquivado as String = trim(pathe) & trim(LocalPFX)

        DIM XX AS STRING = DIR(trim(arquivado))
        IF TRIM(XX) = "" THEN
            Metext.text = "Arquivo " & arquivado & " Não Encontrado. Verifique."
              EXIT SUB
        END IF

        
        DIM SELCERTIFICADO = new UNIMAKE.SECURITY.PLATFORM.CERTIFICADODIGITAL

'        DIM CertificadoByte() as Byte = SelCertificado.ToByteArray(trim(Arquivado))
'        DIM StringCertificado as System.Security.Cryptography.X509Certificates.X509Certificate2 = SelCertificado.ToByteArray(trim(Arquivado))
  

        DIM STRINGCERTIFICADO = SELCERTIFICADO.CARREGARCERTIFICADODIGITALA1(Trim(Arquivado), TRIM(SENHAPFX),X509KeyStorageFlags.MachineKeySet)
  
  
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

        CERTIFICADOVALIDO = TRUE

    END SUB


Public Sub MPNSabra_Shown()
     

            LERARQUIVOINI("MPNSABRA.INI")

'       Certo.text = trim(pathe.text) & "\retorno\"
 '      Errado.text = trim(pathe.text) & "\erro\"    

            SelecionarCertificado()
        
End Sub

Public Sub SelecionarCertificado()

        ' INICIALIZA A CONFIGURAçãO

        IF val(TIPONF) = 55 THEN
            CONFIGURACAO.TIPODFE = TIPODFE.NFE
            
        ELSE
            CONFIGURACAO.TIPODFE = TIPODFE.NFCE
            
        END IF

        CONFIGURACAO.CODIGOUF = CUF
        CONFIGURACAO.SERVICO = 0

        CONFIGURACAO.CERTIFICADOSENHA = TRIM(SENHAPFX)


            IF TRIM(LOCALPFX) = "" THEN
            CERTIFICADOINSTALADO()
        ELSE
            ' LER CERTIFICADO EM ARQUIVO .PFX
            CONFIGURACAO.CERTIFICADOARQUIVO = Trim(Pathe) & trim(LOCALPFX)
            CERTIFICADOPFX()
            CONFIGURACAO.CERTIFICADODIGITAL = OCERTIFICADO
            CONFIGURACAO.CERTIFICADOARQUIVO = Trim(Pathe) & TRIM(LOCALPFX)

        END IF

        '  DIM DATAINICIAL AS STRING =  STRINGCERTIFICADO.NOTBEFORE
        '  DIM DATAFINAL AS STRING =  STRINGCERTIFICADO.NOTAFTER
       

    END SUB
	
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
            metext.text = "Nao Encontrado o arquivo " & trim(arquivo) & ". Programa Encerrado." 
            Exit Sub
        End If
 
        FILEOPEN(HFILE, server.MapPath(TRIM(ARQUIVO)), OPENMODE.INPUT)

        DIM BUFFER AS STRING = ""

        WHILE NOT EOF(HFILE)
            BUFFER = LINEINPUT(HFILE)

            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CUF" THEN
                CUF = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "SERIE" THEN
                SERIE = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPFX" THEN
                LOCALPFX = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPF1" THEN
                LOCALPFX = TRIM(LOCALPFX) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "LOCALPF2" THEN
                LOCALPFX = TRIM(LOCALPFX) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE" THEN
                PATHE = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE1" THEN
                PATHE = TRIM(PATHE) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "PATHE2" THEN
                PATHE = TRIM(PATHE) & TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "SENHAPFX" THEN
                SENHAPFX = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CNPJ" THEN
                CNPJ = TRIM(MID(BUFFER, 12, 40))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TIPONF" THEN
               TIPONF = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TPAMB" THEN
                TPAMB = VAL(TRIM(MID(BUFFER, 12, 40)))
            END IF
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "VERSAO" THEN
                VERSAO = TRIM(MID(BUFFER, 12, 40))
            END IF
           
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "IDENTCSC" THEN
                IDENTCSC = TRIM(MID(BUFFER, 12, 40))
            END IF
            
            IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "TOKENCSC" THEN
                TOKENCSC = TRIM(MID(BUFFER, 12, 40))
           END IF
           IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CHAVENOT" THEN
                CHAVENOTA = TRIM(MID(BUFFER, 12, 40))
           END IF
           IF UCASE(TRIM(MID(BUFFER, 3, 8))) = "CHAVENO1" THEN
                CHAVENOTA = TRIM(CHAVENOTA) & TRIM(MID(BUFFER, 12, 40))
           END IF
        END WHILE
        FILECLOSE(HFILE)


    END SUB


    PUBLIC SUB CONFIGURAR()


        CONFIGURACAO.CODIGOUF = CUF
        CONFIGURACAO.SERVICO = 0
        CONFIGURACAO.CERTIFICADOSENHA = TRIM(SENHAPFX)
  '      CONFIGURACAO.CERTIFICADODIGITAL = OCERTIFICADO
        CONFIGURACAO.CERTIFICADOARQUIVO = Server.MapPath(TRIM(LOCALPFX))
        CONFIGURACAO.TIPOEMISSAO = TIPOEMISSAO.NORMAL
        CONFIGURACAO.TIPOAMBIENTE = TPAMB
        CONFIGURACAO.SCHEMAVERSAO = VERSAO
        IF val(TIPONF) = 55 THEN
            CONFIGURACAO.MODELO = MODELODFE.NFE
            CONFIGURACAO.TIPODFE = TIPODFE.NFE
        ELSE
            CONFIGURACAO.MODELO = MODELODFE.NFCE
            CONFIGURACAO.TIPODFE = TIPODFE.NFCE
            CONFIGURACAO.CSC = TOKENCSC
            CONFIGURACAO.CSCIDTOKEN = IDENTCSC
        END IF
        ' CONFIGURACAO.TPEMISS = "1"
        '  CONFIGURACAO.XSERV = "STATUS"
    END SUB
	
	
	Public Function Preencher(ByVal Campo As String, ByVal DIGITOS As String) As String
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
Public Function DataDoMicro() As String
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

     Public Sub GravarArquivoSequencial(Local as string , Mensagem as string)
         Dim Hfile
         Hfile = FreeFile()
         FileOpen(Hfile, trim(Local) , OpenMode.Output)
         Print(Hfile, Trim(Mensagem))
         FileClose(Hfile) 
     End Sub 
</script>

<body>

<form method="post"  name="Autorizar" runat="server">
 
<center>
 
 
 <table border="0" BGCOLOR= "red" width="100%">
 <td width="100%" align="center" valign="top">
        <font face=small size=3>Autorizacao de NFe e NFCe</font></td>
 </table>
 <table border="0" BGCOLOR= "#0CCCCC" width="100%">
 <td width="100%">
 <div>
        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>
        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>
        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>




        <TR>
        <TD ALIGN="center" VALIGN="center">
        <Font Size=4>
        <asp:TextBox id="MeText"  ForeColor="Red" runat="server" /></Font></TD>
        </TR>

        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>
        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>
        <TR>
        <TD ALIGN="center" VALIGN="center"></TD></TR>

</div>
</td>
</table>
</center>
</form>
</body>
</html>
