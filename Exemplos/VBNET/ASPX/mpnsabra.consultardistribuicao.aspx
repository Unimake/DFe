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
<%@ IMPORT Namespace="UNIMAKE.BUSINESS.DFE.XML" %>
<%@ IMPORT Namespace="UNIMAKE.CRIPTOGRAPHY" %>
<%@ IMPORT Namespace="UNIMAKE.SECURITY.PLATFORM" %>
<%@ IMPORT Namespace="UNIMAKE.SECURITY.EXCEPTIONS" %>
<%@ IMPORT Namespace="UNIMAKE.UTILS" %>

<html>
<head>
<title>Consultar Distribuição</title>
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
        GTIN = 10
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
    
	
    PUBLIC OX509CERT AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()
    PUBLIC OCERTIFICADO AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()
    PUBLIC SEMCERTIFICADO AS X509CERTIFICATE2 = NEW X509CERTIFICATE2()

    PUBLIC DATAHOJE AS STRING
    PUBLIC HOJE AS STRING
    PUBLIC AUTORIZACAO
    Public ArquivoOrigem as string
    Public Versao as string
    Public Pathe as string
   
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
       ConsultarDistribuicao()
    End If

End Sub
    
Public Sub ConsultarDistribuicao()

CNPJ = Request.QueryString("CNPJ")
If trim(CNPJ) = "" then
   CNPJ  = Request.form("CNPJ")
End If
Pathe = Request.QueryString("Pathe")
If trim(Pathe) = "" then
   Pathe  = Request.form("Pathe")
End If
LocalPFX = Request.QueryString("LocalPFX")
If trim(LocalPFX) = "" then
   LocalPFX  = Request.form("LocalPFX")
End If

SenhaPFX = Request.QueryString("SenhaPFX")
If trim(SenhaPFX) = "" then
   SenhaPFX  = Request.form("SenhaPFX")
End If


TPAmb = Request.QueryString("TPAmb")
If trim(TPAmb) = "" then
   TPAmb  = Request.form("TPAmb")
End If

CUF = Request.QueryString("CUF")
If trim(CUF) = "" then
   CUF  = Request.form("CUF")
End If

TokenCSC = Request.QueryString("TokenCSC")
If trim(TokenCSC) = "" then
   TokenCSC  = Request.form("TokenCSC")
End If

IdentCSC = Request.QueryString("IdentCSC")
If trim(IdentCSC) = "" then
   IdentCSC  = Request.form("IdentCSC")
End If
youtext.text = ""
        SelecionarCertificado()
  
       If CertificadoValido = False  then
          Metext.text = "Certificado Não Valido. Verifique !" 
          Exit Sub  
       End If


       Dim DistDFeInt, DistNSU, DistribuicaoDFe
       Dim NSU     As String = "000000000000000"
       Dim Folder  As String = trim(pathe) & "Distribuicao"  ''<<<altere para uma pasta existente em sua máquina



 DistDFeInt = New Unimake.Business.DFe.xml.NFe.DistDFeInt
 DistNSU = New Unimake.Business.DFe.xml.NFe.DistNSU


Do While True

Try    
    DistNSU.UltNSU = NSU
    
    With DistDFeInt
        .Versao = "1.01"
        .TpAmb = TPAmb
        .CNPJ = preencher(CNPJ,"14")
        .CUFAutor = CUF
        .DistNSU = DistNSU
    End With

 
   DistribuicaoDFe = New Unimake.Business.DFe.Servicos.NFe.DistribuicaoDFe(DistDFeInt , Configuracao)
   DistribuicaoDFe.Executar 
   
   metext.text = DistribuicaoDFE.result.CStat & " - " & DistribuicaoDFe.result.XMotivo
    
    If (DistribuicaoDFe.result.CStat <> 138) Then ''Documentos Nao Localizados
       Exit Do
    End IF
     
    DistribuicaoDFe.GravarXMLDocZIP(folder, True)
    NSU = DistribuicaoDFe.result.UltNSU
    Metext1.text = "Ultimo NSU : " & NSU
    If CInt(DistribuicaoDFe.result.UltNSU) >= CInt(DistribuicaoDFe.result.MaxNSU) Then
       Exit Do
    End If

    Catch EX as Exception
       metext.text = "Erro na Execucao"
       metext1.text = Ex.ToString
       Exit Do
    End Try

Loop
      
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



            youtext.text = "Certificado Instalado   Inicio : " & DATAINICIAL & "  Término : " & DATAFINAL

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

        youtext.text = "Certificado Inicio : " & DATAINICIAL & "  Término : " & DFINAL

        CERTIFICADOVALIDO = TRUE

    END SUB



Public Sub SelecionarCertificado()

        ' INICIALIZA A CONFIGURAçãO
  
        CONFIGURACAO.TIPODFE = TIPODFE.NFe
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

</script>

<body>

<form method="post"  name="Form1" runat="server">
 
<center>
 
 
 <table border="0" BGCOLOR= "red" width="70%">
 <td width="100%" align="center" valign="top">
        <font face=small size=3>Consultar GTIN</font></td>
 </table>
 <table border="0" BGCOLOR= "#0CCCCC" width="70%">
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
        <asp:Label id="Youtext"  ForeColor="Red" runat="server" /></Font></TD>
        </TR>


        <TR>
        <TD ALIGN="center" VALIGN="center">
        <Font Size=4>
        <asp:Label id="Metext"  ForeColor="Red" runat="server" /></Font></TD>
        </TR>
<TR>
        <TD ALIGN="center" VALIGN="center">
        <Font Size=4>
        <asp:Label id="Metext1"  ForeColor="Red" runat="server" /></Font></TD>
        </TR>
	 <TR>
        <TD ALIGN="center" VALIGN="center">
        <Font Size=4>
        <asp:Label id="Metext2"  ForeColor="Red" runat="server" /></Font></TD>
        </TR>
	 <TR>
        <TD ALIGN="center" VALIGN="center">
        <Font Size=4>
        <asp:Label id="Metext3"  ForeColor="Red" runat="server" /></Font></TD>
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
