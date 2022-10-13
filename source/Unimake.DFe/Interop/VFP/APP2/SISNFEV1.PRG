close database
   SET SAFETY OFF
   SET EXCLUSIV ON
   SET EXCLUSIV ON
   CLOSE DATABASE
   SET EXCLUSIV ON
   CLOSE DATABASE
   SET EXCLUSIV ON
*ON SHUTDOWN QUIT
*On Error Do errores With Error( ),Message( ),Message(1),Program( ),Lineno( )

*SELE 11
*USE \\SERVIDOR\C\SISPARIS\TDADOS\MAT
*IF DATA#DATE()
*   DO MAT
*   USE \\SERVIDOR\C\SISPARIS\TDADOS\MAT
*   GOTO TOP
*   REPLACE data WITH DATE()
*   USE
*   MESSAGEBOX("Ok Manutencao Realizada com Sucesso")
*ENDIF   


SET EXCLUSIV OFF
CLOSE DATABASE
PUBLIC CGECASISTEM,drivet
drivet=" "
CHECASISTEM=0
SELECT 11
*//******************************
*IF NOT validar_sistema() 
*    QUIT 
*ENDIF



*////**************************************************************


* Programa Inicio
PUBLIC NOME,CODIGO,VALOR_MAX,VCOD,vos,vnome,VCOD,checa,codatend,nomatend,checa,codempre,v_empre,N_EMPRE,VSERIE,vvempre,vvcodcli,vcodcli,tipo_cli,vvempresa,V_PFRETE,xxtipopg,VVENC,vvdias,TTICMS,VVEMP,vdataf,VENC,Z
PUBLIC vcotaS,RDATA,VCONVIA, W1, W2 , W3, WFINAL
VCONVIA=" "
RDATA=DATE()


codatend=" "
nomatend=" "

W1 = YEAR(DATE())
W2 = DAY(DATE())
W3 = MONTH(DATE())
WFINAL = W1+W2+W3

WFINAL = ALLTRIM(STR(WFINAL))
*WAIT WINDOW WFINAL + '-----------------------------------------------' TIMEOUT 5

*=CAPSLOCK(.T)
*SET PROCEDURE TO C:\SISLOJA\DEFCLASS.PRG ADDITIVE  &&   restore action later
*DO C:\SISLOJA\DEFCLASS                             &&   restore action ...
*SET DEFAULT TO C:\SISLOJA\
SET DATE TO BRITI


vcotaS=" "
ZCOD=" "
ZZCOD=" "
VENC=DATE()
vdataf=date()
VVEMP=" "
TTICMS=0.00

vvdias=0
VVENC=1
V_PFRETE=0
vvempresa="RIOVEDA RETENTORES"
tipo_cli=0
vcodcli=" "
vvempre=" "
vvcodcli=" "
N_EMPRE = 0
v_empre = " "
checa=1
*DO BR3
VSERIE=" "
codempre=0
public xdrive,VVALOR,VCAI1,VCAI2,papel1,papel2,pepel3,MOPCAO,MTAM,VTIPO_E,drive,ZZRELATO,drivec,ZZEMPRE,VVPORTA,MMEMPRESA,MMEMPRE,MMEMPR,N_EMPRE,v_nome
PUBLIC VVORDEM, driveecf
v_nome= " "
N_EMPRE=0
PUBLIC tticms,baseicm,vvfrete,ZZSAI
tticms=0.00
baseicm=0.00
vvfrete=0.00
VVPORTA="lpt1"
ZZEMPRE="LOJA"
ZZRELATO=" "
ZZSAI=0
public data_dia
SET STATUS BAR OFF
Dimension aMeses(12)
aMeses[01] = "Janeiro"
aMeses[02] = "Fevereiro"
aMeses[03] = "Marco"
aMeses[04] = "Abril"
aMeses[05] = "Maio"
aMeses[06] = "Junho"
aMeses[07] = "Julho"
aMeses[08] = "Agosto"
aMeses[09] = "Setembro"
aMeses[10] = "Outubro"
aMeses[11] = "Novembro"
aMeses[12] = "Dezembro"
ddatacompleta=date()
cDia = AllTrim(Str(Day(dDataCompleta)))
nMes = Month(dDataCompleta)
cAno = AllTrim(Str(Year(dDataCompleta)))
data_dia = "Rio de Janeiro, " + cDia + " de " + aMeses[nMes] + " de " + cAno

ddatacompleta=date()
cDia = AllTrim(Str(Day(dDataCompleta)))
nMes = Month(dDataCompleta)
cAno = AllTrim(Str(Year(dDataCompleta)))
data_dia = "Rio de Janeiro, " + cDia + " de " + aMeses[nMes] + " de " + cAno



MTAM="UN"
MOPCAO=1
papel1=0.00
papel2=0.00
MMEMPRESA=NOME
MMEMPRE=NOME
papel3=0.00

VCAI1=0
VCAI2=0
VVALOR=0
xdrive="C:"

SET SYSMENU OFF
LOCAL oldscreenleft
LOCAL oldscreentop
LOCAL oldscreenheight
LOCAL oldscreenwidth
LOCAL oldscreencolor
WITH _SCREEN
oldscreenleft=.left
oldscreentop=.top
oldscreenheigth=.height
oldscreencolor=.backcolor
.lockscreen=.f.
.backcolor=rgb(193,193,193)
.borderstyle=0
.closable=.F.
.controlbox=.t.
.maxbutton=.t.
.minbutton=.t.
.movable=.t.
.height=522
.width=795
PUBLIC vvempresa,ZZLOGO
PUBLIC vvempresa,ZZLOGO
SET EXCLUSIV OFF
VCODEMP=" "
use cademp01
goto top
.caption=nome
vvempresa=nome
goto top
.caption=nome
vvempresa=nome
ZZLOGO=LOGO
MMEMPRESA=NOME
MMEMPR=NOME
.lockscreen=.f.
endwith
SET STATUS BAR OFF
EMP="01"
zz_nome = " "
corget="RGB(0,192,192)"
corsay="RGB(0,0,255)"
*-- Verifica se o aplicativo está sendo
*	rodado dentro do Visual FoxPro

*	FILE() Verifica se um determinado
*	arquivo existe em disco
*	HOME() Retorna o diretorio de onde
*	foi inicializado o FoxPro
IF FILE(HOME()+"\VFP.EXE") OR FILE(HOME()+"\VFP6.EXE")
	infox=.T.
ELSE
	infox=.F.
ENDIF
**
SET SYSMENU OFF
SET CLOCK OFF

IF infox
	** Desabilita opções de Desenvolvimento

	PUSH MENU _MSYSMENU  && Armazena o menu ativo.

	DEACTIVATE WINDOW ;
		"gerenciador de projetos",;
		"project manager",;
		"padrão",;
		"standard",;
		"dbibar",;
		"layout",;
		"form controls",;
		"controles de formulário",;
		"report controls",;
		"controles de relatório"

	RELEASE WINDOWS;
		"criador de formulários",;
		"form designer",;
		"criador de relatórios",;
		"report designer",;
		"criador de banco de dados",;
		"database designer",;
		"criador de visualizações",;
		"view designer",;
		"criador de consultas",;
		"query designer",;
		"paleta de cores",;
		"color palette"

ELSE
	ON SHUTDOWN QUIT		&& Permite a saida da aplicação pelo 'X'
ENDIF


*-- Preparação de Ambiente

* salva configurações
oldfundo     = _SCREEN.PICTURE
oldtalk      = SET("Talk")
oldpath      = SET("Path")
olddate      = SET("Date")
olddel       = SET("Deleted")
oldcurrency  = SET("Currency",1)
oldpoint     = SET("Point")
oldseparator = SET("Separator")
oldexclusive = SET("Exclusive")
oldreprocess = SET("Reprocess")
oldrefresh   = SET("Refresh")
olderror     = ON ("Error")
public data_dia
SET STATUS BAR OFF
Dimension aMeses(12)
aMeses[01] = "Janeiro"
aMeses[02] = "Fevereiro"
aMeses[03] = "Marco"
aMeses[04] = "Abril"
aMeses[05] = "Maio"
aMeses[06] = "Junho"
aMeses[07] = "Julho"
aMeses[08] = "Agosto"
aMeses[09] = "Setembro"
aMeses[10] = "Outubro"
aMeses[11] = "Novembro"
aMeses[12] = "Dezembro"
ddatacompleta=date()
cDia = AllTrim(Str(Day(dDataCompleta)))
nMes = Month(dDataCompleta)
cAno = AllTrim(Str(Year(dDataCompleta)))
data_dia = "Rio de Janeiro, " + cDia + " de " + aMeses[nMes] + " de " + cAno

ddatacompleta=date()
cDia = AllTrim(Str(Day(dDataCompleta)))
nMes = Month(dDataCompleta)
cAno = AllTrim(Str(Year(dDataCompleta)))
data_dia = "Rio de Janeiro, " + cDia + " de " + aMeses[nMes] + " de " + cAno

*-- Reconfigura o Ambiente
set color to gr+/w+
CLEAR
SET SAFETY OFF
DO Configi	&& configura os comandos SET

_SCREEN.PICTURE     = "AB.bmp"  && Papel de parede
_SCREEN.WINDOWSTATE = 2 	&& Executar maximizada

*-- Gerenciador de Erro
*	Apartir deste momento, quando acontecer um erro o foxpro irá chamar o
*	programa MEUERRO com a Mensagem de erro, o programa e o nº do erro
*ON ERROR DO meuerro WITH MESSAGE(), SYS(16), ERROR()
*DO FORM awbcinf		&& Formulário de Abertura - windowType = modal
m.cancelou = .f.
tudo_ok = .T.
*DO FORM ENTRADA		&& Formulário do Registro de Usuário - windowType = modal
IF m.cancelou = .t.
   RETURN
ENDIF
*zz_nome = nome
PUBLIC opinc,opmenu,barmenu,VSERIE
OPMENU="M1"
BARMENU="1"
CLOSE DATABASE
vnivel= " "
VNIVEL3=" "
VNIVEL2=" "
VNIVEL4=" "
PUBLIC VMASTER
VMASTER="N"

LOCAL  m.loThermo
m.loThermo = NEWOBJECT("cussqlthermo","DEFCLASS.PRG")
&&IF MESSAGEBOX("Deseja Atualizar o Arquivo: ",36)=6
**IF MESSAGEBOX("Deseja Recriar os Arquivos: ",36)=6
*DO FORM frm_empresa
*
DO confimp
DO MAIN
SET SYSMENU ON
READ EVENTS  
IF infox
  _SCREEN.PICTURE = oldfundo
  SET TALK &oldtalk
      SET PATH TO &oldpath
	  SET DELETED &olddel
	  SET DATE TO &olddate
	  SET CURRENCY TO oldcurrency
	  SET POINT TO oldpoint
	  SET SEPARATOR TO oldseparator
	  SET EXCLUSIVE &oldexclusive
	  SET REPROCESS TO oldreprocess
	  SET REFRESH TO oldrefresh
	  ON  ERROR &olderror
	  IF WEXIST("Gerenciador de Projetos")
		 ACTIVATE WINDOW "Gerenciador de Projetos"
	  ENDIF
	  IF WEXIST("PROJECT MANAGER")
		 ACTIVATE WINDOW "PROJECT MANAGER"
	  ENDIF
	  IF WEXIST("Padrão")
		 ACTIVATE WINDOW "Padrão"
	  ENDIF
	  IF WEXIST("STANDARD")
	     ACTIVATE WINDOW "STANDARD"
	  ENDIF
	  IF WEXIST("DBIBar")
		 ACTIVATE WINDOW "DBIBar"
	  ENDIF
	  IF WEXIST("LAYOUT")
		 ACTIVATE WINDOW "LAYOUT"
	  ENDIF
	  IF WEXIST("FORM CONTROLS")
		 ACTIVATE WINDOW "FORM CONTROLS"
	  ENDIF
	  IF WEXIST("Controles de Formulário")
		 ACTIVATE WINDOW "Controles de Formulário"
	  ENDIF
	  IF WEXIST("Report Controls")
		 ACTIVATE WINDOW "Report Controls"
	  ENDIF
	  IF WEXIST("Controles de Relatório")
		 ACTIVATE WINDOW "Controles de Relatório"
    ENDIF

	POP MENU _MSYSMENU
	CLEAR ALL
	CLEAR
ENDIF
ELSE
*--	Fim da restauração da configuração
*-- Fim do programa início

      cMessageTitle = 'Aviso !!!'
        cMessageText = 'Operador Nao Autorizado a Entrar Neste Modulo!'
       nDialogType = 48
       nAnswer = MESSAGEBOX(cMessageText, nDialogType, cMessageTitle)
      return
ENDIF
ENDIF
enddo

Procedure Errores
PARAMETER merror, mess, mess1, mprog, mlineno
nOpc=     MESSAGEBOX("Ocorreu un error:"+CHR(13)+CHR(13)+;
"Data/Hora.: "+TTOC(DATETIME())+CHR(13)+;
'Número de error: ' + LTRIM(STR(merror))+CHR(13)+ ;
'Menssage do error: ' + mess+ CHR(13)+ ;
'Línha do código com error: ' + mess1+CHR(13)+ ;
'Número da línha do erro : ' +;
LTRIM(STR(mlineno))+CHR(13)+ ;
'Programa com error: ' + mprog, 2+48)
LOCAL cMensagem
cMensagem = ;
CHR(13)+CHR(10)+;
"Data/Hora.: "+TTOC(DATETIME())+CHR(13)+CHR(10)+;
'Número de error: ' + LTRIM(STR(merror))+CHR(13)+CHR(10)+ ;
'Menssage do error: ' + mess+ CHR(13)++CHR(10)+;
'Línha do código com error: ' + mess1+CHR(13)+CHR(10)+ ;
'Número da línha do erro : ' +LTRIM(STR(mlineno))+CHR(13)
STRTOFILE(cMensagem,"Erros.txt",1)

IF nopc=3
   CLOSE DATABASE
   CLOSE ALL
   QUIT
ENDIF
   


RETURN



PROCEDURE tmaster
*set color to gr+/w+
*SET COLOR TO W+/RG+
SET COLOR TO W+/GB
clear
_SCREEN.FONTSIZE=14
_SCREEN.FONTNAME="COURIER"
@ 06,40 SAY "ABDON" FONT "ARIAL",60 STYLE "BS"
@ 11,40 SAY "JEANS" FONT "ARIAL",60 STYLE "BS"
@ 17,40 SAY "Rua Vicente de Lima Cleto , 279" FONT "ARIAL",12 STYLE "BI"
@ 18,40 SAY "Nova Cidade - São Gonçalo - R.J." FONT "ARIAL",12 STYLE "BI"
@ 19,40 SAY "Telefone: 2701-7474" FONT "ARIAL",12 STYLE "BI"
SET COLOR TO W+/GR+
@ 24,02 SAY "ALT+1 CALCULADORA" FONT "CORRIER",08 STYLE "B"
@ 25,02 SAY "ALT+2 CALENDARIO " FONT "CORRIER",08 STYLE "B"
SET COLOR TO W+/GB
FUNCTION MOLDURA(LIN1,COL1,LIN2 ,COL2,TEXTO,DUPLO,ALTO)
SET COLOR TO B+/W+
@LIN1,COL1 CLEAR TO LIN2 ,COL2
@LIN1,COL1 TO LIN2 ,COL2 double
tam=COL2-COL1
novapos=(tam-LEN(texto))/2
SET COLOR TO B/W
@ LIN1,COL1+NOVAPOS SAY TEXTO font "ARIAL",9
SET COLOR TO W+/W
*RETURN (NIL)

FUNCTION confirma(texto)
escolha=MESSAGEBOX(texto,4+32+256,"Controle de Lojas")
return
PROCEDURE valext

   *------------------------------------------------------------------
   *  Retorna o Valor por Extenso
   *------------------------------------------------------------------
   PARAME valnum
   
   
   
   tab1='Cento       Duzentos    Trezentos   QuatrocentosQuinhentos  '
   tab1=tab1+'Seiscentos  Setecentos  Oitocentos  Novecentos  '
   
   tab2='            Vinte       Trinta      Quarenta    Cinquenta   '
   tab2=tab2+'Sessenta    Setenta     Oitenta     Noventa     '
   
   tab3='Um          Dois        Tres        Quatro      Cinco       '
   tab3=tab3+'Seis        Sete        Oito        Nove        '
   
   tab4='Dez         Onze        Doze        Treze        Quatorze   '
   tab4=tab4+'Quinze      Dezesseis   Dezessete   Dezoito     Dezenove    '
   
   noms ='Bilhao          Milhao          Mil             Real            Centavo         '
   
   nomp ='Bilhoes         Milhoes         Mil             Reais           Centavos        '
   
   EXTEN   = ''
   extenso = ''
   
   
   valstr  = STR(valnum,15,2)
   contcas = 5
   estado  = 0
   
   DO WHILE contcas > 0
      
      IF contcas <> 5
         ind = ((contcas - 1) * 3) + 1
         valest = SUBS(valstr,ind,3)
      ELSE
         valest = ' ' + SUBS(valstr,14,2)
      ENDIF
      
      IF VAL(valest) <> 0
         DO valcent WITH valest
         ind = ((contcas - 1) * 16) + 1
         
         IF VAL(valest) <> 1 .OR. contcas = 4 .AND. VAL(SUBS(valstr,1,9)) <> 0
            EXTEN = EXTEN + ' ' + SUBS(nomp,ind,16)
         ELSE
            EXTEN = EXTEN + ' ' + SUBS(noms,ind,16)
         ENDIF
         EXTEN = TRIM(EXTEN)
         
         DO CASE
         CASE estado = 0 .AND. contcas > 3
            estado = 1
         CASE estado = 0 .AND. contcas = 3
            EXTEN = EXTEN + ' Reais'
            estado = 1
         CASE estado = 0 .AND. contcas < 3
            EXTEN = EXTEN + ' de Reais'
            estado = 1
         CASE estado = 1
            EXTEN = EXTEN + ' e '
            estado = 2
         CASE estado = 2
            EXTEN = EXTEN + ', '
         ENDCASE
         
         extenso = EXTEN + extenso
         
      ENDIF
      contcas = contcas - 1
      
   ENDDO
   
   SAIDA=EXTENSO
   
  
  PROCEDURE valcent
   *------------------------------------------------------------------
   PARAM valent
   EXTEN = ''
   
   
   
   contloop = 1
   
   DO WHILE contloop <= 3
      auxext = ''
      valsub = VAL(SUBSTR(valent,contloop,1))
      IF valsub <> 0
         IF contloop = 1 .AND. valsub = 1 .AND. SUBSTR(valent,2,2) = '00'
            auxext = 'CEM'
            contloop = 4
         ELSE
            IF contloop = 2 .AND. valsub = 1
               valsub = VAL(SUBSTR(valent,3,1)) + 1
               contloop = 4
            ENDIF
            ind = ((valsub - 1) * 12) + 1
            conts = STR(contloop,1)
            qtabela = 'tab' + conts
            auxext = SUBS(&qtabela,ind,12)
            auxext = TRIM(auxext)
            IF (contloop = 1 .AND. SUBS(valent,2,2) <> '00') .OR. (contloop = 2  .AND. SUBS(valent,3,1) <> '0' )
               auxext = auxext + ' e '
            ENDIF
         ENDIF
      ENDIF
      EXTEN = EXTEN + auxext
      contloop = contloop + 1
   ENDDO
   
RETURN

FUNCTION validar_sistema 

LOCAL lcOldCaption 
lcOldCaption = _screen.Caption 
_screen.Caption = SYS(3) 

LOCAL lnhwnd 
DECLARE INTEGER FindWindow IN Win32API STRING lpClassName, STRING lpWindowName 
DECLARE INTEGER BringWindowToTop IN Win32API INTEGER HWND 
DECLARE INTEGER SendMessage IN Win32API INTEGER HWND, INTEGER Msg, INTEGER WParam, INTEGER LPARAM 
lnhwnd = findwindow( 0, lcOldCaption) 
_screen.Caption = lcOldCaption 
IF lnhwnd > 0 
    bringwindowtotop(lnhwnd) && Mandar la ventana de la aplicación al frente 
    sendmessage(lnhwnd, 274, 61488, 0) && Maximizar la ventana de la aplicación 
    RETURN .F. 
ENDIF 
RETURN .T.
FUNCTION WinProcess(cProc)
loWMI = Getobject("winmgmts:")
loProcesses = loWMI.ExecQuery("select * from WIN32_Process")
lFound = .F.
lcNomeProc = "" 
For Each loProc In loProcesses 
lcNomeProc = Upper(Alltrim(loProc.Name)) 
IF cProc = lcNomeProc
   CHECASISTEM=1
   lFound = .T.
   EXIT
ENDIF
Endfor 
RETURN lFound



PROCEDURE ENVIOMAIL
PARAMETER cArch1, cArc2, cMail
oOutLook = Createobject( "Outlook.Application" )
oEmail = oOutLook.CreateItem (0)
oEmail.Display
With oEmail
.Recipients. Add(ALLTRIM(cMail))
.Subject = m.assunto
.Importance = 1
.Body = m.corpo+chr(13)+chr(10)+CHR(13)
.Attachments. Add(cArch1)
*.Attachments. Add(cArch2)
.Send
EndWith
Release oEmail
Release oOutLook
replace enviado WITH "Sim"
*MESSAGEBOX("Email enviado com sucesso")
RETURN



FUNCTION ESTALIVRE(cArquivo)
 * Retorna se o arquivo está em uso ou não
 * USO: ESTALIVRE('c:\nomedoarquivo.dbf')
 * RETORNA: .T. ou .F.
 LPARAMETERS m.tabela
 lnarquivo = FOPEN(m.tabela,1)
 IF lnarquivo > 0
 FCLOSE(lnArquivo)
 ELSE
 RETURN .F.
 ENDIF
 ENDFUNC
 
 
 
 
 FUNCTION makepopup
LPARAMETERS lirow, licol
LOCAL ARRAY laMaus(1)
	IF VARTYPE(lirow) = [N] AND VARTYPE(licol) = [N] ;
	AND CAST(lirow AS INTEGER) > 0 AND CAST(licol AS INTEGER) > 0
		DEFINE POPUP Pop1 shortcut relative from INT(lirow / FONTMETRIC(1)), INT(licol / FONTMETRIC(6))
	ELSE
		DEFINE POPUP Pop1 shortcut relative from mrow(), mcol()
	ENDIF
	PUBLIC PopupMenueSelection
ENDFUNC 
FUNCTION add2popup
LPARAMETERS vPrompt, vValue, vMessage, vPicture, vKey
	vKey		= EVL( vKey , [] )
	vPicture	= EVL( vPicture , [] )
	DO CASE 
	CASE EMPTY( vKey ) AND EMPTY( vPicture )
		DEFINE BAR (vValue) OF Pop1 PROMPT vPrompt MESSAGE vPrompt
	CASE EMPTY( vKey ) AND NOT EMPTY( vPicture )
		DEFINE BAR (vValue) OF Pop1 PROMPT vPrompt MESSAGE vPrompt PICTURE vPicture
	CASE NOT EMPTY( vKey ) AND EMPTY( vPicture )
		DEFINE BAR (vValue) OF Pop1 PROMPT vPrompt MESSAGE vPrompt KEY vKey
	CASE NOT EMPTY( vKey ) AND NOT EMPTY( vPicture )
		DEFINE BAR (vValue) OF Pop1 PROMPT vPrompt MESSAGE vPrompt KEY vKey PICTURE vPicture
	ENDCASE 
ENDFUNC 
FUNCTION showpopup
	ON SELECTION popup pop1 PopupMenueSelection = BAR()
	ACTIVATE POPUP Pop1
	RELEASE POPUP Pop1 extended 
ENDFUNC 

PROCEDURE waitCenter as VOID
*********************************

LPARAMETERS pctext as String, pntimeout as Integer
LOCAL lnyy as Integer, lnxx as Integer

* Calcula o número total de linhas
m.lnxx = 1
FOR m.lnyy = 1 TO MEMLINES( m.pctext )
  m.lnxx = MAX( m.lnxx, LEN( ALLTRIM( MLINE(m.pctext, m.lnxx) ) ) )
NEXT

* Calcula a posição central da tela
m.lnyy = ( SROWS()/ 2 ) - ( (m.lnyy-1) / 2 )
m.lnxx = ( SCOLS() / 2 ) - ( m.lnxx / 2 )

* Limpa buffer de teclado e janela wait window
CLEAR TYPEAHEAD
WAIT CLEAR

* Se for especificado o timeout
IF VARTYPE( m.pntimeout ) = "N"
  *!* ---> Para ambiente de teste
  WAIT WINDOW m.pctext TIMEOUT m.pntimeout AT m.lnyy, m.lnxx-5
  *!* ---> Para ambiente de produção
  *!*WAIT WINDOW m.pctext TIMEOUT m.pntimeout AT m.lnyy+INT((SYSMETRIC(2)*3)/600), m.lnxx+INT((SYSMETRIC(1)*40)/800)
ELSE
  *!* ---> Para ambiente de teste
  WAIT WINDOW m.pctext NOWAIT NOCLEAR AT m.lnyy, m.lnxx-5
  *!* ---> Para ambiente de produção
  *!*WAIT WINDOW m.pctext NOWAIT NOCLEAR AT m.lnyy+INT((SYSMETRIC(2)*3)/600), m.lnxx+INT((SYSMETRIC(1)*40)/800)
ENDIF
ENDPROC
FUNCTION diasemana(data_sel)
*---------------------------
LOCAL knomedia
DO CASE
   CASE DOW(data_sel) = 1
      	knomedia = "Domingo"
   CASE DOW(data_sel) = 2	
        knomedia = "Segunda-Feira"
        CASE DOW(data_sel) = 3
        	knomedia = "Terça-Feira"
        	CASE DOW(data_sel) = 4
        		knomedia = "Quarta-Feira"
        		CASE DOW(data_sel) = 5	
        		knomedia = "Quinta-Feira"
        		CASE DOW(data_sel) = 6	
        		knomedia = "Sexta-Feira"
        		CASE DOW(data_sel) = 7	
        		knomedia = "Sábado"
        		ENDCASE
        		RETURN knomedia
*--	Fim da restauração da configuração
*-- Fim do programa início
*******************************************************************************************    
**************
* Azip procedure to zip files for Visual FoxPro using the
* AddZip AZIP32.DLL from shareware
* http://ourworld.compuserve.com/homepages/Stephen_Darlington/addzip.htm
* Requirements: AZIP32.DLL in your Windows\system directory or current directory
*
* USAGE: AZIP(lInitialize, sArchive, sInclFiles[, ZipParams])
*
* Example: AZIP(.T., "ZIPFILE", "*.DBF *.TXT Customer.doc")
*
* PARAMETERS:
*-- lInitialize: .T. first time & Done only once before or when starting file zip - .F. afterward
*-              Calling azip() with no params also initializes
*-- sArchive: Archive FileName with extension - Example "C:\THISFILE.ZIP"
*-- sInclFiles: String repr. file(s) to include
*--        Example1 "D:\CUSTDATA.DBF"
*--     Example2 "C:\CUSTOMER.DBF D:\*.TXT D:\DATABASES\*.*" - only single spaces in between
*--     ZIPparams: Some compression parameters [Optional]
*                  e(x) x = 'X' maximum compression
*                       x = '0' no compression (digital 0 not O)
*                       x = 'S' minimal compression
*                       x = 'N' normal compression (default)
*                  P include directory entries
*                  Spassword must be last part
*
* Return Values: the number of files archived or -1 if archive name not specified.


********************************************************************************************
FUNCTION aZip
PARAMETERS lInitialize, sArchive, sInclFiles, ZIPparams
PRIVATE Params, sTemp


Params = PARAMETERS()
* declare needed DLL functions & pass current window
IF Params = 0 OR lInitialize
    PRIVATE HWND
    DECLARE INTEGER GetActiveWindow IN win32api
    HWND = GetActiveWindow()
    DECLARE addZIP_Initialise IN AZIP32
    DECLARE SHORT addZIP_SetParentWindowHandle IN AZIP32 SHORT @ HWindow
    DECLARE SHORT addZIP_ArchiveName IN AZIP32 STRING @ sArchName
    DECLARE SHORT addZIP_Include IN AZIP32 STRING @ sFileName
    DECLARE SHORT addZIP_Recurse IN AZIP32 SHORT @ nRecurse
    DECLARE SHORT addZIP_SetCompressionLevel IN AZIP32 SHORT @ nComprLvl
    DECLARE SHORT addZIP_IncludeDirectoryEntries IN AZIP32 SHORT @ nInclDir
    DECLARE SHORT addZIP_IncludeEmptyDirectoryEntries IN AZIP32 SHORT @ nInclEDir
    DECLARE SHORT addZIP_Update IN AZIP32 SHORT @ nUpdate
    DECLARE SHORT addZIP_Update IN AZIP32 SHORT @ nUpdate
    DECLARE SHORT addZIP IN AZIP32
    DECLARE SHORT addZIP_Register IN AZIP32 String @ RegName, Integer @ RegNum
    DECLARE SHORT addZIP_Encrypt IN AZIP32 STRING @ sPassw
    addZIP_Initialise()
    addZIP_SetParentWindowHandle(HWND)
    * addZIP_Register("RegistrationName", RegistrationNumber)
    * Do above line if you have registered the software - it permits passwords

    IF Params < 2
        RETURN 0
    ENDIF
ENDIF

IF Params < 3
    ?? CHR(7)
    MESSAGEBOX('Missing parameters!',0, 'AZip Notice!')
    RETURN 0
ENDIF

IF Params > 3 && ZIPparams exist
    ZIPparams = ALLT(ZIPparams)
    PRIVATE LastPos, PASSWORD, nCurPos
    LastPos = LEN(ZIPparams) + 1

    * check for password
    nCurPos = ATC('S', ZIPparams)
    IF nCurPos > 0
        cPassWord = SUBSTR(ZIPparams, nCurPos+1)
        addZIP_Encrypt(cPassWord)
        ZIPparams = LEFT(ZIPparams, nCurPos -1) && now remove password part - it may have other code
    ENDIF

    * check for storing subdirectory information
    nCurPos = ATC('P', ZIPparams)
    IF nCurPos > 0
        addZIP_Recurse(1)
        addZIP_IncludeDirectoryEntries(1)
        * addZIP_IncludeEmptyDirectoryEntries(1)
    ENDIF

    * check for compresion level
    nCurPos = ATC('E', ZIPparams)
    IF nCurPos > 0
        PRIVATE cCompLevel, nCompLevel
        cCompLevel = SUBSTR(ZIPparams, nCurPos + 1, 1)
        DO CASE
        CASE cCompLevel = 'X' && max compression
              nCompLevel = 3
        CASE cCompLevel = 'S' && min compression
              nCompLevel = 1
        CASE cCompLevel = '0' && no compression
              nCompLevel = 0
        OTHERWISE && normal/default compression
              nCompLevel = 2
        ENDCASE
        addZIP_SetCompressionLevel(nCompLevel)
    ENDIF
ELSE
    addZIP_SetCompressionLevel(2)
ENDIF

addZIP_ArchiveName(sArchive) && specify archive filepath
sInclFiles = ALLT(STRTRAN(sInclFiles, ' ', '|')) && file(s) to zip
addZIP_Include(sInclFiles) && (sInclFiles)


Return addZIP() && do it & return # of files compressed

***************************************************************************************
Function aUnzip
* Azip procedure to unzip files for Visual FoxPro using the
* AddZip AUNZIP.DLL's from shareware
* http://ourworld.compuserve.com/homepages/Stephen_Darlington/addzip.htm
*
* Requirements: AUNZIP32.DLL in your Windows\system directory or current directory
*
* USAGE: AUNZIP(lInitialize, sArchive sDir[, sExtractFiles[, UnZIPparams]])
*
* PARAMETERS:
*-- lInitialize: .T. first time & Done only once beforeor when starting file zip - .F. afterward
*-              Calling azip() with no params also initializes
*-- sArchive: Archive FileName with extension - Example "C:\THISFILE.ZIP"
*-- sDir: destination directory string
*--        Example1 "D:\TEMP"
*-- sExtractFiles(optional): string repr. file(s) or types to extract
*--     Default is "*.*"
*--     Example1 "C:\CUSTOMER.DBF"
*--     Example2 "C:\CUSTOMER.DBF D:\*.TXT D:\DATABASES\*.*" - only single spaces in between
*-- UnZIPparams: Some compression parameters [Optional]
*                F freshen files - over older date/time or not existing
*                D include directory information
*                Overwrite: !!!!
*                   !!!! This does not seem to work except OverwriteNone - as of Aug. 1998 version.
*                        OA: overwrite all (default)
*                        O0: do not overwrite (letter O, digit 0)
*                        OU: ask user
*                Spassword (Must be last part - works only with registered version)
*
* Return Values: the number of files archived or -1 if archive name not specified.

PARAMETERS Initialize, sArchive, sDir, sExtractFiles, UnZIPparams
PRIVATE Params, sTemp
Params = PARAMETERS()
* declare needed DLL functions & pass current window
IF Params = 0 OR Initialize
  PRIVATE HWND
  DECLARE INTEGER GetActiveWindow IN win32api
  HWND = GetActiveWindow()
  DECLARE addUNZIP_Initialise IN AUNZIP32
  DECLARE SHORT addUNZIP_SetParentWindowHandle IN AUNZIP32 SHORT @ HWindow
  DECLARE SHORT addUNZIP_ArchiveName IN AUNZIP32 STRING @ sArchName
  DECLARE SHORT addUNZIP_RestoreStructure IN AUNZIP32 SHORT @ nResStr
  DECLARE SHORT addUNZIP_Freshen IN AUNZIP32 SHORT @ nFreshn
  DECLARE SHORT addUNZIP_Include IN AUNZIP32 STRING @ sFileName && default is *.*
  DECLARE SHORT addUNZIP_ExtractTo IN AUNZIP32 STRING @ sExtrTo
  DECLARE SHORT addUNZIP_Register IN AUNZIP32 STRING @ RegName, INTEGER @ RegNum
  DECLARE SHORT addUNZIP_Decrypt IN AUNZIP32 STRING @ sPassw
  DECLARE SHORT addUNZIP_Overwrite IN AUNZIP32 SHORT @ nOvLevel
  DECLARE SHORT addUNZIP IN AUNZIP32
  addUNZIP_Initialise()
  addUNZIP_SetParentWindowHandle(HWND)
* addUNZIP_Register("RegistrationName", RegistrationNumber)
  IF Params < 2
    RETURN 0
  ENDIF
ENDIF

IF Params < 3
  ?? CHR(7)
  MESSAGEBOX('Missing parameters!',0, 'AUNZip Notice!')
  RETURN 0
ENDIF

IF Params > 3 && UnZIPparams exist
  UnZIPparams = ALLT(UnZIPparams)
  PRIVATE LastPos, PASSWORD, nCurPos
  LastPos = LEN(UnZIPparams) + 1

  * check for password
  nCurPos = ATC('S', UnZIPparams)
  IF nCurPos > 0
    cPassWord = SUBSTR(UnZIPparams, nCurPos+1)
    addUNZIP_Decrypt(cPassWord)
    UnZIPparams = LEFT(UnZIPparams, nCurPos -1) && now remove password part - it may have other code
  ENDIF

  * check for restoring subdirectory information
  nCurPos = ATC('D', UnZIPparams)
  IF nCurPos > 0
    addUNZIP_RestoreStructure(1)
  ENDIF

  * check for freshen
  nCurPos = ATC('F', UnZIPparams)
  IF nCurPos > 0
    addUNZIP_RestoreStructure(1)
  ENDIF

  * check for Overwrite - this does not seem to work except, OverwriteNone
  DO CASE
  CASE 'OA' $ UnZIPparams && Overwrite all
    addUNZIP_Overwrite(0x0b)
  CASE 'O0' $ UnZIPparams && overwrite none
    addUNZIP_Overwrite(0x0c)
  CASE 'OU' $ UnZIPparams && overwrite ask user
    addUNZIP_Overwrite(0x0a)
  ENDCASE

  IF nCurPos > 0
    addUNZIP_RestoreStructure(1)
  ENDIF

ELSE
  addUNZIP_Overwrite(0x000a) && this does not seem to work except OverwriteNone
ENDIF

addUNZIP_ArchiveName(sArchive)
addUNZIP_ExtractTo(ALLT(sDir))
sExtractFiles = IIF(Params<4, "*.*", ALLT(STRTRAN(sExtractFiles, ' ', '|'))) && file(s) to extract
addUNZIP_Include(sExtractFiles) && files to extract - *.txt, *.*, etc.
RETURN addUNZIP()
