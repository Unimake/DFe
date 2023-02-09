CLOSE DATABASES
_SCREEN.WINDOWSTATE = 2 	&& Executar maximizada

* Seta Ambiente
PUBLIC vvamb,vamb,ZZNFE
ZZNFE=" "
DO CONFIMP

vvamb="1"
SET DATE TO BRITISH 
SET DEFAULT TO FULLPATH(CURDIR())
SET PATH TO PROGS;MENU;FULLPATH(CURDIR())
mesano=SUBSTR(DTOC(DATE()),4,2)+ALLTRIM(STR(YEAR(DATE()),4))
x=FULLPATH(CURDIR())+"Retorno\"+mesano+"\"
IF NOT DIRECTORY("&x")
   MKDIR ('&x')
ENDIF
PUBLIC vvamb,drive
SELECT 2
USE empresa
GOTO top
drive=ALLTRIM(pasta)
vvamb=amb
vamb=amb
DO FORM acessow
IF checa=1
PUBLIC Aplicativo
Aplicativo= CREATEOBJECT("Custom")
Aplicativo.AddObject("CertificadoSelecionado","Custom")
Aplicativo.CertificadoSelecionado.AddProperty("Selecionado")
Aplicativo.CertificadoSelecionado.AddProperty("Vencido")

Aplicativo.AddObject("Configuracao","Custom")
Aplicativo.Configuracao.AddProperty("Inicializar")

pastaRetorno = FULLPATH(CURDIR())+'Retorno'             
pastaRetorno = FULLPATH(CURDIR())+'Retorno'             

If NOT DIRECTORY(pastaRetorno) 
	MKDIR(pastaRetorno)
ENDIF




*VerificarCertificadoSelecionado()        
selecionarcertificado()





* Configuraçõoes UI
*ACTIVATE WINDOW SCREEN
_SCREEN.WINDOWSTATE = 2
*PUSH MENU _MSYSMENU 
*DO .\MENU\MENU.MPR
DO FORM FRM_MENUNFEw
*ON SHUTDOWN QUIT

*READ EVENTS
*RELEASE ALL	

* Funções
ELSE
MESSAGEBOX("Senha nao Autorizada")
QUIT

ENDIF

FUNCTION ConfiguracaoAtual(iTipoDFe, iTipoEmissao)
	InicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
	InicializarConfiguracao.TipoDFe = iTipoDFe
	InicializarConfiguracao.TipoEmissao = iTipoEmissao
	InicializarConfiguracao.CertificadoDigital = Aplicativo.CertificadoSelecionado.Selecionado
	Aplicativo.Configuracao.Inicializar = InicializarConfiguracao 
	
	RELEASE InicializarConfiguracao 
	
ENDFUNC  

FUNCTION VerificarVencimentoCertificado()
	IF Aplicativo.CertificadoSelecionado.Vencido = .t. 
		=MESSAGEBOX("O Certificado está Vencido")
		RETURN 0
	ENDIF 
ENDFUNC 

FUNCTION VerificarCertificadoSelecionado()
	IF TYPE('Aplicativo.CertificadoSelecionado.Selecionado') <> 'O'
		=MESSAGEBOX("Selecione o Certificado pelo Menu")
		RETURN .F.
	ENDIF 
ENDFUNC 

* NFe 
FUNCTION CriarEvento(XCorrecao, NSeqEvento)
waitCenter("CriarEvento)
	InfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
	DetEventoCCE = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCCE")
	Evento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")

	DetEventoCCE.XCorrecao = XCorrecao
	DetEventoCCE.Versao = "1.00"
	              
	InfEvento.DetEvento = DetEventoCCE
	InfEvento.COrgao = 33
	InfEvento.ChNFe = "41191006117473000150550010000579281779843610"
	InfEvento.CNPJ = "06117473000150"
	InfEvento.DhEvento = DATETIME()
	InfEvento.TpEvento = 110110
	InfEvento.NSeqEvento = NSeqEvento
	InfEvento.VerEvento = "1.00"
	InfEvento.TpAmb = 2
	    
	Evento.Versao = "1.00"
	Evento.InfEvento = InfEvento
	CriarEvento = Evento
	
	RELEASE InfEvento 
	RELEASE DetEventoCCE 
	RELEASE Evento 
	
	RETURN CriarEvento 
ENDFUNC 

Function GetNFe()
waitCenter("GetNFE")

	NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
	NFe.AddInfNFe(GetInfNFe())
	GetNFe = NFe
	
	RELEASE NFe 
	
	RETURN GetNFe 
ENDFUNC 

Function GetInfNFe()
waitCenter("GetInfNFe")

	infNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
	infNFe.Versao = "4.00"
	infNFe.Ide = GetIde()
 	infNFe.Emit = GetEmit()
	infNFe.Dest = GetDest()
	infNFe.Total = GetTotal()
	infNFe.Transp = GetTransp()
    SELECT 8
    USE DUPL
    GOTO TOP
    DO WHILE .NOT. EOF()
       infNFe.Cobr = GetCobr()
	   SKIP
	ENDDO    
	infNFe.Pag = GetPag()
	infNFe.InfAdic = GetInfAdic()
	infNFe.InfRespTec = GetInfRespTec()

    SELECT 5
    GOTO top
    PUBLIC vitem
    vitem=0
    DO WHILE .not. EOF()
       vitem=vitem+1
       infNFe.AddDet(GetDet())
	   SKIP
	enddo   
	GetInfNFe = infNFe
     
    RELEASE infNFe 
    
    RETURN GetInfNFe 
ENDFUNC 

FUNCTION GetIde() 
waitCenter("GetIde")

    SELECT 1
    USE NFE INDEX nfe
    GOTO top
    ZZNFE=NNF
    result = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
	result.CUF = NFE.CUF
	result.NatOp = NFE.Natop
	result.Mod = mod
	result.Serie = serie
	result.NNF = nnf
	result.DhEmi = datetime()
	result.DhSaiEnt = DATETIME()
	replace dhemi WITH DATETIME()
	replace dhsaient WITH DATETIME()
	result.TpNF = NFE.TPNF
	result.IdDest = NFE.IDDEST 
	result.CMunFG =  NFE.CMunFG 
	result.TpImp = nfe.Tpimp
	result.TpEmis =tpemis
	result.TpAmb = tpamb
	result.FinNFe = finnfe
	result.IndFinal = indfinal
	result.IndPres = indpres
	result.ProcEmi = procemi
	result.VerProc = "TESTE 1.00"
	IF mmopcao=4
	    SELECT 214
	    USE docref
	    GOTO TOP
	    IF TIPO=1
	      	**//Inserindo a primeira chave
            oNFref = CreateObject("Unimake.Business.DFe.Xml.NFe.NFref")
            oNFref.RefNFe = ALLTRIM(XCHAVE) &&"33220227863703000108550010000013641800903409"
            result.AddNFref(oNFref)
        ELSE   
            oNFref = CreateObject("Unimake.Business.DFe.Xml.NFe.NFref")
         *   oNFref = CreateObject("Unimake.Business.DFe.Xml.NFe.refNFP")
            oNFref.AAMM="2110"
            oNFref.CNPJ="08744753000385"
            oNFref.IE="79219711"
            oNFref.mod="01"
            oNFref.serie="1"
         *  oNFrefNFP.NFe=522205
            result.AddNFref(oNFref)
        ENDIF
           
    ENDIF
    SELECT 1
    GetIde = result
	VVNNF=NNF
	vvserie=serie
	SELECT 200
	SET KEY TO VVNNF,VVNNF
	GOTO TOP
*	IF EOF()
*	   APPEND BLANK
	   REPLACE numero WITH NFE.nnf
	   replace serie WITH NFE.Serie
	   REPLACE DATA WITH NFE.DHEMI
	   REPLACE STATUS WITH "Em Digitação"
	   
*	ENDIF
	SELECT 1
	USE
	RELEASE result 
	RETURN GetIde 
ENDFUNC 

FUNCTION GetEmit()
waitCenter("GetEmit")

    SELECT 2
    USE EMPRESA
    GOTO top
    result = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
	result.CNPJ = ALLTRIM(empresa.cnpj)
	result.XNome = ALLTRIM(empresa.xnome)
	result.XFant = ALLTRIM(empresa.xfant) &&"CBE"
	result.IE = ALLTRIM(empresa.ie) &&"81668140"
	result.IM = ALLTRIM(empresa.im) &&"14018"
	result.CNAE = ALLTRIM(empresa.cnae) &&"4530703"
	result.CRT =empresa.crt &&1
	ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
	ender.XLgr = ALLTRIM(empresa.xlgr) &&"R DR OTAVIO KELLY"
	ender.Nro = ALLTRIM(empresa.nro)
	ender.XBairro = ALLTRIM(XBAIRRO)
	ender.CMun = EMPRESA.CMun &&3304557 
	ender.XMun = ALLTRIM(EMPRESA.xmun) &&"RIO DE JANEIRO"
	ender.UF = empresa.uf
	ender.CEP = ALLTRIM(empresa.cep) &&"20511280"
	ender.Fone = ALLTRIM(empresa.fone) &&"21964631967"
	
	result.EnderEmit = ender
	GetEmit = result
	
	RELEASE result
	RELEASE ender 
	
	RETURN GetEmit 
ENDFUNC 

FUNCTION GetDest()
waitCenter("GetDest")

    SELECT 3
    USE cliente
    GOTO top
    result = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
    IF FJ=2
	   result.CNPJ = ALLTRIM(cliente.cnpj) 	&&"15698425000193"
    ELSE
       result.CPF = ALLTRIM(cliente.cnpj) 	&&"15698425000193"
    ENDIF
	x=0
	IF X=0
       result.XNome = ALLTRIM(cliente.xnome) &&"NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
	ELSE
      result.XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    endif	
	result.IndIEDest = cliente.IndIEDest &&1
	result.IE = ALLTRIM(cliente.ie) &&"79685771"
	result.Email = ALLTRIM(cliente.email) &&"awbinfo23@gmail.com"
	ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
	ender.XLgr = ALLTRIM(cliente.xlgr) &&"AV PARIS"
	ender.Nro = ALLTRIM(cliente.nro) &&"15"
	ender.XBairro = ALLTRIM(cliente.xbairro) && "BONSUCESSO"
	ender.CMun = cliente.cmun &&3304557
	ender.XMun = ALLTRIM(cliente.xmun) &&"RIO DE JANEIRO"
	ender.UF = cliente.uf &&33
	ender.CEP = ALLTRIM(cliente.cep) &&"21041020"
	ender.Fone = ALLTRIM(cliente.fone) &&"01639611500"

    result.EnderDest = ender
    GetDest = result
    SELECT 200
    REPLACE cnpj WITH cliente.cnpj
    replace cliente WITH cliente.xnome
    
    SELECT 3
    RELEASE result 
    RELEASE ender 
    
    RETURN GetDest 
ENDFUNC 

FUNCTION GetTotal()
waitCenter("GetTotal")

    XXPROD=0
    SELECT 5
    USE PRODUTOS
    GOTO TOP
    DO WHILE .NOT. EOF()
       XXPROD=XXPROD+VPROD
       SKIP
    ENDDO
    GOTO TOP
    SELECT 6
    USE TOTAIS
    GOTO top       
             
    *    	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")
	*ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
	*ICMSTot.VBC = 0
	*ICMSTot.VICMS = 0
	*ICMSTot.VICMSDeson = 0
	*ICMSTot.VFCP = 0
	*ICMSTot.VBCST = 0
	*ICMSTot.VST = 0
	*ICMSTot.VFCPST = 0
	*ICMSTot.VFCPSTRet = 0
	*ICMSTot.VProd = 140.3
	*ICMSTot.VFrete = 0
	*ICMSTot.VSeg = 0
	*ICMSTot.VDesc = 0
	*ICMSTot.VII = 0
	*ICMSTot.VIPI = 0
	*ICMSTot.VIPIDevol = 0
	*ICMSTot.VPIS = 0
	*ICMSTot.VCOFINS = 0
	*ICMSTot.VOutro = 0
	*ICMSTot.VNF = 140.3
	*ICMSTot.VTotTrib = 12.63
    *result.ICMSTot = ICMSTot
	*GetTotal = result
	*RELEASE result 
	*RELEASE ICMSTot 
	*RETURN GetTotal 
	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")
	ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
	ICMSTot.VBC = totais.vbc
	ICMSTot.VICMS = totais.vicms
	ICMSTot.VICMSDeson = totais.vicmsdeson
	ICMSTot.VFCP = totais.vfcp
	ICMSTot.VBCST = totais.vbcst
	ICMSTot.VST = totais.vst
	ICMSTot.VFCPST = totais.vfcpst
	ICMSTot.VFCPSTRet = totais.vfcpstret
	ICMSTot.VProd = totais.vprod &&140.3
	ICMSTot.VFrete = totais.vfrete
	ICMSTot.VSeg = totais.vseg
	ICMSTot.VDesc = totais.vdesc
	ICMSTot.VII = totais.vii
	ICMSTot.VIPI = totais.vipi
	ICMSTot.VIPIDevol = totais.VIPIDevol
	ICMSTot.VPIS = totais.vpis
	ICMSTot.VCOFINS = totais.vcofins
	ICMSTot.VOutro = totais.voutro
	ICMSTot.VNF = totais.vnf
	**ICMSTot.VTotTrib = 34.00
	iCMSTot.VTotTrib = totais.vtottrib
	result.ICMSTot = ICMSTot
	GetTotal = result
*	MESSAGEBOX("TOTAL"+STR(xxprod,10,2))
	RELEASE result 
	RELEASE ICMSTot 
	RETURN GetTotal 
ENDFUNC 

FUNCTION GetTransp()
waitCenter("GetTransp")

    SELECT 7
    USE transp
    GOTO top
	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
	Transporta = CreateObject("Unimake.Business.DFe.Xml.NFe.Transporta")
	Vol = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")

	Transporta.XNome = transp.xnome
	Transporta.XEnder = transp.xender
	Transporta.XMun = transp.xmun
	Transporta.UF = 33

	Vol.QVol = transp.qvol
	Vol.Esp = transp.esp
	Vol.Marca = transp.marca
	Vol.PesoL = transp.pesol
	Vol.PesoB = transp.pesob

	result.ModFrete = transp.frete
	result.Transporta = Transporta
	GetTransp = result
	
	RELEASE result 
	RELEASE Transporta 
	RELEASE Vol 
	
	RETURN GetTransp 
ENDFUNC 

FUNCTION GetCobr()
waitCenter("GetCobr")

   	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
	Dup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
	Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
  	Fat.NFat = ALLTRIM(dupl.nfat)
	Fat.VOrig = dupl.vorig
	Fat.VDesc = dupl.vdesc
	Fat.VLiq = dupl.vliq
	result.Fat = fat
	Dup.NDup = ALLTRIM(dupl.nfat)
	Dup.DVenc = dvenc
	Dup.VDup = vdup
	GetCobr = result
	
	RELEASE result 
	RELEASE Dup 
	RELEASE Fat 
	RETURN GetCobr 
ENDFUNC 

FUNCTION GetPag()
waitCenter("GetPag")

    IF mmopcao#4
	   result = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
	   DetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
	   DetPag.TPag = 15
	   DetPag.VPag = XXPROD
	ELSE
       result = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
	   DetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
	   detpag.indPag=0
	   DetPag.TPag = 90
	   DetPag.VPag = 0.00
	ENDIF
	DetPag.SetIndPag(1)
    result.AddDetPag(DetPag)
	GetPag = result
	RELEASE result 
	RELEASE DetPag 
		
	RETURN GetPag 
ENDFUNC 

FUNCTION GetInfAdic()
waitCenter("GetInfAdic")

    SELECT 9
    USE inforM
    GOTO top
    result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
	result.InfCpl = ALLTRIM(inform.OBS1)+;
	ALLTRIM(inform.OBS2)+;
	ALLTRIM(inform.OBS3)+;
	ALLTRIM(inform.OBS4)+;
	ALLTRIM(inform.OBS5)+;
	ALLTRIM(inform.OBS6)+;
	ALLTRIM(inform.OBS7)+;
	ALLTRIM(inform.OBS8)+;
	ALLTRIM(inform.OBS9)
	
	
	
	 &&";Trib aprox: Federal Estadual Municipal ; Trib aprox: Federal Estadual Municipal Fonte: ;"
    GetInfAdic = result
    
    RELEASE result 
    
    RETURN GetInfAdic 
ENDFUNC 

FUNCTION GetInfRespTec()
waitCenter("GetInfRespTec")

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
	result.CNPJ = "05413671000106"
	result.XContato = "SML INFORMATICA M.E."
	result.Email = "awbinfo23@gmail.com"
	result.Fone = "21964631967"
	GetInfRespTec = result
	
	RELEASE result 
	
	RETURN GetInfRespTec 
ENDFUNC 

FUNCTION GetDet()

waitCenter("GetDet")

*	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
*	result.NItem = 1
*	Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
*	Prod.CProd = "01042"
*	Prod.CEAN = "SEM GTIN"
*	Prod.XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
*	Prod.NCM = "84714900"
*	Prod.CFOP = "6101"
*	Prod.UCom = "LU"
*	Prod.QCom = 1
*	Prod.VUnCom = 140.3
*	Prod.VProd = 140.3
*	Prod.CEANTrib = "SEM GTIN"
*	Prod.UTrib = "LU"
*	Prod.QTrib = 1
*	Prod.VUnTrib = 140.3
*	Prod.IndTot = 1
*	Prod.XPed = "300474"
*	Prod.NItemPed = 1
*    result.Prod = Prod

       result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
	   result.NItem = vitem &&item
	   prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
	   Prod.CProd = ALLTRIM(PRODUTOS.CPROD) &&"01042"
	   Prod.CEAN = ALLTRIM(produtoS.cean) &&"SEM GTIN"
	   Prod.XProd = ALLTRIM(produtoS.xprod) &&"NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
	   Prod.NCM = ALLTRIM(PRODUTOS.NCM) &&"84714900"
	   Prod.CFOP = ALLTRIM(produtoS.cfop) &&"5405"
	   Prod.UCom = ALLTRIM(produtoS.ucom) &&"LU"
	   Prod.QCom = produtoS.qcom &&1
	   Prod.VUnCom = produtoS.vuncom &&140.3
	   Prod.VProd = produtoS.vprod &&140.3
	   Prod.CEANTrib = "SEM GTIN"
	   Prod.UTrib = ALLTRIM(produtoS.utrib) &&"LU"
	   Prod.QTrib = produtoS.qtrib &&1
	   Prod.VUnTrib = produtoS.vuntrib &&140.3
	   Prod.IndTot = prodUToS.indtot &&1
	   Prod.XPed = ALLTRIM(produtoS.xped) &&"300474"
       Prod.NItemPed = 1&&produtoS.nitemped &&1
       xxprod=xxprod+vprod
	   result.Prod = Prod
	   Imposto = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
       Imposto.VTotTrib = vtrib &&TRIBUTO
       DO CASE
          CASE XICMS="101"
               ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
               ICMSSN101 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
               ICMSSN101.Orig = 0
               ICMSSN101.PCredSN = 0 &&2.8255
               ICMSSN101.VCredICMSSN = 0 &&2.4
               ICMS.ICMSSN101 = ICMSSN101
               Imposto.ICMS = ICMS
          CASE Xicms="500"
               ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
               ICMSSN500 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN500")
               ICMSSN500.Orig = 0
               ICMS.ICMSSN500 = ICMSSN500
               Imposto.ICMS = ICMS
          CASE Xicms="060"
               ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
               ICMS60 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS60")
               ICMS60.Orig = 0
               ICMS.ICMS60 = ICMS60
               Imposto.ICMS = ICMS
         
       ENDCASE
       PIS = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")
       PISOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
       PISOutr.CST = "99"
       
       PISOutr.VBC = 0.00
       PISOutr.PPIS = 0.00
       PISOutr.VPIS = 0.00
     	PIS.PISOutr = PISOutr
       Imposto.PIS = PIS

       COFINS = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")
	   COFINSOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
       COFINSOutr.CST = "99"
       COFINSOutr.VBC = 0.00
       COFINSOutr.PCOFINS = 0.00
       COFINSOutr.VCOFINS = 0.00
	   COFINS.COFINSOutr = COFINSOutr
	   Imposto.COFINS = COFINS
       result.Imposto = Imposto
    GetDet = result
       	
	
	RELEASE result 
	RELEASE Prod 
	RELEASE imposto 
	RELEASE ICMS 
	RELEASE ICMSSN101 
	RELEASE ICMSSN500 
	RELEASE PIS 
	RELEASE PISOutr 
	RELEASE COFINS 
	RELEASE COFINSOutr 
	
	RETURN GetDet 
ENDFUNC 

FUNCTION GetFromFileNFe(arquivo)
waitCenter("GetFromFileNFe")
    MESSAGEBOX(ARQUIVO)
	NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
	GetFromFileNFe = NFe.LoadFromFile(arquivo)	
	RELEASE NFe  
	
 	RETURN GetFromFileNFe 
ENDFUNC 



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




******************
PROCEDURE shell_exec
  LPARAMETERS lclink, lcaction, lcparms, lcdir, nshowwindow
  
  DECLARE INTEGER FindWindow IN WIN32API AS WGFindWindow STRING, STRING  
  DECLARE INTEGER ShellExecute IN SHELL32.DLL AS WGShellExecute ;
      INTEGER, STRING, STRING, STRING, STRING, INTEGER
  
  m.lcaction = IIF(EMPTY(lcaction), "Open", lcaction)
  m.lcparms = IIF(EMPTY(lcparms), "", lcparms)
  m.lcdir = IIF(EMPTY(lcdir), "", lcdir)
  m.nshowwindow = IIF(VARTYPE(m.nshowwindow) == "N", m.nshowwindow, 1)

  RETURN wgshellexecute(wgfindwindow(0, _SCREEN.CAPTION),  ;
      @lcaction, @lclink,  ;
      @lcparms, @lcdir,  ;
      m.nshowwindow)
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

