* Seta Ambiente
SET DATE TO BRITISH 
SET DEFAULT TO FULLPATH(CURDIR())
SET PATH TO PROGS;MENU;FULLPATH(CURDIR())

PUBLIC Aplicativo
Aplicativo= CREATEOBJECT("Custom")

Aplicativo.AddObject("CertificadoSelecionado","Custom")
Aplicativo.CertificadoSelecionado.AddProperty("Selecionado")
Aplicativo.CertificadoSelecionado.AddProperty("Vencido")

Aplicativo.AddObject("Configuracao","Custom")
Aplicativo.Configuracao.AddProperty("Inicializar")

pastaRetorno = FULLPATH(CURDIR())+'Retorno'             
If NOT DIRECTORY(pastaRetorno) 
	MKDIR(pastaRetorno)
ENDIF
        
* Configuraçõoes UI
ACTIVATE WINDOW SCREEN
_SCREEN.WINDOWSTATE = 2
PUSH MENU _MSYSMENU 
DO .\MENU\MENU.MPR
ON SHUTDOWN QUIT

READ EVENTS
RELEASE ALL	

* Funções
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
	InfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
	DetEventoCCE = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoCCE")
	Evento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")

	DetEventoCCE.XCorrecao = XCorrecao
	DetEventoCCE.Versao = "1.00"
	              
	InfEvento.DetEvento = DetEventoCCE
	InfEvento.COrgao = 41
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
	NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
	NFe.AddInfNFe(GetInfNFe())
	GetNFe = NFe
	
	RELEASE NFe 
	
	RETURN GetNFe 
ENDFUNC 

Function GetInfNFe()
	infNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
	infNFe.Versao = "4.00"
	infNFe.Ide = GetIde()
	infNFe.Emit = GetEmit()
	infNFe.Dest = GetDest()
	infNFe.Total = GetTotal()
	infNFe.Transp = GetTransp()
	infNFe.Cobr = GetCobr()
	infNFe.Pag = GetPag()
	infNFe.InfAdic = GetInfAdic()
	infNFe.InfRespTec = GetInfRespTec()
	infNFe.AddDet(GetDet())
    GetInfNFe = infNFe
    
    RELEASE infNFe 
    
    RETURN GetInfNFe 
ENDFUNC 

FUNCTION GetIde() 

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
	result.CUF = 41
	result.NatOp = "VENDA PRODUC.DO ESTABELEC"
	result.Mod = 55
	result.Serie = 1
	result.NNF = 57980
	result.DhEmi = DATETIME()
	result.DhSaiEnt = DATETIME()
	result.TpNF = 1
	result.IdDest = 1 
	result.CMunFG = 4118402
	result.TpImp = 1
	result.TpEmis = 1
	result.TpAmb = 2
	result.FinNFe = 1
	result.IndFinal = 1
	result.IndPres = 1
	result.ProcEmi = 0
	result.VerProc = "TESTE 1.00"
	GetIde = result
	
	RELEASE result 
	
	RETURN GetIde 
ENDFUNC 

FUNCTION GetEmit()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
	result.CNPJ = "06117473000150"
	result.XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
	result.XFant = "UNIMAKE - PARANAVAI"
	result.IE = "9032000301"
	result.IM = "14018"
	result.CNAE = "6202300"
	result.CRT = 1
	 
	ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
	ender.XLgr = "RUA ANTONIO FELIPE"
	ender.Nro = "1500"
	ender.XBairro = "CENTRO"
	ender.CMun = 4118402
	ender.XMun = "PARANAVAI"
	ender.UF = 41
	ender.CEP = "87704030"
	ender.Fone = "04431414900"
	
	result.EnderEmit = ender
	GetEmit = result
	
	RELEASE result
	RELEASE ender 
	
	RETURN GetEmit 
ENDFUNC 

FUNCTION GetDest()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
	result.CNPJ = "04218457000128"
	result.XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
	result.IndIEDest = 1
	result.IE = "582614838110"
	result.Email = "janelaorp@janelaorp.com.br"

	ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
	ender.XLgr = "AVENIDA DA SAUDADE"
	ender.Nro = "1555"
	ender.XBairro = "CAMPOS ELISEOS"
	ender.CMun = 3543402
	ender.XMun = "RIBEIRAO PRETO"
	ender.UF = 35
	ender.CEP = "14080000"
	ender.Fone = "01639611500"

    result.EnderDest = ender
    GetDest = result
    
    RELEASE result 
    RELEASE ender 
    
    RETURN GetDest 
ENDFUNC 

FUNCTION GetTotal()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")
	ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
	ICMSTot.VBC = 0
	ICMSTot.VICMS = 0
	ICMSTot.VICMSDeson = 0
	ICMSTot.VFCP = 0
	ICMSTot.VBCST = 0
	ICMSTot.VST = 0
	ICMSTot.VFCPST = 0
	ICMSTot.VFCPSTRet = 0
	ICMSTot.VProd = 140.3
	ICMSTot.VFrete = 0
	ICMSTot.VSeg = 0
	ICMSTot.VDesc = 0
	ICMSTot.VII = 0
	ICMSTot.VIPI = 0
	ICMSTot.VIPIDevol = 0
	ICMSTot.VPIS = 0
	ICMSTot.VCOFINS = 0
	ICMSTot.VOutro = 0
	ICMSTot.VNF = 140.3
	ICMSTot.VTotTrib = 12.63

	result.ICMSTot = ICMSTot
	GetTotal = result
	
	RELEASE result 
	RELEASE ICMSTot 
	
	RETURN GetTotal 
ENDFUNC 

FUNCTION GetTransp()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
	Transporta = CreateObject("Unimake.Business.DFe.Xml.NFe.Transporta")
	Vol = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")

	Transporta.XNome = "RETIRADO PELO CLIENTE"
	Transporta.XEnder = "RUA RIO DE JANEIRO"
	Transporta.XMun = "POCOS DE CALDAS"
	Transporta.UF = 31

	Vol.QVol = 2
	Vol.Esp = "VOLUMES"
	Vol.Marca = "CAIXAS"
	Vol.PesoL = 0
	Vol.PesoB = 0

	result.ModFrete = 1
	result.Transporta = Transporta
	GetTransp = result
	
	RELEASE result 
	RELEASE Transporta 
	RELEASE Vol 
	
	RETURN GetTransp 
ENDFUNC 

FUNCTION GetCobr()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
	Dup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
	Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
	Fat.NFat = "151342"
	Fat.VOrig = 140.3
	Fat.VDesc = 0
	Fat.VLiq = 140.3
	result.Fat = Fat
	Dup.NDup = "001"
	Dup.DVenc = DATETIME()
	Dup.VDup = 140.3
	GetCobr = result
	
	RELEASE result 
	RELEASE Dup 
	RELEASE Fat 
	
	RETURN GetCobr 
ENDFUNC 

FUNCTION GetPag()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
	DetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
	DetPag.TPag = 15
	DetPag.VPag = 140.3
	DetPag.SetIndPag(1)
		
	result.AddDetPag(DetPag)
	
	GetPag = result
	
	RELEASE result 
	RELEASE DetPag 
		
	RETURN GetPag 
ENDFUNC 

FUNCTION GetInfAdic()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
	result.InfCpl = ";Trib aprox: Federal Estadual Municipal ; Trib aprox: Federal Estadual Municipal Fonte: ;"
    GetInfAdic = result
    
    RELEASE result 
    
    RETURN GetInfAdic 
ENDFUNC 

FUNCTION GetInfRespTec()

	result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
	result.CNPJ = "05413671000106"
	result.XContato = "Oduvaldo de Oliveira"
	result.Email = "oduvaldo@visualsistemas.net"
	result.Fone = "3537215351"
	GetInfRespTec = result
	
	RELEASE result 
	
	RETURN GetInfRespTec 
ENDFUNC 

FUNCTION GetDet()
	result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
	result.NItem = 1
	Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
	Prod.CProd = "01042"
	Prod.CEAN = "SEM GTIN"
	Prod.XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
	Prod.NCM = "84714900"
	Prod.CFOP = "6101"
	Prod.UCom = "LU"
	Prod.QCom = 1
	Prod.VUnCom = 140.3
	Prod.VProd = 140.3
	Prod.CEANTrib = "SEM GTIN"
	Prod.UTrib = "LU"
	Prod.QTrib = 1
	Prod.VUnTrib = 140.3
	Prod.IndTot = 1
	Prod.XPed = "300474"
	Prod.NItemPed = 1
    result.Prod = Prod

    Imposto = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
    Imposto.VTotTrib = 12.63

    ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
    ICMSSN101 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
    ICMSSN101.Orig = 0
    ICMSSN101.PCredSN = 2.8255
    ICMSSN101.VCredICMSSN = 2.4
    ICMS.ICMSSN101 = ICMSSN101
    Imposto.ICMS = ICMS
                                                                          
	PIS = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")
	PISOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
    PISOutr.CST = "99"
    PISOutr.VBC = 0
    PISOutr.PPIS = 0
    PISOutr.VPIS = 0
	PIS.PISOutr = PISOutr
	Imposto.PIS = PIS

	COFINS = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")
	COFINSOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
    COFINSOutr.CST = "99"
    COFINSOutr.VBC = 0
    COFINSOutr.PCOFINS = 0
    COFINSOutr.VCOFINS = 0
	COFINS.COFINSOutr = COFINSOutr
	Imposto.COFINS = COFINS
	
	result.Imposto = Imposto
	GetDet = result
	
	RELEASE result 
	RELEASE Prod 
	RELEASE imposto 
	RELEASE ICMS 
	RELEASE ICMSSN101 
	RELEASE PIS 
	RELEASE PISOutr 
	RELEASE COFINS 
	RELEASE COFINSOutr 
	
	RETURN GetDet 
ENDFUNC 

FUNCTION GetFromFileNFe(arquivo)
	NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
	GetFromFileNFe = NFe.LoadFromFile(arquivo)	
	RELEASE NFe  
	
 	RETURN GetFromFileNFe 
ENDFUNC 
