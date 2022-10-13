IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

consSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
consSitNFe.Versao = "4.00"
consSitNFe.TpAmb = 1
consSitNFe.ChNFe = "41211206117473000150550010000710231016752423"

consultaNota = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
consultaNota.Executar(consSitNFe,Aplicativo.Configuracao.Inicializar) 

MESSAGEBOX(ALLTRIM(STR(consultaNota.Result.CStat)) + ": " + consultaNota.Result.XMotivo)

RELEASE consSitNFe 
RELEASE consultaNota 