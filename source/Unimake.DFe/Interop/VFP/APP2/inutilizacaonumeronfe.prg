IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

InutNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFe")
InutNFeInfInut = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")
Inutilizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Inutilizacao")

InutNFeInfInut.Ano = "19"
InutNFeInfInut.CNPJ = "06117473000150"
InutNFeInfInut.CUF = 41
InutNFeInfInut.Mod = 55
InutNFeInfInut.NNFIni = 57919
InutNFeInfInut.NNFFin = 57919
InutNFeInfInut.Serie = 1
InutNFeInfInut.TpAmb = 2
InutNFeInfInut.XJust = "Justificativa da inutilizacao de teste"

InutNFe.Versao = "4.00"
InutNFe.InfInut = InutNFeInfInut

Inutilizacao.Executar(InutNFe,Aplicativo.Configuracao.Inicializar)

MESSAGEBOX(Inutilizacao.RetornoWSString)
MESSAGEBOX(ALLTRIM(STR(Inutilizacao.result.InfInut.CStat)) + ": " + Inutilizacao.result.InfInut.XMotivo) 
						
DO CASE 
	CASE Inutilizacao.result.InfInut.CStat = 102 && 102 Inutilizacao homologada
	
		MESSAGEBOX(Inutilizacao.result.InfInut.DhRecbto)
		MESSAGEBOX(Inutilizacao.result.InfInut.NProt)
	
		Inutilizacao.GravarXmlDistribuicao(FULLPATH(CURDIR())+'Retorno\') 
	OTHERWISE  && Inutilização rejeitada
		Inutilizacao.GravarXmlDistribuicao(FULLPATH(CURDIR())+'Retorno\') 
ENDCASE

RELEASE InutNFe 
RELEASE InutNFeInfInut 
RELEASE Inutilizacao 