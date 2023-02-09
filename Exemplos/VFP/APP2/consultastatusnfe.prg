IF VerificarCertificadoSelecionado() = .F. 
	RETURN 0 
ENDIF 
VerificarVencimentoCertificado()
ConfiguracaoAtual(0,1)

consStatServ= CreateObject("Unimake.Business.DFe.Xml.NFe.ConsStatServ")
consStatServ.Versao = "4.00"
consStatServ.CUF = 41
consStatServ.TpAmb = 2

statusServico = CreateObject("Unimake.Business.DFe.Servicos.NFe.StatusServico")
statusServico.Executar(consStatServ,Aplicativo.Configuracao.Inicializar)

MESSAGEBOX(statusServico.RetornoWSString)
MESSAGEBOX(statusServico.Result.XMotivo)

RELEASE consStatServ
RELEASE statusServico 