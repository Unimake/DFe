* ---------------------------------------------------------------------------------
* Download Eventos eSocial por ID
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function DownloadEventoESocialPorID()
   Local oConfiguracao, oExceptionInterop, oErro
   Local oDownloadEventosPorID, oDownload, oIdeEmpregador, oSolicitacaoDownloadPorId, oDownloadEvtsID

 * Criar o objeto de configuração mínima
   oConfiguracao := CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // TipoDFe.ESocial
   oConfiguracao:Servico            := 71 // Servico.ESocialDownloadEvts
   oConfiguracao:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente       := 2  // TipoAmbiente.Homologacao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
    * Criar o XML
      oDownloadEventosPorID := CreateObject("Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorID")
	  
	  oDownload := CreateObject("Unimake.Business.DFe.Xml.ESocial.Download")
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:TpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:NrInsc := "06117473000150"
	  oDownload:IdeEmpregador := oIdeEmpregador	  
	  
	  oSolicitacaoDownloadPorId := CreateObject("Unimake.Business.DFe.Xml.ESocial.SolicitacaoDownloadPorId")
	  oSolicitacaoDownloadPorId:Id := "ID123908129312894812"
	  oDownload:SolicitacaoDownloadPorId := oSolicitacaoDownloadPorId
	  
	  oDownloadEventosPorID:Download := oDownload

    * Consumir o serviço	
	  oDownloadEvtsID := CreateObject("Unimake.Business.DFe.Servicos.ESocial.DownloadPorID")
      oDownloadEvtsID:Executar(oDownloadEventosPorID, oConfiguracao)
	  
    * String do XML do Evento retornado
	  ? oDownloadEvtsID:RetornoWSString
	
      Wait
      Cls

   Catch oErro
    * Demonstrar exceções geradas no proprio Harbour, se existir.
      ? "ERRO/EXCECAO"
      ? "============"
	  ?
      ? oErro:Description
      ? oErro:Operation
    
   * Demonstrar a exceção do CSHARP
      ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?     
    
      Wait
      Cls   
   End
Return (Nil)
