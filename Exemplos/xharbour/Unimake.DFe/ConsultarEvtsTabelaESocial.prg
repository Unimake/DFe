* ---------------------------------------------------------------------------------
* Consultar Eventos ESocial - Tabela
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultarEvtsTabelaESocial()
   Local oConfiguracao, oExceptionInterop, oErro

 * Criar o objeto de configuração mínima
   oConfiguracao := CreateObject("Unimake.Business.DFe.Servicos.Configuracao")

   oConfiguracao:TipoDFe            := 12 // TipoDFe.ESocial
   oConfiguracao:Servico            := 70 // Servico.ESocialConsultaEvts
   oConfiguracao:CertificadoArquivo := "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha   := "12345678"
   oConfiguracao:TipoAmbiente       := 2  // TipoAmbiente.Homologacao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop:= CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
    * Criar o XML
      oConsultarEvtsTabelaESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTabelaESocial")
	  
	  oConsultaIdentificadoresEvts := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaIdentificadoresEvts")
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:TpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:NrInsc := "06117473000150"
	  oConsultaIdentificadoresEvts:IdeEmpregador := oIdeEmpregador	  
	  
	  oConsultaEvtsTabela := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTabela")
	  oConsultaEvtsTabela:ChEvt := "123123"
	  oConsultaEvtsTabela:TpEvt := "S-1200"
	  oConsultaEvtsTabela:DtIniField := "2024-11-28T12:12:12"
	  oConsultaEvtsTabela:DtFimField := "2024-11-30T12:12:12"
	  oConsultaIdentificadoresEvts:ConsultaEvtsTabela := oConsultaEvtsTabela
	  
	  oConsultarEvtsTabelaESocial:ConsultaIdentificadoresEvts := oConsultaIdentificadoresEvts

    * Consumir o serviço	
	  oConsultarEvtsTabela := CreateObject("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsTabela")
      oConsultarEvtsTabela:Executar(oConsultarEvtsTabelaESocial, oConfiguracao)
	  
    * String do XML retornado
	  ? oConsultarEvtsTabela:RetornoWSString
	
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
