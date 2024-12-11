* ---------------------------------------------------------------------------------
* Consultar Eventos ESocial - Trabalhador
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultarEvtsTrabalhadorESocial()
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
      oConsultarEvtsTrabalhadorESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTrabalhadorESocial")
	  
	  oConsultaIdentificadoresEvts := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaIdentificadoresEvts")
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:TpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:NrInsc := "06117473000150"
	  oConsultaIdentificadoresEvts:IdeEmpregador := oIdeEmpregador	  
	  
	  oConsultaEvtsTrabalhador := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsTrabalhador")
	  oConsultaEvtsTrabalhador:CpfTrab = "07303304940"
      oConsultaEvtsTrabalhador:DtIniField = "2024-11-28T12:12:12"
      oConsultaEvtsTrabalhador:DtFimField = "2024-11-30T12:12:12"
	  oConsultaIdentificadoresEvts:ConsultaEvtsTrabalhador := oConsultaEvtsTrabalhador
	  
	  oConsultarEvtsTrabalhadorESocial:ConsultaIdentificadoresEvts := oConsultaIdentificadoresEvts

    * Consumir o serviço	
	  oConsultarEvtsTrabalhador := CreateObject("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsTrabalhador")
      oConsultarEvtsTrabalhador:Executar(oConsultarEvtsTrabalhadorESocial, oConfiguracao)
	  
    * String do XML retornado
	  ? oConsultarEvtsTrabalhador:RetornoWSString
	
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
