* ---------------------------------------------------------------------------------
* Consultar Eventos ESocial - Empregador
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultarEvtsEmpregadorESocial()
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
      oConsultarEvtsEmpregadorESocial := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsEmpregadorESocial")
	  
	  oConsultaIdentificadoresEvts := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaIdentificadoresEvts")
	  
	  oIdeEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.IdeEmpregador")
	  oIdeEmpregador:TpInsc := 1 //TiposInscricao.CNPJ
	  oIdeEmpregador:NrInsc := "06117473000150"
	  oConsultaIdentificadoresEvts:IdeEmpregador := oIdeEmpregador	  
	  
	  oConsultaEvtsEmpregador := CreateObject("Unimake.Business.DFe.Xml.ESocial.ConsultaEvtsEmpregador")
	  oConsultaEvtsEmpregador:TpEvt := "S-1200"
	  oConsultaEvtsEmpregador:PerApurField := "2024-01"
	  oConsultaIdentificadoresEvts:ConsultaEvtsEmpregador := oConsultaEvtsEmpregador
	  
	  oConsultarEvtsEmpregadorESocial:ConsultaIdentificadoresEvts := oConsultaIdentificadoresEvts

    * Consumir o serviço	
	  oConsultarEvtsEmpregador := CreateObject("Unimake.Business.DFe.Servicos.ESocial.ConsultarEvtsEmpregador")
      oConsultarEvtsEmpregador:Executar(oConsultarEvtsEmpregadorESocial, oConfiguracao)
	  
    * String do XML retornado
	  ? oConsultarEvtsEmpregador:RetornoWSString
	
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
