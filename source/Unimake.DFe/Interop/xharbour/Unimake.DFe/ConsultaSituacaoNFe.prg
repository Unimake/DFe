* ---------------------------------------------------------------------------------
* Consumindo o serviço de consulta a situacao da NFe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function ConsultaSituacaoNfe()
   Local oConfig
   Local oConsSitNfe, oErro, oExceptionInterop
   Local oConsultaProtocolo

 * Criar configuraçao básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:TipoDfe = 0 // 0=nfe
   oConfig:Servico = 1 // 1=Situacao da NFE
   oConfig:CertificadoSenha = "12345678"
   oConfig:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar XML
   oConsSitNfe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNfe")
   oConsSitNfe:Versao = "4.00"
   oConsSitNfe:TpAmb  = 2  // Homologação
   oConsSitNfe:ChNfe  = "412001061174730001505500100006066414037532101" // Chave da NFE 
   
   //Criar objeto para pegar exceção do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try   
    * Consumir o serviço
      oConsultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
      oConsultaProtocolo:Executar(oConsSitNfe,oConfig)

      ? "XML Retornado pela SEFAZ"
      ? "========================"
      ? oConsultaProtocolo:RetornoWSString
      ?
      ? "Codigo de Status e Motivo"
      ? "========================="
      ? AllTrim(Str(oConsultaProtocolo:Result:CStat,5)), oConsultaProtocolo:Result:XMotivo
      ?
	  
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar consultar o status do servico."
      ? oErro:Description
      ? oErro:Operation
	  
	  //Demonstrar a exceção do CSHARP
	  ?
      ? "Excecao do CSHARP - Message: ", oExceptionInterop:GetMessage()
      ? "Excecao do CSHARP - Codigo: ", oExceptionInterop:GetErrorCode()
      ?
   End	  
   
   Wait
Return

