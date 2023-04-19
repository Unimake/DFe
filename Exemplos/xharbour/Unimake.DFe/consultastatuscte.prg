* ---------------------------------------------------------------------------------
* Consumindo o serviço de consulta status da CTe
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif

Function ConsultaStatusCTe()
   Local oConfig
   Local oConsStatServCTe, oErro, oExceptionInterop
   Local oStatusServico
   
 * Criar configuração básica para consumir o serviço
   oConfig = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfig:TipoDFe = 2 //0=CTe
   oConfig:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfig:CertificadoSenha = "12345678"
   oConfig:TipoEmissao = 1 //1=Normal
   oConfig:CodigoUF = 41 //Paraná (No caso do CTe temos que definir a UF nas configurações pois o XML de consulta status não tem e não temos como saber de qual estado consultar.
   
 * Criar XML   
   oConsStatServCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.ConsStatServCTe")
   oConsStatServCTe:Versao = "3.00"
   oConsStatServCTe:TpAmb = 2 //2=Homologação
   
   //Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
   
   Try
    * Consumir o serviço
	  oStatusServico = CreateObject("Unimake.Business.DFe.Servicos.CTe.StatusServico")
	  oStatusServico:Executar(oConsStatServCTe, oConfig)

	  ? "XML Retornado pela SEFAZ"
      ? "========================"
      ? oStatusServico:RetornoWSString
      ?
      ? "Codigo de Status e Motivo"
      ? "========================="
	  ? Alltrim(Str(oStatusServico:Result:CStat,5)), oStatusServico:Result:XMotivo
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