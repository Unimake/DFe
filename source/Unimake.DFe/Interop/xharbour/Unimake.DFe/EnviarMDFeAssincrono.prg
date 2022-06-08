* ---------------------------------------------------------------------------------
* Enviar MDFe de forma assincrona
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarMDFeAssincrono()
   Local oConfiguracao, oEnviMDFe, oCertificado, oCertificadoSelecionado, oAutorizacao, oErro

   //Carregar o certificado digital
   oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
   oCertificadoSelecionado = oCertificado:AbrirTelaSelecao()

   //Montar o objeto de configuração com informações mínimas 
   //para ser utilizado na hora de consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 4    //4 = MDFe
   oConfiguracao:CertificadoDigital = oCertificadoSelecionado

   //Criar o XML do MDFe
   oEnviMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.EnviMDFe")
   oEnviMDFe:Versao = "3.00"
   oEnviMDFe:IdLote = "000000000000001"
   oEnviMDFe:MDFe = GetFromFileMDFe() //Ler XML de um arquivo que foi elaborado anteriormente 

   //Resgatar algumas informações do objeto do XML
   ? "Versao schema: " + oEnviMDFe:Versao
   ? "Numero lote: " + oEnviMDFe:IdLote
   ? "Nome Emitente: " + oEnviMDFe:MDFe:InfMDFe:Emit:XNome
   ? "Chave MDFe: " + oEnviMDFe:MDFe:InfMDFe:Chave
   
   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   
   Try
      //Criar serviço de autorização e enviar o XML para SEFAZ
      oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.MDFe.Autorizacao")                                 
      oAutorizacao:SetXMLConfiguracao(oEnviMDFe, oConfiguracao)
      oAutorizacao:Executar(oEnviMDFe, oConfiguracao)

      //Demonstrar o XML retornado pela SEFAZ
      ? oAutorizacao:RetornoWSString
	  Wait
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
	  
	  Wait
	  cls
   End 
Return   

//----------------------------------------------------
//Serializar o XML do MDFe
//----------------------------------------------------
Function GetFromFileMDFe()
   Local oMDFe, Retorna
   
   oMDFe = CreateObject("Unimake.Business.DFe.Xml.MDFe.MDFe")
   Retorna = oMDFe:LoadFromFile("C:\Users\Wandrey\Downloads\Telegram Desktop\31220437920833000180580010000000021000000020-mdfe.xml")
Return Retorna