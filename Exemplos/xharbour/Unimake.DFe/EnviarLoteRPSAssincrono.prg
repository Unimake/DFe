* ---------------------------------------------------------------------------------
* Enviar NFSe para prefeitura
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarLoteRPSAssincrono()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oEnvioRps, cArqXML, cStr
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 5 //5=NFSe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 45 //Servico.NFSeEnvioRps
   oConfiguracao:CodigoMunicipio = 3550308 //São Paulo  
   oConfiguracao:SchemaVersao = "2.00"
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      cArqXML := "D:\testenfe\xharbour\Unimake.DFe\EnviarLoteRpsEnvio-env-loterps.xml"
	  cStr := Memoread(cArqXML)
	  cStr := SubStr(cStr, 4)
	  
	  ? "String do XML:"
	  ?
	  ?
	  ? cStr
	  ?
	  ?
	  wait
	  cls 
    
	  oEnvioRPS := CreateObject("Unimake.Business.DFe.Servicos.NFSe.EnvioRps")
      oEnvioRPS:Executar(cStr, oConfiguracao)
	  
	  ? oEnvioRPS:RetornoWSString
	  ?
	  ?
	  Wait
	  Cls
   
   Catch oErro
      //Demonstrar exceções geradas no proprio Harbour, se existir.
	  ? "ERRO"
	  ? "===="
	  ? "Falha ao tentar enviar a NFSe."
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