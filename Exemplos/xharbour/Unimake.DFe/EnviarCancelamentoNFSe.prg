* ---------------------------------------------------------------------------------
* Enviar Cancelamento NFSe para prefeitura
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EnviarCancelamentoNFSe()
   Local oExceptionInterop, oErro, oConfiguracao
   Local oCancelamentoNfe, cArqXML, cStr
   
 * Criar o objeto de configuração mínima
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDFe = 5 //5=NFSe
   oConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:Servico = 46 //Servico.NFSeCancelamentoNfe
   oConfiguracao:CodigoMunicipio = 3550308 //São Paulo  
   oConfiguracao:SchemaVersao = "2.00"
   oConfiguracao:TipoAmbiente = 1 //TipoAmbiente.Producao

 * Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   

   Try         
      cArqXML := "D:\testenfe\xharbour\Unimake.DFe\CancelamentoNfe-ped-cannfse.xml"
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
    
	  oCancelamentoNfe := CreateObject("Unimake.Business.DFe.Servicos.NFSe.CancelamentoNfe")
      oCancelamentoNfe:Executar(cStr, oConfiguracao)
	  
	  ? oCancelamentoNfe:RetornoWSString
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