* ---------------------------------------------------------------------------------
* Consultar MDFes não encerrados
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function ConsultaMDFeNaoEnc()
   Local oErro, oExceptionInterop
   Local oConfiguracao, oConsMDFeNaoEnc, oConsNaoEnc
   
 * Criar configuraçao básica para consumir o serviço
   oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oConfiguracao:TipoDfe = 4 && 4=MDFe
   oConfiguracao:CertificadoSenha = "12345678"
   oConfiguracao:CodigoUF = 41 && UFBrasil.PR
   oConfiguracao:CertificadoArquivo = "D:\projetos\certificados\TESTECERT.pfx"

 * Criar o XML de consulta
   oConsMDFeNaoEnc = CreateObject("Unimake.Business.DFe.Xml.MDFe.ConsMDFeNaoEnc")
   oConsMDFeNaoEnc:Versao = "3.00"
   oConsMDFeNaoEnc:TpAmb = 1 && TipoAmbiente.Homologacao
   oConsMDFeNaoEnc:CNPJ = "00000000000000"

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try 
    * Enviar a consulta
      oConsNaoEnc = CreateObject("Unimake.Business.DFe.Servicos.MDFe.ConsNaoEnc")
      oConsNaoEnc:Executar(oConsMDFeNaoEnc, oConfiguracao)
	  
      ? "XML retornado pela SEFAZ:"
	  ?
      ? oConsNaoEnc:RetornoWSString
	  ?
	  ?
	  wait
	  cls
	  
	  ? "Chaves e protocolos dos MDFes autorizados e nao encerrados"
	  ?
	  For X := 1 To oConsNaoEnc:Result:GetInfMDFeCount()
	      oMDFe = oConsNaoEnc:Result:GetInfMDFe(X-1)
		  
		  ? oMDFe:chMDFe + " " + oMDFe:NProt
	  Next X
	  ?
	  ?
	  Wait
	  Cls

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