* ---------------------------------------------------------------------------------
* Encriptar assinatura da NFSe do município de São Paulo
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function EncriptarAssinaturaSP()
   Local oErro, oExceptionInterop, oConverterInterop
   Local oCertificado, assinatura, assinaturaEncriptada, certificadoSelecionado

   // Criar objeto para pegar exceção do lado do CSHARP
   oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")   
   
   Try   
    * Carregar o certificado digital a ser utilizado na encriptação
	  oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigitalInterop")
	  
      oCertificado:CarregarCertificadoDigitalA1("c:\projetos\certificados\UnimakePV.pfx", "12345678")
	  cSerialNumber = oCertificado:GetSerialNumber()
   
    * Criar objetoda classe de encriptação
      oConverterInterop = CreateObject("Unimake.Business.DFe.Utility.ConverterInterop")
   
    * Assinatura
      assinatura = "31000000OL03 00000000000120070103TNN00000000205000000000000050000002658100013167474254"
   
      ? assinatura
      ?
      ?
      wait
	  
	  ? cSerialNumber
	  ?
	  ?
	  Wait
   
    * Encriptar
      assinaturaEncriptada = oConverterInterop:ToRSASHA1SerialNumber(cSerialNumber, assinatura)
   
      ?
	  ?
      ? assinaturaEncriptada
      ?
      ? 
      wait
      cls

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