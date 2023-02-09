* ---------------------------------------------------------------------------------
* Testes diversos com certificado digital no xHarbour pago ou versão free (BBC)
* ---------------------------------------------------------------------------------
#IfNdef __XHARBOUR__
   #xcommand TRY => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#endif
 
Function TesteDiversoCertificado()
   Local oCertificado, oCertSel1, oCertSel2, oCertSel3, oCertSel4, oCertSel5, oCertA3
   Local thumbprint, serialNumber
   Local oErro, oExceptionInterop
   
   Try
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Criar objeto para trabalhar com certificados digitais
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Certificado A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que esta instalado no repositorio do windows
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  	   ? "A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que está instalado no repositorio do windows"
	   
	   oCertSel1 = oCertificado:AbrirTelaSelecao()
	   
	   //Voce pode salvar o Thumbprint ou SerialNumber do certificado para salvar em sua base de dados para resgatar ele no futuro no repositório do windows sem precisar abrir tela para selecionar novamente.
	   
	   thumbPrint = oCertificado:GetThumbPrint(oCertSel1)
	   serialNumber = oCertificado:GetSerialNumber(oCertSel1)
	   
	   ? thumbPrint
	   ? serialNumber
	   ?
	   ?
	   wait
	   
	   MostrarDados(oCertificado, oCertSel1)
		   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Somente certificado A1 - Carregar o certificado digital direto do arquivo .PFX.
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   ? "A1 - Carregar o certificado digital direto do arquivo .PFX."
	   
	   oCertSel2 = oCertificado:CarregarCertificadoDigitalA1("c:\projetos\certificados\UnimakePV.pfx", "12345678")
	   
	   MostrarDados(oCertificado, oCertSel2)	   
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       ? "A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number"
	   
	   oCertSel3 = oCertificado:BuscarCertificadoDigital(serialNumber)
	   
	   MostrarDados(oCertificado, oCertSel3)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 * Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   ? "A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint"
	   
	   oCertSel4 = oCertificado:BuscarCertificadoDigital(thumbPrint)
	   
	   MostrarDados(oCertificado, oCertSel4)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     * Certificado A1 - Criando um Base64 do arquivo do certificado para gravar em banco de dados (visando maior seguranca) para resgatar o conteudo direto da base de dados.
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   ? "Transformando um arquivo de certificado A1 em Base64"
	   ?
	   ?
	   
	   certBase64 = oCertificado:ToBase64("C:\Projetos\certificados\UnimakePV.pfx")
	   
	   ? certBase64
	   ?
	   ?
	   wait
	   
	   //Agora você pode gravar o conteúdo da "certBase64" no banco de dados
	   
	   //Recuperar o certificado para uso a partir de um Base64
	   oCertSel5 = oCertificado:FromBase64(certBase64, "12345678")
	   
	   MostrarDados(oCertificado, oCertSel5)
	   
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
     * Certificado A3 - Setar o PIN do A3 para não precisar informar manualmente
	 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       Cls
	   
  	   ? "Setar o PIN do A3 para nao precisar informar manualmente"
	   ?
	   
	   oCertA3 = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")
	   
	   //Ver se é um certificado A3
	   If oCertA3:IsA3(oCertSel1)
	      ? "Eh certificado A3"
		  
		  oCertA3:SetPinPrivateKey(oCertSel1, "123456")
	   else
	      ? "Eh certificado A1"
	   endif
	   
	   ?
	   ?
	   
	   Wait
	   Cls
	 
   Catch oErro

   End	   
Return


Function MostrarDados(oCert, oCertSel)
   Local oVerificarA3
   
   ?
   ? "ID do Certificado....: ", oCert:GetThumbPrint(oCertSel)
   ? "Dados do proprietario: ", oCert:GetSubject(oCertSel)
   ? "Numero de Serie......: ", oCert:GetSerialNumber(oCertSel)
   ? "Validade Inicial.....: ", oCert:GetNotBefore(oCertSel)
   ? "Validade Final.......: ", oCert:GetNotAfter(oCertSel)
   ? "Certificado vencido?.: ", oCert:Vencido(oCertSel)
   
 * Verificar se o certificado é A1 ou A3
   oVerificarA3 = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")
   If oVerificarA3:IsA3(oCertSel)
      ? "Tipo do certificado digital e A3."
   Else
      ? "Tipo do certificado digital e A1."
   Endif
   
   ?
   
   Wait
   Cls
Return