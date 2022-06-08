* ---------------------------------------------------------------------------------
* Testes diversos com certificado digital
* ---------------------------------------------------------------------------------
Function TesteDiversoCertificado()
   Local oCertificado, oCertSel1, oCertSel2, oCertSel3, oCertSel4
   Local thumbprint, serialNumber
   
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
   
   thumbprint = oCertificado:GetThumbPrint(oCertSel1)
   serialNumber = oCertificado:GetSerialNumber(oCertSel1)
   MostrarDados(oCertificado, oCertSel1)   
   
 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
 * Somente certificado A1 - Carregar o certificado digital direto do arquivo .PFX.
 * -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   ? "A1 - Carregar o certificado digital direto do arquivo .PFX."
   oCertSel2 = oCertificado:CarregarCertificadoDigitalA1("C:\Projetos\certificados\UnimakePV.pfx", "12345678")
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
   oCertSel4 = oCertificado:BuscarCertificadoDigital(thumbprint)
 

   MostrarDados(oCertificado, oCertSel4)
RETURN

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