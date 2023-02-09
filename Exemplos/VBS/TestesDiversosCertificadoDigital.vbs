' ----------------------------------------------------
' Testes diversos com certificado digital
' ----------------------------------------------------
Dim oCertificado
Dim oCertSel1
Dim thumbprint
Dim serialNumber
Dim oCertSel2
Dim oCertSel3
Dim oCertSel4

' Criar objeto para trabalhar com certificado digital
Set oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")

' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Certificado A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que esta instalado no repositorio do windows
' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' A1 e A3 - Abrir tela para selecionar o certificado digital que eu desejo trabalhar, certificado que está instalado no repositorio do windows
oCertSel1 = oCertificado.AbrirTelaSelecao()
   
' Voce pode salvar o Thumbprint ou SerialNumber do certificado para salvar em sua base de dados para resgatar ele no futuro no repositório do windows sem precisar abrir tela para selecionar novamente.

thumbprint = oCertificado.GetThumbPrint ((oCertSel1))
serialNumber = oCertificado.GetSerialNumber ((oCertSel1))
MostrarDados (oCertificado), (oCertSel1)

' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Somente certificado A1 - Carregar o certificado digital direto do arquivo .PFX.
' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

' A1 - Carregar o certificado digital direto do arquivo .PFX.
oCertSel2 = oCertificado.CarregarCertificadoDigitalA1 ("C:\Projetos\certificados\UnimakePV.pfx","12345678")
MostrarDados (oCertificado), (oCertSel2)

' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number
' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo Serial Number
oCertSel3 = oCertificado.BuscarCertificadoDigital (serialNumber)
MostrarDados (oCertificado), (oCertSel3)

' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' Certificado A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint
' -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
' A1 e A3 - Buscar o certificado digital, instalado no repositorio do windows, pelo ThumbPrint
oCertSel4 = oCertificado.BuscarCertificadoDigital (thumbprint)
MostrarDados (oCertificado), (oCertSel4)

Function MostrarDados(oCert, oCertSel)
    Dim oVerificarA3
    Dim vencido	
	
	if oCert.Vencido ((oCertSel)) then
	   vencido = "SIM"
	else
	   vencido = "NÃO"
	end if  

	MsgBox "ID do Certificado: " + oCert.GetThumbPrint ((oCertSel))
	MsgBox "Dados do proprietario: " + oCert.GetSubject ((oCertSel))
	MsgBox "Numero de Serie: " + oCert.GetSerialNumber ((oCertSel))
	MsgBox "Validade Inicial: " + oCert.GetNotBefore ((oCertSel))
	MsgBox "Validade Final: " + oCert.GetNotAfter ((oCertSel))
	MsgBox "Certificado vencido: " + Vencido

	Set oVerificarA3 = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")

	If oVerificarA3.IsA3 ((oCertSel)) then
	   MsgBox "Tipo do certificado digital e A3."
	Else
	   MsgBox "Tipo do certificado digital e A1."
	End if
End Function