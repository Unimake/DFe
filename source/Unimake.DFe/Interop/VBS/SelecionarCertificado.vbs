Dim selCertificado 
Dim caminhoPFX     
Dim senhaCertificado 
Dim cert

caminhoPFX = "D:\Dropbox\Unimake\Unimake_PV.pfx"
senhaCertificado = "123456"

Set selCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set cert = selCertificado.CarregarCertificadoDigitalA1(caminhoPFX, senhaCertificado)
selCertificado.Vencido (cert)
MsgBox "Certificado selecionado com sucesso.", vbInformation + vbOKOnly, "Aviso!"

