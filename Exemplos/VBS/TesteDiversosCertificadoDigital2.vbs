Dim oCertificado
Dim oCertSelA3 
Dim oTipoCert 
Dim serialNumber

serialNumber = "7b36c4cefcd0f2b9"

' Criar objeto para trabalhar com certificado digital
Set oCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")

Set oCertSelA3 = oCertificado.BuscarCertificadoDigital(serialNumber)

Set oTipoCert = CreateObject("Unimake.Business.DFe.Security.ClsX509Certificate2ExtensionInterop")

If oTipoCert.IsA3((oCertSelA3)) then
   MsgBox "E um certificado A3"
Else
   MsgBox "E um certificado A1"
End if

MsgBox oCertificado.GetThumbPrint ((oCertSelA3))

