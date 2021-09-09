Attribute VB_Name = "SelecionarCertificado"
Public Function SelecionarCertificado()
Dim selCertificado
Set selCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set SelecionarCertificado = selCertificado.Selecionar()
End Function


Public Function SelecionarCertificadoDeArquivo()
Dim selCertificado      As Variant
Dim caminhoPFX          As String
Dim senhaCertificado    As String
Dim dlg                 As CommonDialog: Set dlg = frmMain.OpenFileDialog

dlg.Filter = "Arquivo PFX (*.pfx)"
dlg.DefaultExt = "pfx"
dlg.DialogTitle = "Selecionar arquivo A1"
dlg.ShowOpen

caminhoPFX = dlg.FileName
senhaCertificado = InputBox("Informe a senha do certificado", "Senha do certificado")

Set selCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set SelecionarCertificadoDeArquivo = selCertificado.CarregarCertificadoDigitalA1(caminhoPFX, senhaCertificado)
End Function




