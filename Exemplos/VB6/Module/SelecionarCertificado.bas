Attribute VB_Name = "SelecionarCertificado"
Option Explicit
Public Function SelecionarCertificado()
Dim selCertificado
Set selCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set SelecionarCertificado = selCertificado.Selecionar()
End Function

Public Function SelecionarCertificadoDeArquivo()
Dim selCertificado      As Variant
Dim caminhoPFX          As String
Dim senhaCertificado    As String
caminhoPFX = GetPFX
senhaCertificado = InputBox("Informe a senha do certificado", "Senha do certificado")

Set selCertificado = CreateObject("Unimake.Security.Platform.CertificadoDigital")
Set SelecionarCertificadoDeArquivo = selCertificado.CarregarCertificadoDigitalA1(caminhoPFX, senhaCertificado)
End Function

Public Function SelecionarDeBase64()
Dim caminhoPFX: caminhoPFX = GetPFX

''Criar o objeto do certificado
Dim cert: Set cert = CreateObject("Unimake.Security.Platform.CertificadoDigital")

''Converte o certificado digital em array byte
Dim bytes: bytes = cert.ToByteArray(caminhoPFX)

''Agora, caso você pecisar gravar em banco de dados, converter para base64
Dim base64: base64 = cert.ToBase64(caminhoPFX)
Debug.Print base64

''Para converter de base64 para o certificado
Dim certFromBase64: Set certFromBase64 = cert.FromBase64(base64, "123456")
''Pronto, só usar o certificado
Set SelecionarDeBase64 = certFromBase64
End Function

Private Function GetPFX() As String
Dim dlg                 As CommonDialog: Set dlg = frmMain.OpenFileDialog

dlg.Filter = "Arquivo PFX (*.pfx)"
dlg.DefaultExt = "pfx"
dlg.DialogTitle = "Selecionar arquivo A1"
dlg.ShowOpen
GetPFX = dlg.FileName
End Function
