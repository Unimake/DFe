Attribute VB_Name = "Config"

Public Function InicializarConfiguracao(ByVal pTipoDFe As TipoDFe, Optional ByVal pCUF = 0)
Static flagCertificado As Boolean

If flagCertificado = False Then
    flagCertificado = True
End If

Set InicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
InicializarConfiguracao.TipoDFe = CInt(pTipoDFe)

If pCUF > 0 Then InicializarConfiguracao.CodigoUF = pCUF

InicializarConfiguracao.CertificadoSenha = "12345678"
InicializarConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

End Function
