Attribute VB_Name = "ConsultaGTIN"
Option Explicit
Public Sub ConsultarGTIN()
On Error GoTo erro
Dim consGTIN
Dim ccgConsGTIN
Dim configuracao

Log.ClearLog

Set configuracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
configuracao.TipoDFe = 10 ''TipoDFe.CCG
configuracao.TipoAmbiente = 1 ''TipoAmbiente.Producao
configuracao.CertificadoArquivo = "c:\projetos\unimake_pv.pfx"
configuracao.CertificadoSenha = "12345678"
''configuracao.CertificadoSerialNumberOrThumbPrint = "1231231231221"

Set consGTIN = CreateObject("Unimake.Business.DFe.Xml.CCG.ConsGTIN")
consGTIN.Versao = "1.00"
consGTIN.GTIN = "7896714200217"

Set ccgConsGTIN = CreateObject("Unimake.Business.DFe.Servicos.CCG.CcgConsGTIN")
ccgConsGTIN.Executar (consGTIN), (configuracao)


Log.EscreveLog ccgConsGTIN.RetornoWSString, False
Log.EscreveLog ccgConsGTIN.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException
End Sub
