Attribute VB_Name = "ConsultaStatusNFCe"
Option Explicit

Public Sub ConsultarStatusNFCe()
On Error GoTo erro
Dim consStatServ
Dim statusServico

Log.ClearLog

Set consStatServ = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsStatServ")
Set statusServico = CreateObject("Unimake.Business.DFe.Servicos.NFCe.StatusServico")

consStatServ.Versao = "4.00"
consStatServ.CUF = UFBrasil.PR
consStatServ.TpAmb = TpAmb
statusServico.Executar (consStatServ), (Config.InicializarConfiguracao(TipoDFe.NFCe))

Log.EscreveLog statusServico.RetornoWSString, True
Log.EscreveLog statusServico.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException
End Sub

