Attribute VB_Name = "ConsultaStatusCTe"
Option Explicit

Public Sub ConsultarStatusCTe()
On Error GoTo erro
Dim ConsStatServCte
Dim statusServico

Log.ClearLog

Set ConsStatServCte = CreateObject("Unimake.Business.DFe.Xml.CTe.ConsStatServCte")
Set statusServico = CreateObject("Unimake.Business.DFe.Servicos.CTe.StatusServico")

ConsStatServCte.Versao = "3.00"
ConsStatServCte.TpAmb = TpAmb
statusServico.Executar (ConsStatServCte), (Config.InicializarConfiguracao(TipoDFe.CTe, UFBrasil.PR))

Log.EscreveLog statusServico.RetornoWSString, True
Log.EscreveLog statusServico.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException
End Sub


