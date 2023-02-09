Attribute VB_Name = "ConsultaReciboNFe"
Option Explicit
Public Sub ConsultarReciboNFe()
On Error GoTo erro
Dim ConsReciNFe, retAutorizacao

Log.ClearLog

Set ConsReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
Set retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.RetAutorizacao")

With ConsReciNFe
    .Versao = "4.00"
    .TpAmb = TpAmb
    .NRec = "310000069231900"
End With

retAutorizacao.Executar (ConsReciNFe), (Config.InicializarConfiguracao(TipoDFe.NFe))

Log.EscreveLog retAutorizacao.RetornoWSString, True
Log.EscreveLog retAutorizacao.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException

End Sub


