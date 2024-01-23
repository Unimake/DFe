Attribute VB_Name = "ConsultaReciboNFCe"
Option Explicit
Public Sub ConsultarReciboNFCe()
On Error GoTo erro
Dim ConsReciNFe, retAutorizacao

Log.ClearLog

Set ConsReciNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsReciNFe")
Set retAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.RetAutorizacao")

With ConsReciNFe
    .Versao = "4.00"
    .TpAmb = TpAmb
    .NRec = UFBrasil.PR & "3456789012345"
End With

retAutorizacao.Executar (ConsReciNFe), (Config.InicializarConfiguracao(TipoDFe.NFCe))

Log.EscreveLog retAutorizacao.RetornoWSString, True
Log.EscreveLog retAutorizacao.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException

End Sub



