Attribute VB_Name = "ConsultaSituacaoNFCe"
Option Explicit

Public Sub ConsultarSituacaoNFCe()
On Error GoTo erro
Dim consSitNFe
Dim consultaProtocolo

Log.ClearLog

Set consSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
consSitNFe.Versao = "4.00"
consSitNFe.TpAmb = TpAmb
consSitNFe.ChNFe = "31191222204648000384650010000001881000524048"

Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo")
consultaProtocolo.Executar (consSitNFe), (Config.InicializarConfiguracao(TipoDFe.NFCe))

Log.EscreveLog consultaProtocolo.RetornoWSString, True
Log.EscreveLog consultaProtocolo.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException

End Sub


