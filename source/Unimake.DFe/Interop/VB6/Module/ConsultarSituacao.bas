Attribute VB_Name = "ConsultaSituacaoNFe"
Option Explicit

Public Sub ConsultarSituacaoNFe()
On Error GoTo erro
Dim consSitNFe
Dim consultaProtocolo

Log.ClearLog

Set consSitNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")
consSitNFe.Versao = "4.00"
consSitNFe.TpAmb = TpAmb
consSitNFe.ChNFe = "41200106117473000150550010000606641403753210"

Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo")
consultaProtocolo.Executar (consSitNFe), (Config.InicializarConfiguracao(TipoDFe.NFe))

Log.EscreveLog consultaProtocolo.RetornoWSString, True
Log.EscreveLog consultaProtocolo.result.XMotivo, True

Exit Sub
erro:
Utility.TrapException

End Sub

