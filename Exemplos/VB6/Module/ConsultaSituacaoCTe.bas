Attribute VB_Name = "ConsultaSituacaoCTe"
Option Explicit

Public Sub ConsultarSituacaoCTe()
On Error GoTo erro
Dim ConsSitCTe
Dim consultaProtocolo

Log.ClearLog

Set ConsSitCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.ConsSitCTe")
ConsSitCTe.Versao = "3.00"
ConsSitCTe.TpAmb = TpAmb
ConsSitCTe.ChCTe = UFBrasil.PR & "170701761135000132570010000186931903758906"

Set consultaProtocolo = CreateObject("Unimake.Business.DFe.Servicos.CTe.ConsultaProtocolo")
consultaProtocolo.Executar (ConsSitCTe), (Config.InicializarConfiguracao(TipoDFe.CTe))

Log.EscreveLog consultaProtocolo.RetornoWSString, True
Log.EscreveLog consultaProtocolo.result.XMotivo, False

Exit Sub
erro:
Utility.TrapException

End Sub



