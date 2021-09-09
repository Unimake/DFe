Attribute VB_Name = "ConsultaContribuinteNFCe"
Option Explicit

Public Sub ConsultarContribuinteNFCe()
On Error GoTo erro
Dim ConsCad, InfCons, ConsultaCadastro

Log.ClearLog

Set ConsCad = CreateObject("Unimake.Business.DFe.Xml.NFe.ConsCad")
Set InfCons = CreateObject("Unimake.Business.DFe.Xml.NFe.InfCons")
Set ConsultaCadastro = CreateObject("Unimake.Business.DFe.Servicos.NFCe.ConsultaCadastro")

With InfCons
    .CNPJ = "06117473000150"
    .UF = UFBrasil.PR
End With

ConsCad.Versao = "2.00"
Set ConsCad.InfCons = InfCons

ConsultaCadastro.Executar (ConsCad), (Config.InicializarConfiguracao(TipoDFe.NFCe))

Log.EscreveLog ConsultaCadastro.RetornoWSString, True
Log.EscreveLog ConsultaCadastro.result.InfCons.XMotivo, False

Exit Sub
erro:
Utility.TrapException

End Sub


