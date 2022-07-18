Attribute VB_Name = "ManifestacaoNFe"
Option Explicit
Public Sub ManifestarNFe()
On Error GoTo erro
Dim EnvEvento, RecepcaoEvento, Evento, InfEvento, DetEventoManif, CStat

Log.ClearLog

Set RecepcaoEvento = CreateObject("Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento")
Set EnvEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.EnvEvento")
Set Evento = CreateObject("Unimake.Business.DFe.Xml.NFe.Evento")
Set InfEvento = CreateObject("Unimake.Business.DFe.Xml.NFe.InfEvento")
Set DetEventoManif = CreateObject("Unimake.Business.DFe.Xml.NFe.DetEventoManif")

With DetEventoManif
    .Versao = "1.00"
    .DescEvento = "Confirmacao da Operacao"
    .XJust = "Justificativa para manifestação da NFe de teste"
End With
              
With InfEvento
    Set .DetEvento = DetEventoManif
    .COrgao = AN
    .ChNFe = "41191000563803000154550010000020901551010553"
    .CNPJ = "06117473000150"
    .DhEvento = DateTime.Now
    .TpEvento = 210200
    .NSeqEvento = 1
    .VerEvento = "1.00"
    .TpAmb = TpAmb
End With
    
Evento.Versao = "1.00"
Set Evento.InfEvento = InfEvento

EnvEvento.AddEvento (Evento)
EnvEvento.Versao = "1.00"
EnvEvento.IdLote = "000000000000001"

RecepcaoEvento.Executar (EnvEvento), (Config.InicializarConfiguracao(TipoDFe.NFe))

''Gravar o XML de distribuição se a inutilização foi homologada
If (RecepcaoEvento.result.CStat = 128) Then ''128 = Lote de evento processado com sucesso
    CStat = RecepcaoEvento.result.GetRetEvento(0).InfEvento.CStat
    
    '' 135: Evento homologado com vinculação da respectiva NFe
    '' 136: Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
    '' 155: Evento de Cancelamento homologado fora do prazo permitido para cancelamento
                        
    Select Case CStat
        Case 135, 136, 155
            RecepcaoEvento.GravarXmlDistribuicao "C:\temp\"
        Case Else ''Evento rejeitado
            Log.EscreveLog "Evento rejeitado", False
    End Select
End If

Log.EscreveLog RecepcaoEvento.RetornoWSString, True

Exit Sub
erro:
Utility.TrapException

End Sub


