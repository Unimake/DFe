Attribute VB_Name = "InutilizacaoNumeroNFCe"
Option Explicit
Public Sub InutilizarNumeroNFCe()
On Error GoTo erro
Dim InutNFe, InutNFeInfInut, Inutilizacao

Log.ClearLog

Set InutNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFe")
Set InutNFeInfInut = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")
Set Inutilizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Inutilizacao")

With InutNFeInfInut
    .Ano = "19"
    .CNPJ = "06117473000150"
    .CUF = UFBrasil.PR
    .Mod = 65
    .NNFIni = 57919
    .NNFFin = 57919
    .Serie = 1
    .TpAmb = TpAmb
    .XJust = "Justificativa da inutilizacao de teste"
End With

InutNFe.Versao = "4.00"
Set InutNFe.InfInut = InutNFeInfInut

Inutilizacao.Executar (InutNFe), (Config.InicializarConfiguracao(TipoDFe.NFCe))

Log.EscreveLog Inutilizacao.RetornoWSString, True
Log.EscreveLog Inutilizacao.result.InfInut.XMotivo, False

''retornos da Inutilizacao.result.InfInut.CStat
''    102: //Inutilização homologada
''    Inutilizacao.GravarXmlDistribuicao(@"c:\testenfe\");
''
'' outros //Inutilização rejeitada
''        inutilizacao.GravarXmlDistribuicao(@"c:\testenfe\");

Exit Sub
erro:
Utility.TrapException

End Sub


   
   
   

                

