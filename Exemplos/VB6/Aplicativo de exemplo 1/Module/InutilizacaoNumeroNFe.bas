Attribute VB_Name = "InutilizacaoNumeroNFe"
Option Explicit
Public Sub InutilizarNumeroNFe()
On Error GoTo erro
Dim InutNFe, InutNFeInfInut, Inutilizacao

Log.ClearLog

Set InutNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFe")
Set InutNFeInfInut = CreateObject("Unimake.Business.DFe.Xml.NFe.InutNFeInfInut")
Set Inutilizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Inutilizacao")

With InutNFeInfInut
    .Ano = "19"
    .CNPJ = "06117473000150"
    .CUF = UFBrasil.PR
    .Mod = 55
    .NNFIni = 57919
    .NNFFin = 57919
    .Serie = 1
    .TpAmb = TpAmb
    .XJust = "Justificativa da inutilizacao de teste"
End With

InutNFe.Versao = "4.00"
Set InutNFe.InfInut = InutNFeInfInut

Inutilizacao.Executar (InutNFe), (Config.InicializarConfiguracao(TipoDFe.NFe))

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


   
   
   

                
