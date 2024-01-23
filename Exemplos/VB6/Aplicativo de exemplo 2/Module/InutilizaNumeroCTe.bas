Attribute VB_Name = "InutilizaNumeroCTe"
Option Explicit
Public Sub InutilizarNumeroCTe()
On Error GoTo erro
Dim InutCTe, InutCTeInfInut, Inutilizacao

Log.ClearLog

Set InutCTe = CreateObject("Unimake.Business.DFe.Xml.CTe.InutCTe")
Set InutCTeInfInut = CreateObject("Unimake.Business.DFe.Xml.CTe.InutCTeInfInut")
Set Inutilizacao = CreateObject("Unimake.Business.DFe.Servicos.CTe.Inutilizacao")

With InutCTeInfInut
    .Ano = "19"
    .CNPJ = "06117473000150"
    .CUF = UFBrasil.PR
    .Mod = 57
    .NCTIni = 57919
    .NCTFin = 57919
    .Serie = 1
    .TpAmb = TpAmb
    .XJust = "Justificativa da inutilizacao de teste"
End With

InutCTe.Versao = "3.00"
Set InutCTe.InfInut = InutCTeInfInut

Inutilizacao.Executar (InutCTe), (Config.InicializarConfiguracao(TipoDFe.CTe))

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


   
   
   

                


