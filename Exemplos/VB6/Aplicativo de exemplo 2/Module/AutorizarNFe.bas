Attribute VB_Name = "ServicosNFe"
Option Explicit
Public Sub ValidarXML()
On Error GoTo erro
Dim validarSchema: Set validarSchema = CreateObject("Unimake.Business.DFe.ValidarSchema")
Dim schema: schema = "NFe.nfe_v4.00.xsd"

validarSchema.Validar "D:\Temp\DFe_ZIPFile_2021_03_02_11_58_41_-1133296159\41210206268208000172650020000031071794904016-procnfe.xml", _
                        schema, _
                        "http://www.portalfiscal.inf.br/nfe"

If validarSchema.Success Then
    MsgBox "XML validado com sucesso.", vbOKOnly + vbInformation
Else
    MsgBox "Code: " & validarSchema.errorCode & vbCrLf & "Message: " + validarSchema.ErrorMessage, vbOKOnly + vbCritical
End If

Exit Sub
erro:
Utility.TrapException
End Sub


Public Sub AutorizarPorArquivoNFe()
On Error GoTo erro
Dim EnviNFe:        Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
Dim Autorizacao:    Set Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")

Log.ClearLog

EnviNFe.Versao = "4.00"
EnviNFe.IdLote = "000000000000001"
EnviNFe.IndSinc = 1
EnviNFe.AddNFe (GetFromFileNFe())

Autorizacao.Executar (EnviNFe), (Config.InicializarConfiguracao(TipoDFe.NFe))

Log.EscreveLog Autorizacao.RetornoWSString, True
Log.EscreveLog Autorizacao.result.XMotivo, False

GravarXmlDistribuicao Autorizacao

Exit Sub
erro:
Utility.TrapException

End Sub


Public Sub AutorizarNFe()
On Error GoTo erro
Dim EnviNFe:        Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
Dim Autorizacao:    Set Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")

Log.ClearLog

EnviNFe.Versao = "4.00"
EnviNFe.IdLote = "000000000000001"
EnviNFe.IndSinc = 1
EnviNFe.AddNFe GetNFe()

Autorizacao.Executar (EnviNFe), (Config.InicializarConfiguracao(TipoDFe.NFe))

Log.EscreveLog Autorizacao.RetornoWSString, True
Log.EscreveLog Autorizacao.result.XMotivo, False

GravarXmlDistribuicao Autorizacao

Exit Sub
erro:
Utility.TrapException

End Sub

Sub GravarXmlDistribuicao(Autorizacao)
If Not Autorizacao.result.ProtNFe Is Nothing Then
    'Gravar o XML de distribuição
    Dim CStat: CStat = Autorizacao.result.ProtNFe.InfProt.CStat

    If CStat = 100 Or _
       CStat = 110 Or _
       CStat = 150 Or _
       CStat = 205 Or _
       CStat = 301 Or _
       CStat = 302 Or _
       CStat = 303 Then
            Autorizacao.GravarXmlDistribuicao "D:\Temp\Uninfe\"
    End If

End If
End Sub


Function GetFromFileNFe()
Dim NFe: Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
Set GetFromFileNFe = NFe.LoadFromFile("Z:\uninfe\exemplos\Antigos\NFe e NFCe 3.10\NFCe\51140499999999999999650010000000121123456788-nfe.xml")
End Function

Function GetNFe()
Dim NFe
Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
NFe.AddInfNFe GetInfNFe()
Set GetNFe = NFe
End Function

                        
Function GetInfNFe()
Dim infNFe
Set infNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
infNFe.Versao = "4.00"
Set infNFe.Ide = GetIde()
Set infNFe.Emit = GetEmit()
Set infNFe.Dest = GetDest()
Set infNFe.Total = GetTotal()
Set infNFe.Transp = GetTransp()
Set infNFe.Cobr = GetCobr()
Set infNFe.Pag = GetPag()
Set infNFe.InfAdic = GetInfAdic()
Set infNFe.InfRespTec = GetInfRespTec()
infNFe.AddDet GetDet()
Set GetInfNFe = infNFe
End Function

Function GetIde()
Dim result
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
With result
    .CUF = UFBrasil.PR
    .NatOp = "VENDA PRODUC.DO ESTABELEC"
    .Mod = 55
    .Serie = 1
    .NNF = 57966
    .DhEmi = Now
    .DhSaiEnt = Now
    .TpNF = 1
    .IdDest = 2
    .CMunFG = 4118402
    .TpImp = 1
    .TpEmis = 1
    .TpAmb = TpAmb
    .FinNFe = 1
    .IndFinal = 1
    .IndPres = IndicadorPresenca.NaoSeAplica
    .IndIntermed = IndicadorIntermediario.OperacaoSemIntermediador
    .ProcEmi = 0
    .VerProc = "TESTE 1.00"
End With
Set GetIde = result
End Function

Function GetEmit()
Dim result
Dim ender
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
With result
    .CNPJ = "06117473000150"
    .XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
    .XFant = "UNIMAKE - PARANAVAI"
    .IE = "9032000301"
    .IM = "14018"
    .CNAE = "6202300"
    .CRT = 1
End With

Set ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
With ender
    .XLgr = "RUA ANTONIO FELIPE"
    .Nro = "1500"
    .XBairro = "CENTRO"
    .CMun = 4118402
    .XMun = "PARANAVAI"
    .UF = 41
    .CEP = "87704030"
    .Fone = "04431414900"
End With

Set result.EnderEmit = ender

Set GetEmit = result
End Function

Function GetDest()
Dim result
Dim ender
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")

With result
    .CNPJ = "04218457000128"
    .XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    .IndIEDest = 1
    .IE = "582614838110"
    .Email = "janelaorp@janelaorp.com.br"
End With

Set ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
With ender
    .XLgr = "AVENIDA DA SAUDADE"
    .Nro = "1555"
    .XBairro = "CAMPOS ELISEOS"
    .CMun = 3543402
    .XMun = "RIBEIRAO PRETO"
    .UF = 35
    .CEP = "14080000"
    .Fone = "01639611500"
End With

Set result.EnderDest = ender

Set GetDest = result
End Function

Function GetTotal()
Dim result, ICMSTot
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")
Set ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
With ICMSTot
    .VBC = 0
    .VICMS = 0
    .VICMSDeson = 0
    .VFCP = 0
    .VBCST = 0
    .VST = 0
    .VFCPST = 0
    .VFCPSTRet = 0
    .VProd = 140.3
    .VFrete = 0
    .VSeg = 0
    .VDesc = 0
    .VII = 0
    .VIPI = 0
    .VIPIDevol = 0
    .VPIS = 0
    .VCOFINS = 0
    .VOutro = 0
    .VNF = 140.3
    .VTotTrib = 12.63
End With
Set result.ICMSTot = ICMSTot

Set GetTotal = result
End Function

Function GetTransp()
Dim result, Transporta, Vol
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
Set Transporta = CreateObject("Unimake.Business.DFe.Xml.NFe.Transporta")
Set Vol = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")

With Transporta
    .XNome = "RETIRADO PELO CLIENTE"
    .XEnder = "RUA RIO DE JANEIRO"
    .XMun = "POCOS DE CALDAS"
    .UF = 31
End With
 
With Vol
    .QVol = 2
    .Esp = "VOLUMES"
    .Marca = "CAIXAS"
    .PesoL = 0#
    .PesoB = 0#
End With

result.ModFrete = 1
Set result.Transporta = Transporta
result.AddVol (Vol)

Set GetTransp = result
End Function

Function GetCobr()
Dim result, Dup, Fat
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
Set Dup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
Set Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")

With Fat
    .NFat = "151342"
    .VOrig = 140.3
    .VDesc = 0
    .VLiq = 140.3
End With
Set result.Fat = Fat

With Dup
    .NDup = "001"
    .DVenc = Now
    .VDup = 140.3
End With
result.AddDup (Dup)
    
Set GetCobr = result
End Function

Function GetPag()
Dim result, DetPag
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
Set DetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")

With DetPag
    .TPag = 15
    .VPag = 140.3
End With
DetPag.SetIndPag 1

result.AddDetPag (DetPag)
Set GetPag = result
End Function

Function GetInfAdic()
Dim result
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
result.InfCpl = ";Trib aprox: Federal Estadual Municipal ; Trib aprox: Federal Estadual Municipal Fonte: ;"
Set GetInfAdic = result
End Function

Function GetInfRespTec()
Dim result
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
With result
    .CNPJ = "05413671000106"
    .XContato = "Oduvaldo de Oliveira"
    .Email = "oduvaldo@visualsistemas.net"
    .Fone = "3537215351"
End With
Set GetInfRespTec = result
End Function

Function GetDet()
Dim result, Prod, Imposto, ICMS, ICMSSN101, PIS, PISOutr, COFINS, COFINSOutr

Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
result.NItem = 1

Set Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
With Prod
    .CProd = "01042"
    .CEAN = "SEM GTIN"
    .XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    .NCM = "84714900"
    .CFOP = "6101"
    .UCom = "LU"
    .QCom = 1#
    .VUnCom = 140.3
    .VProd = 140.3
    .CEANTrib = "SEM GTIN"
    .UTrib = "LU"
    .QTrib = 1#
    .VUnTrib = 140.3
    .IndTot = 1
    .XPed = "300474"
    .NItemPed = 1
End With
Set result.Prod = Prod

Set Imposto = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
Imposto.VTotTrib = 12.63

Set ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
Set ICMSSN101 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")

With ICMSSN101
    .Orig = 0
    .PCredSN = 2.8255
    .VCredICMSSN = 2.4
End With

Set ICMS.ICMSSN101 = ICMSSN101
Set Imposto.ICMS  = ICMS
                                                    
Set PIS = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")
Set PISOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")

With PISOutr
    .CST = "99"
    .VBC = 0#
    .PPIS = 0#
    .VPIS = 0#
End With

Set PIS.PISOutr = PISOutr
Set Imposto.PIS = PIS

Set COFINS = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")
Set COFINSOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")

With COFINSOutr
    .CST = "99"
    .VBC = 0#
    .PCOFINS = 0#
    .VCOFINS = 0#
End With

Set COFINS.COFINSOutr = COFINSOutr
Set Imposto.COFINS = COFINS
Set result.Imposto = Imposto

Set GetDet = result
End Function
