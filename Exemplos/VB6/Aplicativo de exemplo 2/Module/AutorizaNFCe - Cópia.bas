Attribute VB_Name = "ServicosNFCe"
Option Explicit
Public Sub AutorizarNFCe()
    On Error GoTo erro
    
    Dim rsTabNFCePendentes      As New ADODB.Recordset
    
    Dim EnviNFe
    Dim Autorizacao
    Dim localConfig
    
    Log.ClearLog
    
    Set rsTabNFCePendentes = fcDB_OpenRecordSet("Select top 1 nacesso, nfiscal, cserie from tabcadnfce where csituacao = 'P' order by tabcadnfce.nacesso ASC ")
    If Not rsTabNFCePendentes.EOF Then
    
        Set localConfig = Config.InicializarConfiguracao(TipoDFe.NFCe)
        
        localConfig.CSC = "8E359BBE-CBB3-4445-A6C6-B73D35B9EEA9"
        localConfig.CSCIDToken = 1
        
        Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
        EnviNFe.Versao = "4.00"
        EnviNFe.IdLote = Format(rsTabNFCePendentes!nacesso, "000000000000000")
        EnviNFe.IndSinc = 1
        EnviNFe.AddNFe GetNFCe(rsTabNFCePendentes!nfiscal, rsTabNFCePendentes!cSerie)
        
        Set Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")
        
        Autorizacao.Executar (EnviNFe), (localConfig)
        
        If Autorizacao.result.CStat = 104 Then  '104 = Lote Processado
           If Autorizacao.result.ProtNFe.InfProt.CStat = 100 Then '100 = Autorizado o uso da NF-e
           
             'Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
              Autorizacao.GravarXmlDistribuicao App.Path
          
              'Capturando Chave da Nota
              Debug.Print Autorizacao.result.ProtNFe.InfProt.ChNFe
              
             'Como pegar o numero do protocolo de autorizacao para gravar na base
              Debug.Print Autorizacao.result.ProtNFe.InfProt.NProt
              
             'Pegar a string do XML de distribuição para gravar em uma base de dados
              Debug.Print Autorizacao.GetNFeProcResults(Autorizacao.result.ProtNFe.InfProt.ChNFe)
              
              
           Else
             'Rejeitada ou Denegada - Fazer devidos tratamentos
           End If
        Else
          'Lote nao processado - Fazer devidos tratamentos
        End If
        
        
        Log.EscreveLog Autorizacao.RetornoWSString, True
        Log.EscreveLog Autorizacao.result.XMotivo, False
    End If
    rsTabNFCePendentes.Close
    Set rsTabNFCePendentes = Nothing
    
    Exit Sub
erro:
    Utility.TrapException
End Sub

Function GetNFCe(ByVal p_lngNotaFiscal As Long, ByVal p_strSerie As String)
    On Error GoTo erro
    
    Dim NFe
    Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
    NFe.AddInfNFe GetInfNFe(p_lngNotaFiscal, p_strSerie)
    Set GetNFCe = NFe
    Exit Function
erro:
    Utility.TrapException
End Function

                        
Function GetInfNFe(ByVal p_lngNotaFiscal As Long, ByVal p_strSerie As String)
    On Error GoTo erro
    
    Dim infNFe
    Set infNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
    infNFe.Versao = "4.00"
    Set infNFe.Ide = GetIde(p_lngNotaFiscal, p_strSerie)
    Set infNFe.Emit = GetEmit()
    Set infNFe.Dest = GetDest()
    Set infNFe.Total = GetTotal()
    Set infNFe.Transp = GetTransp()
    'Set infNFe.Cobr = GetCobr()
    Set infNFe.Pag = GetPag()
    Set infNFe.InfAdic = GetInfAdic()
    Set infNFe.InfRespTec = GetInfRespTec()
    infNFe.AddDet GetDet()
    Set GetInfNFe = infNFe
    
    Exit Function
erro:
    Utility.TrapException
End Function

Function GetIde(ByVal p_lngNotaFiscal As Long, ByVal p_strSerie As String)
    On Error GoTo erro
    
    Dim result
    
    Dim nCodigo_IBGE As Long
    Dim nCodigo_UF As Integer
    
    nCodigo_IBGE = fcRetornaDadosDaTabelaFARQ("IBGE")
    If nCodigo_IBGE = 0 Then nCodigo_IBGE = 2611606
    
    nCodigo_UF = Mid(nCodigo_IBGE, 1, 2)
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
    With result
        .CUF = nCodigo_UF
        .NatOp = "VENDA PRODUC.DO ESTABELEC"
        .Mod = 65
        .Serie = p_strSerie
        .NNF = p_lngNotaFiscal
        .DhEmi = Now
        .TpNF = 1
        .IdDest = 1
        .CMunFG = nCodigo_IBGE
        .TpImp = 4 ''nfce
        .TpEmis = 1
        
        '.TpAmb = TpAmb
        If Val(fcRetornaConteudoDaTabGeral("NfeProdHom")) = 1 Then
            .TpAmb = TipoAmbiente.Producao
        Else
            .TpAmb = TipoAmbiente.Homologacao
        End If
        
        .FinNFe = p_lngNotaFiscal
        .IndFinal = 1
        .IndPres = IndicadorPresenca.OperacaoPresencial
        .IndIntermed = IndicadorIntermediario.OperacaoSemIntermediador
        .ProcEmi = 0
        .VerProc = App.Major & "." & App.Minor & "." & Format(App.Revision, "000")
    End With
    Set GetIde = result
    
    Exit Function
erro:
    Utility.TrapException
End Function

Function GetEmit()
    On Error GoTo erro
    
    Dim result
    Dim ender
    
    Dim nCodigo_IBGE As Long
    Dim nCodigo_UF As Integer
    
    nCodigo_IBGE = fcRetornaDadosDaTabelaFARQ("IBGE")
    If nCodigo_IBGE = 0 Then nCodigo_IBGE = 2611606
    
    nCodigo_UF = Mid(nCodigo_IBGE, 1, 2)
    
    Set rsGenerico = fcDB_OpenRecordSet("Select top 1 * from tabfarq")
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
    With result
        .CNPJ = rsGenerico!CGC
        .XNome = rsGenerico!RazaoSocial
        .XFant = rsGenerico!cTextoBanner
        .IE = rsGenerico!Inscricao
        
        '.IM = "14018"
        '.CNAE = "5611201"
        .IM = ""
        .CNAE = ""
        
        .CRT = Val(fcRetornaConteudoDaTabGeral("NFE_CRT"))
    End With
    
    Set ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
    With ender
        .XLgr = rsGenerico!Endereco
        .Nro = rsGenerico!cNumEnd
        .XBairro = rsGenerico!Bairro
        .CMun = nCodigo_IBGE
        .XMun = rsGenerico!Cidade
        .UF = nCodigo_UF
        .CEP = rsGenerico!CEP
        .Fone = rsGenerico!Telefone
        
    End With
    
    Set result.EnderEmit = ender
    
    Set GetEmit = result
    
    Exit Function
erro:
    Utility.TrapException
End Function

Function GetDest()
Dim result
Dim ender
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")

With result
    .CNPJ = "04218457000128"
    .XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    .IndIEDest = 9
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
    .vFrete = 0
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
'Set Vol = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")
'
'With Transporta
'    .XNome = "RETIRADO PELO CLIENTE"
'    .XEnder = "RUA RIO DE JANEIRO"
'    .XMun = "POCOS DE CALDAS"
'    .UF = 31
'End With
'
'With Vol
'    .QVol = 2
'    .Esp = "VOLUMES"
'    .Marca = "CAIXAS"
'    .PesoL = 0#
'    .PesoB = 0#
'End With

result.ModFrete = 9
'Set result.Transporta = Transporta
'result.AddVol (Vol)

Set GetTransp = result
End Function

'Function GetCobr()
'    Dim result, Dup, Fat
'Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
'Set Dup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
'Set Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
'
'With Fat
'    .NFat = "151342"
'    .VOrig = 140.3
'    .VDesc = 0
'    .VLiq = 140.3
'End With
'Set result.Fat = Fat
'
'With Dup
'    .NDup = "001"
'    .DVenc = Now
'    .VDup = 140.3
'End With
'result.AddDup (Dup)
'
'Set GetCobr = result
'End Function

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
Dim result, Prod, Imposto, ICMS, ICMSSN102, ICMSSN500, PIS, PISOutr, COFINS, COFINSOutr

Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
result.NItem = 1

Set Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
With Prod
    .CProd = "01042"
    .CEAN = "SEM GTIN"
    .XProd = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    .NCM = "84714900"
    .CFOP = "5405"
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

'''Set ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
'''Set ICMSSN102 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN102")
'''
'''With ICMSSN102
'''    .Orig = 0
'''    .CSOSN = "102"
'''End With
'''
'''Set ICMS.ICMSSN102 = ICMSSN102
'''Set Imposto.ICMS = ICMS
                                                    
Set ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
Set ICMSSN500 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN500")

With ICMSSN500
    .Orig = 0
    .CSOSN = "500"
End With

Set ICMS.ICMSSN500 = ICMSSN500
Set Imposto.ICMS = ICMS
                                                    
                                                    
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

