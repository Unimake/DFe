Attribute VB_Name = "EnviaNFeSincronoRTC"
' ------------------------------------------------------------------------
' Enviar NFe Sincrono (Com as tags da reforma tributária)
' ------------------------------------------------------------------------

Option Explicit

Public Sub EnviarNFeSincronoRTC()
    Dim oConfiguracao As Object
    Dim oExceptionInterop As Object
    
    Dim oEnviNFe As Object
    Dim oNFe As Object
    Dim oDet As Object
    Dim oVol As Object
    Dim oDup As Object
    Dim oDetPag As Object
    
    Dim oConteudoNFe As Object
    Dim oConteudoInfNFe As Object
    Dim chaveNFe As String
    
    Dim oAutorizacao As Object
    
    Dim notaAssinada As String
    Dim caminhoArquivo As String
    
    Dim xmlRetornado As String
    Dim statusRetorno As String
    Dim docProcNFe As String
    Dim numeroProtocolo As String
    
    Dim i As Long
    
    On Error GoTo TrataErro
   
    ' Criar objeto de configuração mínima
    Set oConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
    oConfiguracao.TipoDFe = 0      ' 0 = NFe
    oConfiguracao.TipoEmissao = 1  ' 1 = Normal
    oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
    oConfiguracao.CertificadoSenha = "12345678"
    
    ' Criar objeto para pegar exceção do lado do C#
    Set oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
    
    ' Criar a tag <enviNFe>
    Set oEnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
    oEnviNFe.Versao = "4.00"
    oEnviNFe.IdLote = "000000000000001"
    oEnviNFe.IndSinc = 1   ' 1 = Sim
    
    ' Criar a tag <NFe>
    Set oNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
    
    ' Criar a tag <InfNFe>
    Set oNFe.InfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
    oNFe.InfNFe.Versao = "4.00"
    
    ' Criar tag Ide
    Set oNFe.InfNFe.Ide = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
    oNFe.InfNFe.Ide.CUF = 41
    oNFe.InfNFe.Ide.NatOp = "VENDA PRODUC.DO ESTABELEC"
    oNFe.InfNFe.Ide.Mod = 55
    oNFe.InfNFe.Ide.Serie = 59
    oNFe.InfNFe.Ide.NNF = 33
    oNFe.InfNFe.Ide.DhEmi = Now
    oNFe.InfNFe.Ide.DhSaiEnt = Now
    oNFe.InfNFe.Ide.TpNF = 1       ' Saída
    oNFe.InfNFe.Ide.IdDest = 2     ' Operação interestadual
    oNFe.InfNFe.Ide.CMunFG = 4118402
    oNFe.InfNFe.Ide.TpImp = 1      ' DANFE normal retrato
    oNFe.InfNFe.Ide.TpEmis = 1     ' Normal
    oNFe.InfNFe.Ide.TpAmb = 2      ' Homologação
    oNFe.InfNFe.Ide.FinNFe = 1     ' Normal
    oNFe.InfNFe.Ide.IndFinal = 1   ' Consumidor final = Sim
    oNFe.InfNFe.Ide.IndPres = 1    ' Operação presencial
    oNFe.InfNFe.Ide.ProcEmi = 0    ' Aplicativo do contribuinte
    oNFe.InfNFe.Ide.VerProc = "TESTE 1.00"
    
    ' RTC
    oNFe.InfNFe.Ide.CMunFGIBS = 3543402
    Set oNFe.InfNFe.Ide.GCompraGov = CreateObject("Unimake.Business.DFe.Xml.NFe.GCompraGov")
    oNFe.InfNFe.Ide.GCompraGov.PRedutor = 0#
    oNFe.InfNFe.Ide.GCompraGov.TpEnteGov = 4   ' Município
    oNFe.InfNFe.Ide.GCompraGov.TpOperGov = 1   ' Fornecimento
    
    Set oNFe.InfNFe.Ide.GPagAntecipado = CreateObject("Unimake.Business.DFe.Xml.NFe.GPagAntecipado")
    oNFe.InfNFe.Ide.GPagAntecipado.AddRefNFe "00000000000000000000000000000000000000000000"
    oNFe.InfNFe.Ide.GPagAntecipado.AddRefNFe "11111111111111111111111111111111111111111111"
    
    oNFe.InfNFe.Ide.TpNFCredito = 2
    oNFe.InfNFe.Ide.TpNFDebito = 6
    
    ' Criar tag Emit
    Set oNFe.InfNFe.Emit = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
    oNFe.InfNFe.Emit.CNPJ = "06117473000150"
    oNFe.InfNFe.Emit.XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
    oNFe.InfNFe.Emit.XFant = "UNIMAKE - PARANAVAI"
    oNFe.InfNFe.Emit.IE = "9032000301"
    oNFe.InfNFe.Emit.IM = "14018"
    oNFe.InfNFe.Emit.CNAE = "6202300"
    oNFe.InfNFe.Emit.CRT = 1
    
    Set oNFe.InfNFe.Emit.EnderEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
    oNFe.InfNFe.Emit.EnderEmit.XLgr = "RUA PAULO ANTONIO COSTA"
    oNFe.InfNFe.Emit.EnderEmit.Nro = "575"
    oNFe.InfNFe.Emit.EnderEmit.XBairro = "CENTRO"
    oNFe.InfNFe.Emit.EnderEmit.CMun = 4118402
    oNFe.InfNFe.Emit.EnderEmit.XMun = "PARANAVAI"
    oNFe.InfNFe.Emit.EnderEmit.UF = 41
    oNFe.InfNFe.Emit.EnderEmit.CEP = "87707210"
    oNFe.InfNFe.Emit.EnderEmit.Fone = "04431421010"
    
    ' Criar tag Dest
    Set oNFe.InfNFe.Dest = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
    oNFe.InfNFe.Dest.CNPJ = "04218457000128"
    oNFe.InfNFe.Dest.XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
    oNFe.InfNFe.Dest.IndIEDest = 1
    oNFe.InfNFe.Dest.IE = "582614838110"
    oNFe.InfNFe.Dest.Email = "janelaorp@janelaorp.com.br"
    
    Set oNFe.InfNFe.Dest.EnderDest = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
    oNFe.InfNFe.Dest.EnderDest.XLgr = "AVENIDA DA SAUDADE"
    oNFe.InfNFe.Dest.EnderDest.Nro = "1555"
    oNFe.InfNFe.Dest.EnderDest.XBairro = "CAMPOS ELISEOS"
    oNFe.InfNFe.Dest.EnderDest.CMun = 3543402
    oNFe.InfNFe.Dest.EnderDest.XMun = "RIBEIRAO PRETO"
    oNFe.InfNFe.Dest.EnderDest.UF = 35
    oNFe.InfNFe.Dest.EnderDest.CEP = "14080000"
    oNFe.InfNFe.Dest.EnderDest.Fone = "01639611500"
    
    ' Itens Det
    For i = 1 To 3
        Set oDet = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
        oDet.NItem = i
        
        Set oDet.Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
        oDet.Prod.CProd = "0000" & CStr(i)
        oDet.Prod.CEAN = "SEM GTIN"
        oDet.Prod.XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
        oDet.Prod.NCM = "84714900"
        oDet.Prod.CFOP = "6101"
        oDet.Prod.UCom = "LU"
        oDet.Prod.QCom = 1#
        oDet.Prod.VUnCom = 84.9
        oDet.Prod.VProd = 84.9
        oDet.Prod.CEANTrib = "SEM GTIN"
        oDet.Prod.UTrib = "LU"
        oDet.Prod.QTrib = 1#
        oDet.Prod.VUnTrib = 84.9
        oDet.Prod.IndTot = 1
        oDet.Prod.XPed = "300474"
        oDet.Prod.NItemPed = 1
        
        ' Impostos
        Set oDet.Imposto = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
        oDet.Imposto.VTotTrib = 12.63
        
        ' ICMS
        Set oDet.Imposto.ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
        Set oDet.Imposto.ICMS.ICMSSN101 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
        oDet.Imposto.ICMS.ICMSSN101.Orig = 0
        oDet.Imposto.ICMS.ICMSSN101.PCredSN = 2.8255
        oDet.Imposto.ICMS.ICMSSN101.VCredICMSSN = 2.4
        
        ' PIS
        Set oDet.Imposto.PIS = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")
        Set oDet.Imposto.PIS.PISOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
        oDet.Imposto.PIS.PISOutr.CST = "99"
        oDet.Imposto.PIS.PISOutr.VBC = 0#
        oDet.Imposto.PIS.PISOutr.PPIS = 0#
        oDet.Imposto.PIS.PISOutr.VPIS = 0#
        
        ' COFINS
        Set oDet.Imposto.COFINS = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")
        Set oDet.Imposto.COFINS.COFINSOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
        oDet.Imposto.COFINS.COFINSOutr.CST = "99"
        oDet.Imposto.COFINS.COFINSOutr.VBC = 0#
        oDet.Imposto.COFINS.COFINSOutr.PCOFINS = 0#
        oDet.Imposto.COFINS.COFINSOutr.VCOFINS = 0#
        
        ' RTC - IBSCBS
        Set oDet.Imposto.IBSCBS = CreateObject("Unimake.Business.DFe.Xml.NFe.IBSCBS")
        oDet.Imposto.IBSCBS.CST = "000"
        oDet.Imposto.IBSCBS.CClassTrib = "000001"
        
        Set oDet.Imposto.IBSCBS.GIBSCBS = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSCBS")
        oDet.Imposto.IBSCBS.GIBSCBS.VBC = 0#
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GCBS = CreateObject("Unimake.Business.DFe.Xml.NFe.GCBS")
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.PCBS = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.VCBS = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDevTrib = CreateObject("Unimake.Business.DFe.Xml.NFe.GDevTrib")
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GDevTrib.VDevTrib = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed = CreateObject("Unimake.Business.DFe.Xml.NFe.GRed")
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed.PAliqEfet = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.GRed.PRedAliq = 0#
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSMun")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.PIBSMun = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.VIBSMun = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDevTrib = CreateObject("Unimake.Business.DFe.Xml.NFe.GDevTrib")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GDevTrib.VDevTrib = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed = CreateObject("Unimake.Business.DFe.Xml.NFe.GRed")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed.PAliqEfet = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMun.GRed.PRedAliq = 0#
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSUF")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.PIBSUF = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.VIBSUF = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDevTrib = CreateObject("Unimake.Business.DFe.Xml.NFe.GDevTrib")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GDevTrib.VDevTrib = 0#
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed = CreateObject("Unimake.Business.DFe.Xml.NFe.GRed")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed.PAliqEfet = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.GRed.PRedAliq = 0#
        
        oDet.Imposto.IBSCBS.GIBSCBS.VIBS = 0#
        
        ' Adicionar Det em InfNFe
        oNFe.InfNFe.AddDet oDet
    Next i
    
    ' Total
    Set oNFe.InfNFe.Total = CreateObject("Unimake.Business.DFe.Xml.NFe.Total")
    
    Set oNFe.InfNFe.Total.ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSTot")
    oNFe.InfNFe.Total.ICMSTot.VBC = 0#
    oNFe.InfNFe.Total.ICMSTot.VICMS = 0#
    oNFe.InfNFe.Total.ICMSTot.VICMSDeson = 0#
    oNFe.InfNFe.Total.ICMSTot.VFCP = 0#
    oNFe.InfNFe.Total.ICMSTot.VBCST = 0#
    oNFe.InfNFe.Total.ICMSTot.VST = 0#
    oNFe.InfNFe.Total.ICMSTot.VFCPST = 0#
    oNFe.InfNFe.Total.ICMSTot.VFCPSTRet = 0#
    oNFe.InfNFe.Total.ICMSTot.VProd = 254.7
    oNFe.InfNFe.Total.ICMSTot.VFrete = 0#
    oNFe.InfNFe.Total.ICMSTot.VSeg = 0#
    oNFe.InfNFe.Total.ICMSTot.VDesc = 0#
    oNFe.InfNFe.Total.ICMSTot.VII = 0#
    oNFe.InfNFe.Total.ICMSTot.VIPI = 0#
    oNFe.InfNFe.Total.ICMSTot.VIPIDevol = 0#
    oNFe.InfNFe.Total.ICMSTot.VPIS = 0#
    oNFe.InfNFe.Total.ICMSTot.VCOFINS = 0#
    oNFe.InfNFe.Total.ICMSTot.VOutro = 0#
    oNFe.InfNFe.Total.ICMSTot.VNF = 254.7
    oNFe.InfNFe.Total.ICMSTot.VTotTrib = 37.89
    
    ' RTC
    Set oNFe.InfNFe.Total.IBSCBSTot = CreateObject("Unimake.Business.DFe.Xml.NFe.IBSCBSTot")
    oNFe.InfNFe.Total.IBSCBSTot.VBCIBSCBS = 0#
    
    Set oNFe.InfNFe.Total.IBSCBSTot.GCBS = CreateObject("Unimake.Business.DFe.Xml.NFe.GCBSTot")
    oNFe.InfNFe.Total.IBSCBSTot.GCBS.VCBS = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GCBS.VCredPres = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GCBS.VCredPresCondSus = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GCBS.VDevTrib = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GCBS.VDif = 0#
    
    Set oNFe.InfNFe.Total.IBSCBSTot.GIBS = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSTot")
    Set oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSMun = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSMunTot")
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSMun.VDevTrib = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSMun.VDif = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSMun.VIBSMun = 0#
    
    Set oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSUF = CreateObject("Unimake.Business.DFe.Xml.NFe.GIBSUFTot")
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSUF.VDevTrib = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSUF.VDif = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.GIBSUF.VIBSUF = 0#
    
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.VCredPres = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.VCredPresCondSus = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GIBS.VIBS = 0#
    
    Set oNFe.InfNFe.Total.IBSCBSTot.GMono = CreateObject("Unimake.Business.DFe.Xml.NFe.GMono")
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VCBSMono = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VCBSMonoRet = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VCBSMonoReten = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VIBSMono = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VIBSMonoRet = 0#
    oNFe.InfNFe.Total.IBSCBSTot.GMono.VIBSMonoReten = 0#
    
    Set oNFe.InfNFe.Total.ISTot = CreateObject("Unimake.Business.DFe.Xml.NFe.ISTot")
    oNFe.InfNFe.Total.ISTot.VIS = 0#
    
    oNFe.InfNFe.Total.VNFTot = 1
    
    ' Transporte
    Set oNFe.InfNFe.Transp = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")
    oNFe.InfNFe.Transp.ModFrete = 0
    
    For i = 1 To 3
        Set oVol = CreateObject("Unimake.Business.DFe.Xml.NFe.Vol")
        oVol.QVol = 1
        oVol.Esp = "LU"
        oVol.Marca = "UNIMAKE"
        oVol.PesoL = 0#
        oVol.PesoB = 0#
        
        oNFe.InfNFe.Transp.AddVol oVol
    Next i
    
    ' Cobrança
    Set oNFe.InfNFe.Cobr = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
    Set oNFe.InfNFe.Cobr.Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")
    oNFe.InfNFe.Cobr.Fat.NFat = "057910"
    oNFe.InfNFe.Cobr.Fat.VOrig = 254.7
    oNFe.InfNFe.Cobr.Fat.VDesc = 0#
    oNFe.InfNFe.Cobr.Fat.VLiq = 254.7
    
    For i = 1 To 2
        Set oDup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
        oDup.NDup = "00" & CStr(i)
        oDup.DVenc = Date
        oDup.VDup = 127.35
        
        oNFe.InfNFe.Cobr.AddDup oDup
    Next i
    
    ' Pagamento
    Set oNFe.InfNFe.Pag = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
    Set oDetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
    oDetPag.IndPag = 0
    oDetPag.TPag = 1
    oDetPag.VPag = 254.7
    oNFe.InfNFe.Pag.AddDetPag oDetPag
    
    ' Informações adicionais
    Set oNFe.InfNFe.InfAdic = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
    oNFe.InfNFe.InfAdic.InfCpl = "Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008"
    
    ' Responsável técnico
    Set oNFe.InfNFe.InfRespTec = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
    oNFe.InfNFe.InfRespTec.CNPJ = "06117473000150"
    oNFe.InfNFe.InfRespTec.XContato = "Ze das Couves"
    oNFe.InfNFe.InfRespTec.Email = "zedascouves@gmail.com"
    oNFe.InfNFe.InfRespTec.Fone = "04430000000"
    oNFe.InfNFe.InfRespTec.IdCSRT = "01"
    oNFe.InfNFe.InfRespTec.CSRT = "8WCARAO9D8P00R845TARUPPTGY5CL40WS3J1"
    
    ' Adicionar InfNFe em NFe
    oNFe.AddInfNFe oNFe.InfNFe
    
    ' Adicionar NFe em EnviNFe
    oEnviNFe.AddNFe oNFe
    
    ' Recuperar a chave da NFe
    Set oConteudoNFe = oEnviNFe.GetNFe(0)
    Set oConteudoInfNFe = oConteudoNFe.GetInfNFe(0)
    chaveNFe = CStr(oConteudoInfNFe.Chave)
    MsgBox "Chave NFe: " & chaveNFe
    
    ' Recuperar outras informações da NFe
    MsgBox CStr(oConteudoInfNFe.Ide.CUF)
    MsgBox CStr(oConteudoInfNFe.Emit.XNome)
    
    ' Consumir o serviço
    Set oAutorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
    oAutorizacao.SetXMLConfiguracao oEnviNFe, oConfiguracao
    
    ' Recuperar conteúdo do XML assinado
    notaAssinada = CStr(oAutorizacao.GetConteudoNFeAssinada(0))
    
    ' Exibir XML assinado
    MsgBox notaAssinada
    
    ' Caminho do arquivo
    caminhoArquivo = "d:\testenfe\" & chaveNFe & "-nfe.xml"
    
    ' Excluir arquivo existente
    If FileExists(caminhoArquivo) Then
        Kill caminhoArquivo
    End If
    
    ' Gravar XML assinado
    Call SalvarTextoEmArquivo(caminhoArquivo, notaAssinada)
    
    ' Executar envio síncrono
    oAutorizacao.Executar oEnviNFe, oConfiguracao
    
    ' XML retornado
    xmlRetornado = CStr(oAutorizacao.RetornoWSString)
    MsgBox xmlRetornado
    
    ' Código de status e motivo
    statusRetorno = CStr(oAutorizacao.Result.CStat) & " " & CStr(oAutorizacao.Result.XMotivo)
    MsgBox statusRetorno
    
    ' Verificar autorização
    If oAutorizacao.Result.CStat = 104 Then
        If oAutorizacao.Result.ProtNFe.InfProt.CStat = 100 Then
            ' Gravar XML de distribuição
            oAutorizacao.GravarXmlDistribuicao "d:\testenfe"
            
            ' XML de distribuição
            docProcNFe = CStr(oAutorizacao.GetNFeProcResults(chaveNFe))
            MsgBox docProcNFe
            
            ' Número do protocolo
            numeroProtocolo = CStr(oAutorizacao.Result.ProtNFe.InfProt.NProt)
            MsgBox numeroProtocolo
        Else
            MsgBox "NF-e rejeitada ou denegada. Verifique os detalhes."
        End If
    End If
    
    Exit Sub
    
TrataErro:
    MsgBox "Erro, " & Err.Description
    On Error Resume Next
    MsgBox oExceptionInterop.GetMessage
    MsgBox CStr(oExceptionInterop.GetErrorCode)
End Sub

Private Function FileExists(ByVal FileName As String) As Boolean
    FileExists = (Len(Dir$(FileName)) > 0)
End Function

Private Sub SalvarTextoEmArquivo(ByVal FileName As String, ByVal Conteudo As String)
    Dim f As Integer
    f = FreeFile
    Open FileName For Binary As #f
    Put #f, , Conteudo
    Close #f
End Sub
