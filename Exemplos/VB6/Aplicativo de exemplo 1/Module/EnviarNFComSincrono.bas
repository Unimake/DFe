Attribute VB_Name = "EnviaNFComSincrono"
' ------------------------------------------------------------------------
' Enviar NFCom Sincrono
' ------------------------------------------------------------------------

Option Explicit

Public Sub EnviarNFComSincrono()
    Dim oConfiguracao As Object
    Dim oExceptionInterop As Object
    
    Dim oNFCom As Object
    Dim oDet As Object
    
    Dim oAutorizacaoSinc As Object
    
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
    oConfiguracao.TipoDFe = 15     ' 15 = NFCom
    oConfiguracao.TipoEmissao = 1  ' 1 = Normal
    oConfiguracao.CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"
    oConfiguracao.CertificadoSenha = "12345678"
    
    ' Criar objeto para pegar exceção do lado do C#
    Set oExceptionInterop = CreateObject("Unimake.Exceptions.ThrowHelper")
    
    ' Criar a tag <NFCom>
    Set oNFCom = CreateObject("Unimake.Business.DFe.Xml.NFCom.NFCom")
    
    ' Criar a tag <InfNFCom>
    Set oNFCom.InfNFCom = CreateObject("Unimake.Business.DFe.Xml.NFCom.InfNFCom")
    oNFCom.InfNFCom.Versao = "1.00"
    
    ' Criar tag Ide
    Set oNFCom.InfNFCom.Ide = CreateObject("Unimake.Business.DFe.Xml.NFCom.Ide")
    oNFCom.InfNFCom.Ide.CUF = 41                  ' PR
    oNFCom.InfNFCom.Ide.TpAmb = 2                 ' Homologação
    oNFCom.InfNFCom.Ide.Mod = 62                  ' Modelo NFCom
    oNFCom.InfNFCom.Ide.Serie = 59
    oNFCom.InfNFCom.Ide.NNF = 5
    oNFCom.InfNFCom.Ide.DhEmi = Now
    oNFCom.InfNFCom.Ide.TpEmis = 1                ' Normal
    oNFCom.InfNFCom.Ide.NSiteAutoriz = "0"
    oNFCom.InfNFCom.Ide.CMunFG = 3132404
    oNFCom.InfNFCom.Ide.FinNFCom = 0              ' Normal
    oNFCom.InfNFCom.Ide.TpFat = 0                 ' Faturamento normal
    oNFCom.InfNFCom.Ide.VerProc = "TESTE 1.00"
    
    ' Criar tag Emit
    Set oNFCom.InfNFCom.Emit = CreateObject("Unimake.Business.DFe.Xml.NFCom.Emit")
    oNFCom.InfNFCom.Emit.CNPJ = "06117473000150"
    oNFCom.InfNFCom.Emit.IE = "9032000301"
    oNFCom.InfNFCom.Emit.XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
    oNFCom.InfNFCom.Emit.XFant = "UNIMAKE PARANAVAI"
    oNFCom.InfNFCom.Emit.CRT = 1      ' Simples Nacional
    
    ' EndereÃ§o do emitente
    Set oNFCom.InfNFCom.Emit.EnderEmit = CreateObject("Unimake.Business.DFe.Xml.NFCom.EnderEmit")
    oNFCom.InfNFCom.Emit.EnderEmit.XLgr = "RUA PAULO ANTONIO COSTA"
    oNFCom.InfNFCom.Emit.EnderEmit.Nro = "575"
    oNFCom.InfNFCom.Emit.EnderEmit.XCpl = "teste"
    oNFCom.InfNFCom.Emit.EnderEmit.XBairro = "CENTRO"
    oNFCom.InfNFCom.Emit.EnderEmit.CMun = 4118402
    oNFCom.InfNFCom.Emit.EnderEmit.XMun = "PARANAVAI"
    oNFCom.InfNFCom.Emit.EnderEmit.UF = 41        ' PR
    oNFCom.InfNFCom.Emit.EnderEmit.CEP = "87707210"
    oNFCom.InfNFCom.Emit.EnderEmit.Fone = "04431421010"
    
    ' Criar tag Dest
    Set oNFCom.InfNFCom.Dest = CreateObject("Unimake.Business.DFe.Xml.NFCom.Dest")
    oNFCom.InfNFCom.Dest.CNPJ = "11111111111111"
    oNFCom.InfNFCom.Dest.XNome = "NF E EMITIDA EM AMBIENTE DE HOMOLOGACAO SEM VALOR FISCAL"
    oNFCom.InfNFCom.Dest.IndIEDest = 9   ' Não contribuinte
    
    ' Endereço do destinatário
    Set oNFCom.InfNFCom.Dest.EnderDest = CreateObject("Unimake.Business.DFe.Xml.NFCom.EnderDest")
    oNFCom.InfNFCom.Dest.EnderDest.XLgr = "XXXXXXX XX XXXXXXX"
    oNFCom.InfNFCom.Dest.EnderDest.Nro = "1111"
    oNFCom.InfNFCom.Dest.EnderDest.XBairro = "XXXXXX XXXXXX"
    oNFCom.InfNFCom.Dest.EnderDest.CMun = 3543402
    oNFCom.InfNFCom.Dest.EnderDest.XMun = "XXXXXXXX XXXXX"
    oNFCom.InfNFCom.Dest.EnderDest.UF = 35       ' SP
    oNFCom.InfNFCom.Dest.EnderDest.CEP = "14080000"
    oNFCom.InfNFCom.Dest.EnderDest.Fone = "01231111111"
    oNFCom.InfNFCom.Dest.EnderDest.Email = "teste@teste.com"
    
    ' Criar tag Assinante
    Set oNFCom.InfNFCom.Assinante = CreateObject("Unimake.Business.DFe.Xml.NFCom.Assinante")
    oNFCom.InfNFCom.Assinante.ICodAssinante = "0095311"
    oNFCom.InfNFCom.Assinante.TpAssinante = 99          ' Outros
    oNFCom.InfNFCom.Assinante.TpServUtil = 4           ' Provimento acesso internet
    oNFCom.InfNFCom.Assinante.NContrato = "20250095311"
    oNFCom.InfNFCom.Assinante.DContratoIni = Now
    oNFCom.InfNFCom.Assinante.DContratoFim = Now
    
    ' Itens Det
    For i = 1 To 1
        Set oDet = CreateObject("Unimake.Business.DFe.Xml.NFCom.Det")
        oDet.NItem = i
        
        Set oDet.Prod = CreateObject("Unimake.Business.DFe.Xml.NFCom.Prod")
        oDet.Prod.CProd = "0000" & CStr(i)
        oDet.Prod.XProd = "ACESSO INTERNET"
        oDet.Prod.CClass = "0400501"
        oDet.Prod.CFOP = "5307"
        oDet.Prod.uMed = 2                 ' MB
        oDet.Prod.qFaturada = 500
        oDet.Prod.vItem = 0.2098
        oDet.Prod.VProd = 104.12345679
        
        ' Impostos
        Set oDet.Imposto = CreateObject("Unimake.Business.DFe.Xml.NFCom.Imposto")
        
        ' ICMSSN
        Set oDet.Imposto.ICMSSN = CreateObject("Unimake.Business.DFe.Xml.NFCom.ICMSSN")
        oDet.Imposto.ICMSSN.CST = "90"
        oDet.Imposto.ICMSSN.IndSN = 1      ' Sim
        
        ' PIS
        Set oDet.Imposto.PIS = CreateObject("Unimake.Business.DFe.Xml.NFCom.PIS")
        oDet.Imposto.PIS.CST = 49
        oDet.Imposto.PIS.VBC = 0#
        oDet.Imposto.PIS.PPIS = 0#
        oDet.Imposto.PIS.VPIS = 0#
        
        ' COFINS
        Set oDet.Imposto.COFINS = CreateObject("Unimake.Business.DFe.Xml.NFCom.COFINS")
        oDet.Imposto.COFINS.CST = 49
        oDet.Imposto.COFINS.VBC = 0#
        oDet.Imposto.COFINS.PCOFINS = 0#
        oDet.Imposto.COFINS.VCOFINS = 0#
        
        ' IBSCBS
        Set oDet.Imposto.IBSCBS = CreateObject("Unimake.Business.DFe.Xml.NFCom.IBSCBS")
        oDet.Imposto.IBSCBS.CST = "000"
        oDet.Imposto.IBSCBS.CClassTrib = "000001"
        
        Set oDet.Imposto.IBSCBS.GIBSCBS = CreateObject("Unimake.Business.DFe.Xml.NFCom.GIBSCBS")
        oDet.Imposto.IBSCBS.GIBSCBS.VBC = 104.9
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF = CreateObject("Unimake.Business.DFe.Xml.NFCom.GIBSUF")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.PIBSUF = 0.1
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSUF.vIBSUF = 0.1
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN = CreateObject("Unimake.Business.DFe.Xml.NFCom.GIBSMUN")
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN.PIBSMUN = 0#
        oDet.Imposto.IBSCBS.GIBSCBS.GIBSMUN.vIBSMun = 0#
        
        oDet.Imposto.IBSCBS.GIBSCBS.vIBS = 0.1
        
        Set oDet.Imposto.IBSCBS.GIBSCBS.GCBS = CreateObject("Unimake.Business.DFe.Xml.NFCom.GCBS")
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.PCBS = 0.9
        oDet.Imposto.IBSCBS.GIBSCBS.GCBS.vCBS = 0.94
        
        ' Adicionar Det em InfNFCom
        oNFCom.InfNFCom.AddDet oDet
    Next i
    
    ' Total
    Set oNFCom.InfNFCom.Total = CreateObject("Unimake.Business.DFe.Xml.NFCom.Total")
    oNFCom.InfNFCom.Total.VProd = Format$(104.9, "0.00")
    
    Set oNFCom.InfNFCom.Total.ICMSTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.ICMSTot")
    oNFCom.InfNFCom.Total.ICMSTot.VBC = 0#
    oNFCom.InfNFCom.Total.ICMSTot.VICMS = 0#
    oNFCom.InfNFCom.Total.ICMSTot.VICMSDeson = 0#
    oNFCom.InfNFCom.Total.ICMSTot.VFCP = 0#
    
    oNFCom.InfNFCom.Total.VCOFINS = 0#
    oNFCom.InfNFCom.Total.VPIS = 0#
    oNFCom.InfNFCom.Total.vFUNTTEL = 0#
    oNFCom.InfNFCom.Total.vFUST = 0#
    
    Set oNFCom.InfNFCom.Total.vRetTribTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.vRetTribTot")
    oNFCom.InfNFCom.Total.vRetTribTot.vRetPIS = 0#
    oNFCom.InfNFCom.Total.vRetTribTot.vRetCofins = 0#
    oNFCom.InfNFCom.Total.vRetTribTot.vRetCSLL = 0#
    oNFCom.InfNFCom.Total.vRetTribTot.vIRRF = 0#
    
    oNFCom.InfNFCom.Total.VDesc = 0#
    oNFCom.InfNFCom.Total.VOutro = 0#
    oNFCom.InfNFCom.Total.VNF = CCur(104.9)
    
    Set oNFCom.InfNFCom.Total.IBSCBSTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.IBSCBSTot")
    oNFCom.InfNFCom.Total.IBSCBSTot.vBCIBSCBS = 104.9
    
    Set oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.gIBSTot")
    
    Set oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.gIBSUFTot")
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vDif = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vDevTrib = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSUFTot.vIBSUF = 0.1
    
    Set oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.gIBSMunTot")
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vDif = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vDevTrib = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.gIBSMunTot.vIBSMun = 0#
    
    oNFCom.InfNFCom.Total.IBSCBSTot.gIBSTot.vIBS = 0.1
    
    Set oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot = CreateObject("Unimake.Business.DFe.Xml.NFCom.gCBSTot")
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vDif = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vDevTrib = 0#
    oNFCom.InfNFCom.Total.IBSCBSTot.gCBSTot.vCBS = 0.94
    
    oNFCom.InfNFCom.Total.vTotDFe = 105.95
    
    ' gFat
    Set oNFCom.InfNFCom.GFat = CreateObject("Unimake.Business.DFe.Xml.NFCom.GFat")
    oNFCom.InfNFCom.GFat.CompetFat = "202508"
    oNFCom.InfNFCom.GFat.dVencFat = Now
    oNFCom.InfNFCom.GFat.codBarras = "11111111111111111111111111111111111111111111111"
    
    ' infAdic
    Set oNFCom.InfNFCom.InfAdic = CreateObject("Unimake.Business.DFe.Xml.NFCom.infAdic")
    oNFCom.InfNFCom.InfAdic.AddInfCpl "Optante pelo Simples Nacional conforme Lei Complementar NÂº 123/2006."
    oNFCom.InfNFCom.InfAdic.AddInfCpl "Optante pelo Simples Nacional conforme Lei Complementar NÂº 123/2006-2"
    
    ' gRespTec
    Set oNFCom.InfNFCom.gRespTec = CreateObject("Unimake.Business.DFe.Xml.NFCom.gRespTec")
    oNFCom.InfNFCom.gRespTec.CNPJ = "99999999999999"
    oNFCom.InfNFCom.gRespTec.XContato = "XXXXXXX XXXXXX XXXXXXXX"
    oNFCom.InfNFCom.gRespTec.Email = "teste@teste.com.br"
    oNFCom.InfNFCom.gRespTec.Fone = "99999999999"
    
    ' Consumir o serviço
    Set oAutorizacaoSinc = CreateObject("Unimake.Business.DFe.Servicos.NFCom.AutorizacaoSinc")
    oAutorizacaoSinc.SetXMLConfiguracao oNFCom, oConfiguracao
    
    ' Recuperar conteúdo do XML assinado
    notaAssinada = CStr(oAutorizacaoSinc.GetConteudoXMLAssinado)
    
    ' Exibir XML assinado
    MsgBox notaAssinada
    
    ' Caminho do arquivo
    caminhoArquivo = "d:\testenfe\" & oNFCom.InfNFCom.Chave & "-nfcom.xml"
    
    ' Excluir arquivo existente
    If FileExists(caminhoArquivo) Then
        Kill caminhoArquivo
    End If
    
    ' Gravar XML assinado
    Call SalvarTextoEmArquivo(caminhoArquivo, notaAssinada)
    
    ' Executar envio síncrono
    oAutorizacaoSinc.Executar oNFCom, oConfiguracao
    
    ' XML retornado
    xmlRetornado = CStr(oAutorizacaoSinc.RetornoWSString)
    MsgBox xmlRetornado
    
    ' Código de status e motivo
    statusRetorno = CStr(oAutorizacaoSinc.result.CStat) & " " & CStr(oAutorizacaoSinc.result.XMotivo)
    MsgBox statusRetorno
    
    ' Verificar autorização
    If oAutorizacaoSinc.result.CStat = 100 Then
        If oAutorizacaoSinc.result.ProtNFCom.InfProt.CStat = 100 Then
            ' Gravar XML de distribuição
            oAutorizacaoSinc.GravarXmlDistribuicao "d:\testenfe"
            
            ' XML de distribuição
            docProcNFe = CStr(oAutorizacaoSinc.GetNFeProcResults(oNFCom.InfNFCom.Chave))
            MsgBox docProcNFe
            
            ' Número do protocolo
            numeroProtocolo = CStr(oAutorizacaoSinc.result.ProtNFCom.InfProt.NProt)
            MsgBox numeroProtocolo
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
