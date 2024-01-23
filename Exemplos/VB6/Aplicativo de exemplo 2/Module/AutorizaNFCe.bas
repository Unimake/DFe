Attribute VB_Name = "ServicosNFCe"
Option Explicit
Public Sub AutorizarNFCe()
    On Error GoTo erro
    
    Dim rsTabNFCePendentes      As New ADODB.Recordset
    Dim strSql                  As String
    
    Dim EnviNFe
    Dim Autorizacao
    Dim localConfig
    
    Log.ClearLog
    
    
    strSql = "select Top 1 TabCadNFCe.nAcesso, TabCadNFCe.nFiscal, TabCadNFCe.cSerie, TabCadNFCe.ddtemissaonf, TabCadNFCe.dDtSaida, " & vbCrLf
    strSql = strSql & "'0' as nOtDesepsas, TabCfop.cdescricao, TabComanda.nPedido, Isnull(TabComanda.nCliente,1) as nCliente," & vbCrLf
    strSql = strSql & "(case when dbo.fcValorPedidoPago(TabComanda.nPedido) > dbo.fcValorPedidoFull(TabComanda.nPedido, 'F') then (dbo.fcValorPedidoPago(TabComanda.nPedido)) - dbo.fcValorPedidoFull(TabComanda.nPedido, 'F') else 0 end ) as nValAcres," & vbCrLf
    strSql = strSql & "(case when dbo.fcValorPedidoPago(TabComanda.nPedido) < dbo.fcValorPedidoFull(TabComanda.nPedido, 'F') then (dbo.fcValorPedidoFull(TabComanda.nPedido, 'F') - dbo.fcValorPedidoPago(TabComanda.nPedido)) else 0 end ) as nDesconto, " & vbCrLf
    strSql = strSql & "'0' as nBaseIcmsSub, '0' as nCalcIcmsSub, '0' as nVlIPI, 'S' as cTipo, " & vbCrLf
    strSql = strSql & "'0' as nValorSeguro, '0' as nValorFrete, dbo.fcImpostoPedido(TabComanda.nPedido) as nTotalImposto, '9' as nTipoFrete,'' as cNome_Transportadora, " & vbCrLf
    strSql = strSql & "'' as cPlaca_Transportadora, '' as cUfVeiculo_Transportadora, '0' as nQuantidade_Transportadora, '' as cObs, " & vbCrLf
    strSql = strSql & "dbo.fcValorPedidoPago(TabComanda.nPedido) as ValorPago," & vbCrLf
    strSql = strSql & "dbo.fcValorPedidoFull(TabComanda.nPedido, 'F') as ValorProdutos, isnull(TabComanda.nMesa,0) as DocVenda, '' as cPedidoCompra " & vbCrLf
    strSql = strSql & "from TabCadNFCe" & vbCrLf
    strSql = strSql & "inner join TabComanda on TabCadNFCe.nPedido = TabComanda.nPedido" & vbCrLf
    strSql = strSql & "inner join tabcfop on TabCadNFCe.ncfop = tabcfop.ncodigo" & vbCrLf
    strSql = strSql & "where" & vbCrLf
    strSql = strSql & "TabCadNFCe.cSituacao = 'P' " & vbCrLf
    strSql = strSql & "And TabComanda.cStatus = 'A'" & vbCrLf
    strSql = strSql & "Order By TabCadNFCe.nAcesso ASC" & vbCrLf

    Set rsTabNFCePendentes = fcDB_OpenRecordSet(strSql)
    If Not rsTabNFCePendentes.EOF Then
    
        Set localConfig = Config.InicializarConfiguracao(TipoDFe.NFCe)
        
        '#criar campos tabfarq (csc e tcsc)
        localConfig.CSC = fcRetornaDadosDaTabelaFARQ("CSC")
        localConfig.CSCIDToken = fcRetornaDadosDaTabelaFARQ("TCSC")
        ''
        
        'zerando vairaveis globais
        glb_ValorTotalLiquidoProduto = 0
        glb_ValorTotalLiquidoNF = 0
        glb_ValorTotalImpostoPrevisto = 0
        glb_ValorTotal_Desconto = 0
        glb_ValorTotal_Acrescimo = 0
        ''
        
        'totalizadores
        glb_ValorTotal_Acrescimo = Format(CheckNull(rsTabNFCePendentes!nValAcres, True), "#,##0.00")
        glb_ValorTotal_Desconto = Format(CheckNull(rsTabNFCePendentes!nDesconto, True), "#,##0.00")
        
        If CheckNull(rsTabNFCePendentes!ValorPago, True) <> CheckNull(rsTabNFCePendentes!ValorProdutos, True) Then
            Call fcCalculaDescAcresItens(rsTabNFCePendentes!nAcesso, 0, CheckNull(rsTabNFCePendentes!ValorPago, True), CheckNull(rsTabNFCePendentes!ValorProdutos, True), CheckNull(rsTabNFCePendentes!nDesconto, True), CheckNull(rsTabNFCePendentes!nValAcres, True))
        End If
        ''
        
        Set EnviNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
        EnviNFe.Versao = "4.00"
        EnviNFe.IdLote = Format(rsTabNFCePendentes!nAcesso, "000000000000000")
        EnviNFe.IndSinc = 1
        EnviNFe.AddNFe GetNFCe(rsTabNFCePendentes!nAcesso, rsTabNFCePendentes!nfiscal, rsTabNFCePendentes!cSerie, rsTabNFCePendentes!nCliente)
        
        Set Autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFCe.Autorizacao")
        
        Autorizacao.Executar (EnviNFe), (localConfig)
        
        If Autorizacao.result.CStat = 104 Then  '104 = Lote Processado
        
            'Gravar XML de distribuicao em uma pasta (NFe com o protocolo de autorizacao anexado)
            Autorizacao.GravarXmlDistribuicao App.path
            
           If Autorizacao.result.ProtNFe.InfProt.CStat = 100 Then '100 = Autorizado o uso da NF-e
            
              Call sbDB_AlterarNDados("TabCadNFCe", _
                        "nAcesso=" & rsTabNFCePendentes!nAcesso, _
                        "cKeyNFCe", Autorizacao.result.ProtNFe.InfProt.ChNFe, _
                        "cProtocoloNFCe", Autorizacao.result.ProtNFe.InfProt.NProt, _
                        "cSituacao", "A")
              
              'Capturando Chave da Nota
              MsgBox "Chave da Nota: " & Autorizacao.result.ProtNFe.InfProt.ChNFe
              
             'Como pegar o numero do protocolo de autorizacao para gravar na base
              MsgBox "Protocolo da Nota: " & Autorizacao.result.ProtNFe.InfProt.NProt
              
             'Pegar a string do XML de distribuição para gravar em uma base de dados
              'Debug.Print Autorizacao.GetNFeProcResults(Autorizacao.result.ProtNFe.InfProt.ChNFe)
                 
           Else
             'Rejeitada ou Denegada - Fazer devidos tratamentos
             
           End If
           Call ShellExecute(1, "OPEN", Autorizacao.result.ProtNFe.InfProt.ChNFe & "-procnfe.xml", 1, App.path, 1)
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

Function GetNFCe(ByVal p_lngNotaFiscal_ID As Long, ByVal p_lngNotaFiscal As Long, ByVal p_strSerie As String, ByVal p_lngCliente As Long)
Dim NFe
Set NFe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")
NFe.AddInfNFe GetInfNFe(p_lngNotaFiscal_ID, p_lngNotaFiscal, p_strSerie, p_lngCliente)
Set GetNFCe = NFe
End Function

                        
Function GetInfNFe(ByVal p_lngNotaFiscal_ID As Long, ByVal p_lngNotaFiscal As Long, ByVal p_strSerie As String, ByVal p_lngCliente As Long)
    On Error GoTo erro

    Dim rsTabProdFiscal         As New ADODB.Recordset
    Dim strSql                  As String
    Dim nItem                   As Integer
    
    Dim infNFe
    Set infNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
    infNFe.Versao = "4.00"
    Set infNFe.Ide = GetIde(p_lngNotaFiscal, p_strSerie)
    Set infNFe.Emit = GetEmit()
    Set infNFe.Dest = GetDest(p_lngCliente)
    
    'produtos
    strSql = "select TabComanda.nAcesso as IdItemComanda " & vbCrLf
    strSql = strSql & " From TabCadNFCe inner join tabcomanda on tabcomanda.nPedido = TabCadNFCe.nPedido" & vbCrLf
    strSql = strSql & " inner join tabproduto on tabproduto.Acesso = tabcomanda.nProduto " & vbCrLf
    strSql = strSql & " Where TabCadNFCe.nAcesso = " & p_lngNotaFiscal_ID & vbCrLf
    strSql = strSql & " and TabComanda.cStatus = 'A' Order By tabcomanda.nAcesso asc " & vbCrLf
    
    Set rsTabProdFiscal = fcDB_OpenRecordSet(strSql)
    Do While Not rsTabProdFiscal.EOF
        nItem = nItem + 1
        infNFe.AddDet GetDet(rsTabProdFiscal!IdItemComanda, nItem)
        
        rsTabProdFiscal.MoveNext
    Loop
    rsTabProdFiscal.Close
    Set rsTabProdFiscal = Nothing
    ''
                
    'atualizando totalizadores
    glb_ValorTotalLiquidoNF = glb_ValorTotalLiquidoNF + glb_ValorTotal_Acrescimo - glb_ValorTotal_Desconto
    ''
                
    Set infNFe.Total = GetTotal()
    Set infNFe.Pag = GetPag(p_lngNotaFiscal_ID)
    Set infNFe.Transp = GetTransp()
    Set infNFe.InfAdic = GetInfAdic()
    Set infNFe.InfRespTec = GetInfRespTec()
        
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
        
        .FinNFe = 1
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
    
    Set rsGenerico = fcDB_OpenRecordSet("Select top 1 * from tabfarq")
    
    nCodigo_IBGE = rsGenerico!nIBGE
    If nCodigo_IBGE = 0 Then nCodigo_IBGE = 2611606
    
    nCodigo_UF = Mid(nCodigo_IBGE, 1, 2)
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
    With result
        .CNPJ = rsGenerico!CGC
        .XNome = rsGenerico!RazaoSocial
        .XFant = rsGenerico!cTextoBanner
        .IE = rsGenerico!Inscricao
        
        'esses campos sao obrigatorios, caso contrario, ocorre erro Automation Error
        '### criar campos na tabfarq (tabfarq
        ' InscMunicipal nvarchar(200)
        ' CNAE_Primario nvarchar(200)
        ' CSC nvarchar(200)
        ' TCSC nvarchar(200)
        ' CERT_ID nvarchar(200)
        .IM = rsGenerico!InscMunicipal
        .CNAE = rsGenerico!CNAE_Primario
        
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

Function GetDest(ByVal p_lngCliente As Long)
    On Error GoTo erro
    
    Dim result
    Dim ender
    
    Dim nCodigo_IBGE As Long
    Dim nCodigo_UF As Integer
        
    Set rsGenerico = fcDB_OpenRecordSet("Select top 1 * from tabcliente where acesso = " & p_lngCliente)
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
    
    nCodigo_IBGE = rsGenerico!nCidadeIBGE
    If nCodigo_IBGE = 0 Then nCodigo_IBGE = 2611606
    
    nCodigo_UF = Mid(nCodigo_IBGE, 1, 2)
        
    With result
        If rsGenerico!CGC = "ISENTO" Then
            .CNPJ = "99999999000191"
        Else
            .CNPJ = rsGenerico!CGC
        End If
                
        If Val(fcRetornaConteudoDaTabGeral("NfeProdHom")) = 1 Then
            .XNome = rsGenerico!Nome
        Else
            .XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
        End If
        .IndIEDest = 9
        .Email = CheckNull(rsGenerico!cemail)
    End With
    
    Set ender = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
    With ender
        .XLgr = rsGenerico!entEndereco
        .Nro = rsGenerico!EntNumeroEndereco
        .XBairro = rsGenerico!EntBairro
        .CMun = nCodigo_IBGE
        .XMun = rsGenerico!EntCidade
        .UF = nCodigo_UF
        .CEP = Left(Format(CheckNull(rsGenerico!entcep, True), "00000000") & String(8, 0), 8)
        .Fone = Left(Val(Replace(Replace(Replace(CheckNull(rsGenerico!EntFone), "-", ""), ".", ""), ",", "")) & String(10, "0"), 10)
    End With
    
    Set result.EnderDest = ender
    
    Set GetDest = result
    
    Exit Function
erro:
    Utility.TrapException
End Function
Function GetDet(ByVal p_lngProdutoNotaFiscal_ID As Long, ByVal p_nItem As Integer)
    On Error GoTo erro
    
    Dim rsTabProdFiscal         As New ADODB.Recordset
    Dim strSql                  As String
    Dim nItem                   As Integer
    
    Dim result, Prod, Imposto, ICMS, PIS, PISOutr, COFINS, COFINSOutr
    
    Dim ICMSSN101, ICMSSN102, ICMSSN103, ICMSSN201, ICMSSN202, ICMSSN203, ICMSSN300, ICMSSN400, ICMSSN500, ICMSSN900
    
    strSql = "select" & vbCrLf
    strSql = strSql & "TabComanda.nAcesso as IdItemComanda, TabCadNFCe.nAcesso as nAcessoNF, (row_number() over (order by TabComanda.nAcesso)) as nitem, TabComanda.nProduto as nProduto, TabProduto.referencia," & vbCrLf
    strSql = strSql & "TabProduto.descricao, TabCadNFCe.ncfop, TabProduto.UnidVenda as cUnidade, TabComanda.nQtd as nQtd, TabComanda.nvalor as npreco," & vbCrLf
    strSql = strSql & "(TabComanda.nQtd * TabComanda.nValor) as nSubtotal, TabProduto.nCst, (CASE WHEN Isnull(TabComanda.nAliqICMS_Dentro_UF,0) = 0 THEN 0 ELSE (TabComanda.nQtd * TabComanda.nValor) END) as nbaseicms," & vbCrLf
    strSql = strSql & "'0' as nSubsTitIcms, '0' as nIPI, Isnull(TabComanda.nAliqICMS_Dentro_UF,0) as naliqicms_PF," & vbCrLf
    strSql = strSql & "'0' as icmreducaoestado, '0' as icmreducaofora, '' as cicmssubs, Isnull(TabComanda.nAliqICMS_Dentro_UF,0) as ntxicms, '0' as nIncentivo," & vbCrLf
    strSql = strSql & "TabProduto.cSituacaoTributaria, '' as cSituacaoTribFora, (CASE WHEN TabProduto.cSituacaoTributaria = 'F' THEN 5405 ELSE 5102 END) as nCfopItem, isnull(TabProduto.Descricao,'') As cProdDesc," & vbCrLf
    strSql = strSql & "Isnull(TabProduto.nCst_IPI_Saida,0) as nCstPis, Isnull(TabProduto.nCstCofins,0) as nCstCofins, TabProduto.cTipoST, TabProduto.cNCM, TabProduto.nIcmsSubSaidaPercent," & vbCrLf
    strSql = strSql & "'0' as nBaseSt, '0' as nPercentualST, '0' as nDesconto, '0' as nBaseIPI, '0' as nValorFrete," & vbCrLf
    strSql = strSql & "'0' as nValorSeguro, '0' as nAcrescimo, '0' as nValorIPI, '0' as nDespAcessorias, Isnull(TabProduto.nCEST,0) as nCEST, " & vbCrLf
    strSql = strSql & "dbo.fcImpostoProduto(TabComanda.nProduto,(TabComanda.nQtd * TabComanda.nValor)) as nValorImposto, IsNull(TabProduto.ICMCreditoEstado,0) as ICMCreditoEstado, IsNull(TabProduto.ICMCreditoFora,0) as ICMCreditoFora, TabComanda.nCFOP_NF" & vbCrLf
    strSql = strSql & "From TabCadNFCe inner join tabcomanda on tabcomanda.nPedido = TabCadNFCe.nPedido inner join tabproduto on tabproduto.Acesso = tabcomanda.nProduto " & vbCrLf
    strSql = strSql & "Where TabComanda.nAcesso = " & p_lngProdutoNotaFiscal_ID
    
    Set rsTabProdFiscal = fcDB_OpenRecordSet(strSql)
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
    
    result.nItem = p_nItem
    
    Set Prod = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
    With Prod
        .CProd = CheckNull(rsTabProdFiscal("nproduto"), True)
        If Len(CheckNull(rsTabProdFiscal("referencia"))) = 13 Then
            .CEAN = CheckNull(rsTabProdFiscal("referencia"))
        Else
            .CEAN = "SEM GTIN"
        End If
        
        If Val(fcRetornaConteudoDaTabGeral("NfeProdHom")) = 1 Then
            .XProd = CheckNull(rsTabProdFiscal("cProdDesc"))
        Else
            .XProd = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
        End If
        
        If Val(CheckNull(rsTabProdFiscal("cNCM"))) <> 0 Then
            .NCM = CheckNull(rsTabProdFiscal("cNCM"))
        Else
            .NCM = "84714900"
        End If
        
        .CFOP = CheckNull(rsTabProdFiscal("ncfopitem"))
        .UCom = CheckNull(rsTabProdFiscal("cunidade"))
        .QCom = CheckNull(rsTabProdFiscal("nqtd"), True)
        .VUnCom = CheckNull(rsTabProdFiscal("npreco"), True)
        .VProd = CheckNull(rsTabProdFiscal("nsubtotal"), True)
        
        'totalizadores parciais
        glb_ValorTotalLiquidoProduto = glb_ValorTotalLiquidoProduto + CheckNull(rsTabProdFiscal("nsubtotal"), True)
        glb_ValorTotalLiquidoNF = glb_ValorTotalLiquidoNF + CheckNull(rsTabProdFiscal("nsubtotal"), True)
        ''
        
        If Len(CheckNull(rsTabProdFiscal("referencia"))) = 13 Then
            .CEANTrib = CheckNull(rsTabProdFiscal("referencia"))
        Else
            .CEANTrib = "SEM GTIN"
        End If
        
        .UTrib = CheckNull(rsTabProdFiscal("cunidade"))
        .QTrib = CheckNull(rsTabProdFiscal("nqtd"), True)
        .VUnTrib = CheckNull(rsTabProdFiscal("nsubtotal"), True)
        
        If glb_ValorTotal_Desconto > 0 Then
            .vDesc = fcCalculaDescAcresItens(0, rsTabProdFiscal!IdItemComanda, 0, 0, 0, 0)
        End If
        
        If glb_ValorTotal_Acrescimo > 0 Then
            .vOutro = fcCalculaDescAcresItens(0, rsTabProdFiscal!IdItemComanda, 0, 0, 0, 0)
        End If
        
        .IndTot = 1
    End With
    
    Set result.Prod = Prod
    
    Set Imposto = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
    Imposto.VTotTrib = CheckNull(rsTabProdFiscal("nvalorimposto"), True)
    
    glb_ValorTotalImpostoPrevisto = glb_ValorTotalImpostoPrevisto + CheckNull(rsTabProdFiscal("nvalorimposto"), True)
    
    Set ICMS = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")
    
    Select Case CheckNull(rsTabProdFiscal("nCST"))
        Case 0, 102, 103, 300, 400
            Set ICMSSN102 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN102")
            With ICMSSN102
                .Orig = 0
                If CheckNull(rsTabProdFiscal("nCST")) = 0 Then
                    .CSOSN = 102
                Else
                    .CSOSN = CheckNull(rsTabProdFiscal("nCST"))
                End If
            End With
            Set ICMS.ICMSSN102 = ICMSSN102
            Set Imposto.ICMS = ICMS
        
        Case 101
            Set ICMSSN101 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
            With ICMSSN101
                .Orig = 0
                .CSOSN = "101"
            End With
            Set ICMS.ICMSSN101 = ICMSSN101
            Set Imposto.ICMS = ICMS
            
        Case 201
        
            'Tem que ter estas propriedades:
            '
            'Orig = OrigemMercadoria.Nacional
            'CSOSN = "201"
            'ModBCST = ModalidadeBaseCalculoICMSST.Pauta
            'PCredSN = 0#
            'PFCPST = 0#
            'PICMSST = 0#
            'PMVAST = 0#
            'PRedBCST = 0#
            'VBCFCPST = 0#
            'VBCST = 0#
            'VCredICMSSN = 0#
            'VFCPST = 0#
            'VICMSST = 0#
            '
            'Veja pelo manual quais são obrigatórias e quando usar, nem todas são obrigatórias.
            Set ICMSSN201 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN201")
            With ICMSSN201
                .Orig = 0
                .CSOSN = "201"
            End With
            Set ICMS.ICMSSN201 = ICMSSN201
            Set Imposto.ICMS = ICMS
            
        Case 202, 203
            Set ICMSSN202 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN202")
            With ICMSSN202
                .Orig = 0
                .CSOSN = CheckNull(rsTabProdFiscal("nCST"))
            End With
            Set ICMS.ICMSSN202 = ICMSSN202
            Set Imposto.ICMS = ICMS
                
        Case 500
            Set ICMSSN500 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN500")
            
            With ICMSSN500
                .Orig = 0
                .CSOSN = "500"
            End With
            
            Set ICMS.ICMSSN500 = ICMSSN500
            Set Imposto.ICMS = ICMS
            
        Case 900
            Set ICMSSN900 = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN900")
            
            With ICMSSN900
                .Orig = 0
                .CSOSN = "900"
            End With
            
            Set ICMS.ICMSSN900 = ICMSSN900
            Set Imposto.ICMS = ICMS
            
    End Select
                                                       
    Set PIS = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")
    Set PISOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
    
    With PISOutr
        If Val(CheckNull(rsTabProdFiscal("nCstPis"), True)) = 0 Then
            .CST = "99"
        Else
            .CST = Format(CheckNull(rsTabProdFiscal("nCstPis"), True), "00")
        End If
        .VBC = 0#
        .PPIS = 0#
        .VPIS = 0#
    End With
    
    Set PIS.PISOutr = PISOutr
    Set Imposto.PIS = PIS
    
    Set COFINS = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINS")
    Set COFINSOutr = CreateObject("Unimake.Business.DFe.Xml.NFe.COFINSOutr")
    
    With COFINSOutr
        If Val(CheckNull(rsTabProdFiscal("nCstCofins"), True)) = 0 Then
            .CST = "99"
        Else
            .CST = Format(CheckNull(rsTabProdFiscal("nCstCofins"), True), "00")
        End If
        .VBC = 0#
        .PCOFINS = 0#
        .VCOFINS = 0#
    End With
    
    Set COFINS.COFINSOutr = COFINSOutr
    Set Imposto.COFINS = COFINS
    
    Set result.Imposto = Imposto
    
    rsTabProdFiscal.Close
    Set rsTabProdFiscal = Nothing
    
    Set GetDet = result
    
    Exit Function
erro:
    Utility.TrapException
End Function

Function GetTotal()
    On Error GoTo erro
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
        .VProd = glb_ValorTotalLiquidoProduto
        .VFrete = 0
        .VSeg = 0
        .vDesc = glb_ValorTotal_Desconto
        .VII = 0
        .VIPI = 0
        .VIPIDevol = 0
        .VPIS = 0
        .VCOFINS = 0
        .vOutro = glb_ValorTotal_Acrescimo
        .VNF = glb_ValorTotalLiquidoNF
        .VTotTrib = glb_ValorTotalImpostoPrevisto
    End With
    Set result.ICMSTot = ICMSTot
    
    Set GetTotal = result
    
    Exit Function
erro:
    Utility.TrapException
End Function
Function GetPag(ByVal p_lngNotaFiscal_ID As Long)
    On Error GoTo erro

    Dim rsTabPagamentos     As New ADODB.Recordset
    Dim strSql              As String
    
    Dim result, DetPag
    
    strSql = "select" & vbCrLf
    strSql = strSql & "TabCadNFCe_Pagamentos.nAcessoFormaControlX as CodFormaLocal," & vbCrLf
    strSql = strSql & "TabCadNFCe_Pagamentos.nAcessoFormaNFCe as AcessoFormaNFCe," & vbCrLf
    strSql = strSql & "TabCadNFCe_Pagamentos.cCodigoFormaNFCe as CodFormaNFCe," & vbCrLf
    strSql = strSql & "Isnull(TabFormaDePagto.cDescricao,'') as DescricaoFormaNFCe," & vbCrLf
    strSql = strSql & "TabCadNFCe_Pagamentos.nValorPagamento As nValor," & vbCrLf
    strSql = strSql & "TabCadNFCe_Pagamentos.nAcessoAdmNFCe as AcessoAdmNFCe," & vbCrLf
    strSql = strSql & "Isnull(TabCadNFCe_Pagamentos.cCodigoAdmNFCe,'') as CodAdmNFCe," & vbCrLf
    strSql = strSql & "Isnull(TabCadNFCe_Pagamentos.cCnpjADM,0) as CNPJ_Adm," & vbCrLf
    strSql = strSql & "Isnull(TabCadNFCe_Pagamentos.cAutorizacaoTEF,'') as cAutorizacaoTEF," & vbCrLf
    strSql = strSql & "Isnull(TabCadNFCe_Pagamentos.cExigeADM,'N') as ExigeAdm" & vbCrLf
    strSql = strSql & "From TabCadNFCe_Pagamentos" & vbCrLf
    strSql = strSql & "Left Join TabFormaDePagto on TabCadNFCe_Pagamentos.nAcessoFormaControlX = TabFormaDePagto.nAcesso" & vbCrLf
    strSql = strSql & "Where TabCadNFCe_Pagamentos.nAcessoNFCe = " & p_lngNotaFiscal_ID & vbCrLf
    
    Set rsTabPagamentos = fcDB_OpenRecordSet(strSql)
    
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Pag")
    
    Do While Not rsTabPagamentos.EOF
        Set DetPag = CreateObject("Unimake.Business.DFe.Xml.NFe.DetPag")
        
        With DetPag
            .TPag = Int(rsTabPagamentos!CodFormaNFCe)
            .VPag = Format(rsTabPagamentos!nValor, "#,##0.00")
        End With
        DetPag.SetIndPag 1
        
        result.AddDetPag (DetPag)
        
        rsTabPagamentos.MoveNext
    Loop
    
    Set GetPag = result
    
    Exit Function
erro:
    Utility.TrapException
End Function
Function GetTransp()
Dim result, Transporta, Vol
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Transp")

'Set Transporta = CreateObject("Unimake.Business.DFe.Xml.NFe.Transporta")
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

Function GetCobr()
Dim result, Dup, Fat
Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.Cobr")
Set Dup = CreateObject("Unimake.Business.DFe.Xml.NFe.Dup")
Set Fat = CreateObject("Unimake.Business.DFe.Xml.NFe.Fat")

With Fat
    .NFat = "151342"
    .VOrig = 140.3
    .vDesc = 0
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
Function GetInfAdic()
    On Error GoTo erro
    Dim result
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfAdic")
    result.InfCpl = "Imposto Aprox. Total: " & glb_ValorTotalImpostoPrevisto
    Set GetInfAdic = result
    Exit Function
erro:
    Utility.TrapException
End Function

Function GetInfRespTec()
    On Error GoTo erro
    Dim rsSoftH As New ADODB.Recordset
    
    Dim result
    Set result = CreateObject("Unimake.Business.DFe.Xml.NFe.InfRespTec")
    
    Set rsSoftH = fcDB_OpenRecordSet("select top 1 nCNPJ,cResponsavel,cemail,nTelefone from tabsofthouse")
    If rsSoftH.EOF Then
        With result
            .CNPJ = "99999999000191"
            .XContato = "Padrao"
            .Email = "padrao@padrao.com.br"
            .Fone = "0000000000"
        End With
    Else
        With result
            .CNPJ = Trim(rsSoftH!nCnpj)
            .XContato = Trim(rsSoftH!cResponsavel)
            .Email = Trim(rsSoftH!cemail)
            .Fone = Trim(rsSoftH!nTelefone)
        End With
    End If
    Set GetInfRespTec = result
    rsSoftH.Close
    Set rsSoftH = Nothing
    
    Exit Function
erro:
    Utility.TrapException
End Function

'alt 04/11/15 #xxxx alberto.oliveira resolvendo em definitivo rateio de desconto ou acréscimo
Private Function fcCalculaDescAcresItens(ByVal p_lngNotaFiscal As Long, ByVal p_lngIDItem As Long, ByVal p_dblValorPago As Double, ByVal p_dblValorProdutos As Double, ByVal p_dblValorDesconto As Double, ByVal p_dblValorAcrescimo As Double, Optional ByVal p_blnMobile As Boolean) As Double
    On Error GoTo erro
    
    Dim rsTabItens              As New ADODB.Recordset
    Dim rsTabAuxiliar           As New ADODB.Recordset
    
    Dim nValorDescAcresTotal    As Double

    Dim nPercentualFator        As Double
    Dim nValorDescAcresItem     As Double
    
    Dim nIdItem                 As Double
    Dim strSql                  As String
    
    'indica que irá calcular
    If p_lngIDItem = 0 Then
        Call sbDB_ExcluiNRegistro("TabDescAcresItem_TMP", " nTerminal = " & Val(glb_Terminal))
        
        strSql = "select" & vbCrLf
        strSql = strSql & "TabComanda.nAcesso as IdItem," & vbCrLf
        strSql = strSql & "(TabComanda.nQtd * TabComanda.nValor) as nSubtotal " & vbCrLf
        strSql = strSql & "From" & vbCrLf
        strSql = strSql & "TabCadNFCe" & vbCrLf
        strSql = strSql & "inner join tabcomanda on" & vbCrLf
        strSql = strSql & "tabcomanda.nPedido = TabCadNFCe.nPedido" & vbCrLf
        strSql = strSql & "Where" & vbCrLf
        strSql = strSql & "TabCadNFCe.nAcesso = " & p_lngNotaFiscal & vbCrLf
        strSql = strSql & "and TabComanda.cStatus = 'A'" & vbCrLf
        strSql = strSql & "Order By (TabComanda.nQtd * TabComanda.nValor) DESC"
        
        Set rsTabItens = fcDB_OpenRecordSet(strSql)
        If Not rsTabItens.EOF Then
            If p_dblValorPago > 0 And p_dblValorProdutos > 0 Then
                nPercentualFator = p_dblValorPago / p_dblValorProdutos
                nValorDescAcresTotal = IIf(p_dblValorDesconto > 0, p_dblValorDesconto, p_dblValorAcrescimo)
                
                Do While Not rsTabItens.EOF
                    If nValorDescAcresTotal >= 0 Then
                        If p_dblValorDesconto > 0 Then
                            nValorDescAcresItem = Format(rsTabItens!nSubTotal - (rsTabItens!nSubTotal * nPercentualFator), "#,##0.00")
                        Else
                            nValorDescAcresItem = Format((rsTabItens!nSubTotal * nPercentualFator) - rsTabItens!nSubTotal, "#,##0.00")
                        End If
                        
                        'tratando para items que o fator é baixo e ficam zerados, neste caso irá considerar ao menos 0,01 (um centavo) para evitar erro negativo no total
                        If nValorDescAcresItem = 0 Then nValorDescAcresItem = "0,01"
                        ''
                        
                        nValorDescAcresTotal = Format(nValorDescAcresTotal - nValorDescAcresItem, "#,##0.00")
                        Call fcDB_IncluirNDados("TabDescAcresItem_TMP", "nIDItem", rsTabItens!IdItem, "nValor", nValorDescAcresItem, "nTerminal", Val(glb_Terminal))
                        nIdItem = rsTabItens!IdItem
                    End If
                    rsTabItens.MoveNext
                    If rsTabItens.EOF And nValorDescAcresTotal <> 0 Then
                        rsTabItens.MovePrevious
                        Call fcDB_IncluirNDados("TabDescAcresItem_TMP", "nIDItem", nIdItem, "nValor", nValorDescAcresTotal, "nTerminal", Val(glb_Terminal))
                        rsTabItens.MoveNext
                    End If
                Loop
                
            End If
        End If
        rsTabItens.Close
        Set rsTabItens = Nothing
        
    'indica que deverá retornar
    Else
        Set rsTabAuxiliar = fcDB_OpenRecordSet("Select sum(isnull(nValor,0)) as nValorItem from TabDescAcresItem_TMP where nIDItem = " & p_lngIDItem & " and nTerminal = " & Val(glb_Terminal))
        If CheckNull(rsTabAuxiliar!nValorItem, True) > 0 Then
            fcCalculaDescAcresItens = CheckNull(rsTabAuxiliar!nValorItem, True)
        End If
        rsTabAuxiliar.Close
        Set rsTabAuxiliar = Nothing
    End If
    ''
    
    Exit Function
erro:
    Utility.TrapException
End Function

