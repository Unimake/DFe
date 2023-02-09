Module MPNSabra_InutilizarNumero
    Public Sub InutilizarNumero()

        '    * ---------------------------------------------------------------------------------
        '* Consumindo o serviço de inutilização de números da NFe
        '* ---------------------------------------------------------------------------------


        Dim InutNFe = New Unimake.Business.DFe.Xml.NFe.InutNFe
        Dim InutNFeInfInut = New Unimake.Business.DFe.Xml.NFe.InutNFeInfInut

        InutNFeInfInut.Ano = "22"
        InutNFeInfInut.CNPJ = Preencher(CNPJ, "14")
        InutNFeInfInut.CUF = CUF
        InutNFeInfInut.Mod = TipoNF
        InutNFeInfInut.NNFIni = Trim(MPNSabra.NumInicial.Text)
        InutNFeInfInut.NNFFin = Trim(MPNSabra.NumFinal.Text)
        InutNFeInfInut.Serie = Serie
        InutNFeInfInut.TpAmb = TPAmb
        InutNFeInfInut.XJust = "Justificativa da inutilizacao de teste"
        InutNFe.Versao = Versao

        InutNFe.InfInut = InutNFeInfInut

        '* Consumir o serviço

        '    Dim Inutilizacao
        If TipoNF = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Inutilizacao(InutNFe, Configuracao)

        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Inutilizacao(InutNFe, Configuracao)
        End If

        Resposta = TestarAutorizacao()

        If Resposta = "7" Then
            Exit Sub
        End If



        MsgBox(Autorizacao.RetornoWSString)
        MsgBox(Autorizacao.result.InfInut.CStat & " - " & Autorizacao.result.InfInut.XMotivo)

        '* 102 Inutilizacao homologada

        If Autorizacao.result.InfInut.CStat = 102 Then
            Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\retorno\")
        Else
            ' Erro na Inutilização
            Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\erro\")
        End If



    End Sub
End Module
