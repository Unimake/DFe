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

        Dim Inutilizacao
        If TipoNF = 55 Then
            Inutilizacao = New Unimake.Business.DFe.Servicos.NFe.Inutilizacao(InutNFe, Configuracao)

        Else
            Inutilizacao = New Unimake.Business.DFe.Servicos.NFCe.Inutilizacao(InutNFe, Configuracao)
        End If

        Try

            Inutilizacao.Executar()


            MsgBox(Inutilizacao.RetornoWSString)
            MsgBox(Inutilizacao.result.InfInut.CStat & " - " & Inutilizacao.result.InfInut.XMotivo)

            '* 102 Inutilizacao homologada

            If Inutilizacao.result.InfInut.CStat = 102 Then
                Inutilizacao.GravarXmlDistribuicao("c:\mpnsabra\retorno\")
            Else
                ' Erro na Inutilização
                Inutilizacao.GravarXmlDistribuicao("c:\mpnsabra\erro\")
            End If


        Catch EX As Exception

            MsgBox(EX.ToString)

        End Try


    End Sub
End Module
