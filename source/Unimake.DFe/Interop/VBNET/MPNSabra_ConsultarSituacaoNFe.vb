Module MPNSabra_ConsultarSituacaoNFe
    Public Sub ConsultarSituacaoNF()

        Dim Xml = New Unimake.Business.DFe.Xml.NFe.ConsSitNFe

        Xml.versao = Versao
        Xml.TpAmb = TPAmb
        Xml.ChNfe = Trim(MPNSabra.ChaveNF.Text)
        ' chave tipo 55
        ' Xml.ChNfe = "33220329359981000140550550000005311810007820"
        ' chave tipo 65
        '        Xml.ChNfe = "33220329359981000140650650000005301810008011"



        Configuracao.TipoEmissao = TipoEmissao.Normal

        Dim ConsultaProtocolo
        If TipoNF = 55 Then
            ConsultaProtocolo = New Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo(Xml, Configuracao)
        Else
            ConsultaProtocolo = New Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo(Xml, Configuracao)
        End If

        Try

            ConsultaProtocolo.Executar()

            MessageBox.Show(ConsultaProtocolo.RetornoWSString)
            MessageBox.Show(ConsultaProtocolo.result.cstat & " - " & ConsultaProtocolo.Result.XMotivo)


        Catch EX As Exception

            MsgBox(EX.ToString)

        End Try


    End Sub
End Module
