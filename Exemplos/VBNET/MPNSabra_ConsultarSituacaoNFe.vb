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

        '  Dim ConsultaProtocolo
        If TipoNF = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.ConsultaProtocolo(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.ConsultaProtocolo(Xml, Configuracao)
        End If

        Resposta = TestarAutorizacao()

        If Resposta = "7" Then
            Exit Sub
        End If




        MessageBox.Show(Autorizacao.RetornoWSString)
        MessageBox.Show(Autorizacao.result.cstat & " - " & Autorizacao.Result.XMotivo)




    End Sub
End Module
