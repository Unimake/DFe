Module MPNSabra_AutorizarNFe

' Funcao ainda em desenvolvimento.
    Public Sub EnviarNFe()

    End Sub

    Public Sub AutorizarPorArquivo()

    End Sub

    Public Sub EnviarXMLNF()

        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe


        Dim Det = New Unimake.Business.DFe.Xml.NFe.Det
        Dim Prod = New Unimake.Business.DFe.Xml.NFe.Prod
        Dim ICMS = New Unimake.Business.DFe.Xml.NFe.ICMS
        Dim Pis = New Unimake.Business.DFe.Xml.NFe.PIS
        Dim PISOutr = New Unimake.Business.DFe.Xml.NFe.PISOutr
        Dim COFINS = New Unimake.Business.DFe.Xml.NFe.COFINS
        Dim COFINSOutr = New Unimake.Business.DFe.Xml.NFe.COFINSOutr
        Dim Imposto = New Unimake.Business.DFe.Xml.NFe.Imposto
        Dim Total = New Unimake.Business.DFe.Xml.NFe.Total
        Dim ICMSTot = New Unimake.Business.DFe.Xml.NFe.ICMSTot
        Dim Transp = New Unimake.Business.DFe.Xml.NFe.Transp
        Dim Vol = New Unimake.Business.DFe.Xml.NFe.Vol
        Dim Cobr = New Unimake.Business.DFe.Xml.NFe.Cobr
        Dim Fat = New Unimake.Business.DFe.Xml.NFe.Fat
        Dim Dup = New Unimake.Business.DFe.Xml.NFe.Dup
        Dim Pag = New Unimake.Business.DFe.Xml.NFe.Pag
        Dim DetPag = New Unimake.Business.DFe.Xml.NFe.DetPag
        Dim InfAdic = New Unimake.Business.DFe.Xml.NFe.InfAdic
        Dim InfRespTec = New Unimake.Business.DFe.Xml.NFe.InfRespTec
        Dim ICMSSN101 = New Unimake.Business.DFe.Xml.NFe.ICMSSN101

End Sub
    Public Function GetFromFileNFe()
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        GetFromFileNFe = NFe.LoadFromFile("c:\mpnsabra\masimo.xml")
    End Function
End Module

 Sub GravarXmlDistribuicao(Autorizacao)
        If Not Autorizacao.result.ProtNFe Is Nothing Then
            'Gravar o XML de distribuição
            '        Dim CStat = New Autorizacao.result.ProtNFe.InfProt.CStat

            '        If CStat = 100 Or
            '       CStat = 110 Or
            '      CStat = 150 Or
            'CStat = 205 Or
            'CStat = 301 Or
            'CStat = 302 Or
            'CStat = 303 Then
            Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\")
            'End If

        End If
    End Sub
