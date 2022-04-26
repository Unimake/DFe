  Public Sub EnviarNFe()

        Configurar()

        '     AutorizarPorArquivoNFe()

        '     Exit Sub


        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe

        '
        ' Lendo o arquivo WandreyNfe.xml
        '
        Dim Doc = New System.Xml.XmlDocument

        Doc.Load("c:\mpnsabra\wandreynfe.xml")

        With Xml
            .IdLote = "000000000000001"
            .IndSinc = SimNao.Sim
            .Versao = "4.00"

        End With

        MsgBox("Leu o arquivo .xml ")
        Try


            Xml.NFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.NFe)(Doc))

            '
            '   Neste ponto está apresentando o erro "Variável de objeto ou variável com bloco não definida"
            '
        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try

        MsgBox("Não Passou da Deserialização")




        '
        ' Lendo o arquivo WandreyEnviNfe
        '
        Dim Doc2 = New System.Xml.XmlDocument

        Doc2.Load("c:\mpnsabra\wandreyenvinfe.xml")

        MsgBox("Leu o arquivo .xml ")

        Try


            Xml.NFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.EnviNFe)(Doc2))

            '
            '   Neste ponto está apresentando o erro "Variável de objeto ou variável com bloco não definida"
            '
            '
            ' Se trocar a instrução para EnviNFe
            'Xml.EnviNFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.EnviNFe)(Doc2))
            ' Apresenta erro de não existir EnviNFe no XML (XML.EnviNFE)
            '

        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try


        MsgBox("Não Passou da Deserialização")


End Sub
