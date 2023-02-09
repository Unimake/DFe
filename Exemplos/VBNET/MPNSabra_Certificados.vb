Module MPNSabra_Certificados
    Public Sub CertificadoInstalado()
        ' Acessar Certificado Instalado


        Configuracao.CertificadoArquivo = ""

        Configuracao.CertificadoDigital = SemCertificado


        SelecionarCertificado()

        If VRetorna = True Then

            Dim Xml = New Unimake.Business.DFe.Xml.NFe.ConsStatServ

            Xml.versao = Versao
            Xml.cUF = CUF

            Xml.TpAmb = TPAmb
            ' Xml.TpEmiss = "1"
            Xml.xServ = "STATUS"

            Configuracao.servico = 0

            ' Para Certificados Instalados , a senha não será testada.
            'Configuracao.CertificadoSenha = "xxxxxxxxxx"

            Configuracao.CertificadoDigital = oCertificado

            '            Dim StatusServico
            If TipoNF = 55 Then
                Autorizacao = New Unimake.Business.DFe.Servicos.NFe.StatusServico(Xml, Configuracao)
            Else
                Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.StatusServico(Xml, Configuracao)

            End If


            Resposta = TestarAutorizacao()
            If Resposta = "7" Then
                MPNSabra.Text = TextoTela & " Sem Certificado Instalado"
                Exit Sub
            End If


            ' Após sistema testado , pode comentar os 2 comandos abaixo
            If JaFeito = 2 Then
                MessageBox.Show("Certificado Instalado " & Autorizacao.RetornoWSString)
                MessageBox.Show(Autorizacao.result.cstat & " - " & Autorizacao.Result.XMotivo)
            End If



        End If
    End Sub
    Public Sub SelecionarCertificado()
        Dim store As X509Store = New X509Store("MY", StoreLocation.CurrentUser)

        store.Open(OpenFlags.ReadOnly Or OpenFlags.OpenExistingOnly)

        Dim Collection As X509Certificate2Collection = New X509Certificate2Collection(store.Certificates)
        ' Seleciona os certificados dentro das datas de validade
        Dim Collection1 As X509Certificate2Collection = New X509Certificate2Collection(Collection.Find(X509FindType.FindByTimeValid, DateTime.Now, False))

        '  Dim Collection2 As X509Certificate2Collection = New X509Certificate2Collection(Collection.Find(X509FindType.FindByKeyUsage, X509KeyUsageFlags.DigitalSignature, False))
        ' Dim Collection3 As X509Certificate2Collection = New X509Certificate2Collection(Collection.Find(X509FindType.FindByIssuerName, "Masimo Ind e Com de Art De Vestuario Ltda", False))
        Dim scollection As X509Certificate2Collection
        If JaFeito = 2 Then
            scollection = X509Certificate2UI.SelectFromCollection(Collection1, "Certificado(s) Digital(is) disponível(is)", "Selecione o certificado digital para uso no aplicativo", X509SelectionFlag.SingleSelection)
            '  Dim scollection As X509Certificate2Collection = X509Certificate2UI.SelectFromCollection(Collection1, "Certificado(s) Digital(is) disponível(is)", "Selecione o certificado digital para uso no aplicativo", X509SelectionFlag.SingleSelection)
        Else
            scollection = Collection1
        End If
        If (scollection.Count = 0) Then

            Dim msgResultado As String = "Nenhum certificado digital foi selecionado ou o certificado selecionado está com problemas."
            MessageBox.Show(msgResultado, "Advertência", MessageBoxButtons.OK, MessageBoxIcon.Warning)
            VRetorna = False

        Else

            oX509Cert = scollection(0)
            oCertificado = oX509Cert


            Dim DataInicial As String = oCertificado.NotBefore
            Dim DataFinal As String = oCertificado.NotAfter



            MPNSabra.Text = TextoTela & "   Certificado Instalado  Inicio : " & DataInicial & "  Término : " & DataFinal




            '  Exibir dados do Certificado Selecionado



            '  Dim campo As String = "Data Inicial : " & oCertificado.NotBefore & " Data Final : " & oCertificado.NotAfter & Environment.NewLine
            ' campo = campo & "Certificate Verified ?  " & oCertificado.Verify() & Environment.NewLine
            'campo = campo & "Simple Name :  " & oCertificado.GetNameInfo(X509NameType.SimpleName, True) & Environment.NewLine
            'campo = campo & "Signature Algorithm : " & oCertificado.SignatureAlgorithm.FriendlyName & Environment.NewLine
            'campo = campo & "Public Key: " & oCertificado.PublicKey.Key.ToXmlString(False) & Environment.NewLine
            'campo = campo & "Certificate Archived ?  " & oCertificado.Archived & Environment.NewLine
            'campo = campo & "Length of Raw Data : " & oCertificado.RawData.Length & Environment.NewLine
            '  MsgBox(campo)
            '      X509Certificate2UI.DisplayCertificate(oCertificado)


            VRetorna = True
            store.Close()
        End If

    End Sub

    Public Sub CertificadoPFX()
        ' ---------------------------------------------------------------------------------------------------------
        'Lendo Certificado A1 , arquivo .PFX
        ' ---------------------------------------------------------------------------------------------------------

        ObterValidadeCertificado()

        If CertificadoValido = False Then
            Exit Sub
        End If

        Dim Xml = New Unimake.Business.DFe.Xml.NFe.ConsStatServ

        Xml.versao = Versao
        Xml.cUF = CUF
        Xml.TpAmb = TPAmb
        ' Xml.TpEmiss = "1"
        Xml.xServ = "STATUS"

        '      Dim StatusServico
        If TipoNF = 55 Then

            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.StatusServico(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.StatusServico(Xml, Configuracao)

        End If

        Resposta = TestarAutorizacao()
        If Resposta = "7" Then
            MPNSabra.Text = TextoTela & " Sem Certificado Instalado"
            Exit Sub
        End If

        ' Após sistema testado , pode comentar os 2 comandos seguinte.
        If JaFeito = 2 Then
            MessageBox.Show("Certificado .PFX : " & Autorizacao.RetornoWSString)
            MessageBox.Show(Autorizacao.result.cstat & " - " & Autorizacao.Result.XMotivo)
        End If



    End Sub

    Public Sub ObterValidadeCertificado()


        Dim Hfile
        Hfile = FreeFile()

        CertificadoValido = False



        If Trim(MPNSabra.TLocalPFX.Text) = "" Then
            MsgBox("Informe o Certificado a Ser Utilizado.")
            Exit Sub
        End If


        Dim xx As String = Dir(Trim(MPNSabra.TLocalPFX.Text))
        If Trim(xx) = "" Then
            MsgBox("Arquivo Informado Não Encontrado. Verifique.")
            Exit Sub
        End If


        Dim selCertificado = New Unimake.Security.Platform.CertificadoDigital


        Dim StringCertificado = selCertificado.CarregarCertificadoDigitalA1(Trim(MPNSabra.TLocalPFX.Text), Trim(MPNSabra.TSenhaPFX.Text))


        ' Fiz desta forma para testar a leitura dos dados do certificado
        ' Pode ser feito lendo direto o certificado

        '  Dim DataInicial As String =  StringCertificado.NotBefore
        '  Dim DataFinal as string =  StringCertificado.NotAfter




        Dim Inicio As Double = Strings.InStr(UCase(StringCertificado.ToString), "[NOT BEFORE]")

        If Inicio = 0 Then
            MsgBox("Nâo Encontrada a Data de Validade Inicial do Certificado.")
            Exit Sub
        End If

        While Mid(StringCertificado.ToString, Inicio, 1) <> "]"
            Inicio = Inicio + 1
        End While

        Inicio = Inicio + 1

        While Trim(Mid(StringCertificado.ToString, Inicio, 1)) <> "0" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "1" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "2" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "3" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "4" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "5" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "6" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "7" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "8" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "9"

            Inicio = Inicio + 1
        End While



        Dim DataInicio As String = ""

        While Trim(Mid(StringCertificado.ToString, Inicio, 1)) <> ""
            DataInicio = DataInicio & Mid(StringCertificado.ToString, Inicio, 1)
            Inicio = Inicio + 1
        End While


        Inicio = Strings.InStr(UCase(StringCertificado.ToString), "[NOT AFTER]")

        If Inicio = 0 Then
            MsgBox("Não Encontrada a Data Final de Validade do Certificado")
            Exit Sub
        End If


        While Mid(StringCertificado.ToString, Inicio, 1) <> "]"
            Inicio = Inicio + 1
        End While

        Inicio = Inicio + 1

        While Trim(Mid(StringCertificado.ToString, Inicio, 1)) <> "0" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "1" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "2" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "3" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "4" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "5" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "6" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "7" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "8" _
            And Mid(StringCertificado.ToString, Inicio, 1) <> "9"

            Inicio = Inicio + 1
        End While



        Dim DataFinal As String = ""

        While Trim(Mid(StringCertificado.ToString, Inicio, 1)) <> ""
            DataFinal = DataFinal & Mid(StringCertificado.ToString, Inicio, 1)
            Inicio = Inicio + 1
        End While

        If TrocaData(DataInicio) > TrocaData(DataDoMicro) Then
            MsgBox("Certificado Ainda Não Válido.")
            Exit Sub
        End If
        If TrocaData(DataFinal) < TrocaData(DataDoMicro) Then
            MsgBox("Data de Validade do Certificado expirada.")
            Exit Sub
        End If


        oCertificado = StringCertificado


        Dim DataInicial As String = oCertificado.NotBefore
        Dim DFinal As String = oCertificado.NotAfter



        MPNSabra.Text = TextoTela & "   Certificado .PFX  Inicio : " & DataInicial & "  Término : " & DFinal






        CertificadoValido = True

    End Sub
End Module
