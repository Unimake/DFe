Module MPNSabra_EnviarEventoCCe

    ' Enviar Evento Carta de Correção

    Public Sub EnviarEventoCCe()

        Configurar()

        Dim EnvEvento = New Unimake.Business.DFe.Xml.NFe.EnvEvento
        EnvEvento.Versao = "1.00"
        EnvEvento.IdLote = "000000000000001"

        Dim Evento = New Unimake.Business.DFe.Xml.NFe.Evento
        Evento.Versao = "1.00"

        Dim DetEventoCanc = New Unimake.Business.DFe.Xml.NFe.DetEventoCCE

        With DetEventoCanc
            .Versao = "1.00"
            .XCorrecao = "CFOP errada, segue CFOP correta."
        End With

        Dim InfEvento = New Unimake.Business.DFe.Xml.NFe.InfEvento
        InfEvento.DetEvento = DetEventoCanc

        With InfEvento

            .COrgao = UFBrasil.RJ
            .ChNFe = Trim(MPNSabra.ChaveNF.Text)
            .CNPJ = Preencher(CNPJ, "14")
            .DhEvento = DateTime.Now
            .TpEvento = TipoEventoMDFe.CartaCorrecao ' 110110 
            .NSeqEvento = 1
            .VerEvento = "1.00"
            .TpAmb = TPAmb

        End With

        Evento.InfEvento = InfEvento

        EnvEvento.Evento.Add(Evento)



        '      Dim RecepcaoEvento
        If TipoNF = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.RecepcaoEvento(EnvEvento, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.RecepcaoEvento(EnvEvento, Configuracao)
        End If


        Resposta = TestarAutorizacao()

        If Resposta = "7" Then
            Exit Sub
        End If


        MsgBox(Autorizacao.RetornoWSString)
        MsgBox(Autorizacao.Result.RetEvento(0).InfEvento.CStat & " - " & Autorizacao.Result.RetEvento(0).InfEvento.XMotivo)

        '  Gravar o XML de distribuição se a inutilização foi homologada
        If Autorizacao.result.CStat = 128 Then ''128 = Lote de evento processado com sucesso
                Dim CStatr As String = Autorizacao.Result.RetEvento(0).InfEvento.CStat

                '' 134: Evento homologado
                '' 135: Evento homologado com vinculação da respectiva NFe
                '' 136: Evento homologado sem vinculação com a respectiva NFe (SEFAZ não encontrou a NFe na base dela)
                '' 155: Evento de Cancelamento homologado fora do prazo permitido para cancelamento


                If CStatr = 134 Or CStatr = 135 Or CStatr = 136 Or CStatr = 155 Then

                    Autorizacao.GravarXmlDistribuicao("C:\mpnsabra\retorno\")

                Else ''Evento rejeitado

                    Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\erro\")

                End If
            Else
                MsgBox("Lote não processado. Stat = " & Autorizacao.result.CStat)
            End If




            ' Apenas para gravar a mensagem de erro retornado.
        '          Dim Hfile
        '          Hfile = FreeFile()
        '          FileOpen(Hfile, "erro.txt", OpenMode.Output)
        '          Print(Hfile, EX.ToString)
        '          FileClose(Hfile)




    End Sub



End Module
