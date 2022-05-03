Module MPNSabra_CamposeFuncoes

    Public Enum UFBrasil
        AC = 12
        AL = 27
        AP = 16
        AM = 13
        BA = 29
        CE = 23
        DF = 53
        ES = 32
        GO = 52
        MA = 21
        MT = 51
        MS = 50
        MG = 31
        PA = 15
        PB = 25
        PR = 41
        PE = 26
        PI = 22
        RJ = 33
        RN = 24
        RS = 43
        RO = 11
        RR = 14
        SC = 42
        SP = 35
        SE = 28
        [TO] = 17
        SUFRAMA = 90
        AN = 91
        SVCRS = 94
        SVCSP = 95
        SincChavesRSparaSVSP = 96
        EX = 99
        NaoDefinido = 0
    End Enum
    Public Enum SimNao
        Sim = 1
        Nao = 0
    End Enum
    Public Enum TipoEventoMDFe
        Desconhecido = 0
        CartaCorrecao = 110110
        Cancelamento = 110111
        Encerramento = 110112
        InclusaoCondutor = 110114
        InclusaoDFe = 110115
        EncerramentoFisco = 310112
        RegistroPassagem = 310620
        RegistroPassagemBRId = 510620
        LiberacaoPrazoCancelamento = 240170
    End Enum

    Public Enum TipoEmitenteMDFe
        PrestadorServicoTransporte = 1
        TransportadorCargaPropria = 2
        PrestadorServicoTransporteCteGlobalizado = 3
    End Enum

    Public Enum ModeloDFe
        NFe = 55
        NFCe = 65
        CTe = 57
        MDFe = 58
        CTeOS = 67
    End Enum

    Public Enum ModalidadeTransporteMDFe
        Rodoviario = 1
        Aereo = 2
        Aquaviario = 3
        Ferroviario = 4
    End Enum

    Public Enum TipoEmissao
        Normal = 1
        ContingenciaFSIA = 2
        RegimeEspecialNFF
        ContingenciaEPEC = 4
        ContingenciaFSDA = 5
        ContingenciaSVCAN = 6
        ContingenciaSVCRS = 7
        ContingenciaSVCSP = 8
        ContingenciaOffLine = 9
    End Enum

    Public Enum ProcessoEmissao
        AplicativoContribuinte = 0
        AvulsaPeloFisco = 1
        AvulsaPeloContribuinteSiteFisco = 2
        AplicativoFisco = 3
    End Enum

    Public Enum TipoProprietarioMDFe
        TACAgregado = 0
        TACIndependente = 1
        Outros = 2
        NaoDefinido = 99999
    End Enum

    Public Enum TipoRodado
        Truck = 1
        Toco = 2
        CavaloMecanico = 3
        VAN = 4
        Utilitario = 5
        Outros = 6
    End Enum

    Public Enum TipoCarroceriaMDFe
        NaoAplicavel = 0
        Aberta = 1
        FechadaBau = 2
        Granelera = 3
        PortaContainer = 4
        Sider = 5
    End Enum

    Public Enum TipoCargaMDFe
        GranelSolido = 1
        GranelLiquido = 2
        Frigorificada = 3
        Conteinerizada = 4
        CargaGeral = 5
        Neogranel = 6
        PerigosaGranelSolido = 7
        PerigosaGranelLiquido = 8
        PerigosaFrigorificada = 9
        PerigosaConteinerizada = 10
        PerigosaCargaGeral = 11
    End Enum

    Public Enum ResponsavelSeguroMDFe
        EmitenteMDFe = 1
        ContratanteServicoTransporte = 2
    End Enum

    Public Enum CodigoUnidadeMedidaMDFe
        KG = 1
        TON = 2
    End Enum


    Public Enum TipoDFe
        Desconhecido = -1
        NFe = 0
        NFCe = 1
        CTe = 2
        CTeOS = 3
        MDFe = 4
        NFSe = 5
        SAT = 6
        CFe = 7
        GNRE = 8
    End Enum

    Public Enum IndicadorPresenca
        NaoSeAplica = 0
        OperacaoPresencial = 1
        OperacaoInternet = 2
        OperacaoTeleAtendimento = 3
        NFCeEntregaDomicilio = 4
        PresencialForaEstabelecimento = 5
        OperacaoOutros = 9
    End Enum

    Public Enum IndicadorIntermediario
        OperacaoSemIntermediador = 0
        OperacaoSitePlataformaTerceiro = 1
    End Enum

    Public TextoTela As String
    Public JaFeito As Integer

    Public CertificadoValido As Boolean
    Public Configuracao = New Unimake.Business.DFe.Servicos.Configuracao

    Public VRetorna As Boolean
    Public oX509Cert As X509Certificate2 = New X509Certificate2()
    Public oCertificado As X509Certificate2 = New X509Certificate2()
    Public SemCertificado As X509Certificate2 = New X509Certificate2()

    Public DataHoje As String
    Public Hoje As String

    Public LocalPFX As String
    Public SenhaPfx As String
    Public Versao As String
    Public CUF As Integer
    Public CNPJ As String
    Public Serie As String
    Public TipoNF As Integer
    Public TPAmb As Integer

    Public Autorizacao
    Public Resposta As String = ""
    Public Espera As String = ""
    Public Mensagem As String = (Chr(10) & "Erro na Execução ou a Resposta do Servidor Está Com Tempo Maior Que o Esperado." & Chr(10) & Chr(10) & "Deseja Continuar Com a Espera ? " & Chr(10))

    Function TestarAutorizacao()
        Espera = ""
        Resposta = ""

        While Trim(Espera) = ""
            Try
                Autorizacao.Executar
                Espera = "Foi"

            Catch ex As Exception
                MsgBox(ex.ToString)
                Beep()

                Resposta = CStr(MsgBox(Mensagem, 292))

                If Resposta = "6" Then

                Else
                    Espera = "Foi"
                End If

            End Try

        End While

        TestarAutorizacao = Resposta

    End Function

    Function CriarEvento(ByVal XCorrecao As String, ByVal NSeqEvento As Integer)
        ' Não Utilizei

        Dim DetEventoCCE, Evento, InfEvento
        InfEvento = New Unimake.Business.DFe.Xml.NFe.InfEvento
        DetEventoCCE = New Unimake.Business.DFe.Xml.NFe.DetEventoCCE
        Evento = New Unimake.Business.DFe.Xml.NFe.Evento

        With DetEventoCCE
            .XCorrecao = XCorrecao
            .Versao = "1.00"
        End With

        With InfEvento
            .DetEvento = DetEventoCCE
            .COrgao = CUF
            .ChNFe = Trim(MPNSabra.ChaveNF.Text)
            .CNPJ = Preencher(CNPJ, "14")
            .DhEvento = DateTime.Now
            .TpEvento = 110110
            .NSeqEvento = NSeqEvento
            .VerEvento = "1.00"
            .TpAmb = TPAmb
        End With

        Evento.Versao = "1.00"
        Evento.InfEvento = InfEvento
        CriarEvento = Evento
    End Function

    Public Function Preencher(ByVal Campo As String, ByVal DIGITOS As String) As String

        ' Recebe uma string (campo) e retorna uma string apenas com caracteres de 0 a 9 , no tamanho desejado.
        ' A string será completada com zeros à esquerda até completar o tamanho do campo.

        Dim x As Short
        Dim Z As Short
        Dim y As String = ""
        Dim Linha1 As String
        x = 1
        Linha1 = Trim(Campo)
        Z = Len(Trim(Linha1))
        While x <= Z
            If Mid(Linha1, x, 1) = "0" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "1" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "2" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "3" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "4" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "5" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "6" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "7" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "8" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            If Mid(Linha1, x, 1) = "9" Then
                y = Trim(y) & Mid(Linha1, x, 1)
            End If
            x = x + 1
        End While
        x = Val(DIGITOS)
        Z = Len(Trim(y))
        If x > Z Then
            Z = x
        End If
        Linha1 = Mid(y, Z - x + 1, x)
        Linha1 = New String("0", x - Len(Trim(Linha1))) & Trim(Linha1)
        Preencher = Trim(Linha1)

    End Function
    Public Function TrocaData(ByVal texte As String) As String
        ' Inverte a data desejada  de  ddmmaa para aammdd e vice-versa
        Dim TEXTO As String
        Dim Posso As String
        TEXTO = Preencher(Trim(texte), "06")
        Posso = Mid(TEXTO, 5, 2) & Mid(TEXTO, 3, 2) & Mid(TEXTO, 1, 2)
        TrocaData = Posso
    End Function
    Function DataDoMicro() As String

        ' Obtem a data do Micro e retorna  dd/mm/aa na variavel DataHoje e ddmmaa na variavel Hoje.
        Dim Dia As String
        Dim Mes As String
        Dim Ano As String
        Dim Ana As String

        Dia = Preencher(Trim(CStr(Today.Day)), "02")
        Mes = Preencher(Trim(CStr(Today.Month)), "02")
        Ana = CStr(Today.Year)

        If Len(Trim(Ana)) > 2 Then
            Ano = Preencher(Mid(Ana, 3, 2), "02")
        Else
            Ano = Preencher(Mid(Ana, 1, 2), "02")
        End If
        DataHoje = Dia & "/" & Mes & "/" & Ano
        Hoje = Dia & Mes & Ano
        DataDoMicro = Dia & Mes & Ano



    End Function

    Public Sub LerArquivoINI(ByVal Arquivo As String)

        ' Ler o arquivo de Inicializacao e parametros do Sistema
        ' Atentar para o formato do arquivo , bem como por seus campos.
        ' ' Modelo Default da NF Fiscal   55(75) ou 65       '
        ' *[TIPONF  ]55                                      *
        ' Tipo de Impressao Emissao da NF                    '
        ' 0-Sem  1-Retrato 2 - Paisagem  3 -                 '
        ' 4 NFC  5-Email   6 -           7 -                 '
        ' [TIPOIMP ]1                                        *

        Dim Hfile = FreeFile()

        If Dir(Trim(Arquivo)) = "" Then
            MsgBox("Não Encontrado o Arquivo MPNSabra.ini. Verifique")
            Exit Sub
        End If


        FileOpen(Hfile, Trim(Arquivo), OpenMode.Input)

        Dim Buffer As String = ""

        While Not EOF(Hfile)
            Buffer = LineInput(Hfile)

            If UCase(Trim(Mid(Buffer, 3, 8))) = "CUF" Then
                CUF = Val(Trim(Mid(Buffer, 12, 40)))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "SERIE" Then
                Serie = Trim(Mid(Buffer, 12, 40))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "LOCALPFX" Then
                LocalPFX = Trim(Mid(Buffer, 12, 40))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "SENHAPFX" Then
                SenhaPfx = Trim(Mid(Buffer, 12, 40))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "CNPJ" Then
                CNPJ = Trim(Mid(Buffer, 12, 40))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "TIPONF" Then
                'If UCase(Mid(Buffer, 2, 10)) = "[TIPONF ]" Then
                TipoNF = Val(Trim(Mid(Buffer, 12, 40)))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "TPAMB" Then
                TPAmb = Val(Trim(Mid(Buffer, 12, 40)))
            End If
            If UCase(Trim(Mid(Buffer, 3, 8))) = "VERSAO" Then
                Versao = Trim(Mid(Buffer, 12, 40))
            End If



        End While
        FileClose(Hfile)


    End Sub


    Public Sub Configurar()


        Configuracao.CodigoUF = CUF
        Configuracao.Servico = 0
        Configuracao.CertificadoSenha = Trim(MPNSabra.TSenhaPFX.Text)
        Configuracao.CertificadoDigital = oCertificado
        Configuracao.CertificadoArquivo = Trim(MPNSabra.TLocalPFX.Text)
        Configuracao.TipoEMissao = TipoEmissao.Normal
        Configuracao.TipoAmbiente = TPAmb
        Configuracao.SchemaVersao = Versao
        If TipoNF = 55 Then
            Configuracao.Modelo = ModeloDFe.NFe
            Configuracao.TipoDfe = TipoDFe.NFe
        Else
            Configuracao.Modelo = ModeloDFe.NFCe
            Configuracao.TipoDfe = TipoDFe.NFCe
        End If
        ' configuracao.TpEmiss = "1"
        '  configuracao.xServ = "STATUS"
    End Sub


End Module
