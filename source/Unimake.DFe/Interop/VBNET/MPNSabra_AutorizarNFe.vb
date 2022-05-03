Module MPNSabra_AutorizarNFe

    Public Sub AutorizarPorArquivoNFe()


        Configurar()

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

        With InfAdic
            .InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;"
        End With

        With InfRespTec

            .CNPJ = "06117473000150"
            .XContato = "Wandrey Mundin Ferreira"
            .Email = "wandrey@unimake.com.br"
            .Fone = "04431414900"
        End With



        With Vol

            .QVol = 1
            .Esp = "LU"
            .Marca = "UNIMAKE"
            .PesoL = 0.000
            .PesoB = 0.000

        End With

        Dim Jura As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Vol)
        Jura.Add(Vol)

        With Transp
            .ModFrete = 9 'ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
            .Vol = Jura
        End With


        With Fat
            .NFat = "057910"
            .VOrig = 169.8
            .VDesc = 0
            .VLiq = 169.8
        End With

        With Dup
            .NDup = "001"
            .DVenc = DateTime.Now
            .VDup = 169.8
        End With


        With DetPag
            .IndPag = 0 'IndicadorPagamento.PagamentoVista
            .TPag = 10 'MeioPagamento.Dinheiro
            .VPag = 169.8
        End With
        Dim Jura1 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.DetPag)
        Jura1.Add(DetPag)

        With Pag
            .DetPag = Jura1
        End With


        '      Dim Jura2 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Fat)
        '      Jura2.Add(Fat)
        Dim Jura3 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Dup)
        Jura3.Add(Dup)
        Dim Jura4 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Pag)
        Jura4.Add(Pag)
        With Cobr

            .Fat = Fat
            .Dup = Jura3

        End With


        With ICMSTot
            .VBC = 0
            .VICMS = 0
            .VICMSDeson = 0
            .VFCP = 0
            .VBCST = 0
            .VST = 0
            .VFCPST = 0
            .VFCPSTRet = 0
            .VProd = 169.8
            .VFrete = 0
            .VSeg = 0
            .VDesc = 0
            .VII = 0
            .VIPI = 0
            .VIPIDevol = 0
            .VPIS = 0
            .VCOFINS = 0
            .VOutro = 0
            .VNF = 169.8
            .VTotTrib = 25.26
        End With

        With Total
            '			
            .ICMSTot = ICMSTot

        End With




        With PISOutr
            .CST = "99"
            .VBC = 0.00
            .PPIS = 0.00
            .VPIS = 0.00
        End With

        With Pis
            .PISOutr = PISOutr
        End With



        With COFINSOutr

            .CST = "99"
            .VBC = 0.00
            .PCOFINS = 0.00
            .VCOFINS = 0.00

        End With

        With COFINS
            .cofinsoutr = COFINSOutr
        End With


        With ICMSSN101
            .Orig = 0 'OrigemMercadoria.Nacional
            .PCredSN = 2.8255
            .VCredICMSSN = 2.4
        End With

        With Prod
            .CProd = "01042"
            .CEAN = "SEM GTIN"
            .XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
            .NCM = "84714900"
            .CFOP = "6101"
            .UCom = "LU"
            .QCom = 1.0
            .VUnCom = 84.9
            .VProd = 84.9
            .CEANTrib = "SEM GTIN"
            .UTrib = "LU"
            .QTrib = 1.0
            .VUnTrib = 84.9
            .IndTot = 1 'SimNao.Sim
            .XPed = "300474"
            .NItemPed = 1
        End With



        With ICMS
            .icmssn101 = ICMSSN101
        End With



        Dim Jura5 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.ICMS)
        Jura5.Add(ICMS)



        With Imposto
            .VTotTrib = 12.63
            .ICMS = Jura5
            .PIS = Pis
            '            .PISOutr = PISOutr
            .COFINS = COFINS
            '           .COFINSOutr = COFINSOutr
        End With




        Dim Emit As New Unimake.Business.DFe.Xml.NFe.Emit
        Dim EnderEmit As New Unimake.Business.DFe.Xml.NFe.EnderEmit
        Dim Dest As New Unimake.Business.DFe.Xml.NFe.Dest
        Dim EnderDest As New Unimake.Business.DFe.Xml.NFe.EnderDest
        Dim Ide As New Unimake.Business.DFe.Xml.NFe.Ide
        With EnderDest
            .XLgr = "AVENIDA DA SAUDADE"
            .Nro = "1555"
            .XBairro = "CAMPOS ELISEOS"
            .CMun = 3543402
            .XMun = "RIBEIRAO PRETO"
            .UF = UFBrasil.SP
            .CEP = "14080000"
            .Fone = "01639611500"
        End With


        With Dest
            .CNPJ = "04218457000128"
            .XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
            .EnderDest = EnderDest

            .IndIEDest = 0 'IndicadorIEDestinatario.ContribuinteICMS

            .IE = "582614838110"
            .Email = "janelaorp@janelaorp.com.br"
        End With

        With EnderEmit
            .XLgr = "RUA ANTONIO FELIPE"
            .Nro = "1500"
            .XBairro = "CENTRO"
            .CMun = 4118402
            .XMun = "PARANAVAI"
            .UF = UFBrasil.PR
            .CEP = "87704030"
            .Fone = "04431414900"
        End With

        With Emit
            .CNPJ = "06117473000150"
            .XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
            .XFant = "UNIMAKE - PARANAVAI"
            .EnderEmit = EnderEmit

            .IE = "9032000301"
            .IM = "14018"
            .CNAE = "6202300"
            .CRT = 1 'CRT.SimplesNacional
        End With

        With Det
            .NITEM = 1
            .Prod = Prod
            .Imposto = Imposto
        End With


        Dim Jura2 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.Det)
        Jura2.Add(Det)

        'Dim NFe = New List < NFe >

        '     New NFe
        '
        '       Dim InfNFe = New List < InfNFe >
        '              New InfNFe



        '
        With Ide
            .CNF = "1234567"
            .CUF = UFBrasil.PR
            .NatOp = "VENDA PRODUC.DO ESTABELEC"
            .Mod = ModeloDFe.NFe
            .Serie = 1
            .NNF = 57972
            .DhEmi = DateTime.Now
            .DhSaiEnt = DateTime.Now
            .TpNF = 1 'TipoOperacao.Saida
            .IdDest = 1 'DestinoOperacao.OperacaoInterestadual
            .CMunFG = 4118402
            .TpImp = 1 'FormatoImpressaoDANFE.NormalRetrato
            .TpEmis = TipoEmissao.Normal
            .TpAmb = TPAmb
            .FinNFe = 1 'FinalidadeNFe.Normal
            .IndFinal = 1 'SimNao.Sim
            .IndPres = IndicadorPresenca.OperacaoPresencial
            .ProcEmi = ProcessoEmissao.AplicativoContribuinte
            .VerProc = "TESTE 1.00"

        End With


        With InfNFe

            InfNFe.Versao = Versao
            InfNFe.Ide = Ide

            .Emit = Emit
            .Dest = Dest


            .Det = Jura2

            '				New Det
            '				{
            'NItem = 2,
            'Prod = New Prod
            '					{
            'CProd = "11111",
            'CEAN = "SEM GTIN",
            'XProd = "TESTE DO PRODUTO DO ITEM 2",
            'NCM = "84714900",
            'CFOP = "6101",
            'UCom = "LU",
            'QCom = 1.0,
            'VUnCom = 84.9,
            'VProd = 84.9,
            'CEANTrib = "SEM GTIN",
            'UTrib = "LU",
            'QTrib = 1.0,
            'VUnTrib = 84.9,
            'IndTot = SimNao.Sim,
            'XPed = "300474",
            'NItemPed = 1
            '					},
            'Imposto = New Imposto
            '					{
            'VTotTrib = 12.63,
            'ICMS = New List < ICMS >
            '                       {
            '                          New ICMS
            '						{
            'ICMSSN101 = New ICMSSN101
            '								{
            'Orig = OrigemMercadoria.Nacional,
            'PCredSN = 2.8255,
            'VCredICMSSN = 2.4
            '								}
            '							}
            '						},
            'PIS = New PIS
            '						{                                    
            'PISOutr = New PISOutr
            '							{
            'CST = "99",
            'VBC = 0.00,
            'PPIS = 0.00,
            'VPIS = 0.00
            '							}
            '						},
            'COFINS = New COFINS
            '						{
            'COFINSOutr = New COFINSOutr
            '							{
            'CST = "99",
            'VBC = 0.00,
            'PCOFINS = 0.00,
            'VCOFINS = 0.00
            '							}
            '						}
            '					}
            '				}
            '			},
            .Total = Total


            .Transp = Transp

            .Cobr = Cobr
            .Pag = Pag


            .InfAdic = InfAdic

            .InfRespTec = InfRespTec

            '
        End With


        Dim Jura6 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.InfNFe)
        Jura6.Add(InfNFe)

        With NFe
            .InfNFe = Jura6
        End With

        'Dim Jura7 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.NFe)
        'Jura7.Add(NFe)


        Xml.Versao = Versao
        Xml.IdLote = "000000000000001"
        Xml.IndSinc = 0

        'Xml.nfe = Jura7
        Xml.nfe.add(NFe)





        Dim Autorizacao
        If TipoNF = 55 Then
            Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Autorizacao(Xml, Configuracao)
        Else
            Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Autorizacao(Xml, Configuracao)
        End If

        Autorizacao.Executar()
        '
        '//Gravar o arquivo do conteúdo retornado em uma pasta qualquer para ter em segurança. Pode-se também gravar na base de dados. Fica a critério de cada um.
        'File.WriteAllText(@"c:\testenfe\retorno\nomearquivoretorno.xml", autorizacao.RetornoWSString);'''

        If Autorizacao.Result.ProtNFe <> "" Then
            '{
            '	//Gravar o XML de distribuição se a nota foi autorizada ou denegada
            '	Switch(autorizacao.Result.ProtNFe.InfProt.CStat)
            '	{
            '		Case 100 :  //Autorizado o uso da NF-e
            '		Case 110 :  //Uso Denegado
            '		Case 150 :  //Autorizado o uso da NF-e, autorização fora de prazo
            '		Case 205 :  //NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
            '		Case 301 :  //Uso Denegado: Irregularidade fiscal do emitente
            '       Case 302 :  //Uso Denegado: Irregularidade fiscal do destinatário
            '      Case 303 :  //Uso Denegado: Destinatário não habilitado a operar na UF
            '         autorizacao.GravarXmlDistribuicao(@"c:\testenfe\");
            '		var docProcNFe = autorizacao.NfeProcResult.GerarXML();
            '       MessageBox.Show(autorizacao.NfeProcResult.NomeArquivoDistribuicao);
            '	break;
            '
            'Default:    //NF Rejeitada
            '				//tratamentos diversos
            '		break;
            '}'
            '}

        End If
    End Sub





    Sub GravarXmlDistribuicao(Autorizacao)
        '  Gravar o XML recebido da SEFAZ
        'If Not Autorizacao.result.ProtNFe Is Nothing Then

        'Dim CStat = New Autorizacao.result.ProtNFe.InfProt.CStat

        'If CStat = 100 Or
        'CStat = 110 Or
        ''      CStat = 150 Or
        ''CStat = 205 Or
        ''CStat = 301 Or
        ''CStat = 302 Or
        ''CStat = 303 Then
        '  Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\")
        '    End If

        'End If
    End Sub
    Public Sub EnviarNFe()

        'Enviar NFE. Usando o .XML previamente gerado.

        Configurar()


        Dim Xml = New Unimake.Business.DFe.Xml.NFe.EnviNFe
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        Dim InfNFe = New Unimake.Business.DFe.Xml.NFe.InfNFe
        '
        Dim Doc = New System.Xml.XmlDocument

        Doc.load(Trim(MPNSabra.ChaveNF.Text))

        With Xml
            .IdLote = "000000000000021"
            .IndSinc = SimNao.Sim
            .Versao = "4.00"

        End With

        Dim Jura6 As New System.Collections.Generic.List(Of Unimake.Business.DFe.Xml.NFe.NFe)
        Xml.NFe = Jura6


        Try

            Xml.NFe.Add(Unimake.Business.DFe.Utility.XMLUtility.Deserializar(Of Unimake.Business.DFe.Xml.NFe.NFe)(Doc))

        Catch ex As Exception
            MsgBox(ex.ToString)
        End Try


        Try
            '           Dim Autorizacao
            If TipoNF = 55 Then
                Autorizacao = New Unimake.Business.DFe.Servicos.NFe.Autorizacao(Xml, Configuracao)
            Else
                Configuracao.CSCIdToken = "000002"
                Configuracao.CSC = "700AF32597135C031195D7160554ECF5TJGI"
                Autorizacao = New Unimake.Business.DFe.Servicos.NFCe.Autorizacao(Xml, Configuracao)
            End If

            '          Dim Espera As String = " "
            '         Dim Mensagem As String = (Chr(10) & "Retorno dos Arquivos com Tempo Maior que o Esperado." & Chr(10) & Chr(10) & "Deseja Continuar com a Espera ? " & Chr(10))
            '        Dim Resposta As String = ""

            Resposta = TestarAutorizacao()

            If Resposta = "7" Then
                Exit Sub
            End If

            MsgBox(Autorizacao.RetornoWSString)
            MsgBox(Autorizacao.Result.Cstat & " - " & Autorizacao.Result.XMotivo)

            'Processo Sincrono

            '       If (Autorizacao.Result.ProtNFe = vbNull) Then
            '      Else
            '       MsgBox("Result : " & Autorizacao.Result.ProtNFe.ToString)
            Dim CStato = Autorizacao.Result.ProtNFe.InfProt.CStat


            If CStato =
            100 Or      '//Autorizado o uso da NFe
                      CStato = 110 Or    '//Uso Denegado
                    CStato = 150 Or    '//Autorizado o uso da NF-e, autorização fora de prazo
                  CStato = 205 Or     '//NF-e está denegada na base de dados da SEFAZ [nRec:999999999999999]
                CStato = 301 Or     '//Uso Denegado: Irregularidade fiscal do emitente
              CStato = 302 Or      '//Uso Denegado: Irregularidade fiscal do destinatário
            CStato = 303 Then      '//Uso Denegado: Destinatário não habilitado a operar na UF

                ' gravar o arquivo autorizado
                Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\retorno\")

                ' Gerar o Objeto para pegar a string e gravar em banco de dados

                Dim docProcNFe As System.Xml.XmlDocument
                docProcNFe = Autorizacao.NfeProcResult.GerarXML()

                ' mostra o nome do arquivo gerado
                MessageBox.Show(Autorizacao.NfeProcResult.NomeArquivoDistribuicao)

            Else
                MsgBox("Nota Rejeitada : " & CStato & " - " & Autorizacao.Result.ProtNFe.InfProt.XMotivo)

            End If



            '     End If

            ' assincrono

            '   If (Autorizacao.Result.ProtNFe <> vbNull) Then
            '   MsgBox("Result : " & Autorizacao.Result.ProtNFe.ToString)
            '  Dim CStato = Autorizacao.Result.CStat
            ' MsgBox("Estado : " & CStato)
            'If CStato = 103 Then  ' Lote recebido com sucesso
            'Else
            'Dim xmlRec = Xml.ConsReciNFe

            'xmlRec.Versao = "4.00"
            'xmlRec.TPAmb = TPAmb
            'xmlRec.NRec = Autorizacao.Result.InfRec.NRec


            'Dim retAutorizacao = New Unimake.Business.DFe.Servicos.NFe.RetAutorizacao(xmlRec, Configuracao)
            'retAutorizacao.Executar()

            'Autorizacao.RetConsReciNFe = retAutorizacao.Result

            'Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\retorno\")


            'End If

            'End If



            '    Dim docProcNFe = Autorizacao.NfeProcResults.GerarXML()

            'MsgBox("procnfe : " & docProcNFe.ToString)

            'MessageBox.Show(Autorizacao.NfeProcResults.NomeArquivoDistribuicao)





            'MsgBox(Autorizacao.nfeprocresults.ToString)

            'Dim Retorno As New Unimake.Business.DFe.Xml.NFe.RetEnviNFe

            'MsgBox(Retorno.ToString)

            '  Dim Volta As New Unimake.Business.DFe.Servicos.NFe.Autorizacao.NfeProcResults


            '  Gravar o XML recebido da SEFAZ


            ' ver  retconsrecinfe   e retconssitnfe


            'If Not Autorizacao.result.ProtNFe Is Nothing Then

            'Dim CStat = New Autorizacao.result.ProtNFe.InfProt.CStat

            'If CStat = 100 Or
            'CStat = 110 Or
            ''      CStat = 150 Or
            ''CStat = 205 Or
            ''CStat = 301 Or
            ''CStat = 302 Or
            ''CStat = 303 Then
            '           Autorizacao.GravarXmlDistribuicao("c:\mpnsabra\")
            '    End If

            'End If

        Catch ex As Exception
            MsgBox("Autorizacao : " & ex.ToString)
        End Try


    End Sub


    Public Function GetFromFileNFe()
        Dim NFe = New Unimake.Business.DFe.Xml.NFe.NFe
        GetFromFileNFe = NFe.LoadFromFile(Trim(MPNSabra.ChaveNF.Text))
    End Function
End Module
