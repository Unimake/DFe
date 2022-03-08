* ---------------------------------------------------------------------------------
* Enviar Nfe de forma assincrona
* ---------------------------------------------------------------------------------
Function EnviarNfeAssincrono()
   Local oInicializarConfiguracao
   Local oXml, oNfe, oInfNFe, oIde, oEmit, oEnderEmit, oDest, oEnderDest
   Local oDet, oProd
   Local oImposto, oICMS, oICMSSN101, oPIS, oPISOutr
   Local oRetAutorizacao
   Local I

 * Criar configuraçao básica para consumir o serviço
   oInicializarConfiguracao = CreateObject("Unimake.Business.DFe.Servicos.Configuracao")
   oInicializarConfiguracao:TipoDfe = 0 // 0=nfe
   oInicializarConfiguracao:Servico = 6 // 6=Autorização Nfe
   oInicializarConfiguracao:TipoEmissao = 1 // 1=Normal
   oInicializarConfiguracao:CertificadoSenha = "12345678"
   oInicializarConfiguracao:CertificadoArquivo = "C:\Projetos\certificados\UnimakePV.pfx"

 * Criar XML
   // criar tag EnviNfe
   oXml = CreateObject("Unimake.Business.DFe.Xml.NFe.EnviNFe")
   oXml:Versao = "4.00"
   oXml:IdLote = "000000000000001"
   oXml:IndSinc = 0 // 1=Sim 0=Nao
   
   // criar tag Nfe
   onfe = CreateObject("Unimake.Business.DFe.Xml.NFe.NFe")

   // criar tag InfNfe
   oInfNFe = CreateObject("Unimake.Business.DFe.Xml.NFe.InfNFe")
   oInfNFe:Versao = "4.00"

   // cria tag Ide
   oIde = CreateObject("Unimake.Business.DFe.Xml.NFe.Ide")
   oIde:CUF      = 41 // Brasil.PR
   oIde:NatOp    = "VENDA PRODUC.DO ESTABELEC"
   oIde:Mod      = 55 // NFe
   oIde:Serie    = 1
   oIde:NNF      = 57980
   oIde:DhEmi    = DateTime() // Transform(DtoS(Date()),"@R 9999-99-99") + " " + Time() + ".0-03")
   oIde:DhSaiEnt = DateTime()
   oIde:TpNF     = 1 // Saida
   oIde:IdDest   = 2 // OperacaoInterestadual
   oIde:CMunFG   = 4118402
   oIde:TpImp    = 1 // FormatoImpressaoDANFE.NormalRetrato
   oIde:TpEmis   = 1 // TipoEmissao.Normal
   oIde:TpAmb    = 2 // TipoAmbiente.Homologacao
   oIde:FinNFe   = 1 // FinalidadeNFe.Normal
   oIde:IndFinal = 1 // SimNao.Sim
   oIde:IndPres  = 1 // IndicadorPresenca.OperacaoPresencial
   oIde:ProcEmi  = 0 // ProcessoEmissao.AplicativoContribuinte
   oIde:VerProc  = "TESTE 1.00"

   // adicionar a tag Ide dentro da tag InfDfe
   oInfNFe:Ide = oIde

   // criar tag Emit
   oEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.Emit")
   oEmit:CNPJ  = "06117473000150"
   oEmit:XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA"
   oEmit:XFant = "UNIMAKE - PARANAVAI"
   oEmit:IE    = "9032000301"
   oEmit:IM    = "14018"
   oEmit:CNAE  = "6202300"
   oEmit:CRT   = 1 // CRT.SimplesNacional

   oEnderEmit = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderEmit")
   oEnderEmit:XLgr    = "RUA PAULO ANTONIO COSTA"
   oEnderEmit:Nro     = "575"
   oEnderEmit:XBairro = "CENTRO"
   oEnderEmit:CMun    = 4118402
   oEnderEmit:XMun    = "PARANAVAI"
   oEnderEmit:UF      = 41 // UFBrasil.PR
   oEnderEmit:CEP     = "87707210"
   oEnderEmit:Fone    = "04431421010"

   // adicionar a tag EnderEmit dentro da tag Emit 
   oEmit:EnderEmit = oEnderEmit

   // adicionar a tag Emit dentro da tag InfNfe
   oInfNfe:Emit = oEmit

   // criar tag Emit
   oDest = CreateObject("Unimake.Business.DFe.Xml.NFe.Dest")
   oDest:CNPJ      = "04218457000128"
   oDest:XNome     = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
   oDest:IndIEDest = 1 // IndicadorIEDestinatario.ContribuinteICMS,
   oDest:IE        = "582614838110"
   oDest:Email     = "janelaorp@janelaorp.com.br"
   
   oEnderDest = CreateObject("Unimake.Business.DFe.Xml.NFe.EnderDest")
   oEnderDest:XLgr    = "AVENIDA DA SAUDADE"
   oEnderDest:Nro     = "1555"
   oEnderDest:XBairro = "CAMPOS ELISEOS"
   oEnderDest:CMun    = 3543402
   oEnderDest:XMun    = "RIBEIRAO PRETO"
   oEnderDest:UF      = 35 // UFBrasil.SP
   oEnderDest:CEP     = "14080000"
   oEnderDest:Fone    = "01639611500"

   // adicionar a tag EnderDest dentro da tag Dest 
   oDest:EnderDest = oEnderDest

   // adicionar a tag Emit dentro da tag InfNfe
   oInfNfe:Dest = oDest

   For I = 1 To 3 // 3 produtos para teste 
       // criar tag Det
       oDet           = CreateObject("Unimake.Business.DFe.Xml.NFe.Det")
       oDet:NItem     = I

       oProd          = CreateObject("Unimake.Business.DFe.Xml.NFe.Prod")
       oProd:CProd    = StrZero(I,5)
       oProd:CEAN     = "SEM GTIN"
       oProd:XProd    = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL"
       oProd:NCM      = "84714900"
       oProd:CFOP     = "6101"
       oProd:UCom     = "LU"
       oProd:QCom     = 1.00
       oProd:VUnCom   = 84.90
       oProd:VProd    = 84.90
       oProd:CEANTrib = "SEM GTIN"
       oProd:UTrib = "LU"
       oProd:QTrib = 1.00
       oProd:VUnTrib = 84.90
       oProd:IndTot = 1 // SimNao.Sim
       oProd:XPed = "300474"
       oProd:NItemPed = 1

    // adicionar a tag Prod dentro da tag Det
       oDet:Prod = oProd

    // criar tag Imposto
       oImposto          = CreateObject("Unimake.Business.DFe.Xml.NFe.Imposto")
       oImposto:VTotTrib = 12.63

    // criar tag Icms
       oICMS             = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMS")

    // criar tag ICMSSN101
       oICMSSN101            = CreateObject("Unimake.Business.DFe.Xml.NFe.ICMSSN101")
       oICMSSN101:Orig       = 0 // OrigemMercadoria.Nacional
       oICMSSN101:PCredSN     = 2.8255
       oICMSSN101:VCredICMSSN = 2.40

    // adicionar a tag ICMSSN101 dentro da tag ICMS
       oICMS:ICMSSN101 = oICMSSN101

    // adicionar a tag ICMS dentro da tag Imposto
       oImposto:AddIcms(oICMS)

    // criar tag PIS
       oPIS           = CreateObject("Unimake.Business.DFe.Xml.NFe.PIS")

    // criar tag PISOutr
       oPISOutr      = CreateObject("Unimake.Business.DFe.Xml.NFe.PISOutr")
       oPISOutr:CST  = "99"
       oPISOutr:VBC  = 0.00
       oPISOutr:PPIS = 0.00
       oPISOutr:VPIS = 0.00

    // adicionar a PisOutr dentro da tag Pis
       oPIS:PISOutr = oPISOutr   

    // adicionar a tag Pis dentro da tag Imposto
       oImposto:PIS = oPIS

    // adicionar a tag Imposto dentro da tag Det
       oDet:Imposto = oImposto

    // adicionar a tag Det dentro da tag InfNfe 
       oInfNfe:AddDet(oDet)
   Next I

/*
                        COFINS = new Unimake.Business.DFe.Xml.NFe.COFINS
                        {
                            COFINSOutr = new Unimake.Business.DFe.Xml.NFe.COFINSOutr
                            {
                                CST = "99",
                                VBC = 0.00,
                                PCOFINS = 0.00,
                                VCOFINS = 0.00
                            }
                        }
                    }

                });
            }

            return dets;
*/

   // adicionar a tag InfNfe dentro da tag Nfe 
   oNfe:AddInfNFe(oInfNFe)

   // adiconar a tag nfe dentro da tag EnviNfe 
   oXml:AddNfe(oNfe)

/*
                                },
                                Det = CriarDet(),
                                Total = new Unimake.Business.DFe.Xml.NFe.Total
                                {
                                    ICMSTot = new Unimake.Business.DFe.Xml.NFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 169.80,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 169.80,
                                        VTotTrib = 25.26
                                    }
                                },
                                Transp = new Unimake.Business.DFe.Xml.NFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<Unimake.Business.DFe.Xml.NFe.Vol>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new Unimake.Business.DFe.Xml.NFe.Cobr()
                                {
                                    Fat = new Unimake.Business.DFe.Xml.NFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 169.80,
                                        VDesc = 0,
                                        VLiq = 169.80
                                    },
                                    Dup = new List<Unimake.Business.DFe.Xml.NFe.Dup>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 169.80
                                        }
                                    }
                                },
                                Pag = new Unimake.Business.DFe.Xml.NFe.Pag
                                {
                                    DetPag = new List<Unimake.Business.DFe.Xml.NFe.DetPag>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 169.80
                                        }
                                    }
                                },
                                InfAdic = new Unimake.Business.DFe.Xml.NFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new Unimake.Business.DFe.Xml.NFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    },
                    new Unimake.Business.DFe.Xml.NFe.NFe
                    {
                        InfNFe = new List<Unimake.Business.DFe.Xml.NFe.InfNFe>
                        {
                            new Unimake.Business.DFe.Xml.NFe.InfNFe
                            {
                                Versao = "4.00",
                                Ide = new Unimake.Business.DFe.Xml.NFe.Ide
                                {
                                    CUF = UFBrasil.PR,
                                    NatOp = "VENDA PRODUC.DO ESTABELEC",
                                    Mod = ModeloDFe.NFe,
                                    Serie = 1,
                                    NNF = 57980,
                                    DhEmi = DateTime.Now,
                                    DhSaiEnt = DateTime.Now,
                                    TpNF = TipoOperacao.Saida,
                                    IdDest = DestinoOperacao.OperacaoInterestadual,
                                    CMunFG = 4118402,
                                    TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                    TpEmis = TipoEmissao.Normal,
                                    TpAmb = TipoAmbiente.Homologacao,
                                    FinNFe = FinalidadeNFe.Normal,
                                    IndFinal = SimNao.Sim,
                                    IndPres = IndicadorPresenca.OperacaoPresencial,
                                    ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                    VerProc = "TESTE 1.00"
                                },
                                Emit = new Unimake.Business.DFe.Xml.NFe.Emit
                                {
                                    CNPJ = "06117473000150",
                                    XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                    XFant = "UNIMAKE - PARANAVAI",
                                    EnderEmit = new Unimake.Business.DFe.Xml.NFe.EnderEmit
                                    {
                                        XLgr = "RUA ANTONIO FELIPE",
                                        Nro = "1500",
                                        XBairro = "CENTRO",
                                        CMun = 4118402,
                                        XMun = "PARANAVAI",
                                        UF = UFBrasil.PR,
                                        CEP = "87704030",
                                        Fone = "04431414900"
                                    },
                                    IE = "9032000301",
                                    IM = "14018",
                                    CNAE = "6202300",
                                    CRT = CRT.SimplesNacional
                                },
                                Dest = new Unimake.Business.DFe.Xml.NFe.Dest
                                {
                                    CNPJ = "04218457000128",
                                    XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                    EnderDest = new Unimake.Business.DFe.Xml.NFe.EnderDest
                                    {
                                        XLgr = "AVENIDA DA SAUDADE",
                                        Nro = "1555",
                                        XBairro = "CAMPOS ELISEOS",
                                        CMun = 3543402,
                                        XMun = "RIBEIRAO PRETO",
                                        UF = UFBrasil.SP,
                                        CEP = "14080000",
                                        Fone = "01639611500"
                                    },
                                    IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                    IE = "582614838110",
                                    Email = "janelaorp@janelaorp.com.br"
                                },
                                Det = CriarDet(),
                                Total = new Unimake.Business.DFe.Xml.NFe.Total
                                {
                                    ICMSTot = new Unimake.Business.DFe.Xml.NFe.ICMSTot
                                    {
                                        VBC = 0,
                                        VICMS = 0,
                                        VICMSDeson = 0,
                                        VFCP = 0,
                                        VBCST = 0,
                                        VST = 0,
                                        VFCPST = 0,
                                        VFCPSTRet = 0,
                                        VProd = 169.80,
                                        VFrete = 0,
                                        VSeg = 0,
                                        VDesc = 0,
                                        VII = 0,
                                        VIPI = 0,
                                        VIPIDevol = 0,
                                        VPIS = 0,
                                        VCOFINS = 0,
                                        VOutro = 0,
                                        VNF = 169.80,
                                        VTotTrib = 25.26
                                    }
                                },
                                Transp = new Unimake.Business.DFe.Xml.NFe.Transp
                                {
                                    ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                    Vol = new List<Unimake.Business.DFe.Xml.NFe.Vol>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.Vol
                                        {
                                            QVol = 1,
                                            Esp = "LU",
                                            Marca = "UNIMAKE",
                                            PesoL = 0.000,
                                            PesoB = 0.000
                                        }
                                    }
                                },
                                Cobr = new Unimake.Business.DFe.Xml.NFe.Cobr()
                                {
                                    Fat = new Unimake.Business.DFe.Xml.NFe.Fat
                                    {
                                        NFat = "057910",
                                        VOrig = 169.80,
                                        VDesc = 0,
                                        VLiq = 169.80
                                    },
                                    Dup = new List<Unimake.Business.DFe.Xml.NFe.Dup>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.Dup
                                        {
                                            NDup = "001",
                                            DVenc = DateTime.Now,
                                            VDup = 169.80
                                        }
                                    }
                                },
                                Pag = new Unimake.Business.DFe.Xml.NFe.Pag
                                {
                                    DetPag = new List<Unimake.Business.DFe.Xml.NFe.DetPag>
                                    {
                                        new Unimake.Business.DFe.Xml.NFe.DetPag
                                        {
                                            IndPag = IndicadorPagamento.PagamentoVista,
                                            TPag = MeioPagamento.Dinheiro,
                                            VPag = 169.80
                                        }
                                    }
                                },
                                InfAdic = new Unimake.Business.DFe.Xml.NFe.InfAdic
                                {
                                    InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                },
                                InfRespTec = new Unimake.Business.DFe.Xml.NFe.InfRespTec
                                {
                                    CNPJ = "06117473000150",
                                    XContato = "Wandrey Mundin Ferreira",
                                    Email = "wandrey@unimake.com.br",
                                    Fone = "04431414900"
                                }
                            }
                        }
                    }
                }
            };

ok          var autorizacao = new ServicoNFe.Autorizacao(xml, configuracao);
ok          autorizacao.Executar();

            var configSit = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = CertificadoSelecionado
            };

            if (autorizacao.Result != null)
            {
                if (autorizacao.Result.CStat == 103) //103 = Lote Recebido com Sucesso
                {
                    #region Finalizar através da consulta do recibo.

                    var xmlRec = new Unimake.Business.DFe.Xml.NFe.ConsReciNFe
                    {
                        Versao = "4.00",
                        TpAmb = TipoAmbiente.Homologacao,
                        NRec = autorizacao.Result.InfRec.NRec
                    };

                    var configRec = new Configuracao
                    {
                        TipoDFe = TipoDFe.NFe,
                        CertificadoDigital = CertificadoSelecionado
                    };

                    var retAutorizacao = new ServicoNFe.RetAutorizacao(xmlRec, configRec);
                    retAutorizacao.Executar();

                    autorizacao.RetConsReciNFe = retAutorizacao.Result;

                    autorizacao.GravarXmlDistribuicao(@"c:\testenfe");

                    #endregion Finalizar através da consulta do recibo.
                }
            }

*/            

/*   
   * Consumir o serviço
     autorizacao = CreateObject("Unimake.Business.DFe.Servicos.NFe.Autorizacao")
    ? autorizacao:GetConteudoXMLAssinado()

  autorizacao:Executar(xml,InicializarConfiguracao)
   ? "XML Retornado pela SEFAZ"
   ? "========================"
   ? retAutorizacao:RetornoWSString
   ?
   ? "Codigo de Status e Motivo"
   ? "========================="
   ? AllTrim(Str(retAutorizacao:Result:CStat,5)),retAutorizacao:Result:XMotivo
*/
 * ---------------------------------
 * Recuperando informações do objeto
 * ---------------------------------
   ? "resgatando conteudos da tag xml"
   ? oxml:versao
   ? oxml:IdLote
   ?

   ? "resgatando conteudos da tag Nfe"
   oTagNfe = oxml:GetNfe(0)
   ? oTagNfe:GetInfNfeCount
   ?

   ? "resgatando conteudos da tag InfNfe e suas filhas"
   oTagInfNfe = oTagNfe:GetInfNfe(0)
   ? oTagInfNfe:Ide:VerProc
   ? oTagInfNfe:Ide:cUf
   ? oTagInfNfe:Ide:DhEmi
   ? oTagInfNfe:Emit:XNome
   ? oTagInfNfe:Emit:EnderEmit:XLgr
   ? oTagInfNfe:Dest:XNome
   ? oTagInfNfe:Dest:EnderDest:XLgr
   ?

   ? "resgatando conteudo da tag Det e suas filhas"
   For I = 1 To oTagInfNfe:GetDetCount() // loop com todos os produtos
       oTagDet = oTagInfNfe:GetDet(I-1) 
       // dados do item
       ? oTagDet:NItem, oTagDet:Prod:CProd, oTagDet:Prod:xProd
       // dados do imposto do item
       ? Space(2),oTagDet:Imposto:VTotTrib

       // pega o primeiro ICMS definido (na prática nunca tem mais que um)
       oTagIcms = oTagDet:Imposto:GetIcms(0) 

       // mostra dados do ICMSSN101 se existir
       If oTagIcms:ICMSSN101 <> NIL
          ?? "",oTagIcms:ICMSSN101:Orig
          ?? "",oTagIcms:ICMSSN101:PCredSN
          ?? "",oTagIcms:ICMSSN101:VCredICMSSN
       EndIf

       // mostra dados do ICMSSN102 se existir
       If oTagIcms:ICMSSN102 <> NIL
          ?? "",oTagIcms:ICMSSN102:Orig
       EndIf

       // dados do PIS
       ?? "",oTagDet:Imposto:Pis:PISOutr:CST
       ?? "",oTagDet:Imposto:Pis:PISOutr:VBC
       ?? "",oTagDet:Imposto:Pis:PISOutr:PPIS
       ?? "",oTagDet:Imposto:Pis:PISOutr:VPIS
    Next I    
   ?
   Wait
Return

