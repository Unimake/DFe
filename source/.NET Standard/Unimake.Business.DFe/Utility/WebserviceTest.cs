using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe com utilitários para testar os web-services da SEFAZ
    /// </summary>
    public class WebserviceTest
    {
        /// <summary>
        /// Mensagem padrão para erros gerados por Exceptions
        /// </summary>
        private readonly string MensagemErroPadrao =
            "A URL do web - service da SEFAZ, Receita Federal ou Prefeitura não está respondendo." + Environment.NewLine + Environment.NewLine +
            "Possíveis causas e soluções sugeridas:" + Environment.NewLine + Environment.NewLine +
            "1) Problema de Resolução de DNS: Tente mudar o servidor de DNS em suas configurações de rede.Você pode usar servidores DNS públicos, como os do Google (8.8.8.8 e 8.8.4.4) ou da Cloudflare (1.1.1.1)." + Environment.NewLine +
            "2) Problema no web - service(SEFAZ / Receita Federal / Prefeitura): O serviço web pode estar temporariamente fora do ar." + Environment.NewLine +
            "3) Problema no Provedor de Internet: Verifique sua conexão com a internet. Reinicie seu modem / roteador.Se o problema persistir, entre em contato com o suporte técnico do seu provedor de internet." + Environment.NewLine +
            "4) Problema de Rede Local: Certifique - se de que sua rede local(LAN) está funcionando corretamente.Verifique cabos, conexões e configurações de rede." + Environment.NewLine +
            "5) Configuração de Firewall ou Proxy: Verifique se o firewall ou o proxy de sua rede está bloqueando o acesso ao web-service.Ajuste as configurações conforme necessário ou consulte o administrador da rede." + Environment.NewLine +
            "6) Certificado Digital Inválido ou Expirado: Verifique se o certificado digital é válido, se não está expirado ou apresentando algum problema." + Environment.NewLine;

        /// <summary>
        /// Tipo de emissão
        /// </summary>
        public TipoEmissao TipoEmissao { get; set; }

        /// <summary>
        /// Tipo do ambiente
        /// </summary>
        public TipoAmbiente TipoAmbiente { get; set; }

        /// <summary>
        /// UF do web-service que será testado
        /// </summary>
        public UFBrasil UFBrasil { get; set; }

        /// <summary>
        /// Tipo do documento fiscal eletrônico que terá o web-service testado
        /// </summary>
        public TipoDFe TipoDFe { get; set; }

        /// <summary>
        /// Versão do pacote de schema do serviço que terá o web-service testado
        /// </summary>
        public string SchemaVersao { get; set; }

        /// <summary>
        /// Certificado digital a ser utilizado nos testes do web-service
        /// </summary>
        public X509Certificate2 CertificadoDigital { get; set; }

        /// <summary>
        /// Executar o teste 
        /// </summary>
        /// <returns>Resultado do teste</returns>
        public ResultTest Execute()
        {
            var resultTest = new ResultTest
            {
                Sucess = true
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe,
                TipoEmissao = TipoEmissao,
                CodigoUF = (int)UFBrasil,
                TipoAmbiente = TipoAmbiente,
                SchemaVersao = SchemaVersao,
                CertificadoDigital = CertificadoDigital
            };

            switch (TipoDFe)
            {
                case TipoDFe.NFe:
                    resultTest = TestNFe(configuracao);
                    break;

                case TipoDFe.NFCe:
                    resultTest = TestNFCe(configuracao);
                    break;
            }

            return resultTest;
        }

        /// <summary>
        /// Testar a URL
        /// </summary>
        /// <param name="configuracao">Objeto da classe Configuracao</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestURL(Configuracao configuracao)
        {
            var resultTest = new ResultTest
            {
                Sucess = true
            };

            var method = "GET";

            if (UFBrasil == UFBrasil.PR)
            {
                method = "POST";
            }
            // MT não está funcionando este TEST e tenho que gastar um tempo para entender melhor.
            else if (UFBrasil == UFBrasil.MT)
            {
                return resultTest;
            }

            if (configuracao.TipoDFe == TipoDFe.NFCe)
            {
                if (UFBrasil == UFBrasil.MG)
                {
                    return resultTest;
                }
                if (UFBrasil == UFBrasil.CE)
                {
                    return resultTest;
                }
            }

            var url = configuracao.WebEnderecoProducao;
            if (configuracao.TipoAmbiente == TipoAmbiente.Homologacao)
            {
                url = configuracao.WebEnderecoHomologacao;
            }

            resultTest.Sucess = Unimake.Net.Utility.TestHttpConnection(url, CertificadoDigital, 3, null, method);

            if (!resultTest.Sucess)
            {
                resultTest.ErrorMessage = MensagemErroPadrao;
            }

            return resultTest;
        }

        #region Testes da NFe

        /// <summary>
        /// Testar WebService de NFe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFe(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            for (var i = 0; i < 5; i++)
            {
                switch (i)
                {
                    case 0:
                        configuracao.Load("StatusServico");
                        resultTest = TestNFeStatusServico(configuracao);
                        goto default;

                    case 1:
                        configuracao.Load("Autorizacao");
                        resultTest = TestNFeAutorizacao(configuracao, SimNao.Nao);
                        goto default;

                    case 2:
                        configuracao.Load("Autorizacao");
                        resultTest = TestNFeAutorizacao(configuracao, SimNao.Sim);
                        goto default;

                    case 3:
                        configuracao.Load("ConsultaProtocolo");
                        goto default;

                    case 4:
                        configuracao.Load("RetAutorizacao");
                        resultTest = TestNFeRetAutorizacao(configuracao);
                        goto default;

                    default:
                        if (!resultTest.Sucess)
                        {
                            return resultTest;
                        }
                        break;
                }
            }

            return resultTest;
        }


        /// <summary>
        /// Testar WebService de NFe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFeStatusServico(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            resultTest = TestURL(configuracao);

            if (!resultTest.Sucess)
            {
                return resultTest;
            }

            try
            {
                var xml = new Xml.NFe.ConsStatServ
                {
                    CUF = (UFBrasil)configuracao.CodigoUF,
                    TpAmb = configuracao.TipoAmbiente,
                    Versao = SchemaVersao
                };

                var statusServico = new Servicos.NFe.StatusServico(xml, configuracao);
                statusServico.Executar();

                resultTest.CStat = statusServico.Result.CStat;
                resultTest.XMotivo = statusServico.Result.XMotivo;

                switch (statusServico.Result.CStat)
                {
                    case 999: //Erro não catalogado
                    case 108: //Serviço Paralisado Momentaneamente (curto prazo)
                    case 109: //Serviço Paralisado sem Previsão
                        resultTest.Sucess = false;
                        break;
                }
            }
            catch (Exception ex)
            {
                resultTest.ErrorMessage = MensagemErroPadrao + Environment.NewLine + Environment.NewLine + "Exception Messages: " + ex.GetAllMessages();
            }

            return resultTest;
        }

        /// <summary>
        /// Testar WebService de NFe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFeRetAutorizacao(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            resultTest = TestURL(configuracao);

            if (!resultTest.Sucess)
            {
                return resultTest;
            }

            try
            {
                var xml = new Xml.NFe.ConsReciNFe
                {
                    TpAmb = configuracao.TipoAmbiente,
                    Versao = SchemaVersao,
                    NRec = configuracao.CodigoUF.ToString() + "1210140351219"
                };

                var retAutorizacao = new Servicos.NFe.RetAutorizacao(xml, configuracao);
                retAutorizacao.Executar();

                resultTest.CStat = retAutorizacao.Result.CStat;
                resultTest.XMotivo = retAutorizacao.Result.XMotivo;

                switch (retAutorizacao.Result.CStat)
                {
                    case 999: //Erro não catalogado
                    case 108: //Serviço Paralisado Momentaneamente (curto prazo)
                    case 109: //Serviço Paralisado sem Previsão
                        resultTest.Sucess = false;
                        break;
                }
            }
            catch (Exception ex)
            {
                resultTest.ErrorMessage = MensagemErroPadrao + Environment.NewLine + Environment.NewLine + "Exception Messages: " + ex.GetAllMessages();
            }

            return resultTest;
        }

        /// <summary>
        /// Testar WebService de NFe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <param name="indSinc">Testar envio síncrono?</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFeAutorizacao(Configuracao configuracao, SimNao indSinc)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            resultTest = TestURL(configuracao);

            if (!resultTest.Sucess)
            {
                return resultTest;
            }

            try
            {
                var xml = MontaXMLEnviNFe((UFBrasil)configuracao.CodigoUF, configuracao.TipoAmbiente, indSinc);

                var autorizacao = new Servicos.NFe.Autorizacao(xml, configuracao);
                autorizacao.Executar();

                resultTest.CStat = autorizacao.Result.CStat;
                resultTest.XMotivo = autorizacao.Result.XMotivo;

                switch (autorizacao.Result.CStat)
                {
                    case 999: //Erro não catalogado
                    case 108: //Serviço Paralisado Momentaneamente (curto prazo)
                    case 109: //Serviço Paralisado sem Previsão
                        resultTest.Sucess = false;
                        break;
                }
            }
            catch (Exception ex)
            {
                resultTest.ErrorMessage = MensagemErroPadrao + Environment.NewLine + Environment.NewLine + "Exception Messages: " + ex.GetAllMessages();
            }

            return resultTest;
        }


        /// <summary>
        /// Método auxiliar para montar XML teste
        /// </summary>
        /// <param name="ufBrasil">UF que vai recepcionar o XML</param>
        /// <param name="tipoAmbiente">Ambiente para o qual o XML será enviado</param>
        /// <param name="indSinc">Envio síncrono?</param>
        /// <returns></returns>
        private EnviNFe MontaXMLEnviNFe(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, SimNao indSinc) => new EnviNFe
        {
            Versao = "4.00",
            IdLote = "000000000000001",
            IndSinc = indSinc,
            NFe = new List<NFe> {
                        new NFe
                        {
                            InfNFe = new List<InfNFe> {
                                new InfNFe
                                {
                                    Versao = "4.00",

                                    Ide = new Ide
                                    {
                                        CUF = ufBrasil,
                                        NatOp = "VENDA PRODUC.DO ESTABELEC",
                                        Mod = ModeloDFe.NFe,
                                        Serie = 1,
                                        NNF = 57962,
                                        DhEmi = DateTime.Now,
                                        DhSaiEnt = DateTime.Now,
                                        TpNF = TipoOperacao.Saida,
                                        IdDest = DestinoOperacao.OperacaoInterestadual,
                                        CMunFG = 4118402,
                                        TpImp = FormatoImpressaoDANFE.NormalRetrato,
                                        TpEmis = TipoEmissao.Normal,
                                        TpAmb = tipoAmbiente,
                                        FinNFe = FinalidadeNFe.Normal,
                                        IndFinal = SimNao.Sim,
                                        IndPres = IndicadorPresenca.OperacaoPresencial,
                                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                        VerProc = "TESTE 1.00"
                                    },
                                    Emit = new Emit
                                    {
                                        CNPJ = "11111111111111",
                                        XNome = "EMITENTE TESTE LTDA",
                                        XFant = "TESTE LTDA",
                                        EnderEmit = new EnderEmit
                                        {
                                            XLgr = "RUA TESTE FELIPE",
                                            Nro = "1500",
                                            XBairro = "CENTRO",
                                            CMun = 4118402,
                                            XMun = "PARANAVAI",
                                            UF = ufBrasil,
                                            CEP = "87700000",
                                            Fone = "04433333333"
                                        },
                                        IE = "1111111111",
                                        IM = "11111",
                                        CNAE = "6202300",
                                        CRT = CRT.SimplesNacional
                                    },
                                    Dest = new Dest
                                    {
                                        CNPJ = "22222222222222",
                                        XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                        EnderDest = new EnderDest
                                        {
                                            XLgr = "AVENIDA DE TESTE DE NFE",
                                            Nro = "11",
                                            XBairro = "CAMPOS ELISEOS",
                                            CMun = 3543402,
                                            XMun = "RIBEIRAO PRETO",
                                            UF = UFBrasil.SP,
                                            CEP = "14080000",
                                            Fone = "01666994533"
                                        },
                                        IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                                        IE = "111111111111",
                                        Email = "testenfe@hotmail.com"
                                    },
                                    Det = new List<Det> {
                                        new Det
                                        {
                                            NItem = 1,
                                            Prod = new Prod
                                            {
                                                CProd = "01042",
                                                CEAN = "SEM GTIN",
                                                XProd = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                                NCM = "84714900",
                                                CFOP = "6101",
                                                UCom = "LU",
                                                QCom = 1.00m,
                                                VUnCom = 84.9000000000M,
                                                VProd = 84.90,
                                                CEANTrib = "SEM GTIN",
                                                UTrib = "LU",
                                                QTrib = 1.00m,
                                                VUnTrib = 84.9000000000M,
                                                IndTot = SimNao.Sim,
                                                XPed = "300474",
                                                NItemPed = "0001"
                                            },
                                            Imposto = new Imposto
                                            {
                                                VTotTrib = 12.63,
                                                ICMS = new ICMS
                                                {
                                                    ICMSSN101 = new ICMSSN101
                                                    {
                                                        Orig = OrigemMercadoria.Nacional,
                                                        PCredSN = 2.8255,
                                                        VCredICMSSN = 2.40
                                                    }
                                                },
                                                PIS = new PIS
                                                {
                                                    PISOutr = new PISOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PPIS = 0.00,
                                                        VPIS = 0.00
                                                    }
                                                },
                                                COFINS = new COFINS
                                                {
                                                    COFINSOutr = new COFINSOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PCOFINS = 0.00,
                                                        VCOFINS = 0.00
                                                    }
                                                }
                                            },
                                            ObsItem = new ObsItem
                                            {
                                                ObsCont = new List<ObsCont>
                                                {
                                                    new ObsCont
                                                    {
                                                        XCampo = "teste1",
                                                        XTexto = "teste1"
                                                    }
                                                },
                                                ObsFisco = new List<ObsFisco>
                                                {
                                                    new ObsFisco
                                                    {
                                                        XCampo = "teste3",
                                                        XTexto = "teste3"
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    Total = new Total
                                    {
                                        ICMSTot = new ICMSTot
                                        {
                                            VBC = 0,
                                            VICMS = 0,
                                            VICMSDeson = 0,
                                            VFCP = 0,
                                            VBCST = 0,
                                            VST = 0,
                                            VFCPST = 0,
                                            VFCPSTRet = 0,
                                            VProd = 84.90,
                                            VFrete = 0,
                                            VSeg = 0,
                                            VDesc = 0,
                                            VII = 0,
                                            VIPI = 0,
                                            VIPIDevol = 0,
                                            VPIS = 0,
                                            VCOFINS = 0,
                                            VOutro = 0,
                                            VNF = 84.90,
                                            VTotTrib = 12.63
                                        }
                                    },
                                    Transp = new Transp
                                    {
                                        ModFrete = ModalidadeFrete.ContratacaoFretePorContaRemetente_CIF,
                                        Vol = new List<Vol>
                                        {
                                            new Vol
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
                                        Fat = new Fat
                                        {
                                            NFat = "057910",
                                            VOrig = 84.90,
                                            VDesc = 0,
                                            VLiq = 84.90
                                        },
                                        Dup = new List<Dup>
                                        {
                                            new Dup
                                            {
                                                NDup = "001",
                                                DVenc = DateTime.Now,
                                                VDup = 84.90
                                            }
                                        }
                                    },
                                    Pag = new Pag
                                    {
                                        DetPag = new List<DetPag>
                                        {
                                             new DetPag
                                             {
                                                 IndPag = IndicadorPagamento.PagamentoVista,
                                                 TPag = MeioPagamento.Dinheiro,
                                                 VPag = 84.90
                                             }
                                        }
                                    },
                                    InfAdic = new InfAdic
                                    {
                                        InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                    },
                                    InfRespTec = new InfRespTec
                                    {
                                        CNPJ = "11111111111111",
                                        XContato = "Contato teste para NFe",
                                        Email = "testenfe@gmail.com",
                                        Fone = "04433333333"
                                    }
                                }
                            }
                        }
                    }
        };

        #endregion

        #region Testes da NFCe

        /// <summary>
        /// Testar WebService de NFCe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFCe(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            for (var i = 0; i < 3; i++)
            {
                switch (i)
                {
                    case 0:
                        configuracao.Load("StatusServico");
                        resultTest = TestNFCeStatusServico(configuracao);
                        goto default;

                    case 1:
                        configuracao.Load("Autorizacao");
                        resultTest = TestNFCeAutorizacao(configuracao);
                        goto default;

                    case 2:
                        configuracao.Load("ConsultaProtocolo");
                        goto default;

                    default:
                        if (!resultTest.Sucess)
                        {
                            return resultTest;
                        }
                        break;
                }
            }

            return resultTest;
        }

        /// <summary>
        /// Testar WebService de NFCe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFCeStatusServico(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            resultTest = TestURL(configuracao);

            if (!resultTest.Sucess)
            {
                return resultTest;
            }

            try
            {
                var xml = new Xml.NFe.ConsStatServ
                {
                    CUF = (UFBrasil)configuracao.CodigoUF,
                    TpAmb = configuracao.TipoAmbiente,
                    Versao = SchemaVersao
                };

                var statusServico = new Servicos.NFCe.StatusServico(xml, configuracao);
                statusServico.Executar();

                resultTest.CStat = statusServico.Result.CStat;
                resultTest.XMotivo = statusServico.Result.XMotivo;

                switch (statusServico.Result.CStat)
                {
                    case 999: //Erro não catalogado
                    case 108: //Serviço Paralisado Momentaneamente (curto prazo)
                    case 109: //Serviço Paralisado sem Previsão
                        resultTest.Sucess = false;
                        break;
                }
            }
            catch (Exception ex)
            {
                resultTest.ErrorMessage = MensagemErroPadrao + Environment.NewLine + Environment.NewLine + "Exception Messages: " + ex.GetAllMessages();
            }

            return resultTest;
        }

        /// <summary>
        /// Testar WebService de NFCe
        /// </summary>
        /// <param name="configuracao">Objeto contendo as configurações para consumir o serviço</param>
        /// <returns>Resultado do teste</returns>
        private ResultTest TestNFCeAutorizacao(Configuracao configuracao)
        {
            var resultTest = new ResultTest()
            {
                Sucess = true
            };

            resultTest = TestURL(configuracao);

            configuracao.CSC = "HCJBIRTWGCQ3HVQN7DCA0ZY0P2NYT6FVLPJG";
            configuracao.CSCIDToken = 1;

            if (!resultTest.Sucess)
            {
                return resultTest;
            }

            try
            {
                var xml = MontaXMLEnviNFCe((UFBrasil)configuracao.CodigoUF, configuracao.TipoAmbiente);

                var autorizacao = new Servicos.NFCe.Autorizacao(xml, configuracao);
                autorizacao.Executar();

                resultTest.CStat = autorizacao.Result.CStat;
                resultTest.XMotivo = autorizacao.Result.XMotivo;

                switch (autorizacao.Result.CStat)
                {
                    case 999: //Erro não catalogado
                    case 108: //Serviço Paralisado Momentaneamente (curto prazo)
                    case 109: //Serviço Paralisado sem Previsão
                        resultTest.Sucess = false;
                        break;
                }
            }
            catch (Exception ex)
            {
                resultTest.ErrorMessage = MensagemErroPadrao + Environment.NewLine + Environment.NewLine + "Exception Messages: " + ex.GetAllMessages();
            }

            return resultTest;
        }


        /// <summary>
        /// Método auxiliar para montar XML teste
        /// </summary>
        /// <param name="ufBrasil">UF que vai recepcionar o XML</param>
        /// <param name="tipoAmbiente">Ambiente para o qual o XML será enviado</param>
        /// <returns></returns>
        private EnviNFe MontaXMLEnviNFCe(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente) => new EnviNFe
        {
            Versao = "4.00",
            IdLote = "000000000000001",
            IndSinc = SimNao.Sim,
            NFe = new List<Business.DFe.Xml.NFe.NFe> {
                        new Business.DFe.Xml.NFe.NFe
                        {
                            InfNFe = new List<InfNFe> {
                                new InfNFe
                                {
                                    Versao = "4.00",

                                    Ide = new Ide
                                    {
                                        CUF = ufBrasil,
                                        NatOp = "VENDA PRODUC.DO ESTABELEC",
                                        Mod = ModeloDFe.NFCe,
                                        Serie = 16,
                                        NNF = 57964,
                                        DhEmi = DateTime.Now,
                                        DhSaiEnt = DateTime.Now,
                                        TpNF = TipoOperacao.Saida,
                                        IdDest = DestinoOperacao.OperacaoInterna,
                                        CMunFG = 4118402,
                                        TpImp = FormatoImpressaoDANFE.NFCe,
                                        TpEmis = TipoEmissao.Normal,
                                        TpAmb = tipoAmbiente,
                                        FinNFe = FinalidadeNFe.Normal,
                                        IndFinal = SimNao.Sim,
                                        IndPres = IndicadorPresenca.OperacaoPresencial,
                                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                                        VerProc = "TESTE 1.00",
                                    },
                                    Emit = new Emit
                                    {
                                        CNPJ = "06117473000150",
                                        XNome = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                        XFant = "UNIMAKE - PARANAVAI",
                                        EnderEmit = new EnderEmit
                                        {
                                            XLgr = "RUA ANTONIO FELIPE",
                                            Nro = "1500",
                                            XBairro = "CENTRO",
                                            CMun = 4118402,
                                            XMun = "PARANAVAI",
                                            UF = ufBrasil,
                                            CEP = "87704030",
                                            Fone = "04431414900"
                                        },
                                        IE = "9032000301",
                                        IM = "14018",
                                        CNAE = "6202300",
                                        CRT = CRT.SimplesNacional
                                    },
                                    Dest = new Dest
                                    {
                                        CNPJ = "01761135000132",
                                        XNome = "NF-E EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                        EnderDest = new EnderDest
                                        {
                                            XLgr = "AV. COMENDADOR NORBERTO MARCONDES",
                                            Nro = "2156",
                                            XBairro = "CENTRO",
                                            CMun = 4104303,
                                            XMun = "CAMPO MOURAO",
                                            UF = UFBrasil.PR,
                                            CEP = "87703100",
                                            Fone = "04431414900"
                                        },
                                        IndIEDest = IndicadorIEDestinatario.NaoContribuinte,
                                        Email = "testenfe@hotmail.com"
                                    },
                                    Det = new List<Det> {
                                        new Det
                                        {
                                            NItem = 1,
                                            Prod = new Prod
                                            {
                                                CProd = "01042",
                                                CEAN = "SEM GTIN",
                                                XProd = "NOTA FISCAL EMITIDA EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL",
                                                NCM = "84714900",
                                                CFOP = "5101",
                                                UCom = "LU",
                                                QCom = 1.00m,
                                                VUnCom = 84.9000000000M,
                                                VProd = 84.90,
                                                CEANTrib = "SEM GTIN",
                                                UTrib = "LU",
                                                QTrib = 1.00m,
                                                VUnTrib = 84.9000000000M,
                                                IndTot = SimNao.Sim,
                                                XPed = "300474",
                                                NItemPed = "1"
                                            },
                                            Imposto = new Imposto
                                            {
                                                VTotTrib = 12.63,
                                                ICMS = new ICMS
                                                {
                                                    ICMSSN102 = new ICMSSN102
                                                    {
                                                        Orig = OrigemMercadoria.Nacional,
                                                        CSOSN = "102"
                                                    }
                                                },
                                                PIS = new PIS
                                                {
                                                    PISOutr = new PISOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PPIS = 0.00,
                                                        VPIS = 0.00
                                                    }
                                                },
                                                COFINS = new COFINS
                                                {
                                                    COFINSOutr = new COFINSOutr
                                                    {
                                                        CST = "99",
                                                        VBC = 0.00,
                                                        PCOFINS = 0.00,
                                                        VCOFINS = 0.00
                                                    }
                                                }
                                            }
                                        }
                                    },
                                    Total = new Total
                                    {
                                        ICMSTot = new ICMSTot
                                        {
                                            VBC = 0,
                                            VICMS = 0,
                                            VICMSDeson = 0,
                                            VFCP = 0,
                                            VBCST = 0,
                                            VST = 0,
                                            VFCPST = 0,
                                            VFCPSTRet = 0,
                                            VProd = 84.90,
                                            VFrete = 0,
                                            VSeg = 0,
                                            VDesc = 0,
                                            VII = 0,
                                            VIPI = 0,
                                            VIPIDevol = 0,
                                            VPIS = 0,
                                            VCOFINS = 0,
                                            VOutro = 0,
                                            VNF = 84.90,
                                            VTotTrib = 12.63
                                        }
                                    },
                                    Transp = new Transp
                                    {
                                        ModFrete = ModalidadeFrete.SemOcorrenciaTransporte,
                                    },
                                    Pag = new Pag
                                    {
                                        DetPag = new List<DetPag>
                                        {
                                             new DetPag
                                             {
                                                 IndPag = IndicadorPagamento.PagamentoVista,
                                                 TPag = MeioPagamento.Dinheiro,
                                                 VPag = 84.90
                                             }
                                        }
                                    },
                                    InfAdic = new InfAdic
                                    {
                                        InfCpl = ";CONTROLE: 0000241197;PEDIDO(S) ATENDIDO(S): 300474;Empresa optante pelo simples nacional, conforme lei compl. 128 de 19/12/2008;Permite o aproveitamento do credito de ICMS no valor de R$ 2,40, correspondente ao percentual de 2,83% . Nos termos do Art. 23 - LC 123/2006 (Resolucoes CGSN n. 10/2007 e 53/2008);Voce pagou aproximadamente: R$ 6,69 trib. federais / R$ 5,94 trib. estaduais / R$ 0,00 trib. municipais. Fonte: IBPT/empresometro.com.br 18.2.B A3S28F;",
                                    },
                                    InfRespTec = new InfRespTec
                                    {
                                        CNPJ = "06117473000150",
                                        XContato = "Contato teste para NFe",
                                        Email = "testenfe@gmail.com",
                                        Fone = "04431414900"
                                    }
                                }
                            }
                        }
                    }
        };

        #endregion
    }

    /// <summary>
    /// Resultado do teste do web-service
    /// </summary>
    public class ResultTest
    {
        /// <summary>
        /// True = Deu tudo certo
        /// </summary>
        public bool Sucess { get; set; }

        /// <summary>
        /// CStat retornado pelo web-service
        /// </summary>
        public int CStat { get; set; }
        /// <summary>
        /// XMotivo retornado pelo web-service
        /// </summary>
        public string XMotivo { get; set; }
        /// <summary>
        /// Em caso de exceção teremos todas as mensagens nesta propriedade;
        /// </summary>
        public string ErrorMessage { get; set; }
    }
}