using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCom;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFCom;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    /// <summary>
    /// Testasr o serviço de envio da NFCom
    /// </summary>
    public class AutorizacaoSincTest
    {
        /// <summary>
        /// Enviar uma NFCom no modo síncrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a NFCom</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a NFCom</param>
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void EnviarNFComSincronoXml(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var arqXML = "..\\..\\..\\NFCom\\Resources\\"+ "nfcom.xml";

            //Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NFCom.NFCom>(conteudoXML.OuterXml);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                CodigoUF = (int)ufBrasil,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSincNFCom = new AutorizacaoSinc(xml, configuracao);
            autorizacaoSincNFCom.Executar();
        }

        /// <summary>
        /// Enviar uma NFCom no modo síncrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a NFCom</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a NFCom</param>
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void EnviarNFComSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var conteudoXML = MontarXMLNFCom(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCom,
                CodigoUF = (int)ufBrasil,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSincNFCom = new AutorizacaoSinc(conteudoXML, configuracao);
            autorizacaoSincNFCom.Executar();
        }

        private Business.DFe.Xml.NFCom.NFCom MontarXMLNFCom(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new Business.DFe.Xml.NFCom.NFCom
            {
                InfNFCom = new InfNFCom
                {
                    Versao = "1.00",
                    Ide = new Ide
                    {
                        CUF = ufBrasil,
                        TpAmb = tipoAmbiente,
                        Mod = ModeloDFe.NFCom,
                        Serie = 1,
                        NNF = 123,
                        CNF = "1234567",
                        DhEmi = System.DateTime.Now,
                        TpEmis = TipoEmissao.Normal,
                        NSiteAutoriz = "0",
                        CMunFG = "1234567",
                        FinNFCom = FinalidadeNFCom.Normal,
                        TpFat = TipoFaturamentoNFCom.FaturamentoNormal,
                        VerProc = "teste 1.0",
                        IndPrePago = IndicadorServicoPrePago.ServicoPrePago,
                        IndCessaoMeiosRede = IndicadorCessaoMeiosDeRede.IndicadorCessaoMeioDeRede,
                        IndNotaEntrada = IndicadorNotaEntrada.IndicaNotaEntradaAjuste
                    },
                    Emit = new Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "12345678",
                        CRT = CRT.SimplesNacional,
                        XNome = "Unimake Solucoes Corporativas",
                        XFant = "Unimake Software",
                        EnderEmit = new EnderEmit
                        {
                            XLgr = "Rua",
                            Nro = "11",
                            XCpl = "Fundos",
                            XBairro = "Vila Maria",
                            CMun = "1234567",
                            XMun = "Paranavai",
                            CEP = "12345678",
                            UF = ufBrasil,
                            Fone = "12345678",
                            Email = "teste@test.com"
                        }
                    },
                    Dest = new Dest
                    {
                        XNome = "Unifake Software",
                        CNPJ = "06117473000150",
                        IndIEDest = IndicadorIEDestinatario.ContribuinteICMS,
                        IE = "12345678",
                        EnderDest = new EnderDest
                        {
                            XLgr = "Rua",
                            Nro = "11",
                            XCpl = "Fundos",
                            XBairro = "Vila Maria",
                            CMun = "1234567",
                            XMun = "Paranavai",
                            CEP = "12345678",
                            UF = ufBrasil,
                            CPais = "1058",
                            XPais = "BRASIL",
                            Fone = "12345678",
                            Email = "teste@test.com"
                        }
                    },
                    Assinante = new Assinante
                    {
                        ICodAssinante = "1",
                        TpAssinante = TipoAssinante.ProdutorRural,
                        TpServUtil = TipoServicoUtilizado.Outros,
                        NContrato = "123",
                        DContratoIni = System.DateTime.Today,
                        DContratoFim = System.DateTime.Today,
                        NroTermPrinc = "1234567",
                        CUFPrinc = ufBrasil
                    },
                    GSub = new GSub
                    {
                        ChNFCom = "12345678901234567890123456789012345678901234",
                        MotSub = MotivoSubstituicaoNFCom.DecisaoJudicial
                    },
                    GCofat = new GCofat
                    {
                        ChNFComLocal = "12345678901234567890123456789012345678901234"
                    },
                    Det = new System.Collections.Generic.List<Det>
                    {
                        new Det
                        {
                            NItem = "1",
                            Prod = new Prod
                            {
                                CProd = "1",
                                XProd = "teste",
                                CClass = "1234567",
                                CFOP = "5120",
                                CNPJLD = "06117473000150",
                                UMed = UnidadeBasicaMedida.UN,
                                QFaturada = 1.12,
                                VItem = 1.123M,
                                VDesc = 1.11,
                                VOutro = 1.11,
                                VProd = 1234.10M,
                                DExpiracao = System.DateTime.Today,
                                IndDevolucao = IndicadorDevolucao.DevolucaoValorItem
                            },
                            Imposto = new Imposto
                            {
                                ICMS00 = new ICMS00NFCom
                                {
                                    CST = "00",
                                    VBC = 1.11,
                                    PICMS = 1.15,
                                    VICMS = 1.17,
                                    PFCP = 1.190,
                                    VFCP = 111.47
                                },
                                ICMSUFDest = new System.Collections.Generic.List<ICMSUFDest>
                                {
                                    new ICMSUFDest
                                    {
                                        CUFDest = ufBrasil,
                                        VBCUFDest = 158.55,
                                        PFCPUFDest = 158.55,
                                        PICMSUFDest = 158.55,
                                        VFCPUFDest = 158.55,
                                        VICMSUFDest = 158.55,
                                        VICMSUFEmi = 158.55,
                                        CBenefUFDest = "11"
                                    }
                                },
                                PIS = new PISNFCom
                                {
                                    CST = CSTPisCofins.OperacaoComSuspensao,
                                    VBC = 1587.45,
                                    PPIS = 123.4500,
                                    VPIS = 1587.45
                                },
                                COFINS = new COFINSNFCom
                                {
                                    CST = CSTPisCofins.AliquotaBasica,
                                    VBC = 11.98,
                                    PCOFINS = 11.9800,
                                    VCOFINS = 11.98
                                },
                                FUST = new FUST
                                {
                                    VBC = 1879.88,
                                    PFUST = 132.88,
                                    VFUST = 1879.88
                                },
                                FUNTTEL = new FUNTTEL
                                {
                                    VBC = 1.47,
                                    PFUNTTEL = 1.4700,
                                    VFUNTTEL = 1.47
                                },
                                RetTribNFCom = new RetTribNFCom
                                {
                                    VRetPIS = 1444.85M,
                                    VRetCofins = 1444.85M,
                                    VRetCSLL = 1444.85M,
                                    VBCIRRF = 1444.85M,
                                    VIRRF = 1444.85M
                                }
                            },
                            GProcRef = new GProcRef
                            {
                                VItem = 123.48M,
                                QFaturada = 123.4800,
                                VProd = 123.48M,
                                VDesc = 123.48,
                                VOutro = 123.48,
                                IndDevolucao = IndicadorDevolucao.DevolucaoValorItem,
                                VBC = 123.48,
                                PICMS = 123.48,
                                VICMS = 123.48,
                                VPIS = 123.48,
                                VCOFINS = 123.48,
                                VFCP = 123.48,
                                GProc = new System.Collections.Generic.List<GProc>
                                {
                                    new GProc
                                    {
                                        TpProc = TipoProcessoNF3eNFCom.JusticaFederal,
                                        NProcesso = "12345678"
                                    }
                                }
                            },
                            GRessarc = new GRessarc
                            {
                                TpRessarc = TipoRessarcimento.CobrancaIndevida,
                                DRef = System.DateTime.Today,
                                NProcesso = "11222",
                                NProtReclama = "1111",
                                XObs = "Teste total da NFCom"
                            }
                        }
                    },
                    Total = new Total
                    {
                        VProd = 111.54,
                        ICMSTot = new ICMSTot
                        {
                            VBC = 111.54,
                            VICMS = 111.54,
                            VICMSDeson = 111.54,
                            VFCP = 111.54
                        },
                        VCOFINS = 111.54, 
                        VPIS = 111.54,
                        VFUNTTEL = 111.54,
                        VFUST = 111.54,
                        VRetTribTot = new VRetTribTotNFCom
                        {
                            VRetPIS = 111.54,
                            VRetCofins = 111.54,
                            VRetCSLL = 111.54,
                            VIRRF = 111.54
                        },
                        VDesc = 111.54,
                        VOutro = 111.54,
                        VNF = 111.54
                    },
                    AutXML = new System.Collections.Generic.List<AutXMLNFCom>
                    {
                        new AutXMLNFCom
                        {
                            CNPJ = "06117473000150"
                        }
                    },
                    InfAdic = new InfAdicNFCom
                    {
                        InfAdFisco = "teste total da NFCom",
                        InfCpl = new System.Collections.Generic.List<string>
                        {
                            "Informacao 1",
                            "Informacao 2"
                        }
                    },
                    GRespTec = new GRespTecNFCom
                    {
                        CNPJ = "06117473000150",
                        XContato = "Fulano de tal",
                        Email = "email_fulando@gmail.com",
                        Fone = "12345678",
                        IdCSRT = "123",
                        HashCSRT = "oBbYbxIbKXRZhoJ2zEzYy458+YU="
                    }
                }
            };

            return xml;
        }
    }
}
