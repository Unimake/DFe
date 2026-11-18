using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using DFeNFeABI = Unimake.Business.DFe.Xml.NFeABI;
using Xunit;

namespace Unimake.DFe.Test.NFeABI.Serializacao
{
    public class SerializacaoNFeABITest
    {
        private const string NamespaceNFeABI = "http://www.portalfiscal.inf.br/nfeabi";

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void DesserializaFixtureOficialMinimaEPreservaConteudo()
        {
            var original = new XmlDocument();
            original.Load(Path.Combine(@"..\..\..\NFeABI\Resources", "NFeABI-minima.xml"));

            var objeto = XMLUtility.Deserializar<DFeNFeABI.NFeABI>(original.OuterXml);
            var roundTrip = objeto.GerarXML();

            Assert.Equal(original.InnerText, roundTrip.InnerText);
            Assert.Equal(ModeloDFe.NFeABI, objeto.InfNFeABI.Ide.Mod);
            Assert.Equal(2, (int)objeto.InfNFeABI.Ide.TpAmb);
            Assert.Single(objeto.InfNFeABI.Transmit);
            Assert.Single(objeto.InfNFeABI.Adquirente);
            Assert.NotNull(objeto.Signature);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SerializaModeloCompletoComGruposRepetiveisTributosPagamentosETotais()
        {
            var documento = CriarDocumentoCompleto();
            var xml = documento.GerarXML();
            var ns = new XmlNamespaceManager(xml.NameTable);
            ns.AddNamespace("n", NamespaceNFeABI);

            Assert.Equal(44, documento.InfNFeABI.Chave.Length);
            Assert.Equal("77", xml.SelectSingleNode("/n:NFeABI/n:infNFeABI/n:ide/n:mod", ns).InnerText);
            Assert.Equal(2, xml.SelectNodes("/n:NFeABI/n:infNFeABI/n:transmit", ns).Count);
            Assert.Equal(2, xml.SelectNodes("/n:NFeABI/n:infNFeABI/n:adquirente", ns).Count);
            Assert.Equal("250000.00", xml.SelectSingleNode("/n:NFeABI/n:infNFeABI/n:total/n:vNF", ns).InnerText);
            Assert.Equal("0.1000", xml.SelectSingleNode("/n:NFeABI/n:infNFeABI/n:gInfTrib/n:IBSCBS/n:gIBSCBS/n:gIBSUF/n:pIBSUF", ns).InnerText);
            Assert.NotNull(xml.SelectSingleNode("/n:NFeABI/n:infNFeABI/n:pag/n:detPagImovel", ns));
            Assert.NotNull(xml.SelectSingleNode("/n:NFeABI/n:infNFeABI/n:infAdic/n:obsCont", ns));

            var validador = new ValidarSchema();
            validador.Validar(xml, "NFeABI.NFeABI_v1.00.xsd", NamespaceNFeABI);
            Assert.True(validador.Success, validador.ErrorMessage);

            var roundTrip = XMLUtility.Deserializar<DFeNFeABI.NFeABI>(xml.OuterXml).GerarXML();
            Assert.Equal(xml.InnerText, roundTrip.InnerText);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SerializaPagamentoAntecipadoComDataTipadaENovosCadastros()
        {
            var documento = CriarDocumentoCompleto();
            documento.InfNFeABI.Ide.TpPagAnt = TipoPagamentoAntecipadoNFeABI.PagamentoAntecipado;
            documento.InfNFeABI.InfOper.GIncorpLote = new DFeNFeABI.GIncorpLote
            {
                TipoIncorpLote = IndicadorIncorporacaoLoteNFeABI.Loteamento,
                CNPJempImob = "12345678000123",
                CNO = "123456789012"
            };
            documento.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.GInfPagAntecip = new DFeNFeABI.GInfPagAntecip
            {
                VPagAntecip = 250000,
#if INTEROP
                DtPagAntecip = new DateTime(2026, 11, 18, 15, 30, 0),
#else
                DtPagAntecip = new DateTimeOffset(2026, 11, 18, 15, 30, 0, TimeSpan.FromHours(-3)),
#endif
                IndCNPJempImob = IndicadorSimNaoNFeABI.Sim,
                CNPJempImob = "12345678000123"
            };
            documento.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.VOperacIndiv = -1;
            documento.InfNFeABI.Total.VTotalOperacao = -1;
            documento.InfNFeABI.Total.VOperacIndiv = -1;

            var xml = documento.GerarXML();
            Assert.Contains("<dtPagAntecip>2026-11-18</dtPagAntecip>", xml.OuterXml);
            Assert.Contains("<indIncorpLote>2</indIncorpLote>", xml.OuterXml);
            Assert.Contains("<CNO>123456789012</CNO>", xml.OuterXml);
            Assert.Contains("<nIPTU>123</nIPTU>", xml.OuterXml);
            Assert.Contains("<indINCRA>1</indINCRA>", xml.OuterXml);
            Assert.Contains("<nINCRA>INCRA-123</nINCRA>", xml.OuterXml);
            Assert.Contains("<indRIP>1</indRIP>", xml.OuterXml);
            Assert.Contains("<nRIP>RIP-123</nRIP>", xml.OuterXml);
            Assert.DoesNotContain("<nIptu>", xml.OuterXml);
            Assert.DoesNotContain("<indIncra>", xml.OuterXml);
            Assert.DoesNotContain("<nIncra>", xml.OuterXml);
            Assert.DoesNotContain("<vOperacIndiv>", xml.OuterXml);
            AssertSchema(xml, "NFeABI.NFeABI_v1.00.xsd");

            var roundTrip = XMLUtility.Deserializar<DFeNFeABI.NFeABI>(xml.OuterXml);
            Assert.Equal(2026, roundTrip.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.GInfPagAntecip.DtPagAntecip.Year);
            Assert.Equal(11, roundTrip.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.GInfPagAntecip.DtPagAntecip.Month);
            Assert.Equal(18, roundTrip.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.GInfPagAntecip.DtPagAntecip.Day);
            Assert.Equal("2026-11-18", roundTrip.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.GInfPagAntecip.DtPagAntecipField);
            Assert.Equal("123", roundTrip.InfNFeABI.Imovel.Cadastro.NIptu);

            Assert.Equal(typeof(double), typeof(DFeNFeABI.Total).GetProperty(nameof(DFeNFeABI.Total.VTotalOperacao)).PropertyType);
            Assert.Equal(typeof(double), typeof(DFeNFeABI.Total).GetProperty(nameof(DFeNFeABI.Total.VOperacIndiv)).PropertyType);
            Assert.Equal(typeof(double), typeof(DFeNFeABI.GIBSCBS).GetProperty(nameof(DFeNFeABI.GIBSCBS.VOperacIndiv)).PropertyType);
#if INTEROP
            var propriedadesInterop = new[]
            {
                typeof(DFeNFeABI.Total).GetProperty(nameof(DFeNFeABI.Total.VTotalOperacao)),
                typeof(DFeNFeABI.Total).GetProperty(nameof(DFeNFeABI.Total.VOperacIndiv)),
                typeof(DFeNFeABI.GIBSCBS).GetProperty(nameof(DFeNFeABI.GIBSCBS.VOperacIndiv))
            };
            Assert.All(propriedadesInterop, propriedade =>
            {
                var atributo = (System.Runtime.InteropServices.ComVisibleAttribute)Attribute.GetCustomAttribute(propriedade, typeof(System.Runtime.InteropServices.ComVisibleAttribute));
                Assert.True(atributo == null || atributo.Value);
            });
#endif

            documento.InfNFeABI.GInfTrib.IBSCBS.GIBSCBS.VOperacIndiv = 0;
            documento.InfNFeABI.Total.VTotalOperacao = 0;
            documento.InfNFeABI.Total.VOperacIndiv = 0;
            var xmlComZerosExplicitos = documento.GerarXML();
            var valoresTotaisOperacao = xmlComZerosExplicitos.GetElementsByTagName("vTotalOperacao").Cast<XmlElement>().ToArray();
            Assert.Equal(2, valoresTotaisOperacao.Length);
            Assert.Contains(valoresTotaisOperacao, elemento => elemento.InnerText == "0.00");
            Assert.Equal(2, xmlComZerosExplicitos.GetElementsByTagName("vOperacIndiv").Count);
            Assert.All(xmlComZerosExplicitos.GetElementsByTagName("vOperacIndiv").Cast<XmlElement>(), elemento => Assert.Equal("0.00", elemento.InnerText));
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SerializaRegimeDeTransicaoComDataOpcionalTipada()
        {
            var documento = CriarDocumentoCompleto();
            documento.InfNFeABI.Ide.TpNF = TipoNotaFiscalNFeABI.Transicao;
            documento.InfNFeABI.Ide.GModNat = null;
            documento.InfNFeABI.Ide.TpPagAnt = TipoPagamentoAntecipadoNFeABI.FornecimentoPagamentoRealizadoAnteriormente;
            documento.InfNFeABI.GPagAntecipado = new DFeNFeABI.GPagAntecipado
            {
                ChNFePagAnt = new List<string> { new string('2', 44), new string('3', 44) }
            };
            documento.InfNFeABI.Transmit = null;
            documento.InfNFeABI.Adquirente = null;
            documento.InfNFeABI.Imovel = null;
            documento.InfNFeABI.InfOper = null;
            documento.InfNFeABI.GInfTrib = null;
            documento.InfNFeABI.GInfTribTransic = new DFeNFeABI.GInfTribTransic
            {
                CNPJ = "12345678000123",
                XNome = "CONTRIBUINTE TESTE",
                IBSCBS = new DFeNFeABI.IBSCBSTransic
                {
                    CST = "000",
                    CClassTrib = "000001",
                    GExigSusp = new DFeNFeABI.GExigSusp
                    {
                        TpSusp = TipoSuspensaoExigibilidadeNFeABI.DecisaoJudicial,
                        NProcesso = "PROCESSO-TESTE-001"
                    },
                    GIBSCBSTransic = new DFeNFeABI.GIBSCBSTransic
                    {
                        CNPJIncImob = "12345678000123",
#if INTEROP
                        DtRecUni = new DateTime(2026, 11, 17, 20, 0, 0),
#else
                        DtRecUni = new DateTimeOffset(2026, 11, 17, 20, 0, 0, TimeSpan.FromHours(-3)),
#endif
                        VRecRecebRET = 100,
                        VRecFinRET = 10,
                        VRecTotal = 110,
                        VBC = 110,
                        GIBSUF = new DFeNFeABI.GIBSUFTransic { PIBSUF = 0.1, VIBSUF = 0.11 },
                        GIBSMun = new DFeNFeABI.GIBSMunTransic { PIBSMun = 0.1, VIBSMun = 0.11 },
                        GCBS = new DFeNFeABI.GCBSTransic { PCBS = 0.9, VCBS = 0.99 }
                    }
                }
            };
            documento.InfNFeABI.Total.VTotalOperacao = -1;
            documento.InfNFeABI.Total.VOperacIndiv = -1;

            var xml = documento.GerarXML();
            Assert.Contains("<tpNF>2</tpNF>", xml.OuterXml);
            Assert.Contains("<tpSusp>01</tpSusp>", xml.OuterXml);
            Assert.Contains("<dtRecUni>2026-11-17</dtRecUni>", xml.OuterXml);
            Assert.Equal(2, xml.GetElementsByTagName("chNFePagAnt").Count);
            AssertSchema(xml, "NFeABI.NFeABI_v1.00.xsd");

#if INTEROP
            documento.InfNFeABI.GInfTribTransic.IBSCBS.GIBSCBSTransic.DtRecUni = DateTime.MinValue;
#else
            documento.InfNFeABI.GInfTribTransic.IBSCBS.GIBSCBSTransic.DtRecUni = DateTimeOffset.MinValue;
#endif
            Assert.DoesNotContain("<dtRecUni>", documento.GerarXML().OuterXml);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SerializaConsultasRetornosEProcessado()
        {
            var consultaStatus = new DFeNFeABI.ConsStatServNFeABI { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, CUF = UFBrasil.PR };
            var consultaSituacao = new DFeNFeABI.ConsSitNFeABI { Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, ChNFeABI = new string('1', 44) };
            var protocolo = new DFeNFeABI.ProtNFeABI
            {
                Versao = "1.00",
                InfProt = new DFeNFeABI.InfProtNFeABI
                {
                    TpAmb = TipoAmbiente.Homologacao,
                    VerAplic = "TESTE",
                    ChNFeABI = new string('1', 44),
                    DhRecbtoField = "2026-09-14T12:00:00-03:00",
                    NProt = "1412600000000001",
                    CStat = 100,
                    XMotivo = "Autorizado"
                }
            };
            var retorno = new DFeNFeABI.RetNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                VerAplic = "TESTE",
                CStat = 100,
                XMotivo = "Autorizado",
                CUF = UFBrasil.PR,
                DhRecbtoField = "2026-09-14T12:00:00-03:00",
                ProtNFeABI = protocolo
            };
            var processado = new DFeNFeABI.NFeABIProc { Versao = "1.00", NFeABI = CriarDocumentoCompleto(), ProtNFeABI = protocolo };
            var retornoStatus = new DFeNFeABI.RetConsStatServNFeABI
            {
                Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE", CStat = 107,
                XMotivo = "Servico em operacao", CUF = UFBrasil.PR,
                DhRecbtoField = "2026-09-14T12:00:00-03:00", TMed = 0
            };
            var retornoSituacao = new DFeNFeABI.RetConsSitNFeABI
            {
                Versao = "1.00", TpAmb = TipoAmbiente.Homologacao, VerAplic = "TESTE", CStat = 217,
                XMotivo = "NFeABI nao consta", CUF = UFBrasil.PR,
                DhRecbtoField = "2026-09-14T12:00:00-03:00",
                ChNFeABI = new string('1', 44),
                ProtNFeABI = new DFeNFeABI.ProtocoloConsultaNFeABI { Versao = "1.00", ConteudoXML = "<protocolo xmlns=\"urn:teste\">OK</protocolo>" },
                ProcEventoNFeABI = new List<DFeNFeABI.ProcessoEventoConsultaNFeABI>
                {
                    new DFeNFeABI.ProcessoEventoConsultaNFeABI { Versao = "1.00", ConteudoXML = "<evento xmlns=\"urn:teste\">OK</evento>" }
                }
            };

            AssertSchema(consultaStatus.GerarXML(), "NFeABI.consStatServNFeABI_v1.00.xsd");
            AssertSchema(consultaSituacao.GerarXML(), "NFeABI.consSitNFeABI_v1.00.xsd");
            AssertSchema(retorno.GerarXML(), "NFeABI.retNFeABI_v1.00.xsd");
            AssertSchema(processado.GerarXML(), "NFeABI.procNFeABI_v1.00.xsd");
            AssertSchema(retornoStatus.GerarXML(), "NFeABI.retConsStatServNFeABI_v1.00.xsd");
            AssertSchema(retornoSituacao.GerarXML(), "NFeABI.retConsSitNFeABI_v1.00.xsd");
            Assert.Contains("<tMed>0</tMed>", retornoStatus.GerarXML().OuterXml);
            Assert.Equal(0, XMLUtility.Deserializar<DFeNFeABI.RetConsStatServNFeABI>(retornoStatus.GerarXML().OuterXml).TMed);
            var consultaRoundTrip = XMLUtility.Deserializar<DFeNFeABI.RetConsSitNFeABI>(retornoSituacao.GerarXML().OuterXml);
            Assert.Contains("<protocolo", consultaRoundTrip.ProtNFeABI.ConteudoXML);
            Assert.Contains("<evento", consultaRoundTrip.ProcEventoNFeABI[0].ConteudoXML);
            Assert.Equal(new string('1', 44) + "-procNFeABI.xml", processado.NomeArquivoDistribuicao);
        }

        private static DFeNFeABI.NFeABI CriarDocumentoCompleto()
        {
            var endereco = new DFeNFeABI.Endereco
            {
                XLgr = "RUA EXEMPLO", Nro = "100", XBairro = "CENTRO", CMun = 4106902,
                XMun = "CURITIBA", UF = UFBrasil.PR, CEP = "80000000"
            };

            return new DFeNFeABI.NFeABI
            {
                InfNFeABI = new DFeNFeABI.InfNFeABI
                {
                    Versao = "1.00",
                    Ide = new DFeNFeABI.Ide
                    {
                        CUF = UFBrasil.PR, CNF = "1234567", Mod = ModeloDFe.NFeABI, Serie = 1, NNF = 1,
                        DhEmiField = "2026-09-14T12:00:00-03:00",
                        TpNF = TipoNotaFiscalNFeABI.Completa, TpImp = TipoImpressaoNFeABI.Retrato,
                        GModNat = new DFeNFeABI.GModNat { ModOper = "01", NatOper = "01", DetOper = "01" },
                        TpEmis = TipoEmissaoNFeABI.Normal, NSiteAutoriz = "0", TpAmb = TipoAmbiente.Homologacao,
                        FinNFe = FinalidadeNFeABI.Normal, ProcEmi = ProcessoEmissaoNFeABI.AplicativoContribuinte, VerProc = "TESTE ABI-003"
                    },
                    NFref = new List<DFeNFeABI.NFref> { new DFeNFeABI.NFref { RefNFeABI = new string('2', 44) } },
                    Emit = new DFeNFeABI.Emit { CNPJ = "12345678000123", XNome = "EMITENTE TESTE", XFant = "EMITENTE", EnderEmit = endereco, TpEmit = "01" },
                    Transmit = new List<DFeNFeABI.Transmit>
                    {
                        new DFeNFeABI.Transmit { NTransmit = 1, CNPJ = "12345678000123", XNome = "TRANSMITENTE 1", PTransIndiv = 60, IndContrib = IndicadorContribuinteNFeABI.Contribuinte },
                        new DFeNFeABI.Transmit { NTransmit = 2, CPF = "12345678901", XNome = "TRANSMITENTE 2", PTransIndiv = 40, IndContrib = IndicadorContribuinteNFeABI.NaoContribuinte }
                    },
                    Adquirente = new List<DFeNFeABI.Adquirente>
                    {
                        new DFeNFeABI.Adquirente { NAdquir = 1, CPF = "12345678901", XNome = "ADQUIRENTE 1", EnderAdquirente = endereco, PAquisicao = 70 },
                        new DFeNFeABI.Adquirente { NAdquir = 2, CNPJ = "98765432000198", XNome = "ADQUIRENTE 2", EnderAdquirente = endereco, PAquisicao = 30 }
                    },
                    Imovel = new DFeNFeABI.Imovel
                    {
                        Tipo = TipoImovelNFeABI.Urbano,
                        Cadastro = new DFeNFeABI.CadastroImovel
                        {
                            CCIB = "12345678",
                            IndIPTU = IndicadorSimNaoNFeABI.Sim,
                            NIptu = "123",
                            IndIncra = IndicadorSimNaoNFeABI.Sim,
                            NIncra = "INCRA-123",
                            IndRIP = IndicadorSimNaoNFeABI.Sim,
                            NRIP = "RIP-123"
                        },
                        EspImovel = "01", Enquadramento = "01", EnderImovel = endereco, AreaTotal = 100, UAreaTotal = "m2",
                        CartorioRegistro = "CARTORIO TESTE", MatricTransc = "12345"
                    },
                    AutXML = new List<DFeNFeABI.AutXML> { new DFeNFeABI.AutXML { NAutXML = "1", CNPJ = "12345678000123" } },
                    InfOper = new DFeNFeABI.InfOper
                    {
                        PTransImovel = 100, VTotalOperacao = 250000, IndTorna = IndicadorSimNaoNFeABI.Sim,
                        GTorna = new DFeNFeABI.GTorna { VTorna = 10000, ImovelTorna = new List<DFeNFeABI.Imovel> { new DFeNFeABI.Imovel { Tipo = TipoImovelNFeABI.Rural, Cadastro = new DFeNFeABI.CadastroImovel { CCIB = "87654321" }, EspImovel = "02", Enquadramento = "01", EnderImovel = endereco } } },
                        GRedAjusteImovel = new DFeNFeABI.GRedAjusteImovel { VInicial = 200000, VITBI = 5000, VRedAjusteImovel = 195000 },
                        IndRedSocial = IndicadorSimNaoNFeABI.Nao,
                        Instrumento = new DFeNFeABI.Instrumento { TpInstrumento = "01", DtInstrumento = new DateTime(2026, 9, 14) },
                        IndIntermedCorret = IndicadorSimNaoNFeABI.Sim,
                        GIntermedCorret = new List<DFeNFeABI.GIntermedCorret> { new DFeNFeABI.GIntermedCorret { NCorretagem = 1, CNPJ = "12345678000123", XNome = "CORRETOR", VCorretagem = 5000 } }
                    },
                    GInfTrib = new DFeNFeABI.GInfTrib
                    {
                        IBSCBS = new DFeNFeABI.IBSCBS
                        {
                            CST = "000", CClassTrib = "000001",
                            GIBSCBS = new DFeNFeABI.GIBSCBS
                            {
                                VOperacIndiv = 250000, VBC = 250000,
                                GIBSUF = new DFeNFeABI.GIBSUF { PIBSUF = 0.1, GRed = new DFeNFeABI.GRed { PRedAliq = 0, PAliqEfet = 0.1 }, VIBSUF = 250 },
                                GIBSMun = new DFeNFeABI.GIBSMun { PIBSMun = 0.1, GRed = new DFeNFeABI.GRed { PRedAliq = 0, PAliqEfet = 0.1 }, VIBSMun = 250 },
                                GCBS = new DFeNFeABI.GCBS { PCBS = 0.9, GRed = new DFeNFeABI.GRed { PRedAliq = 0, PAliqEfet = 0.9 }, VCBS = 2250 }
                            }
                        }
                    },
                    Pag = new DFeNFeABI.Pag
                    {
                        DetPagImovel = new List<DFeNFeABI.DetPagImovel> { new DFeNFeABI.DetPagImovel { NDetPag = "1", TPag = "01", VPag = 250000 } },
                        DetPagIncorpLote = new List<DFeNFeABI.DetPagIncorpLote> { new DFeNFeABI.DetPagIncorpLote { NDetPag = "2", TParcela = "01", NParcela = "1", XParcela = "ENTRADA" } }
                    },
                    Total = new DFeNFeABI.Total
                    {
                        VTotalOperacao = 250000, VOperacIndiv = 250000,
                        TribTot = new DFeNFeABI.TribTot { VRedAjusteIndiv = 0, VRedSocialIndiv = 0, VBC = 250000, VIBSUF = 250, VIBSMun = 250, VCBS = 2250 },
                        VNF = 250000
                    },
                    InfAdic = new DFeNFeABI.InfAdic { InfCpl = "TESTE COMPLETO", ObsCont = new List<DFeNFeABI.Obs> { new DFeNFeABI.Obs { XCampo = "CAMPO", XTexto = "VALOR" } } },
                    InfRespTec = new DFeNFeABI.InfRespTec { CNPJ = "12345678000123", XContato = "CONTATO", Email = "teste@example.com", Fone = "41999999999" }
                },
                InfNFeSupl = new DFeNFeABI.InfNFeSupl { QrCode = "https://example.com/nfeabi/consulta?chave=41260912345678000123770010000000011012345670&ambiente=2&teste=abi003", UrlChave = "https://example.com/nfeabi" },
                Signature = CarregarAssinaturaFixture()
            };
        }

        private static Unimake.Business.DFe.Xml.Signature CarregarAssinaturaFixture()
        {
            var original = new XmlDocument();
            original.Load(Path.Combine(@"..\..\..\NFeABI\Resources", "NFeABI-minima.xml"));
            return XMLUtility.Deserializar<DFeNFeABI.NFeABI>(original.OuterXml).Signature;
        }

        private static void AssertSchema(XmlDocument documento, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(documento, schema, NamespaceNFeABI);
            Assert.True(validador.Success, validador.ErrorMessage);
        }
    }
}
