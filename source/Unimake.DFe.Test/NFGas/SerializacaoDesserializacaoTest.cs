using System.Collections.Generic;
using System.IO;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.NFGas;
using Xunit;

namespace Unimake.DFe.Test.NFGas
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\nfgas.xml")]
        public void SerializacaoDesserializacaoNFGas(string arqXML)
        {
            SerializarDesserializar<Unimake.Business.DFe.Xml.NFGas.NFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\nfgasProc.xml")]
        public void SerializacaoDesserializacaoNFGasProc(string arqXML)
        {
            SerializarDesserializar<NFGasProc>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retNFGas.xml")]
        public void SerializacaoDesserializacaoRetNFGas(string arqXML)
        {
            SerializarDesserializar<RetNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\consStatServNFGas-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServNFGas(string arqXML)
        {
            SerializarDesserializar<ConsStatServNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retConsStatServNFGas.xml")]
        public void SerializacaoDesserializacaoRetConsStatServNFGas(string arqXML)
        {
            SerializarDesserializar<RetConsStatServNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\consSitNFGas-ped-sit.xml")]
        public void SerializacaoDesserializacaoConsSitNFGas(string arqXML)
        {
            SerializarDesserializar<ConsSitNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retConsSitNFGas.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFGas(string arqXML)
        {
            SerializarDesserializar<RetConsSitNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\eventoNFGas-110111.xml")]
        [InlineData(@"..\..\..\NFGas\Resources\eventoNFGas-110300.xml")]
        [InlineData(@"..\..\..\NFGas\Resources\eventoNFGas-110301.xml")]
        public void SerializacaoDesserializacaoEventoNFGas(string arqXML)
        {
            SerializarDesserializar<EventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\retEventoNFGas.xml")]
        public void SerializacaoDesserializacaoRetEventoNFGas(string arqXML)
        {
            SerializarDesserializar<RetEventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\procEventoNFGas.xml")]
        public void SerializacaoDesserializacaoProcEventoNFGas(string arqXML)
        {
            SerializarDesserializar<ProcEventoNFGas>(arqXML);
        }

        [Theory]
        [Trait("DFe", "NFGas")]
        [InlineData(@"..\..\..\NFGas\Resources\evCancNFGas.xml")]
        public void SerializacaoDesserializacaoEvCancNFGas(string arqXML)
        {
            SerializarDesserializar<EvCancNFGas>(arqXML);
        }

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DeveGerarIdComChaveAcessoNFGas()
        {
            var infNFGas = new InfNFGas
            {
                Ide = new Ide
                {
                    CUF = UFBrasil.SP,
                    Mod = ModeloDFe.NFGas,
                    Serie = 1,
                    NNF = 1,
                    CNF = "0000010",
                    DhEmi = "2026-05-18T10:00:00-03:00",
                    TpEmis = TipoEmissaoNFGas.Normal,
                    NSiteAutoriz = "0"
                },
                Emit = new Emit
                {
                    CNPJ = "12345678000195"
                }
            };

            Assert.Equal(44, infNFGas.Chave.Length);
            Assert.StartsWith("3526051234567800019576001000000001100000010", infNFGas.Chave);
            Assert.Equal("NFGas" + infNFGas.Chave, infNFGas.Id);
            Assert.Equal(49, infNFGas.Id.Length);
            Assert.Equal(infNFGas.Chave[43].ToString(), infNFGas.Ide.CDV.ToString());
        }

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DeveSerializarNovosCamposDosSchemasNFGas()
        {
            var nfgas = new Unimake.Business.DFe.Xml.NFGas.NFGas
            {
                InfNFGas = new InfNFGas
                {
                    Id = "NFGas35260512345678000195760010000000011000000109",
                    Versao = "1.00",
                    Ide = new Ide
                    {
                        CUF = UFBrasil.SP,
                        TpAmb = TipoAmbiente.Homologacao,
                        Mod = ModeloDFe.NFGas,
                        TpEmis = TipoEmissaoNFGas.Normal,
                        FinNFGas = FinalidadeNFGas.Normal,
                        TpFat = TipoFaturamentoNFGas.Normal,
                        TpPagAnt = TipoPagamentoAntecipadoNFCom.PagamentoAntecipadoServicosNaoContinuados
                    },
                    Emit = new Emit
                    {
                        ISUFEmit = "12345678"
                    },
                    PgtoVinc = new PgtoVinc
                    {
                        Pgto = new List<Pgto>
                        {
                            new Pgto
                            {
                                NPag = "1",
                                IdTransacao = "TRANSACAO01",
                                TpMeioPgto = "01",
                                CNPJReceb = "12345678000195",
                                CNPJBasePSP = "12345678"
                            }
                        }
                    },
                    Det = new List<Det>
                    {
                        new Det
                        {
                            NItem = 1,
                            GNormal = new GNormal
                            {
                                Prod = new Prod
                                {
                                    IndOrigemQtd = OrigemQuantidadeFaturadaNFGas.Media,
                                    UMed = UnidadeMedidaNFGas.M3,
                                    GPagAntecipado = new GPagAntecipado
                                    {
                                        ChDFePagAnt = "35260512345678000195760010000000011000000109",
                                        NItemPagAnt = 1
                                    }
                                },
                                Imposto = new Imposto
                                {
                                    IBSCBS = new IBSCBS
                                    {
                                        IndDoacao = IndicadorDoacao.OperacaoDoacao,
                                        GIBSCBS = new GIBSCBS
                                        {
                                            GCBS = new GCBS
                                            {
                                                GDevTrib = new GDevTrib
                                                {
                                                    PDevTrib = "10.00",
                                                    VDevTrib = "1.00"
                                                },
                                                GALCZFMCBS = new GALCZFMCBS
                                                {
                                                    TpALCZFMCBS = TipoAplicacaoAliquotaZeroCBS.ComProcessoSuframa,
                                                    NProcSuframa = "12345678",
                                                    PAliqEfetRegCBS = "10.00",
                                                    VTribRegCBS = "1.00"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var doc = nfgas.GerarXML();
            var ns = new XmlNamespaceManager(doc.NameTable);
            ns.AddNamespace("nfgas", "http://www.portalfiscal.inf.br/nfgas");

            Assert.Equal("1", doc.SelectSingleNode("//nfgas:tpPagAnt", ns)?.InnerText);
            Assert.Equal("12345678", doc.SelectSingleNode("//nfgas:ISUFEmit", ns)?.InnerText);
            Assert.Equal("35260512345678000195760010000000011000000109", doc.SelectSingleNode("//nfgas:gPagAntecipado/nfgas:chDFePagAnt", ns)?.InnerText);
            Assert.Equal("1", doc.SelectSingleNode("//nfgas:gPagAntecipado/nfgas:nItemPagAnt", ns)?.InnerText);
            Assert.Equal("1", doc.SelectSingleNode("//nfgas:IBSCBS/nfgas:indDoacao", ns)?.InnerText);
            Assert.Equal("10.00", doc.SelectSingleNode("//nfgas:gDevTrib/nfgas:pDevTrib", ns)?.InnerText);
            Assert.Equal("2", doc.SelectSingleNode("//nfgas:gALCZFMCBS/nfgas:tpALCZFMCBS", ns)?.InnerText);
            Assert.Equal("12345678", doc.SelectSingleNode("//nfgas:gALCZFMCBS/nfgas:nProcSuframa", ns)?.InnerText);
            Assert.Equal("1.00", doc.SelectSingleNode("//nfgas:gALCZFMCBS/nfgas:vTribRegCBS", ns)?.InnerText);
            Assert.Equal("01", doc.SelectSingleNode("//nfgas:pgtoVinc/nfgas:pgto/nfgas:tpMeioPgto", ns)?.InnerText);
            Assert.Equal("TRANSACAO01", doc.SelectSingleNode("//nfgas:pgtoVinc/nfgas:pgto/@idTransacao", ns)?.InnerText);
        }

        [Fact]
        [Trait("DFe", "NFGas")]
        public void DeveSerializarTiposBasicosRTCNFGas()
        {
            var impostoSeletivo = SerializarObjeto(new IS
            {
                CSTIS = "000",
                CClassTribIS = "000001",
                VBCIS = "10.00",
                PIS = "1.0000",
                AdRemIS = "0.5000",
                UTrib = "M3",
                QTrib = "1.0000",
                VIS = "0.10"
            }, "IS");

            var creditoPresumido = SerializarObjeto(new GCredPresIBSZFM
            {
                CompetApur = "2026-07",
                TpCredPresIBSZFM = TipoCreditoPresumidoIBSZFM.BensCapital,
                VCredPresIBSZFM = "12.34"
            }, "gCredPresIBSZFM");

            var tribItemSN = SerializarObjeto(new TribItemSN
            {
                NItem = "1",
                VRBSNItem = "100.00",
                TpRBSN = TipoReceitaBrutaSimplesNacional.ReceitaBrutaInternaSemCalculoIBSCBS,
                PIBSSN = "1.0000",
                VIBSSN = "1.00",
                PCBSSN = "2.0000",
                VCBSSN = "2.00",
                VIBSPendSusp = "3.00",
                VCBSPendSusp = "4.00"
            }, "gTribItemSN");

            var totalSN = SerializarObjeto(new TotalSN
            {
                VRBSNTot = "100.00",
                VIBSSNTot = "1.00",
                VIBSSNtotPendSusp = "3.00",
                VCBSSNTot = "2.00",
                VCBSSNtotPendSusp = "4.00"
            }, "totalSN");

            Assert.Null(impostoSeletivo.SelectSingleNode("/*[local-name()='IS']/*[local-name()='pISEspec']"));
            Assert.Equal("0.5000", impostoSeletivo.SelectSingleNode("/*[local-name()='IS']/*[local-name()='adRemIS']")?.InnerText);
            Assert.Equal("2026-07", creditoPresumido.SelectSingleNode("/*[local-name()='gCredPresIBSZFM']/*[local-name()='competApur']")?.InnerText);
            Assert.Equal("2", creditoPresumido.SelectSingleNode("/*[local-name()='gCredPresIBSZFM']/*[local-name()='tpCredPresIBSZFM']")?.InnerText);
            Assert.Equal("1", tribItemSN.SelectSingleNode("/*[local-name()='gTribItemSN']/@nItem")?.InnerText);
            Assert.Equal("2", tribItemSN.SelectSingleNode("/*[local-name()='gTribItemSN']/*[local-name()='tpRBSN']")?.InnerText);
            Assert.Equal("4.00", totalSN.SelectSingleNode("/*[local-name()='totalSN']/*[local-name()='vCBSSNtotPendSusp']")?.InnerText);
        }

        private static void SerializarDesserializar<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new T().LerXML<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        private static XmlDocument SerializarObjeto<T>(T objeto, string nomeRaiz)
        {
            var serializer = new XmlSerializer(typeof(T), new XmlRootAttribute(nomeRaiz));

            using (var writer = new StringWriter())
            {
                serializer.Serialize(writer, objeto);

                var doc = new XmlDocument();
                doc.LoadXml(writer.ToString());

                return doc;
            }
        }
    }
}
