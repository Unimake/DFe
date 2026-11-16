using System.Collections.Generic;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    [Trait("DFe", "NFe")]
    public class SimplesNacionalTest
    {
        private const string Ns = "http://www.portalfiscal.inf.br/nfe";
        private const string Recurso = @"..\..\..\NFe\Resources\EventoTributacaoSimplesNacional.xml";

        [Theory]
        [InlineData(1, "01", true)]
        [InlineData(2, "02", false)]
        [InlineData(3, "03", true)]
        [InlineData(4, "04", false)]
        [InlineData(4, "90", true)]
        public void NFePreservaRegimeAtividadeGruposEOrdem(int regime, string atividade, bool informarIBS)
        {
            var nfe = CriarNFe();
            var inf = nfe.InfNFeField;
            inf.Emit.TpRegimeIBSCBS = (TipoRegimeIBSCBS)regime;
            inf.Emit.ISUFEmit = "12345678";
            inf.Det[0].Prod.CAtivSN = (CodigoAtividadeSimplesNacional)int.Parse(atividade);
            inf.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "000", CClassTrib = "000001",
                GTribSN = new GTribSN
                {
                    GIBSSN = informarIBS ? new GIBSSN { PIBSSN = 1.2345, VIBSSN = 1.23 } : null,
                    GCBSSN = new GCBSSN { PCBSSN = 2.3456, VCBSSN = 0 }
                },
                GEstornoCred = new GEstornoCred()
            };
            inf.Total.IBSCBSTot = new IBSCBSTot
            {
                VBCIBSCBS = 100, GEstornoCred = new GEstornoCred(),
                GTribSN = new GTribSNTot { VIBSSN = informarIBS ? 1.23 : 0, VCBSSN = 0 }
            };

            var xml = XMLUtility.Serializar(nfe);
            Validar(xml, "nfe_v4.00.xsd");
            Assert.Equal(atividade, xml.GetElementsByTagName("cAtivSN")[0].InnerText);
            Assert.Equal(regime.ToString(), xml.GetElementsByTagName("tpRegimeIBSCBS")[0].InnerText);
            Assert.Equal(new[] { "CRT", "tpRegimeIBSCBS", "ISUFEmit" }, Filhos(xml.GetElementsByTagName("emit")[0]).TakeLast(3));
            var produto = Filhos(xml.GetElementsByTagName("prod")[0]).ToList();
            Assert.True(produto.IndexOf("cAtivSN") > produto.IndexOf("indTot"));
            Assert.Equal(new[] { "CST", "cClassTrib", "gTribSN", "gEstornoCred" }, Filhos(xml.GetElementsByTagName("IBSCBS")[0]));
            Assert.Equal(new[] { "vBCIBSCBS", "gEstornoCred", "gTribSN" }, Filhos(xml.GetElementsByTagName("IBSCBSTot")[0]));
            Assert.Equal("2.3456", xml.GetElementsByTagName("pCBSSN")[0].InnerText);
            Assert.Equal("0.00", xml.GetElementsByTagName("vCBSSN")[0].InnerText);
            Assert.Equal(informarIBS ? 1 : 0, xml.GetElementsByTagName("gIBSSN").Count);

            var lido = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(xml);
            Assert.Equal((TipoRegimeIBSCBS)regime, lido.InfNFeField.Emit.TpRegimeIBSCBS);
            Assert.Equal((CodigoAtividadeSimplesNacional)int.Parse(atividade), lido.InfNFeField.Det[0].Prod.CAtivSN);
            Assert.Equal(2.3456, lido.InfNFeField.Det[0].Imposto.IBSCBS.GTribSN.GCBSSN.PCBSSN);
            Assert.Equal(0, lido.InfNFeField.Total.IBSCBSTot.GTribSN.VCBSSN);
            var lote = new EnviNFe { Versao = "4.00", IdLote = "1", IndSinc = SimNao.Sim, NFe = new List<Business.DFe.Xml.NFe.NFe> { lido } };
            Validar(lote.GerarXML(), "enviNFe_v4.00.xsd");
        }

        [Fact]
        public void NFeOmiteCamposNovosNaoInformadosEPreservaSchemaLegado()
        {
            var xml = XMLUtility.Serializar(CriarNFe());
            foreach (var nome in new[] { "tpRegimeIBSCBS", "cAtivSN", "gTribSN" })
            {
                Assert.Equal(0, xml.GetElementsByTagName(nome).Count);
            }
            Validar(xml, "nfe_v4.00.xsd");
            Validar(xml, "nfe_v4.00_monofasia_legado.xsd");
        }

        [Fact]
        public void SchemaExigeCBSAoInformarComposicaoSimplesNacional()
        {
            var nfe = CriarNFe();
            nfe.InfNFeField.Det[0].Imposto.IBSCBS = new IBSCBS
            {
                CST = "000", CClassTrib = "000001", GTribSN = new GTribSN { GIBSSN = new GIBSSN() }
            };
            var validar = new ValidarSchema();
            validar.Validar(XMLUtility.Serializar(nfe), "NFe.nfe_v4.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains("gCBSSN", validar.ErrorMessage);
        }

        [Fact]
        public void Evento412140LeTodosOsItensTotaisEOpcionais()
        {
            var original = new XmlDocument();
            original.Load(Recurso);
            var modelo = XMLUtility.Deserializar<EnvEvento>(original);
            var detalhe = Assert.IsType<DetEventoTributacaoSN>(modelo.Evento[0].InfEvento.DetEvento);
            Assert.Equal(TipoEventoNFe.Evento412140, modelo.Evento[0].InfEvento.TpEvento);
            Assert.Equal(UFBrasil.PR, detalhe.COrgaoAutor);
            Assert.Equal(TipoAutor.Fisco, detalhe.TpAutor);
            Assert.Equal("TESTE & XML", detalhe.VerAplic);
            Assert.Equal(2, detalhe.GTribItemSN.Count);
            Assert.Equal(990, detalhe.GTribItemSN[1].NItem);
            Assert.Equal(100, detalhe.GTribItemSN[0].VRBSNItem);
            Assert.Equal(TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna, detalhe.GTribItemSN[0].TpRBSN);
            Assert.Equal(1.2345, detalhe.GTribItemSN[0].PIBSSN);
            Assert.Equal(0, detalhe.GTribItemSN[0].VCBSSN);
            Assert.False(detalhe.GTribItemSN[1].ShouldSerializePIBSSNField());
            Assert.Equal(100, detalhe.GTotalSN.VRBSNTot);
            Assert.Equal(0.1, detalhe.GTotalSN.VIBSSNTotPendSusp);
            var xml = modelo.GerarXML();
            Assert.Equal(original.InnerText, xml.InnerText);
            Validar(Documento(xml.GetElementsByTagName("detEvento")[0]), "e412140_v1.00.xsd");
            Assert.Equal(new[] { "descEvento", "cOrgaoAutor", "tpAutor", "verAplic", "gTribItemSN", "gTribItemSN", "gTotalSN" },
                Filhos(xml.GetElementsByTagName("detEvento")[0]));
            var lido = XMLUtility.Deserializar<EnvEvento>(xml);
            Assert.Equal(0, Assert.IsType<DetEventoTributacaoSN>(lido.Evento[0].InfEvento.DetEvento).GTotalSN.VCBSSNTot);
        }

        [Theory]
        [InlineData("Tributação do Simples Nacional")]
        [InlineData("Tributacao do Simples Nacional")]
        public void Evento412140GeradoPelaApiPreservaZeroEOmiteAusentes(string descricao)
        {
            var detalhe = new DetEventoTributacaoSN
            {
                DescEvento = descricao, COrgaoAutor = UFBrasil.PR, VerAplic = "TESTE & XML",
                GTribItemSN = new List<GTribItemSN>
                {
                    new GTribItemSN { NItem = 1, VRBSNItem = 100, TpRBSN = TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna, PIBSSN = 0, VIBSSN = 0 },
                    new GTribItemSN { NItem = 2, VRBSNItem = 0, TpRBSN = TipoReceitaBrutaSimplesNacional.NaoReceitaBruta }
                },
                GTotalSN = new GTotalSN { VRBSNTot = 100, VIBSSNTot = 0 }
            };
            var modelo = new EnvEvento
            {
                Versao = "1.00", IdLote = "1", Evento = new List<Evento>
                {
                    new Evento
                    {
                        Versao = "1.00", InfEvento = new InfEvento(detalhe)
                        {
                            TpEvento = TipoEventoNFe.Evento412140, TpAmb = TipoAmbiente.Homologacao,
                            COrgao = UFBrasil.PR, CNPJ = "00000000000191", ChNFe = "41261100000000000191550010000000011000000010",
                            DhEventoField = "2026-11-13T10:00:00-03:00", NSeqEvento = 1, VerEvento = "1.00"
                        }
                    }
                }
            };
            var xml = modelo.GerarXML();
            Validar(Documento(xml.GetElementsByTagName("detEvento")[0]), "e412140_v1.00.xsd");
            Assert.StartsWith("ID412140", modelo.Evento[0].InfEvento.Id);
            var itens = xml.GetElementsByTagName("gTribItemSN");
            Assert.Equal(new[] { "vRBSNitem", "tpRBSN", "pIBSSN", "vIBSSN" }, Filhos(itens[0]));
            Assert.Equal(new[] { "vRBSNitem", "tpRBSN" }, Filhos(itens[1]));
            Assert.Equal("0.00", xml.GetElementsByTagName("pIBSSN")[0].InnerText);
            Assert.Equal(new[] { "vRBSNtot", "vIBSSNtot" }, Filhos(xml.GetElementsByTagName("gTotalSN")[0]));
            var roundTrip = Assert.IsType<DetEventoTributacaoSN>(XMLUtility.Deserializar<EnvEvento>(xml).Evento[0].InfEvento.DetEvento);
            Assert.True(roundTrip.GTribItemSN[0].ShouldSerializePIBSSNField());
            Assert.False(roundTrip.GTotalSN.ShouldSerializeVCBSSNTotField());
        }

        [Theory]
        [InlineData(0)]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(4)]
        [InlineData(5)]
        [InlineData(9)]
        public void Evento412140AceitaTodoODominioDeReceitaBruta(int codigo)
        {
            var original = new XmlDocument();
            original.Load(Recurso);
            var modelo = XMLUtility.Deserializar<EnvEvento>(original);
            var detalhe = Assert.IsType<DetEventoTributacaoSN>(modelo.Evento[0].InfEvento.DetEvento);
            detalhe.GTribItemSN[0].TpRBSN = (TipoReceitaBrutaSimplesNacional)codigo;
            var xml = modelo.GerarXML();
            Validar(Documento(xml.GetElementsByTagName("detEvento")[0]), "e412140_v1.00.xsd");
            Assert.Equal(codigo.ToString(), xml.GetElementsByTagName("tpRBSN")[0].InnerText);
            var evento = XMLUtility.Deserializar<Evento>(xml.GetElementsByTagName("evento")[0].OuterXml);
            Assert.Equal((TipoReceitaBrutaSimplesNacional)codigo,
                Assert.IsType<DetEventoTributacaoSN>(evento.InfEvento.DetEvento).GTribItemSN[0].TpRBSN);
        }

        [Fact]
        public void LoteComDoisEventosMantemItensETotaisSeparados()
        {
            var xml = new XmlDocument();
            xml.Load(Recurso);
            var segundo = (XmlElement)xml.GetElementsByTagName("evento")[0].CloneNode(true);
            segundo.GetElementsByTagName("nSeqEvento")[0].InnerText = "2";
            segundo.GetElementsByTagName("vRBSNitem")[0].InnerText = "200.00";
            segundo.GetElementsByTagName("vRBSNtot")[0].InnerText = "200.00";
            xml.DocumentElement.AppendChild(segundo);
            var modelo = new EnvEvento().LerXML<EnvEvento>(xml);
            Assert.Equal(2, modelo.Evento.Count);
            var primeiroDet = Assert.IsType<DetEventoTributacaoSN>(modelo.Evento[0].InfEvento.DetEvento);
            var segundoDet = Assert.IsType<DetEventoTributacaoSN>(modelo.Evento[1].InfEvento.DetEvento);
            Assert.Equal(100, primeiroDet.GTribItemSN[0].VRBSNItem);
            Assert.Equal(200, segundoDet.GTribItemSN[0].VRBSNItem);
            Assert.Equal(200, segundoDet.GTotalSN.VRBSNTot);
            Assert.Equal(2, segundoDet.GTribItemSN.Count);
        }

        [Theory]
        [InlineData("tpAutor", "1")]
        [InlineData("nItem", "991")]
        [InlineData("tpRBSN", "8")]
        [InlineData("vRBSNitem", "-1.00")]
        public void SchemaEvento412140RejeitaDominiosInvalidos(string nome, string valor)
        {
            var xml = new XmlDocument();
            xml.Load(Recurso);
            var det = Documento(xml.GetElementsByTagName("detEvento")[0]);
            if (nome == "nItem")
            {
                ((XmlElement)det.GetElementsByTagName("gTribItemSN")[0]).SetAttribute(nome, valor);
            }
            else
            {
                det.GetElementsByTagName(nome)[0].InnerText = valor;
            }
            var validar = new ValidarSchema();
            validar.Validar(det, "NFe.e412140_v1.00.xsd", Ns);
            Assert.False(validar.Success);
            Assert.Contains(nome, validar.ErrorMessage);
        }

#if INTEROP
        [Fact]
        public void HelpersInteropPreservamListaDeItens()
        {
            var detalhe = new DetEventoTributacaoSN();
            Assert.Null(detalhe.GetGTribItemSN(0));
            Assert.Equal(0, detalhe.GetGTribItemSNCount);
            var item = new GTribItemSN { NItem = 1 };
            detalhe.AddGTribItemSN(item);
            Assert.Same(item, detalhe.GetGTribItemSN(0));
            Assert.Equal(1, detalhe.GetGTribItemSNCount);
            foreach (var tipo in new[] { typeof(DetEventoTributacaoSN), typeof(GTribItemSN), typeof(GTotalSN), typeof(GTribSN), typeof(GIBSSN), typeof(GCBSSN), typeof(GTribSNTot) })
            {
                Assert.True(tipo.GetCustomAttributes(typeof(System.Runtime.InteropServices.ComVisibleAttribute), false)
                    .Cast<System.Runtime.InteropServices.ComVisibleAttribute>().Single().Value);
            }
        }
#endif

        private static IEnumerable<string> Filhos(XmlNode node) => node.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName);

        private static Business.DFe.Xml.NFe.NFe CriarNFe()
        {
            var doc = new XmlDocument();
            doc.Load(@"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml");
            var nfe = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(doc.GetElementsByTagName("NFe")[0].OuterXml);
            nfe.InfNFeSupl.QrCode = "https://www.exemplo.gov.br/qrcode?p=41250899999999999900000000000000009000000000|3|2|01|0.00|1|12345678901|YWJjZA==";
            return nfe;
        }

        private static XmlDocument Documento(XmlNode node)
        {
            var doc = new XmlDocument();
            doc.AppendChild(doc.ImportNode(node, true));
            return doc;
        }

        private static void Validar(XmlDocument xml, string schema)
        {
            var validar = new ValidarSchema();
            validar.Validar(xml, "NFe." + schema, Ns);
            Assert.True(validar.Success, validar.ErrorMessage);
        }
    }
}
