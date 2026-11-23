using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFCom;
using Xunit;
using ModeloNFCom = Unimake.Business.DFe.Xml.NFCom.NFCom;

namespace Unimake.DFe.Test.NFCom.Serializacao
{
    public class SimplesNacionalTest
    {
        private const string Ns = "http://www.portalfiscal.inf.br/nfcom";

        private static void Validar(XmlDocument xml, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "NFCom." + schema + "_v1.00.xsd", Ns);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        private static XmlDocument Extrair(XmlDocument xml, string nome)
        {
            var doc = new XmlDocument();
            doc.AppendChild(doc.ImportNode(xml.GetElementsByTagName(nome, Ns)[0], true));
            return doc;
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(4)]
        public void NotaPreservaRegimeGruposPrecisaoEOrdem(int regime)
        {
            var modelo = XMLUtility.Deserializar<ModeloNFCom>(File.ReadAllText(@"..\..\..\NFCom\Resources\NFCom_AtualizacaoSchemas.xml"));
            modelo.InfNFCom.Emit.TpRegimeIBSCBS = (TipoRegimeIBSCBS)regime;
            modelo.InfNFCom.Det[0].Imposto.IBSCBS = new IBSCBS { CST = "000", CClassTrib = "000001", GEstornoCred = new GEstornoCred() };
            modelo.InfNFCom.Total.IBSCBSTot = new IBSCBSTot
            {
                GIBSTot = new GIBSTot { GIBSUFTot = new GIBSUFTot(), GIBSMunTot = new GIBSMunTot() },
                GCBSTot = new GCBSTot()
            };
            modelo.InfNFCom.Det[0].Imposto.IBSCBS.GTribSN = new GTribSN
            {
                GIBSSN = new GIBSSN { PIBSSN = 1.2345, VIBSSN = 1.23 },
                GCBSSN = new GCBSSN { PCBSSN = 2.3456, VCBSSN = 2.35 }
            };
            modelo.InfNFCom.Total.IBSCBSTot.GTribSN = new GTribSNTot { VIBSSN = 1.23, VCBSSN = 2.35 };
            var xml = modelo.GerarXML();
            Validar(xml, "nfcom");
            Assert.Equal(regime.ToString(), xml.GetElementsByTagName("tpRegimeIBSCBS", Ns)[0].InnerText);
            Assert.Equal("1.2345", xml.GetElementsByTagName("pIBSSN", Ns)[0].InnerText);
            Assert.Equal("cClassTrib", xml.GetElementsByTagName("gTribSN", Ns)[0].PreviousSibling.LocalName);
            Assert.Equal("gEstornoCred", xml.GetElementsByTagName("gTribSN", Ns)[0].NextSibling.LocalName);
            Assert.Equal("gCBS", xml.GetElementsByTagName("gTribSN", Ns)[1].PreviousSibling.LocalName);
            var lido = XMLUtility.Deserializar<ModeloNFCom>(xml);
            Assert.Equal(1.2345, lido.InfNFCom.Det[0].Imposto.IBSCBS.GTribSN.GIBSSN.PIBSSN);
            Assert.Equal(2.35, lido.InfNFCom.Total.IBSCBSTot.GTribSN.VCBSSN);
            Assert.Equal(xml.InnerText, lido.GerarXML().InnerText);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void NotaLegadaOmiteNovosGrupos()
        {
            var modelo = XMLUtility.Deserializar<ModeloNFCom>(File.ReadAllText(@"..\..\..\NFCom\Resources\NFCom_AtualizacaoSchemas.xml"));
            var xml = modelo.GerarXML();
            Assert.Equal(0, xml.GetElementsByTagName("tpRegimeIBSCBS", Ns).Count);
            Assert.Equal(0, xml.GetElementsByTagName("gTribSN", Ns).Count);
            Validar(xml, "nfcom");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData("412140", "evTributacaoSN", "evTributacaoSN")]
        [InlineData("412141", "evCancTribSN", "evCancTributacaoSN")]
        public void EventosPreservamTiposConteudoEValidamSchema(string codigo, string raiz, string schema)
        {
            var original = new XmlDocument();
            original.Load(@"..\..\..\NFCom\Resources\eventoNFCom-" + codigo + ".xml");
            Validar(original, "eventoNFCom");
            Validar(Extrair(original, raiz), schema);
            var modelo = XMLUtility.Deserializar<EventoNFCom>(original);
            Assert.Equal(codigo, ((int)modelo.InfEvento.TpEvento).ToString());
            if (codigo == "412140")
            {
                var detalhe = Assert.IsType<DetEventoTributacaoSN>(modelo.InfEvento.DetEvento);
                Assert.Equal(2, detalhe.GTribItemSN.Count);
                Assert.Equal("2", detalhe.GTribItemSN[1].NItem);
                Assert.Equal(TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna, detalhe.GTribItemSN[0].TpRBSN);
                Assert.Equal(1.2345, detalhe.GTribItemSN[0].PIBSSN);
                Assert.Equal(0, detalhe.GTribItemSN[0].VIBSPendSusp);
                Assert.Equal(3.58, detalhe.GTotalSN.VCBSSNTot);
                Assert.False(detalhe.GTribItemSN[1].ShouldSerializePIBSSNField());
            }
            else
            {
                var detalhe = Assert.IsType<DetEventoCancTributacaoSN>(modelo.InfEvento.DetEvento);
                Assert.Equal("1412600000000002", detalhe.NProtEvTribSN);
            }
            var xml = modelo.GerarXML();
            Assert.Equal(original.InnerText, xml.InnerText);
            Assert.NotNull(modelo.Signature);
            Validar(xml, "eventoNFCom");
            Validar(Extrair(xml, raiz), schema);
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<EventoNFCom>(xml).GerarXML().InnerText);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void EventoCriadoPorObjetosPreservaListaOmissoesEZero()
        {
            var detalhe = new DetEventoTributacaoSN
            {
                VersaoEvento = "1.00",
                NProt = "1412600000000001",
                GTribItemSN = new List<GTribItemSN>
                {
                    new GTribItemSN { NItem = "1", VRBSNItem = 100, TpRBSN = TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna, PIBSSN = 0, VIBSSN = 0 },
                    new GTribItemSN { NItem = "2", VRBSNItem = 0, TpRBSN = TipoReceitaBrutaSimplesNacional.NaoReceitaBruta }
                },
                GTotalSN = new GTotalSN { VRBSNTot = 100, VIBSSNTot = 0 }
            };
            var modelo = new EventoNFCom
            {
                Versao = "1.00",
                InfEvento = new InfEvento(detalhe) { TpEvento = TipoEventoNFCom.TributacaoSimplesNacional, TpAmb = TipoAmbiente.Homologacao }
            };
            var xml = modelo.GerarXML();
            var esperado = XElement.Parse("""
                <evTributacaoSN xmlns="http://www.portalfiscal.inf.br/nfcom">
                  <descEvento>Tributação do Simples Nacional</descEvento>
                  <nProt>1412600000000001</nProt>
                  <gTribItemSN nItem="1">
                    <vRBSNItem>100.00</vRBSNItem>
                    <tpRBSN>1</tpRBSN>
                    <pIBSSN>0.00</pIBSSN>
                    <vIBSSN>0.00</vIBSSN>
                  </gTribItemSN>
                  <gTribItemSN nItem="2">
                    <vRBSNItem>0.00</vRBSNItem>
                    <tpRBSN>0</tpRBSN>
                  </gTribItemSN>
                  <gTotalSN>
                    <vRBSNTot>100.00</vRBSNTot>
                    <vIBSSNTot>0.00</vIBSSNTot>
                  </gTotalSN>
                </evTributacaoSN>
                """);
            var gerado = XElement.Parse(Extrair(xml, "evTributacaoSN").OuterXml);
            Assert.True(XNode.DeepEquals(esperado, gerado), gerado.ToString());
            Validar(Extrair(xml, "evTributacaoSN"), "evTributacaoSN");
            var lido = XMLUtility.Deserializar<EventoNFCom>(xml);
            var detalheLido = Assert.IsType<DetEventoTributacaoSN>(lido.InfEvento.DetEvento);
            Assert.Collection(detalheLido.GTribItemSN,
                item =>
                {
                    Assert.Equal("1", item.NItem);
                    Assert.Equal(100, item.VRBSNItem);
                    Assert.Equal(0, item.PIBSSN);
                    Assert.Equal(0, item.VIBSSN);
                },
                item =>
                {
                    Assert.Equal("2", item.NItem);
                    Assert.Equal(0, item.VRBSNItem);
                    Assert.Null(item.PIBSSN);
                    Assert.Null(item.VIBSSN);
                });
            Assert.Equal(100, detalheLido.GTotalSN.VRBSNTot);
            Assert.Equal(0, detalheLido.GTotalSN.VIBSSNTot);
            Assert.Null(detalheLido.GTotalSN.VCBSSNTot);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void LeituraSemItensIgnoraGruposDeOutroNamespaceEElementosDesconhecidos()
        {
            var xml = XDocument.Load(@"..\..\..\NFCom\Resources\eventoNFCom-412140.xml");
            XNamespace ns = Ns;
            XNamespace outroNs = "urn:teste";
            var evento = xml.Descendants(ns + "evTributacaoSN").Single();
            var total = evento.Element(ns + "gTotalSN");
            evento.Elements(ns + "gTribItemSN").Remove();
            total.AddBeforeSelf(new XElement(outroNs + "gTribItemSN"), new XElement(outroNs + "gTotalSN"),
                new XElement(ns + "grupoDesconhecido"));
            total.Add(new XElement(ns + "campoDesconhecido"));

            var modelo = XMLUtility.Deserializar<EventoNFCom>(xml.ToString());
            var detalhe = Assert.IsType<DetEventoTributacaoSN>(modelo.InfEvento.DetEvento);
            Assert.Empty(detalhe.GTribItemSN);
            Assert.Equal(100, detalhe.GTotalSN.VRBSNTot);
        }

        [Fact]
        [Trait("DFe", "NFCom")]
        public void LeituraSemTotalContinuaFalhando()
        {
            var xml = XDocument.Load(@"..\..\..\NFCom\Resources\eventoNFCom-412140.xml");
            XNamespace ns = Ns;
            xml.Descendants(ns + "gTotalSN").Remove();

            using var reader = xml.CreateReader();
            var serializer = new XmlSerializer(typeof(EventoNFCom));
            var erro = Assert.Throws<InvalidOperationException>(() => serializer.Deserialize(reader));
            Assert.IsType<NullReferenceException>(erro.GetBaseException());
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(false)]
        [InlineData(true)]
        public void EscritaOmiteGruposAusentes(bool listaVazia)
        {
            var detalhe = new DetEventoTributacaoSN
            {
                VersaoEvento = "1.00",
                NProt = "1412600000000001",
                GTribItemSN = listaVazia ? new List<GTribItemSN>() : null
            };
            var modelo = new EventoNFCom
            {
                Versao = "1.00",
                InfEvento = new InfEvento(detalhe) { TpEvento = TipoEventoNFCom.TributacaoSimplesNacional, TpAmb = TipoAmbiente.Homologacao }
            };

            XNamespace ns = Ns;
            var evento = XElement.Parse(Extrair(modelo.GerarXML(), "evTributacaoSN").OuterXml);
            Assert.Equal(new[] { ns + "descEvento", ns + "nProt" }, evento.Elements().Select(e => e.Name));
        }
    }
}
