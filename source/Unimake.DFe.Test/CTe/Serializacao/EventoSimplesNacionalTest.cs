using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    public class EventoSimplesNacionalTest
    {
        private const string NS = "http://www.portalfiscal.inf.br/cte";
        private static string Caminho(string codigo) => @"..\..\..\CTe\Resources\eventoCTe_" + codigo + ".xml";

        private static void Validar(XmlDocument xml, string raiz, string schema)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.eventoCTe_v4.00.xsd", NS);
            Assert.True(validador.Success, validador.ErrorMessage);
            var detalhe = new XmlDocument();
            detalhe.LoadXml(xml.GetElementsByTagName(raiz, NS)[0].OuterXml);
            validador.Validar(detalhe, "CTe." + schema + "_v1.00.xsd", NS);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData("412140", "evTributacaoSN", "evTributacaoSN")]
        [InlineData("412141", "evCancTribSN", "evCancTributacaoSN")]
        public void ModelosPreservamTodosOsCamposEValidamSchema(string codigo, string raiz, string schema)
        {
            var original = new XmlDocument();
            original.Load(Caminho(codigo));
            var modelo = new EventoCTe().LoadFromFile(Caminho(codigo));
            Assert.Equal(codigo, ((int)modelo.InfEvento.TpEvento).ToString());
            Assert.Equal("4.00", modelo.InfEvento.DetEvento.VersaoEvento);
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
                Assert.Equal("141260000000002", detalhe.NProtEvTribSN);
            }
            var xml = modelo.GerarXML();
            Assert.Equal(original.InnerText, xml.InnerText);
            Assert.NotNull(modelo.Signature);
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<EventoCTe>(xml).GerarXML().InnerText);
            Validar(xml, raiz, schema);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(true)]
        [InlineData(false)]
        public void CriacaoPorObjetoPreservaListaZeroEOmissoes(bool acentos)
        {
            var modelo = new EventoCTe().LoadFromFile(Caminho("412140"));
            var detalhe = new DetEventoTributacaoSN
            {
                VersaoEvento = "4.00",
                DescEvento = acentos ? "Tributação do Simples Nacional" : "Tributacao do Simples Nacional",
                NProt = "141260000000001",
                GTribItemSN = new List<GTribItemSN>
                {
                    new GTribItemSN { NItem = "1", VRBSNItem = 100, TpRBSN = TipoReceitaBrutaSimplesNacional.ReceitaBrutaInterna, PIBSSN = 0, VIBSSN = 0 },
                    new GTribItemSN { NItem = "2", VRBSNItem = 0, TpRBSN = TipoReceitaBrutaSimplesNacional.NaoReceitaBruta }
                },
                GTotalSN = new GTotalSN { VRBSNTot = 100, VIBSSNTot = 0 }
            };
            modelo.InfEvento.DetEvento = detalhe;
            Assert.Same(detalhe, modelo.InfEvento.DetEvento);
#if INTEROP
            Assert.Equal(2, detalhe.GetGTribItemSNCount);
            detalhe.AddGTribItemSN(new GTribItemSN { NItem = "3", VRBSNItem = 0 });
            Assert.Equal("3", detalhe.GetGTribItemSN(2).NItem);
            Assert.Equal(3, detalhe.GetGTribItemSNCount);
#endif
            var xml = modelo.GerarXML();
            Assert.Equal(1, xml.GetElementsByTagName("pIBSSN", NS).Count);
            Assert.Equal("0.00", xml.GetElementsByTagName("pIBSSN", NS)[0].InnerText);
            Assert.Equal(0, xml.GetElementsByTagName("vCBSSNTot", NS).Count);
            var grupo = xml.GetElementsByTagName("evTributacaoSN", NS)[0];
            Assert.Equal(new[] { "descEvento", "nProt" }, grupo.ChildNodes.Cast<XmlNode>().Take(2).Select(n => n.LocalName));
            Assert.Equal("gTotalSN", grupo.LastChild.LocalName);
            Assert.All(grupo.ChildNodes.Cast<XmlNode>(), n => Assert.Equal(NS, n.NamespaceURI));
            var item = xml.GetElementsByTagName("gTribItemSN", NS)[0];
            Assert.Equal(new[] { "vRBSNItem", "tpRBSN", "pIBSSN", "vIBSSN" }, item.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<EventoCTe>(xml).GerarXML().InnerText);
            Validar(xml, "evTributacaoSN", "evTributacaoSN");
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData("Cancelamento da Tributação do Simples Nacional")]
        [InlineData("Cancelamento da Tributacao do Simples Nacional")]
        public void CancelamentoCriadoPorObjetoPreservaProtocolos(string descricao)
        {
            var modelo = new EventoCTe().LoadFromFile(Caminho("412141"));
            var detalhe = new DetEventoCancTributacaoSN
            {
                VersaoEvento = "4.00", DescEvento = descricao,
                NProt = "141260000000001", NProtEvTribSN = "141260000000002"
            };
            modelo.InfEvento.DetEvento = detalhe;
            Assert.Same(detalhe, modelo.InfEvento.DetEvento);
            var xml = modelo.GerarXML();
            var grupo = xml.GetElementsByTagName("evCancTribSN", NS)[0];
            Assert.Equal(new[] { "descEvento", "nProt", "nProtEvTribSN" }, grupo.ChildNodes.Cast<XmlNode>().Select(n => n.LocalName));
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<EventoCTe>(xml).GerarXML().InnerText);
            Validar(xml, "evCancTribSN", "evCancTributacaoSN");
        }

        [Fact]
        [Trait("DFe", "CTe")]
        public void ConfiguracaoCentralResolveOsDoisSchemas()
        {
            var assembly = typeof(ValidarSchema).Assembly;
            var nome = assembly.GetManifestResourceNames().Single(n => n.EndsWith(".ValidarConfig.xml"));
            using (var stream = assembly.GetManifestResourceStream(nome))
            {
                var config = new XmlDocument();
                config.Load(stream);
                foreach (var codigo in new[] { "412140", "412141" })
                {
                    var tipo = config.SelectNodes("//Tipo[ID='" + codigo + "']").Cast<XmlNode>()
                        .Single(n => n["SchemaArquivo"]?.InnerText == "eventoCTe_v4.00.xsd");
                    Assert.Equal(codigo == "412140" ? "evTributacaoSN_v1.00.xsd" : "evCancTributacaoSN_v1.00.xsd", tipo["SchemaArquivoEspecifico"].InnerText);
                }
            }
        }
    }
}
