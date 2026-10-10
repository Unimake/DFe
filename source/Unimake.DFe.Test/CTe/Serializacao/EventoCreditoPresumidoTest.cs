using System;
using System.IO;
using System.Linq;
using System.Xml;
using System.Xml.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    public class EventoCreditoPresumidoTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";
        private const string ArquivoModelo = @"..\..\..\CTe\Resources\eventoCTe_211110.xml";

        private static void Validar(XmlDocument xml)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.eventoCTe_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
            var detalhe = new XmlDocument();
            detalhe.LoadXml(xml.GetElementsByTagName("evApropriaCredPres", NamespaceCTe)[0].OuterXml);
            validador.Validar(detalhe, "CTe.evApropriaCredPres_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        [Trait("DFe", "CTe")]
        public void ModeloCompletoDesserializaESerializaTodosOsCampos()
        {
            var original = new XmlDocument();
            original.Load(ArquivoModelo);
            var evento = new EventoCTe().LoadFromFile(ArquivoModelo);
            Assert.Equal(TipoEventoCTe.ApropriacaoCreditoPresumido, evento.InfEvento.TpEvento);
            var detalhe = Assert.IsType<DetEventoApropriacaoCreditoPresumido>(evento.InfEvento.DetEvento);
            Assert.Equal("4.00", detalhe.VersaoEvento);
            Assert.Equal(1500d, detalhe.VBCCredPres);
            Assert.Equal("01", detalhe.CCredPres);
            Assert.Equal(1.2345d, detalhe.GIBSCredPres.PCredPres);
            Assert.Equal(18.52d, detalhe.GIBSCredPres.VCredPres);
            Assert.Equal(2.5d, detalhe.GCBSCredPres.PCredPres);
            Assert.Equal(37.5d, detalhe.GCBSCredPres.VCredPres);
            Assert.Equal(DeclaracaoPagamentoCreditoPresumidoCTe.ComAcentuacao, detalhe.XDecPag);
            var gerado = evento.GerarXML();
            Assert.Equal(original.InnerText, gerado.InnerText);
            var esperado = XElement.Parse(original.OuterXml);
            var resultado = XElement.Parse(gerado.OuterXml);
            foreach (var atributo in esperado.DescendantsAndSelf().Attributes().Concat(resultado.DescendantsAndSelf().Attributes())
                .Where(x => x.IsNamespaceDeclaration).ToList())
            {
                atributo.Remove();
            }
            Assert.True(XNode.DeepEquals(esperado, resultado), gerado.OuterXml);
            var grupo = gerado.GetElementsByTagName("evApropriaCredPres", NamespaceCTe)[0];
            Assert.Equal(new[] { "descEvento", "vBCCredPres", "cCredPres", "gIBSCredPres", "gCBSCredPres", "xDecPag" },
                grupo.ChildNodes.Cast<XmlNode>().Select(x => x.LocalName).ToArray());
            Assert.All(grupo.ChildNodes.Cast<XmlNode>(), x => Assert.Equal(NamespaceCTe, x.NamespaceURI));
            Assert.IsType<DetEventoApropriacaoCreditoPresumido>(new EventoCTe().LerXML<EventoCTe>(gerado).InfEvento.DetEvento);
            Validar(gerado);
        }

        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(true, true, DeclaracaoPagamentoCreditoPresumidoCTe.ComAcentuacao)]
        [InlineData(true, false, DeclaracaoPagamentoCreditoPresumidoCTe.SemAcentuacao)]
        [InlineData(false, true, DeclaracaoPagamentoCreditoPresumidoCTe.ComAcentuacao)]
        [InlineData(false, false, DeclaracaoPagamentoCreditoPresumidoCTe.SemAcentuacao)]
        public void CriacaoPorObjetoPreservaGruposOpcionaisEDeclaracao(bool ibs, bool cbs, DeclaracaoPagamentoCreditoPresumidoCTe declaracao)
        {
            var assinatura = new EventoCTe().LoadFromFile(ArquivoModelo).Signature;
            var detalhe = new DetEventoApropriacaoCreditoPresumido
            {
                VersaoEvento = "4.00",
                VBCCredPres = 1500,
                CCredPres = "01",
                GIBSCredPres = ibs ? new GCredPresEvento { PCredPres = 1.2345, VCredPres = 18.52 } : null,
                GCBSCredPres = cbs ? new GCredPresEvento { PCredPres = 2.5, VCredPres = 37.5 } : null,
                XDecPag = declaracao
            };
            var evento = new EventoCTe
            {
                Versao = "4.00",
                Signature = assinatura,
                InfEvento = new InfEvento
                {
                    COrgao = UFBrasil.PR,
                    TpAmb = TipoAmbiente.Homologacao,
                    CNPJ = "00000000000000",
                    ChCTe = "41261000000000000000570010000000011000000000",
                    DhEvento = new DateTimeOffset(2026, 10, 10, 10, 0, 0, TimeSpan.FromHours(-3)),
                    TpEvento = TipoEventoCTe.ApropriacaoCreditoPresumido,
                    NSeqEvento = 1,
                    DetEvento = detalhe
                }
            };
            Assert.Same(detalhe, evento.InfEvento.DetEvento);
            var xml = evento.GerarXML();
            Assert.Equal(ibs ? 1 : 0, xml.GetElementsByTagName("gIBSCredPres").Count);
            Assert.Equal(cbs ? 1 : 0, xml.GetElementsByTagName("gCBSCredPres").Count);
            var lido = Assert.IsType<DetEventoApropriacaoCreditoPresumido>(XMLUtility.Deserializar<EventoCTe>(xml).InfEvento.DetEvento);
            Assert.Equal(declaracao, lido.XDecPag);
            Assert.Equal(ibs, lido.GIBSCredPres != null);
            Assert.Equal(cbs, lido.GCBSCredPres != null);
            Assert.Equal(xml.InnerText, XMLUtility.Deserializar<EventoCTe>(xml).GerarXML().InnerText);
            Validar(xml);
        }

        [Fact]
        [Trait("DFe", "CTe")]
        public void ValidacaoCentralEncontraSchemaEspecificoDoEvento()
        {
            var assembly = typeof(ValidarSchema).Assembly;
            var recurso = assembly.GetManifestResourceNames().Single(x => x.EndsWith(".ValidarConfig.xml"));
            using (var stream = assembly.GetManifestResourceStream(recurso))
            {
                var config = new XmlDocument();
                config.Load(stream);
                var tipos = config.SelectNodes("//Servico[@tagRaiz='eventoCTe'][@versao='4.00']/SchemasEspecificos/Tipo[ID='211110']");
                Assert.Single(tipos.Cast<XmlNode>());
                Assert.Equal("eventoCTe_v4.00.xsd", tipos[0]["SchemaArquivo"].InnerText);
                Assert.Equal("evApropriaCredPres_v4.00.xsd", tipos[0]["SchemaArquivoEspecifico"].InnerText);
            }
        }
    }
}
