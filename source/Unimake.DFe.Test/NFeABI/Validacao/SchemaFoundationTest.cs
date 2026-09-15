using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.NFeABI.Validacao
{
    public class SchemaFoundationTest
    {
        private const string NamespaceNFeABI = "http://www.portalfiscal.inf.br/nfeabi";

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ContratosPublicosRegistramTipoModeloEServicosSemRenumerarExistentes()
        {
            Assert.Equal(23, (int)TipoDFe.NFeABI);
            Assert.Equal(77, (int)ModeloDFe.NFeABI);
            Assert.Equal("77", ObterXmlEnum(ModeloDFe.NFeABI));
            Assert.Equal(134, (int)Servico.NFeABIStatusServico);
            Assert.Equal(135, (int)Servico.NFeABIConsultaProtocolo);
            Assert.Equal(136, (int)Servico.NFeABIRecepcaoEvento);
            Assert.Equal(137, (int)Servico.NFeABIAutorizacaoSinc);
            Assert.Equal(110, (int)TipoXML.NFeABIStatusServico);
            Assert.Equal(111, (int)TipoXML.NFeABIConsultaSituacao);
            Assert.Equal(112, (int)TipoXML.NFeABIEnvioEvento);
            Assert.Equal(113, (int)TipoXML.NFeABI);
            Assert.Equal(133, (int)Servico.CIOTObterOperacaoTransportePdf);
            Assert.Equal(22, (int)TipoDFe.BPe);
            Assert.Equal(109, (int)TipoXML.BPeTA);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void PacoteOficialPossuiVinteSchemasEmbutidos()
        {
            var schemas = typeof(ValidarSchema).Assembly.GetManifestResourceNames()
                .Where(x => x.Contains(".Xml.Schemas.NFeABI.") && x.EndsWith(".xsd", StringComparison.OrdinalIgnoreCase))
                .ToArray();

            Assert.Equal(20, schemas.Length);
            Assert.Contains(schemas, x => x.EndsWith(".NFeABI_v1.00.xsd", StringComparison.Ordinal));
            Assert.Contains(schemas, x => x.EndsWith(".eventoNFeABI_v1.00.xsd", StringComparison.Ordinal));
            Assert.Contains(schemas, x => x.EndsWith(".xmldsig-core-schema_v1.01.xsd", StringComparison.Ordinal));
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void RaizPrincipalComModelo77ValidaNoSchemaOficial()
        {
            var documento = Carregar("NFeABI-minima.xml");

            AssertSchema(documento, "NFeABI.NFeABI_v1.00.xsd", true);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ConfiguracaoCentralDetectaEValidaConsultaStatus()
        {
            var documento = Carregar("consStatServNFeABI-valido.xml");
            var configuracao = new Configuracao
            {
                CodigoUF = 41,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(documento, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Contains("alienação de bens imóveis", resultado.Descricao);
        }

        [Theory]
        [InlineData("consStatServNFeABI-valido.xml", TipoXML.NFeABIStatusServico)]
        [InlineData("consSitNFeABI-valido.xml", TipoXML.NFeABIConsultaSituacao)]
        [InlineData("eventoNFeABI-cancelamento-valido.xml", TipoXML.NFeABIEnvioEvento)]
        [InlineData("NFeABI-minima.xml", TipoXML.NFeABI)]
        [Trait("DFe", "NFeABI")]
        public void DetectorPublicoReconheceRaizesNFeABI(string arquivo, TipoXML esperado)
        {
            Assert.Equal(esperado, XMLUtility.DetectXMLType(Carregar(arquivo)));
        }

        [Theory]
        [InlineData("consSitNFeABI-valido.xml")]
        [InlineData("eventoNFeABI-cancelamento-valido.xml")]
        [Trait("DFe", "NFeABI")]
        public void ConfiguracaoCentralValidaDemaisRaizesDeEntrada(string arquivo)
        {
            var configuracao = new Configuracao
            {
                CodigoUF = 41,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(Carregar(arquivo), configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void ConfiguracaoCentralRejeitaNamespaceIncorretoNoDetalheDoEvento()
        {
            var documento = Carregar("eventoNFeABI-cancelamento-valido.xml");
            var detalhe = (XmlElement)documento.GetElementsByTagName("evCancNFeABI", NamespaceNFeABI)[0];
            detalhe.SetAttribute("xmlns", "urn:nfeabi:namespace-incorreto");
            var documentoAlterado = new XmlDocument();
            documentoAlterado.LoadXml(documento.OuterXml);
            var configuracao = new Configuracao
            {
                CodigoUF = 41,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(documentoAlterado, configuracao);

            Assert.False(resultado.Validado);
            Assert.Contains("namespace", resultado.MensagemRetorno);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SchemaRejeitaNamespaceIncorreto()
        {
            var original = Carregar("consStatServNFeABI-valido.xml");
            var documento = new XmlDocument();
            documento.LoadXml(original.OuterXml.Replace(NamespaceNFeABI, "http://www.portalfiscal.inf.br/nfe"));
            var configuracao = new Configuracao
            {
                CodigoUF = 41,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(documento, configuracao);

            Assert.False(resultado.Validado);
            Assert.Contains("namespace", resultado.MensagemRetorno);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SchemaRejeitaModeloDiferenteDe77()
        {
            var documento = Carregar("NFeABI-minima.xml");
            documento.GetElementsByTagName("mod", NamespaceNFeABI)[0].InnerText = "55";

            AssertSchema(documento, "NFeABI.NFeABI_v1.00.xsd", false);
        }

        [Fact]
        [Trait("DFe", "NFeABI")]
        public void SchemaRejeitaCampoObrigatorioAusente()
        {
            var documento = Carregar("consStatServNFeABI-valido.xml");
            var xServ = documento.GetElementsByTagName("xServ", NamespaceNFeABI)[0];
            xServ.ParentNode.RemoveChild(xServ);

            AssertSchema(documento, "NFeABI.consStatServNFeABI_v1.00.xsd", false);
        }

        private static XmlDocument Carregar(string arquivo)
        {
            var documento = new XmlDocument();
            documento.Load(Path.Combine(@"..\..\..\NFeABI\Resources", arquivo));
            return documento;
        }

        private static void AssertSchema(XmlDocument documento, string schema, bool esperado)
        {
            var validador = new ValidarSchema();
            validador.Validar(documento, schema, NamespaceNFeABI);
            Assert.Equal(esperado, validador.Success);
        }

        private static string ObterXmlEnum(ModeloDFe modelo)
        {
            var membro = typeof(ModeloDFe).GetMember(modelo.ToString()).Single();
            return membro.GetCustomAttribute<XmlEnumAttribute>().Name;
        }
    }
}
