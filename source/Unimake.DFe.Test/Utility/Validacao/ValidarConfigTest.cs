using System;
using System.Linq;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class ValidarConfigTest
    {
        private const string NomeRecurso = "Unimake.Business.DFe.Xml.Validar.ValidarConfig.xml";

        [Fact]
        public void DeveIncorporarECarregarCatalogoDeValidacao()
        {
            var assembly = typeof(ValidarEstruturaXML).Assembly;

            Assert.Contains(NomeRecurso, assembly.GetManifestResourceNames());

            var catalogo = CarregarCatalogo();

            Assert.Equal("ServicosValidacao", catalogo.DocumentElement.Name);
            Assert.NotEmpty(catalogo.SelectNodes("/ServicosValidacao/*").Cast<XmlNode>());
            Assert.NotEmpty(catalogo.SelectNodes("//Servico").Cast<XmlNode>());
        }

        [Fact]
        public void DeveManterServicosVersionadosENaoVersionados()
        {
            var catalogo = CarregarCatalogo();

            Assert.NotEmpty(catalogo.SelectNodes("//Servico[string-length(normalize-space(@versao)) > 0]").Cast<XmlNode>());
            Assert.NotEmpty(catalogo.SelectNodes("//Servico[not(@versao) or string-length(normalize-space(@versao)) = 0]").Cast<XmlNode>());
        }

        [Fact]
        public void DeveConterSomenteAlgoritmosDeAssinaturaConhecidos()
        {
            var catalogo = CarregarCatalogo();
            var valores = catalogo
                .SelectNodes("//*[local-name()='SignatureAlgorithmType'][not(*)]")
                .Cast<XmlNode>()
                .Select(x => x.InnerText?.Trim())
                .Where(x => !string.IsNullOrWhiteSpace(x));

            foreach (var valor in valores)
            {
                Assert.True(
                    Enum.TryParse(valor, true, out AlgorithmType algoritmo) &&
                    Enum.IsDefined(typeof(AlgorithmType), algoritmo),
                    $"SignatureAlgorithmType inválido no catálogo: {valor}."
                );
            }
        }

        [Fact]
        public void DeveResolverServicoSemVersaoComoFallback()
        {
            var catalogo = new XmlDocument();
            catalogo.LoadXml(
                "<ServicosValidacao>" +
                "<DARE>" +
                "<Servico tagRaiz=\"Dare\" versao=\"\"><Descricao>DARE</Descricao></Servico>" +
                "</DARE>" +
                "</ServicosValidacao>"
            );

            var metodo = typeof(ValidarEstruturaXML).GetMethod(
                "TratarDFe",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            var xml = new XmlDocument();
            xml.LoadXml("<Dare versao=\"1.00\" />");

            var servico = (XmlNode)metodo.Invoke(
                null,
                new object[] { xml, "1.00", TipoDFe.DARE, "Dare", catalogo }
            );

            Assert.NotNull(servico);
            Assert.Equal(string.Empty, servico.Attributes["versao"].Value);
        }

        [Fact]
        public void DeveResolverESocialPelaTagIdentificadora()
        {
            var catalogo = new XmlDocument();
            catalogo.LoadXml(
                "<ServicosValidacao><ESocial>" +
                "<Servico tagIdentificadora=\"consulta\"><Descricao>Consulta</Descricao></Servico>" +
                "<Servico tagIdentificadora=\"envioLoteEventos\"><Descricao>Envio</Descricao></Servico>" +
                "</ESocial></ServicosValidacao>"
            );
            var xml = new XmlDocument();
            xml.LoadXml("<eSocial><envioLoteEventos /></eSocial>");

            var servico = InvocarResolvedor(
                "TratarESocialEFDReinf",
                xml,
                string.Empty,
                TipoDFe.ESocial,
                "eSocial",
                catalogo
            );

            Assert.Equal("Envio", servico.SelectSingleNode("Descricao").InnerText);
        }

        [Fact]
        public void DevePreservarAliasDeCaixaDoStatusServicoCTe()
        {
            var catalogo = new XmlDocument();
            catalogo.LoadXml(
                "<ServicosValidacao><CTe>" +
                "<Servico tagRaiz=\"consStatServCTe\" versao=\"4.00\"><Descricao>Status</Descricao></Servico>" +
                "</CTe></ServicosValidacao>"
            );
            var xml = new XmlDocument();
            xml.LoadXml("<consStatServCte versao=\"4.00\" />");

            var servico = InvocarResolvedor(
                "TratarDFe",
                xml,
                "4.00",
                TipoDFe.CTe,
                "consStatServCte",
                catalogo
            );

            Assert.Equal("Status", servico.SelectSingleNode("Descricao").InnerText);
        }

        private static XmlDocument CarregarCatalogo()
        {
            var metodo = typeof(ValidarEstruturaXML).GetMethod(
                "CarregarConfigValidacao",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            Assert.NotNull(metodo);
            return (XmlDocument)metodo.Invoke(null, null);
        }

        private static XmlNode InvocarResolvedor(string nomeMetodo, params object[] parametros)
        {
            var metodo = typeof(ValidarEstruturaXML).GetMethod(
                nomeMetodo,
                BindingFlags.NonPublic | BindingFlags.Static
            );

            Assert.NotNull(metodo);
            return (XmlNode)metodo.Invoke(null, parametros);
        }
    }
}
