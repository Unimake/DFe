using System;
using System.Linq;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.Validar;
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
        public void TodaGeracaoQRCode_DeveDeclararSuplementoEPoliticaDePreparacao()
        {
            var catalogo = CarregarCatalogo();
            var servicos = catalogo.SelectNodes("/ServicosValidacao/*/Servico[GerarQrCode='true']").Cast<XmlNode>().ToList();
            Assert.NotEmpty(servicos);
            foreach (var servico in servicos)
            {
                var grupoUrl = servico.SelectSingleNode("GrupoUrl");
                Assert.NotNull(grupoUrl);
                var tagSuplementar = grupoUrl.Attributes["tagSuplementar"]?.Value;
                Assert.False(string.IsNullOrWhiteSpace(tagSuplementar), servico.OuterXml);
                Assert.Equal(tagSuplementar, XmlConvert.VerifyNCName(tagSuplementar));
                Assert.Contains(grupoUrl.Attributes["prepararComSuplemento"]?.Value, new[] { "true", "false" });
            }
        }

        [Fact]
        public void ConfiguracaoNaoDeveExporEstruturasLegadasDeValidacao()
        {
            var tipoConfiguracao = typeof(Configuracao);
            var assembly = tipoConfiguracao.Assembly;

            Assert.Null(tipoConfiguracao.GetField("SchemasEspecificos"));
            Assert.Null(tipoConfiguracao.GetField("TiposEventosEspecificos"));
            Assert.Null(tipoConfiguracao.GetProperty("TagAssinatura"));
            Assert.Null(tipoConfiguracao.GetProperty("TagAtributoID"));
            Assert.Null(tipoConfiguracao.GetProperty("TagLoteAssinatura"));
            Assert.Null(tipoConfiguracao.GetProperty("TagLoteAtributoID"));
            Assert.Null(tipoConfiguracao.GetProperty("SchemaVersaoEvento"));
            Assert.Null(assembly.GetType("Unimake.Business.DFe.Servicos.SchemaEspecifico"));
            Assert.Null(assembly.GetType("Unimake.Business.DFe.Servicos.TiposEventosEspecificos"));
        }

        [Fact]
        public void ConfiguracaoDevePreservarCamposAindaUtilizadosPorCodigo()
        {
            var configuracao = new Configuracao
            {
                SchemaArquivo = "schema-programatico.xsd",
                TagExtraAssinatura = "SubstituirNfseEnvio",
                TagExtraAtributoID = "SubstituicaoNfse"
            };

            Assert.Equal("schema-programatico.xsd", configuracao.SchemaArquivo);
            Assert.Equal("SubstituirNfseEnvio", configuracao.TagExtraAssinatura);
            Assert.Equal("SubstituicaoNfse", configuracao.TagExtraAtributoID);
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

        [Theory]
        [InlineData("enviNFe")]
        [InlineData("NFe")]
        public void DeveConterUrlsQRCodeNFCeParaAlagoas(string tagRaiz)
        {
            var catalogo = CarregarCatalogo();
            var servico = catalogo.SelectSingleNode($"ServicosValidacao/NFCe/Servico[@tagRaiz='{tagRaiz}' and @versao='4.00']");
            var configuracao = new Configuracao();

            Assert.NotNull(servico);
            InvocarAtribuirUrl(servico, UFBrasil.AL, configuracao);

            Assert.Equal("www.sefaz.al.gov.br/nfce/consulta", configuracao.UrlChaveHomologacao);
            Assert.Equal("www.sefaz.al.gov.br/nfce/consulta", configuracao.UrlChaveProducao);
            Assert.Equal("http://nfce.sefaz.al.gov.br/QRCode/consultarNFCe.jsp", configuracao.UrlQrCodeHomologacao);
            Assert.Equal("http://nfce.sefaz.al.gov.br/QRCode/consultarNFCe.jsp", configuracao.UrlQrCodeProducao);
        }

        [Theory]
        [InlineData("enviNFe")]
        [InlineData("NFe")]
        public void DeveConterUrlsQRCodeNFCeParaAcre(string tagRaiz)
        {
            var catalogo = CarregarCatalogo();
            var servico = catalogo.SelectSingleNode($"ServicosValidacao/NFCe/Servico[@tagRaiz='{tagRaiz}' and @versao='4.00']");
            var configuracao = new Configuracao();

            Assert.NotNull(servico);
            InvocarAtribuirUrl(servico, UFBrasil.AC, configuracao);

            Assert.Equal("www.sefaznet.ac.gov.br/nfce/consulta", configuracao.UrlChaveHomologacao);
            Assert.Equal("www.sefaznet.ac.gov.br/nfce/consulta", configuracao.UrlChaveProducao);
            Assert.Equal("http://www.hml.sefaznet.ac.gov.br/nfce/qrcode", configuracao.UrlQrCodeHomologacao);
            Assert.Equal("http://www.sefaznet.ac.gov.br/nfce/qrcode", configuracao.UrlQrCodeProducao);
        }

        [Theory]
        [InlineData("enviNFe")]
        [InlineData("NFe")]
        public void ReferenciaDaNFe_DeveReutilizarUrlsDaNFCe(string tagRaiz)
        {
            var catalogo = CarregarCatalogo();
            var servico = catalogo.SelectSingleNode($"ServicosValidacao/NFe/Servico[@tagRaiz='{tagRaiz}' and @versao='4.00']");
            var origem = catalogo.SelectSingleNode($"ServicosValidacao/NFCe/Servico[@tagRaiz='{tagRaiz}' and @versao='4.00']");
            var configuracao = new Configuracao { CodigoUF = (int)UFBrasil.AC };
            var controle = new Configuracao { CodigoUF = (int)UFBrasil.AC };

            ResolvedorUrlsConsulta.Atribuir(null, servico, configuracao);
            ResolvedorUrlsConsulta.Atribuir(null, origem, controle);

            Assert.False(string.IsNullOrWhiteSpace(configuracao.UrlQrCodeHomologacao));
            Assert.Equal(controle.UrlQrCodeHomologacao, configuracao.UrlQrCodeHomologacao);
            Assert.Equal(controle.UrlQrCodeProducao, configuracao.UrlQrCodeProducao);
            Assert.Equal(controle.UrlChaveHomologacao, configuracao.UrlChaveHomologacao);
            Assert.Equal(controle.UrlChaveProducao, configuracao.UrlChaveProducao);
        }

        [Theory]
        [InlineData("")]
        [InlineData("<Servico tagRaiz=\"NFe\" versao=\"3.10\"><GrupoUrl/></Servico>")]
        public void ReferenciaAusenteOuDeOutraVersao_DeveFalharComContexto(string servicoOrigem)
        {
            var catalogo = new XmlDocument();
            catalogo.LoadXml("<ServicosValidacao><NFe><Servico tagRaiz=\"NFe\" versao=\"4.00\">"
                + "<GrupoUrl referenciaDFe=\"NFCe\"/></Servico></NFe><NFCe>" + servicoOrigem + "</NFCe></ServicosValidacao>");
            var servico = catalogo.SelectSingleNode("ServicosValidacao/NFe/Servico");

            var erro = Assert.Throws<InvalidOperationException>(() => ResolvedorUrlsConsulta.Atribuir(null, servico, new Configuracao()));

            Assert.Contains("NFe", erro.Message);
            Assert.Contains("4.00", erro.Message);
            Assert.Contains("NFCe", erro.Message);
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

        private static void InvocarAtribuirUrl(XmlNode servico, UFBrasil codigoUF, Configuracao configuracao)
        {
            configuracao.CodigoUF = (int)codigoUF;
            ResolvedorUrlsConsulta.Atribuir(null, servico, configuracao);
        }
    }
}
