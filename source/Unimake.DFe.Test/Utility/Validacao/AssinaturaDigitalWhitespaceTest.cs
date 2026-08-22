using System;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe.Security;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class AssinaturaDigitalWhitespaceTest
    {
        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        [Trait("Utility", "Validacao")]
        public void DeveAssinarQuandoTagAssinaturaETagAtributoIdSaoIguais(bool formatado)
        {
            var xml = CarregarXml(formatado
                ? "<nfse>\r\n  <!-- comentário -->\r\n  <nf />\r\n</nfse>"
                : "<nfse><nf /></nfse>");
            var quantidadeWhitespaceAntes = xml.SelectNodes("//text()[normalize-space(.)='']").Count;

            using (var certificado = CriarCertificado())
            {
                AssinaturaDigital.Assinar(xml, "nfse", "nfse", certificado);

                ValidarAssinatura(xml, certificado, string.Empty);
            }

            Assert.Equal(1, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            Assert.Equal(quantidadeWhitespaceAntes, xml.SelectNodes("//text()[normalize-space(.)='']").Count);
            Assert.Equal(formatado ? 1 : 0, xml.SelectNodes("//comment()").Count);
        }

        [Fact]
        [Trait("Utility", "Validacao")]
        public void DeveIgnorarWhitespaceEComentarioAoLocalizarTagAtributoId()
        {
            var xml = CarregarXml("<root>\r\n  <!-- comentário -->\r\n  <inf Id=\"ID1\" />\r\n</root>");

            using (var certificado = CriarCertificado())
            {
                AssinaturaDigital.Assinar(xml, "root", "inf", certificado);

                ValidarAssinatura(xml, certificado, "#ID1");
            }

            Assert.Equal(1, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            Assert.Equal(1, xml.SelectNodes("//comment()").Count);
        }

        [Theory]
        [InlineData("GIF", "ConsultarNfseServicoPrestadoEnvio")]
        [InlineData("QUASAR", "ConsultarDps")]
        [InlineData("QUASAR", "ConsultarStatusDps")]
        [Trait("Utility", "Validacao")]
        public void DevePreservarPadroesNFSeComTagAssinaturaIgualAoAtributoId(
            string padrao,
            string tag)
        {
            var xml = CarregarXml($"<{tag}>\r\n  <!-- {padrao} -->\r\n  <conteudo />\r\n</{tag}>");

            using (var certificado = CriarCertificado())
            {
                AssinaturaDigital.Assinar(xml, tag, tag, certificado);

                ValidarAssinatura(xml, certificado, string.Empty);
            }

            Assert.Equal(1, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            Assert.Equal(1, xml.SelectNodes("//comment()").Count);
        }

        [Theory]
        [InlineData("NFe", "infNFe")]
        [InlineData("CTe", "infCte")]
        [InlineData("MDFe", "infMDFe")]
        [InlineData("NFCom", "infNFCom")]
        [Trait("Utility", "Validacao")]
        public void DevePreservarDocumentosFiscaisComTagsDeAssinaturaDistintas(
            string tagAssinatura,
            string tagAtributoId)
        {
            var xml = CarregarXml(
                $"<{tagAssinatura}>\r\n  <!-- documento fiscal -->\r\n  <{tagAtributoId} Id=\"ID1\" />\r\n</{tagAssinatura}>");

            using (var certificado = CriarCertificado())
            {
                AssinaturaDigital.Assinar(xml, tagAssinatura, tagAtributoId, certificado);

                ValidarAssinatura(xml, certificado, "#ID1");
            }

            Assert.Equal(1, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            Assert.Equal(1, xml.SelectNodes("//comment()").Count);
        }

        [Fact]
        [Trait("Utility", "Validacao")]
        public void DeveIgnorarTagAssinaturaSemFilhoAlvoQuandoTagsSaoDistintas()
        {
            var xml = CarregarXml(
                "<container>\r\n" +
                "  <Rps><InfDeclaracaoPrestacaoServico Id=\"ID1\" /></Rps>\r\n" +
                "  <Rps Id=\"RPS1\"><IdentificacaoRps /></Rps>\r\n" +
                "</container>");

            using (var certificado = CriarCertificado())
            {
                AssinaturaDigital.Assinar(xml, "Rps", "InfDeclaracaoPrestacaoServico", certificado);

                ValidarAssinatura(xml, certificado, "#ID1");
            }

            Assert.Equal(1, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
        }

        [Fact]
        [Trait("Utility", "Validacao")]
        public void DeveFalharComMensagemClaraQuandoNaoExisteElementoAssinavel()
        {
            var xml = CarregarXml("<nfse>texto sem elemento filho</nfse>");

            using (var certificado = CriarCertificado())
            {
                var excecao = Assert.Throws<Exception>(() =>
                    AssinaturaDigital.Assinar(xml, "nfse", "nfse", certificado));

                Assert.Contains("elemento XML válido", excecao.Message);
            }
        }

        private static XmlDocument CarregarXml(string conteudo)
        {
            var xml = new XmlDocument
            {
                PreserveWhitespace = true,
                XmlResolver = null
            };
            xml.LoadXml(conteudo);
            return xml;
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest(
                    "CN=AssinaturaDigitalWhitespaceTest",
                    rsa,
                    HashAlgorithmName.SHA256,
                    RSASignaturePadding.Pkcs1);

                return request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private static void ValidarAssinatura(XmlDocument xml, X509Certificate2 certificado, string uriEsperada)
        {
            var assinatura = xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0] as XmlElement;
            Assert.NotNull(assinatura);

            var referencia = assinatura
                .SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='Reference']") as XmlElement;
            var metodoAssinatura = assinatura
                .SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='SignatureMethod']") as XmlElement;
            var metodoCanonicalizacao = assinatura
                .SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='CanonicalizationMethod']") as XmlElement;

            Assert.NotNull(referencia);
            Assert.NotNull(metodoAssinatura);
            Assert.NotNull(metodoCanonicalizacao);
            Assert.Equal(uriEsperada, referencia.GetAttribute("URI"));
            Assert.Equal(SignedXml.XmlDsigRSASHA1Url, metodoAssinatura.GetAttribute("Algorithm"));
            Assert.Equal(SignedXml.XmlDsigC14NTransformUrl, metodoCanonicalizacao.GetAttribute("Algorithm"));

            var signedXml = new SignedXml(xml);
            signedXml.LoadXml(assinatura);
            Assert.True(signedXml.CheckSignature(certificado, true));
        }
    }
}
