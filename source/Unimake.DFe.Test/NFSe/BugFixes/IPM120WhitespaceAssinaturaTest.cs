using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.BugFixes
{
    public class IPM120WhitespaceAssinaturaTest
    {
        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        [Trait("DFe", "NFSe")]
        public void DeveConstruirEmissaoFormatada(bool comRtc)
        {
            var rtc = comRtc ? "<valores_ibs_cbs><valor_ibs>0,01</valor_ibs></valores_ibs_cbs>" : string.Empty;
            var xml = CarregarXml(
                "<nfse>\r\n" +
                "  <!-- XML IPM 1.20 sintético -->\r\n" +
                "  <nfse_teste>1</nfse_teste>\r\n" +
                "  <nf><serie_nfse>1</serie_nfse>" + rtc + "</nf>\r\n" +
                "</nfse>");

            using (var certificado = CriarCertificado())
            using (var servico = new GerarNfse(xml, CriarConfiguracao(certificado, Servico.NFSeGerarNfse)))
            {
                AssertAssinaturaValida(servico.ConteudoXMLAssinado, certificado);
                Assert.Equal(PadraoNFSe.IPM, servico.Configuracoes.PadraoNFSe);
                Assert.Equal("1.20", servico.Configuracoes.SchemaVersao);
            }
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void DeveConstruirCancelamentoFormatado()
        {
            var xml = CarregarXml(
                "<nfse>\r\n" +
                "  <!-- XML IPM 1.20 sintético -->\r\n" +
                "  <nf><numero>1</numero><serie_nfse>1</serie_nfse><situacao>C</situacao></nf>\r\n" +
                "</nfse>");

            using (var certificado = CriarCertificado())
            using (var servico = new CancelarNfse(xml, CriarConfiguracao(certificado, Servico.NFSeCancelarNfse)))
            {
                AssertAssinaturaValida(servico.ConteudoXMLAssinado, certificado);
                Assert.Equal(PadraoNFSe.IPM, servico.Configuracoes.PadraoNFSe);
                Assert.Equal("1.20", servico.Configuracoes.SchemaVersao);
            }
        }

        private static Configuracao CriarConfiguracao(X509Certificate2 certificado, Servico servico) => new Configuracao
        {
            TipoDFe = TipoDFe.NFSe,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CodigoMunicipio = 4118402,
            Servico = servico,
            SchemaVersao = "1.20",
            CertificadoDigital = certificado,
            MunicipioUsuario = "USUARIO-SINTETICO",
            MunicipioSenha = "SENHA-SINTETICA"
        };

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
                    "CN=IPM120WhitespaceAssinaturaTest",
                    rsa,
                    HashAlgorithmName.SHA256,
                    RSASignaturePadding.Pkcs1);

                return request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private static void AssertAssinaturaValida(XmlDocument xml, X509Certificate2 certificado)
        {
            var assinatura = xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0] as XmlElement;
            Assert.NotNull(assinatura);

            var signedXml = new SignedXml(xml);
            signedXml.LoadXml(assinatura);
            Assert.True(signedXml.CheckSignature(certificado, true));
        }
    }
}
