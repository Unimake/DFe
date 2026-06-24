using System;
using System.Reflection;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.TesteValidacao
{
    public class SignatureAlgorithmTypeTest
    {
        [Fact]
        public void DeveUsarSha1QuandoConfiguracaoNaoExistir()
        {
            var servico = CriarServico("<Servico />");

            Assert.Equal(AlgorithmType.Sha1, ObterAlgoritmo(servico, 0));
        }

        [Fact]
        public void DeveLerConfiguracaoGlobalDoTipoDFe()
        {
            var servico = CriarServico(
                "<ESocial>" +
                "<SignatureAlgorithmType>Sha256</SignatureAlgorithmType>" +
                "<Servico />" +
                "</ESocial>"
            );

            Assert.Equal(AlgorithmType.Sha256, ObterAlgoritmo(servico, 0));
        }

        [Fact]
        public void DevePriorizarConfiguracaoSimplesDoServico()
        {
            var servico = CriarServico(
                "<ESocial>" +
                "<SignatureAlgorithmType>Sha256</SignatureAlgorithmType>" +
                "<Servico><SignatureAlgorithmType>Sha1</SignatureAlgorithmType></Servico>" +
                "</ESocial>"
            );

            Assert.Equal(AlgorithmType.Sha1, ObterAlgoritmo(servico, 0));
        }

        [Fact]
        public void DevePriorizarExcecaoMunicipalDoServico()
        {
            var servico = CriarServico(
                "<NFSe>" +
                "<Servico>" +
                "<SignatureAlgorithmType>" +
                "<Excecao codMunicipio=\"2917508\">Sha256</Excecao>" +
                "</SignatureAlgorithmType>" +
                "</Servico>" +
                "</NFSe>"
            );

            Assert.Equal(AlgorithmType.Sha256, ObterAlgoritmo(servico, 2917508));
        }

        [Fact]
        public void DeveRejeitarValorInvalido()
        {
            var servico = CriarServico(
                "<Servico><SignatureAlgorithmType>Sha265</SignatureAlgorithmType></Servico>"
            );

            var exception = Assert.Throws<TargetInvocationException>(
                () => ObterAlgoritmo(servico, 0)
            );

            Assert.IsType<Exception>(exception.InnerException);
            Assert.Contains("SignatureAlgorithmType", exception.InnerException.Message);
        }

        [Fact]
        public void DeveAplicarSha256NaAssinatura()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<root><inf Id=\"ID1\" /></root>");

            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest(
                    "CN=SignatureAlgorithmTypeTest",
                    rsa,
                    HashAlgorithmName.SHA256,
                    RSASignaturePadding.Pkcs1
                );
                using (var certificate = request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1)
                ))
                {
                    var informacao = new ValidarEstruturaXML.InformacaoXML
                    {
                        TagAssinatura = "root",
                        TagAtributoID = "inf",
                        UsaCertificadoDigital = true,
                        SignatureAlgorithmType = AlgorithmType.Sha256
                    };

                    var method = typeof(ValidarEstruturaXML).GetMethod(
                        "AssinarSeNecessario",
                        BindingFlags.NonPublic | BindingFlags.Instance
                    );

                    method.Invoke(
                        new ValidarEstruturaXML(),
                        new object[]
                        {
                            xml,
                            informacao,
                            certificate,
                            new Configuracao(),
                            TipoAmbiente.Homologacao,
                            TipoDFe.NFe
                        }
                    );
                }
            }

            var signatureMethod = xml.SelectSingleNode(
                "//*[local-name()='SignatureMethod']"
            ) as XmlElement;

            Assert.NotNull(signatureMethod);
            Assert.Equal(
                "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256",
                signatureMethod.GetAttribute("Algorithm")
            );
        }

        private static XmlNode CriarServico(string xml)
        {
            var document = new XmlDocument();
            document.LoadXml(xml);

            return document.DocumentElement.Name == "Servico"
                ? document.DocumentElement
                : document.DocumentElement.SelectSingleNode("Servico");
        }

        private static AlgorithmType ObterAlgoritmo(XmlNode servico, int codigoConfiguracao)
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "VerificarAlgoritmoAssinatura",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (AlgorithmType)method.Invoke(null, new object[] { servico, codigoConfiguracao });
        }
    }
}
