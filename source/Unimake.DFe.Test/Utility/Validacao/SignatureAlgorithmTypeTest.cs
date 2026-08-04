using System;
using System.IO;
using System.Reflection;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Security;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
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
            var servicoDocument = new XmlDocument();
            servicoDocument.LoadXml("<Servico />");

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
                            servicoDocument.DocumentElement,
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

        [Fact]
        public void DeveAssinarLoteESocialComReferenceURIVazio()
        {
            var xml = new XmlDocument();
            xml.LoadXml(
                "<eSocial xmlns=\"http://www.esocial.gov.br/schema/lote/eventos/envio/v1_1_1\">" +
                "<envioLoteEventos>" +
                "<eventos>" +
                "<evento Id=\"ID1785098580000002026070211131400001\">" +
                "<eSocial xmlns=\"http://www.esocial.gov.br/schema/evt/evtRemun/v_S_01_03_00\">" +
                "<evtRemun Id=\"ID1785098580000002026070211131400001\" />" +
                "</eSocial>" +
                "</evento>" +
                "<evento Id=\"ID1785098580000002026070211131400002\">" +
                "<eSocial xmlns=\"http://www.esocial.gov.br/schema/evt/evtInfoEmpregador/v_S_01_03_00\">" +
                "<evtInfoEmpregador Id=\"ID1785098580000002026070211131400002\" />" +
                "</eSocial>" +
                "</evento>" +
                "</eventos>" +
                "</envioLoteEventos>" +
                "</eSocial>"
            );

            var servicoDocument = new XmlDocument();
            servicoDocument.LoadXml(
                "<Servico>" +
                "<SchemasEspecificos>" +
                "<Tipo>" +
                "<Evento>evtRemun</Evento>" +
                "<TagAtributoID>evtRemun</TagAtributoID>" +
                "</Tipo>" +
                "<Tipo>" +
                "<Evento>evtInfoEmpregador</Evento>" +
                "<TagAtributoID>evtInfoEmpregador</TagAtributoID>" +
                "</Tipo>" +
                "</SchemasEspecificos>" +
                "</Servico>"
            );

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
                        UsaCertificadoDigital = true
                    };

                    AssinarLote(xml, servicoDocument.DocumentElement, informacao, certificate, TipoDFe.ESocial);
                    AssinarLote(xml, servicoDocument.DocumentElement, informacao, certificate, TipoDFe.ESocial);

                    Assert.Equal(2, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);

                    foreach (XmlElement evento in xml.GetElementsByTagName("evento"))
                    {
                        ValidarAssinaturaDoEvento(evento, certificate, string.Empty, false);
                    }
                }
            }
            Assert.DoesNotContain("#ID1785098580000002026070211131400001", xml.OuterXml);
            Assert.DoesNotContain("#ID1785098580000002026070211131400002", xml.OuterXml);
        }

        [Fact]
        public void DeveAssinarTodosEventosEFDReinfUmaUnicaVez()
        {
            var xml = new XmlDocument();
            xml.LoadXml(
                "<Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/envioLoteEventosAssincrono/v1_00_00\">" +
                "<envioLoteEventos><eventos>" +
                "<evento id=\"ID1000000000000002026070211131400001\">" +
                "<Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02\">" +
                "<evtInfoContri id=\"ID1000000000000002026070211131400001\" />" +
                "</Reinf></evento>" +
                "<evento id=\"ID1000000000000002026070211131400002\">" +
                "<Reinf xmlns=\"http://www.reinf.esocial.gov.br/schemas/evtInfoContribuinte/v2_01_02\">" +
                "<evtInfoContri id=\"ID1000000000000002026070211131400002\" />" +
                "</Reinf></evento>" +
                "</eventos></envioLoteEventos></Reinf>"
            );

            var servicoDocument = new XmlDocument();
            servicoDocument.LoadXml(
                "<Servico><SchemasEspecificos><Tipo>" +
                "<Evento>evtInfoContri</Evento>" +
                "<TagAtributoID>evtInfoContri</TagAtributoID>" +
                "</Tipo></SchemasEspecificos></Servico>"
            );

            using (var certificate = CriarCertificado())
            {
                var informacao = new ValidarEstruturaXML.InformacaoXML
                {
                    UsaCertificadoDigital = true
                };

                AssinarLote(xml, servicoDocument.DocumentElement, informacao, certificate, TipoDFe.EFDReinf);
                AssinarLote(xml, servicoDocument.DocumentElement, informacao, certificate, TipoDFe.EFDReinf);

                Assert.Equal(2, xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);

                foreach (XmlElement evento in xml.GetElementsByTagName("evento"))
                {
                    ValidarAssinaturaDoEvento(evento, certificate, "#" + ObterIdEventoInterno(evento), false);
                }
            }
        }

        [Fact]
        public void DeveAssinarLoteESocialUsandoCatalogoCentral()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\ESocial\Resources\EnvioLoteEventos-esocial-loteevt.xml");

            using (var certificate = CriarCertificado())
            {
                var configuracao = new Configuracao
                {
                    CertificadoDigital = certificate,
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    TipoDFe = TipoDFe.ESocial
                };

                var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

                Assert.True(resultado.Validado, resultado.MensagemRetorno);
                Assert.Equal(
                    xml.GetElementsByTagName("evento").Count,
                    xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count
                );

                foreach (XmlElement evento in xml.GetElementsByTagName("evento"))
                {
                    ValidarAssinaturaDoEvento(evento, certificate, string.Empty, false);
                }
            }
        }

        [Fact]
        public void DeveAssinarLoteEFDReinfUsandoCatalogoCentral()
        {
            var caminho = @"..\..\..\EFDReinf\Resources\loteEventosAssincrono-Reinf-loteevt.xml";
            Assert.True(File.Exists(caminho), "Arquivo de lote EFD-Reinf não encontrado.");

            var xml = new XmlDocument();
            xml.Load(caminho);

            using (var certificate = CriarCertificado())
            {
                var configuracao = new Configuracao
                {
                    CertificadoDigital = certificate,
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    TipoDFe = TipoDFe.EFDReinf
                };

                var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

                Assert.True(resultado.Validado, resultado.MensagemRetorno);
                Assert.Equal(
                    xml.GetElementsByTagName("evento").Count,
                    xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count
                );

                foreach (XmlElement evento in xml.GetElementsByTagName("evento"))
                {
                    ValidarAssinaturaDoEvento(evento, certificate, "#" + ObterIdEventoInterno(evento), false);
                }
            }
        }

        [Fact]
        public void NaoDeveAssinarNoAmbienteConfigurado()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<root><inf Id=\"ID1\" /></root>");
            var servico = CriarServico("<Servico />");
            var informacao = new ValidarEstruturaXML.InformacaoXML
            {
                TagAssinatura = "root",
                TagAtributoID = "inf",
                UsaCertificadoDigital = true,
                NaoAssina = TipoAmbiente.Homologacao
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
                    servico,
                    informacao,
                    null,
                    new Configuracao(),
                    TipoAmbiente.Homologacao,
                    TipoDFe.NFe
                }
            );

            Assert.Null(xml.SelectSingleNode("//*[local-name()='Signature']"));
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

        private static void AssinarLote(XmlDocument xml, XmlNode servico, ValidarEstruturaXML.InformacaoXML informacao, X509Certificate2 certificado, TipoDFe tipoDFe)
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "AssinarSeNecessario",
                BindingFlags.NonPublic | BindingFlags.Instance
            );

            method.Invoke(
                new ValidarEstruturaXML(),
                new object[]
                {
                    xml,
                    servico,
                    informacao,
                    certificado,
                    new Configuracao(),
                    TipoAmbiente.Homologacao,
                    tipoDFe
                }
            );
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest(
                    "CN=AssinaturaLotesCentralizadaTest",
                    rsa,
                    HashAlgorithmName.SHA256,
                    RSASignaturePadding.Pkcs1
                );

                return request.CreateSelfSigned(
                    DateTimeOffset.UtcNow.AddDays(-1),
                    DateTimeOffset.UtcNow.AddDays(1)
                );
            }
        }

        private static void ValidarAssinaturaDoEvento(XmlElement evento, X509Certificate2 certificado, string uriEsperada, bool canonicalizacaoExclusiva)
        {
            var eventoInterno = evento.ChildNodes[0] as XmlElement;
            var documentoEvento = new XmlDocument();
            documentoEvento.LoadXml(eventoInterno.OuterXml);

            var signature = documentoEvento.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0] as XmlElement;
            Assert.NotNull(signature);

            var reference = signature.SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='Reference']") as XmlElement;
            var signatureMethod = signature.SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='SignatureMethod']") as XmlElement;
            var digestMethod = signature.SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='Reference']/*[local-name()='DigestMethod']") as XmlElement;
            var canonicalizationMethod = signature.SelectSingleNode("*[local-name()='SignedInfo']/*[local-name()='CanonicalizationMethod']") as XmlElement;

            Assert.Equal(uriEsperada, reference.GetAttribute("URI"));
            Assert.Equal(SignedXml.XmlDsigRSASHA256Url, signatureMethod.GetAttribute("Algorithm"));
            Assert.Equal("http://www.w3.org/2001/04/xmlenc#sha256", digestMethod.GetAttribute("Algorithm"));
            Assert.Equal(
                canonicalizacaoExclusiva ? SignedXml.XmlDsigExcC14NTransformUrl : SignedXml.XmlDsigC14NTransformUrl,
                canonicalizationMethod.GetAttribute("Algorithm")
            );

            var signedXml = new SignedXml(documentoEvento);
            signedXml.LoadXml(signature);
            Assert.True(signedXml.CheckSignature(certificado, true));
        }

        private static string ObterIdEventoInterno(XmlElement evento)
        {
            var raizEvento = evento.ChildNodes[0] as XmlElement;
            var eventoFiscal = raizEvento.ChildNodes[0] as XmlElement;
            var id = eventoFiscal.GetAttribute("id");
            return string.IsNullOrWhiteSpace(id) ? eventoFiscal.GetAttribute("Id") : id;
        }
    }
}
