using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;
using NFeABIAutorizacaoSinc = Unimake.Business.DFe.Servicos.NFeABI.AutorizacaoSinc;

namespace Unimake.DFe.Test.NFeABI.Validacao
{
    /// <summary>
    /// Testes da geração do QR Code da NFeABI.
    /// </summary>
    public class QRCodeTest
    {
        private const string UrlQrCodeHomologacao = "https://www.fazenda.pr.gov.br/nfeabi/qrcode";
        private const string UrlChaveHomologacao = "https://www.fazenda.pr.gov.br/nfeabi/consulta";

        /// <summary>
        /// Gera o grupo suplementar online no formato usado em homologação.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void DeveGerarQRCodeOnline()
        {
            var xml = CarregarXmlSemGrupoSuplementar();
            var infNFeABI = (XmlElement)xml.GetElementsByTagName("infNFeABI")[0];
            var chave = infNFeABI.GetAttribute("Id").Substring("NFeABI".Length);
            var configuracao = CriarConfiguracao();

            QrCodeXmlHelper.MontarQrCodeNFeABI(xml, configuracao);
            QrCodeXmlHelper.MontarQrCodeNFeABI(xml, configuracao);

            var grupo = xml.GetElementsByTagName("infNFeSupl");
            Assert.Single(grupo);
            var suplemento = (XmlElement)grupo[0];
            var tagQrCode = suplemento.GetElementsByTagName("qrCode")[0];
            var qrCode = Assert.IsType<XmlText>(Assert.Single(tagQrCode.ChildNodes));
            Assert.Equal(UrlQrCodeHomologacao + "?chNFeABI=" + chave + "&tpAmb=2", qrCode.Value);
            Assert.Contains("&amp;tpAmb=2</qrCode>", tagQrCode.OuterXml);
            Assert.InRange(qrCode.Value.Length, 100, 1000);
            var urlChave = Assert.IsType<XmlText>(Assert.Single(suplemento.GetElementsByTagName("urlChave")[0].ChildNodes));
            Assert.Equal(UrlChaveHomologacao, urlChave.Value);
            Assert.Equal("Signature", grupo[0].NextSibling.LocalName);
        }

        /// <summary>
        /// Inclui os parâmetros e a assinatura exigidos na contingência offline.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void DeveGerarQRCodeOfflineAssinado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=NFeABI QRCode Test", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var xml = CarregarXmlSemGrupoSuplementar();
                    var infNFeABI = (XmlElement)xml.GetElementsByTagName("infNFeABI")[0];
                    var ide = (XmlElement)infNFeABI.GetElementsByTagName("ide")[0];
                    ide.GetElementsByTagName("tpEmis")[0].InnerText = ((int)TipoEmissao.ContingenciaOffLine).ToString();
                    AdicionarTotal(xml, infNFeABI, "100.00");

                    var chave = infNFeABI.GetAttribute("Id").Substring("NFeABI".Length);
                    var parametrosSemAssinatura = chave + "|1|2|14|100.00|2|12345678901";
                    var configuracao = CriarConfiguracao();
                    configuracao.CertificadoDigital = certificado;

                    QrCodeXmlHelper.MontarQrCodeNFeABI(xml, configuracao);

                    var assinatura = Converter.ToRSASHA1(certificado, parametrosSemAssinatura);
                    var qrCode = Assert.IsType<XmlText>(Assert.Single(xml.GetElementsByTagName("qrCode")[0].ChildNodes));
                    Assert.Equal(
                        UrlQrCodeHomologacao + "?p=" + parametrosSemAssinatura + "|" + assinatura,
                        qrCode.Value);
                }
            }
        }

        /// <summary>
        /// Confirma que a autorização carrega os endereços de homologação definidos no arquivo SVRS.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoDeveCarregarUrlsDeHomologacao()
        {
            var documento = new Business.DFe.Xml.NFeABI.NFeABI()
                .LoadFromFile(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");

            var servico = new NFeABIAutorizacaoSinc(documento, new Configuracao());

            Assert.Equal(UrlQrCodeHomologacao, servico.Configuracoes.UrlQrCodeHomologacao);
            Assert.True(string.IsNullOrEmpty(servico.Configuracoes.UrlQrCodeProducao));
            Assert.Equal(UrlChaveHomologacao, servico.Configuracoes.UrlChaveHomologacao);
            Assert.True(string.IsNullOrEmpty(servico.Configuracoes.UrlChaveProducao));
        }

        /// <summary>
        /// Rejeita antecipadamente uma URL que produza QR Code menor que o permitido pelo schema.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void DeveRejeitarQRCodeMenorQueOLimiteDoSchema()
        {
            var xml = CarregarXmlSemGrupoSuplementar();
            var configuracao = CriarConfiguracao();
            configuracao.UrlQrCodeHomologacao = "https://example.com";

            var exception = Assert.Throws<Exception>(() => QrCodeXmlHelper.MontarQrCodeNFeABI(xml, configuracao));

            Assert.Contains("entre 100 e 1000 caracteres", exception.Message);
        }

        /// <summary>
        /// Garante que a autorização online gere um QR Code aceito pelo schema da NFeABI.
        /// </summary>
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("p=")]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoOnlineDeveGerarQRCodeValidoParaOSchema(string prefixoLegado)
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=NFeABI QRCode Online Test", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var documento = new Business.DFe.Xml.NFeABI.NFeABI()
                        .LoadFromFile(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");
                    documento.Signature = null;
                    documento.InfNFeABI.Ide.TpEmis = TipoEmissaoNFeABI.Normal;
                    var urlBase = prefixoLegado == null ? UrlQrCodeHomologacao : "https://homologacao.example.com/nfeabi/consulta";
                    var chave = documento.InfNFeABI.Chave;
                    documento.InfNFeSupl = prefixoLegado == null
                        ? null
                        : new Business.DFe.Xml.NFeABI.InfNFeSupl
                        {
                            QrCode = urlBase + "?" + prefixoLegado + chave + "|1|2",
                            UrlChave = UrlChaveHomologacao
                        };

                    var servico = new NFeABIAutorizacaoSinc(
                        documento,
                        new Configuracao { CertificadoDigital = certificado });
                    var xml = new XmlDocument();
                    xml.LoadXml(servico.ConteudoXMLAssinado.OuterXml);
                    var qrCode = Assert.IsType<XmlText>(Assert.Single(xml.GetElementsByTagName("qrCode")[0].ChildNodes));

                    Assert.InRange(qrCode.Value.Length, 100, 1000);
                    Assert.Equal(urlBase + "?chNFeABI=" + chave + "&tpAmb=2", qrCode.Value);
                    Assert.Equal(UrlChaveHomologacao, xml.GetElementsByTagName("urlChave")[0].InnerText);
                }
            }
        }

        /// <summary>
        /// Garante que o pipeline de autorização gere o grupo suplementar quando ele não foi informado.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoDeveGerarQRCodeAutomaticamente()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=NFeABI QRCode Pipeline Test", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var documento = new Business.DFe.Xml.NFeABI.NFeABI()
                        .LoadFromFile(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");
                    documento.InfNFeSupl = null;
                    documento.Signature = null;
                    documento.InfNFeABI.Ide.TpEmis = TipoEmissaoNFeABI.ContingenciaOffline;
                    documento.InfNFeABI.Total = new Business.DFe.Xml.NFeABI.Total
                    {
                        VTotalOperacao = 100,
                        VOperacIndiv = 100,
                        TribTot = new Business.DFe.Xml.NFeABI.TribTot(),
                        VNF = 100
                    };

                    var servico = new NFeABIAutorizacaoSinc(
                        documento,
                        new Configuracao { CertificadoDigital = certificado });

                    Assert.Single(servico.ConteudoXMLAssinado.GetElementsByTagName("infNFeSupl"));
                    Assert.Single(servico.ConteudoXMLAssinado.GetElementsByTagName("qrCode"));
                    Assert.Single(servico.ConteudoXMLAssinado.GetElementsByTagName("urlChave"));
                }
            }
        }

        private static Configuracao CriarConfiguracao()
        {
            return new Configuracao
            {
                TipoAmbiente = TipoAmbiente.Homologacao,
                UrlQrCodeHomologacao = UrlQrCodeHomologacao,
                UrlQrCodeProducao = string.Empty,
                UrlChaveHomologacao = UrlChaveHomologacao,
                UrlChaveProducao = string.Empty
            };
        }

        private static XmlDocument CarregarXmlSemGrupoSuplementar()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");
            var grupo = xml.GetElementsByTagName("infNFeSupl")[0];
            grupo.ParentNode.RemoveChild(grupo);
            return xml;
        }

        private static void AdicionarTotal(XmlDocument xml, XmlElement infNFeABI, string valor)
        {
            const string namespaceNFeABI = "http://www.portalfiscal.inf.br/nfeabi";
            var total = xml.CreateElement("total", namespaceNFeABI);
            var vNF = xml.CreateElement("vNF", namespaceNFeABI);
            vNF.InnerText = valor;
            total.AppendChild(vNF);
            infNFeABI.AppendChild(total);
        }
    }
}
