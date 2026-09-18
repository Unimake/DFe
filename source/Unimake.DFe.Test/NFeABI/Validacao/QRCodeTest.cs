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
        private const string UrlProvisoria = "https://www.fazenda.pr.gov.br/nfeabi/qrcode";

        /// <summary>
        /// Gera o grupo suplementar online conforme a versão 1 do QR Code.
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
            Assert.Equal(UrlProvisoria + "?p=" + chave + "|1|2", ((XmlElement)grupo[0]).GetElementsByTagName("qrCode")[0].InnerText);
            Assert.Equal(UrlProvisoria, ((XmlElement)grupo[0]).GetElementsByTagName("urlChave")[0].InnerText);
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
                    Assert.Equal(
                        UrlProvisoria + "?p=" + parametrosSemAssinatura + "|" + assinatura,
                        xml.GetElementsByTagName("qrCode")[0].InnerText);
                }
            }
        }

        /// <summary>
        /// Confirma que a autorização carrega as URLs provisórias do arquivo SVRS.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        public void AutorizacaoDeveCarregarUrlsProvisorias()
        {
            var documento = new Business.DFe.Xml.NFeABI.NFeABI()
                .LoadFromFile(@"..\..\..\NFeABI\Resources\NFeABI-minima.xml");

            var servico = new NFeABIAutorizacaoSinc(documento, new Configuracao());

            Assert.Equal(UrlProvisoria, servico.Configuracoes.UrlQrCodeHomologacao);
            Assert.Equal(UrlProvisoria, servico.Configuracoes.UrlQrCodeProducao);
            Assert.Equal(UrlProvisoria, servico.Configuracoes.UrlChaveHomologacao);
            Assert.Equal(UrlProvisoria, servico.Configuracoes.UrlChaveProducao);
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
                UrlQrCodeHomologacao = UrlProvisoria,
                UrlQrCodeProducao = UrlProvisoria,
                UrlChaveHomologacao = UrlProvisoria,
                UrlChaveProducao = UrlProvisoria
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
