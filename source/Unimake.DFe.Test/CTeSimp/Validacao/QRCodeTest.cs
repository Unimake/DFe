using System;
using System.IO;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.CTeSimp.Validacao
{
    public class QRCodeTest
    {
        private const string NamespaceCTe = "http://www.portalfiscal.inf.br/cte";
        private const string UrlProducao = "https://example.com/cte/producao";
        private const string UrlHomologacao = "https://example.com/cte/homologacao";

        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(TipoEmissao.ContingenciaOfflineCTe, TipoAmbiente.Producao)]
        [InlineData(TipoEmissao.ContingenciaOfflineCTe, TipoAmbiente.Homologacao)]
        [InlineData(TipoEmissao.ContingenciaEPEC, TipoAmbiente.Producao)]
        public void ContingenciaGeraAssinaturaRSASHA1DaChave(TipoEmissao emissao, TipoAmbiente ambiente)
        {
            var xml = CriarXml(emissao, ambiente);
            var chave = ((XmlElement)xml.GetElementsByTagName("infCte")[0]).GetAttribute("Id").Substring(3);
            var configuracao = CriarConfiguracao(ambiente);

            using (var rsa = RSA.Create(2048))
            {
                var pedido = new CertificateRequest("CN=CTeSimp QRCode TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = pedido.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    configuracao.CertificadoDigital = certificado;
                    QrCodeXmlHelper.MontarQrCodeCTeSimp(xml, configuracao);

                    var qrCode = xml.GetElementsByTagName("qrCodCTe", NamespaceCTe)[0].InnerText;
                    var prefixo = (ambiente == TipoAmbiente.Producao ? UrlProducao : UrlHomologacao) +
                        "?chCTe=" + chave + "&tpAmb=" + (int)ambiente + "&sign=";
                    Assert.StartsWith(prefixo, qrCode);
                    Assert.Matches("^[0-9]{44}$", chave);
                    Assert.Equal(((int)emissao).ToString(), chave.Substring(34, 1));

                    var assinatura = Convert.FromBase64String(qrCode.Substring(prefixo.Length));
                    using (var chavePublica = certificado.GetRSAPublicKey())
                    {
                        Assert.True(chavePublica.VerifyData(Encoding.UTF8.GetBytes(chave), assinatura, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1));
                        Assert.False(chavePublica.VerifyData(Encoding.UTF8.GetBytes(chave + "0"), assinatura, HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1));
                    }

                    QrCodeXmlHelper.MontarQrCodeCTeSimp(xml, configuracao);
                    Assert.Single(xml.GetElementsByTagName("infCTeSupl", NamespaceCTe));
                    Assert.Equal(qrCode, xml.GetElementsByTagName("qrCodCTe", NamespaceCTe)[0].InnerText);
                    Assert.Equal("Signature", xml.GetElementsByTagName("infCTeSupl", NamespaceCTe)[0].NextSibling.LocalName);
                    if (emissao == TipoEmissao.ContingenciaOfflineCTe)
                    {
                        ValidarSchema(xml);
                    }
                }
            }
        }

        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(TipoEmissao.Normal, TipoAmbiente.Producao)]
        [InlineData(TipoEmissao.Normal, TipoAmbiente.Homologacao)]
        [InlineData(TipoEmissao.ContingenciaSVCRS, TipoAmbiente.Producao)]
        [InlineData(TipoEmissao.ContingenciaSVCSP, TipoAmbiente.Producao)]
        public void NormalESVCGeramQRCodeSemAssinaturaOuCertificado(TipoEmissao emissao, TipoAmbiente ambiente)
        {
            var xml = CriarXml(emissao, ambiente);
            var chave = ((XmlElement)xml.GetElementsByTagName("infCte")[0]).GetAttribute("Id").Substring(3);

            QrCodeXmlHelper.MontarQrCodeCTeSimp(xml, CriarConfiguracao(ambiente));

            var url = ambiente == TipoAmbiente.Producao ? UrlProducao : UrlHomologacao;
            Assert.Equal(url + "?chCTe=" + chave + "&tpAmb=" + (int)ambiente,
                xml.GetElementsByTagName("qrCodCTe", NamespaceCTe)[0].InnerText);
            ValidarSchema(xml);
        }

        private static XmlDocument CriarXml(TipoEmissao emissao, TipoAmbiente ambiente)
        {
            var modelo = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTeSimp.CTeSimp>(
                File.ReadAllText(@"..\..\..\CTeSimp\Resources\CTeSimp_AtualizacaoSchemas.xml"));
            modelo.InfCTe.Ide.TpEmis = emissao;
            modelo.InfCTe.Ide.TpAmb = ambiente;
            return modelo.GerarXML();
        }

        private static Configuracao CriarConfiguracao(TipoAmbiente ambiente) => new Configuracao
        {
            TipoAmbiente = ambiente,
            UrlQrCodeProducao = UrlProducao,
            UrlQrCodeHomologacao = UrlHomologacao
        };

        private static void ValidarSchema(XmlDocument xml)
        {
            var validador = new ValidarSchema();
            validador.Validar(xml, "CTe.cteSimp_v4.00.xsd", NamespaceCTe);
            Assert.True(validador.Success, validador.ErrorMessage);
        }
    }
}
