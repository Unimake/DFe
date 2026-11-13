using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.BPe.BugFixes
{
    [Trait("DFe", "BPe")]
    public class ValidacaoQrCodeBPeTests : IDisposable
    {
        private readonly X509Certificate2 _certificado = CriarCertificado();

        public void Dispose() => _certificado.Dispose();

        public static IEnumerable<object[]> Documentos()
        {
            foreach (var raiz in new[] { "BPe", "BPeTA" })
                foreach (var uf in new[] { UFBrasil.PR, UFBrasil.SP })
                    foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
                        yield return new object[] { raiz, uf, ambiente };
        }

        [Theory]
        [MemberData(nameof(Documentos))]
        public void ConfiguracaoMinima_DeveUsarCatalogoDaAutorizacao(string raiz, UFBrasil uf, TipoAmbiente ambiente)
        {
            var certificado = _certificado;
            var xml = CriarXml(raiz, uf, ambiente);
            var configuracao = CriarConfiguracao(certificado, uf, ambiente);
            var esperado = CriarConfiguracao(certificado, uf, ambiente);
            esperado.TipoDFe = TipoDFe.BPe;
            esperado.SchemaVersao = "1.00";
            esperado.Load("Autorizacao" + raiz);

            var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

            ConferirAssinatura(resultado);
            var qr = Assert.Single(resultado.XmlAssinado.GetElementsByTagName("qrCodBPe").OfType<XmlNode>()).InnerText;
            Assert.True(Uri.TryCreate(qr, UriKind.Absolute, out var uri));
            Assert.Equal(ambiente == TipoAmbiente.Homologacao ? esperado.UrlQrCodeHomologacao : esperado.UrlQrCodeProducao, uri.GetLeftPart(UriPartial.Path));
            Assert.Contains("&tpAmb=" + (int)ambiente, qr);
            Assert.Equal(TipoDFe.NFe, configuracao.TipoDFe);
        }

        [Theory]
        [InlineData("BPe", TipoAmbiente.Homologacao)]
        [InlineData("BPe", TipoAmbiente.Producao)]
        [InlineData("BPeTA", TipoAmbiente.Homologacao)]
        [InlineData("BPeTA", TipoAmbiente.Producao)]
        public void GrupoDoErp_DeveSerPreservado(string raiz, TipoAmbiente ambiente)
        {
            var certificado = _certificado;
            var xml = CriarXml(raiz, UFBrasil.PR, ambiente);
            var configuracao = CriarConfiguracao(certificado, UFBrasil.PR, ambiente);
            configuracao.UrlQrCodeHomologacao = "https://erp.example.invalid/bpe";
            configuracao.UrlQrCodeProducao = "https://erp.example.invalid/bpe";
            var chave = ((XmlElement)xml.GetElementsByTagName("infBPe")[0]).GetAttribute("Id").Substring(3);
            var qrErp = "https://erp.example.invalid/bpe?chBPe=" + chave + "&tpAmb=" + (int)ambiente;
            if (raiz == "BPe")
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPe.BPe>(xml);
                modelo.InfBPeSupl = new Business.DFe.Xml.BPe.InfBPeSupl { QrCodBPe = qrErp };
                xml = modelo.GerarXML();
            }
            else
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPeTA.BPeTA>(xml);
                modelo.InfBPeSupl = new Business.DFe.Xml.BPeTA.InfBPeSupl { QrCodBPe = qrErp };
                xml = modelo.GerarXML();
            }
            var original = xml.GetElementsByTagName("qrCodBPe")[0].InnerText;
            configuracao.UrlQrCodeHomologacao = null;
            configuracao.UrlQrCodeProducao = null;

            var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

            ConferirAssinatura(resultado);
            Assert.Equal(original, resultado.XmlAssinado.GetElementsByTagName("qrCodBPe")[0].InnerText);
            Assert.Null(configuracao.UrlQrCodeHomologacao);
            Assert.Null(configuracao.UrlQrCodeProducao);
        }

        [Theory]
        [InlineData("BPe")]
        [InlineData("BPeTA")]
        public void UrlExplicita_DeveTerPrecedencia(string raiz)
        {
            var certificado = _certificado;
            var configuracao = CriarConfiguracao(certificado, UFBrasil.PR, TipoAmbiente.Homologacao);
            configuracao.UrlQrCodeHomologacao = "https://config.example.invalid/bpe";
            var resultado = new ValidarEstruturaXML().ValidarServico(CriarXml(raiz, UFBrasil.PR, TipoAmbiente.Homologacao), configuracao);
            ConferirAssinatura(resultado);
            Assert.StartsWith(configuracao.UrlQrCodeHomologacao + "?", resultado.XmlAssinado.GetElementsByTagName("qrCodBPe")[0].InnerText);
        }

        [Theory]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void TransporteMetropolitano_DeveContinuarSemQRCode(TipoAmbiente ambiente)
        {
            var certificado = _certificado;
            var configuracao = CriarConfiguracao(certificado, UFBrasil.PR, ambiente);
            var resultado = new ValidarEstruturaXML().ValidarServico(CriarXml("BPeTM", UFBrasil.PR, ambiente), configuracao);
            ConferirAssinatura(resultado);
            Assert.Equal(0, resultado.XmlAssinado.GetElementsByTagName("qrCodBPe").Count);
            Assert.Null(configuracao.UrlQrCodeHomologacao);
            Assert.Null(configuracao.UrlQrCodeProducao);
        }

        private static void ConferirAssinatura(ValidarEstruturaXML.ResultadoValidacao resultado)
        {
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal("1", resultado.StatusValidacao);
            var signature = Assert.IsType<XmlElement>(resultado.XmlAssinado.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl)[0]);
            var signed = new SignedXml(resultado.XmlAssinado);
            signed.LoadXml(signature);
            Assert.True(signed.CheckSignature());
        }

        private static Configuracao CriarConfiguracao(X509Certificate2 certificado, UFBrasil uf, TipoAmbiente ambiente) => new Configuracao
        {
            CertificadoDigital = certificado, CodigoUF = (int)uf, TipoAmbiente = ambiente, TipoEmissao = TipoEmissao.Normal, TipoDFe = TipoDFe.NFe
        };

        private static XmlDocument CriarXml(string raiz, UFBrasil uf, TipoAmbiente ambiente)
        {
            var arquivo = raiz == "BPe" ? "bpe_minimo.xml" : raiz == "BPeTA" ? "bpeTA_minimo.xml" : "bpeTM_minimo.xml";
            var xml = new XmlDocument { XmlResolver = null };
            using (var reader = XmlReader.Create(System.IO.Path.Combine("..", "..", "..", "BPe", "Resources", arquivo), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null })) xml.Load(reader);
            if (raiz == "BPe")
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPe.BPe>(xml);
                modelo.Signature = null; modelo.InfBPeSupl = null; modelo.InfBPe.Ide.CUF = uf; modelo.InfBPe.Ide.TpAmb = ambiente; modelo.InfBPe.Id = null;
                return modelo.GerarXML();
            }
            if (raiz == "BPeTA")
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPeTA.BPeTA>(xml);
                modelo.Signature = null; modelo.InfBPeSupl = null; modelo.InfBPe.Ide.CUF = uf; modelo.InfBPe.Ide.TpAmb = ambiente; modelo.InfBPe.Id = null;
                return modelo.GerarXML();
            }
            var metropolitano = XMLUtility.Deserializar<Business.DFe.Xml.BPeTM.BPeTM>(xml);
            metropolitano.Signature = null; metropolitano.InfBPe.Ide.CUF = uf; metropolitano.InfBPe.Ide.TpAmb = ambiente; metropolitano.InfBPe.Id = null;
            return metropolitano.GerarXML();
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=TESTE SINTETICO BP-e", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var temporario = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(7)))
                {
                    return new X509Certificate2(temporario.Export(X509ContentType.Pfx, "teste"), "teste", X509KeyStorageFlags.Exportable | X509KeyStorageFlags.EphemeralKeySet);
                }
            }
        }
    }
}