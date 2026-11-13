using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.BPe;
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
        [MemberData(nameof(Documentos))]
        public void InicializacaoDoServico_DeveGerarMesmoXmlDaValidacaoDireta(string raiz, UFBrasil uf, TipoAmbiente ambiente)
        {
            var xml = CriarXml(raiz, uf, ambiente);
            var configuracaoDireta = CriarConfiguracao(_certificado, uf, ambiente);
            var configuracaoEnvio = CriarConfiguracao(_certificado, uf, ambiente);
            var resultado = new ValidarEstruturaXML().ValidarServico((XmlDocument)xml.CloneNode(true), configuracaoDireta);

            using (var autorizacao = raiz == "BPe"
                ? (Business.DFe.Servicos.ServicoBase)new AutorizacaoBPe(xml.OuterXml, configuracaoEnvio)
                : new AutorizacaoBPeTA(xml.OuterXml, configuracaoEnvio))
            {
                ConferirAssinatura(resultado);
                ConferirAssinaturaXml(autorizacao.ConteudoXMLAssinado);
                Assert.Equal(resultado.XmlAssinado.OuterXml, autorizacao.ConteudoXMLAssinado.OuterXml);
                Assert.Equal(TipoDFe.BPe, configuracaoEnvio.TipoDFe);
                Assert.Equal(raiz == "BPe" ? Servico.BPeAutorizacao : Servico.BPeTAAutorizacao, configuracaoEnvio.Servico);
            }
        }

        [Theory]
        [InlineData("BPe", TipoAmbiente.Homologacao)]
        [InlineData("BPe", TipoAmbiente.Producao)]
        [InlineData("BPeTA", TipoAmbiente.Homologacao)]
        [InlineData("BPeTA", TipoAmbiente.Producao)]
        public void ContingenciaERevalidacao_DevemPreservarQRCodeEAssinaturas(string raiz, TipoAmbiente ambiente)
        {
            var xml = CriarXml(raiz, UFBrasil.PR, ambiente, TipoEmissaoBPe.ContingenciaOffLine);
            var configuracao = CriarConfiguracao(_certificado, UFBrasil.PR, ambiente);
            configuracao.TipoEmissao = (TipoEmissao)(int)TipoEmissaoBPe.ContingenciaOffLine;
            var validador = new ValidarEstruturaXML();
            var resultado = validador.ValidarServico(xml, configuracao);

            ConferirAssinatura(resultado);
            var qr = Assert.Single(xml.GetElementsByTagName("qrCodBPe").OfType<XmlNode>()).InnerText;
            var chave = ((XmlElement)xml.GetElementsByTagName("infBPe")[0]).GetAttribute("Id").Substring(3);
            var posicaoAssinatura = qr.IndexOf("&sign=", StringComparison.Ordinal);
            Assert.True(posicaoAssinatura > 0);
            using (var chavePublica = _certificado.GetRSAPublicKey())
            {
                Assert.True(chavePublica.VerifyData(Encoding.UTF8.GetBytes(chave), Convert.FromBase64String(qr.Substring(posicaoAssinatura + 6)), HashAlgorithmName.SHA1, RSASignaturePadding.Pkcs1));
            }

            var original = xml.OuterXml;
            var revalidado = validador.ValidarServico(xml, configuracao);

            ConferirAssinatura(revalidado);
            Assert.Equal(original, revalidado.XmlAssinado.OuterXml);
            var suplemento = Assert.Single(xml.GetElementsByTagName("infBPeSupl").OfType<XmlNode>());
            Assert.Equal("Signature", suplemento.NextSibling.LocalName);
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
        [InlineData("BPe", TipoAmbiente.Homologacao)]
        [InlineData("BPe", TipoAmbiente.Producao)]
        [InlineData("BPeTA", TipoAmbiente.Homologacao)]
        [InlineData("BPeTA", TipoAmbiente.Producao)]
        public void UrlDeOutroAmbiente_NaoDeveImpedirBuscaOuSerAlterada(string raiz, TipoAmbiente ambiente)
        {
            const string urlOutroAmbiente = "https://outro-ambiente.example.invalid/bpe";
            var configuracao = CriarConfiguracao(_certificado, UFBrasil.PR, ambiente);
            configuracao.Servico = Servico.NFeConsultaProtocolo;
            configuracao.SchemaVersao = "VERSAO DO CONSUMIDOR";
            if (ambiente == TipoAmbiente.Homologacao) configuracao.UrlQrCodeProducao = urlOutroAmbiente;
            else configuracao.UrlQrCodeHomologacao = urlOutroAmbiente;

            var resultado = new ValidarEstruturaXML().ValidarServico(CriarXml(raiz, UFBrasil.PR, ambiente), configuracao);

            ConferirAssinatura(resultado);
            var qr = resultado.XmlAssinado.GetElementsByTagName("qrCodBPe")[0].InnerText;
            var urlAtual = ambiente == TipoAmbiente.Homologacao ? configuracao.UrlQrCodeHomologacao : configuracao.UrlQrCodeProducao;
            Assert.False(string.IsNullOrWhiteSpace(urlAtual));
            Assert.StartsWith(urlAtual + "?", qr);
            Assert.DoesNotContain("example.invalid", qr);
            Assert.Equal(urlOutroAmbiente, ambiente == TipoAmbiente.Homologacao ? configuracao.UrlQrCodeProducao : configuracao.UrlQrCodeHomologacao);
            Assert.Equal(Servico.NFeConsultaProtocolo, configuracao.Servico);
            Assert.Equal("VERSAO DO CONSUMIDOR", configuracao.SchemaVersao);
            Assert.Equal(TipoDFe.NFe, configuracao.TipoDFe);
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

        public static IEnumerable<object[]> ConfiguracoesExternas()
        {
            foreach (var raiz in new[] { "BPe", "BPeTA" })
                foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
                    foreach (var cenario in new[] { "mais-nova", "desabilitada", "antiga", "mesma", "ausente" })
                        yield return new object[] { raiz, ambiente, cenario };
        }

        [Theory]
        [MemberData(nameof(ConfiguracoesExternas))]
        public void ArquivoExterno_DeveRespeitarOpcoesEVersaoDoConsumidor(string raiz, TipoAmbiente ambiente, string cenario)
        {
            using (var externa = new ConfiguracaoExternaTemporaria(cenario))
            {
                var configuracao = CriarConfiguracao(_certificado, UFBrasil.PR, ambiente);
                configuracao.BuscarConfiguracaoPastaBase = cenario != "desabilitada";
                configuracao.PastaArquivoConfiguracao = externa.Pasta;
                var controle = CriarConfiguracao(_certificado, UFBrasil.PR, ambiente);
                controle.TipoDFe = TipoDFe.BPe;
                controle.SchemaVersao = "1.00";
                controle.BuscarConfiguracaoPastaBase = configuracao.BuscarConfiguracaoPastaBase;
                controle.PastaArquivoConfiguracao = externa.Pasta;
                controle.Load("Autorizacao" + raiz);
                var esperada = ambiente == TipoAmbiente.Homologacao ? controle.UrlQrCodeHomologacao : controle.UrlQrCodeProducao;
                if (cenario == "mais-nova") Assert.Equal(ConfiguracaoExternaTemporaria.Url(ambiente), esperada);
                else Assert.DoesNotContain("example.invalid", esperada);

                var resultado = new ValidarEstruturaXML().ValidarServico(CriarXml(raiz, UFBrasil.PR, ambiente), configuracao);

                ConferirAssinatura(resultado);
                var qr = resultado.XmlAssinado.GetElementsByTagName("qrCodBPe")[0].InnerText;
                Assert.Equal(esperada, new Uri(qr).GetLeftPart(UriPartial.Path));
                Assert.Contains("&tpAmb=" + (int)ambiente, qr);
                Assert.Equal(cenario != "desabilitada", configuracao.BuscarConfiguracaoPastaBase);
                Assert.Equal(externa.Pasta, configuracao.PastaArquivoConfiguracao);
                Assert.Equal(TipoDFe.NFe, configuracao.TipoDFe);
            }
        }

        [Theory]
        [InlineData("BPe", TipoAmbiente.Homologacao, false)]
        [InlineData("BPe", TipoAmbiente.Producao, false)]
        [InlineData("BPeTA", TipoAmbiente.Homologacao, false)]
        [InlineData("BPeTA", TipoAmbiente.Producao, false)]
        [InlineData("BPe", TipoAmbiente.Homologacao, true)]
        [InlineData("BPe", TipoAmbiente.Producao, true)]
        [InlineData("BPeTA", TipoAmbiente.Homologacao, true)]
        [InlineData("BPeTA", TipoAmbiente.Producao, true)]
        public void ArquivoExterno_NaoDeveSobreporUrlExplicitaOuSuplemento(string raiz, TipoAmbiente ambiente, bool suplemento)
        {
            using (var externa = new ConfiguracaoExternaTemporaria("mais-nova"))
            {
                var configuracao = CriarConfiguracao(_certificado, UFBrasil.PR, ambiente);
                configuracao.BuscarConfiguracaoPastaBase = true;
                configuracao.PastaArquivoConfiguracao = externa.Pasta;
                var xml = CriarXml(raiz, UFBrasil.PR, ambiente);
                const string url = "https://consumidor.example.invalid/bpe";
                if (suplemento)
                {
                    var chave = ((XmlElement)xml.GetElementsByTagName("infBPe")[0]).GetAttribute("Id").Substring(3);
                    var qr = url + "?chBPe=" + chave + "&tpAmb=" + (int)ambiente;
                    if (raiz == "BPe")
                    {
                        var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPe.BPe>(xml);
                        modelo.InfBPeSupl = new Business.DFe.Xml.BPe.InfBPeSupl { QrCodBPe = qr };
                        xml = modelo.GerarXML();
                    }
                    else
                    {
                        var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPeTA.BPeTA>(xml);
                        modelo.InfBPeSupl = new Business.DFe.Xml.BPeTA.InfBPeSupl { QrCodBPe = qr };
                        xml = modelo.GerarXML();
                    }
                }
                else
                {
                    configuracao.UrlQrCodeHomologacao = url;
                    configuracao.UrlQrCodeProducao = url;
                }

                var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

                ConferirAssinatura(resultado);
                Assert.Equal(url, new Uri(resultado.XmlAssinado.GetElementsByTagName("qrCodBPe")[0].InnerText).GetLeftPart(UriPartial.Path));
                Assert.Equal(suplemento ? null : url, configuracao.UrlQrCodeHomologacao);
                Assert.Equal(suplemento ? null : url, configuracao.UrlQrCodeProducao);
            }
        }

        private sealed class ConfiguracaoExternaTemporaria : IDisposable
        {
            public string Pasta { get; } = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "Unimake-BPe-Externa-" + Guid.NewGuid().ToString("N"));

            public ConfiguracaoExternaTemporaria(string cenario)
            {
                System.IO.Directory.CreateDirectory(Pasta);
                if (cenario == "ausente") return;
                foreach (var arquivo in new[] { "PR.xml", "SVRS.xml" })
                {
                    var assembly = typeof(Configuracao).Assembly;
                    var recurso = assembly.GetManifestResourceNames().Single(x => x.EndsWith(".Servicos.Config.BPe." + arquivo, StringComparison.Ordinal));
                    var xml = new XmlDocument { XmlResolver = null };
                    using (var stream = assembly.GetManifestResourceStream(recurso))
                    using (var reader = XmlReader.Create(stream, new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null })) xml.Load(reader);
                    var versao = xml.CreateElement("VersaoConfiguracao");
                    versao.InnerText = cenario == "antiga" ? "1" : cenario == "mesma" ? Info.VersaoDLL : "999999999999999999";
                    xml.DocumentElement.PrependChild(versao);
                    foreach (XmlNode url in xml.GetElementsByTagName("UrlQrCodeHomologacao")) url.InnerText = Url(TipoAmbiente.Homologacao);
                    foreach (XmlNode url in xml.GetElementsByTagName("UrlQrCodeProducao")) url.InnerText = Url(TipoAmbiente.Producao);
                    xml.Save(System.IO.Path.Combine(Pasta, arquivo));
                }
            }

            public static string Url(TipoAmbiente ambiente) => ambiente == TipoAmbiente.Homologacao
                ? "https://externa-homologacao.example.invalid/bpe" : "https://externa-producao.example.invalid/bpe";

            public void Dispose()
            {
                var pasta = System.IO.Path.GetFullPath(Pasta);
                var temporaria = System.IO.Path.GetFullPath(System.IO.Path.GetTempPath());
                if (!pasta.StartsWith(temporaria, StringComparison.OrdinalIgnoreCase)
                    || !System.IO.Path.GetFileName(pasta).StartsWith("Unimake-BPe-Externa-", StringComparison.Ordinal)) throw new InvalidOperationException("Limpeza fora da fixture temporaria.");
                System.IO.Directory.Delete(pasta, true);
            }
        }

        private static void ConferirAssinatura(ValidarEstruturaXML.ResultadoValidacao resultado)
        {
            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal("1", resultado.StatusValidacao);
            ConferirAssinaturaXml(resultado.XmlAssinado);
        }

        private static void ConferirAssinaturaXml(XmlDocument xml)
        {
            var signature = Assert.Single(xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).OfType<XmlElement>());
            var signed = new SignedXml(xml);
            signed.LoadXml(signature);
            Assert.True(signed.CheckSignature());
        }

        private static Configuracao CriarConfiguracao(X509Certificate2 certificado, UFBrasil uf, TipoAmbiente ambiente) => new Configuracao
        {
            CertificadoDigital = certificado, CodigoUF = (int)uf, TipoAmbiente = ambiente, TipoEmissao = TipoEmissao.Normal, TipoDFe = TipoDFe.NFe
        };

        private static XmlDocument CriarXml(string raiz, UFBrasil uf, TipoAmbiente ambiente, TipoEmissaoBPe emissao = TipoEmissaoBPe.Normal)
        {
            var arquivo = raiz == "BPe" ? "bpe_minimo.xml" : raiz == "BPeTA" ? "bpeTA_minimo.xml" : "bpeTM_minimo.xml";
            var xml = new XmlDocument { XmlResolver = null };
            using (var reader = XmlReader.Create(System.IO.Path.Combine("..", "..", "..", "BPe", "Resources", arquivo), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null })) xml.Load(reader);
            if (raiz == "BPe")
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPe.BPe>(xml);
                modelo.Signature = null; modelo.InfBPeSupl = null; modelo.InfBPe.Ide.CUF = uf; modelo.InfBPe.Ide.TpAmb = ambiente; modelo.InfBPe.Id = null;
                modelo.InfBPe.Ide.TpEmis = emissao;
                if (emissao == TipoEmissaoBPe.ContingenciaOffLine)
                {
                    modelo.InfBPe.Ide.DhContField = modelo.InfBPe.Ide.DhEmiField;
                    modelo.InfBPe.Ide.XJust = "CONTINGENCIA SINTETICA PARA TESTE OFFLINE";
                }
                return modelo.GerarXML();
            }
            if (raiz == "BPeTA")
            {
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.BPeTA.BPeTA>(xml);
                modelo.Signature = null; modelo.InfBPeSupl = null; modelo.InfBPe.Ide.CUF = uf; modelo.InfBPe.Ide.TpAmb = ambiente; modelo.InfBPe.Id = null;
                modelo.InfBPe.Ide.TpEmis = emissao;
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
