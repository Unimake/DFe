using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Security.Cryptography.Xml;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    [Trait("Utility", "Validacao")]
    public class PreparacaoUrlsQRCodeTest : IDisposable
    {
        private readonly X509Certificate2 _certificado = CriarCertificado();

        public void Dispose() => _certificado.Dispose();

        public static IEnumerable<object[]> Documentos()
        {
            var documentos = new[]
            {
                new object[] { TipoDFe.NFe, @"NFe\Resources\NFe1.xml", "qrCode", "http://www.fazenda.pr.gov.br/nfce/qrcode", 3 },
                new object[] { TipoDFe.NFCe, @"NFe\Resources\NFe1.xml", "qrCode", "http://www.fazenda.pr.gov.br/nfce/qrcode", 2 },
                new object[] { TipoDFe.NFCe, @"NFe\Resources\NFe1.xml", "qrCode", "http://www.fazenda.pr.gov.br/nfce/qrcode", 3 },
                new object[] { TipoDFe.CTe, @"CTe\Resources\CTe_AtualizacaoSchemas.xml", "qrCodCTe", "https://dfe-portal.svrs.rs.gov.br/cte/qrCode", 3 },
                new object[] { TipoDFe.MDFe, @"MDFe\Resources\99999999999999999999999999999999999999999999-procMDFe.xml", "qrCodMDFe", "https://dfe-portal.svrs.rs.gov.br/mdfe/qrCode", 3 },
                new object[] { TipoDFe.NFCom, @"NFCom\Resources\NFCom_AtualizacaoSchemas.xml", "qrCodNFCom", "https://dfe-portal.svrs.rs.gov.br/NFCom/qrCode", 3 },
                new object[] { TipoDFe.NF3e, @"NF3e\Resources\NF3e_AtualizacaoSchemas.xml", "qrCodNF3e", "http://www.fazenda.pr.gov.br/nf3e/qrcode", 3 },
                new object[] { TipoDFe.NFGas, @"NFGas\Resources\nfgas-atualizacao-schema.xml", "qrCodNFGas", "https://dfe-portal.svrs.rs.gov.br/nfgas/qrCode", 3 },
                new object[] { TipoDFe.DCe, @"DCe\Resources\dce.xml", "qrCodDCe", "https://www.fazenda.pr.gov.br/dce/qrcode", 3 }
            };

            foreach (var documento in documentos)
            {
                foreach (var ambiente in new[] { TipoAmbiente.Homologacao, TipoAmbiente.Producao })
                {
                    yield return documento.Concat(new object[] { ambiente }).ToArray();
                }
            }
        }

        [Theory]
        [MemberData(nameof(Documentos))]
        public void ValidacaoDireta_DevePrepararUrlsAntesDeAssinarEValidar(TipoDFe tipoDFe, string arquivo, string tagQRCode, string urlEsperada, int versaoQRCode, TipoAmbiente ambiente)
        {
            var xml = CriarXml(tipoDFe, arquivo, ambiente);
            var configuracao = CriarConfiguracao(tipoDFe, xml, versaoQRCode, ambiente);
            var validador = new ValidarEstruturaXML();

            var resultado = validador.ValidarServico(xml, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            var qrCode = Assert.Single(xml.GetElementsByTagName(tagQRCode).Cast<XmlNode>());
            Assert.Equal(xml.DocumentElement.NamespaceURI, qrCode.NamespaceURI);
            Assert.StartsWith(urlEsperada + "?", qrCode.InnerText);
            var informacoes = qrCode.ParentNode.ParentNode.SelectSingleNode("*[@versao]");
            Assert.NotNull(informacoes);
            var versao = informacoes.Attributes["versao"].Value;
            var catalogo = CatalogoValidacaoXML.CriarCopia();
            var grupoUrl = catalogo.SelectSingleNode("ServicosValidacao/" + tipoDFe + "/Servico[@tagRaiz='"
                + xml.DocumentElement.LocalName + "' and @versao='" + versao + "']/GrupoUrl");
            Assert.NotNull(grupoUrl);
            Assert.Equal(qrCode.ParentNode.LocalName, grupoUrl.Attributes["tagSuplementar"]?.Value);
            var signature = Assert.Single(xml.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Cast<XmlElement>());
            var signed = new SignedXml(xml);
            signed.LoadXml(signature);
            Assert.True(signed.CheckSignature());
            Assert.Equal(tipoDFe, configuracao.TipoDFe);

            var original = xml.OuterXml;
            var revalidado = validador.ValidarServico(xml, configuracao);

            Assert.True(revalidado.Validado, revalidado.MensagemRetorno);
            Assert.Equal(original, revalidado.XmlAssinado.OuterXml);
        }

        [Theory]
        [MemberData(nameof(Documentos))]
        public void SuplementoExistente_DeveManterPrecedenciaERegeracaoAtuais(TipoDFe tipoDFe, string arquivo, string tagQRCode, string urlEsperada, int versaoQRCode, TipoAmbiente ambiente)
        {
            var xml = CriarXml(tipoDFe, arquivo, ambiente);
            var controle = CriarConfiguracao(tipoDFe, xml, versaoQRCode, ambiente);
            var validador = new ValidarEstruturaXML();
            var preparado = validador.ValidarServico(xml, controle);
            Assert.True(preparado.Validado, preparado.MensagemRetorno);
            Assert.StartsWith(urlEsperada + "?", xml.GetElementsByTagName(tagQRCode)[0].InnerText);
            var original = (XmlDocument)xml.CloneNode(true);

            foreach (var informarUrls in new[] { false, true })
            {
                var configuracao = CriarConfiguracao(tipoDFe, xml, versaoQRCode, ambiente);
                const string qrHomologacao = "https://consumidor.example.invalid/qr/homologacao";
                const string qrProducao = "https://consumidor.example.invalid/qr/producao";
                const string chaveHomologacao = "https://consumidor.example.invalid/chave/homologacao";
                const string chaveProducao = "https://consumidor.example.invalid/chave/producao";
                if (informarUrls)
                {
                    configuracao.UrlQrCodeHomologacao = qrHomologacao;
                    configuracao.UrlQrCodeProducao = qrProducao;
                    configuracao.UrlChaveHomologacao = chaveHomologacao;
                    configuracao.UrlChaveProducao = chaveProducao;
                }

                var preservarUrls = tipoDFe == TipoDFe.NFe && informarUrls;
                var esperado = (XmlDocument)original.CloneNode(true);
                if (preservarUrls)
                {
                    // A normalização da NF-e recria o suplemento usando as URLs explícitas.
                    var qrEsperado = esperado.GetElementsByTagName(tagQRCode)[0];
                    var parametros = qrEsperado.InnerText.Substring(qrEsperado.InnerText.IndexOf('?'));
                    qrEsperado.InnerText = (ambiente == TipoAmbiente.Homologacao ? qrHomologacao : qrProducao) + parametros;
                    esperado.GetElementsByTagName("urlChave")[0].InnerText = ambiente == TipoAmbiente.Homologacao ? chaveHomologacao : chaveProducao;
                }

                var resultado = validador.ValidarServico(xml, configuracao);

                Assert.True(resultado.Validado, resultado.MensagemRetorno);
                Assert.Equal(esperado.OuterXml, resultado.XmlAssinado.OuterXml);
                Assert.Equal(preservarUrls ? qrHomologacao : controle.UrlQrCodeHomologacao, configuracao.UrlQrCodeHomologacao);
                Assert.Equal(preservarUrls ? qrProducao : controle.UrlQrCodeProducao, configuracao.UrlQrCodeProducao);
                Assert.Equal(preservarUrls ? chaveHomologacao : controle.UrlChaveHomologacao, configuracao.UrlChaveHomologacao);
                Assert.Equal(preservarUrls ? chaveProducao : controle.UrlChaveProducao, configuracao.UrlChaveProducao);
            }
        }

        [Theory]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void NFCe_LoteComSuplementoParcial_DevePrepararUrlsEValidarTodasAsNotas(TipoAmbiente ambiente)
        {
            var notas = new List<Business.DFe.Xml.NFe.NFe>();
            var validador = new ValidarEstruturaXML();
            for (var numero = 1; numero <= 2; numero++)
            {
                var xml = CriarXml(TipoDFe.NFCe, @"NFe\Resources\NFe1.xml", ambiente);
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(xml);
                modelo.InfNFeField.Ide.NNF = numero;
                modelo.InfNFeField.Id = null;
                xml = XMLUtility.Serializar(modelo);
                var preparado = validador.ValidarServico(xml, CriarConfiguracao(TipoDFe.NFCe, xml, 3, ambiente));
                Assert.True(preparado.Validado, preparado.MensagemRetorno);
                notas.Add(XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(xml));
            }
            notas[1].InfNFeSupl = null;
            var lote = new Business.DFe.Xml.NFe.EnviNFe
            {
                Versao = "4.00",
                IdLote = "000000000000001",
                IndSinc = SimNao.Sim,
                NFe = notas
            }.GerarXML();
            Assert.Single(lote.GetElementsByTagName("infNFeSupl").Cast<XmlNode>());
            var configuracao = CriarConfiguracao(TipoDFe.NFCe, lote, 3, ambiente);

            var resultado = validador.ValidarServico(lote, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal(2, lote.GetElementsByTagName("infNFeSupl").Count);
            Assert.Equal(2, lote.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Count);
            foreach (XmlElement nota in lote.GetElementsByTagName("NFe"))
            {
                var qr = Assert.Single(nota.GetElementsByTagName("qrCode").Cast<XmlNode>());
                Assert.StartsWith("http://www.fazenda.pr.gov.br/nfce/qrcode?", qr.InnerText);
                var assinatura = Assert.Single(nota.GetElementsByTagName("Signature", SignedXml.XmlDsigNamespaceUrl).Cast<XmlElement>());
                var signed = new SignedXml(lote);
                signed.LoadXml(assinatura);
                Assert.True(signed.CheckSignature());
            }
        }

        [Theory]
        [InlineData("NFe", TipoAmbiente.Homologacao)]
        [InlineData("NFe", TipoAmbiente.Producao)]
        [InlineData("enviNFe", TipoAmbiente.Homologacao)]
        [InlineData("enviNFe", TipoAmbiente.Producao)]
        public void NFe_IsoladaOuEmLote_DevePreservarUrlsExplicitas(string tagRaiz, TipoAmbiente ambiente)
        {
            var xml = CriarXml(TipoDFe.NFe, @"NFe\Resources\NFe1.xml", ambiente);
            if (tagRaiz == "enviNFe")
            {
                xml = new Business.DFe.Xml.NFe.EnviNFe
                {
                    Versao = "4.00",
                    IdLote = "000000000000001",
                    IndSinc = SimNao.Sim,
                    NFe = new List<Business.DFe.Xml.NFe.NFe> { XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(xml) }
                }.GerarXML();
            }
            var configuracao = CriarConfiguracao(TipoDFe.NFe, xml, 3, ambiente);
            const string urlHomologacao = "https://consumidor.example.invalid/qr/homologacao";
            const string urlProducao = "https://consumidor.example.invalid/qr/producao";
            configuracao.UrlQrCodeHomologacao = urlHomologacao;
            configuracao.UrlQrCodeProducao = urlProducao;
            configuracao.UrlChaveHomologacao = "https://consumidor.example.invalid/chave/homologacao";
            configuracao.UrlChaveProducao = "https://consumidor.example.invalid/chave/producao";

            var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            var qr = Assert.Single(xml.GetElementsByTagName("qrCode").Cast<XmlNode>());
            Assert.StartsWith((ambiente == TipoAmbiente.Homologacao ? urlHomologacao : urlProducao) + "?", qr.InnerText);
            Assert.Equal(urlHomologacao, configuracao.UrlQrCodeHomologacao);
            Assert.Equal(urlProducao, configuracao.UrlQrCodeProducao);
        }

        [Theory]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void NFe_UrlDeProducaoIsolada_DeveManterResolucaoAtualDoCatalogo(TipoAmbiente ambiente)
        {
            var xml = CriarXml(TipoDFe.NFe, @"NFe\Resources\NFe1.xml", ambiente);
            var configuracao = CriarConfiguracao(TipoDFe.NFe, xml, 3, ambiente);
            configuracao.UrlQrCodeProducao = "https://consumidor.example.invalid/qr/producao";

            var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            const string urlCatalogo = "http://www.fazenda.pr.gov.br/nfce/qrcode";
            Assert.Equal(urlCatalogo, configuracao.UrlQrCodeHomologacao);
            Assert.Equal(urlCatalogo, configuracao.UrlQrCodeProducao);
            Assert.StartsWith(urlCatalogo + "?", xml.GetElementsByTagName("qrCode")[0].InnerText);
        }

        private static XmlDocument CriarXml(TipoDFe tipoDFe, string arquivo, TipoAmbiente ambiente)
        {
            var xml = new XmlDocument { XmlResolver = null };
            using (var reader = XmlReader.Create(Path.Combine("..", "..", "..", arquivo), new XmlReaderSettings { DtdProcessing = DtdProcessing.Prohibit, XmlResolver = null }))
            {
                xml.Load(reader);
            }

            if (tipoDFe == TipoDFe.MDFe)
            {
                var mdfe = xml.DocumentElement["MDFe"];
                var documento = new XmlDocument { XmlResolver = null };
                documento.AppendChild(documento.ImportNode(mdfe, true));
                xml = documento;
            }

            foreach (var node in xml.SelectNodes("//*[local-name()='Signature' or starts-with(local-name(), 'inf') and substring(local-name(), string-length(local-name()) - 3)='Supl']").Cast<XmlNode>().ToList())
            {
                node.ParentNode.RemoveChild(node);
            }

            if (tipoDFe == TipoDFe.DCe)
            {
                // A massa de serialização possui Id ilustrativo; gerar uma chave consistente para validar o schema.
                var modelo = XMLUtility.Deserializar<Business.DFe.Xml.DCe.DCe>(xml);
                modelo.InfDCe.Id = null;
                xml = modelo.GerarXML();
            }

            var ide = xml.SelectSingleNode("//*[local-name()='ide']");
            ide.SelectSingleNode("*[local-name()='tpAmb']").InnerText = ((int)ambiente).ToString();
            if (tipoDFe == TipoDFe.NFe || tipoDFe == TipoDFe.NFCe)
            {
                ide.SelectSingleNode("*[local-name()='mod']").InnerText = tipoDFe == TipoDFe.NFe ? "55" : "65";
                ide.SelectSingleNode("*[local-name()='tpImp']").InnerText = tipoDFe == TipoDFe.NFe ? "6" : "4";
            }

            return xml;
        }

        private Configuracao CriarConfiguracao(TipoDFe tipoDFe, XmlDocument xml, int versaoQRCode, TipoAmbiente ambiente)
        {
            var ide = xml.SelectSingleNode("//*[local-name()='ide']");
            return new Configuracao
            {
                TipoDFe = tipoDFe,
                CodigoUF = int.Parse(ide.SelectSingleNode("*[local-name()='cUF']").InnerText),
                TipoAmbiente = ambiente,
                TipoEmissao = (TipoEmissao)int.Parse(ide.SelectSingleNode("*[local-name()='tpEmis']").InnerText),
                CertificadoDigital = _certificado,
                CSC = "0123456789ABCDEF0123456789ABCDEF",
                CSCIDToken = 1,
                VersaoQRCodeNFCe = versaoQRCode
            };
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=TESTE SINTETICO URLS QR CODE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            }
        }
    }
}
