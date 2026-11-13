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

            var configuracao = new Configuracao
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
            var validador = new ValidarEstruturaXML();

            var resultado = validador.ValidarServico(xml, configuracao);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            var qrCode = Assert.Single(xml.GetElementsByTagName(tagQRCode).Cast<XmlNode>());
            Assert.Equal(xml.DocumentElement.NamespaceURI, qrCode.NamespaceURI);
            Assert.StartsWith(urlEsperada + "?", qrCode.InnerText);
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
