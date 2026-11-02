using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFCom.Validacao
{
    public class EventosSimplesNacionalValidacaoTest
    {
        private static XmlDocument LerEvento(string codigo)
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFCom\Resources\eventoNFCom-" + codigo + ".xml");
            // O fluxo centralizado assina e normaliza o XML usando um certificado
            // temporário do próprio teste, sem arquivo PFX nem acesso ao webservice.
            var assinatura = xml.GetElementsByTagName("Signature", "http://www.w3.org/2000/09/xmldsig#")[0];
            assinatura.ParentNode.RemoveChild(assinatura);
            return xml;
        }

        private static ValidarEstruturaXML.ResultadoValidacao Validar(XmlDocument xml)
        {
            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=EventosNFComValidacaoTest", rsa,
                    HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);

                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var configuracao = new Configuracao
                    {
                        TipoDFe = TipoDFe.NFCom,
                        TipoAmbiente = TipoAmbiente.Homologacao,
                        CodigoUF = 41,
                        PadraoNFSe = PadraoNFSe.None,
                        CertificadoDigital = certificado
                    };

                    return new ValidarEstruturaXML().ValidarServico(xml, configuracao);
                }
            }
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData("412140")]
        [InlineData("412141")]
        public void DeveValidarEventoPeloCatalogoCentralizado(string codigo)
        {
            var xml = LerEvento(codigo);
            var resultado = Validar(xml);

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal("1", resultado.StatusValidacao);
            Assert.Same(xml, resultado.XmlAssinado);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData("412140")]
        [InlineData("412141")]
        public void DeveRejeitarPayloadInvalidoPeloSchemaEspecifico(string codigo)
        {
            var xml = LerEvento(codigo);
            xml.GetElementsByTagName("nProt", "http://www.portalfiscal.inf.br/nfcom")[0].InnerText = "INVALIDO";

            var resultado = Validar(xml);

            Assert.False(resultado.Validado);
            Assert.Contains("Erro ao validar schema específico:", resultado.MensagemRetorno);
            Assert.Contains("nProt", resultado.MensagemRetorno);
            Assert.Contains("INVALIDO", resultado.MensagemRetorno);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData("412140")]
        [InlineData("412141")]
        public void DeveRejeitarEnvelopeInvalidoPeloSchemaGeral(string codigo)
        {
            var xml = LerEvento(codigo);
            xml.GetElementsByTagName("nSeqEvento", "http://www.portalfiscal.inf.br/nfcom")[0].InnerText = "1000";

            var resultado = Validar(xml);

            Assert.False(resultado.Validado);
            Assert.Contains("Erro ao validar schema geral:", resultado.MensagemRetorno);
            Assert.Contains("nSeqEvento", resultado.MensagemRetorno);
        }
    }
}
