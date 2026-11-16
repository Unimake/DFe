using System;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFe.Validacao
{
    [Trait("DFe", "NFe")]
    public class EventoSimplesNacionalValidacaoTest
    {
        [Theory]
        [InlineData(TipoDFe.NFe, false)]
        [InlineData(TipoDFe.NFe, true)]
        [InlineData(TipoDFe.NFCe, false)]
        [InlineData(TipoDFe.NFCe, true)]
        public void CatalogoCentralSelecionaSchemaDoEvento412140(TipoDFe tipoDFe, bool invalido)
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFe\Resources\EventoTributacaoSimplesNacional.xml");
            if (tipoDFe == TipoDFe.NFCe)
            {
                xml.GetElementsByTagName("chNFe")[0].InnerText = "41261100000000000191650010000000011000000010";
                ((XmlElement)xml.GetElementsByTagName("infEvento")[0]).SetAttribute("Id", "ID4121404126110000000000019165001000000001100000001001");
            }
            if (invalido)
            {
                xml.GetElementsByTagName("tpAutor")[0].InnerText = "1";
            }

            using (var rsa = RSA.Create(2048))
            {
                var request = new CertificateRequest("CN=EventoSimplesNacionalTest", rsa,
                    HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                using (var certificado = request.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1)))
                {
                    var configuracao = new Configuracao
                    {
                        TipoDFe = tipoDFe, TipoAmbiente = TipoAmbiente.Homologacao, CodigoUF = 41,
                        PadraoNFSe = PadraoNFSe.None, CertificadoDigital = certificado
                    };
                    var resultado = new ValidarEstruturaXML().ValidarServico(xml, configuracao);
                    if (invalido)
                    {
                        Assert.False(resultado.Validado);
                        Assert.Contains("Erro ao validar schema específico:", resultado.MensagemRetorno);
                        Assert.Contains("tpAutor", resultado.MensagemRetorno);
                    }
                    else
                    {
                        Assert.True(resultado.Validado, resultado.MensagemRetorno);
                        Assert.Equal("1", resultado.StatusValidacao);
                        Assert.Same(xml, resultado.XmlAssinado);
                    }
                }
            }
        }
    }
}
