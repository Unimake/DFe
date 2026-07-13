using System.Xml;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.BPe.Servicos
{
    public abstract class BPeServicoTestBase
    {
        protected static Configuracao CriarConfiguracao(UFBrasil ufBrasil)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.BPe,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)ufBrasil,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }

        protected static void AssertQRCodeBPe(XmlDocument xml, string chave, TipoAmbiente tipoAmbiente)
        {
            var qrCodBPe = xml.GetElementsByTagName("qrCodBPe");
            Assert.Equal(1, qrCodBPe.Count);

            var qrCode = qrCodBPe[0].InnerText;
            Assert.StartsWith("https://", qrCode);
            Assert.Contains("?chBPe=", qrCode);
            Assert.Contains("&tpAmb=" + ((int)tipoAmbiente).ToString(), qrCode);

            var infBPeSupl = xml.GetElementsByTagName("infBPeSupl");
            Assert.Equal(1, infBPeSupl.Count);
            Assert.NotNull(infBPeSupl[0].NextSibling);
            Assert.Equal("Signature", infBPeSupl[0].NextSibling.LocalName);
        }
    }
}
