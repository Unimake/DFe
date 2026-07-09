using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeAutorizacaoBPe = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de autorização do BPe
    /// </summary>
    public class AutorizacaoBPeTest : BPeServicoTestBase
    {
        /// <summary>
        /// Autorizar BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void AutorizacaoBPe(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPe.BPe().LerXML<Business.DFe.Xml.BPe.BPe>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPe(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal((int)bpeObjeto.InfBPe.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(bpeObjeto.InfBPe.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetBPe>(autorizacao.Result);
        }

        /// <summary>
        /// Gerar QRCode automaticamente no XML do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void GerarQRCodeBPe(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPe.BPe().LerXML<Business.DFe.Xml.BPe.BPe>(xml);
            bpeObjeto.Signature = null;
            bpeObjeto.InfBPeSupl = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPe(bpeObjeto, configuracao);

            AssertQRCodeBPe(autorizacao.ConteudoXMLAssinado, bpeObjeto.InfBPe.Chave, bpeObjeto.InfBPe.Ide.TpAmb);
        }
    }
}
