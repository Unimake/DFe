using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeAutorizacaoBPeTM = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de autorização do BPe TM
    /// </summary>
    public class AutorizacaoBPeTMTest : BPeServicoTestBase
    {
        /// <summary>
        /// Autorizar BPe TM.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTM")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTM_minimo.xml")]
        public void AutorizacaoBPeTM(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTM.BPeTM().LerXML<Business.DFe.Xml.BPeTM.BPeTM>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPeTM(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal((int)bpeObjeto.InfBPe.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(bpeObjeto.InfBPe.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetBPe>(autorizacao.Result);
        }
    }
}
