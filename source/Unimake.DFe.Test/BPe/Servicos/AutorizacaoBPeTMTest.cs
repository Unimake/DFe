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

        /// <summary>
        /// Autorizar em homologação o BPe TM do Expresso Amarelinho.
        /// </summary>
        [Fact]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTM")]
        public void AutorizarBPeTMAmarelinhoEmHomologacao()
        {
            const string arqXML = @"C:\projetos\certificados\DosClientes\BPeTM-Cert_Amarelinho_10062027_senha 123321-bpe-tm.xml";
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTM.BPeTM().LerXML<Business.DFe.Xml.BPeTM.BPeTM>(xml);

            Assert.Equal(TipoAmbiente.Homologacao, bpeObjeto.InfBPe.Ide.TpAmb);

            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);
            configuracao.TipoAmbiente = TipoAmbiente.Homologacao;

            var autorizacao = new BPeAutorizacaoBPeTM(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.NotNull(autorizacao.Result.ProtBPe);
            Assert.NotNull(autorizacao.Result.ProtBPe.InfProt);
            Assert.True(
                autorizacao.Result.ProtBPe.InfProt.CStat == 100,
                $"BPe TM não autorizado. cStat: {autorizacao.Result.ProtBPe.InfProt.CStat} - {autorizacao.Result.ProtBPe.InfProt.XMotivo}");
        }
    }
}
