using System.Reflection;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Validacao
{
    public class NormalizacaoXMLTest
    {
        [Theory]
        [InlineData(TipoDFe.NFe, "NFe")]
        [InlineData(TipoDFe.NFCe, "envEvento")]
        [InlineData(TipoDFe.CTe, "consStatServCte")]
        [InlineData(TipoDFe.CTe, "consStatServCTe")]
        [InlineData(TipoDFe.MDFe, "consMDFeNaoEnc")]
        [InlineData(TipoDFe.NFCom, "eventoNFCom")]
        [InlineData(TipoDFe.NFGas, "consSitNFGas")]
        [InlineData(TipoDFe.BPe, "BPeTA")]
        [InlineData(TipoDFe.NF3e, "consReciNF3e")]
        [InlineData(TipoDFe.DCe, "eventoDCe")]
        [InlineData(TipoDFe.CCG, "consGTIN")]
        [InlineData(TipoDFe.CIOT, "GerarIdOperacaoTransporte")]
        [InlineData(TipoDFe.GNRE, "TLote_GNRE")]
        [InlineData(TipoDFe.DARE, "DareLote")]
        [InlineData(TipoDFe.EFDReinf, "Reinf")]
        [InlineData(TipoDFe.ESocial, "eSocial")]
        public void DeveNormalizarRaizesRegistradas(TipoDFe tipoDFe, string tagRaiz)
        {
            Assert.True(DeveNormalizar(tipoDFe, tagRaiz));
        }

        [Theory]
        [InlineData(TipoDFe.NFe, "nfe")]
        [InlineData(TipoDFe.CTe, "retConsStatServCTe")]
        [InlineData(TipoDFe.MDFe, "")]
        [InlineData(TipoDFe.NFCom, "retNFCom")]
        [InlineData(TipoDFe.NFGas, "nfgas")]
        [InlineData(TipoDFe.BPe, "retBPe")]
        [InlineData(TipoDFe.NF3e, "retNF3e")]
        [InlineData(TipoDFe.DCe, "dce")]
        [InlineData(TipoDFe.CCG, "retConsGTIN")]
        [InlineData(TipoDFe.CIOT, "RetornoOperacaoTransporte")]
        [InlineData(TipoDFe.GNRE, "TRetLote_GNRE")]
        [InlineData(TipoDFe.DARE, "RetDare")]
        [InlineData(TipoDFe.EFDReinf, "reinf")]
        [InlineData(TipoDFe.ESocial, "ESocial")]
        public void NaoDeveNormalizarRaizesNaoRegistradas(TipoDFe tipoDFe, string tagRaiz)
        {
            Assert.False(DeveNormalizar(tipoDFe, tagRaiz));
        }

        private static bool DeveNormalizar(TipoDFe tipoDFe, string tagRaiz)
        {
            var metodo = typeof(ValidarEstruturaXML).GetMethod(
                "DeveNormalizarXmlPeloObjeto",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (bool)metodo.Invoke(null, new object[] { tipoDFe, tagRaiz });
        }
    }
}
