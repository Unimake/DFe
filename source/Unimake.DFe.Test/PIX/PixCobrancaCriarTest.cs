using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.PIX
{
    public class PixCobrancaCriarTest : PIXTestBase
    {
        /// <summary>
        /// Testar criacao de cobranca PIX (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void CriarCobrancaPIX()
        {
            var xml = CriarPixCobrancaCriar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.PIX.PixCobrancaCriar(xml, CriarConfiguracao(Servico.PIXCobrancaCriar)),
                TemConfiguracaoPIXValida());
        }
    }
}
