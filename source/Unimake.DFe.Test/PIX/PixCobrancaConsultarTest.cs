using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.PIX
{
    public class PixCobrancaConsultarTest : PIXTestBase
    {
        /// <summary>
        /// Testar consulta de cobranca PIX (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void ConsultarCobrancaPIX()
        {
            var xml = CriarPixCobrancaConsultar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.PIX.PixCobrancaConsultar(xml, CriarConfiguracao(Servico.PIXCobrancaConsultar)),
                TemConfiguracaoPIXValida());
        }
    }
}
