using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.PIX.Servicos
{
    public class PixConsultarTest : PIXTestBase
    {
        /// <summary>
        /// Testar consulta de PIX (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void ConsultarPIX()
        {
            var xml = CriarPixConsultar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.PIX.PixConsultar(xml, CriarConfiguracao(Servico.PIXConsultar)),
                TemConfiguracaoPIXValida(exigeTxId: true));
        }
    }
}
