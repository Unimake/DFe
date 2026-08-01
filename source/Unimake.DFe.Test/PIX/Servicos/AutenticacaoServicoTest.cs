using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.PIX.Servicos
{
    public class AutenticacaoServicoTest : PIXTestBase
    {
        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveAdquirirTokenSomenteAoExecutar()
        {
            var configuracao = CriarConfiguracao(Servico.PIXConsultar);
            configuracao.AppId = "APP-TESTE";
            configuracao.Secret = "SEGREDO-TESTE";

            var servico = new Business.DFe.Servicos.PIX.PixConsultar(CriarPixConsultar(), configuracao);

            Assert.NotNull(servico.Configuracoes);
            Assert.Null(servico.Configuracoes.MunicipioToken);
        }
    }
}
