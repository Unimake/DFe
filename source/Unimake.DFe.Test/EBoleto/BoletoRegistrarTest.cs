using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.EBoleto
{
    public class BoletoRegistrarTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar registro de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void RegistrarBoleto()
        {
            var xml = CriarBoletoRegistrar();

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoRegistrar(xml, CriarConfiguracao(Servico.EBoletoRegistrar)),
                TemConfiguracaoEBoletoValida());
        }
    }
}
