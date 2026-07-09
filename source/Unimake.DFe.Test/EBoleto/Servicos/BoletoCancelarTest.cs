using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class BoletoCancelarTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar cancelamento/baixa de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void CancelarBoleto()
        {
            var xml = new BoletoCancelar
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                NumeroNoBanco = PropConfig.EBoletoNumeroNoBancoTeste,
                Testing = true,
                TestingSpecified = true
            };

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoCancelar(xml, CriarConfiguracao(Servico.EBoletoCancelar)),
                TemConfiguracaoEBoletoValida(true));
        }
    }
}
