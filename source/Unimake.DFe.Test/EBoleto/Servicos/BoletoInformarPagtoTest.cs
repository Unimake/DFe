using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class BoletoInformarPagtoTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar informação de pagamento de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void InformarPagamentoBoleto()
        {
            var xml = new BoletoInformarPagto
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                NumeroNoBanco = PropConfig.EBoletoNumeroNoBancoTeste,
                Testing = true,
                TestingSpecified = true
            };

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoInformarPagto(xml, CriarConfiguracao(Servico.EBoletoInformarPagt)),
                TemConfiguracaoEBoletoValida(true));
        }
    }
}
