using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class BoletoAlterarVenctoTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar alteração de vencimento de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void AlterarVencimentoBoleto()
        {
            var xml = new BoletoAlterarVencto
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                DataVencimento = DateTimeOffset.Now.Date.AddDays(10),
                NumeroNoBanco = PropConfig.EBoletoNumeroNoBancoTeste,
                Testing = true,
                TestingSpecified = true
            };

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoAlterarVencto(xml, CriarConfiguracao(Servico.EBoletoAlterarVencto)),
                TemConfiguracaoEBoletoValida(true));
        }
    }
}
