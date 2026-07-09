using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class BoletoConsultarTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar consulta de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void ConsultarBoleto()
        {
            var xml = new BoletoConsultar
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                NumerosNoBanco = new EBoletoNumerosNoBanco
                {
                    NumeroNoBanco = new List<string>
                    {
                        PropConfig.EBoletoNumeroNoBancoTeste
                    }
                },
                PageNumber = 1,
                PageNumberSpecified = true,
                PageSize = 10,
                PageSizeSpecified = true,
                Testing = true,
                TestingSpecified = true
            };

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoConsultar(xml, CriarConfiguracao(Servico.EBoletoConsultar)),
                TemConfiguracaoEBoletoValida(true));
        }
    }
}
