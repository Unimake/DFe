using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class BoletoEnviarInstrucaoTest : EBoletoTestBase
    {
        /// <summary>
        /// Testar envio de instrução de boleto via eBoleto (sandbox)
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void EnviarInstrucaoBoleto()
        {
            var xml = new BoletoEnviarInstrucao
            {
                ConfigurationId = PropConfig.EBoletoConfigurationId,
                Data = DateTimeOffset.Now.Date,
                Instrucao = EBoletoInstrucao.Codigo3,
                NumeroNoBanco = PropConfig.EBoletoNumeroNoBancoTeste,
                Valor = 0,
                ValorSpecified = true,
                Testing = true,
                TestingSpecified = true
            };

            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoEnviarInstrucao(xml, CriarConfiguracao(Servico.EBoletoEnviarInstrucao)),
                TemConfiguracaoEBoletoValida(true));
        }
    }
}
