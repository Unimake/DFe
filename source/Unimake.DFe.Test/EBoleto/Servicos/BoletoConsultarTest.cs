using System.Collections.Generic;
using System.Threading.Tasks;
using Newtonsoft.Json.Linq;
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

        /// <summary>
        /// Garante os valores padrão historicamente enviados pelo UniNFe
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public async Task DeveAplicarValoresPadraoDePaginacao()
        {
            var xml = new BoletoConsultar
            {
                ConfigurationId = "CONFIG_TESTE",
                Testing = true,
                TestingSpecified = true
            };

            using (var servico = new Business.DFe.Servicos.EBoleto.BoletoConsultar(xml, CriarConfiguracao(Servico.EBoletoConsultar)))
            {
                var conteudo = await servico.Configuracoes.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken);
                var json = JObject.Parse(conteudo);

                Assert.Equal(1, json.Value<int>("pageNumber"));
                Assert.Equal(50, json.Value<int>("pageSize"));
            }
        }
    }
}
