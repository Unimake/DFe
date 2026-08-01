using System.Threading.Tasks;
using Newtonsoft.Json.Linq;
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

#pragma warning disable CS0618
            ExecutarTesteServico(
                () => new Business.DFe.Servicos.EBoleto.BoletoInformarPagto(xml, CriarConfiguracao(Servico.EBoletoInformarPagt)),
                TemConfiguracaoEBoletoValida(true));
#pragma warning restore CS0618
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public async Task DeveManterMesmoPayloadEEndpointDoBoletoCancelar()
        {
            const string configurationId = "CONFIGURACAO-TESTE";
            const string numeroNoBanco = "NUMERO-BANCO-TESTE";

            var informarPagtoXml = new BoletoInformarPagto
            {
                ConfigurationId = configurationId,
                NumeroNoBanco = numeroNoBanco,
                Testing = true,
                TestingSpecified = true
            };
            var cancelarXml = new BoletoCancelar
            {
                ConfigurationId = configurationId,
                NumeroNoBanco = numeroNoBanco,
                Testing = true,
                TestingSpecified = true
            };

#pragma warning disable CS0618
            var informarPagto = new Business.DFe.Servicos.EBoleto.BoletoInformarPagto(
                informarPagtoXml,
                CriarConfiguracao(Servico.EBoletoInformarPagt));
#pragma warning restore CS0618
            var cancelar = new Business.DFe.Servicos.EBoleto.BoletoCancelar(
                cancelarXml,
                CriarConfiguracao(Servico.EBoletoCancelar));

            Assert.Equal(cancelar.Configuracoes.RequestURIHomologacao, informarPagto.Configuracoes.RequestURIHomologacao);
            Assert.Equal(cancelar.Configuracoes.RequestURIProducao, informarPagto.Configuracoes.RequestURIProducao);
            Assert.Equal(cancelar.Configuracoes.MetodoAPI, informarPagto.Configuracoes.MetodoAPI);

            var payloadInformarPagto = await informarPagto.Configuracoes.HttpContent
                .ReadAsStringAsync(TestContext.Current.CancellationToken);
            var payloadCancelar = await cancelar.Configuracoes.HttpContent
                .ReadAsStringAsync(TestContext.Current.CancellationToken);

            Assert.True(JToken.DeepEquals(JToken.Parse(payloadCancelar), JToken.Parse(payloadInformarPagto)));
        }
    }
}
