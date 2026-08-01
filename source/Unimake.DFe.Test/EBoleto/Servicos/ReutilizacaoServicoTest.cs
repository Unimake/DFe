using System.Reflection;
using System.Threading.Tasks;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe.Servicos;
using Xunit;
using BoletoCancelarXml = Unimake.Business.DFe.Xml.EBoleto.BoletoCancelar;
using BoletoCancelarServico = Unimake.Business.DFe.Servicos.EBoleto.BoletoCancelar;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class ReutilizacaoServicoTest : EBoletoTestBase
    {
        [Fact]
        [Trait("DFe", "EBoleto")]
        public async Task DeveReconstruirPayloadAoReutilizarMesmaInstancia()
        {
            var configuracao = CriarConfiguracao(Servico.EBoletoCancelar);
            var servico = new BoletoCancelarServico(CriarEnvio("BANCO-PRIMEIRO"), configuracao);

            var primeiroPayload = JObject.Parse(await CriarPayload(servico)
                .ReadAsStringAsync(TestContext.Current.CancellationToken));

            Reinicializar(servico, CriarEnvio("BANCO-SEGUNDO"), configuracao);
            var segundoPayload = JObject.Parse(await CriarPayload(servico)
                .ReadAsStringAsync(TestContext.Current.CancellationToken));

            Assert.Equal("BANCO-PRIMEIRO", primeiroPayload.Value<string>("numeroNoBanco"));
            Assert.Equal("BANCO-SEGUNDO", segundoPayload.Value<string>("numeroNoBanco"));
        }

        private static BoletoCancelarXml CriarEnvio(string numeroNoBanco) => new BoletoCancelarXml
        {
            ConfigurationId = "CONFIGURACAO-TESTE",
            NumeroNoBanco = numeroNoBanco,
            Testing = true,
            TestingSpecified = true
        };

        private static System.Net.Http.HttpContent CriarPayload(BoletoCancelarServico servico)
        {
            var tipoBase = typeof(Business.DFe.Servicos.EBoleto.ServicoBase<BoletoCancelarXml>);
            var metodo = tipoBase.GetMethod("CriarHttpContentPadrao", BindingFlags.Instance | BindingFlags.NonPublic);

            return (System.Net.Http.HttpContent)metodo.Invoke(servico, null);
        }

        private static void Reinicializar(BoletoCancelarServico servico, BoletoCancelarXml xml, Configuracao configuracao)
        {
            var tipoBase = typeof(Business.DFe.Servicos.EBoleto.ServicoBase<BoletoCancelarXml>);
            var metodo = tipoBase.GetMethod(
                "InicializarServico",
                BindingFlags.Instance | BindingFlags.NonPublic,
                null,
                new[] { typeof(BoletoCancelarXml), typeof(Configuracao) },
                null);

            metodo.Invoke(servico, new object[] { xml, configuracao });
        }
    }
}
