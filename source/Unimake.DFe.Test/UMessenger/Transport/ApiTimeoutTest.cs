using System;
using System.Net.Http;
using System.Reflection;
using Unimake.Business.DFe;
using Xunit;

namespace Unimake.DFe.Test.UMessenger.Transport
{
    public class ApiTimeoutTest
    {
        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveManterTimeoutPadraoQuandoNaoInformado()
        {
            using (var client = CriarHttpClient(0))
            {
                Assert.Equal(TimeSpan.FromSeconds(100), client.Timeout);
            }
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveAplicarTimeoutDeTresMinutosParaAnexos()
        {
            using (var client = CriarHttpClient(180000))
            {
                Assert.Equal(TimeSpan.FromSeconds(180), client.Timeout);
            }
        }

        private static HttpClient CriarHttpClient(int timeout)
        {
            var assembly = typeof(APIConfig).Assembly;
            var mapperType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Compatibility.ApiConfigTransportRequestMapper", true);
            var executorType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Transport.ApiTransportExecutor", true);

            var configuracao = new APIConfig
            {
                RequestURI = "https://example.test/",
                Timeout = timeout
            };

            var mapper = Activator.CreateInstance(mapperType, true);
            var request = mapperType.GetMethod("Map", BindingFlags.Instance | BindingFlags.Public)
                .Invoke(mapper, new object[] { configuracao, null });

            var executor = Activator.CreateInstance(executorType, true);
            return (HttpClient)executorType
                .GetMethod("CreateClient", BindingFlags.Instance | BindingFlags.NonPublic)
                .Invoke(executor, new[] { request });
        }
    }
}
