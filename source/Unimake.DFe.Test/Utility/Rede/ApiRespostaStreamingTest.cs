using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Trait("Utility", "Rede")]
    public class ApiRespostaStreamingTest
    {
        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void HandlerRespeitaPoliticasDeCookieERedirecionamento(bool habilitar)
        {
            var request = new TransportRequest { UseProxy = false, UseCookies = habilitar, AllowAutoRedirect = habilitar };
            var metodo = typeof(ApiTransportExecutor).GetMethod("CriarHttpClientHandler", BindingFlags.NonPublic | BindingFlags.Instance);
            using var handler = (HttpClientHandler)metodo.Invoke(new ApiTransportExecutor(), new object[] { request });
            Assert.Equal(habilitar, handler.UseCookies);
            Assert.Equal(habilitar, handler.AllowAutoRedirect);
            Assert.False(handler.UseProxy);
            Assert.True(new TransportRequest().UseCookies);
            Assert.True(new TransportRequest().AllowAutoRedirect);
            Assert.False(new TransportRequest().ResponseHeadersRead);
        }

        [Fact]
        public async Task RetornaHeadersAntesDoCorpoEMantemConexaoAteDispose()
        {
            using var listener = new TcpListener(IPAddress.Loopback, 0);
            listener.Start();
            var liberarCorpo = new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);
            var servidor = Task.Run(async () =>
            {
                using var cliente = await listener.AcceptTcpClientAsync();
                using var stream = cliente.GetStream();
                using var reader = new StreamReader(stream, Encoding.ASCII, false, 1024, true);
                while (!string.IsNullOrEmpty(await reader.ReadLineAsync())) { }
                await stream.WriteAsync(Encoding.ASCII.GetBytes("HTTP/1.1 200 OK\r\nContent-Length: 4\r\nConnection: close\r\n\r\n"));
                await stream.FlushAsync();
                await liberarCorpo.Task.WaitAsync(TimeSpan.FromSeconds(10));
                await stream.WriteAsync(Encoding.ASCII.GetBytes("BODY"));
            }, TestContext.Current.CancellationToken);
            try
            {
                var porta = ((IPEndPoint)listener.LocalEndpoint).Port;
                var executar = Task.Run(() => new ApiTransportExecutor().Execute(new TransportRequest
                {
                    Method = "GET", RequestUri = "http://127.0.0.1:" + porta + "/teste",
                    Timeout = 5000, UseProxy = false, ResponseHeadersRead = true,
                    DisableAutomaticClientCertificateSelection = true, UseCookies = false, AllowAutoRedirect = false
                }), TestContext.Current.CancellationToken);
                using var resposta = await executar.WaitAsync(TimeSpan.FromSeconds(8), TestContext.Current.CancellationToken);
                Assert.Equal(HttpStatusCode.OK, resposta.StatusCode);
                Assert.NotNull(resposta.ResponseLifetime);
                liberarCorpo.TrySetResult();
                Assert.Equal("BODY", await resposta.HttpResponseMessage.Content.ReadAsStringAsync(TestContext.Current.CancellationToken));
                await servidor;
            }
            finally
            {
                liberarCorpo.TrySetResult();
                listener.Stop();
            }
        }
    }
}
