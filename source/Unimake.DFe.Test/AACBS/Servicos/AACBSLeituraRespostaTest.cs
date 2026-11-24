using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSLeituraRespostaTest
    {
        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void FalhaAposHeadersPreservaHttpLiberaTransporteENaoExpoeConteudo(bool timeout)
        {
            var transporte = new Executor(timeout);
            using var substituir = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            var erro = Record.Exception(servico.Executar);
            if (timeout) Assert.IsType<TimeoutException>(erro);
            else Assert.IsType<HttpRequestException>(erro);
            Assert.Equal(HttpStatusCode.OK, servico.HttpStatusCode);
            Assert.Null(servico.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", servico.Result.ErroLocal.Codigo);
            Assert.DoesNotContain("SENTINELA-CORPO-PRIVADO", erro.ToString());
            Assert.True(transporte.Descartado);
            Assert.Equal(1, transporte.Chamadas);
        }

        private sealed class Executor : IApiTransportExecutor, IDisposable
        {
            private readonly bool timeout;
            internal bool Descartado { get; private set; }
            internal int Chamadas { get; private set; }
            internal Executor(bool timeout) => this.timeout = timeout;
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                Assert.True(request.ResponseHeadersRead);
                Assert.False(request.AllowAutoRedirect);
                Assert.False(request.UseCookies);
                Assert.False(request.UseDefaultCredentials);
                Assert.False(request.UseCertificate);
                Assert.False(request.UseProxy);
                Assert.True(request.DisableAutomaticClientCertificateSelection);
                Assert.Empty(request.Headers);
                return new TransportResponse { StatusCode = HttpStatusCode.OK, ContentStream = new CorpoComFalha(timeout), ResponseLifetime = this };
            }
            public void Dispose() => Descartado = true;
        }

        private sealed class CorpoComFalha : MemoryStream
        {
            private readonly bool timeout;
            internal CorpoComFalha(bool timeout) => this.timeout = timeout;
            public override Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken) =>
                Task.FromException<int>(timeout ? new OperationCanceledException("SENTINELA-CORPO-PRIVADO") : new IOException("SENTINELA-CORPO-PRIVADO"));
        }
    }
}
