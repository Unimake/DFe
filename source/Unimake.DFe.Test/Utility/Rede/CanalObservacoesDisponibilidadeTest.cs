using System;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    public class CanalObservacoesDisponibilidadeTest
    {
        private static Configuracao Config(TipoDFe tipo = TipoDFe.NFe) => new Configuracao
        {
            ColetarTelemetriaDisponibilidade = true,
            TipoDFe = tipo,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CodigoUF = 35,
            CodigoMunicipio = 3550308
        };

        private static XmlDocument Retorno(int codigo)
        {
            var doc = new XmlDocument { XmlResolver = null };
            doc.LoadXml("<ret><cStat>" + codigo + "</cStat><prot><cStat>108</cStat></prot></ret>");
            return doc;
        }

        private static void Executar(CanalObservacoesDisponibilidade canal, Configuracao config, int codigo = 204)
        {
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://servico.test/segredo?token=secreto", "SOAP");
                sessao.Concluir(Retorno(codigo), HttpStatusCode.OK);
            }
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void OptInDesabilitadoNaoPublicaNemContaPerda()
        {
            var canal = new CanalObservacoesDisponibilidade();
            var config = Config();
            config.ColetarTelemetriaDisponibilidade = false;
            Executar(canal, config);
            Assert.False(canal.TentarLer(out _));
            Assert.Equal(0, canal.ObservacoesDescartadas);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(TipoDFe.NFe)]
        [InlineData(TipoDFe.NFSe)]
        [InlineData(TipoDFe.CIOT)]
        [InlineData(TipoDFe.ESocial)]
        [InlineData(TipoDFe.EFDReinf)]
        [InlineData(TipoDFe.GNRE)]
        [InlineData(TipoDFe.DARE)]
        [InlineData(TipoDFe.CCG)]
        [InlineData(TipoDFe.PIX)]
        [InlineData(TipoDFe.EBoleto)]
        [InlineData(TipoDFe.UMessenger)]
        public void CanalNaoDependeDaListaDoDiagnosticoHistorico(TipoDFe tipo)
        {
            var canal = new CanalObservacoesDisponibilidade();
            Executar(canal, Config(tipo));
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(tipo, item.TipoDFe);
            Assert.Equal(204, item.CStat);
            Assert.Equal(StatusDisponibilidade.Operacional, item.Status);
            Assert.False(canal.TentarLer(out _));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FotografiaEhImutavelSemCorpoExcecaoOuConfiguracao()
        {
            var canal = new CanalObservacoesDisponibilidade();
            var config = Config();
            config.CertificadoArquivo = @"C:\privado\inexistente.pfx";
            config.CertificadoSenha = "segredo";
            Executar(canal, config);
            config.CodigoMunicipio = 1;
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(3550308, item.CodigoMunicipio);
            Assert.Equal("https://servico.test", item.Destino);
            Assert.Null(config.CertificadoDigitalCarregado);
            Assert.All(typeof(ObservacaoDisponibilidade).GetProperties(), p => Assert.Null(p.SetMethod));
            Assert.DoesNotContain(typeof(ObservacaoDisponibilidade).GetFields(BindingFlags.Instance | BindingFlags.NonPublic),
                f => typeof(XmlNode).IsAssignableFrom(f.FieldType) || typeof(Exception).IsAssignableFrom(f.FieldType) ||
                f.FieldType == typeof(Configuracao));
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(108, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(109, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(999, StatusDisponibilidade.Indisponivel, TipoFalhaDisponibilidade.Protocolo)]
        [InlineData(656, StatusDisponibilidade.Degradado, TipoFalhaDisponibilidade.ConsumoIndevido)]
        [InlineData(678, StatusDisponibilidade.Degradado, TipoFalhaDisponibilidade.ConsumoIndevido)]
        [InlineData(107, StatusDisponibilidade.Operacional, TipoFalhaDisponibilidade.Nenhuma)]
        public void ClassificaPrimeiroCodigoSemMudarRegraFiscal(int codigo, StatusDisponibilidade status, TipoFalhaDisponibilidade falha)
        {
            var canal = new CanalObservacoesDisponibilidade();
            Executar(canal, Config(), codigo);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(codigo, item.CStat);
            Assert.Equal(status, item.Status);
            Assert.Equal(falha, item.TipoFalha);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(false, StatusDisponibilidade.Inconclusivo)]
        [InlineData(true, StatusDisponibilidade.Operacional)]
        public void Http200ExigeReconhecimentoDaAplicacao(bool reconhecida, StatusDisponibilidade esperado)
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.PIX), Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://api.test/recurso", "REST");
                sessao.Concluir(null, HttpStatusCode.OK, reconhecida);
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(esperado, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SemTransporteNaoAfirmaOperacaoRemota()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid()))
                sessao.Concluir(Retorno(107), HttpStatusCode.OK, true);
            Assert.True(canal.TentarLer(out var item));
            Assert.False(item.TransporteObservado);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SaturacaoDescartaSemBloquearEVoltaAAceitarAposDrenagem()
        {
            var canal = new CanalObservacoesDisponibilidade(1);
            Executar(canal, Config());
            Executar(canal, Config());
            Assert.Equal(1, canal.ObservacoesDescartadas);
            Assert.True(canal.TentarLer(out _));
            Executar(canal, Config());
            Assert.True(canal.TentarLer(out _));
            Assert.False(canal.TentarLer(out _));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SessaoAbandonadaEhPerdaESessaoConcluidaNaoDuplica()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid())) { }
            var sessao = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid());
            sessao.Concluir(null, 0);
            sessao.Falhar(new Exception("segredo"));
            sessao.Dispose();
            sessao.Dispose();
            Assert.Equal(1, canal.ObservacoesDescartadas);
            Assert.True(canal.TentarLer(out _));
            Assert.False(canal.TentarLer(out _));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task OperacoesConcorrentesMantemIdentidadeEContexto()
        {
            var canal = new CanalObservacoesDisponibilidade(128);
            var tarefas = Enumerable.Range(0, 100).Select(_ => Task.Run(() => Executar(canal, Config())));
            await Task.WhenAll(tarefas);
            var ids = new System.Collections.Generic.HashSet<Guid>();
            while (canal.TentarLer(out var item))
            {
                Assert.True(ids.Add(item.OperacaoId));
                Assert.Equal("https://servico.test", item.Destino);
            }
            Assert.Equal(100, ids.Count);
            Assert.Equal(0, canal.ObservacoesDescartadas);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void SessaoAninhadaRestauraContextoAnterior()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var externa = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid()))
            {
                using (var interna = canal.IniciarOperacao(Config(TipoDFe.PIX), Guid.NewGuid(), Guid.NewGuid()))
                {
                    SessaoObservacaoDisponibilidade.InformarTransporte("https://interna.test", "REST");
                    interna.Concluir(null, HttpStatusCode.OK, true);
                }
                SessaoObservacaoDisponibilidade.InformarTransporte("https://externa.test", "SOAP");
                externa.Concluir(Retorno(107), HttpStatusCode.OK);
            }
            Assert.True(canal.TentarLer(out var primeiro));
            Assert.True(canal.TentarLer(out var segundo));
            Assert.Equal("https://interna.test", primeiro.Destino);
            Assert.Equal("https://externa.test", segundo.Destino);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void FalhaRealDaApiPreservaExcecaoEIdentificaLoginSemDuplicarOperacao()
        {
            var canal = new CanalObservacoesDisponibilidade();
            var config = Config(TipoDFe.CIOT);
            config.RequestURILoginHomologacao = "https://api.test/login/segredo";
            var falha = new WebException("senha=segredo", new SocketException((int)SocketError.HostNotFound));
            var executor = new ExecutorFake(falha);
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                var recebida = Assert.Throws<WebException>(() => consumidor.ExecutarServico(new APIConfig
                {
                    RequestURI = config.RequestURILoginHomologacao,
                    MetodoAPI = "get",
                    TagRetorno = "prop:innertext",
                    UsaCertificadoDigital = false,
                    UseProxy = false
                }, null));
                Assert.Same(falha, recebida);
                sessao.Falhar(recebida);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal("Autenticacao", item.Etapa);
            Assert.Equal("https://api.test", item.Destino);
            Assert.Equal(TipoFalhaDisponibilidade.DNS, item.TipoFalha);
            Assert.False(canal.TentarLer(out _));
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumidorApiRealEhObservadoComTransporteOffline()
        {
            var canal = new CanalObservacoesDisponibilidade();
            var executor = new ExecutorFake();
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.NFSe), Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                consumidor.ExecutarServico(new APIConfig
                {
                    RequestURI = "https://municipio.test/nota/123456789?token=segredo",
                    MetodoAPI = "get",
                    TagRetorno = "prop:innertext",
                    UsaCertificadoDigital = false,
                    UseProxy = false
                }, null);
                sessao.Concluir(consumidor.RetornoServicoXML, consumidor.HttpStatusCode);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.True(item.TransporteObservado);
            Assert.Equal("REST", item.Protocolo);
            Assert.Equal("https://municipio.test", item.Destino);
            Assert.Equal(204, item.CStat);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(503, StatusDisponibilidade.Indisponivel)]
        [InlineData(500, StatusDisponibilidade.Degradado)]
        [InlineData(401, StatusDisponibilidade.Inconclusivo)]
        public void HttpDeFalhaEhPreservadoMesmoQuandoParsingFalha(int http, StatusDisponibilidade esperado)
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.NFSe), Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte("https://api.test", "REST");
                SessaoObservacaoDisponibilidade.InformarRespostaHttp((HttpStatusCode)http);
                sessao.Falhar(new ExcecaoSemMensagem());
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(http, item.HttpStatusCode);
            Assert.Equal(TipoFalhaDisponibilidade.HTTP, item.TipoFalha);
            Assert.Equal(esperado, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CertificadoAusenteNaoCarregaArquivoNemViraQuedaRemota()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid()))
                sessao.Falhar(new Unimake.Exceptions.CertificadoDigitalException());
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal(TipoFalhaDisponibilidade.Certificado, item.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.False(item.TransporteObservado);
        }

        private sealed class ExcecaoSemMensagem : Exception
        {
            public override string Message => throw new InvalidOperationException("O diagnóstico não deve acessar texto externo.");
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void CiotProducaoObservaComponenteExternoSemInventarHttpOuEndpoint()
        {
            var canal = new CanalObservacoesDisponibilidade();
            var config = Config(TipoDFe.CIOT);
            config.TipoAmbiente = TipoAmbiente.Producao;
            config.Definida = true;
            using (var sessao = canal.IniciarOperacao(config, Guid.NewGuid(), Guid.NewGuid()))
            {
                var servico = new AnttFake(new Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte
                    { CpfCnpj = "12345678000195" }, config);
                servico.Executar();
                Assert.Equal("123456789012", servico.Result.IdOperacaoTransporte);
                sessao.Concluir(servico.RetornoWSXML, 0, true);
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.True(item.TransporteObservado);
            Assert.Equal("ComponenteExterno", item.Protocolo);
            Assert.Equal(string.Empty, item.Destino);
            Assert.Equal(0, item.HttpStatusCode);
            Assert.Equal(TipoFalhaDisponibilidade.Nenhuma, item.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
            Assert.True(item.DuracaoTransporteMilissegundos >= 0);
        }

        private sealed class AnttFake : Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte
        {
            internal AnttFake(Unimake.Business.DFe.Xml.CIOT.GerarIdOperacaoTransporte xml, Configuracao config)
                : base(xml, config) { }
            protected override string GerarCIOTProducao(string cpfCnpj) => "123456789012";
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public void ConsumidorSoapRealObservaFalhaDeCertificadoAntesDeQualquerRede()
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirWS())
            {
                var falha = Assert.Throws<Unimake.Exceptions.CertificadoDigitalException>(() =>
                    consumidor.ExecutarServico(new XmlDocument(), new WSSoap
                    {
                        EnderecoWeb = "https://soap.test/segredo?token=secreto",
                        UsaCertificadoDigital = true
                    }, null));
                sessao.Falhar(falha);
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal("SOAP", item.Protocolo);
            Assert.Equal("https://soap.test", item.Destino);
            Assert.Equal(TipoFalhaDisponibilidade.Certificado, item.TipoFalha);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(null)]
        [InlineData("")]
        public void EndpointAusenteNaoEhConfundidoComLogin(string endpoint)
        {
            var canal = new CanalObservacoesDisponibilidade();
            using (var sessao = canal.IniciarOperacao(Config(), Guid.NewGuid(), Guid.NewGuid()))
            {
                SessaoObservacaoDisponibilidade.InformarTransporte(endpoint, "REST");
                sessao.Falhar(new InvalidOperationException());
            }
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal("Servico", item.Etapa);
            Assert.Equal(string.Empty, item.Destino);
            Assert.Equal(StatusDisponibilidade.Inconclusivo, item.Status);
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task TimeoutRealDoHttpClientPublicaTimeoutDegradadoSemRede()
        {
            using (var handler = new HandlerAguardaCancelamento())
            using (var client = new HttpClient(handler) { Timeout = TimeSpan.FromMilliseconds(100) })
            {
                var falha = await Assert.ThrowsAnyAsync<OperationCanceledException>(async () =>
                    await client.GetAsync("https://api.test/segredo?token=secreto", TestContext.Current.CancellationToken).WaitAsync(TimeSpan.FromSeconds(10), TestContext.Current.CancellationToken));
                Assert.IsType<TimeoutException>(falha.InnerException);
                PublicarFalhaRest(falha, TipoFalhaDisponibilidade.Timeout, StatusDisponibilidade.Degradado);
                Assert.Equal(1, handler.Chamadas);
            }
        }

        [Fact]
        [Trait("Utility", "Disponibilidade")]
        public async Task CancelamentoExplicitoDoHttpClientNaoEhTimeoutNemProtocolo()
        {
            using (var origem = System.Threading.CancellationTokenSource.CreateLinkedTokenSource(TestContext.Current.CancellationToken))
            using (var handler = new HandlerAguardaCancelamento(origem))
            using (var client = new HttpClient(handler) { Timeout = System.Threading.Timeout.InfiniteTimeSpan })
            {
                var falha = await Assert.ThrowsAnyAsync<OperationCanceledException>(async () =>
                    await client.GetAsync("https://api.test", origem.Token).WaitAsync(TimeSpan.FromSeconds(10), TestContext.Current.CancellationToken));
                Assert.True(origem.IsCancellationRequested);
                Assert.Null(falha.InnerException as TimeoutException);
                PublicarFalhaRest(falha, TipoFalhaDisponibilidade.Desconhecida, StatusDisponibilidade.Inconclusivo);
                Assert.Equal(1, handler.Chamadas);
            }
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(0)]
        [InlineData(1)]
        [InlineData(2)]
        [InlineData(3)]
        [InlineData(4)]
        [InlineData(5)]
        public void TimeoutNaCadeiaNaoDependeDeWebExceptionOuMensagem(int formato)
        {
            Exception falha = new TimeoutException();
            if (formato == 1) falha = new HttpRequestException("texto ignorado", falha);
            if (formato == 2) falha = new TaskCanceledException("texto ignorado", falha);
            if (formato == 3) falha = new AggregateException(new InvalidOperationException(), falha);
            if (formato == 4) falha = new WebException("texto ignorado", WebExceptionStatus.Timeout);
            if (formato == 5) falha = new HttpRequestException("texto ignorado", new WebException("texto ignorado", WebExceptionStatus.Timeout));
            PublicarFalhaRest(falha, TipoFalhaDisponibilidade.Timeout, StatusDisponibilidade.Degradado);
        }

        [Theory]
        [Trait("Utility", "Disponibilidade")]
        [InlineData(false)]
        [InlineData(true)]
        public void CancelamentoSemEvidenciaDeTimeoutPermaneceInconclusivo(bool encapsulado)
        {
            // Runtimes antigos podem representar timeout e cancelamento da mesma forma.
            Exception falha = new TaskCanceledException("timeout; senha=segredo");
            if (encapsulado) falha = new AggregateException(new InvalidOperationException(), falha);
            PublicarFalhaRest(falha, TipoFalhaDisponibilidade.Desconhecida, StatusDisponibilidade.Inconclusivo);
        }

        private static void PublicarFalhaRest(Exception falha, TipoFalhaDisponibilidade tipo,
            StatusDisponibilidade status)
        {
            var canal = new CanalObservacoesDisponibilidade();
            var executor = new ExecutorFake(falha);
            using (ApiTransportExecutorFactory.Override(() => executor))
            using (var sessao = canal.IniciarOperacao(Config(TipoDFe.NFSe), Guid.NewGuid(), Guid.NewGuid()))
            using (var consumidor = new ConsumirAPI())
            {
                var recebida = Record.Exception(() => consumidor.ExecutarServico(new APIConfig
                {
                    RequestURI = "https://api.test/segredo?token=secreto",
                    MetodoAPI = "get",
                    TagRetorno = "prop:innertext",
                    UsaCertificadoDigital = false,
                    UseProxy = false
                }, null));
                Assert.Same(falha, recebida);
                sessao.Falhar(recebida);
            }
            Assert.Equal(1, executor.Chamadas);
            Assert.True(canal.TentarLer(out var item));
            Assert.Equal("REST", item.Protocolo);
            Assert.Equal("https://api.test", item.Destino);
            Assert.Equal(0, item.HttpStatusCode);
            Assert.Equal(tipo, item.TipoFalha);
            Assert.Equal(status, item.Status);
            Assert.False(canal.TentarLer(out _));
            Assert.Equal(0, canal.ObservacoesDescartadas);
        }

        private sealed class HandlerAguardaCancelamento : HttpMessageHandler
        {
            private readonly System.Threading.CancellationTokenSource origem;
            internal int Chamadas { get; private set; }

            internal HandlerAguardaCancelamento(System.Threading.CancellationTokenSource origem = null)
            {
                this.origem = origem;
            }

            protected override async Task<HttpResponseMessage> SendAsync(HttpRequestMessage request,
                System.Threading.CancellationToken cancellationToken)
            {
                Chamadas++;
                origem?.Cancel();
                await Task.Delay(System.Threading.Timeout.Infinite, cancellationToken);
                throw new InvalidOperationException("O handler deve terminar por cancelamento.");
            }
        }

        private sealed class ExecutorFake : IApiTransportExecutor
        {
            private readonly Exception falha;
            internal int Chamadas { get; private set; }
            internal ExecutorFake(Exception falha = null) { this.falha = falha; }
            public TransportResponse Execute(TransportRequest request)
            {
                Chamadas++;
                if (falha != null) throw falha;
                return new TransportResponse
                {
                    StatusCode = HttpStatusCode.OK,
                    HttpResponseMessage = new HttpResponseMessage(HttpStatusCode.OK)
                    {
                        Content = new StringContent("<ret><cStat>204</cStat></ret>", Encoding.UTF8, "application/xml")
                    }
                };
            }
        }
    }
}