using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoConsultasTest;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSDarfContinuidadeTest
    {
        private const string CnpjCompleto = "00000001000001";
        private const string NovoToken = "SENTINELA-TOKEN-DARF-RENOVADO";
        private const string SentinelaCabecalho = "SENTINELA-CABECALHO-DARF";
        private const string SentinelaCorpo = "SENTINELA-CORPO-DARF";
        private const string Sentinela404 = "SENTINELA-CORPO-404";
        private const string JsonRad = """
            {
              "cdRetorno": "DARF_GERADO",
              "darf": { "chavePIX": "PIX-SINTETICO-RAD", "codigoBarras": "0001", "linkDarfPDF": "https://example.test/rad.pdf" },
              "informacoesPagamento": { "dataVencimento": "2028-03-20", "valorDarfPrincipal": 4.55, "valorDarfTotal": 4.55 }
            }
            """;
        private const string JsonPcont = """
            {
              "cdRetorno": "DARF_GERADO",
              "darf": { "chavePIX": "PIX-SINTETICO-PCONT", "codigoBarras": "0002", "linkDarfPDF": "https://example.test/pcont.pdf" },
              "informacoesPagamento": { "dataVencimento": "2028-03-20", "valorDarfPrincipal": 6.70, "valorDarfMulta": 0, "valorDarfJuros": 0.01, "valorDarfTotal": 6.71 }
            }
            """;

        [Fact]
        public void RadEPcontComConfiguracaoCompartilhadaRenovamTokenSoAposNovaChamadaExplicita()
        {
            var configuracao = CriarConfiguracao();
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.OK, JsonRad);
            transporte.Enfileirar(HttpStatusCode.Unauthorized,
                """{"cdRetorno":"DADOS_DE_ENTRADA_INVALIDOS","mensagem":"Recusa sintética do PCONT","dadosInvalidos":["pa"]}""");
            transporte.Enfileirar(HttpStatusCode.OK, $$"""{"access_token":"{{NovoToken}}","token_type":"Bearer","expires_in":3600}""");
            transporte.Enfileirar(HttpStatusCode.OK, JsonPcont);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var rad = new ServicosAACBS.GerarDarfAdquirente(XmlEnvio("gerarDarfAdquirente"), configuracao);
            using var pcont = new ServicosAACBS.GerarDarfContribuinte(XmlEnvio("gerarDarfContribuinte"), configuracao);

            rad.Executar();

            ValidarRetorno(rad, "retGerarDarfAdquirente");
            Assert.Equal("DARF_GERADO", rad.Result.CdRetorno);
            Assert.Equal(2, transporte.Requisicoes.Count);

            pcont.Executar();

            ValidarRetorno(pcont, "retGerarDarfContribuinte");
            Assert.Equal(HttpStatusCode.Unauthorized, pcont.HttpStatusCode);
            Assert.Equal("DADOS_DE_ENTRADA_INVALIDOS", pcont.Result.CdRetorno);
            Assert.Equal("Recusa sintética do PCONT", pcont.Result.Mensagem);
            Assert.Null(pcont.Result.Darf);
            Assert.Equal(3, transporte.Requisicoes.Count);

            pcont.Executar();

            ValidarRetorno(pcont, "retGerarDarfContribuinte");
            Assert.Equal(HttpStatusCode.OK, pcont.HttpStatusCode);
            Assert.Equal("DARF_GERADO", pcont.Result.CdRetorno);
            Assert.Null(pcont.Result.Mensagem);
            Assert.Equal(6.71m, pcont.Result.InformacoesPagamento.ValorDarfTotal);
            Assert.Collection(transporte.Requisicoes,
                ConferirPostToken,
                radPost => ConferirPostFiscal(radPost, "rad", CnpjCompleto),
                pcontPost => ConferirPostFiscal(pcontPost, "pcont", CnpjCompleto),
                ConferirPostToken,
                novoPost =>
                {
                    Assert.Equal("POST", novoPost.Transporte.Method);
                    Assert.Equal(BaseNegocio + "/pcont/" + CnpjCompleto, novoPost.Transporte.RequestUri);
                    Assert.Equal("Bearer " + NovoToken, novoPost.Headers["Authorization"]);
                    Assert.Equal(transporte.Requisicoes[2].Corpo, novoPost.Corpo);
                });
        }

        [Theory]
        [InlineData(false)]
        [InlineData(true)]
        public void CorpoParcialAposSucessoLimpaResultadoPreservaHttpENaoRepetePost(bool timeout)
        {
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            var transporte = new AACBSTransporteControlado();
            var leitura = new TransporteComCorpoParcial(timeout);
            var raiz = timeout ? "retGerarDarfContribuinte" : "retGerarDarfAdquirente";
            transporte.Enfileirar(HttpStatusCode.OK, timeout ? JsonPcont : JsonRad);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using ServicosAACBS.ServicoBase servico = timeout
                ? new ServicosAACBS.GerarDarfContribuinte(XmlEnvio("gerarDarfContribuinte"), configuracao)
                : new ServicosAACBS.GerarDarfAdquirente(XmlEnvio("gerarDarfAdquirente"), configuracao);
            servico.Executar();
            ValidarRetorno(servico, raiz);
            Assert.Single(transporte.Requisicoes);

            using var executorLeitura = ApiTransportExecutorFactory.Override(() => leitura);
            var erro = Record.Exception(servico.Executar);

            if (timeout) Assert.IsType<TimeoutException>(erro);
            else Assert.IsType<HttpRequestException>(erro);
            Assert.DoesNotContain(SentinelaCabecalho, erro.ToString());
            Assert.DoesNotContain(SentinelaCorpo, erro.ToString());
            Assert.Equal(HttpStatusCode.OK, servico.HttpStatusCode);
            Assert.Null(servico.RetornoWSXML);
            Assert.True(string.IsNullOrEmpty(servico.RetornoWSString));
            Assert.Equal("SEM_RETORNO", servico switch
            {
                ServicosAACBS.GerarDarfAdquirente rad => rad.Result.ErroLocal?.Codigo,
                ServicosAACBS.GerarDarfContribuinte pcont => pcont.Result.ErroLocal?.Codigo,
                _ => throw new ArgumentException(nameof(servico))
            });
            Assert.Equal(2, leitura.Corpo.Leituras);
            Assert.True(leitura.Corpo.Descartado);
            Assert.True(leitura.Descartado);
            var post = Assert.Single(leitura.Requisicoes);
            ConferirPostFiscal(post, timeout ? "pcont" : "rad", CnpjCompleto);
            Assert.True(post.Transporte.ResponseHeadersRead);
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Pagamentos, "pagamentos", "application/json", "{\"title\":\"" + Sentinela404 + "\",\"status\":404}")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "recolhimentos", "text/html", "<p>" + Sentinela404 + "</p>")]
        public void AberturaCom404IncompativelPreservaHttpSemCriarTiqueteOuFazerNovaChamada(
            TipoConsultaAACBS familia, string rota, string tipoMidia, string corpo)
        {
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.NotFound, corpo, tipoMidia);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            var operacao = "Consultar" + familia;
            var xml = XmlEnvio("consultar" + familia);
            using ServicosAACBS.ServicoBase consulta = familia == TipoConsultaAACBS.Pagamentos
                ? new ServicosAACBS.ConsultarPagamentos(xml, configuracao)
                : new ServicosAACBS.ConsultarRecolhimentos(xml, configuracao);

            var erro = Assert.Throws<FormatException>(consulta.Executar);

            Assert.Contains(operacao, erro.Message);
            Assert.DoesNotContain(Sentinela404, erro.ToString());
            Assert.Equal(HttpStatusCode.NotFound, consulta.HttpStatusCode);
            Assert.Null(consulta.RetornoWSXML);
            Assert.True(string.IsNullOrEmpty(consulta.RetornoWSString));
            var (tiquete, codigoFiscal, erroLocal) = consulta switch
            {
                ServicosAACBS.ConsultarPagamentos pagamentos => (pagamentos.Result.TiqueteSolicitacao, pagamentos.Result.CodigoErro, pagamentos.Result.ErroLocal),
                ServicosAACBS.ConsultarRecolhimentos recolhimentos => (recolhimentos.Result.TiqueteSolicitacao, recolhimentos.Result.CodigoErro, recolhimentos.Result.ErroLocal),
                _ => throw new ArgumentException(nameof(consulta))
            };
            Assert.Null(tiquete);
            Assert.Null(codigoFiscal);
            Assert.Equal("SEM_RETORNO", erroLocal.Codigo);
            var post = Assert.Single(transporte.Requisicoes);
            ConferirPostFiscal(post, rota, Cnpj);
            Assert.Equal("""{"urlRetorno":""}""", post.Corpo);
        }

        private static string XmlEnvio(string raiz) => AACBSFixture.Carregar(raiz).DocumentElement.OuterXml;

        private static void ConferirPostToken(AACBSTransporteControlado.Requisicao post)
        {
            Assert.Equal("POST", post.Transporte.Method);
            Assert.Equal(EndpointToken, post.Transporte.RequestUri);
            Assert.StartsWith("Basic ", post.Headers["Authorization"]);
            Assert.Equal("grant_type=client_credentials", post.Corpo);
        }

        private sealed class TransporteComCorpoParcial(bool timeout) : IApiTransportExecutor, IDisposable
        {
            internal CorpoParcial Corpo { get; } = new(timeout);
            internal bool Descartado { get; private set; }
            internal List<AACBSTransporteControlado.Requisicao> Requisicoes { get; } = new();

            public TransportResponse Execute(TransportRequest request)
            {
                Requisicoes.Add(new AACBSTransporteControlado.Requisicao(request));
                if (Requisicoes.Count != 1)
                    throw new InvalidOperationException("POST adicional durante a leitura de um DARF.");

                var resposta = new HttpResponseMessage(HttpStatusCode.OK) { Content = new StreamContent(Corpo) };
                resposta.Headers.Add("X-Contexto-Privado", SentinelaCabecalho);
                return new TransportResponse { HttpResponseMessage = resposta, ResponseLifetime = this };
            }

            public void Dispose() => Descartado = true;
        }

        private sealed class CorpoParcial(bool timeout) : MemoryStream(Encoding.UTF8.GetBytes("{\"darf\":\"" + SentinelaCorpo))
        {
            internal int Leituras { get; private set; }
            internal bool Descartado { get; private set; }

            public override Task<int> ReadAsync(byte[] buffer, int offset, int count, CancellationToken cancellationToken)
            {
                if (++Leituras == 1)
                    return Task.FromResult(Read(buffer, offset, count));

                var mensagem = SentinelaCabecalho + " " + SentinelaCorpo;
                return Task.FromException<int>(timeout ? new OperationCanceledException(mensagem) : new IOException(mensagem));
            }

            protected override void Dispose(bool disposing)
            {
                Descartado = true;
                base.Dispose(disposing);
            }
        }
    }
}
