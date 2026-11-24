using System;
using System.Net;
using System.Text.Json;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Servicos;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Parsing
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSContratosHttpTest
    {
        [Theory]
        [InlineData("", -1)]
        [InlineData("https://webhook.example.test/consulta?x=%2B", 120)]
        public void PostMantemWebhookEEstimativa(string webhook, int estimativa)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Created, JsonSerializer.Serialize(new
            {
                tiqueteSolicitacao = Tiquete,
                tEASegundos = estimativa
            }));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos
            {
                Cnpj = Cnpj,
                UrlRetorno = webhook
            }, CriarConfiguracao());
            abrir.Executar();
            Assert.Equal(estimativa, abrir.Result.TEASegundos);
            Assert.Equal(2, transporte.Requisicoes.Count);
            using var json = JsonDocument.Parse(transporte.Requisicoes[1].Corpo);
            var propriedade = Assert.Single(json.RootElement.EnumerateObject());
            Assert.Equal("urlRetorno", propriedade.Name);
            Assert.Equal(webhook, propriedade.Value.GetString());
        }

        [Theory]
        [InlineData(HttpStatusCode.BadRequest)]
        [InlineData(HttpStatusCode.Unauthorized)]
        [InlineData(HttpStatusCode.Forbidden)]
        [InlineData(HttpStatusCode.TooManyRequests)]
        [InlineData(HttpStatusCode.InternalServerError)]
        public void AberturaPreservaStatusECodigoFiscalAbertoSemRepetirPost(HttpStatusCode status)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(status, """{"codigoErro":"NOVO_CODIGO_FISCAL","mensagemErro":"Falha sintética"}""");
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                CriarConfiguracao());
            abrir.Executar();
            Assert.Equal(status, abrir.HttpStatusCode);
            Assert.Equal("NOVO_CODIGO_FISCAL", abrir.Result.CodigoErro);
            Assert.Equal("Falha sintética", abrir.Result.MensagemErro);
            Assert.Null(abrir.Result.TiqueteSolicitacao);
            Assert.Null(abrir.Result.ErroLocal);
            ValidarRetorno(abrir, "retConsultarDebitos");
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData(HttpStatusCode.OK, "PENDENTE", SituacaoSolicitacaoAACBS.Pendente, false)]
        [InlineData(HttpStatusCode.OK, "EXPIRADA", SituacaoSolicitacaoAACBS.Expirada, true)]
        [InlineData(HttpStatusCode.NotFound, "ERRO", SituacaoSolicitacaoAACBS.Erro, true)]
        public void SituacaoPreservaEstadoEStatusSeparadamente(HttpStatusCode status, string situacao,
            SituacaoSolicitacaoAACBS estado, bool temErro)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            var corpo = temErro
                ? JsonSerializer.Serialize(new { situacao, codigoErro = "FALHA_TESTE", mensagemErro = "Erro sintético" })
                : JsonSerializer.Serialize(new { situacao });
            transporte.Enfileirar(status, corpo);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var consultar = new ServicosAACBS.ConsultarSituacaoSolicitacao(
                new XmlAACBS.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = Tiquete }, CriarConfiguracao());
            consultar.Executar();
            Assert.Equal(status, consultar.HttpStatusCode);
            Assert.Equal(estado, consultar.Result.Situacao);
            Assert.Equal(temErro ? "FALHA_TESTE" : null, consultar.Result.CodigoErro);
            Assert.Equal(temErro ? "Erro sintético" : null, consultar.Result.MensagemErro);
            ValidarRetorno(consultar, "retConsultarSituacaoSolicitacao");
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData(HttpStatusCode.OK)]
        [InlineData(HttpStatusCode.Unauthorized)]
        [InlineData(HttpStatusCode.NotFound)]
        [InlineData(HttpStatusCode.InternalServerError)]
        public void LinkPreservaOpcionaisEFormaDeErro(HttpStatusCode status)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            var corpo = status == HttpStatusCode.OK
                ? JsonSerializer.Serialize(new { urlAssinada = UrlAssinada })
                : """{"codigoErro":"ARQUIVO_INDISPONIVEL","mensagemErro":"Arquivo sintético indisponível"}""";
            transporte.Enfileirar(status, corpo);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var consultar = new ServicosAACBS.ConsultarLinkDownload(
                new XmlAACBS.ConsultarLinkDownload { TiqueteSolicitacao = Tiquete }, CriarConfiguracao());
            consultar.Executar();
            Assert.Equal(status, consultar.HttpStatusCode);
            Assert.Equal(status == HttpStatusCode.OK ? UrlAssinada : null, consultar.Result.UrlAssinada);
            Assert.Null(consultar.Result.UrlAssinadaExpiraEm);
            Assert.Equal(status == HttpStatusCode.OK ? null : "ARQUIVO_INDISPONIVEL", consultar.Result.CodigoErro);
            ValidarRetorno(consultar, "retConsultarLinkDownload");
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData("""{"situacao":"PENDENTE"}""")]
        [InlineData("""{"tiqueteSolicitacao":"TESTE","tEASegundos":null}""")]
        [InlineData("""{"tiqueteSolicitacao":"TESTE","tEASegundos":"0"}""")]
        [InlineData("""{"tiqueteSolicitacao":"TESTE","tEASegundos":0,"tEASegundos":1}""")]
        [InlineData("""{"tiqueteSolicitacao":"TESTE","tEASegundos":0,"extensaoFiscal":true}""")]
        [InlineData("""{"tiqueteSolicitacao":"TESTE","tEASegundos":0,"codigoErro":"X","mensagemErro":"X"}""")]
        [InlineData("""[{"tiqueteSolicitacao":"TESTE","tEASegundos":0}]""")]
        [InlineData("")]
        public void RejeitaFormaInvalidaNaAberturaPreservandoHttp(string corpo)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Created, corpo);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                CriarConfiguracao());
            Assert.NotNull(Record.Exception(abrir.Executar));
            Assert.Equal(HttpStatusCode.Created, abrir.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", abrir.Result.ErroLocal.Codigo);
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData("""{"urlAssinada":"https://storage.example.test/arq"}""")]
        [InlineData("""{"situacao":"DESCONHECIDA"}""")]
        [InlineData("""{"situacao":"EXPIRADA","codigoErro":"X"}""")]
        [InlineData("""{"situacao":"PENDENTE","codigoErro":"X","mensagemErro":"X"}""")]
        public void RejeitaFormaInvalidaNaSituacaoPreservandoHttp(string corpo)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.OK, corpo);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var consultar = new ServicosAACBS.ConsultarSituacaoSolicitacao(
                new XmlAACBS.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = Tiquete }, CriarConfiguracao());
            Assert.NotNull(Record.Exception(consultar.Executar));
            Assert.Equal(HttpStatusCode.OK, consultar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", consultar.Result.ErroLocal.Codigo);
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData("0000001", null)]
        [InlineData(Cnpj, "http://webhook.example.test/consulta")]
        public void EntradaInvalidaImpedeInclusiveAutenticacao(string cnpj, string webhook)
        {
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            Assert.NotNull(Record.Exception(() =>
            {
                using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos
                {
                    Cnpj = cnpj,
                    UrlRetorno = webhook
                }, CriarConfiguracao());
                abrir.Executar();
            }));
            Assert.Empty(transporte.Requisicoes);
        }
    }
}
