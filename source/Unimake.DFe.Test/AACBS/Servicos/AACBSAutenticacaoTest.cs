using System;
using System.Net;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSAutenticacaoTest
    {
        private const string RespostaComScopeVazio =
            """{"access_token":"SENTINELA-TOKEN-OAUTH","token_type":"Bearer","scope":""}""";

        [Fact]
        public void AplicaFormEncodingUtf8AntesDoBasicEIgnoraExtensaoOAuth()
        {
            var configuracao = CriarConfiguracao();
            var identidade = configuracao.ClientID;
            configuracao.ClientID += "+:% á";
            configuracao.ClientSecret = " ê:+% ";
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK,
                """{"access_token":"TOKEN-AUXILIAR","token_type":"bEaReR","scope":"a b","extensao":{"emitido":true}}""");
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticar = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao);
            autenticar.Executar();
            Assert.Equal("bEaReR", autenticar.Result.TokenType);
            Assert.Equal("a b", autenticar.Result.Scope);
            Assert.Null(autenticar.Result.ExpiresIn);
            ValidarRetorno(autenticar, "retAutenticar");
            Assert.DoesNotContain("extensao", autenticar.RetornoWSString);

            var request = Assert.Single(transporte.Requisicoes);
            Assert.Equal(EndpointToken, request.Transporte.RequestUri);
            var basic = request.Headers["Authorization"];
            Assert.StartsWith("Basic ", basic);
            var codificado = Encoding.ASCII.GetString(Convert.FromBase64String(basic["Basic ".Length..]));
            Assert.Equal(identidade + "%2B%3A%25+%C3%A1:+%C3%AA%3A%2B%25+", codificado);
            Assert.Equal("grant_type=client_credentials", request.Corpo);
            Assert.DoesNotContain(identidade, autenticar.ConteudoXMLOriginal.OuterXml);
        }

        [Fact]
        public void CacheMantemValidadeOriginalERenovaApenasComOrcamento()
        {
            var agora = new DateTimeOffset(2026, 11, 24, 12, 0, 0, TimeSpan.Zero);
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, """{"access_token":"TOKEN-UM","token_type":"Bearer","expires_in":120}""");
            transporte.Enfileirar(HttpStatusCode.OK, """{"access_token":"TOKEN-DOIS","token_type":"Bearer","expires_in":120}""");
            transporte.Enfileirar(HttpStatusCode.OK, """{"access_token":"TOKEN-TRES","token_type":"Bearer","expires_in":120}""");
            var configuracao = CriarConfiguracao();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticar = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao)
            {
                RelogioUtc = () => agora
            };
            autenticar.Executar();
            Assert.Equal(HttpStatusCode.OK, autenticar.HttpStatusCode);
            Assert.Equal("TOKEN-UM", autenticar.Result.AccessToken);
            Assert.Single(transporte.Requisicoes);

            agora = agora.AddSeconds(1);
            autenticar.Executar();
            Assert.Equal((HttpStatusCode)0, autenticar.HttpStatusCode);
            Assert.Equal("TOKEN-UM", autenticar.Result.AccessToken);
            Assert.Equal(120, autenticar.Result.ExpiresIn);
            Assert.Single(transporte.Requisicoes);

            agora = agora.AddMinutes(2);
            autenticar.Executar();
            Assert.Equal(HttpStatusCode.OK, autenticar.HttpStatusCode);
            Assert.Equal("TOKEN-DOIS", autenticar.Result.AccessToken);
            Assert.Equal(2, transporte.Requisicoes.Count);

            agora = agora.AddMinutes(2);
            Assert.NotNull(Record.Exception(autenticar.Executar));
            Assert.Equal((HttpStatusCode)0, autenticar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", autenticar.Result.ErroLocal.Codigo);
            Assert.Equal(2, transporte.Requisicoes.Count);

            agora = agora.AddHours(1);
            autenticar.Executar();
            Assert.Equal("TOKEN-TRES", autenticar.Result.AccessToken);
            Assert.Equal(3, transporte.Requisicoes.Count);
        }

        [Fact]
        public void CotaContaTentativaAmbiguaERotacaoNaoReaproveitaToken()
        {
            var agora = new DateTimeOffset(2026, 11, 24, 12, 0, 0, TimeSpan.Zero);
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.EnfileirarFalha(new TimeoutException("SENTINELA-TIMEOUT-AUTH"));
            var configuracao = CriarConfiguracao();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticar = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao)
            {
                RelogioUtc = () => agora
            };
            autenticar.Executar();
            Assert.Equal(Token, autenticar.Result.AccessToken);
            configuracao.ClientSecret = "NOVA-REVISAO-SINTETICA";
            Assert.NotNull(Record.Exception(autenticar.Executar));
            Assert.Equal((HttpStatusCode)0, autenticar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", autenticar.Result.ErroLocal.Codigo);
            Assert.Equal(2, transporte.Requisicoes.Count);

            Assert.NotNull(Record.Exception(autenticar.Executar));
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Fact]
        public async Task ChamadasConcorrentesPartilhamEmissaoApenasDaMesmaCredencial()
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.OK, """{"access_token":"TOKEN-OUTRA-CREDENCIAL","token_type":"Bearer"}""");
            var primeiroAcesso = CriarConfiguracao();
            var mesmoAcesso = CriarConfiguracao();
            mesmoAcesso.ClientID = primeiroAcesso.ClientID;
            mesmoAcesso.ClientSecret = primeiroAcesso.ClientSecret;
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var primeiro = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), primeiroAcesso);
            using var repetido = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), mesmoAcesso);
            using var partida = new Barrier(2);
            Task ExecutarJuntos(ServicosAACBS.Autenticar servico) => Task.Factory.StartNew(() =>
            {
                if (!partida.SignalAndWait(TimeSpan.FromSeconds(10)))
                {
                    throw new TimeoutException("As duas autenticações controladas não iniciaram.");
                }

                servico.Executar();
            }, CancellationToken.None, TaskCreationOptions.LongRunning, TaskScheduler.Default);

            await Task.WhenAll(ExecutarJuntos(primeiro), ExecutarJuntos(repetido));
            Assert.Equal(Token, primeiro.Result.AccessToken);
            Assert.Equal(Token, repetido.Result.AccessToken);
            Assert.Single(transporte.Requisicoes);

            using var distinto = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), CriarConfiguracao());
            distinto.Executar();
            Assert.Equal("TOKEN-OUTRA-CREDENCIAL", distinto.Result.AccessToken);
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Fact]
        public void ErroOAuthReconhecidoPreservaStatusEMensagemOperacional()
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.BadRequest,
                """{"error":"invalid_client","error_description":"Credencial sintética recusada","error_uri":"https://oauth.example.test/erro"}""");
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticar = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), CriarConfiguracao());
            autenticar.Executar();
            Assert.Equal(HttpStatusCode.BadRequest, autenticar.HttpStatusCode);
            Assert.Equal("invalid_client", autenticar.Result.Error);
            Assert.Equal("Credencial sintética recusada", autenticar.Result.ErrorDescription);
            Assert.Equal("https://oauth.example.test/erro", autenticar.Result.ErrorUri);
            Assert.Null(autenticar.Result.AccessToken);
            ValidarRetorno(autenticar, "retAutenticar");
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("""{"access_token":"TOKEN-TESTE","token_type":"Basic"}""")]
        [InlineData("""{"access_token":"TOKEN-TESTE","token_type":"Bearer","expires_in":null}""")]
        [InlineData("""{"access_token":"TOKEN-TESTE","token_type":"Bearer","expires_in":"3600"}""")]
        [InlineData("""{"access_token":"TOKEN-TESTE","token_type":"Bearer","token_type":"Bearer"}""")]
        [InlineData("""{"access_token":"TOKEN-TESTE","token_type":"Bearer","error":"invalid_client"}""")]
        public void TokenIncompativelImpedePostFiscalComUmUnicoRequest(string resposta)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, resposta);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abertura = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                CriarConfiguracao());
            Assert.NotNull(Record.Exception(abertura.Executar));
            Assert.Equal((HttpStatusCode)0, abertura.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", abertura.Result.ErroLocal.Codigo);
            var request = Assert.Single(transporte.Requisicoes);
            Assert.Equal(EndpointToken, request.Transporte.RequestUri);
        }

        [Theory]
        [InlineData(HttpStatusCode.OK, RespostaComScopeVazio, "$.scope")]
        [InlineData(HttpStatusCode.BadRequest, """{"error":"","error_description":"SENTINELA-ERRO-OAUTH"}""", "$")]
        public void RespostaOAuthAuxiliarInvalidaIdentificaAutenticacaoESuspendePostFiscal(
            HttpStatusCode status, string resposta, string caminho)
        {
            var configuracao = CriarConfiguracao();
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, resposta);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abertura = new ServicosAACBS.ConsultarDebitos(
                new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj }, configuracao);

            var falha = Assert.Throws<FormatException>(abertura.Executar);

            Assert.Equal($"AACBS Autenticar: resposta incompatível com o contrato em {caminho}.", falha.Message);
            Assert.DoesNotContain("SENTINELA-", falha.ToString());
            Assert.DoesNotContain(configuracao.ClientID, falha.ToString());
            Assert.DoesNotContain(configuracao.ClientSecret, falha.ToString());
            Assert.Equal((HttpStatusCode)0, abertura.HttpStatusCode);
            Assert.Null(abertura.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", abertura.Result.ErroLocal.Codigo);
            var request = Assert.Single(transporte.Requisicoes);
            Assert.Equal(EndpointToken, request.Transporte.RequestUri);
        }

        [Fact]
        public void AutenticacaoDiretaRejeitaScopeVazioEPreservaStatusHttp()
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, RespostaComScopeVazio);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticar = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), CriarConfiguracao());

            var falha = Assert.Throws<FormatException>(autenticar.Executar);

            Assert.Equal("AACBS Autenticar: resposta incompatível com o contrato em $.scope.", falha.Message);
            Assert.DoesNotContain("SENTINELA-", falha.ToString());
            Assert.Equal(HttpStatusCode.OK, autenticar.HttpStatusCode);
            Assert.Null(autenticar.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", autenticar.Result.ErroLocal.Codigo);
            var request = Assert.Single(transporte.Requisicoes);
            Assert.Equal(EndpointToken, request.Transporte.RequestUri);
        }
    }
}
