using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Http;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Unimake.Exceptions;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSFalhasExecucaoTest
    {
        [Fact]
        public void CnpjCompletoNaConsultaDeDebitosFalhaAntesDeAutenticacaoOuPost()
        {
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);

            Assert.Throws<ValidarXMLException>(() =>
            {
                using var abertura = new ServicosAACBS.ConsultarDebitos(
                    new XmlAACBS.ConsultarDebitos { Cnpj = "00000001000001" }, CriarConfiguracao());
                abertura.Executar();
            });

            Assert.Empty(transporte.Requisicoes);
        }

        [Fact]
        public void CnpjCompletoNoPedidoDeDownloadDeDebitosFalhaAntesDoGet()
        {
            var pedido = CriarPedidoDownload();
            pedido.Cnpj = "00000001000001";
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);

            Assert.Throws<ValidarXMLException>(() =>
            {
                using var baixar = new ServicosAACBS.BaixarArquivoRetorno(pedido, CriarConfiguracao());
                baixar.Executar();
            });

            Assert.Empty(transporte.Requisicoes);
        }

        [Fact]
        public void ReexecucaoLimpaRetornoAposTimeoutEHtmlPreservaStatusSemReplay()
        {
            const string sentinelaTimeout = "SENTINELA-AACBS-TIMEOUT-ABERTURA";
            const string sentinelaCorpo = "SENTINELA-AACBS-RESPOSTA-HTML";
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Created, JsonAbertura);
            transporte.EnfileirarFalha(new TimeoutException(sentinelaTimeout));
            transporte.Enfileirar(HttpStatusCode.BadGateway, "<html>" + sentinelaCorpo + "</html>", "text/html");
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                CriarConfiguracao());
            abrir.Executar();
            Assert.Equal(Tiquete, abrir.Result.TiqueteSolicitacao);
            Assert.Equal(HttpStatusCode.Created, abrir.HttpStatusCode);
            Assert.Equal(2, transporte.Requisicoes.Count);

            VerificarExcecaoSanitizada(Record.Exception(abrir.Executar), sentinelaTimeout);
            Assert.Equal((HttpStatusCode)0, abrir.HttpStatusCode);
            VerificarSemRetorno(abrir);
            Assert.Equal(3, transporte.Requisicoes.Count);

            VerificarExcecaoSanitizada(Record.Exception(abrir.Executar), sentinelaCorpo);
            Assert.Equal(HttpStatusCode.BadGateway, abrir.HttpStatusCode);
            VerificarSemRetorno(abrir);
            Assert.Equal(4, transporte.Requisicoes.Count);
        }

        [Fact]
        public void Negocio401RenovaTokenSomenteNaProximaExecucaoExplicita()
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Unauthorized,
                """{"codigoErro":"NAO_AUTORIZADO","mensagemErro":"Token sintético invalidado"}""");
            transporte.Enfileirar(HttpStatusCode.OK, """{"access_token":"TOKEN-NOVO","token_type":"Bearer"}""");
            transporte.Enfileirar(HttpStatusCode.Created, JsonAbertura);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abrir = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                CriarConfiguracao());
            abrir.Executar();
            Assert.Equal(HttpStatusCode.Unauthorized, abrir.HttpStatusCode);
            Assert.Equal("NAO_AUTORIZADO", abrir.Result.CodigoErro);
            Assert.Equal(2, transporte.Requisicoes.Count);

            abrir.Executar();
            Assert.Equal(HttpStatusCode.Created, abrir.HttpStatusCode);
            Assert.Null(abrir.Result.CodigoErro);
            Assert.Equal(Tiquete, abrir.Result.TiqueteSolicitacao);
            Assert.Equal(4, transporte.Requisicoes.Count);
            Assert.Equal(EndpointToken, transporte.Requisicoes[2].Transporte.RequestUri);
            Assert.Equal("Bearer TOKEN-NOVO", transporte.Requisicoes[3].Headers["Authorization"]);
        }

        [Fact]
        public void AmbienteSemEndpointFalhaAntesDeAutenticacaoOuNegocio()
        {
            var configuracao = CriarConfiguracao();
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using (var autenticacao = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao))
            {
                autenticacao.Executar();
                Assert.Equal(Token, autenticacao.Result.AccessToken);
            }

            configuracao.TipoAmbiente = TipoAmbiente.Homologacao;
            Assert.NotNull(Record.Exception(() =>
            {
                using var autenticacao = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao);
                autenticacao.Executar();
            }));
            Assert.NotNull(Record.Exception(() =>
            {
                using var abertura = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj },
                    configuracao);
                abertura.Executar();
            }));
            Assert.Single(transporte.Requisicoes);
        }

        [Fact]
        public void TraceEExcecoesOmitemXmlTokensCorpoRemotoEQueryAssinada()
        {
            const string webhook = "SENTINELA-AACBS-WEBHOOK-TRACE";
            const string corpo = "SENTINELA-AACBS-CORPO-TRACE";
            const string tiquete = "SENTINELA-AACBS-TIQUETE-TRACE";
            const string assinatura = "SENTINELA-ASSINATURA";
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.BadGateway, "{" + corpo);
            transporte.EnfileirarFalha(new HttpRequestException("GET " + UrlAssinada));
            var configuracao = CriarConfiguracao();

            using var texto = new StringWriter();
            using var trace = new TextWriterTraceListener(texto);
            Trace.Listeners.Add(trace);
            try
            {
                using var executor = ApiTransportExecutorFactory.Override(() => transporte);
                using var consultar = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos
                {
                    Cnpj = Cnpj,
                    UrlRetorno = "https://callback.example.test/" + webhook
                }, configuracao);
                var pedido = CriarPedidoDownload();
                pedido.TiqueteSolicitacao = tiquete;
                using var baixar = new ServicosAACBS.BaixarArquivoRetorno(pedido, configuracao);
                VerificarExcecaoSanitizada(Record.Exception(consultar.Executar), corpo, webhook, Token);
                VerificarExcecaoSanitizada(Record.Exception(baixar.Executar), assinatura, tiquete);
                Assert.Equal(3, transporte.Requisicoes.Count);
            }
            finally
            {
                Trace.Listeners.Remove(trace);
            }

            trace.Flush();
            VerificarDiagnosticoSanitizado(texto.ToString(), webhook, corpo, tiquete, assinatura, Token);
        }

        private static void VerificarSemRetorno(ServicosAACBS.ConsultarDebitos abertura)
        {
            Assert.Null(abertura.RetornoWSXML);
            Assert.True(string.IsNullOrEmpty(abertura.RetornoWSString));
            Assert.True(string.IsNullOrEmpty(abertura.RetornoWSRawString));
            Assert.Null(abertura.RetornoWSStream);
            Assert.Null(abertura.Result.TiqueteSolicitacao);
            Assert.Equal("SEM_RETORNO", abertura.Result.ErroLocal.Codigo);
            Assert.True(AACBSFixture.ValidarSchemaDe(abertura.Result.GerarXML(), "retConsultarDebitos").Success);
        }

        private static void VerificarExcecaoSanitizada(Exception erro, params string[] sentinelas)
        {
            Assert.NotNull(erro);
            VerificarDiagnosticoSanitizado(erro.ToString(), sentinelas);
        }

        private static void VerificarDiagnosticoSanitizado(string diagnostico, params string[] sentinelas)
        {
            foreach (var sentinela in sentinelas)
            {
                Assert.False(diagnostico.Contains(sentinela, StringComparison.Ordinal),
                    "O diagnóstico AACBS contém uma sentinela de dado operacional.");
            }
        }
    }
}
