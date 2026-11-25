using System;
using System.Net;
using System.Text.Json;
#if INTEROP
using System.Reflection;
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Unimake.Exceptions;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Parsing.AACBSArquivosConsultasTest;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSFluxoConsultasTest
    {
        private const string WebhookHttps = "https://example.com/a%2fb?teste=a%2Bb&v=2&v=%20";

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, "creditos", "https://example.com/a%2Fb?teste=a%2Bb&v=2", -1)]
        [InlineData(TipoConsultaAACBS.Pagamentos, "pagamentos", "", 0)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "recolhimentos", "", 60)]
        public void ExecutarCicloPreservaContratoEContextoDaFamilia(
            TipoConsultaAACBS familia, string rota, string webhook, int estimativa)
        {
            const string url = "https://storage.example.test/arquivo.json?z=%2B&signature=TESTE%2F%3D&z=%20";
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Created, JsonSerializer.Serialize(new
            {
                tiqueteSolicitacao = TiqueteArquivo,
                tEASegundos = estimativa
            }));
            transporte.Enfileirar(HttpStatusCode.OK, """{"situacao":"PENDENTE"}""");
            transporte.Enfileirar(HttpStatusCode.OK, """{"situacao":"CONCLUIDA"}""");
            transporte.Enfileirar(HttpStatusCode.OK, JsonSerializer.Serialize(new
            {
                urlAssinada = url,
                urlAssinadaExpiraEm = UrlExpiraEm
            }));
            transporte.Enfileirar(HttpStatusCode.OK, ArquivoJson(familia));
            var configuracao = CriarConfiguracao();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);

            using var abertura = CriarConsulta(familia, configuracao);
            Assert.Equal("SEM_RETORNO", Resultado(abertura).ErroLocal.Codigo);
            Assert.Empty(transporte.Requisicoes);

            abertura.Executar();
            Assert.Equal(HttpStatusCode.Created, abertura.HttpStatusCode);
            var retorno = Resultado(abertura);
            Assert.Equal(TiqueteArquivo, retorno.TiqueteSolicitacao);
            Assert.Equal(estimativa, retorno.Estimativa);
            Assert.Null(retorno.ErroLocal);
            ValidarRetorno(abertura, "retConsultar" + familia);
            var roundTrip = retorno.Valor.GerarXML();
            CompararXml(abertura.RetornoWSXML.DocumentElement.OuterXml, roundTrip.DocumentElement.OuterXml);
            Assert.Equal(2, transporte.Requisicoes.Count);

            using var situacao = new ServicosAACBS.ConsultarSituacaoSolicitacao(
                new XmlAACBS.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = retorno.TiqueteSolicitacao }, configuracao);
            situacao.Executar();
            Assert.Equal(SituacaoSolicitacaoAACBS.Pendente, situacao.Result.Situacao);
            Assert.Equal(3, transporte.Requisicoes.Count);
            situacao.Executar();
            Assert.Equal(SituacaoSolicitacaoAACBS.Concluida, situacao.Result.Situacao);
            ValidarRetorno(situacao, "retConsultarSituacaoSolicitacao");
            Assert.Equal(4, transporte.Requisicoes.Count);

            using var link = new ServicosAACBS.ConsultarLinkDownload(
                new XmlAACBS.ConsultarLinkDownload { TiqueteSolicitacao = retorno.TiqueteSolicitacao }, configuracao);
            link.Executar();
            Assert.Equal(url, link.Result.UrlAssinada);
            Assert.Equal(UrlExpiraEm, link.Result.UrlAssinadaExpiraEmField);
            ValidarRetorno(link, "retConsultarLinkDownload");
            Assert.Equal(5, transporte.Requisicoes.Count);

            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(new XmlAACBS.BaixarArquivoRetorno
            {
                TipoConsulta = familia,
                Cnpj = Cnpj,
                TiqueteSolicitacao = retorno.TiqueteSolicitacao,
                UrlAssinada = link.Result.UrlAssinada,
                UrlAssinadaExpiraEmField = link.Result.UrlAssinadaExpiraEmField
            }, configuracao);
            baixar.Executar();
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            ConferirArquivo(baixar, familia);

            Assert.Collection(transporte.Requisicoes,
                token =>
                {
                    Assert.Equal("POST", token.Transporte.Method);
                    Assert.Equal(EndpointToken, token.Transporte.RequestUri);
                    Assert.StartsWith("Basic ", token.Headers["Authorization"]);
                    Assert.Equal("grant_type=client_credentials", token.Corpo);
                },
                post =>
                {
                    ConferirPostFiscal(post, rota, Cnpj);
                    ConferirWebhook(post, webhook);
                },
                get => ConferirGetFiscal(get, "situacao"),
                get => ConferirGetFiscal(get, "situacao"),
                get => ConferirGetFiscal(get, "download"),
                get =>
                {
                    Assert.Equal("GET", get.Transporte.Method);
                    Assert.Equal(url, get.Transporte.RequestUri);
                    Assert.Empty(get.Headers);
                    Assert.False(get.Transporte.AllowAutoRedirect);
                    Assert.False(get.Transporte.UseCookies);
                    Assert.Null(get.Transporte.Cookies);
                    Assert.Null(get.Transporte.Certificate);
                    Assert.Null(get.Corpo);
                });
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, "creditos", null, -1)]
        [InlineData(TipoConsultaAACBS.Creditos, "creditos", "", 0)]
        [InlineData(TipoConsultaAACBS.Creditos, "creditos", WebhookHttps, 60)]
        [InlineData(TipoConsultaAACBS.Pagamentos, "pagamentos", null, -1)]
        [InlineData(TipoConsultaAACBS.Pagamentos, "pagamentos", "", 0)]
        [InlineData(TipoConsultaAACBS.Pagamentos, "pagamentos", WebhookHttps, 60)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "recolhimentos", null, -1)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "recolhimentos", "", 0)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "recolhimentos", WebhookHttps, 60)]
        public void ExecutarAberturaPreservaWebhookEContextoDaFamilia(
            TipoConsultaAACBS familia, string rota, string webhook, int estimativa)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.Created, JsonSerializer.Serialize(new
            {
                tiqueteSolicitacao = TiqueteArquivo,
                tEASegundos = estimativa
            }));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            using ServicosAACBS.ServicoBase abertura = familia switch
            {
                TipoConsultaAACBS.Creditos => new ServicosAACBS.ConsultarCreditos(
                    new XmlAACBS.ConsultarCreditos { Cnpj = Cnpj, UrlRetorno = webhook }, configuracao),
                TipoConsultaAACBS.Pagamentos => new ServicosAACBS.ConsultarPagamentos(
                    new XmlAACBS.ConsultarPagamentos { Cnpj = Cnpj, UrlRetorno = webhook }, configuracao),
                TipoConsultaAACBS.Recolhimentos => new ServicosAACBS.ConsultarRecolhimentos(
                    new XmlAACBS.ConsultarRecolhimentos { Cnpj = Cnpj, UrlRetorno = webhook }, configuracao),
                _ => throw new ArgumentOutOfRangeException(nameof(familia))
            };

            var envio = abertura.ConteudoXMLOriginal.DocumentElement;
            Assert.Equal(webhook, envio["urlRetorno", envio.NamespaceURI]?.InnerText);

            abertura.Executar();

            var post = Assert.Single(transporte.Requisicoes);
            ConferirPostFiscal(post, rota, Cnpj);
            ConferirWebhook(post, webhook ?? string.Empty);
            Assert.Equal(HttpStatusCode.Created, abertura.HttpStatusCode);
            var retorno = Resultado(abertura);
            Assert.Equal(TiqueteArquivo, retorno.TiqueteSolicitacao);
            Assert.Equal(estimativa, retorno.Estimativa);
            Assert.Null(retorno.ErroLocal);
            ValidarRetorno(abertura, "retConsultar" + familia);
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, HttpStatusCode.Unauthorized)]
        [InlineData(TipoConsultaAACBS.Pagamentos, HttpStatusCode.TooManyRequests)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, HttpStatusCode.InternalServerError)]
        public void RetornoFiscalPreservaCodigoMensagemEStatusSemReplay(TipoConsultaAACBS familia, HttpStatusCode status)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, """{"codigoErro":"CODIGO_FISCAL_TESTE","mensagemErro":"Mensagem sintética"}""");
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            using var abertura = CriarConsulta(familia, configuracao);

            abertura.Executar();

            ValidarRetorno(abertura, "retConsultar" + familia);
            var resultado = Resultado(abertura);
            Assert.Equal(status, abertura.HttpStatusCode);
            Assert.Null(resultado.TiqueteSolicitacao);
            Assert.Null(resultado.Estimativa);
            Assert.Null(resultado.ErroLocal);
            Assert.Equal("CODIGO_FISCAL_TESTE", resultado.CodigoErro);
            Assert.Equal("Mensagem sintética", resultado.MensagemErro);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, HttpStatusCode.Created, "application/json",
            """{"tiqueteSolicitacao":"TESTE","tEASegundos":"0"}""")]
        [InlineData(TipoConsultaAACBS.Pagamentos, HttpStatusCode.Created, "application/json",
            """{"tiqueteSolicitacao":"TESTE","tEASegundos":0,"codigoErro":"ERRO","mensagemErro":"Teste"}""")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, HttpStatusCode.BadGateway, "text/html",
            "<p>SENTINELA-CORPO-REMOTO</p>")]
        public void AberturaIncompativelMantemStatusSemMaterializarResultadoFiscal(
            TipoConsultaAACBS familia, HttpStatusCode status, string tipoMidia, string corpo)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, corpo, tipoMidia);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            using var abertura = CriarConsulta(familia, configuracao);

            var erro = Assert.Throws<FormatException>(abertura.Executar);

            Assert.Contains("Consultar" + familia, erro.Message);
            Assert.DoesNotContain("SENTINELA-CORPO-REMOTO", erro.ToString());
            Assert.Equal(status, abertura.HttpStatusCode);
            Assert.Null(abertura.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", Resultado(abertura).ErroLocal.Codigo);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, "<cnpj>00000001</cnpj>", "<cnpj>00000001000001</cnpj>")]
        [InlineData(TipoConsultaAACBS.Pagamentos, "<urlRetorno />", "<urlRetorno>http://example.com/webhook</urlRetorno>")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "versao=\"1.00\"", "versao=\"2.00\"")]
        public void EnvioInvalidoRejeitaAntesDeAutenticarOuAbrir(TipoConsultaAACBS familia, string original, string substituicao)
        {
            var xml = AACBSFixture.Carregar("consultar" + familia).DocumentElement.OuterXml;
            Assert.Contains(original, xml);
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);

            Assert.Throws<ValidarXMLException>(() =>
            {
                using var abertura = CriarConsulta(familia, CriarConfiguracao(), xml.Replace(original, substituicao, StringComparison.Ordinal));
                abertura.Executar();
            });

            Assert.Empty(transporte.Requisicoes);
        }

#if INTEROP
        [Theory]
        [InlineData(typeof(ServicosAACBS.ConsultarCreditos), typeof(XmlAACBS.ConsultarCreditos))]
        [InlineData(typeof(ServicosAACBS.ConsultarPagamentos), typeof(XmlAACBS.ConsultarPagamentos))]
        [InlineData(typeof(ServicosAACBS.ConsultarRecolhimentos), typeof(XmlAACBS.ConsultarRecolhimentos))]
        [InlineData(typeof(ServicosAACBS.GerarDarfAdquirente), typeof(XmlAACBS.GerarDarfAdquirente))]
        [InlineData(typeof(ServicosAACBS.GerarDarfContribuinte), typeof(XmlAACBS.GerarDarfContribuinte))]
        public void CadaServicoExpoePorComOEnvioETipoDeResultadoProprios(Type servico, Type envio)
        {
            Assert.Equal(servico.FullName, servico.GetCustomAttribute<ProgIdAttribute>()?.Value);
            Assert.True(servico.GetCustomAttribute<ComVisibleAttribute>()?.Value);
            var executar = servico.GetMethod("Executar", new[] { envio, typeof(Configuracao) });
            Assert.True(executar?.GetCustomAttribute<ComVisibleAttribute>()?.Value);
            Assert.Equal(envio, servico.GetProperty("Envio")?.PropertyType);
            Assert.Equal($"Unimake.Business.DFe.Xml.AACBS.Ret{servico.Name}", servico.GetProperty("Result")?.PropertyType.FullName);
        }
#endif

        internal static void ConferirPostFiscal(AACBSTransporteControlado.Requisicao post, string rota, string cnpj)
        {
            Assert.Equal("POST", post.Transporte.Method);
            Assert.Equal(BaseNegocio + "/" + rota + "/" + cnpj, post.Transporte.RequestUri);
            Assert.Equal("Bearer " + Token, post.Headers["Authorization"]);
            Assert.Equal("application/json", post.Headers["Accept"]);
            Assert.Equal("application/json", post.TipoMidia);
            Assert.Equal("utf-8", post.Charset, ignoreCase: true);
            Assert.False(post.Transporte.UseCertificate);
            Assert.False(post.Transporte.UseDefaultCredentials);
            Assert.False(post.Transporte.AllowAutoRedirect);
        }

        private static void ConferirWebhook(AACBSTransporteControlado.Requisicao post, string webhook)
        {
            using var json = JsonDocument.Parse(post.Corpo);
            var campo = Assert.Single(json.RootElement.EnumerateObject());
            Assert.Equal("urlRetorno", campo.Name);
            Assert.Equal(webhook, campo.Value.GetString());
        }

        private static void ConferirGetFiscal(AACBSTransporteControlado.Requisicao get, string rota)
        {
            Assert.Equal("GET", get.Transporte.Method);
            Assert.Equal(BaseNegocio + "/" + rota + "/T%C3%8DQUETE-SINT%C3%89TICO-01", get.Transporte.RequestUri);
            Assert.Equal("Bearer " + Token, get.Headers["Authorization"]);
            Assert.Equal("application/json", get.Headers["Accept"]);
            Assert.Null(get.Corpo);
            Assert.Null(get.TipoMidia);
        }

        private static ServicosAACBS.ServicoBase CriarConsulta(TipoConsultaAACBS familia, Configuracao configuracao, string xml = null)
        {
            xml ??= AACBSFixture.Carregar("consultar" + familia).DocumentElement.OuterXml;
            return familia switch
            {
                TipoConsultaAACBS.Creditos => new ServicosAACBS.ConsultarCreditos(xml, configuracao),
                TipoConsultaAACBS.Pagamentos => new ServicosAACBS.ConsultarPagamentos(xml, configuracao),
                TipoConsultaAACBS.Recolhimentos => new ServicosAACBS.ConsultarRecolhimentos(xml, configuracao),
                _ => throw new ArgumentOutOfRangeException(nameof(familia))
            };
        }

        private static (XmlAACBS.RetornoAACBSBase Valor, string TiqueteSolicitacao, int? Estimativa, string CodigoErro,
            string MensagemErro, XmlAACBS.ErroLocal ErroLocal) Resultado(ServicosAACBS.ServicoBase servico) =>
            servico switch
            {
                ServicosAACBS.ConsultarCreditos creditos => (creditos.Result, creditos.Result.TiqueteSolicitacao,
                    creditos.Result.TEASegundos, creditos.Result.CodigoErro, creditos.Result.MensagemErro, creditos.Result.ErroLocal),
                ServicosAACBS.ConsultarPagamentos pagamentos => (pagamentos.Result, pagamentos.Result.TiqueteSolicitacao,
                    pagamentos.Result.TEASegundos, pagamentos.Result.CodigoErro, pagamentos.Result.MensagemErro, pagamentos.Result.ErroLocal),
                ServicosAACBS.ConsultarRecolhimentos recolhimentos => (recolhimentos.Result, recolhimentos.Result.TiqueteSolicitacao,
                    recolhimentos.Result.TEASegundos, recolhimentos.Result.CodigoErro, recolhimentos.Result.MensagemErro, recolhimentos.Result.ErroLocal),
                _ => throw new ArgumentOutOfRangeException(nameof(servico))
            };
    }
}
