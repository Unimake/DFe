using System;
using System.IO;
using System.Net;
using System.Text;
using System.Text.Json;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSFluxoDebitosTest
    {
        internal const string BaseNegocio = "https://api.receitafederal.gov.br/apuracao-cbs/v2";
        internal const string EndpointToken = "https://api.receitafederal.gov.br/token";
        internal const string Cnpj = "00000001";
        internal const string Tiquete = "TÍQUETE teste/+=?%";
        internal const string SegmentoTiquete = "T%C3%8DQUETE%20teste%2F%2B%3D%3F%25";
        internal const string UrlAssinada = "https://storage.example.test/debitos.json?z=%2B&signature=SENTINELA-ASSINATURA%2F%3D&z=%20";
        internal const string UrlExpiraEm = "2099-08-31T10:11:12.123456789-03:00";
        internal const string Token = "SENTINELA-TOKEN-BASE";
        internal const string Instante = "2028-02-29T23:59:59.123456799-03:00";
        internal const string JsonToken = """{"access_token":"SENTINELA-TOKEN-BASE","token_type":"Bearer","expires_in":3600}""";
        internal const string JsonAbertura = """{"tiqueteSolicitacao":"TÍQUETE teste/+=?%","tEASegundos":0}""";
        internal const string JsonArquivo = """
            {
              "apuracao": [
                {
                  "debitos": [
                    {
                      "cbs": { "saldoDevedor": 0, "apurado": 9999999999999999.99, "excedente": 0, "inexigivel": -1.01 },
                      "atualizacao": "2028-02-29T23:59:59.123456799-03:00",
                      "registro": "2028-02-29T12:00:00.0100+00:00",
                      "emissao": "2028-02-29T12:00:00Z",
                      "chave": "000CHAVE-TESTE",
                      "documento": 55,
                      "origem": 0
                    },
                    {
                      "origem": 55, "documento": 97, "chave": "000CHAVE-TESTE",
                      "emissao": "2028-02-29T12:00:00Z",
                      "registro": "2028-02-29T12:00:00.0100+00:00",
                      "atualizacao": "2028-02-29T23:59:59.123456799-03:00",
                      "cbs": { "apurado": 0, "saldoDevedor": 0.01 }
                    }
                  ],
                  "pa": "02/2028"
                }
              ],
              "geradoEm": "2028-02-29T23:59:59.123456799-03:00",
              "niConsumidor": "00000001000001",
              "ni": "00000001",
              "tiqueteSolicitacao": "TÍQUETE teste/+=?%"
            }
            """;

        [Theory]
        [InlineData(false, false)]
        [InlineData(true, true)]
        [InlineData(true, false)]
        public void ExecutarCicloExplicitoValidaContratosEIsolaDownload(bool usaProxy, bool autodetectar)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.Created, JsonAbertura);
            transporte.Enfileirar(HttpStatusCode.OK, """{"situacao":"EM_PROCESSAMENTO"}""");
            transporte.Enfileirar(HttpStatusCode.OK, """{"situacao":"CONCLUIDA"}""");
            transporte.Enfileirar(HttpStatusCode.OK, JsonSerializer.Serialize(new
            {
                urlAssinada = UrlAssinada,
                urlAssinadaExpiraEm = UrlExpiraEm
            }));
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivo);
            var configuracao = CriarConfiguracao();
            configuracao.HasProxy = usaProxy;
            configuracao.ProxyAutoDetect = autodetectar;
            if (usaProxy && !autodetectar)
            {
                configuracao.ProxyServer = "proxy.example.test";
                configuracao.ProxyPort = 3128;
                configuracao.ProxyUser = "USUARIO-PROXY-TESTE";
                configuracao.ProxyPassword = "SENHA-PROXY-TESTE";
            }

            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var autenticacao = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao);
            using var abertura = new ServicosAACBS.ConsultarDebitos(
                new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj }.GerarXML().OuterXml, configuracao);

            Assert.Equal("SEM_RETORNO", abertura.Result.ErroLocal.Codigo);
            Assert.Empty(transporte.Requisicoes);
            autenticacao.Executar();
            Assert.Equal(HttpStatusCode.OK, autenticacao.HttpStatusCode);
            Assert.Equal(Token, autenticacao.Result.AccessToken);
            ValidarRetorno(autenticacao, "retAutenticar");
            Assert.Single(transporte.Requisicoes);

            abertura.Executar();
            Assert.Equal(HttpStatusCode.Created, abertura.HttpStatusCode);
            Assert.Equal(Tiquete, abertura.Result.TiqueteSolicitacao);
            Assert.Equal(0, abertura.Result.TEASegundos);
            ValidarRetorno(abertura, "retConsultarDebitos");
            Assert.Equal(2, transporte.Requisicoes.Count);

            using var situacao = new ServicosAACBS.ConsultarSituacaoSolicitacao(
                new XmlAACBS.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = abertura.Result.TiqueteSolicitacao },
                configuracao);
            situacao.Executar();
            Assert.Equal(SituacaoSolicitacaoAACBS.EmProcessamento, situacao.Result.Situacao);
            Assert.Equal(3, transporte.Requisicoes.Count);
            situacao.Executar();
            Assert.Equal(SituacaoSolicitacaoAACBS.Concluida, situacao.Result.Situacao);
            ValidarRetorno(situacao, "retConsultarSituacaoSolicitacao");
            Assert.Equal(4, transporte.Requisicoes.Count);

            using var link = new ServicosAACBS.ConsultarLinkDownload(
                new XmlAACBS.ConsultarLinkDownload { TiqueteSolicitacao = Tiquete }, configuracao);
            link.Executar();
            Assert.Equal(HttpStatusCode.OK, link.HttpStatusCode);
            Assert.Equal(UrlAssinada, link.Result.UrlAssinada);
            Assert.Equal(UrlExpiraEm, link.Result.UrlAssinadaExpiraEmField);
            ValidarRetorno(link, "retConsultarLinkDownload");
            Assert.Equal(5, transporte.Requisicoes.Count);

            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(new XmlAACBS.BaixarArquivoRetorno
            {
                TipoConsulta = TipoConsultaAACBS.Debitos,
                Cnpj = Cnpj,
                TiqueteSolicitacao = Tiquete,
                UrlAssinada = link.Result.UrlAssinada,
                UrlAssinadaExpiraEmField = link.Result.UrlAssinadaExpiraEmField
            }, configuracao);
            baixar.Executar();
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var arquivo = Assert.IsType<XmlAACBS.RetArquivoDebitos>(baixar.Result.ArquivoDebitos);
            Assert.Equal(Cnpj, arquivo.Ni);
            Assert.Equal("00000001000001", arquivo.NiConsumidor);
            Assert.Equal(Tiquete, arquivo.TiqueteSolicitacao);
            Assert.Equal(Instante, arquivo.GeradoEmField);
            Assert.Equal(TimeSpan.FromHours(-3), arquivo.GeradoEm.Offset);
            Assert.True(AACBSFixture.ValidarSchemaDe(arquivo.GerarXML(), "retArquivoDebitos").Success);
            var periodo = Assert.Single(arquivo.Apuracao);
            Assert.Equal("02/2028", periodo.Pa);
            Assert.Collection(periodo.Debitos, primeiro =>
            {
                Assert.Equal(0, primeiro.Origem);
                Assert.Equal(55, primeiro.Documento);
                Assert.Equal("000CHAVE-TESTE", primeiro.Chave);
                Assert.Equal("2028-02-29T12:00:00Z", primeiro.EmissaoField);
                Assert.Equal("2028-02-29T12:00:00.0100+00:00", primeiro.RegistroField);
                Assert.Equal(Instante, primeiro.AtualizacaoField);
                Assert.Equal(9999999999999999.99m, primeiro.Cbs.Apurado);
                Assert.Equal(0m, primeiro.Cbs.Excedente);
                Assert.Equal(-1.01m, primeiro.Cbs.Inexigivel);
                Assert.Equal(0m, primeiro.Cbs.SaldoDevedor);
            }, segundo =>
            {
                Assert.Equal("000CHAVE-TESTE", segundo.Chave);
                Assert.Null(segundo.Cbs.Excedente);
                Assert.Equal(0.01m, segundo.Cbs.SaldoDevedor);
            });

            using var pasta = new PastaTemporaria();
            baixar.GravarXmlDistribuicao(pasta.Caminho, "retorno.xml", baixar.RetornoWSString);
            var gravado = AACBSFixture.Ler(File.ReadAllText(Path.Combine(pasta.Caminho, "retorno.xml"), Encoding.UTF8));
            Assert.Equal(baixar.RetornoWSXML.DocumentElement.OuterXml, gravado.DocumentElement.OuterXml);
            Assert.True(AACBSFixture.ValidarSchemaDe(gravado, "retBaixarArquivoRetorno").Success);

            Assert.Collection(transporte.Requisicoes,
                token =>
                {
                    Assert.Equal("POST", token.Transporte.Method.ToUpperInvariant());
                    Assert.Equal(EndpointToken, token.Transporte.RequestUri);
                    Assert.StartsWith("Basic ", token.Headers["Authorization"]);
                    Assert.Equal("application/x-www-form-urlencoded", token.TipoMidia);
                    Assert.Equal("grant_type=client_credentials", token.Corpo);
                },
                post =>
                {
                    Assert.Equal("POST", post.Transporte.Method.ToUpperInvariant());
                    Assert.Equal(BaseNegocio + "/debitos/" + Cnpj, post.Transporte.RequestUri);
                    Assert.Equal("Bearer " + Token, post.Headers["Authorization"]);
                    Assert.Equal("application/json", post.TipoMidia);
                    Assert.Equal("utf-8", post.Charset, ignoreCase: true);
                    using var json = JsonDocument.Parse(post.Corpo);
                    var campo = Assert.Single(json.RootElement.EnumerateObject());
                    Assert.Equal("urlRetorno", campo.Name);
                    Assert.Empty(campo.Value.GetString());
                },
                get => VerificarGetFiscal(get, "situacao"),
                get => VerificarGetFiscal(get, "situacao"),
                get => VerificarGetFiscal(get, "download"),
                download =>
                {
                    AssertGetSemCorpo(download, UrlAssinada);
                    Assert.Empty(download.Headers);
                    Assert.Null(download.Transporte.Cookies);
                    Assert.False(download.Transporte.UseCookies);
                    Assert.False(download.Transporte.AllowAutoRedirect);
                });
            Assert.All(transporte.Requisicoes, requisicao =>
            {
                var request = requisicao.Transporte;
                Assert.False(request.UseCertificate);
                Assert.Null(request.Certificate);
                Assert.True(request.DisableAutomaticClientCertificateSelection);
                Assert.False(request.UseDefaultCredentials);
                Assert.Equal(usaProxy, request.UseProxy);
                if (usaProxy && !autodetectar)
                {
                    var proxy = Assert.IsType<WebProxy>(request.Proxy);
                    var endereco = new Uri("http://proxy.example.test:3128/");
                    Assert.Equal(endereco, proxy.Address);
                    var credencial = proxy.Credentials.GetCredential(endereco, "Basic");
                    Assert.Equal(configuracao.ProxyUser, credencial.UserName);
                    Assert.Equal(configuracao.ProxyPassword, credencial.Password);
                }
                else if (!usaProxy)
                {
                    Assert.Null(request.Proxy);
                }
            });
        }

        internal static Configuracao CriarConfiguracao() => new()
        {
            TipoAmbiente = TipoAmbiente.Producao,
            ClientID = "CLIENTE-AACBS-TESTE-" + Guid.NewGuid().ToString("N"),
            ClientSecret = "SEGREDO-SINTETICO-AACBS",
            HasProxy = false
        };

        internal static XmlAACBS.BaixarArquivoRetorno CriarPedidoDownload() => new()
        {
            TipoConsulta = TipoConsultaAACBS.Debitos,
            Cnpj = Cnpj,
            TiqueteSolicitacao = Tiquete,
            UrlAssinada = UrlAssinada
        };

        internal static void ValidarRetorno(ServicoBase servico, string raiz)
        {
            Assert.NotNull(servico.RetornoWSXML);
            Assert.Equal(raiz, servico.RetornoWSXML.DocumentElement.LocalName);
            Assert.Equal(AACBSFixture.Namespace, servico.RetornoWSXML.DocumentElement.NamespaceURI);
            Assert.Equal("1.00", servico.RetornoWSXML.DocumentElement.GetAttribute("versao"));
            Assert.True(AACBSFixture.ValidarSchemaDe(servico.RetornoWSXML, raiz).Success);
        }

        private static void VerificarGetFiscal(AACBSTransporteControlado.Requisicao requisicao, string operacao)
        {
            AssertGetSemCorpo(requisicao, BaseNegocio + "/" + operacao + "/" + SegmentoTiquete);
            Assert.Equal("Bearer " + Token, requisicao.Headers["Authorization"]);
            Assert.Equal("application/json", requisicao.Headers["Accept"]);
        }

        private static void AssertGetSemCorpo(AACBSTransporteControlado.Requisicao requisicao, string uri)
        {
            Assert.Equal("GET", requisicao.Transporte.Method.ToUpperInvariant());
            Assert.Equal(uri, requisicao.Transporte.RequestUri);
            Assert.Null(requisicao.Corpo);
            Assert.Null(requisicao.TipoMidia);
            Assert.DoesNotContain("Content-Type", requisicao.Headers.Keys, StringComparer.OrdinalIgnoreCase);
        }

        private sealed class PastaTemporaria : IDisposable
        {
            internal string Caminho { get; } = Directory.CreateTempSubdirectory("aacbs-teste-").FullName;

            public void Dispose()
            {
                var arquivo = Path.Combine(Caminho, "retorno.xml");
                File.Delete(arquivo);
                Directory.Delete(Caminho);
            }
        }
    }
}
