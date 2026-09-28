using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.Lookup;
using Xunit;

namespace Unimake.DFe.Test.NFCe.Utilitarios
{
    /// <summary>
    /// Testes offline (sem rede) da implementação de referência <see cref="CpfCnpjComBrLookup"/>.
    /// Usa um <see cref="HttpMessageHandler"/> falso que devolve JSON canônico da API cpfcnpj.com.br,
    /// exercitando a desserialização, o mapeamento e o tratamento de erros de forma determinística.
    /// </summary>
    [Trait("DFe", "NFCe")]
    public class CpfCnpjComBrLookupTest
    {
        private const string Token = "token-de-teste";

        private static CpfCnpjComBrLookup CriarLookup(string json, out StubHandler handler)
        {
            handler = new StubHandler(json);
            return new CpfCnpjComBrLookup(Token, new HttpClient(handler));
        }

        [Fact]
        public void TokenObrigatorio()
        {
            Assert.Throws<ArgumentException>(() => new CpfCnpjComBrLookup(""));
            Assert.Throws<ArgumentException>(() => new CpfCnpjComBrLookup(null));
        }

        [Fact]
        public async Task ConsultaCpfPacote1RetornaNome()
        {
            var lookup = CriarLookup("{\"status\":1,\"cpf\":\"111.444.777-35\",\"nome\":\"Test Token\"}", out var handler);

            var r = await lookup.ConsultarCpfAsync("111.444.777-35", false, TestContext.Current.CancellationToken);

            Assert.True(r.PessoaFisica);
            Assert.Equal("11144477735", r.Documento);
            Assert.Equal("Test Token", r.Nome);
            Assert.Null(r.Endereco);
            Assert.Contains("/" + Token + "/1/11144477735", handler.LastUrl);
        }

        [Fact]
        public async Task ConsultaCpfPacote3MapeiaEnderecoEIbge()
        {
            const string json = "{\"status\":1,\"cpf\":\"111.444.777-35\",\"nome\":\"Test Token\"," +
                "\"endereco\":\"Rua A\",\"numero\":\"100 B\",\"complemento\":\"Apto 03\",\"bairro\":\"Centro\"," +
                "\"cep\":\"99999123\",\"cidade\":\"Sao Paulo\",\"uf\":\"SP\",\"ibge\":\"3550308\"," +
                "\"enderecos\":[{\"endereco\":\"Rua A\",\"numero\":\"100 B\",\"complemento\":\"Apto 03\",\"bairro\":\"Centro\",\"cep\":\"99999123\",\"cidade\":\"Sao Paulo\",\"uf\":\"SP\",\"ibge\":\"3550308\"}," +
                "{\"endereco\":\"Rua B\",\"numero\":\"200\",\"complemento\":\"\",\"bairro\":\"Centro\",\"cep\":\"99999123\",\"cidade\":\"Sao Paulo\",\"uf\":\"SP\",\"ibge\":\"3550308\"}]}";

            var lookup = CriarLookup(json, out var handler);

            var r = await lookup.ConsultarCpfAsync("111.444.777-35", true, TestContext.Current.CancellationToken);

            Assert.Contains("/3/11144477735", handler.LastUrl);
            Assert.NotNull(r.Endereco);
            Assert.Equal("Rua A", r.Endereco.Logradouro);
            Assert.Equal("100 B", r.Endereco.Numero);
            Assert.Equal("Apto 03", r.Endereco.Complemento);
            Assert.Equal("Centro", r.Endereco.Bairro);
            Assert.Equal("99999123", r.Endereco.Cep);
            Assert.Equal("Sao Paulo", r.Endereco.Municipio);
            Assert.Equal("SP", r.Endereco.Uf);
            Assert.Equal(3550308, r.Endereco.CodigoMunicipioIbge);
            Assert.Equal(2, r.EnderecosAdicionais.Count);
            Assert.Equal("Rua B", r.EnderecosAdicionais[1].Logradouro);
        }

        [Fact]
        public async Task ConsultaCnpjPacote6MapeiaSimplesESituacao()
        {
            const string json = "{\"status\":1,\"cnpj\":\"27.272.134/0001-18\",\"razao\":\"EMPRESA TESTE LTDA\",\"fantasia\":\"EMPRESA TESTE\"," +
                "\"simplesNacional\":{\"optante\":\"Sim\",\"mei\":\"Não\"}," +
                "\"situacao\":{\"id\":2,\"nome\":\"Ativa\"}," +
                "\"matrizEndereco\":{\"cep\":\"39400-000\",\"tipo\":\"Rua\",\"logradouro\":\"Rua Exemplo\",\"numero\":\"1\",\"complemento\":\"Sala 1\",\"bairro\":\"Centro\",\"cidade\":\"Montes Claros\",\"uf\":\"MG\"}," +
                "\"ibge\":{\"pais\":{\"id\":\"1058\"},\"estado\":{\"sigla\":\"MG\",\"ibge_id\":31},\"cidade\":{\"ibge_id\":3143302}}}";

            var lookup = CriarLookup(json, out var handler);

            var r = await lookup.ConsultarCnpjAsync("27272134000118", true, TestContext.Current.CancellationToken);

            Assert.Contains("/6/27272134000118", handler.LastUrl);
            Assert.False(r.PessoaFisica);
            Assert.Equal("EMPRESA TESTE LTDA", r.Nome);
            Assert.Equal("EMPRESA TESTE", r.NomeFantasia);
            Assert.True(r.OptanteSimplesNacional);
            Assert.False(r.Mei);
            Assert.Equal("Ativa", r.SituacaoCadastral);
            Assert.NotNull(r.Endereco);
            Assert.Equal("Rua Exemplo", r.Endereco.Logradouro);
            Assert.Equal("39400000", r.Endereco.Cep);
            Assert.Equal("MG", r.Endereco.Uf);
            Assert.Equal(3143302, r.Endereco.CodigoMunicipioIbge);
        }

        [Fact]
        public async Task ConsultaCnpjPacote5NaoTrazSimples()
        {
            const string json = "{\"status\":1,\"cnpj\":\"27.272.134/0001-18\",\"razao\":\"EMPRESA TESTE LTDA\",\"fantasia\":\"EMPRESA TESTE\"," +
                "\"matrizEndereco\":{\"cep\":\"39400-000\",\"logradouro\":\"Rua Exemplo\",\"numero\":\"1\",\"bairro\":\"Centro\",\"cidade\":\"Montes Claros\",\"uf\":\"MG\"}," +
                "\"ibge\":{\"estado\":{\"sigla\":\"MG\",\"ibge_id\":31},\"cidade\":{\"ibge_id\":3143302}}}";

            var lookup = CriarLookup(json, out var handler);

            var r = await lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken);

            Assert.Contains("/5/27272134000118", handler.LastUrl);
            Assert.False(r.OptanteSimplesNacional);
            Assert.False(r.Mei);
        }

        [Fact]
        public async Task ConsultaInscricoesEstaduaisPacote16()
        {
            const string json = "{\"status\":1,\"cnpj\":\"27.272.134/0001-18\",\"razao\":\"EMPRESA TESTE LTDA\"," +
                "\"inscricoesEstaduais\":[{\"inscricao_estadual\":\"001.010.101.0101\",\"ativo\":true,\"estado\":{\"sigla\":\"MG\",\"ibge_id\":31}}," +
                "{\"inscricao_estadual\":\"111.222.333\",\"ativo\":false,\"estado\":{\"sigla\":\"SP\",\"ibge_id\":35}}]}";

            var lookup = CriarLookup(json, out var handler);

            var lista = await lookup.ConsultarInscricoesEstaduaisAsync("27272134000118", TestContext.Current.CancellationToken);

            Assert.Contains("/16/27272134000118", handler.LastUrl);
            Assert.Equal(2, lista.Count);
            Assert.Equal("0010101010101", lista[0].InscricaoEstadual);
            Assert.True(lista[0].Ativo);
            Assert.Equal("MG", lista[0].Uf);
            Assert.Equal(31, lista[0].CodigoEstadoIbge);
            Assert.False(lista[1].Ativo);
        }

        [Fact]
        public async Task StatusZeroLancaExcecaoComCodigo()
        {
            var lookup = CriarLookup("{\"status\":0,\"erro\":\"Token inválido\",\"erroCodigo\":\"401\"}", out _);

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCpfAsync("11144477735", true, TestContext.Current.CancellationToken));

            Assert.Equal("Token inválido", ex.Message);
            Assert.Equal("401", ex.CodigoErro);
        }

        [Fact]
        public async Task StatusZeroComErroCodigoNumericoLancaExcecao()
        {
            var lookup = CriarLookup("{\"status\":0,\"erro\":\"Documento inválido\",\"erroCodigo\":422}", out _);

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken));

            Assert.Equal("Documento inválido", ex.Message);
            Assert.Equal("422", ex.CodigoErro);
        }

        [Fact]
        public async Task FalhaHttpViraPessoaLookupException()
        {
            var handler = new StubHandler("erro", HttpStatusCode.InternalServerError);
            var lookup = new CpfCnpjComBrLookup(Token, new HttpClient(handler));

            await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken));
        }

        [Fact]
        public async Task ErroDeNegocioComHttp401MantemMensagemECodigoDaApi()
        {
            var handler = new StubHandler("{\"status\":0,\"erro\":\"Token inválido ou não verificado!\",\"pacoteUsado\":1,\"erroCodigo\":1000}", HttpStatusCode.Unauthorized);
            var lookup = new CpfCnpjComBrLookup(Token, new HttpClient(handler));

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCpfAsync("11144477735", false, TestContext.Current.CancellationToken));

            Assert.Equal("Token inválido ou não verificado!", ex.Message);
            Assert.Equal("1000", ex.CodigoErro);
        }

        [Fact]
        public async Task EnvelopeDeGatewayComStatusTextoViraExcecaoComCodigo()
        {
            var handler = new StubHandler("{\"status\":\"error\",\"code\":400,\"message\":\"Incorrect parameters.\"}", HttpStatusCode.BadRequest);
            var lookup = new CpfCnpjComBrLookup(Token, new HttpClient(handler));

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken));

            Assert.Equal("Incorrect parameters.", ex.Message);
            Assert.Equal("400", ex.CodigoErro);
        }

        [Fact]
        public async Task RespostaSemJsonViraExcecaoComStatusHttp()
        {
            var handler = new StubHandler("<html><body>404 Not Found</body></html>", HttpStatusCode.NotFound, "text/html");
            var lookup = new CpfCnpjComBrLookup(Token, new HttpClient(handler));

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken));

            Assert.Contains("HTTP 404", ex.Message);
            Assert.Equal("", ex.CodigoErro);
        }

        [Fact]
        public async Task FalhaDeRedeNaoExpoeOTokenNaMensagem()
        {
            var handler = new ThrowingHandler();
            var lookup = new CpfCnpjComBrLookup(Token, new HttpClient(handler));

            var ex = await Assert.ThrowsAsync<PessoaLookupException>(() => lookup.ConsultarCpfAsync("11144477735", false, TestContext.Current.CancellationToken));

            Assert.DoesNotContain(Token, ex.Message);
            Assert.IsType<HttpRequestException>(ex.InnerException);
        }

        [Fact]
        public async Task TipoDoLogradouroEntraNoEnderecoQuandoAusente()
        {
            const string json = "{\"status\":1,\"razao\":\"EMPRESA TESTE LTDA\"," +
                "\"matrizEndereco\":{\"cep\":\"39400-000\",\"tipo\":\"Avenida\",\"logradouro\":\"Brasil\",\"numero\":\"10\",\"bairro\":\"Centro\",\"cidade\":\"Montes Claros\",\"uf\":\"MG\"}," +
                "\"ibge\":{\"cidade\":{\"ibge_id\":3143302}}}";

            var lookup = CriarLookup(json, out _);

            var r = await lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken);

            Assert.Equal("Avenida Brasil", r.Endereco.Logradouro);
        }

        [Fact]
        public async Task CnpjAlfanumericoEhNormalizadoEmMaiusculasNaUrl()
        {
            var lookup = CriarLookup("{\"status\":1,\"cnpj\":\"12.ABC.345/01DE-35\",\"razao\":\"EMPRESA TESTE LTDA\"}", out var handler);

            var r = await lookup.ConsultarCnpjAsync("12.abc.345/01de-35", false, TestContext.Current.CancellationToken);

            Assert.Equal("12ABC34501DE35", r.Documento);
            Assert.Contains("/5/12ABC34501DE35", handler.LastUrl);
        }

        [Fact]
        public async Task UfDoEnderecoCaiParaOEstadoDoIbgeQuandoAusente()
        {
            const string json = "{\"status\":1,\"razao\":\"EMPRESA TESTE LTDA\"," +
                "\"matrizEndereco\":{\"logradouro\":\"Rua Exemplo\",\"numero\":\"1\",\"cidade\":\"Montes Claros\"}," +
                "\"ibge\":{\"estado\":{\"sigla\":\"MG\",\"ibge_id\":31},\"cidade\":{\"ibge_id\":3143302}}}";

            var lookup = CriarLookup(json, out _);

            var r = await lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken);

            Assert.Equal("MG", r.Endereco.Uf);
        }

        [Fact]
        public async Task InscricaoEstadualPreservaLetrasERemoveSeparadores()
        {
            const string json = "{\"status\":1,\"cnpj\":\"27.272.134/0001-18\",\"razao\":\"EMPRESA TESTE LTDA\"," +
                "\"inscricoesEstaduais\":[{\"inscricao_estadual\":\"P-01100424.3/002\",\"ativo\":true,\"estado\":{\"sigla\":\"SP\",\"ibge_id\":35}}]}";

            var lookup = CriarLookup(json, out _);

            var lista = await lookup.ConsultarInscricoesEstaduaisAsync("27272134000118", TestContext.Current.CancellationToken);

            Assert.Equal("P011004243002", lista[0].InscricaoEstadual);
        }

        [Fact]
        public async Task CepSoComEspacosViraVazio()
        {
            const string json = "{\"status\":1,\"razao\":\"EMPRESA TESTE LTDA\"," +
                "\"matrizEndereco\":{\"cep\":\"   \",\"logradouro\":\"Rua Exemplo\",\"numero\":\"1\",\"cidade\":\"Montes Claros\",\"uf\":\"MG\"}," +
                "\"ibge\":{\"cidade\":{\"ibge_id\":3143302}}}";

            var lookup = CriarLookup(json, out _);

            var r = await lookup.ConsultarCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken);

            Assert.Equal("", r.Endereco.Cep);
        }

        [Fact]
        public async Task SobrecargaComTimeoutEProxyCriaClienteProprioEDescarta()
        {
            var lookup = new CpfCnpjComBrLookup(Token, 15000, null);

            lookup.Dispose();
            lookup.Dispose();

            await Assert.ThrowsAsync<ObjectDisposedException>(() => lookup.ConsultarCpfAsync("11144477735", false, TestContext.Current.CancellationToken));
        }

        [Fact]
        public void DisposeNaoLiberaHttpClientInjetado()
        {
            var handler = new StubHandler("{\"status\":1,\"nome\":\"Test Token\"}");
            var http = new HttpClient(handler);

            new CpfCnpjComBrLookup(Token, http).Dispose();

            Assert.Equal(TimeSpan.FromSeconds(100), http.Timeout);
        }

        /// <summary>
        /// Handler HTTP falso que simula falha de rede.
        /// </summary>
        private sealed class ThrowingHandler : HttpMessageHandler
        {
            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken) =>
                throw new HttpRequestException("Falha simulada de rede.");
        }

        /// <summary>
        /// Handler HTTP falso que devolve sempre a mesma resposta e registra a última URL requisitada.
        /// </summary>
        private sealed class StubHandler : HttpMessageHandler
        {
            private readonly string _body;
            private readonly HttpStatusCode _statusCode;
            private readonly string _mediaType;

            public string LastUrl { get; private set; }

            public StubHandler(string body, HttpStatusCode statusCode = HttpStatusCode.OK, string mediaType = "application/json")
            {
                _body = body;
                _statusCode = statusCode;
                _mediaType = mediaType;
            }

            protected override Task<HttpResponseMessage> SendAsync(HttpRequestMessage request, CancellationToken cancellationToken)
            {
                LastUrl = request.RequestUri.ToString();

                var response = new HttpResponseMessage(_statusCode)
                {
                    Content = new StringContent(_body, Encoding.UTF8, _mediaType)
                };

                return Task.FromResult(response);
            }
        }
    }
}
