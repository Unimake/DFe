using System;
using System.Net.Http;
using System.Threading.Tasks;
using Unimake.Business.DFe.Lookup;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.NFCe.Utilitarios
{
    /// <summary>
    /// Testes de integração do <see cref="CpfCnpjComBrLookup"/> contra a API real da cpfcnpj.com.br,
    /// usando o token público de testes (devolve apenas dados fictícios, sem custo e sem consumo de créditos).
    /// <para>
    /// Estes testes só são executados quando a variável de ambiente <c>CPFCNPJ_INTEGRACAO</c> estiver
    /// definida com o valor <c>1</c>. Sem ela, cada teste é marcado como ignorado, de modo que a suíte
    /// padrão (inclusive em integração contínua) nunca dependa de acesso à internet.
    /// </para>
    /// <para>
    /// Mesmo com o gate habilitado, o teste é ignorado quando não há acesso à API (falha de transporte)
    /// ou quando o IP está temporariamente bloqueado pela regra anti-abuso do serviço.
    /// </para>
    /// <para>
    /// Token inválido não é exercitado aqui de propósito: três chamadas com token inválido em um minuto
    /// bloqueiam o IP por cinco minutos, o que derrubaria os demais testes de integração. Esse cenário
    /// fica coberto pelos testes offline, com respostas simuladas.
    /// </para>
    /// </summary>
    [Trait("DFe", "NFCe")]
    [Trait("Categoria", "Integracao")]
    public class CpfCnpjComBrLookupIntegracaoTest
    {
        /// <summary>
        /// Token público de testes documentado em https://www.cpfcnpj.com.br/dev/ (dados fictícios).
        /// </summary>
        private const string TokenTestes = "5ae973d7a997af13f0aaf2bf60e65803";

        private const string CpfTeste = "00000000000";
        private const string CnpjTeste = "11222333000181";
        private const string CnpjAlfanumericoTeste = "12ABC34501DE35";

        private static readonly HttpClient Http = new HttpClient
        {
            Timeout = TimeSpan.FromSeconds(60)
        };

        private static CpfCnpjComBrLookup CriarLookup() => new CpfCnpjComBrLookup(TokenTestes, Http);

        /// <summary>
        /// Gate de rede: ignora o teste quando a variável de ambiente <c>CPFCNPJ_INTEGRACAO</c>
        /// não estiver definida com o valor <c>1</c>.
        /// </summary>
        private static void ExigirIntegracaoHabilitada()
        {
            if(Environment.GetEnvironmentVariable("CPFCNPJ_INTEGRACAO") != "1")
            {
                Assert.Skip("Defina CPFCNPJ_INTEGRACAO=1 para executar os testes de integração com a API cpfcnpj.com.br (token público de testes, dados fictícios, sem custo).");
            }
        }

        private static async Task<T> ExecutarOuIgnorarAsync<T>(Func<Task<T>> acao)
        {
            ExigirIntegracaoHabilitada();

            try
            {
                return await acao();
            }
            catch(PessoaLookupException ex) when(ex.InnerException is HttpRequestException || ex.InnerException is TaskCanceledException)
            {
                Assert.Skip("Sem acesso à API cpfcnpj.com.br neste ambiente: " + ex.InnerException.Message);
                return default(T);
            }
            catch(PessoaLookupException ex) when(ex.CodigoErro == "1003" || ex.Message.StartsWith("Bloqueio temporário", StringComparison.OrdinalIgnoreCase))
            {
                Assert.Skip("IP temporariamente bloqueado pela API (regra anti-abuso): " + ex.Message);
                return default(T);
            }
        }

        [Fact]
        public async Task ConsultarCpfPacote1RetornaNome()
        {
            var resultado = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarCpfAsync(CpfTeste, false, TestContext.Current.CancellationToken));

            Assert.NotNull(resultado);
            Assert.True(resultado.PessoaFisica);
            Assert.Equal(CpfTeste, resultado.Documento);
            Assert.False(string.IsNullOrWhiteSpace(resultado.Nome));
            Assert.Null(resultado.Endereco);
        }

        [Fact]
        public async Task ConsultarCpfPacote3RetornaEnderecoComCodigoIbge()
        {
            var resultado = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarCpfAsync("000.000.000-00", true, TestContext.Current.CancellationToken));

            Assert.NotNull(resultado);
            Assert.Equal(CpfTeste, resultado.Documento);
            Assert.NotNull(resultado.Endereco);
            Assert.False(string.IsNullOrWhiteSpace(resultado.Endereco.Logradouro));
            Assert.False(string.IsNullOrWhiteSpace(resultado.Endereco.Municipio));
            Assert.Equal(2, resultado.Endereco.Uf.Length);
            Assert.InRange(resultado.Endereco.CodigoMunicipioIbge, 1000000, 9999999);
            Assert.Matches("^[0-9]{8}$", resultado.Endereco.Cep);
        }

        [Fact]
        public async Task ConsultarCnpjPacote5RetornaRazaoEnderecoDaMatriz()
        {
            var resultado = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarCnpjAsync("11.222.333/0001-81", false, TestContext.Current.CancellationToken));

            Assert.NotNull(resultado);
            Assert.False(resultado.PessoaFisica);
            Assert.Equal(CnpjTeste, resultado.Documento);
            Assert.False(string.IsNullOrWhiteSpace(resultado.Nome));
            Assert.NotNull(resultado.Endereco);
            Assert.False(string.IsNullOrWhiteSpace(resultado.Endereco.Logradouro));
            Assert.Equal(2, resultado.Endereco.Uf.Length);
            Assert.InRange(resultado.Endereco.CodigoMunicipioIbge, 1000000, 9999999);
            Assert.False(resultado.OptanteSimplesNacional);
        }

        [Fact]
        public async Task ConsultarCnpjPacote6RetornaSimplesNacionalESituacao()
        {
            var resultado = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarCnpjAsync(CnpjTeste, true, TestContext.Current.CancellationToken));

            Assert.NotNull(resultado);
            Assert.Equal(CnpjTeste, resultado.Documento);
            Assert.False(string.IsNullOrWhiteSpace(resultado.SituacaoCadastral));
            Assert.NotNull(resultado.Endereco);
            Assert.InRange(resultado.Endereco.CodigoMunicipioIbge, 1000000, 9999999);
        }

        [Fact]
        public async Task ConsultarCnpjAlfanumericoEhAceitoPelaApi()
        {
            var resultado = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarCnpjAsync("12.ABC.345/01DE-35", false, TestContext.Current.CancellationToken));

            Assert.NotNull(resultado);
            Assert.Equal(CnpjAlfanumericoTeste, resultado.Documento);
            Assert.False(string.IsNullOrWhiteSpace(resultado.Nome));
            Assert.NotNull(resultado.Endereco);
        }

        [Fact]
        public async Task ConsultarInscricoesEstaduaisPacote16RetornaListaComUf()
        {
            var inscricoes = await ExecutarOuIgnorarAsync(() => CriarLookup().ConsultarInscricoesEstaduaisAsync(CnpjTeste, TestContext.Current.CancellationToken));

            Assert.NotNull(inscricoes);
            Assert.NotEmpty(inscricoes);
            Assert.All(inscricoes, ie =>
            {
                Assert.False(string.IsNullOrWhiteSpace(ie.InscricaoEstadual));
                Assert.Equal(2, ie.Uf.Length);
                Assert.InRange(ie.CodigoEstadoIbge, 11, 53);
            });
        }

        [Fact]
        public async Task DocumentoRejeitadoPelaApiGeraExcecaoComMensagemDaApi()
        {
            ExigirIntegracaoHabilitada();

            try
            {
                await CriarLookup().ConsultarCpfAsync("1234", false, TestContext.Current.CancellationToken);
            }
            catch(PessoaLookupException ex) when(ex.InnerException is HttpRequestException || ex.InnerException is TaskCanceledException)
            {
                Assert.Skip("Sem acesso à API cpfcnpj.com.br neste ambiente: " + ex.InnerException.Message);
                return;
            }
            catch(PessoaLookupException ex) when(ex.CodigoErro == "1003" || ex.Message.StartsWith("Bloqueio temporário", StringComparison.OrdinalIgnoreCase))
            {
                Assert.Skip("IP temporariamente bloqueado pela API (regra anti-abuso): " + ex.Message);
                return;
            }
            catch(PessoaLookupException ex)
            {
                Assert.False(string.IsNullOrWhiteSpace(ex.CodigoErro));
                Assert.False(string.IsNullOrWhiteSpace(ex.Message));
                return;
            }

            Assert.Fail("A API deveria ter rejeitado o documento inválido e lançado PessoaLookupException.");
        }

        [Fact]
        public async Task TomadorResolverPreencheDestinatarioPessoaFisicaComEndereco()
        {
            var resolver = new TomadorResolver(CriarLookup());

            var dest = await ExecutarOuIgnorarAsync(() => resolver.ResolverPorCpfAsync(CpfTeste, true, TestContext.Current.CancellationToken));

            Assert.NotNull(dest);
            Assert.Equal(CpfTeste, dest.CPF);
            Assert.False(string.IsNullOrWhiteSpace(dest.XNome));
            Assert.Equal(IndicadorIEDestinatario.NaoContribuinte, dest.IndIEDest);
            Assert.NotNull(dest.EnderDest);
            Assert.InRange(dest.EnderDest.CMun, 1000000, 9999999);
            Assert.Equal(1058, dest.EnderDest.CPais);
            Assert.True(Enum.IsDefined(typeof(UFBrasil), dest.EnderDest.UF));
        }

        [Fact]
        public async Task TomadorResolverPreencheDestinatarioPessoaJuridicaComInscricaoEstadual()
        {
            var resolver = new TomadorResolver(CriarLookup());

            var dest = await ExecutarOuIgnorarAsync(() => resolver.ResolverPorCnpjAsync(CnpjTeste, true, TestContext.Current.CancellationToken));

            Assert.NotNull(dest);
            Assert.Equal(CnpjTeste, dest.CNPJ);
            Assert.False(string.IsNullOrWhiteSpace(dest.XNome));
            Assert.NotNull(dest.EnderDest);
            Assert.InRange(dest.EnderDest.CMun, 1000000, 9999999);

            if(dest.IndIEDest == IndicadorIEDestinatario.ContribuinteICMS)
            {
                Assert.False(string.IsNullOrWhiteSpace(dest.IE));
            }
            else
            {
                Assert.Equal(IndicadorIEDestinatario.NaoContribuinte, dest.IndIEDest);
                Assert.True(string.IsNullOrEmpty(dest.IE));
            }
        }
    }
}
