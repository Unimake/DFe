using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Unimake.Business.DFe.Lookup;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFCe.Utilitarios
{
    /// <summary>
    /// Testes offline (sem rede) do <see cref="TomadorResolver"/>, usando um
    /// <see cref="IPessoaLookup"/> falso com dados canônicos. Verificam o preenchimento do
    /// destinatário da NFC-e e a regra de Inscrição Estadual/indicador de IE.
    /// </summary>
    [Trait("DFe", "NFCe")]
    public class TomadorResolverTest
    {
        private static EnderecoPessoa EnderecoSP() => new EnderecoPessoa
        {
            Logradouro = "Rua Exemplo",
            Numero = "100",
            Complemento = "Apto 03",
            Bairro = "Centro",
            Cep = "01001000",
            Municipio = "Sao Paulo",
            Uf = "SP",
            CodigoMunicipioIbge = 3550308,
            Pais = "Brasil"
        };

        /// <summary>
        /// Preenche um destinatário pessoa física com o endereço informado, para exercitar
        /// as validações de <c>enderDest</c> isoladamente.
        /// </summary>
        private static Dest PreencherComEndereco(EnderecoPessoa endereco)
        {
            var dest = new Dest { IndIEDest = IndicadorIEDestinatario.NaoContribuinte };

            new TomadorResolver(new FakeLookup()).PreencherDestinatario(dest, new ResultadoPessoa
            {
                Documento = "11144477735",
                PessoaFisica = true,
                Nome = "Cliente Teste",
                Endereco = endereco
            });

            return dest;
        }

        [Fact]
        public async Task ConsumidorPessoaFisicaPreencheDestEIndicaNaoContribuinte()
        {
            var fake = new FakeLookup
            {
                Cpf = new ResultadoPessoa
                {
                    Documento = "11144477735",
                    PessoaFisica = true,
                    Nome = "Cliente Teste",
                    Endereco = EnderecoSP()
                }
            };

            var dest = await new TomadorResolver(fake).ResolverPorCpfAsync("111.444.777-35", true, TestContext.Current.CancellationToken);

            Assert.Equal("11144477735", dest.CPF);
            Assert.True(string.IsNullOrEmpty(dest.CNPJ));
            Assert.Equal("Cliente Teste", dest.XNome);
            Assert.Equal(IndicadorIEDestinatario.NaoContribuinte, dest.IndIEDest);
            Assert.NotNull(dest.EnderDest);
            Assert.Equal("Rua Exemplo", dest.EnderDest.XLgr);
            Assert.Equal(3550308, dest.EnderDest.CMun);
            Assert.Equal(UFBrasil.SP, dest.EnderDest.UF);
            Assert.Equal("01001000", dest.EnderDest.CEP);
            Assert.Equal(1058, dest.EnderDest.CPais);
        }

        [Fact]
        public async Task PessoaJuridicaComIeAtivaNaUfPreencheIeEContribuinte()
        {
            var fake = new FakeLookup
            {
                Cnpj = new ResultadoPessoa
                {
                    Documento = "27272134000118",
                    PessoaFisica = false,
                    Nome = "Empresa Teste LTDA",
                    Endereco = EnderecoSP()
                },
                Ie = new List<InscricaoEstadualInfo>
                {
                    new InscricaoEstadualInfo { InscricaoEstadual = "111222333", Ativo = true, Uf = "SP", CodigoEstadoIbge = 35 }
                }
            };

            var dest = await new TomadorResolver(fake).ResolverPorCnpjAsync("27272134000118", true, TestContext.Current.CancellationToken);

            Assert.Equal("27272134000118", dest.CNPJ);
            Assert.Equal("Empresa Teste LTDA", dest.XNome);
            Assert.Equal("111222333", dest.IE);
            Assert.Equal(IndicadorIEDestinatario.ContribuinteICMS, dest.IndIEDest);
        }

        [Fact]
        public async Task PessoaJuridicaSemIeAtivaNaUfFicaNaoContribuinteSemPresumirIsento()
        {
            var fake = new FakeLookup
            {
                Cnpj = new ResultadoPessoa
                {
                    Documento = "27272134000118",
                    PessoaFisica = false,
                    Nome = "Empresa Teste LTDA",
                    Endereco = EnderecoSP()
                },
                Ie = new List<InscricaoEstadualInfo>
                {
                    new InscricaoEstadualInfo { InscricaoEstadual = "999", Ativo = false, Uf = "SP", CodigoEstadoIbge = 35 },
                    new InscricaoEstadualInfo { InscricaoEstadual = "888", Ativo = true, Uf = "MG", CodigoEstadoIbge = 31 }
                }
            };

            var dest = await new TomadorResolver(fake).ResolverPorCnpjAsync("27272134000118", true, TestContext.Current.CancellationToken);

            Assert.True(string.IsNullOrEmpty(dest.IE));
            Assert.Equal(IndicadorIEDestinatario.NaoContribuinte, dest.IndIEDest);
            Assert.NotEqual(IndicadorIEDestinatario.ContribuinteIsento, dest.IndIEDest);
        }

        [Fact]
        public async Task PessoaJuridicaSemResolverIeNaoConsultaPacote16()
        {
            var fake = new FakeLookup
            {
                Cnpj = new ResultadoPessoa
                {
                    Documento = "27272134000118",
                    PessoaFisica = false,
                    Nome = "Empresa Teste LTDA",
                    Endereco = EnderecoSP()
                }
            };

            var dest = await new TomadorResolver(fake).ResolverPorCnpjAsync("27272134000118", false, TestContext.Current.CancellationToken);

            Assert.False(fake.InscricoesConsultadas);
            Assert.True(string.IsNullOrEmpty(dest.IE));
            Assert.Equal(IndicadorIEDestinatario.NaoContribuinte, dest.IndIEDest);
        }

        [Fact]
        public void SelecionarInscricaoEstadualPreferindoAtivaNaUf()
        {
            var resolver = new TomadorResolver(new FakeLookup());
            var lista = new List<InscricaoEstadualInfo>
            {
                new InscricaoEstadualInfo { InscricaoEstadual = "1", Ativo = false, Uf = "SP" },
                new InscricaoEstadualInfo { InscricaoEstadual = "2", Ativo = true, Uf = "SP" },
                new InscricaoEstadualInfo { InscricaoEstadual = "3", Ativo = true, Uf = "MG" }
            };

            Assert.Equal("2", resolver.SelecionarInscricaoEstadual(lista, "sp").InscricaoEstadual);
            Assert.Null(resolver.SelecionarInscricaoEstadual(lista, "RJ"));
            Assert.Null(resolver.SelecionarInscricaoEstadual(null, "SP"));
            Assert.Null(resolver.SelecionarInscricaoEstadual(lista, ""));
        }

        [Fact]
        public void EnderecoSemIbgeLancaExcecao()
        {
            var endereco = EnderecoSP();
            endereco.CodigoMunicipioIbge = 0;

            var excecao = Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(endereco));

            Assert.Contains("cMun", excecao.Message);
        }

        [Fact]
        public void EnderecoSemLogradouroBairroOuMunicipioLancaExcecao()
        {
            var semLogradouro = EnderecoSP();
            semLogradouro.Logradouro = "   ";
            Assert.Contains("xLgr", Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(semLogradouro)).Message);

            var semBairro = EnderecoSP();
            semBairro.Bairro = null;
            Assert.Contains("xBairro", Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(semBairro)).Message);

            var semMunicipio = EnderecoSP();
            semMunicipio.Municipio = "";
            Assert.Contains("xMun", Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(semMunicipio)).Message);
        }

        [Fact]
        public void UfInvalidaLancaExcecao()
        {
            var endereco = EnderecoSP();
            endereco.Uf = "999";

            var excecao = Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(endereco));

            Assert.Contains("UF do endereço", excecao.Message);
        }

        [Theory]
        [InlineData("EX")]
        [InlineData("90")]
        [InlineData("SVRS")]
        [InlineData("AN")]
        [InlineData("35")]
        [InlineData("")]
        [InlineData(null)]
        public void UfQueNaoEEstadoLancaExcecao(string uf)
        {
            var endereco = EnderecoSP();
            endereco.Uf = uf;

            Assert.Throws<PessoaLookupException>(() => PreencherComEndereco(endereco));
        }

        [Fact]
        public void NumeroVazioViraSemNumero()
        {
            var endereco = EnderecoSP();
            endereco.Numero = "   ";

            var dest = PreencherComEndereco(endereco);

            Assert.Equal("S/N", dest.EnderDest.Nro);
        }

        [Fact]
        public void PessoaFisicaComDocumentoForaDoFormatoLanca()
        {
            var resolver = new TomadorResolver(new FakeLookup());

            var excecao = Assert.Throws<PessoaLookupException>(() => resolver.PreencherDestinatario(
                new Dest(),
                new ResultadoPessoa { Documento = "1114447773", PessoaFisica = true, Nome = "Cpf Curto" }));

            Assert.Contains("CPF", excecao.Message);
        }

        [Fact]
        public void PessoaJuridicaLimpaCpfPreexistente()
        {
            var resolver = new TomadorResolver(new FakeLookup());
            var dest = new Dest { CPF = "11144477735" };

            resolver.PreencherDestinatario(dest, new ResultadoPessoa
            {
                Documento = "27272134000118",
                PessoaFisica = false,
                Nome = "Empresa Teste LTDA"
            });

            Assert.Equal("27272134000118", dest.CNPJ);
            Assert.True(string.IsNullOrEmpty(dest.CPF));
        }

        [Fact]
        public void PessoaFisicaLimpaCnpjPreexistente()
        {
            var resolver = new TomadorResolver(new FakeLookup());
            var dest = new Dest { CNPJ = "27272134000118" };

            resolver.PreencherDestinatario(dest, new ResultadoPessoa
            {
                Documento = "111.444.777-35",
                PessoaFisica = true,
                Nome = "Cliente Teste"
            });

            Assert.Equal("11144477735", dest.CPF);
            Assert.True(string.IsNullOrEmpty(dest.CNPJ));
        }

        [Fact]
        public void CnpjAlfanumericoEAceito()
        {
            var resolver = new TomadorResolver(new FakeLookup());
            var dest = new Dest();

            resolver.PreencherDestinatario(dest, new ResultadoPessoa
            {
                Documento = "12ABC34501DE35",
                PessoaFisica = false,
                Nome = "Empresa Alfanumerica LTDA"
            });

            Assert.Equal("12ABC34501DE35", dest.CNPJ);
            Assert.True(string.IsNullOrEmpty(dest.CPF));
        }

        [Fact]
        public void ConstrutorEArgumentosValidamNulos()
        {
            Assert.Throws<ArgumentNullException>(() => new TomadorResolver(null));

            var resolver = new TomadorResolver(new FakeLookup());
            Assert.Throws<ArgumentNullException>(() => resolver.PreencherDestinatario(null, new ResultadoPessoa()));
            Assert.Throws<ArgumentNullException>(() => resolver.PreencherDestinatario(new Dest(), null));
        }

        private sealed class FakeLookup : IPessoaLookup
        {
            public ResultadoPessoa Cpf { get; set; }
            public ResultadoPessoa Cnpj { get; set; }
            public IReadOnlyList<InscricaoEstadualInfo> Ie { get; set; } = new List<InscricaoEstadualInfo>();
            public bool InscricoesConsultadas { get; private set; }

            public Task<ResultadoPessoa> ConsultarCpfAsync(string cpf, bool comEndereco, CancellationToken cancellationToken = default(CancellationToken)) =>
                Task.FromResult(Cpf);

            public Task<ResultadoPessoa> ConsultarCnpjAsync(string cnpj, bool comSimplesNacional, CancellationToken cancellationToken = default(CancellationToken)) =>
                Task.FromResult(Cnpj);

            public Task<IReadOnlyList<InscricaoEstadualInfo>> ConsultarInscricoesEstaduaisAsync(string cnpj, CancellationToken cancellationToken = default(CancellationToken))
            {
                InscricoesConsultadas = true;
                return Task.FromResult(Ie);
            }
        }
    }
}
