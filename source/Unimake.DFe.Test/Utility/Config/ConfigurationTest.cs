using System.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.Config
{
    /// <summary>
    /// Testes unitários para validar leituras de configurações gerais.
    /// </summary>
    public class ConfigurationTest
    {
        [Theory]
        [Trait("Utility", "Config")]
        [InlineData(3505500, PadraoNFSe.RLZ_INFORMATICA)]
        [InlineData(5205109, PadraoNFSe.PRODATA)]
        [InlineData(4314407, PadraoNFSe.AVMB)]
        [InlineData(2802908, PadraoNFSe.WEBISS)]
        [InlineData(3550308, PadraoNFSe.PAULISTANA)]
        public void GetPadraoNFSeByCodigoMunicipio_DeveRetornarPadraoCorretoParaMunicipiosImplementados(int codigoMunicipio, PadraoNFSe padraoEsperado)
        {
            Assert.Equal(padraoEsperado, Configuration.GetPadraoNFSe(codigoMunicipio));
        }

        [Theory]
        [Trait("Utility", "Config")]
        [InlineData(3505500, "SP", "BarretosSP.xml", PadraoNFSe.RLZ_INFORMATICA)]
        [InlineData(5205109, "GO", "CatalaoGO.xml", PadraoNFSe.PRODATA)]
        [InlineData(4314407, "RS", "PelotasRS.xml", PadraoNFSe.AVMB)]
        [InlineData(2802908, "SE", "ItabaianaSE.xml", PadraoNFSe.WEBISS)]
        [InlineData(3550308, "SP", "SaoPauloSP.xml", PadraoNFSe.PAULISTANA)]
        public void CarregarMunicipio_DeveRetornarDadosCompletosParaMunicipiosConfigurados(int codigoMunicipio, string ufEsperada, string arqConfigEsperado, PadraoNFSe padraoEsperado)
        {
            var municipios = Configuration.CarregarMunicipio();
            var municipio = municipios.FirstOrDefault(x => x.CodigoMunicipio == codigoMunicipio);

            Assert.NotNull(municipio);
            Assert.Equal(ufEsperada, municipio.UF);
            Assert.Equal(arqConfigEsperado, municipio.ArquivoConfiguracao);
            Assert.Equal(padraoEsperado, municipio.PadraoNFSe);
            Assert.False(string.IsNullOrWhiteSpace(municipio.Nome));
        }

        [Fact]
        [Trait("Utility", "Config")]
        public void CarregarEstados_DeveRetornarSomenteEstadosEIgnorarANeSVRS()
        {
            var estados = Configuration.CarregarEstados();

            Assert.Contains(estados, x => x.CodigoMunicipio == 35 && x.UF == "SP" && x.ArquivoConfiguracao == "SP.xml");
            Assert.Contains(estados, x => x.CodigoMunicipio == 41 && x.UF == "PR" && x.ArquivoConfiguracao == "PR.xml");
            Assert.DoesNotContain(estados, x => x.UF == "AN");
            Assert.DoesNotContain(estados, x => x.UF == "SVRS");
            Assert.DoesNotContain(estados, x => x.CodigoMunicipio >= 1000);
        }
    }
}
