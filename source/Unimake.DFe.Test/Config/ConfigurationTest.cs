using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Config
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
    }
}
