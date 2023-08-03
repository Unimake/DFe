using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar classe e método que monta o cenário de testes da NFSe
    /// </summary>
    public class TesteUtilityTest
    {
        /// <summary>
        /// Testar classe e método que monta o cenário de testes da NFSe
        /// </summary>
        [Fact]
        [Trait("DFe", "NFSe")]
        public void TestarElaboracaoCenario()
        {
            var lista = TestUtility.PreparaDadosCenario("GerarNfse");
        }
    }
}