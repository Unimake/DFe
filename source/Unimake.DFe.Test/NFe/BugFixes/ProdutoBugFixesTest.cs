using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe.BugFixes
{
    public class ProdutoBugFixesTest
    {
        #region Public Methods

        [Trait("Bug", "143504")]
        [Theory]
        [InlineData("&amp;", "&")]
        [InlineData("&lt;", "<")]
        [InlineData("&gt;", ">")]
        [InlineData("&quot;", "\"")]
        [InlineData("&#39;", "'")]
        [InlineData("&", "&")]
        [InlineData("<", "<")]
        [InlineData(">", ">")]
        [InlineData("\"", "\"")]
        [InlineData("'", "'")]
        public void DeveNormalizarCaracteresReservadosNaDescricao(string specialChar, string expectedChar) => Assert.Equal(expectedChar, new Prod
        {
            XProd = specialChar
        }.XProd);

        #endregion Public Methods
    }
}
