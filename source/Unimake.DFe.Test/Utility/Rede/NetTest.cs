using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    /// <summary>
    /// Testes dos utilitários de rede.
    /// </summary>
    public class NetTest
    {
        [Fact]
        [Trait("Utility", "Net")]
        public void HasInternetConnectionTest()
        {
            var testUrls = new[]
            {
                "http://clients3.google.com/generate_204",
                "8.8.8.8",
                "1.1.1.1"
            };

            Assert.True(Business.DFe.Utility.Net.HasInternetConnection(null, 3, testUrls));
        }
    }
}
