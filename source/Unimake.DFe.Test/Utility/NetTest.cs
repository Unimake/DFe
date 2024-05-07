using Unimake.Business.DFe.Utility;
using Xunit;
using static System.Net.WebRequestMethods;

namespace Unimake.DFe.Test.Utility
{
    /// <summary>
    /// Teste utilitário da classe Net.cs da Utility
    /// </summary>
    public class NetTest
    {
        [Fact]
        [Trait("Utility", "Net")]
        public void HasInternetConnectionTest()
        {
            string[] testUrls = new string[] {
                "http://clients3.google.com/generate_204",
                "http://www.microsoft.com",
                "http://www.cloudflare.com",
                "http://www.amazon.com",
                "http://www.unimake.com.br"
            };

            string[] urls;

            foreach (string url in testUrls)
            {
                urls = new string[] { url };
                Assert.True(Business.DFe.Utility.Net.HasInternetConnection(null, 3, urls));
            }

            urls = new string[] { "http://www.unimake.com" };

            Assert.False(Business.DFe.Utility.Net.HasInternetConnection(null, 3, urls));
        }
    }
}
