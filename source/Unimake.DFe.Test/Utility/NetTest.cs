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
                    "8.8.8.8", //Servidor Primário de DNS do Google
                    "8.8.4.4", //Servidor Secundário de DNS do Google
                    "http://www.microsoft.com",
                    "http://www.cloudflare.com",
                    "1.1.1.1", //Servidor Primário de DNS do Cloudfare
                    "1.0.0.1",  //Servidor Secundário de DNS do Cloudfare
                    "http://www.amazon.com",
                    "9.9.9.9", //Servidor Primário de DNS do Quad 9
                    "149.112.112.112", //Servidor Secundário de DNS do Quad 9
                    "http://www.unimake.com.br",
                    "http://67.205.183.164"
            };

            string[] urls;

            foreach (string url in testUrls)
            {
                urls = new string[] { url };

                Assert.True(Business.DFe.Utility.Net.HasInternetConnection(null, 3, urls));
            }

            testUrls = new string[] {
                "http://www.unimake.com",
                "3.3.3.3",
            };

            foreach (string url in testUrls)
            {
                urls = new string[] { url };

                Assert.False(Business.DFe.Utility.Net.HasInternetConnection(null, 3, urls));
            }

            Assert.True(Business.DFe.Utility.Net.HasInternetConnection());
        }
    }
}
