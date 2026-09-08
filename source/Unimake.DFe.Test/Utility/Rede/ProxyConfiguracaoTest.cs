using System;
using System.Net;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Rede
{
    [Trait("Utility", "Rede")]
    public class ProxyConfiguracaoTest
    {
        [Fact]
        public void ProxyManual_DeveUsarServidorPortaECredenciaisInformados()
        {
            var proxy = Proxy.DefinirServidor("proxy.exemplo.test", 3128, false, "usuario", "senha");

            Assert.Equal(new Uri("http://proxy.exemplo.test:3128/"), proxy.GetProxy(new Uri("https://servico.exemplo.test/")));
            var credencial = Assert.IsType<NetworkCredential>(proxy.Credentials.GetCredential(new Uri("http://proxy.exemplo.test:3128/"), "Basic"));
            Assert.Equal("usuario", credencial.UserName);
            Assert.Equal("senha", credencial.Password);
        }

        [Fact]
        public void ProxyManual_DeveAceitarUriHttpsSemCaminho()
        {
            var proxy = Proxy.DefinirServidor("https://proxy.exemplo.test", 8443);

            Assert.Equal(new Uri("https://proxy.exemplo.test:8443/"), proxy.GetProxy(new Uri("https://servico.exemplo.test/")));
        }

        [Theory]
        [InlineData("usuario", "")]
        [InlineData("", "senha")]
        public void Proxy_DeveRejeitarCredencialParcial(string usuario, string senha)
        {
            Assert.Throws<ArgumentException>(() =>
                Proxy.DefinirServidor("proxy.exemplo.test", 3128, false, usuario, senha));
        }

        [Fact]
        public void MapperSoap_DeveAplicarProxyManual()
        {
            var configuracao = CriarConfiguracaoComProxy();

            var soap = new ConfiguracaoWSSoapMapper().Map(configuracao);
            var request = new WSSoapTransportRequestMapper().Map(soap, null, "<soap />", new CookieContainer());

            Assert.True(request.UseProxy);
            Assert.Equal(new Uri("http://proxy.exemplo.test:3128/"), soap.Proxy.GetProxy(new Uri("https://servico.exemplo.test/")));
            Assert.Same(soap.Proxy, request.Proxy);
        }

        [Fact]
        public void MapperSoap_ComProxyDesligado_DeveForcarConexaoDireta()
        {
            var configuracao = new Configuracao { HasProxy = false };

            var soap = new ConfiguracaoWSSoapMapper().Map(configuracao);
            var request = new WSSoapTransportRequestMapper().Map(soap, null, "<soap />", new CookieContainer());

            Assert.False(request.UseProxy);
            Assert.Null(request.Proxy);
        }

        [Fact]
        public void MapperApi_DevePropagarProxyAteOTransporte()
        {
            var configuracao = CriarConfiguracaoComProxy();
            configuracao.RequestURI = "https://servico.exemplo.test/";
            configuracao.MetodoAPI = "get";

            var api = new ConfiguracaoApiConfigMapper().Map(configuracao);
            var request = new ApiConfigTransportRequestMapper().Map(api, null);

            Assert.True(request.UseProxy);
            Assert.Equal(new Uri("http://proxy.exemplo.test:3128/"), request.Proxy.GetProxy(new Uri(configuracao.RequestURI)));
        }

        [Fact]
        public void MapperApi_ComProxyDesligado_DeveForcarConexaoDireta()
        {
            var configuracao = new Configuracao
            {
                HasProxy = false,
                RequestURI = "https://servico.exemplo.test/",
                MetodoAPI = "get"
            };

            var api = new ConfiguracaoApiConfigMapper().Map(configuracao);
            var request = new ApiConfigTransportRequestMapper().Map(api, null);

            Assert.False(request.UseProxy);
            Assert.Null(request.Proxy);
        }

        private static Configuracao CriarConfiguracaoComProxy() => new Configuracao
        {
            HasProxy = true,
            ProxyAutoDetect = false,
            ProxyServer = "proxy.exemplo.test",
            ProxyPort = 3128,
            ProxyUser = "usuario",
            ProxyPassword = "senha"
        };
    }
}
