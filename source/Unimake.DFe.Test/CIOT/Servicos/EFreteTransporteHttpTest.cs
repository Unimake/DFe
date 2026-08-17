using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using SituacaoServico = Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador;

namespace Unimake.DFe.Test.CIOT.Servicos
{
    public class EFreteTransporteHttpTest
    {
        [Fact]
        [Trait("DFe", "CIOT")]
        public async Task TransporteRealEnviaGetComJsonNoCorpo()
        {
            var listener = new TcpListener(IPAddress.Loopback, 0);
            listener.Start();
            try
            {
                var porta = ((IPEndPoint)listener.LocalEndpoint).Port;
                var servidor = ReceberRequisicoes(listener, "{}");
                var json = "{\"Integrador\":\"INTEGRADOR-TESTE\",\"Versao\":1}";
                var request = new TransportRequest
                {
                    Method = "get",
                    RequestUri = "http://127.0.0.1:" + porta + "/services/Logon/Login",
                    HttpContent = new StringContent(json, Encoding.UTF8, "application/json"),
                    UseWinHttpHandler = true,
                    Timeout = 10000
                };

                using (var response = new ApiTransportExecutor().Execute(request))
                {
                    Assert.Equal(HttpStatusCode.OK, response.StatusCode);
                }

                var requisicao = Assert.Single(await servidor);
                Assert.Equal("GET", requisicao.Metodo);
                Assert.Equal("/services/Logon/Login", requisicao.Caminho);
                Assert.Equal(json, requisicao.Corpo);
                Assert.Contains("application/json", requisicao.ContentType, StringComparison.OrdinalIgnoreCase);
            }
            finally
            {
                listener.Stop();
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void GetComConteudoSemOpcaoEFreteMantemTransportePadrao()
        {
            var request = new TransportRequest
            {
                Method = "get",
                RequestUri = "http://127.0.0.1/servico-generico",
                HttpContent = new StringContent("{}", Encoding.UTF8, "application/json")
            };
            var criarHandler = typeof(ApiTransportExecutor).GetMethod("CriarHandler", BindingFlags.Instance | BindingFlags.NonPublic);

            using (var handler = (HttpMessageHandler)criarHandler.Invoke(new ApiTransportExecutor(), new object[] { request }))
            {
                Assert.IsType<HttpClientHandler>(handler);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void WinHttpSemCertificadoNaoSelecionaCertificadoAutomaticamente()
        {
            if (!OperatingSystem.IsWindows())
            {
                return;
            }

            var request = new TransportRequest
            {
                Method = "get",
                RequestUri = "https://localhost/login",
                HttpContent = new StringContent("{}", Encoding.UTF8, "application/json"),
                UseWinHttpHandler = true,
                UseCertificate = false
            };
            var criarHandler = typeof(ApiTransportExecutor).GetMethod("CriarHandler", BindingFlags.Instance | BindingFlags.NonPublic);

            using (var handler = (WinHttpHandler)criarHandler.Invoke(new ApiTransportExecutor(), new object[] { request }))
            {
                Assert.Equal(ClientCertificateOption.Manual, handler.ClientCertificateOption);
                Assert.Empty(handler.ClientCertificates);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void PostEFreteSemCertificadoNaoSelecionaCertificadoAutomaticamente()
        {
            var request = new TransportRequest
            {
                Method = "post",
                RequestUri = "https://localhost/Services/Pef/AdicionarOperacaoTransporteV2",
                HttpContent = new StringContent("{}", Encoding.UTF8, "application/json"),
                UseCertificate = false,
                DisableAutomaticClientCertificateSelection = true
            };
            var criarHandler = typeof(ApiTransportExecutor).GetMethod("CriarHandler", BindingFlags.Instance | BindingFlags.NonPublic);

            using (var handler = (HttpClientHandler)criarHandler.Invoke(new ApiTransportExecutor(), new object[] { request }))
            {
                Assert.Equal(ClientCertificateOption.Manual, handler.ClientCertificateOptions);
                Assert.Empty(handler.ClientCertificates);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void PostGenericoSemCertificadoMantemSelecaoAutomatica()
        {
            var request = new TransportRequest
            {
                Method = "post",
                RequestUri = "https://localhost/servico-generico",
                HttpContent = new StringContent("{}", Encoding.UTF8, "application/json"),
                UseCertificate = false
            };
            var criarHandler = typeof(ApiTransportExecutor).GetMethod("CriarHandler", BindingFlags.Instance | BindingFlags.NonPublic);

            using (var handler = (HttpClientHandler)criarHandler.Invoke(new ApiTransportExecutor(), new object[] { request }))
            {
                Assert.Equal(ClientCertificateOption.Automatic, handler.ClientCertificateOptions);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void SomenteConfiguracaoEFretePropagaUsoDoWinHttpHandler()
        {
            var configuracaoGenerica = new Configuracao
            {
                MetodoAPI = "get",
                RequestURI = "http://127.0.0.1/servico-generico",
                HttpContent = new StringContent("{}", Encoding.UTF8, "application/json")
            };
            var apiGenerica = new ConfiguracaoApiConfigMapper().Map(configuracaoGenerica);
            var requestGenerica = new ApiConfigTransportRequestMapper().Map(apiGenerica, null);
            Assert.False(requestGenerica.UseWinHttpHandler);
            Assert.False(requestGenerica.DisableAutomaticClientCertificateSelection);

            var configuracaoEFrete = new Configuracao
            {
                TipoAmbiente = TipoAmbiente.Homologacao,
                EFreteIntegrador = "INTEGRADOR-TESTE",
                EFreteToken = "TOKEN-TESTE"
            };
            new SituacaoServico(new ConsultarSituacaoTransportador
            {
                ProvedorCIOT = ProvedorCIOT.EFrete,
                CpfCnpjInteressado = "12345678000199",
                CpfCnpjTransportador = "12345678901",
                RNTRCTransportador = "123456789",
                PlacasConsulta = new System.Collections.Generic.List<string> { "BRA2E19" }
            }, configuracaoEFrete);
            var apiEFrete = new ConfiguracaoApiConfigMapper().Map(configuracaoEFrete);
            var requestEFrete = new ApiConfigTransportRequestMapper().Map(apiEFrete, null);

            Assert.True(requestEFrete.UseWinHttpHandler);
            Assert.True(requestEFrete.DisableAutomaticClientCertificateSelection);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public async Task ExecutarConsultaSituacaoComLoginUsaGetComCorpoNoTransporteReal()
        {
            var listener = new TcpListener(IPAddress.Loopback, 0);
            listener.Start();
            try
            {
                var porta = ((IPEndPoint)listener.LocalEndpoint).Port;
                var servidor = ReceberRequisicoes(
                    listener,
                    "{\"Sucesso\":true,\"Token\":\"TOKEN-LOCAL\"}",
                    "{\"Sucesso\":true,\"ProtocoloServico\":\"PROTO-LOCAL\",\"CpfOuCnpj\":\"12345678901\",\"RNTRC\":\"123456789\",\"RNTRCAtivo\":true,\"TACouEquiparado\":false,\"Versao\":1}");
                var configuracao = new Configuracao
                {
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    EFreteIntegrador = "INTEGRADOR-TESTE",
                    EFreteUsuario = "USUARIO-SINTETICO",
                    EFreteSenha = "SENHA-SINTETICA"
                };
                var envio = new ConsultarSituacaoTransportador
                {
                    ProvedorCIOT = ProvedorCIOT.EFrete,
                    CpfCnpjInteressado = "12345678000199",
                    CpfCnpjTransportador = "12345678901",
                    RNTRCTransportador = "123456789",
                    PlacasConsulta = new System.Collections.Generic.List<string> { "BRA2E19" }
                };
                var servico = new SituacaoServico(envio, configuracao);
                configuracao.RequestURILoginHomologacao = "http://127.0.0.1:" + porta + "/services/Logon/Login";
                configuracao.RequestURI = "http://127.0.0.1:" + porta + "/services/Pef/ConsultaSituacaoTransportador";

                servico.Executar();

                var requisicoes = await servidor;
                Assert.Equal(2, requisicoes.Count);
                Assert.All(requisicoes, requisicao => Assert.Equal("GET", requisicao.Metodo));
                Assert.Contains("SENHA-SINTETICA", requisicoes[0].Corpo);
                Assert.DoesNotContain("SENHA-SINTETICA", requisicoes[1].Corpo);
                Assert.Contains("TOKEN-LOCAL", requisicoes[1].Corpo);
                Assert.Equal("TOKEN-LOCAL", configuracao.EFreteToken);
                Assert.Equal("PROTO-LOCAL", servico.Result.Protocolo);
                Assert.True(servico.Result.RNTRCAtivo);
            }
            finally
            {
                listener.Stop();
            }
        }

        private static async Task<System.Collections.Generic.IList<RequisicaoRecebida>> ReceberRequisicoes(TcpListener listener, params string[] respostas)
        {
            var requisicoes = new System.Collections.Generic.List<RequisicaoRecebida>();
            foreach (var respostaJson in respostas)
            {
                requisicoes.Add(await ReceberRequisicao(listener, respostaJson));
            }
            return requisicoes;
        }

        private static async Task<RequisicaoRecebida> ReceberRequisicao(TcpListener listener, string respostaJson)
        {
            using (var cliente = await listener.AcceptTcpClientAsync())
            using (var stream = cliente.GetStream())
            using (var leitor = new StreamReader(stream, Encoding.ASCII, false, 1024, true))
            {
                var linhaInicial = await leitor.ReadLineAsync();
                var partes = linhaInicial.Split(' ');
                var contentLength = 0;
                var contentType = string.Empty;
                string linha;
                while (!string.IsNullOrEmpty(linha = await leitor.ReadLineAsync()))
                {
                    var separador = linha.IndexOf(':');
                    if (separador < 0)
                    {
                        continue;
                    }

                    var nome = linha.Substring(0, separador);
                    var valor = linha.Substring(separador + 1).Trim();
                    if (string.Equals(nome, "Content-Length", StringComparison.OrdinalIgnoreCase))
                    {
                        contentLength = int.Parse(valor);
                    }
                    else if (string.Equals(nome, "Content-Type", StringComparison.OrdinalIgnoreCase))
                    {
                        contentType = valor;
                    }
                }

                var caracteres = new char[contentLength];
                var lidos = 0;
                while (lidos < caracteres.Length)
                {
                    var quantidade = await leitor.ReadAsync(caracteres, lidos, caracteres.Length - lidos);
                    if (quantidade == 0)
                    {
                        break;
                    }
                    lidos += quantidade;
                }

                var conteudoResposta = Encoding.UTF8.GetBytes(respostaJson);
                var cabecalhoResposta = Encoding.ASCII.GetBytes("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: " + conteudoResposta.Length + "\r\nConnection: close\r\n\r\n");
                await stream.WriteAsync(cabecalhoResposta, 0, cabecalhoResposta.Length);
                await stream.WriteAsync(conteudoResposta, 0, conteudoResposta.Length);
                await stream.FlushAsync();

                return new RequisicaoRecebida
                {
                    Metodo = partes[0],
                    Caminho = partes[1],
                    ContentType = contentType,
                    Corpo = new string(caracteres, 0, lidos)
                };
            }
        }

        private sealed class RequisicaoRecebida
        {
            internal string Metodo { get; set; }

            internal string Caminho { get; set; }

            internal string ContentType { get; set; }

            internal string Corpo { get; set; }
        }
    }
}
