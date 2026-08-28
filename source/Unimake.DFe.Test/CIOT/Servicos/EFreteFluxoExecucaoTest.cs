using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Security.Cryptography;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using ConsultaServico = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;
using DeclaracaoServico = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;

namespace Unimake.DFe.Test.CIOT.Servicos
{
    public class EFreteFluxoExecucaoTest
    {
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExecutarEnviaPostComTokenFornecidoENormalizaResult()
        {
            var transporte = new TransporteControlado();
            transporte.AdicionarResposta("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000126\",\"ProtocoloServico\":\"PROTO-POST\"}");
            var configuracao = CriarConfiguracaoEFrete();
            var servico = new DeclaracaoServico(LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-lotacao-completa.xml"), configuracao);
            var endpointEsperado = configuracao.RequestURI;

            using (ApiTransportExecutorFactory.Override(() => transporte))
            {
                servico.Executar();
            }

            var requisicao = Assert.Single(transporte.Requisicoes);
            var json = JObject.Parse(requisicao.Corpo);
            var notasFiscais = Assert.IsType<JArray>(json.SelectToken("Viagens[0].NotasFiscais"));
            Assert.Equal("post", requisicao.Metodo, ignoreCase: true);
            Assert.Equal(endpointEsperado, requisicao.Url);
            Assert.Equal("TOKEN-TESTE", json.Value<string>("Token"));
            Assert.Single(notasFiscais);
            Assert.False(requisicao.UsaCertificado);
            Assert.Equal("992000000126", servico.Result.IdOperacaoTransporte);
            Assert.Equal("110", servico.Result.Codigo);
            Assert.Equal("PROTO-POST", servico.Result.Protocolo);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExecutarFazLoginIncluiTokenNoPostEReaproveitaToken()
        {
            var transporte = new TransporteControlado();
            transporte.AdicionarResposta("{\"Sucesso\":true,\"Retorno\":{\"Token\":\"TOKEN-OBTIDO\"}}");
            transporte.AdicionarResposta("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000127\",\"ProtocoloServico\":\"PROTO-LOGIN\"}");
            transporte.AdicionarResposta("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000128\",\"ProtocoloServico\":\"PROTO-REUSO\"}");
            var configuracao = CriarConfiguracaoEFrete();
            configuracao.EFreteToken = null;
            configuracao.EFreteUsuario = "USUARIO-SINTETICO";
            configuracao.EFreteSenha = "SENHA-SINTETICA";
            var declaracao = LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-lotacao-completa.xml");

            using (ApiTransportExecutorFactory.Override(() => transporte))
            {
                var primeiroServico = new DeclaracaoServico(declaracao, configuracao);
                primeiroServico.Executar();
                var segundoServico = new DeclaracaoServico(declaracao, configuracao);
                segundoServico.Executar();

                Assert.Equal("992000000127", primeiroServico.Result.IdOperacaoTransporte);
                Assert.Equal("992000000128", segundoServico.Result.IdOperacaoTransporte);
            }

            Assert.Equal(3, transporte.Requisicoes.Count);
            Assert.Equal("get", transporte.Requisicoes[0].Metodo, ignoreCase: true);
            Assert.Contains("Login", transporte.Requisicoes[0].Url, StringComparison.OrdinalIgnoreCase);
            Assert.Equal("SENHA-SINTETICA", JObject.Parse(transporte.Requisicoes[0].Corpo).Value<string>("Senha"));
            Assert.Equal("post", transporte.Requisicoes[1].Metodo, ignoreCase: true);
            Assert.Equal("TOKEN-OBTIDO", JObject.Parse(transporte.Requisicoes[1].Corpo).Value<string>("Token"));
            Assert.Equal("TOKEN-OBTIDO", JObject.Parse(transporte.Requisicoes[2].Corpo).Value<string>("Token"));
            Assert.DoesNotContain("SENHA-SINTETICA", transporte.Requisicoes[1].Corpo);
            Assert.Equal("TOKEN-OBTIDO", configuracao.EFreteToken);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExecutarEnviaGetComJsonNoCorpoENormalizaErroNoResult()
        {
            var transporte = new TransporteControlado();
            transporte.AdicionarResposta("{\"Sucesso\":false,\"Excecao\":{\"Codigo\":\"EF123\",\"Mensagem\":\"Operação rejeitada\"}}");
            var configuracao = CriarConfiguracaoEFrete();
            var servico = new ConsultaServico(new ConsultarCIOTGerado
            {
                ProvedorCIOT = ProvedorCIOT.EFrete,
                MatrizCNPJ = "12345678000199",
                IdOperacaoCliente = "CIOT-CLIENTE-001"
            }, configuracao);

            using (ApiTransportExecutorFactory.Override(() => transporte))
            {
                servico.Executar();
            }

            var requisicao = Assert.Single(transporte.Requisicoes);
            var json = JObject.Parse(requisicao.Corpo);
            Assert.Equal("get", requisicao.Metodo, ignoreCase: true);
            Assert.Equal("12345678000199", json.Value<string>("MatrizCNPJ"));
            Assert.Equal("CIOT-CLIENTE-001", json.Value<string>("IdOperacaoCliente"));
            Assert.Equal("TOKEN-TESTE", json.Value<string>("Token"));
            Assert.NotNull(servico.Result.Temp);
            Assert.Equal("EF123", servico.Result.Codigo[0]);
            Assert.Equal("Operação rejeitada", servico.Result.Mensagem[0]);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExecutarUsaCertificadoSemEnviarToken()
        {
            var transporte = new TransporteControlado();
            transporte.AdicionarResposta("{\"Sucesso\":true,\"CodigoIdentificacaoOperacao\":\"992000000129\",\"ProtocoloServico\":\"PROTO-CERT\"}");
            using (var certificado = CriarCertificado())
            {
                var configuracao = CriarConfiguracaoEFrete();
                configuracao.EFreteToken = null;
                configuracao.CertificadoDigital = certificado;
                var servico = new DeclaracaoServico(LerXML<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\efrete-declaracao-carga-lotacao-completa.xml"), configuracao);

                using (ApiTransportExecutorFactory.Override(() => transporte))
                {
                    servico.Executar();
                }

                var requisicao = Assert.Single(transporte.Requisicoes);
                Assert.True(requisicao.UsaCertificado);
                Assert.Same(certificado, requisicao.Certificado);
                Assert.Null(JObject.Parse(requisicao.Corpo)["Token"]);
                Assert.Equal("992000000129", servico.Result.IdOperacaoTransporte);
            }
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public async Task ExecutarSemSelecionarProvedorPreservaFluxoANTT()
        {
            var transporte = new TransporteControlado();
            transporte.AdicionarResposta("{\"CodigoIdentificacaoOperacao\":\"1234567890123456\",\"Codigo\":{\"Item\":\"000000\"},\"Mensagem\":{\"Item\":\"Operacao realizada com sucesso\"}}");
            using (var certificado = CriarCertificado())
            {
                var configuracao = new Configuracao
                {
                    TipoDFe = TipoDFe.CIOT,
                    TipoEmissao = TipoEmissao.Normal,
                    TipoAmbiente = TipoAmbiente.Homologacao,
                    CodigoUF = (int)UFBrasil.AN,
                    CertificadoDigital = certificado
                };
                var envio = LerXML<ConsultarCIOTGerado>(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml");
                var servico = new ConsultaServico(envio, configuracao);
                var endpointEsperado = configuracao.RequestURI;
                var corpoEsperado = await configuracao.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken);

                using (ApiTransportExecutorFactory.Override(() => transporte))
                {
                    servico.Executar();
                }

                var requisicao = Assert.Single(transporte.Requisicoes);
                Assert.Equal(ProvedorCIOT.ANTT, configuracao.ProvedorCIOT);
                Assert.Equal("post", requisicao.Metodo, ignoreCase: true);
                Assert.Equal(endpointEsperado, requisicao.Url);
                Assert.True(JToken.DeepEquals(JToken.Parse(corpoEsperado), JToken.Parse(requisicao.Corpo)));
                Assert.True(requisicao.UsaCertificado);
                Assert.Equal("1234567890123456", servico.Result.CodigoIdentificacaoOperacao);
                Assert.Equal("000000", servico.Result.Codigo[0]);
            }
        }

        private static Configuracao CriarConfiguracaoEFrete()
        {
            return new Configuracao
            {
                TipoAmbiente = TipoAmbiente.Homologacao,
                ProvedorCIOT = ProvedorCIOT.EFrete,
                EFreteIntegrador = "INTEGRADOR-TESTE",
                EFreteToken = "TOKEN-TESTE"
            };
        }

        private static X509Certificate2 CriarCertificado()
        {
            using (var rsa = RSA.Create(2048))
            {
                var requisicao = new CertificateRequest("CN=CIOT TESTE", rsa, HashAlgorithmName.SHA256, RSASignaturePadding.Pkcs1);
                return requisicao.CreateSelfSigned(DateTimeOffset.UtcNow.AddDays(-1), DateTimeOffset.UtcNow.AddDays(1));
            }
        }

        private static T LerXML<T>(string caminho) where T : Unimake.Business.DFe.Xml.XMLBase, new()
        {
            var documento = new XmlDocument();
            documento.Load(caminho);
            return new T().LerXML<T>(documento);
        }

        private sealed class TransporteControlado : IApiTransportExecutor
        {
            private readonly Queue<string> _respostas = new Queue<string>();

            internal IList<RequisicaoCapturada> Requisicoes { get; } = new List<RequisicaoCapturada>();

            internal void AdicionarResposta(string resposta)
            {
                _respostas.Enqueue(resposta);
            }

            public TransportResponse Execute(TransportRequest request)
            {
                if (_respostas.Count == 0)
                {
                    throw new InvalidOperationException("Não há resposta configurada para a requisição controlada.");
                }

                Requisicoes.Add(new RequisicaoCapturada
                {
                    Metodo = request.Method,
                    Url = request.RequestUri,
                    Corpo = request.HttpContent == null ? null : request.HttpContent.ReadAsStringAsync().GetAwaiter().GetResult(),
                    UsaCertificado = request.UseCertificate,
                    Certificado = request.Certificate
                });

                return new TransportResponse
                {
                    StatusCode = HttpStatusCode.OK,
                    HttpResponseMessage = new HttpResponseMessage(HttpStatusCode.OK)
                    {
                        Content = new StringContent(_respostas.Dequeue(), Encoding.UTF8, "application/json")
                    }
                };
            }
        }

        private sealed class RequisicaoCapturada
        {
            internal string Metodo { get; set; }

            internal string Url { get; set; }

            internal string Corpo { get; set; }

            internal bool UsaCertificado { get; set; }

            internal X509Certificate2 Certificado { get; set; }
        }
    }
}
