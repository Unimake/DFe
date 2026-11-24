using System;
using System.IO;
using System.Net;
using System.Reflection;
using System.Xml.Serialization;
#if INTEROP
using System.Runtime.InteropServices;
using Unimake.DFe.Test.AACBS.Validacao;
using ServicosNFe = Unimake.Business.DFe.Servicos.NFe;
using XmlNFe = Unimake.Business.DFe.Xml.NFe;
#endif
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSRevisaoSegurancaTest
    {
        private const string VersaoExterna = "999999999999";
        private const string EndpointNFe = "https://nfe.example.test/distribuicao";
        private const string DestinoInseguro = "http://destino.example.test/fiscal?dado=SENTINELA-URI";

        [Fact]
        public void UsaMetodosERotasEmbutidosMesmoComConfiguracaoExternaAtiva()
        {
            var pasta = Directory.CreateTempSubdirectory("aacbs-config-teste-");
            var arquivo = Path.Combine(pasta.FullName, "AN.xml");
            try
            {
                File.WriteAllText(arquivo, $$"""
                    <Configuracoes>
                      <VersaoConfiguracao>{{VersaoExterna}}</VersaoConfiguracao>
                      <Servicos ID="AACBS">
                        <Autenticar versao="1.00">
                          <MetodoAPI>get</MetodoAPI>
                          <RequestURIProducao>http://destino.example.test/token</RequestURIProducao>
                        </Autenticar>
                        <ConsultarDebitos versao="1.00">
                          <MetodoAPI>get</MetodoAPI>
                          <RequestURIProducao>http://destino.example.test/debitos/{cnpj}</RequestURIProducao>
                          <RequestURILoginProducao>http://destino.example.test/login</RequestURILoginProducao>
                        </ConsultarDebitos>
                      </Servicos>
                      <Servicos ID="NFe">
                        <DistribuicaoDFe versao="1.35">
                          <WebEnderecoProducao>{{EndpointNFe}}</WebEnderecoProducao>
                        </DistribuicaoDFe>
                      </Servicos>
                    </Configuracoes>
                    """);

                var configuracao = CriarConfiguracao();
                configuracao.TipoDFe = TipoDFe.NFe;
                configuracao.Servico = Servico.NFeDistribuicaoDFe;
                configuracao.CodigoUF = (int)UFBrasil.AN;
                configuracao.SchemaVersao = "1.35";
                configuracao.BuscarConfiguracaoPastaBase = true;
                configuracao.PastaArquivoConfiguracao = pasta.FullName;
                configuracao.HasProxy = true;
                configuracao.ProxyServer = "proxy.example.test";
                configuracao.ProxyPort = 3128;
                configuracao.TimeOutWebServiceConnect = 12345;
                configuracao.ColetarTelemetriaDisponibilidade = true;
                configuracao.Load("DistribuicaoDFe");
                Assert.Equal(EndpointNFe, configuracao.WebEnderecoProducao);

                var transporte = new AACBSTransporteControlado();
                transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
                transporte.Enfileirar(HttpStatusCode.Created, JsonAbertura);
                using var executor = ApiTransportExecutorFactory.Override(() => transporte);
                using var autenticacao = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao);
                Assert.Equal("Autenticar", configuracao.NomeServicoObservacaoDisponibilidade);
                using var abertura = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj }, configuracao);
                Assert.Equal("ConsultarDebitos", configuracao.NomeServicoObservacaoDisponibilidade);

                abertura.Executar();
                autenticacao.Executar();

                Assert.Collection(transporte.Requisicoes,
                    token =>
                    {
                        Assert.Equal(EndpointToken, token.Transporte.RequestUri);
                        Assert.Equal("POST", token.Transporte.Method);
                        Assert.StartsWith("Basic ", token.Headers["Authorization"]);
                    },
                    debitos =>
                    {
                        Assert.Equal(BaseNegocio + "/debitos/" + Cnpj, debitos.Transporte.RequestUri);
                        Assert.Equal("POST", debitos.Transporte.Method);
                        Assert.Equal("Bearer " + Token, debitos.Headers["Authorization"]);
                    });
                Assert.All(transporte.Requisicoes, requisicao =>
                {
                    Assert.Equal(12345, requisicao.Transporte.Timeout);
                    Assert.True(requisicao.Transporte.UseProxy);
                    var proxy = Assert.IsType<WebProxy>(requisicao.Transporte.Proxy);
                    Assert.Equal(new Uri("http://proxy.example.test:3128/"), proxy.Address);
                });

                Assert.True(configuracao.BuscarConfiguracaoPastaBase);
                Assert.Equal(pasta.FullName, configuracao.PastaArquivoConfiguracao);
                Assert.True(configuracao.ColetarTelemetriaDisponibilidade);
                Assert.True(abertura.Configuracoes.ColetarTelemetriaDisponibilidade);
            }
            finally
            {
                File.Delete(arquivo);
                pasta.Delete();
            }
        }

        [Fact]
        public void RejeitaDestinoHttpAntesDeBuscarOuEnviarCredencial()
        {
            var configuracao = CriarConfiguracao();
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abertura = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj }, configuracao);
            using var autenticacao = new ServicosAACBS.Autenticar(new XmlAACBS.Autenticar(), configuracao);

            var falhaFiscal = Assert.Throws<InvalidOperationException>(() =>
                abertura.EnviarFiscal(DestinoInseguro, null, ServicosAACBS.RespostaAACBS.Abertura));

            var baseAACBS = typeof(ServicosAACBS.ServicoBase);
            var emissor = baseAACBS.GetField("emissor", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.NotNull(emissor);
            emissor.SetValue(autenticacao, DestinoInseguro);
            var falhaOAuth = Assert.Throws<InvalidOperationException>(() => autenticacao.Executar());

            const string autorizacao = "Bearer SENTINELA-AUTORIZACAO";
            var enviar = baseAACBS.GetMethod("Enviar", BindingFlags.Instance | BindingFlags.NonPublic);
            Assert.NotNull(enviar);
            var falhaEnvio = Assert.Throws<TargetInvocationException>(() =>
                enviar.Invoke(abertura, new object[] { "POST", DestinoInseguro, null, autorizacao, false, null }));

            Assert.All(new[] { falhaFiscal, falhaOAuth, Assert.IsType<InvalidOperationException>(falhaEnvio.InnerException) }, falha =>
            {
                Assert.Contains("destino", falha.Message, StringComparison.OrdinalIgnoreCase);
                Assert.DoesNotContain("SENTINELA-", falha.ToString());
                Assert.DoesNotContain(configuracao.ClientSecret, falha.ToString());
                Assert.DoesNotContain(configuracao.ClientID, falha.ToString());
            });
            Assert.Empty(transporte.Requisicoes);
            Assert.Equal((HttpStatusCode)0, abertura.HttpStatusCode);
            Assert.Equal((HttpStatusCode)0, autenticacao.HttpStatusCode);
        }

        [Fact]
        public void MantemTokenPublicoEmDotNetEExcluidoDeXmlECom()
        {
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            var propriedade = typeof(Configuracao).GetProperty(nameof(Configuracao.AACBSToken));

            Assert.NotNull(propriedade);
            Assert.True(propriedade.GetMethod.IsPublic);
            Assert.True(propriedade.SetMethod.IsPublic);
            Assert.Equal(Token, configuracao.AACBSToken);
            Assert.NotNull(propriedade.GetCustomAttribute<XmlIgnoreAttribute>());
#if INTEROP
            Assert.False(Assert.IsType<ComVisibleAttribute>(propriedade.GetCustomAttribute<ComVisibleAttribute>()).Value);
#endif

            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.Created, JsonAbertura);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var abertura = new ServicosAACBS.ConsultarDebitos(new XmlAACBS.ConsultarDebitos { Cnpj = Cnpj }, configuracao);
            abertura.Executar();

            var requisicao = Assert.Single(transporte.Requisicoes);
            Assert.Equal(BaseNegocio + "/debitos/" + Cnpj, requisicao.Transporte.RequestUri);
            Assert.Equal("Bearer " + Token, requisicao.Headers["Authorization"]);
        }

#if INTEROP
        [Fact]
        public void LeitoresComPreservamQuebrasAACBSESerializacaoAnteriorDaNFe()
        {
            const string texto = "A\rB\r\nC";
            using var consulta = new ServicosAACBS.ConsultarSituacaoSolicitacao(
                new XmlAACBS.ConsultarSituacaoSolicitacao { TiqueteSolicitacao = texto }, CriarConfiguracao());

            var original = AACBSFixture.Ler(consulta.GetConteudoXMLOriginal());
            var assinado = AACBSFixture.Ler(consulta.GetConteudoXMLAssinado());
            Assert.Equal(texto, AACBSFixture.Procurar(original, "tiqueteSolicitacao").InnerText);
            Assert.Equal(texto, AACBSFixture.Procurar(assinado, "tiqueteSolicitacao").InnerText);

            using var nfe = new ServicosNFe.StatusServico(new XmlNFe.ConsStatServ
            {
                Versao = "4.00",
                CUF = UFBrasil.PR,
                TpAmb = TipoAmbiente.Producao
            }, new Configuracao { TipoDFe = TipoDFe.NFe, TipoEmissao = TipoEmissao.Normal });
            // Insere CR após a validação do leiaute, que exige xServ = STATUS.
            nfe.ConteudoXMLOriginal.GetElementsByTagName("xServ")[0].InnerText = texto;
            nfe.ConteudoXMLAssinado.GetElementsByTagName("xServ")[0].InnerText = texto;

            Assert.Equal(nfe.ConteudoXMLOriginal.OuterXml, nfe.GetConteudoXMLOriginal());
            Assert.Equal(nfe.ConteudoXMLAssinado.OuterXml, nfe.GetConteudoXMLAssinado());
        }
#endif
    }
}
