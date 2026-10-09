using System.Net;
using System.Net.Http;
using System.Text;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Unimake.Business.DFe.Xml.Validar;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Validacao
{
    [Trait("DFe", "NFSe")]
    public class SIGCORPConfigurationTest
    {
        [Fact]
        public void DeveConfigurarEndpointsDaApiSIGCORP()
        {
            var configuracaoXml = new XmlDocument();
            configuracaoXml.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFSe\SIGCORP.xml");

            Assert.NotNull(configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/GerarNfse[@versao='1.01' and MetodoAPI='post' and WebContentType='application/xml']"));
            Assert.NotNull(configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/ConsultarNfse[@versao='1.01' and MetodoAPI='get' and RequestURIProducao[contains(., '{protocolo}')]]"));
            Assert.NotNull(configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/GerarNfse[WebTagRetorno='prop:innertext']"));
            Assert.NotNull(configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/ConsultarNfse[WebTagRetorno='prop:innertext']"));
            Assert.Equal(
                "https://qa.webservice.meumunicipio.online/v1/nfse/recepcao",
                configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/GerarNfse/RequestURIHomologacao").InnerText);
            Assert.Equal(
                "https://qa.webservice.meumunicipio.online/v1/nfse/consulta/{protocolo}",
                configuracaoXml.SelectSingleNode("/Configuracoes/Servicos/ConsultarNfse/RequestURIHomologacao").InnerText);
        }

        [Fact]
        public void DeveIdentificarGeracaoSIGCORP101()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<DPS versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infDPS Id=\"DPS123\" /></DPS>");

            Assert.Equal(
                Servico.NFSeGerarNfse,
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SIGCORP, "1.01", 9999908));
        }

        [Fact]
        public void DeveIdentificarConsultaSIGCORP101()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<NFSe versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe Id=\"NFSe123\" /><CNPJ>12345678000195</CNPJ></NFSe>");

            Assert.Equal(
                Servico.NFSeConsultarNfse,
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SIGCORP, "1.01", 9999908));
        }

        [Fact]
        public void DeveRemoverNNFSeAntesDaValidacaoSIGCORP()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFSe\Resources\SIGCORP\1.01\GerarNfseEnvio-env-loterps.xml");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 9999908,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.01",
                MunicipioUsuario = "123456",
                MunicipioSenha = "senha"
            };

            new GerarNfse(xml, configuracao);

            Assert.Equal("1", configuracao.Headers["X-NFSe-nNFSe"]);
            Assert.Null(xml.SelectSingleNode("//*[local-name()='infDPS']/*[local-name()='nNFSe']"));
        }

        [Fact]
        public void DeveInicializarGeracaoLegadaSIGCORP()
        {
            var configuracao = CriarConfiguracaoSIGCORP204(Servico.NFSeGerarNfse);
            var excecao = Record.Exception(() => new GerarNfse(
                CarregarXmlSIGCORP204("GerarNfseEnvio-env-loterps.xml"), configuracao));

            Assert.Null(excecao);
            Assert.Equal("TOKEN-LEGADO", configuracao.MunicipioToken);
            Assert.Empty(configuracao.Headers);
        }

        [Fact]
        public void DeveInicializarCancelamentoLegadoSIGCORP()
        {
            var excecao = Record.Exception(() => new CancelarNfse(
                CarregarXmlSIGCORP204("CancelarNfseEnvio-ped-cannfse.xml"),
                CriarConfiguracaoSIGCORP204(Servico.NFSeCancelarNfse)));

            Assert.Null(excecao);
        }

        [Fact]
        public void DeveInicializarRecepcaoLoteLegadaSIGCORP()
        {
            var excecao = Record.Exception(() => new GerarNfse(
                CarregarXmlSIGCORP204("EnviarLoteRpsEnvio-env-loterps.xml"),
                CriarConfiguracaoSIGCORP204(Servico.NFSeRecepcionarLoteRps)));

            Assert.Null(excecao);
        }

        [Fact]
        public void DeveManterRespostaDeSucessoSIGCORP()
        {
            var configuracao = CriarConfiguracaoSIGCORP101();
            var servico = new GerarNfse(CarregarXmlSIGCORP101(), configuracao);

            using (ApiTransportExecutorFactory.Override(() => new TransporteSIGCORP("{\"protocolo\":\"PROTOCOLO-TESTE\"}")))
            {
                servico.Executar();
            }

            Assert.Contains("PROTOCOLO-TESTE", servico.RetornoWSString);
        }

        [Fact]
        public void DevePreservarResolucaoLegadaDoSIGCORP()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<GerarNota><DescricaoRps /></GerarNota>");

            Assert.Equal(
                Servico.NFSeGerarNfse,
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SIGCORP, "1.03", 4113700));
        }

        private static XmlDocument CarregarXmlSIGCORP204(string nomeArquivo)
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFSe\Resources\SIGCORP\2.04\" + nomeArquivo);
            return xml;
        }

        private static Configuracao CriarConfiguracaoSIGCORP204(Servico servico) => new Configuracao
        {
            TipoDFe = TipoDFe.NFSe,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CodigoMunicipio = 4204202,
            Servico = servico,
            SchemaVersao = "2.04",
            MunicipioUsuario = "usuario-legado",
            MunicipioSenha = "senha-legado",
            MunicipioToken = "TOKEN-LEGADO"
        };

        private static XmlDocument CarregarXmlSIGCORP101()
        {
            var xml = new XmlDocument();
            xml.Load(@"..\..\..\NFSe\Resources\SIGCORP\1.01\GerarNfseEnvio-env-loterps.xml");
            return xml;
        }

        private static Configuracao CriarConfiguracaoSIGCORP101() => new Configuracao
        {
            TipoDFe = TipoDFe.NFSe,
            TipoAmbiente = TipoAmbiente.Homologacao,
            CodigoMunicipio = 9999908,
            Servico = Servico.NFSeGerarNfse,
            SchemaVersao = "1.01",
            MunicipioUsuario = "123456",
            MunicipioSenha = "senha"
        };

        private sealed class TransporteSIGCORP : IApiTransportExecutor
        {
            private readonly string _resposta;

            internal TransporteSIGCORP(string resposta)
            {
                _resposta = resposta;
            }

            public TransportResponse Execute(TransportRequest request) => new TransportResponse
            {
                StatusCode = HttpStatusCode.Accepted,
                HttpResponseMessage = new HttpResponseMessage(HttpStatusCode.Accepted)
                {
                    Content = new StringContent(_resposta, Encoding.UTF8, "application/json")
                }
            };
        }
    }
}
