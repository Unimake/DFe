using System.Xml;
using Unimake.Business.DFe;
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
        public void DevePreservarResolucaoLegadaDoSIGCORP()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<GerarNota><DescricaoRps /></GerarNota>");

            Assert.Equal(
                Servico.NFSeGerarNfse,
                ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.SIGCORP, "1.03", 4113700));
        }
    }
}
