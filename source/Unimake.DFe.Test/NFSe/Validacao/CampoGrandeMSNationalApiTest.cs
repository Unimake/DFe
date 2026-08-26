using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;
using NFSeServicoBase = Unimake.Business.DFe.Servicos.NFSe.ServicoBase;

namespace Unimake.DFe.Test.NFSe.Validacao
{
    [Trait("DFe", "NFSe")]
    public class CampoGrandeMSNationalApiTest
    {
        [Theory]
        [InlineData("GerarNfse", "post", "dpsXmlGZipB64", "nfseXmlGZipB64", "true", "/notafiscal-ws/nfse")]
        [InlineData("CancelarNfse", "post", "pedidoRegistroEventoXmlGZipB64", "eventoXmlGZipB64", "true", "/notafiscal-ws/nfse/{Chave}/eventos")]
        [InlineData("ConsultarNfse", "get", "", "nfseXmlGZipB64", "false", "/notafiscal-ws/nfse/{Chave}")]
        [InlineData("ConsultarNfsePorRps", "get", "", "chaveAcesso", "false", "/notafiscal-ws/nfse/dps/{Chave}")]
        public void DeveConfigurarServicosNacionaisVersao101(
            string nomeServico,
            string metodoApi,
            string webAction,
            string webTagRetorno,
            string gzipCompress,
            string caminho)
        {
            var configuracaoXml = new XmlDocument();
            configuracaoXml.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFSe\CampoGrandeMS.xml");

            var servico = configuracaoXml.SelectSingleNode($"/Configuracoes/Servicos/{nomeServico}[@versao='1.01']");

            Assert.NotNull(servico);
            Assert.Equal(metodoApi, servico.SelectSingleNode("MetodoAPI").InnerText);
            Assert.Equal(webAction, servico.SelectSingleNode("WebActionProducao").InnerText);
            Assert.Equal(webTagRetorno, servico.SelectSingleNode("WebTagRetorno").InnerText);
            Assert.Equal(gzipCompress, servico.SelectSingleNode("GZIPCompress").InnerText);
            Assert.Equal("https://nfseapi.campogrande.ms.gov.br" + caminho, servico.SelectSingleNode("RequestURIProducao").InnerText);
            Assert.Equal("https://nfse2-cgr.dsfweb.com.br" + caminho, servico.SelectSingleNode("RequestURIHomologacao").InnerText);
        }

        [Fact]
        public void DeveConfigurarSchemaNoCancelamentoNacionalDSF()
        {
            var configuracaoXml = new XmlDocument();
            configuracaoXml.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            var servico = configuracaoXml.SelectSingleNode(
                "/ServicosValidacao/NFSe/Padrao[@nome='DSF']/Servico[@tagRaiz='pedRegEvento' and @versao='1.01' and @tagIdentificadora='e101101']");

            Assert.NotNull(servico);
            Assert.Equal("pedRegEvento_v1.01.xsd", servico.SelectSingleNode("SchemaArquivo").InnerText);
        }

        [Fact]
        public void DeveUsarChaveDaNFSeNaUrlDeCancelamento()
        {
            var conteudoXml = new XmlDocument();
            conteudoXml.Load(@"..\..\..\NFSe\Resources\DSF\1.01\CancelarNfseEnvio-ped-cannfse.xml");

            var method = typeof(NFSeServicoBase).GetMethod(
                "ObterChaveDSF",
                BindingFlags.NonPublic | BindingFlags.Static);

            var chave = (string)method.Invoke(
                null,
                new object[] { conteudoXml, Servico.NFSeCancelarNfse });

            Assert.Equal(
                "14001591201761135000132000000000000022096100197260",
                chave);
        }

        [Fact]
        public void DeveValidarCancelamentoNacionalDSFComSchema()
        {
            var conteudoXml = new XmlDocument();
            conteudoXml.Load(@"..\..\..\NFSe\Resources\DSF\1.01\CancelarNfseEnvio-ped-cannfse.xml");

            var validador = new ValidarSchema();
            validador.Validar(
                conteudoXml,
                "NFSe.DSF.pedRegEvento_v1.01.xsd",
                "http://www.sped.fazenda.gov.br/nfse",
                PadraoNFSe.DSF);

            Assert.True(validador.Success, validador.ErrorMessage);
        }

        [Fact]
        public void DeveRejeitarCancelamentoDSFSemChaveDaNFSe()
        {
            var conteudoXml = new XmlDocument();
            conteudoXml.Load(@"..\..\..\NFSe\Resources\DSF\1.01\CancelarNfseEnvio-ped-cannfse.xml");
            conteudoXml.DocumentElement.SelectSingleNode("//*[local-name()='chNFSe']").ParentNode.RemoveChild(
                conteudoXml.DocumentElement.SelectSingleNode("//*[local-name()='chNFSe']"));

            var validador = new ValidarSchema();
            validador.Validar(
                conteudoXml,
                "NFSe.DSF.pedRegEvento_v1.01.xsd",
                "http://www.sped.fazenda.gov.br/nfse",
                PadraoNFSe.DSF);

            Assert.False(validador.Success);
            Assert.Contains("chNFSe", validador.ErrorMessage);
        }
    }
}
