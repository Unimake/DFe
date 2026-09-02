using System.Xml;
using Unimake.Business.DFe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Validacao
{
    [Trait("DFe", "NFSe")]
    public class CaceresMTNationalApiTest
    {
        [Fact]
        public void DeveIncorporarConfiguracaoDoMunicipioNaDLL()
        {
            using (var stream = typeof(Configuration).Assembly.GetManifestResourceStream(
                "Unimake.Business.DFe.Servicos.Config.NFSe.CaceresMT.xml"))
            {
                Assert.NotNull(stream);
            }
        }

        [Theory]
        [InlineData("GerarNfse", "post", "dpsXmlGZipB64", "nfseXmlGZipB64", "true", "/nota/nacional/nfse")]
        [InlineData("CancelarNfse", "post", "pedidoRegistroEventoXmlGZipB64", "eventoXmlGZipB64", "true", "/nota/nacional/nfse/{Chave}/eventos")]
        [InlineData("ConsultarNfse", "get", "", "nfseXmlGZipB64", "false", "/nota/nacional/nfse/{Chave}")]
        [InlineData("ConsultarNfsePorRps", "get", "", "chaveAcesso", "false", "/nota/nacional/dps/{Chave}")]
        public void DeveConfigurarServicosNacionaisVersao101(
            string nomeServico,
            string metodoApi,
            string webAction,
            string webTagRetorno,
            string gzipCompress,
            string caminho)
        {
            var configuracaoXml = new XmlDocument();
            configuracaoXml.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Servicos\Config\NFSe\CaceresMT.xml");

            var servico = configuracaoXml.SelectSingleNode($"/Configuracoes/Servicos/{nomeServico}[@versao='1.01']");

            Assert.NotNull(servico);
            Assert.Equal(metodoApi, servico.SelectSingleNode("MetodoAPI").InnerText);
            Assert.Equal(webAction, servico.SelectSingleNode("WebActionProducao").InnerText);
            Assert.Equal(webTagRetorno, servico.SelectSingleNode("WebTagRetorno").InnerText);
            Assert.Equal("1.01", servico.SelectSingleNode("SchemaVersao").InnerText);
            Assert.Equal(gzipCompress, servico.SelectSingleNode("GZIPCompress").InnerText);
            Assert.Equal("https://cidadaoonline.caceres.rlz.com.br" + caminho, servico.SelectSingleNode("RequestURIProducao").InnerText);
            Assert.Equal("https://caceres.prefeitura.rlz.com.br" + caminho, servico.SelectSingleNode("RequestURIHomologacao").InnerText);
        }

        [Fact]
        public void DeveUsarValidacaoNacionalExistenteDoPadraoRLZ()
        {
            var configuracaoXml = new XmlDocument();
            configuracaoXml.Load(@"..\..\..\..\.NET Standard\Unimake.Business.DFe\Xml\Validar\ValidarConfig.xml");

            Assert.NotNull(configuracaoXml.SelectSingleNode(
                "/ServicosValidacao/NFSe/Padrao[@nome='RLZ_INFORMATICA']/Servico[@tagRaiz='DPS' and @versao='1.01']/SchemaArquivo[text()='DPS_v1.01.xsd']"));
            Assert.NotNull(configuracaoXml.SelectSingleNode(
                "/ServicosValidacao/NFSe/Padrao[@nome='RLZ_INFORMATICA']/Servico[@tagRaiz='pedRegEvento' and @versao='1.01']/SchemaArquivo[text()='pedRegEvento_v1.01.xsd']"));
        }
    }
}
