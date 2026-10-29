using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    [Trait("DFe", "NFSe")]
    public class JataiConsultaNacionalTest
    {
        [Theory]
        [InlineData(TipoAmbiente.Producao, "https://api.centi.com.br/nfe/consultar/rps/go/jatai")]
        [InlineData(TipoAmbiente.Homologacao, "https://api.centi.com.br/nfe/consultar/homologacao/rps/go/jatai")]
        public void DeveCarregarConsultaPorRpsCenti(TipoAmbiente ambiente, string endereco)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CodigoMunicipio = 5211909,
                TipoAmbiente = ambiente,
                Servico = Servico.NFSeConsultarNfsePorRps,
                SchemaVersao = "2.00"
            };

            configuracao.Load("ConsultarNfsePorRps");

            Assert.Equal(endereco, configuracao.RequestURI);
            Assert.Equal(PadraoNFSe.CENTI, configuracao.PadraoNFSe);
            Assert.Equal("post", configuracao.MetodoAPI);
            Assert.True(configuracao.LoginConexao);
            Assert.Equal("prop:innertext", configuracao.WebTagRetorno);
        }

        [Theory]
        [InlineData(TipoAmbiente.Producao, "https://sefin.nfse.gov.br/sefinnacional/nfse/", "1.01")]
        [InlineData(TipoAmbiente.Homologacao, "https://sefin.producaorestrita.nfse.gov.br/SefinNacional/nfse/", "1.01")]
        [InlineData(TipoAmbiente.Producao, "https://sefin.nfse.gov.br/sefinnacional/nfse/", null)]
        public void DeveResolverConsultaEPrepararUrlNacional(TipoAmbiente ambiente, string endereco, string versaoDeclarada)
        {
            var chave = new string('1', 50);
            var xml = new XmlDocument();
            xml.LoadXml("<NFSe xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe Id=\"NFS" + chave + "\"/></NFSe>");
            if (versaoDeclarada != null)
            {
                xml.DocumentElement.SetAttribute("versao", versaoDeclarada);
            }

            var versao = ValidarEstruturaXML.DefinirVersaoNFSe(xml, PadraoNFSe.CENTI, 5211909);
            var servico = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.CENTI, versao, 5211909, ambiente);
            Assert.Equal("1.01", versao);
            Assert.Equal(Servico.NFSeConsultarNfse, servico);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CodigoMunicipio = 5211909,
                TipoAmbiente = ambiente,
                Servico = servico,
                SchemaVersao = versao
            };

            using (var consulta = new ConsultarNfse(xml, configuracao))
            {
                Assert.Equal(PadraoNFSe.CENTI, consulta.Configuracoes.PadraoNFSe);
                Assert.Equal(endereco + chave, consulta.Configuracoes.RequestURI);
                Assert.Equal("get", consulta.Configuracoes.MetodoAPI);
                Assert.Equal("nfseXmlGZipB64", consulta.Configuracoes.WebTagRetorno);
                Assert.False(consulta.Configuracoes.LoginConexao);
                Assert.True(consulta.Configuracoes.UsaCertificadoDigital);
            }
        }

        [Theory]
        [InlineData("ConsultarNfseRpsEnvio", "IdentificacaoRps", Servico.NFSeConsultarNfsePorRps)]
        [InlineData("GerarNfseEnvio", "Rps", Servico.NFSeGerarNfse)]
        [InlineData("CancelarNfseEnvio", "InfPedidoCancelamento", Servico.NFSeCancelarNfse)]
        [InlineData("SubstituirNfseEnvio", "RpsSubstituido", Servico.NFSeSubstituirNfse)]
        public void DeveManterOperacoesCentiNaVersao200(string raiz, string identificador, Servico servicoEsperado)
        {
            var xml = new XmlDocument();
            xml.LoadXml("<" + raiz + " xmlns=\"http://www.centi.com.br/files/nfse.xsd\"><" + identificador + "/></" + raiz + ">");

            var versao = ValidarEstruturaXML.DefinirVersaoNFSe(xml, PadraoNFSe.CENTI, 5211909);
            var servico = ValidarEstruturaXML.DefinirTipoServicoNFSe(xml, PadraoNFSe.CENTI, versao, 5211909);

            Assert.Equal("2.00", versao);
            Assert.Equal(servicoEsperado, servico);
        }

        [Fact]
        public void DevePreservarMontagemDaUrlNoPadraoNacional()
        {
            var chave = new string('2', 50);
            var xml = new XmlDocument();
            xml.LoadXml("<NFSe versao=\"1.01\" xmlns=\"http://www.sped.fazenda.gov.br/nfse\"><infNFSe Id=\"NFS" + chave + "\"/></NFSe>");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CodigoMunicipio = 1001058,
                TipoAmbiente = TipoAmbiente.Producao,
                Servico = Servico.NFSeConsultarNfse,
                SchemaVersao = "1.01"
            };

            using (var consulta = new ConsultarNfse(xml, configuracao))
            {
                Assert.Equal("https://sefin.nfse.gov.br/sefinnacional/nfse/" + chave, consulta.Configuracoes.RequestURI);
            }
        }

        [Fact]
        public void DeveExtrairChaveDoAtributoComNamespaceEOutrosAtributos()
        {
            var chave = new string('3', 50);
            var xml = new XmlDocument();
            xml.LoadXml("<n:NFSe versao='1.01' xmlns:n='http://www.sped.fazenda.gov.br/nfse'><n:infNFSe Id='NFS" + chave + "'/></n:NFSe>");
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CodigoMunicipio = 5211909,
                TipoAmbiente = TipoAmbiente.Homologacao,
                Servico = Servico.NFSeConsultarNfse,
                SchemaVersao = "1.01"
            };

            using (var consulta = new ConsultarNfse(xml, configuracao))
            {
                Assert.Equal("https://sefin.producaorestrita.nfse.gov.br/SefinNacional/nfse/" + chave, consulta.Configuracoes.RequestURI);
            }
        }
    }
}
