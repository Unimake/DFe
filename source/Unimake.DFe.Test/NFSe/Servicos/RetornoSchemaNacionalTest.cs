using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFSe.NACIONAL;
using Xunit;
using NFSeXml = Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe.NFSe;

namespace Unimake.DFe.Test.NFSe.Servicos
{
    public class RetornoSchemaNacionalTest
    {
        [Theory]
        [Trait("DFe", "NFSe")]
        [InlineData("GerarNfseEnvio-env-loterps.xml", false)]
        [InlineData("GerarNfseEnvio_IndicativoDecisaoJudicial-env-loterps.xml", true)]
        public void DeveInicializarGeracaoNacionalSemTransmitir(string arquivo, bool decisaoJudicial)
        {
            var xml = new XmlDocument();
            xml.Load(Path.Combine(@"..\..\..\NFSe\Resources\NACIONAL\1.01", arquivo));
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                PadraoNFSe = PadraoNFSe.NACIONAL,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoMunicipio = 1001058,
                Servico = Servico.NFSeGerarNfse,
                SchemaVersao = "1.01",
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            // Exercita assinatura e validação dos mesmos construtores dos testes de integração.
            // Não chama Executar(), para não transmitir uma operação fiscal.
            Business.DFe.Servicos.NFSe.GerarNfse servico = decisaoJudicial
                ? new Business.DFe.Servicos.NFSe.GerarNfseIndicativoDecisaoJudicial(xml, configuracao)
                : new Business.DFe.Servicos.NFSe.GerarNfse(xml, configuracao);

            Assert.NotNull(servico.ConteudoXMLAssinado);
            Assert.True(servico.ConteudoXMLAssinado.GetElementsByTagName(
                "Signature", "http://www.w3.org/2000/09/xmldsig#").Count > 0);
            Assert.True(string.IsNullOrEmpty(servico.RetornoWSString));
        }

        [Fact]
        [Trait("DFe", "NFSe")]
        public void ResultDosServicosDeveLerNFSeAtualizadaSemTransporte()
        {
            var modelo = XMLUtility.Deserializar<NFSeXml>(File.ReadAllText(
                @"..\..\..\NFSe\Resources\NACIONAL\1.01\RetornoNACIONAL.xml"));
            modelo.InfNFSe.DPS = XMLUtility.Deserializar<DPS>(File.ReadAllText(
                @"..\..\..\NFSe\Resources\NACIONAL\1.01\DPS-AjusteBC.xml"));
            modelo.InfNFSe.Valores.VCalcAjusteBCISSQN = 100;
            var xml = modelo.GerarXML();
            var configuracao = new Configuracao { PadraoNFSe = PadraoNFSe.NACIONAL };
            var gerar = new Business.DFe.Servicos.NFSe.GerarNfse
            {
                Configuracoes = configuracao, RetornoWSXML = xml, RetornoWSString = xml.OuterXml
            };
            var consultar = new Business.DFe.Servicos.NFSe.ConsultarNfse
            {
                Configuracoes = configuracao, RetornoWSXML = xml, RetornoWSString = xml.OuterXml
            };
            Assert.Equal(100, gerar.Result.InfNFSe.Valores.VCalcAjusteBCISSQN);
            Assert.Equal(2, consultar.Result.InfNFSe.DPS.InfDPS.IBSCBS.BensMoveis.Count);
        }
    }
}
