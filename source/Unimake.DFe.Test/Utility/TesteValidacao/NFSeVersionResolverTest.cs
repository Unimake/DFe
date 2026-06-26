using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.Utility.TesteValidacao
{
    public class NFSeVersionResolverTest
    {
        [Theory]
        [InlineData(PadraoNFSe.DSF, "<ns1:ConsultaSeqRps xmlns:ns1=\"urn:dsf\" />", 0, "1.00")]
        [InlineData(PadraoNFSe.DSF, "<CancelarNfseEnvio xmlns=\"http://www.ginfes.com.br/servico_cancelar_nfse_envio\" />", 0, "2.00")]
        [InlineData(PadraoNFSe.DSF, "<ConsultarSituacaoLoteRpsEnvio />", 0, "3.00")]
        [InlineData(PadraoNFSe.FIORILLI, "<ConsultarLoteRpsEnvio />", 0, "1.01")]
        [InlineData(PadraoNFSe.FIORILLI, "<GerarNfseEnvio />", 0, "2.01")]
        [InlineData(PadraoNFSe.PRONIM, "<ConsultarSituacaoLoteRpsEnvio />", 0, "1.00")]
        [InlineData(PadraoNFSe.PRONIM, "<GerarNfseEnvio />", 0, "2.03")]
        [InlineData(PadraoNFSe.SMARAPD, "<NFSe />", 0, "1.01")]
        [InlineData(PadraoNFSe.SMARAPD, "<nfd />", 0, "1.00")]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarLoteRpsEnvio />", 0, "2.04")]
        [InlineData(PadraoNFSe.SMARAPD, "<ConsultarLoteRpsEnvio />", 3205002, "2.04")]
        [InlineData(PadraoNFSe.IPM, "<nfse><nfse_teste /></nfse>", 0, "1.20")]
        [InlineData(PadraoNFSe.IPM, "<GerarNfseEnvio />", 0, "2.04")]
        public void DeveResolverVersoesDependentesDaRaiz(
            PadraoNFSe padrao,
            string conteudoXML,
            int codigoMunicipio,
            string versaoEsperada)
        {
            Assert.Equal(
                versaoEsperada,
                DefinirVersao(conteudoXML, padrao, codigoMunicipio)
            );
        }

        [Fact]
        public void DevePriorizarVersaoDeclaradaNoXml()
        {
            Assert.Equal(
                "2.03",
                DefinirVersao(
                    "<EnviarLoteRpsEnvio versao=\"2.03\" />",
                    PadraoNFSe.TINUS,
                    0
                )
            );
        }

        [Fact]
        public void DeveUsarTagIdentificadoraMesmoComVersaoConhecida()
        {
            var xml = CriarXml("<DPS><infDPS><tpAmb>2</tpAmb></infDPS></DPS>");
            var configuracao = CriarXml(
                "<ServicosValidacao>" +
                "<NFSe><Padrao nome=\"NACIONAL\">" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"tpAmb\">" +
                "<Descricao>Gerar NFSe</Descricao>" +
                "</Servico>" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"infDPS\">" +
                "<Descricao>Consultar NFSe por DPS</Descricao>" +
                "</Servico>" +
                "</Padrao></NFSe>" +
                "</ServicosValidacao>"
            );

            var servico = TratarNFSe(xml, "1.01", configuracao, PadraoNFSe.NACIONAL);

            Assert.Equal("Gerar NFSe", servico.SelectSingleNode("Descricao").InnerText);
        }

        [Fact]
        public void DeveContinuarAteONodeGenericoQuandoEspecificoNaoCombinar()
        {
            var xml = CriarXml("<DPS><infDPS /></DPS>");
            var configuracao = CriarXml(
                "<ServicosValidacao>" +
                "<NFSe><Padrao nome=\"NACIONAL\">" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\" tagIdentificadora=\"tpAmb\">" +
                "<Descricao>Gerar NFSe</Descricao>" +
                "</Servico>" +
                "<Servico tagRaiz=\"DPS\" versao=\"1.01\">" +
                "<Descricao>Genérico</Descricao>" +
                "</Servico>" +
                "</Padrao></NFSe>" +
                "</ServicosValidacao>"
            );

            var servico = TratarNFSe(xml, "1.01", configuracao, PadraoNFSe.NACIONAL);

            Assert.Equal("Genérico", servico.SelectSingleNode("Descricao").InnerText);
        }

        [Theory]
        [InlineData("<nfse><nfse_teste /></nfse>", "Gerar NFSe")]
        [InlineData("<nfse><nf><situacao>C</situacao></nf></nfse>", "Evento de cancelamento da NFSe")]
        [InlineData("<nfse><pesquisa /></nfse>", "Consulta da NFSe")]
        public void DeveIdentificarServicosIPM120(string conteudoXML, string descricaoEsperada)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                PadraoNFSe = PadraoNFSe.IPM,
                CodigoMunicipio = 4201307,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(
                CriarXml(conteudoXML),
                configuracao
            );

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal(descricaoEsperada, resultado.Descricao);
        }

        [Fact]
        public void DeveIdentificarConsultaLoteRpsIPM204()
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                PadraoNFSe = PadraoNFSe.IPM,
                CodigoMunicipio = 4202305,
                TipoAmbiente = TipoAmbiente.Homologacao
            };

            var resultado = new ValidarEstruturaXML().ValidarServico(
                CriarXml(
                    "<ConsultarLoteRpsEnvio>" +
                    "<Prestador><CpfCnpj><Cnpj>99999999999999</Cnpj></CpfCnpj></Prestador>" +
                    "<Protocolo>1</Protocolo>" +
                    "</ConsultarLoteRpsEnvio>"
                ),
                configuracao
            );

            Assert.True(resultado.Validado, resultado.MensagemRetorno);
            Assert.Equal("Consulta NFSe por RPS", resultado.Descricao);
        }

        private static string DefinirVersao(
            string conteudoXML,
            PadraoNFSe padrao,
            int codigoMunicipio)
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "DefinirVersaoNFSe",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (string)method.Invoke(
                null,
                new object[] { CriarXml(conteudoXML), padrao, codigoMunicipio }
            );
        }

        private static XmlNode TratarNFSe(
            XmlDocument xml,
            string versao,
            XmlDocument configuracao,
            PadraoNFSe padrao)
        {
            var method = typeof(ValidarEstruturaXML).GetMethod(
                "TratarNFSe",
                BindingFlags.NonPublic | BindingFlags.Static
            );

            return (XmlNode)method.Invoke(
                null,
                new object[]
                {
                    xml,
                    versao,
                    TipoDFe.NFSe,
                    xml.DocumentElement.Name,
                    configuracao,
                    padrao
                }
            );
        }

        private static XmlDocument CriarXml(string conteudoXML)
        {
            var xml = new XmlDocument();
            xml.LoadXml(conteudoXML);
            return xml;
        }
    }
}
