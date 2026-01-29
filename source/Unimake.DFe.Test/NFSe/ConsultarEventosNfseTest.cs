using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFSe;
using Xunit;

namespace Unimake.DFe.Test.NFSe
{
    /// <summary>
    /// Testar o serviço: ConsultarEventosNfse
    /// </summary>
    public class ConsultarEventosNfseTest
    {
        /// <summary>
        /// Consultar Eventos NFSe para saber se a conexão com o webservice está ocorrendo corretamente.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o XML</param>
        [Theory]
        [Trait("DFe", "NFSe")]
        [Trait("Layout", "Nacional")]
        [InlineData(TipoAmbiente.Homologacao, PadraoNFSe.NACIONAL, "1.01", 1001058)]
        [InlineData(TipoAmbiente.Producao, PadraoNFSe.NACIONAL, "1.01", 1001058)]
        public void ConsultarEventosNfse(TipoAmbiente tipoAmbiente, PadraoNFSe padraoNFSe, string versaoSchema, int codMunicipio)
        {
            var nomeXMLEnvio = "ConsultarEventosNfse-ped-consevennfse.xml";
            var arqXML = "..\\..\\..\\NFSe\\Resources\\" + padraoNFSe.ToString() + "\\" + versaoSchema + "\\" + nomeXMLEnvio;

            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado.");

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFSe,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                CodigoMunicipio = codMunicipio,
                Servico = Servico.NFSeConsultarEventosDiversos,
                SchemaVersao = versaoSchema
            };

            var consultarEventos = new ConsultarEventosNfse(conteudoXML, configuracao);
            Assert.Multiple(() => TestUtility.AnalisaResultado(consultarEventos));

            var resultado = consultarEventos.Result;
            Assert.NotNull(resultado);

            Assert.NotNull(resultado.DataHoraProcessamento);
            Assert.NotNull(resultado.VersaoAplicativo);
            Assert.InRange(resultado.TipoAmbiente, 1, 2);

            if (resultado.Eventos != null)
            {
                Assert.NotNull(resultado.Eventos.ChaveAcesso);
                Assert.NotNull(resultado.Eventos.TipoEvento);
                Assert.NotNull(resultado.Eventos.ArquivoXml);
                Assert.NotNull(resultado.Eventos.ArquivoXml.Evento);

                var evento = resultado.Eventos.ArquivoXml.Evento;
                Assert.NotNull(evento.InfEvento);
                Assert.NotNull(evento.InfEvento.Id);
                Assert.NotNull(evento.InfEvento.PedRegEvento);

                var xmlEnvio = new Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento();
                xmlEnvio = xmlEnvio.LerXML<Business.DFe.Xml.NFSe.NACIONAL.ConsPedRegEvento>(conteudoXML);

                Assert.Equal(xmlEnvio.InfConsPedRegEvento.ChNFSe, resultado.Eventos.ChaveAcesso);
                Assert.Equal(xmlEnvio.InfConsPedRegEvento.TipoEvento, resultado.Eventos.TipoEvento);
            }
            else if (resultado.Erro != null)
            {
                Assert.NotNull(resultado.Erro.Codigo);
                Assert.NotNull(resultado.Erro.Descricao);
            }
            else
            {
                Assert.Fail("Retorno não contém eventos nem erro");
            }
        }
    }
}
