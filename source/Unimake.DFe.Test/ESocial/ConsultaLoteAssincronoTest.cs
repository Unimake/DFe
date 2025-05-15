using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class ConsultaLoteAssincronoTest
    {
        /// <summary>
        /// Testar o Download de Eventos Por ID
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialConsultaLoteAssincrono(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.ConsultarLoteEventos
            {
                ConsultaLoteEventos = new ConsultaLoteEventos
                {
                    ProtocoloEnvio = "1.8.11111111111111111111",
                }
            };

            var consultaLoteAssincrono = new Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono(conteudoXML, configuracao);
            consultaLoteAssincrono.Executar();
        }

        /// <summary>
        /// Testar a serialização e desserialização da Consulta
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S1210-ret-esocial-consloteevt.xml")]
        public void GerarXmlDistribuicaoESocial(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var retornoConsultaESocial = new XmlDocument();
            retornoConsultaESocial.Load(arqXML);

            var envioESocialXML = new XmlDocument();
            envioESocialXML.Load(@"..\..\..\ESocial\Resources\S1210-esocial-loteevt.xml");

            var envioESocialObjeto = new ESocialEnvioLoteEventos();
            envioESocialObjeto = XMLUtility.Deserializar<ESocialEnvioLoteEventos>(envioESocialXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.ConsultarLoteEventos
            {
                ConsultaLoteEventos = new ConsultaLoteEventos
                {
                    ProtocoloEnvio = "1.8.11111111111111111111",
                }
            };

            var consultaLoteAssincrono = new Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono(conteudoXML, configuracao);

            consultaLoteAssincrono.ESocialEnvioLoteEventos = envioESocialObjeto;
            consultaLoteAssincrono.Executar();
            consultaLoteAssincrono.RetornoWSXML = retornoConsultaESocial;
            consultaLoteAssincrono.RetornoWSString = consultaLoteAssincrono.RetornoWSXML.OuterXml;
            consultaLoteAssincrono.GravarXmlDistribuicao(@"..\..\..\ESocial\Resources");
        }

        /// <summary>
        /// Teste construtor simplificado para uso de API
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultaLoteAssincronoConstrutor(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var protocolo = "1.8.11111111111111111111";

            var consultaLoteAssincrono = new Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono(protocolo, configuracao);
            consultaLoteAssincrono.Executar();

            Assert.Equal(Servico.ESocialConsultaEvts, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Contains(protocolo, consultaLoteAssincrono.ConteudoXMLOriginal.OuterXml);
            Assert.NotNull(consultaLoteAssincrono.RetornoWSXML);
            Assert.NotNull(consultaLoteAssincrono.RetornoWSString);


        }

    }
}