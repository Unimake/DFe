using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserealizacao
    {
        /// <summary>
        /// Testar a serialização e desserialização do Evento 1000 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1000_evtInfoEmpregador-esocial-evt .xml")]
        public void SerializacaoDesserializacaoESocial1000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1005 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1005_evtTabEstab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1005(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.eSocial.ESocial1005>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

    }
}
