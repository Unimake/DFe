using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class SerializacaoDesserealizacaoDownloadsTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do download por ID de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\DownloadEventosPorId-esocial-downevt.xml")]
        public void SerializacaoDesserealizacaoDownloadPorID(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorID>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do download por NrRecibo de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\DownloadEventosPorNrRec-esocial-downevt.xml")]
        public void SerializacaoDesserealizacaoDownloadPorNrRec(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.DownloadEventosPorNrRec>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

    }
}
