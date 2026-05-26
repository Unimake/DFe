using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.UMessenger
{
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar serialização e desserialização do XML de envio de texto
        /// </summary>
        [Theory]
        [Trait("DFe", "UMessenger")]
        [InlineData(@"..\..\..\UMessenger\Resources\uMessengerText.xml")]
        [InlineData(@"..\..\..\UMessenger\Resources\uMessengerTextMultiplo.xml")]
        public void SerializacaoDeserializacaoSendTextMessage(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Business.DFe.Xml.UMessenger.uMessengerSendTextMessage>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
