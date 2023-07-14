using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.CCG;
using Xunit;

namespace Unimake.DFe.Test.CCG
{
    /// <summary>
    /// Testar a serialização e deserialização dos XMLs do CTe
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e deserialização do XML ConsGTIN
        /// </summary>
        [Theory]
        [Trait("DFe", "CCG")]
        [InlineData(@"..\..\..\CCG\Resources\0000000000-consGTIN.xml")]
        public void SerializacaoDeserializacaoConsGTIN(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new ConsGTIN();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML RetConsGTIN
        /// </summary>
        [Theory]
        [Trait("DFe", "CCG")]
        [InlineData(@"..\..\..\CCG\Resources\retConsGTIN.xml")]
        public void SerializacaoDeserializacaoRetConsGTIN(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetConsGTIN();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

    }
}