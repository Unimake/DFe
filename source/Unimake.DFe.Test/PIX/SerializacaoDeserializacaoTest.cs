using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.PIX
{
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar serializacao e desserializacao do XML de criacao de cobranca PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoPixCobrancaCriar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.PixCobrancaCriar>(@"..\..\..\PIX\Resources\PixCobrancaCriar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de consulta de cobranca PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoPixCobrancaConsultar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.PixCobrancaConsultar>(@"..\..\..\PIX\Resources\PixCobrancaConsultar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de consulta de PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoPixConsultar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.PixConsultar>(@"..\..\..\PIX\Resources\PixConsultar.xml");

        private static void SerializarDeserializar<T>(string arqXML) where T : Business.DFe.Xml.XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
