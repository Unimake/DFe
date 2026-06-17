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

        /// <summary>
        /// Testar serializacao e desserializacao do XML de retorno da criacao de cobranca PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoRetPIXCobrancaCriar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.retPIXCobrancaCriar>(@"..\..\..\PIX\Resources\retPIXCobrancaCriar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de retorno da consulta de cobranca PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoRetPIXCobrancaConsultar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.retPIXCobrancaConsultar>(@"..\..\..\PIX\Resources\retPIXCobrancaConsultar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de retorno da consulta de PIX
        /// </summary>
        [Fact]
        [Trait("DFe", "PIX")]
        public void SerializacaoDeserializacaoRetPIXConsultar() =>
            SerializarDeserializar<Business.DFe.Xml.PIX.retPIXConsultar>(@"..\..\..\PIX\Resources\retPIXConsultar.xml");

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
