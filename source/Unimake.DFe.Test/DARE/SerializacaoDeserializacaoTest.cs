using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.DARE
{
    public class SerializacaoDesserializacaoConsultasTest
    {
        /// <summary>
        /// Testar a serialização e desserialização da consulta de Eventos DARE
        /// </summary>
        [Theory]
        [Trait("DFe", "DARE")]
        [InlineData(@"..\..\..\DARE\Resources\Receitas.xml")]
        public void SerializacaoDesserializacaoConsultaTabelaDARE(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DARE.Receitas>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização da consulta de Eventos DARE
        /// </summary>
        [Theory]
        [Trait("DFe", "DARE")]
        [InlineData(@"..\..\..\DARE\Resources\DARELote.xml")]
        public void SerializacaoDesserializacaoEnvioDARELote(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.DARE.DARELote>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}