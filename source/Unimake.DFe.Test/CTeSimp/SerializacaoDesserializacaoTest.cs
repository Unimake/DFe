using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.CTeSimp;
using Xunit;

namespace Unimake.DFe.Test.CTeSimp
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML retCteSimp
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\retCteSimp.xml")]
        public void SerializacaoDesserializacaoRetCteSimp(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetCTeSimp();
            xml = xml.LerXML<RetCTeSimp>(doc);

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CTeSimp
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\CTeSimp.xml")]
        public void SerializacaoDesserializacaoCTeSimp(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Business.DFe.Xml.CTeSimp.CTeSimp();
            xml = xml.LerXML<Business.DFe.Xml.CTeSimp.CTeSimp>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}