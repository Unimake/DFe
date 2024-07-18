using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTeSimp;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.CTeSimp
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\retCTeSimp.xml")]
        public void SerializacaoDesserializacaoRetCTeSimp(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetCTeSimp();
            xml = xml.LerXML<RetCTeSimp>(doc);

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}