using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\nfcom_meio_completa-nfcom.xml")]
        [InlineData(@"..\..\..\NFCom\Resources\nfcom_completa-nfcom.xml")]
        public void SerializacaoDesserializacaoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.NFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.NFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retNFCom.xml")]
        public void SerializacaoDesserializacaoRetNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\protNFCom.xml")]
        public void SerializacaoDesserializacaoProtNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ProtNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ProtNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
