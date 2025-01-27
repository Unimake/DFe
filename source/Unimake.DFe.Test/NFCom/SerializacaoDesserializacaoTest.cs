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

        [Theory]
        [Trait("DFe", "NFCom")]
<<<<<<< HEAD
        [InlineData(@"..\..\..\NFCom\Resources\consStatServNFCom-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServNFCom(string arqXML)
=======
        [InlineData(@"..\..\..\NFCom\Resources\eventoNFCom.xml")]
        public void SerializacaoDesserializacaoEventoNFCom(string arqXML)
>>>>>>> b9bcec7f6e11b5a61479e02d78345d04fb99611b
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

<<<<<<< HEAD
            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom>(doc);
=======
            var nfCom = new Unimake.Business.DFe.Xml.NFCom.EventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.EventoNFCom>(doc);
>>>>>>> b9bcec7f6e11b5a61479e02d78345d04fb99611b

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
<<<<<<< HEAD
        [InlineData(@"..\..\..\NFCom\Resources\retConsStatServNFCom.xml")]
        public void SerializacaoDesserializacaoRetConsStatServNFCom(string arqXML)
=======
        [InlineData(@"..\..\..\NFCom\Resources\retEventoNFCom.xml")]
        public void SerializacaoDesserializacaoRetEventoNFCom(string arqXML)
>>>>>>> b9bcec7f6e11b5a61479e02d78345d04fb99611b
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

<<<<<<< HEAD
            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetConsStatServNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetConsStatServNFCom>(doc);
=======
            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetEventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetEventoNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\procEventoNFCom.xml")]
        public void SerializacaoDesserializacaoProcEventoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ProcEventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ProcEventoNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retConsSitNFCom.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetConsSitNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetConsSitNFCom>(doc);
>>>>>>> b9bcec7f6e11b5a61479e02d78345d04fb99611b

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
