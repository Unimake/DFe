using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.BPe;
using BPeTAXml = Unimake.Business.DFe.Xml.BPeTA;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class BPeTASerializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTA_minimo.xml")]
        public void SerializacaoDesserializacaoBPeTA(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new BPeTAXml.BPeTA();
            var xml = bpe.LerXML<BPeTAXml.BPeTA>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTA_minimo.xml", @"..\..\..\BPe\Resources\retBPe.xml")]
        public void SerializacaoDesserializacaoBPeTAProc(string arqXMLBPeTA, string arqXMLRetBPe)
        {
            Assert.True(File.Exists(arqXMLBPeTA), "Arquivo " + arqXMLBPeTA + " nao foi localizado para a realizacao da serializacao/desserializacao.");
            Assert.True(File.Exists(arqXMLRetBPe), "Arquivo " + arqXMLRetBPe + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var docBPeTA = new XmlDocument();
            docBPeTA.Load(arqXMLBPeTA);

            var docRetBPe = new XmlDocument();
            docRetBPe.Load(arqXMLRetBPe);

            var bpeTAProc = new BPeTAXml.BPeTAProc
            {
                Versao = "1.00",
                BPeTA = new BPeTAXml.BPeTA().LerXML<BPeTAXml.BPeTA>(docBPeTA),
                ProtBPe = new RetBPe().LerXML<RetBPe>(docRetBPe).ProtBPe
            };

            var doc = bpeTAProc.GerarXML();
            var xml = new BPeTAXml.BPeTAProc().LerXML<BPeTAXml.BPeTAProc>(doc);
            var doc2 = xml.GerarXML();

            Assert.Equal("bpeTAProc", doc.DocumentElement.Name);
            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
