using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class BPeSerializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void SerializacaoDesserializacaoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.BPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.BPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeProc.xml")]
        public void SerializacaoDesserializacaoBPeProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new BPeProc();
            var xml = bpe.LerXML<BPeProc>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
