using System.IO;
using System.Xml;
using Xunit;

namespace Unimake.DFe.Test.BPe
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTM_minimo.xml")]
        public void SerializacaoDesserializacaoBPeTM(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.BPeTM();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.BPeTM>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\consStatServBPe-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.ConsStatServBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.ConsStatServBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
