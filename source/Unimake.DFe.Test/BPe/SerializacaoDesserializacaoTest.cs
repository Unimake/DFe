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

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\consSitBPe-ped-sit.xml")]
        public void SerializacaoDesserializacaoConsSitBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.ConsSitBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.ConsSitBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110111-cancelamento.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110115-nao-embarque.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110116-alteracao-poltrona.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110117-excesso-bagagem.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110300-vinc-pgto.xml")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110301-canc-vinc-pgto.xml")]
        public void SerializacaoDesserializacaoEventoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new Unimake.Business.DFe.Xml.BPe.EventoBPe();
            var xml = bpe.LerXML<Unimake.Business.DFe.Xml.BPe.EventoBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
