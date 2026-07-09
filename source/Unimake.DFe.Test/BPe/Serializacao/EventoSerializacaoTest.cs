using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class EventoSerializacaoTest
    {
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

            var bpe = new EventoBPe();
            var xml = bpe.LerXML<EventoBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\procEventoBPe.xml")]
        public void SerializacaoDesserializacaoProcEventoBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new ProcEventoBPe();
            var xml = bpe.LerXML<ProcEventoBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
