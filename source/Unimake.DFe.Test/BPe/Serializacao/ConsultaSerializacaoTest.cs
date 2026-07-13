using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;

namespace Unimake.DFe.Test.BPe.Serializacao
{
    public class ConsultaSerializacaoTest
    {
        [Theory]
        [Trait("DFe", "BPe")]
        [InlineData(@"..\..\..\BPe\Resources\consStatServBPe-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServBPe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var bpe = new ConsStatServBPe();
            var xml = bpe.LerXML<ConsStatServBPe>(doc);

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

            var bpe = new ConsSitBPe();
            var xml = bpe.LerXML<ConsSitBPe>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
