using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.EBoleto
{
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar serializacao e desserializacao do XML de registro de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoRegistrar() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoRegistrar>(@"..\..\..\EBoleto\Resources\BoletoRegistrar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de cancelamento/baixa de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoCancelar() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoCancelar>(@"..\..\..\EBoleto\Resources\BoletoCancelar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de consulta de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoConsultar() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoConsultar>(@"..\..\..\EBoleto\Resources\BoletoConsultar.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de alteracao de vencimento de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoAlterarVencto() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoAlterarVencto>(@"..\..\..\EBoleto\Resources\BoletoAlterarVencto.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de envio de instrucao de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoEnviarInstrucao() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoEnviarInstrucao>(@"..\..\..\EBoleto\Resources\BoletoEnviarInstrucao.xml");

        /// <summary>
        /// Testar serializacao e desserializacao do XML de informacao de pagamento de boleto
        /// </summary>
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void SerializacaoDeserializacaoBoletoInformarPagto() =>
            SerializarDeserializar<Business.DFe.Xml.EBoleto.BoletoInformarPagto>(@"..\..\..\EBoleto\Resources\BoletoInformarPagto.xml");

        private static void SerializarDeserializar<T>(string arqXML) where T : Business.DFe.Xml.XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " nao foi localizado para a realizacao da serializacao/desserializacao.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL esta diferente do conteudo do arquivo serializado.");
        }
    }
}
