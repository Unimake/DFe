using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;

namespace Unimake.DFe.Test.CIOT
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarSituacaoTransportador.xml")]
        public void SerializacaoDesserializacaoConsultarSituacaoTransportador(string arqXML)
        {
            SerializarDesserializar<ConsultarSituacaoTransportador>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarSituacaoTransportador.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarSituacaoTransportadorErro.xml")]
        public void SerializacaoDesserializacaoRetConsultarSituacaoTransportador(string arqXML)
        {
            SerializarDesserializar<RetConsultarSituacaoTransportador>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarFrotaTransportador.xml")]
        public void SerializacaoDesserializacaoConsultarFrotaTransportador(string arqXML)
        {
            SerializarDesserializar<ConsultarFrotaTransportador>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarFrotaTransportador.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarFrotaTransportadorErro.xml")]
        public void SerializacaoDesserializacaoRetConsultarFrotaTransportador(string arqXML)
        {
            SerializarDesserializar<RetConsultarFrotaTransportador>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml")]
        public void SerializacaoDesserializacaoDeclaracaoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<DeclaracaoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporteErro.xml")]
        public void SerializacaoDesserializacaoRetDeclaracaoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<RetDeclaracaoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml")]
        public void SerializacaoDesserializacaoCancelamentoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<CancelamentoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporte.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retCancelamentoOperacaoTransporteErro.xml")]
        public void SerializacaoDesserializacaoRetCancelamentoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<RetCancelamentoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml")]
        public void SerializacaoDesserializacaoRetificacaoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<RetificacaoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporte.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retRetificacaoOperacaoTransporteErro.xml")]
        public void SerializacaoDesserializacaoRetRetificacaoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<RetRetificacaoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml")]
        public void SerializacaoDesserializacaoEncerramentoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<EncerramentoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporte.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retEncerramentoOperacaoTransporteErro.xml")]
        public void SerializacaoDesserializacaoRetEncerramentoOperacaoTransporte(string arqXML)
        {
            SerializarDesserializar<RetEncerramentoOperacaoTransporte>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarExcecao.xml")]
        public void SerializacaoDesserializacaoConsultarExcecao(string arqXML)
        {
            SerializarDesserializar<ConsultarExcecao>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarExcecao.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarExcecaoErro.xml")]
        public void SerializacaoDesserializacaoRetConsultarExcecao(string arqXML)
        {
            SerializarDesserializar<RetConsultarExcecao>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml")]
        public void SerializacaoDesserializacaoConsultarCIOTGerado(string arqXML)
        {
            SerializarDesserializar<ConsultarCIOTGerado>(arqXML);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarCIOTGerado.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retConsultarCIOTGeradoErro.xml")]
        public void SerializacaoDesserializacaoRetConsultarCIOTGerado(string arqXML)
        {
            SerializarDesserializar<RetConsultarCIOTGerado>(arqXML);
        }

        private static void SerializarDesserializar<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new T().LerXML<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }
    }
}
