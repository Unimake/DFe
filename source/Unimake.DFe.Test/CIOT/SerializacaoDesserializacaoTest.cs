using System.IO;
using System.Threading.Tasks;
using System.Xml;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTDeclaracaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;

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
            var xml = SerializarDesserializar<DeclaracaoOperacaoTransporte>(arqXML);
            Assert.Equal(2, xml.DadosCarga.ContratantesCargFrac.Count);
            Assert.Equal("12345678000195", xml.DadosCarga.ContratantesCargFrac[0]);
            Assert.Equal("98765432000110", xml.DadosCarga.ContratantesCargFrac[1]);
            Assert.Equal(2, xml.GerarXML().SelectNodes("/*[local-name()='DeclaracaoOperacaoTransporte']/*[local-name()='DadosCarga']/*[local-name()='ContratantesCargFrac']").Count);
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

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml")]
        public async Task SerializacaoJsonDatasDeclaracaoOperacaoTransporte(string arqXML)
        {
            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(doc);
            var servico = new CIOTDeclaracaoOperacaoTransporte(xml, new Configuracao
            {
                TipoDFe = TipoDFe.CIOT,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = (int)UFBrasil.AN
            });
            var jsonText = await servico.Configuracoes.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken);
            Assert.Contains("\"DataDeclaracao\":\"2026-05-25T10:00:00-03:00\"", jsonText);
            Assert.Contains("\"DataInicioViagem\":\"2026-05-25\"", jsonText);
            Assert.Contains("\"DataFimViagem\":\"2026-05-26\"", jsonText);
            Assert.DoesNotContain("DataDeclaracaoField", jsonText);
            Assert.DoesNotContain("DataInicioViagemField", jsonText);
            Assert.DoesNotContain("DataFimViagemField", jsonText);
            var contratantesCargFrac = (JArray)JObject.Parse(jsonText)["DadosCarga"]["ContratantesCargFrac"];
            Assert.Equal(2, contratantesCargFrac.Count);
            Assert.Equal("12345678000195", contratantesCargFrac[0]);
            Assert.Equal("98765432000110", contratantesCargFrac[1]);
        }

        private static T SerializarDesserializar<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new T().LerXML<T>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
            return xml;
        }
    }
}
