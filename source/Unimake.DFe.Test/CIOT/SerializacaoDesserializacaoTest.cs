using System.IO;
using System.Threading.Tasks;
using System.Xml;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTCancelamentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte;
using CIOTConsultarCIOTGerado = Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado;
using CIOTConsultarExcecao = Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao;
using CIOTConsultarFrotaTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarFrotaTransportador;
using CIOTConsultarSituacaoTransportador = Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador;
using CIOTDeclaracaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte;
using CIOTEncerramentoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte;
using CIOTGerarIdOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte;
using CIOTRetificacaoOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte;

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
            var xml = SerializarDesserializar<RetConsultarFrotaTransportador>(arqXML);

            if (xml.Temp == null)
            {
                Assert.Single(xml.Frota);
                Assert.Equal("ABC1D23", xml.Frota[0].PlacaVeiculo);

                var json = JsonConvert.SerializeObject(xml);
                Assert.Contains("\"Frota\":", json);
                Assert.DoesNotContain("\"VeiculoFrota\":", json);
            }
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml")]
        public void SerializacaoDesserializacaoDeclaracaoOperacaoTransporte(string arqXML)
        {
            var xml = SerializarDesserializar<DeclaracaoOperacaoTransporte>(arqXML);
            Assert.Equal("0001", xml.DadosCarga.CodigoNaturezaCarga);
            Assert.Equal(2, xml.DadosCarga.ContratantesCargFrac.Count);
            Assert.Equal("12345678000195", xml.DadosCarga.ContratantesCargFrac[0]);
            Assert.Equal("98765432000110", xml.DadosCarga.ContratantesCargFrac[1]);
            Assert.Equal(2, xml.GerarXML().SelectNodes("/*[local-name()='DeclaracaoOperacaoTransporte']/*[local-name()='DadosCarga']/*[local-name()='ContratantesCargFrac']").Count);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ValidarSchemaCodigoNaturezaCargaComUmDigito()
        {
            ValidarSchema(CriarXmlDeclaracaoOperacaoTransporte("1"), true);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarCodigoNaturezaCargaComMaisDeQuatroDigitos()
        {
            ValidarSchema(CriarXmlDeclaracaoOperacaoTransporte("12345"), false);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void ValidarSchemaRetDeclaracaoOperacaoTransporteComMensagensMultiplas()
        {
            var doc = new XmlDocument();
            doc.Load(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporteMensagensMultiplas.xml");

            ValidarSchema(doc, true);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporte.xml")]
        [InlineData(@"..\..\..\CIOT\Resources\retDeclaracaoOperacaoTransporteErro.xml")]
        public void SerializacaoDesserializacaoRetDeclaracaoOperacaoTransporte(string arqXML)
        {
            var xml = SerializarDesserializar<RetDeclaracaoOperacaoTransporte>(arqXML);

            if (xml.Temp == null)
            {
                Assert.Single(xml.Mensagens);
                Assert.Equal("000000", xml.Mensagens[0].Codigo);
                Assert.Equal("Operacao realizada com sucesso", xml.Mensagens[0].Descricao);
                Assert.Equal("000000", xml.Codigo);
                Assert.Equal("Operacao realizada com sucesso", xml.Mensagem);
            }
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
            Assert.DoesNotContain("ValorFreteField", jsonText);
            var json = LerJson(jsonText);
            Assert.Equal("1500.50", (string)json["ValorFrete"]);
            Assert.Null(json["InfPagamento"][0]["Parcelas"]);
            Assert.Null(json["InfPagamento"][0]["Parcela"]);
            Assert.Equal("1", (string)json["InfPagamento"][0]["NumeroParcela"]);
            Assert.Equal("2026-05-30", (string)json["InfPagamento"][0]["DataVencimento"]);
            Assert.Equal("1500.50", (string)json["InfPagamento"][0]["ValorParcela"]);
            var contratantesCargFrac = (JArray)json["DadosCarga"]["ContratantesCargFrac"];
            Assert.Equal(2, contratantesCargFrac.Count);
            Assert.Equal("12345678000195", contratantesCargFrac[0]);
            Assert.Equal("98765432000110", contratantesCargFrac[1]);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public async Task SerializacaoJsonEnvioServicosCIOT()
        {
            var consultarSituacaoTransportador = new CIOTConsultarSituacaoTransportador(LerXmlArquivo<ConsultarSituacaoTransportador>(@"..\..\..\CIOT\Resources\consultarSituacaoTransportador.xml"), CriarConfiguracao());
            var consultarSituacaoTransportadorJson = await LerJsonServico(consultarSituacaoTransportador);
            Assert.Equal("12345678901", (string)consultarSituacaoTransportadorJson["CpfCnpjTransportador"]);

            var consultarFrotaTransportador = new CIOTConsultarFrotaTransportador(LerXmlArquivo<ConsultarFrotaTransportador>(@"..\..\..\CIOT\Resources\consultarFrotaTransportador.xml"), CriarConfiguracao());
            var consultarFrotaTransportadorJson = await LerJsonServico(consultarFrotaTransportador);
            Assert.Equal("12345678901", (string)consultarFrotaTransportadorJson["CpfCnpjTransportador"]);

            var declaracaoOperacaoTransporte = new CIOTDeclaracaoOperacaoTransporte(LerXmlArquivo<DeclaracaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml"), CriarConfiguracao());
            var declaracaoOperacaoTransporteJson = await LerJsonServico(declaracaoOperacaoTransporte);
            Assert.Equal("OP1234567890", (string)declaracaoOperacaoTransporteJson["IdOperacaoTransporte"]);
            Assert.Equal("1500.50", (string)declaracaoOperacaoTransporteJson["ValorFrete"]);
            Assert.Equal("2026-05-25T10:00:00-03:00", (string)declaracaoOperacaoTransporteJson["DataDeclaracao"]);
            Assert.Equal("ABC1D23", (string)declaracaoOperacaoTransporteJson["Veiculos"][0]["Placa"]);
            Assert.Equal("4118402", (string)declaracaoOperacaoTransporteJson["OrigemDestino"][0]["Origem"]["CodigoMunicipioOrigem"]);
            Assert.Null(declaracaoOperacaoTransporteJson["ValorFreteField"]);
            Assert.Null(declaracaoOperacaoTransporteJson["DataDeclaracaoField"]);

            var cancelamentoOperacaoTransporte = new CIOTCancelamentoOperacaoTransporte(LerXmlArquivo<CancelamentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\cancelamentoOperacaoTransporte.xml"), CriarConfiguracao());
            var cancelamentoOperacaoTransporteJson = await LerJsonServico(cancelamentoOperacaoTransporte);
            Assert.Equal("1234567890123456", (string)cancelamentoOperacaoTransporteJson["CodigoIdentificacaoOperacao"]);
            Assert.Equal("Operacao nao realizada", (string)cancelamentoOperacaoTransporteJson["MotivoCancelamento"]);

            var retificacaoOperacaoTransporte = new CIOTRetificacaoOperacaoTransporte(LerXmlArquivo<RetificacaoOperacaoTransporte>(@"..\..\..\CIOT\Resources\retificacaoOperacaoTransporte.xml"), CriarConfiguracao());
            var retificacaoOperacaoTransporteJson = await LerJsonServico(retificacaoOperacaoTransporte);
            Assert.Equal("1550.75", (string)retificacaoOperacaoTransporteJson["ValorFrete"]);
            Assert.Equal("2026-05-27", (string)retificacaoOperacaoTransporteJson["DataFimViagem"]);
            Assert.Equal("4118402", (string)retificacaoOperacaoTransporteJson["OrigemDestino"][0]["Origem"]["CodigoMunicipioOrigem"]);
            Assert.Null(retificacaoOperacaoTransporteJson["ValorFreteField"]);
            Assert.Null(retificacaoOperacaoTransporteJson["DataFimViagemField"]);

            var encerramentoOperacaoTransporte = new CIOTEncerramentoOperacaoTransporte(LerXmlArquivo<EncerramentoOperacaoTransporte>(@"..\..\..\CIOT\Resources\encerramentoOperacaoTransporte.xml"), CriarConfiguracao());
            var encerramentoOperacaoTransporteJson = await LerJsonServico(encerramentoOperacaoTransporte);
            Assert.Equal("1234567890123456", (string)encerramentoOperacaoTransporteJson["CodigoIdentificacaoOperacao"]);
            Assert.Equal("1", (string)encerramentoOperacaoTransporteJson["OrigemDestino"][0]["QtdViagens"]);
            Assert.Equal("1000.00", (string)encerramentoOperacaoTransporteJson["DadosCarga"]["PesoTotalCarga"]);

            var consultarCIOTGerado = new CIOTConsultarCIOTGerado(LerXmlArquivo<ConsultarCIOTGerado>(@"..\..\..\CIOT\Resources\consultarCIOTGerado.xml"), CriarConfiguracao());
            var consultarCIOTGeradoJson = await LerJsonServico(consultarCIOTGerado);
            Assert.Equal("123456789012", (string)consultarCIOTGeradoJson["CodigoIdentificacaoOperacao"]);
            Assert.Equal("2026", (string)consultarCIOTGeradoJson["AnoDeclaracao"]);

            var gerarIdOperacaoTransporte = new CIOTGerarIdOperacaoTransporte(LerXmlArquivo<GerarIdOperacaoTransporte>(@"..\..\..\CIOT\Resources\gerarIdOperacaoTransporte.xml"), CriarConfiguracao());
            var gerarIdOperacaoTransporteJson = await LerJsonServico(gerarIdOperacaoTransporte);
            Assert.Equal("41942626000102", (string)gerarIdOperacaoTransporteJson["cpfCnpj"]);
            Assert.Null(gerarIdOperacaoTransporteJson["Versao"]);

            var consultarExcecao = new CIOTConsultarExcecao(LerXmlArquivo<ConsultarExcecao>(@"..\..\..\CIOT\Resources\consultarExcecao.xml"), CriarConfiguracao());
            Assert.Null(consultarExcecao.Configuracoes.HttpContent);
            Assert.Contains("CPFCNPJTransportador=12345678901", consultarExcecao.Configuracoes.RequestURI);
        }

        private static T SerializarDesserializar<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var xml = LerXmlArquivo<T>(arqXML);
            var doc = new XmlDocument();
            doc.Load(arqXML);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
            return xml;
        }

        private static T LerXmlArquivo<T>(string arqXML) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            return new T().LerXML<T>(doc);
        }

        private static async Task<JObject> LerJsonServico(dynamic servico)
        {
            Assert.NotNull(servico.Configuracoes.HttpContent);

            var jsonText = await servico.Configuracoes.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken);

            Assert.DoesNotContain("Field", jsonText);
            return LerJson(jsonText);
        }

        private static JObject LerJson(string jsonText)
        {
            using (var reader = new JsonTextReader(new StringReader(jsonText)) { DateParseHandling = DateParseHandling.None })
            {
                return JObject.Load(reader);
            }
        }

        private static Configuracao CriarConfiguracao()
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.CIOT,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = TipoAmbiente.Homologacao,
                CodigoUF = (int)UFBrasil.AN
            };
        }

        private static XmlDocument CriarXmlDeclaracaoOperacaoTransporte(string codigoNaturezaCarga)
        {
            var doc = new XmlDocument();
            doc.Load(@"..\..\..\CIOT\Resources\declaracaoOperacaoTransporte.xml");
            doc.GetElementsByTagName("CodigoNaturezaCarga")[0].InnerText = codigoNaturezaCarga;

            return doc;
        }

        private static void ValidarSchema(XmlDocument doc, bool sucessoEsperado)
        {
            var validar = new ValidarSchema();
            validar.Validar(doc, "CIOT.declaracaoOperacaoTransporte_v1.00.xsd", "http://www.antt.gov.br/ciot");

            Assert.Equal(sucessoEsperado, validar.Success);
        }
    }
}
