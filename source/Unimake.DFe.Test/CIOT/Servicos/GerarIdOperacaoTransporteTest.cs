using Newtonsoft.Json.Linq;
using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTGerarIdOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte;

namespace Unimake.DFe.Test.CIOT.Servicos
{
    /// <summary>
    /// Testar o serviço auxiliar de geração do identificador da operação de transporte.
    /// </summary>
    public class GerarIdOperacaoTransporteTest
    {
        private const string ArquivoEnvio = @"..\..\..\CIOT\Resources\gerarIdOperacaoTransporte.xml";
        private const string ArquivoRetorno = @"..\..\..\CIOT\Resources\retGerarIdOperacaoTransporte.xml";
        private const string SucessoANTT = """{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Dados":{"CIOT":"560000142776","CpfCnpj":"41.942.626/0001-02","DataGeracao":"2026-06-02T13:56:15.1898189"},"Erros":[]}""";

        /// <summary>
        /// Serializar o XML de envio.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void SerializarEnvio()
        {
            var xml = new GerarIdOperacaoTransporte
            {
                CpfCnpj = "41942626000102"
            }.GerarXML();

            Assert.Equal("GerarIdOperacaoTransporte", xml.DocumentElement.Name);
            Assert.Equal("1.00", xml.DocumentElement.GetAttribute("versao"));
            Assert.Equal("41942626000102", xml.DocumentElement["CpfCnpj", "http://www.antt.gov.br/ciot"].InnerText);
        }

        /// <summary>
        /// Desserializar o XML de envio.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void DesserializarEnvio()
        {
            var xml = LerXML<GerarIdOperacaoTransporte>(ArquivoEnvio);

            Assert.Equal("1.00", xml.Versao);
            Assert.Equal("41942626000102", xml.CpfCnpj);
        }

        /// <summary>
        /// Validar schema do XML de envio.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ValidarSchemaEnvio()
        {
            ValidarSchema(ArquivoEnvio);
        }

        /// <summary>
        /// Serializar o XML de retorno.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void SerializarRetorno()
        {
            var xml = new RetGerarIdOperacaoTransporte
            {
                Sucesso = true,
                Mensagem = "CIOT gerado com sucesso",
                Dados = new DadosGerarIdOperacaoTransporte
                {
                    CIOT = "560000142776",
                    CpfCnpj = "41.942.626/0001-02",
                    DataGeracao = "2026-06-02T13:56:15.1898189"
                },
                Erros = string.Empty
            }.GerarXML();

            Assert.Equal("RetGerarIdOperacaoTransporte", xml.DocumentElement.Name);
            Assert.Equal("1.00", xml.DocumentElement.GetAttribute("versao"));
            Assert.Equal("560000142776", xml.DocumentElement["Dados", "http://www.antt.gov.br/ciot"]["CIOT", "http://www.antt.gov.br/ciot"].InnerText);
            ValidarSchema(xml, true);
        }

        /// <summary>
        /// Desserializar o XML de retorno.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void DesserializarRetorno()
        {
            var xml = LerXML<RetGerarIdOperacaoTransporte>(ArquivoRetorno);

            Assert.Equal("1.00", xml.Versao);
            Assert.True(xml.Sucesso);
            Assert.Equal("CIOT gerado com sucesso", xml.Mensagem);
            Assert.Equal("560000142776", xml.Dados.CIOT);
            Assert.Equal("41.942.626/0001-02", xml.Dados.CpfCnpj);
            Assert.Equal("2026-06-02T13:56:15.1898189", xml.Dados.DataGeracao);
            Assert.Equal("110", xml.Codigo);
        }

        /// <summary>
        /// Validar schema do XML de retorno.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ValidarSchemaRetorno()
        {
            ValidarSchema(ArquivoRetorno);
        }

        /// <summary>
        /// Rejeitar CPF/CNPJ fora do formato previsto no schema.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarCpfCnpjInvalidoNoSchema()
        {
            ValidarSchemaInvalido("<GerarIdOperacaoTransporte xmlns=\"http://www.antt.gov.br/ciot\" versao=\"1.00\"><CpfCnpj>123</CpfCnpj></GerarIdOperacaoTransporte>");
        }

        /// <summary>
        /// Rejeitar identificador fora do formato previsto no schema.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarIdOperacaoTransporteInvalidoNoSchema()
        {
            ValidarSchemaInvalido("<RetGerarIdOperacaoTransporte xmlns=\"http://www.antt.gov.br/ciot\" versao=\"1.00\"><Sucesso>true</Sucesso><Mensagem>CIOT gerado com sucesso</Mensagem><Dados><CIOT>123</CIOT><CpfCnpj>41.942.626/0001-02</CpfCnpj><DataGeracao>2026-06-02T13:56:15.1898189</DataGeracao></Dados><Erros /></RetGerarIdOperacaoTransporte>");
        }

        /// <summary>
        /// Montar somente o campo CpfCnpj no JSON e remover máscara antes do envio.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public async Task SerializarJsonEnvio()
        {
            var servico = new CIOTGerarIdOperacaoTransporte(new GerarIdOperacaoTransporte
            {
                CpfCnpj = "41.942.626/0001-02"
            }, CriarConfiguracao());

            var json = JObject.Parse(await servico.Configuracoes.HttpContent.ReadAsStringAsync(TestContext.Current.CancellationToken));

            Assert.Single(json.Properties());
            Assert.Equal("41942626000102", json["cpfCnpj"]);
            Assert.Equal("https://appservices-hml.antt.gov.br/pefServices/Gerar", servico.Configuracoes.RequestURI);
        }

        /// <summary>
        /// Selecionar o endpoint de produção.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void SelecionarEndpointProducao()
        {
            var configuracao = CriarConfiguracao();
            configuracao.TipoAmbiente = TipoAmbiente.Producao;

            var servico = new CIOTGerarIdOperacaoTransporte(new GerarIdOperacaoTransporte
            {
                CpfCnpj = "41942626000102"
            }, configuracao);

            Assert.Equal("https://appservices.antt.gov.br/pefServices/api/gerar", servico.Configuracoes.RequestURI);
        }

        /// <summary>
        /// Utilizar a biblioteca da ANTT em produção e normalizar seu retorno.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ExecutarProducaoComBibliotecaANTT()
        {
            var configuracao = CriarConfiguracao();
            configuracao.TipoAmbiente = TipoAmbiente.Producao;
            configuracao.Definida = true;

            var servico = new GerarIdOperacaoTransporteProducaoFake(new GerarIdOperacaoTransporte
            {
                CpfCnpj = "41.942.626/0001-02"
            }, configuracao);

            servico.Executar();

            Assert.Equal("41942626000102", servico.CpfCnpjRecebido);
            Assert.Equal("560000142776", servico.Result.IdOperacaoTransporte);
            Assert.Equal("41.942.626/0001-02", servico.Result.Dados.CpfCnpj);
            Assert.Equal(SucessoANTT, servico.RetornoWSRawString);
            Assert.Equal("RetGerarIdOperacaoTransporte", servico.RetornoWSXML.DocumentElement.Name);
            Assert.Equal(servico.RetornoWSXML.OuterXml, servico.RetornoWSString);
            Assert.Equal("560000142776", servico.GerarIdOperacaoTransporteProcResult.RetGerarIdOperacaoTransporte.IdOperacaoTransporte);
        }

        /// <summary>
        /// Processar retorno JSON com o campo IdOperacaoTransporte.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarJsonIdOperacaoTransporte()
        {
            ValidarSucesso(new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""IdOperacaoTransporte"":""560000088376""}"));
        }

        /// <summary>
        /// Processar o retorno JSON efetivamente devolvido pela API ANTT.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarJsonRetornoANTT()
        {
            var retorno = new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""Sucesso"":true,""Mensagem"":""CIOT gerado com sucesso"",""Dados"":{""CIOT"":""560000142776"",""CpfCnpj"":""41.942.626/0001-02"",""DataGeracao"":""2026-06-02T13:56:15.1898189""},""Erros"":[]}");

            ValidarSucesso(retorno, "560000142776");
            Assert.Equal("41.942.626/0001-02", retorno.Dados.CpfCnpj);
            Assert.Equal("2026-06-02T13:56:15.1898189", retorno.Dados.DataGeracao);
            Assert.Equal(string.Empty, retorno.Erros);
        }

        /// <summary>
        /// Processar o XML intermediário efetivamente devolvido pelo transporte HTTP.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarXmlRetornoANTT()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><Sucesso>true</Sucesso><Mensagem>CIOT gerado com sucesso</Mensagem><Dados><CIOT>560000142776</CIOT><CpfCnpj>41.942.626/0001-02</CpfCnpj><DataGeracao>2026-06-02T13:56:15.1898189</DataGeracao></Dados><Erros /></temp>");

            var servico = new CIOTGerarIdOperacaoTransporte
            {
                RetornoWSXML = xml
            };

            ValidarSucesso(servico.Result, "560000142776");
            Assert.Equal("RetGerarIdOperacaoTransporte", servico.RetornoWSXML.DocumentElement.Name);
        }

        /// <summary>
        /// Processar retorno JSON com o campo idOperacaoTransporte.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarJsonIdOperacaoTransporteCamelCase()
        {
            ValidarSucesso(new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""idOperacaoTransporte"":""560000088376""}"));
        }

        /// <summary>
        /// Processar mensagem de erro da API.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarJsonMensagemErro()
        {
            var retorno = new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""Message"":""CPF/CNPJ é obrigatório.""}");

            Assert.Equal("999", retorno.Codigo);
            Assert.Equal("CPF/CNPJ é obrigatório.", retorno.Mensagem);
        }

        /// <summary>
        /// Processar identificador retornado como texto puro.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarTextoPuro()
        {
            ValidarSucesso(new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT("560000088376"));
        }

        /// <summary>
        /// Processar texto puro após a conversão intermediária feita pelo transporte HTTP.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarTextoPuroConvertidoParaXml()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<string xmlns=\"http://schemas.microsoft.com/2003/10/Serialization/\">560000088376</string>");

            var servico = new CIOTGerarIdOperacaoTransporte
            {
                RetornoWSXML = xml
            };

            ValidarSucesso(servico.Result);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("text/plain", "012345678901")]
        [InlineData("application/xml", "<IdOperacaoTransporte>012345678901</IdOperacaoTransporte>")]
        [InlineData("application/xml", "<idOperacaoTransporte>012345678901</idOperacaoTransporte>")]
        [InlineData("application/xml", """<IdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot">012345678901</IdOperacaoTransporte>""")]
        public void ExecutarHomologacaoComIdEmRespostaHttpEscalar(string mediaType, string resposta)
        {
            const string id = "012345678901";
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, CriarConfiguracao());
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(resposta, mediaType)))
            {
                servico.Executar();
            }

            ValidarSucesso(servico.Result, id);
            Assert.Equal(resposta, servico.RetornoWSRawString);
            using var distribuicao = new MemoryStream();
            servico.GravarXmlDistribuicao(distribuicao);
            var xml = new XmlDocument();
            xml.LoadXml(Encoding.UTF8.GetString(distribuicao.ToArray()));
            Assert.Equal("GerarIdOperacaoTransporteProc", xml.DocumentElement.LocalName);
            Assert.Equal(id, xml.GetElementsByTagName("CIOT", xml.DocumentElement.NamespaceURI)[0].InnerText);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("text/plain", "01234567890")]
        [InlineData("text/plain", "Rejeição 012345678901")]
        [InlineData("text/plain", "<string><Erro>012345678901</Erro></string>")]
        [InlineData("application/xml", "<IdOperacaoTransporte>01234567890</IdOperacaoTransporte>")]
        [InlineData("application/xml", "<idOperacaoTransporte><Erro>012345678901</Erro></idOperacaoTransporte>")]
        public void RejeitarRespostaHttpComIdInvalidoOuConteudoExtra(string mediaType, string resposta)
        {
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, CriarConfiguracao());
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(resposta, mediaType)))
            {
                Assert.Throws<InvalidOperationException>(servico.Executar);
            }

            using var distribuicao = new MemoryStream();
            Assert.Throws<InvalidOperationException>(() => servico.GravarXmlDistribuicao(distribuicao));
            Assert.Equal(0, distribuicao.Length);
        }

        /// <summary>
        /// Processar erro estruturado conforme o padrão dos demais serviços CIOT.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarErroEstruturado()
        {
            var retorno = new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""error"":""USUARIO_NAO_AUTORIZADO"",""message"":""Usuário não autorizado.""}");

            Assert.Equal("1.00", retorno.Versao);
            Assert.Equal("USUARIO_NAO_AUTORIZADO", retorno.Temp.Error);
            Assert.Equal("Usuário não autorizado.", retorno.Temp.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void PreservarRejeicaoQuandoTimestampOpcionalForInvalido()
        {
            const string bruto = """{"error":"CADASTRO_BLOQUEADO","message":"Cadastro bloqueado.","timestamp":"DADO_SINTETICO_SIGILOSO"}""";
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("CADASTRO_BLOQUEADO", servico.Result.Temp.Error);
            Assert.Equal("Cadastro bloqueado.", servico.Result.Temp.Message);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            Assert.DoesNotContain("DADO_SINTETICO_SIGILOSO", servico.RetornoWSString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        /// <summary>
        /// Rejeitar retorno JSON sem conteúdo útil.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarJsonNulo()
        {
            Assert.Throws<InvalidOperationException>(() => new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT("null"));
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"CIOT":"560000088376"}""")]
        [InlineData("""{"Sucesso":true,"CIOT":"560000088376","Mensagem":"CIOT gerado com sucesso"}""")]
        [InlineData("""{"IdOperacaoTransporte":560000088376}""")]
        public void PreservarFormatosValidosDoIdentificador(string bruto)
        {
            ValidarSucesso(new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(bruto));
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("IdOperacaoTransporte")]
        [InlineData("idOperacaoTransporte")]
        [InlineData("CIOT")]
        public void ExecutarComIdentificadorEMetadadosConhecidos(string campo)
        {
            const string cpfCnpj = "12345678000199";
            const string dataGeracao = "2026-06-02T13:56:15Z";
            var bruto = $$"""{"{{campo}}":"560000088376","CpfCnpj":"{{cpfCnpj}}","DataGeracao":"{{dataGeracao}}"}""";
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            var proc = servico.GerarIdOperacaoTransporteProcResult;
            ValidarSucesso(proc.RetGerarIdOperacaoTransporte);
            Assert.Equal(cpfCnpj, proc.RetGerarIdOperacaoTransporte.Dados.CpfCnpj);
            Assert.Equal(dataGeracao, proc.RetGerarIdOperacaoTransporte.Dados.DataGeracao);
            ValidarSchema(servico.RetornoWSXML, true);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"IdOperacaoTransporte":"560000088376","Mensagem":"CIOT gerado com sucesso","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"}""")]
        [InlineData("""{"IdOperacaoTransporte":"560000088376","Mensagem":"CIOT gerado com sucesso","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z","Erros":""}""")]
        public void AceitarMensagemDeSucessoConhecidaComIdValido(string bruto)
        {
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            ValidarSucesso(servico.GerarIdOperacaoTransporteProcResult.RetGerarIdOperacaoTransporte);
            ValidarSchema(servico.RetornoWSXML, true);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Dados":{"CIOT":"123456789012","CpfCnpj":"12345678000199","DataGeracao":[]},"Extra":"a","Extra":"b"}""")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Dados":{"CIOT":"123456789012","CpfCnpj":"12345678000199","DataGeracao":[],"DataGeracao":"2026-06-02T13:56:15Z"}}""")]
        public void RejeitarJsonDuplicadoNaHomologacao(string bruto)
        {
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, CriarConfiguracao());
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(bruto)))
            {
                servico.Executar();
            }

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("RETORNO_JSON_INVALIDO", servico.Result.Temp.Error);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","CpfCnpj":"12345678000199","DataGeracao":"2026-02-30T13:56:15Z"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":"2026-06-02"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":""}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":"2026-02-30T13:56:15Z"}}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":["2026-06-02T13:56:15Z"]}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":[]}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":{"Valor":"2026-06-02T13:56:15Z"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":[]}}""")]
        [InlineData("""/* sintético */{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":[]}}""")]
        [InlineData("""/* sintético */{"IdOperacaoTransporte":"123456789012","DataGeracao":[]}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":["2026-06-02T13:56:15Z"]}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":["2026-06-02T13:56:15Z","2026-02-30T13:56:15Z"]}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":{"Valor":"2026-06-02T13:56:15Z"}}}""")]
        public void RejeitarDataGeracaoInvalidaMesmoComIdValido(string bruto)
        {
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            var retorno = servico.Result;
            Assert.Equal("999", retorno.Codigo);
            Assert.Null(retorno.IdOperacaoTransporte);
            Assert.Equal("DATA_GERACAO_INVALIDA", retorno.Temp.Error);
            Assert.Contains("DataGeracao", retorno.Temp.Message);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            Assert.DoesNotContain("<DataGeracao", servico.RetornoWSString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarDataGeracaoInvalidaNoXmlTipado()
        {
            var xml = new XmlDocument();
            xml.LoadXml("""<RetGerarIdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot" versao="1.00"><Sucesso>true</Sucesso><Dados><CIOT>123456789012</CIOT><DataGeracao>2026-02-30T13:56:15Z</DataGeracao></Dados></RetGerarIdOperacaoTransporte>""");
            var servico = new CIOTGerarIdOperacaoTransporte { RetornoWSXML = xml };

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("DATA_GERACAO_INVALIDA", servico.Result.Temp.Error);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""["2026-06-02T13:56:15Z"]""")]
        [InlineData("[]")]
        [InlineData("""{"Valor":"2026-06-02T13:56:15Z"}""")]
        public void RejeitarDataGeracaoMalformadaMesmoQuandoTransporteConverteOJson(string data)
        {
            var bruto = $$$"""{"Sucesso":true,"Dados":{"CIOT":"123456789012","DataGeracao":{{{data}}}}}""";
            var servico = new CIOTGerarIdOperacaoTransporte
            {
                RetornoWSRawString = bruto,
                RetornoWSXML = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(bruto, "temp")
            };

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("DATA_GERACAO_INVALIDA", servico.Result.Temp.Error);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarTextoAvulsoNoXmlIntermediarioDoTransporte()
        {
            var xml = new XmlDocument();
            xml.LoadXml("<temp><CIOT>123456789012</CIOT>Rejeição sintética</temp>");
            var servico = new CIOTGerarIdOperacaoTransporte { RetornoWSXML = xml };

            var erro = Assert.Throws<InvalidOperationException>(() => _ = servico.Result);

            Assert.Contains("conteúdo inesperado", erro.Message);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"Sucesso":true}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":""}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"12345678901"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"12345678901x"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"１２３４５６７８９０１２"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012\n"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"IdOperacaoTransporte":"123456789012"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":["123456789012"]}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":{"#text":"123456789012"}}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012"},"#text":"Rejeição sintética"}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012","IdOperacaoTransporte":"123456789013"}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"123456789012"},"CIOT":"123456789013"}""")]
        public void RejeitarSucessoDeclaradoSemIdValidoEImpedirDistribuicao(string bruto)
        {
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            Assert.False(servico.Result.Sucesso);
            Assert.Equal("999", servico.Result.Codigo);
            Assert.Contains("IdOperacaoTransporte válido de 12 dígitos", servico.Result.Mensagem);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            Assert.Contains("<Sucesso>false</Sucesso>", servico.RetornoWSString);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Message":"Cadastro bloqueado.","Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"}}""", "Cadastro bloqueado.", "")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Message":"","message":"Cadastro bloqueado.","Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"}}""", "Cadastro bloqueado.", "")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Message":["Cadastro bloqueado.","Revise o cadastro."],"Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"}}""", "Cadastro bloqueado.\nRevise o cadastro.", "")]
        [InlineData("""{"Sucesso":true,"Mensagem":"Rejeição original: cadastro bloqueado.","Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"},"Erros":["Cadastro bloqueado."]}""", "Rejeição original: cadastro bloqueado.", "Cadastro bloqueado.")]
        [InlineData("""{"Sucesso":true,"Mensagem":"CIOT gerado com sucesso","Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"},"Erros":["Cadastro bloqueado."]}""", "Cadastro bloqueado.", "Cadastro bloqueado.")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"},"Erros":["Cadastro bloqueado."]}""", "Cadastro bloqueado.", "Cadastro bloqueado.")]
        public void PreservarRejeicaoEstruturadaMesmoQuandoRespostaDeclaraSucesso(string bruto, string mensagem, string erros)
        {
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            var retorno = servico.Result;
            Assert.False(retorno.Sucesso);
            Assert.Equal("999", retorno.Codigo);
            Assert.Equal("012345678901", retorno.IdOperacaoTransporte);
            Assert.Equal(mensagem, retorno.Mensagem);
            Assert.Equal(erros, retorno.Erros);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void PreservarRejeicaoExplicitaNoXmlTipado()
        {
            var xml = new XmlDocument();
            xml.LoadXml("""<RetGerarIdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot" versao="1.00"><Sucesso>true</Sucesso><Mensagem>CIOT gerado com sucesso</Mensagem><Dados><CIOT>012345678901</CIOT><CpfCnpj>12345678000199</CpfCnpj><DataGeracao>2026-06-02T13:56:15Z</DataGeracao></Dados><Erros>Cadastro bloqueado.</Erros></RetGerarIdOperacaoTransporte>""");
            var servico = new CIOTGerarIdOperacaoTransporte { RetornoWSXML = xml };

            Assert.Equal("999", servico.Result.Codigo);
            Assert.False(servico.Result.Sucesso);
            Assert.Equal("012345678901", servico.Result.IdOperacaoTransporte);
            Assert.Equal("Cadastro bloqueado.", servico.Result.Mensagem);
            Assert.Equal("Cadastro bloqueado.", servico.Result.Erros);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void PreservarRejeicaoOriginalMesmoComIdentificador()
        {
            const string bruto = """{"Sucesso":false,"Mensagem":"Rejeição original","Dados":{"CIOT":"123456789012","DataGeracao":"2026-02-30T13:56:15Z"},"Erros":["Primeiro erro","Segundo erro"]}""";
            var servico = CriarServicoProducao(bruto);
            servico.Executar();

            Assert.False(servico.Result.Sucesso);
            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("Rejeição original", servico.Result.Mensagem);
            Assert.Equal("Primeiro erro\nSegundo erro", servico.Result.Erros);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("Cadastro do transportador bloqueado.", "Bad Request", "Cadastro do transportador bloqueado.")]
        [InlineData("", "Cadastro do transportador bloqueado.", "Cadastro do transportador bloqueado.")]
        [InlineData("CIOT gerado com sucesso", "Cadastro do transportador bloqueado.", "Cadastro do transportador bloqueado.")]
        public void PreservarMensagemEspecificaQuandoRetornoJaDeclaraRejeicao(string mensagem, string message, string esperado)
        {
            var bruto = $$"""{"Sucesso":false,"Mensagem":"{{mensagem}}","Message":"{{message}}","Dados":{"CIOT":"012345678901","CpfCnpj":"12345678000199","DataGeracao":"2026-06-02T13:56:15Z"},"Erros":[]}""";
            var servico = CriarServicoProducao(bruto);

            servico.Executar();

            Assert.Equal("999", servico.Result.Codigo);
            Assert.False(servico.Result.Sucesso);
            Assert.Equal(esperado, servico.Result.Mensagem);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":["012345678901"]}}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":{"#text":"012345678901"}}}""")]
        [InlineData("""{"Sucesso":true,"Dados":[{"CIOT":"012345678901"}]}""")]
        [InlineData("""{"Sucesso":true,"Dados":{"CIOT":"012345678901","IdOperacaoTransporte":[]}}""")]
        [InlineData("""{"Sucesso":[true],"Dados":{"CIOT":"012345678901"}}""")]
        public void RejeitarEstruturaJsonAmbiguaQueATransformacaoParaXmlOculta(string bruto)
        {
            var configuracao = CriarConfiguracao();
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, configuracao);
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(bruto)))
            {
                servico.Executar();
            }

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"IdOperacaoTransporte":["012345678901"]}""")]
        [InlineData("""{"IdOperacaoTransporte":{"#text":"012345678901"}}""")]
        [InlineData("""{"IdOperacaoTransporte":"012345678901","Sucesso":[]}""")]
        [InlineData("""{"IdOperacaoTransporte":"012345678901","idOperacaoTransporte":[]}""")]
        public void RejeitarIdentificadorSimplesComEstruturaJsonAmbigua(string bruto)
        {
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, CriarConfiguracao());
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(bruto)))
            {
                Assert.Throws<InvalidOperationException>(servico.Executar);
            }

            Assert.Equal(bruto, servico.RetornoWSRawString);
            using var distribuicao = new MemoryStream();
            Assert.Throws<InvalidOperationException>(() => servico.GravarXmlDistribuicao(distribuicao));
            Assert.Equal(0, distribuicao.Length);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("text/plain", "012345678901")]
        [InlineData("application/json", """{"Sucesso":true,"Dados":{"CIOT":"012345678901"}}""")]
        [InlineData("application/xml", """<RetGerarIdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot" versao="1.00"><Sucesso> true </Sucesso><Dados><CIOT>012345678901</CIOT></Dados></RetGerarIdOperacaoTransporte>""")]
        public void BloquearDistribuicaoQuandoHttpFalhaMesmoComIdValido(string mediaType, string bruto)
        {
            var servico = new CIOTGerarIdOperacaoTransporte(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, CriarConfiguracao());
            servico.Configuracoes.UsaCertificadoDigital = false;

            using (ApiTransportExecutorFactory.Override(() => new TransporteHomologacaoFake(bruto, mediaType, HttpStatusCode.BadRequest)))
            {
                servico.Executar();
            }

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("RETORNO_HTTP_INVALIDO", servico.Result.Temp.Error);
            Assert.Equal(HttpStatusCode.BadRequest, servico.HttpStatusCode);
            Assert.Equal(bruto, servico.RetornoWSRawString);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void BloquearDistribuicaoComIndicadoresDeSucessoConflitantesNoXml()
        {
            var xml = new XmlDocument();
            xml.LoadXml("""<RetGerarIdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot" versao="1.00"><Sucesso>true</Sucesso><Sucesso>false</Sucesso><Dados><CIOT>012345678901</CIOT></Dados></RetGerarIdOperacaoTransporte>""");
            var servico = new CIOTGerarIdOperacaoTransporte { RetornoWSXML = xml };

            Assert.Equal("999", servico.Result.Codigo);
            Assert.Equal("INDICADOR_SUCESSO_INVALIDO", servico.Result.Temp.Error);
            ValidarSchema(servico.RetornoWSXML, true);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData(null, "vazio")]
        [InlineData(" \r\n ", "vazio")]
        [InlineData("null", "inválido")]
        [InlineData("""{"DADO_SINTETICO_SIGILOSO":""", "inválido")]
        [InlineData("""{"DADO_SINTETICO_SIGILOSO":"123456789012"}""", "inesperado")]
        [InlineData("""{"Dados":{"IdOperacaoTransporte":"123456789012"}}""", "inesperado")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","idOperacaoTransporte":"123456789013"}""", "inesperado")]
        [InlineData("""{"CIOT":"123456789012","#text":"Rejeição sintética"}""", "inesperado")]
        [InlineData("""{"IdOperacaoTransporte":{"#text":"123456789012"}}""", "inesperado")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","DataGeracao":"2026-06-02T13:56:15Z","Estado":"Erro"}""", "inesperado")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","IdOperacaoTransporte":"123456789013"}""", "inválido")]
        public void LimparSucessoAnteriorECapturarBrutoQuandoParsingFalha(string bruto, string diagnostico)
        {
            var servico = CriarServicoProducao(SucessoANTT);
            servico.Executar();
            var distribuicoes = servico.GerarIdOperacaoTransporteProcResults;
            Assert.Single(distribuicoes);

            servico.Resposta = bruto;
            var erro = Assert.Throws<InvalidOperationException>(servico.Executar);

            Assert.Contains(diagnostico, erro.Message);
            Assert.DoesNotContain("DADO_SINTETICO_SIGILOSO", erro.ToString());
            Assert.Equal(bruto, servico.RetornoWSRawString);
            Assert.Null(servico.RetornoWSString);
            Assert.Null(servico.RetornoWSXML);
            Assert.Null(servico.Result.IdOperacaoTransporte);
            Assert.Equal("999", servico.Result.Codigo);
            Assert.Empty(distribuicoes);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void LimparBrutoAnteriorQuandoComponenteLancaExcecao()
        {
            var servico = CriarServicoProducao(SucessoANTT);
            servico.Executar();
            servico.FalhaComponente = new InvalidOperationException("DADO_SINTETICO_SIGILOSO");

            var erro = Assert.Throws<InvalidOperationException>(servico.Executar);

            Assert.Contains("HRESULT", erro.Message);
            Assert.DoesNotContain("DADO_SINTETICO_SIGILOSO", erro.ToString());
            Assert.Null(servico.RetornoWSRawString);
            Assert.Null(servico.RetornoWSString);
            Assert.Null(servico.RetornoWSXML);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Theory]
        [Trait("DFe", "CIOT")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Message":"Rejeição original"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"Rejeição original"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"CIOT gerado com sucesso","Message":"Rejeição original"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"CIOT gerado com sucesso","Erros":"","Message":"Rejeição original"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"CIOT gerado com sucesso","Message":"","message":"Rejeição original"}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"CIOT gerado com sucesso","Erros":["Rejeição original"]}""")]
        [InlineData("""{"IdOperacaoTransporte":"123456789012","Mensagem":"CIOT gerado com sucesso","Erros":"Rejeição original"}""")]
        public void MensagemDeErroTemPrecedenciaSobreIdentificador(string bruto)
        {
            var servico = CriarServicoProducao(bruto);
            servico.Executar();

            var retorno = servico.Result;
            Assert.Equal("999", retorno.Codigo);
            Assert.Null(retorno.IdOperacaoTransporte);
            Assert.Equal("Rejeição original", retorno.Mensagem);
            ValidarDistribuicaoBloqueada(servico);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RevalidarXmlTipadoEManterRetornoBrutoDoTransporte()
        {
            var xml = new XmlDocument();
            xml.LoadXml("""<RetGerarIdOperacaoTransporte xmlns="http://www.antt.gov.br/ciot" versao="1.00"><Sucesso>true</Sucesso><Dados><CIOT>123</CIOT></Dados></RetGerarIdOperacaoTransporte>""");
            var retorno = new RetGerarIdOperacaoTransporte().LerXML<RetGerarIdOperacaoTransporte>(xml);
            Assert.Equal("999", retorno.Codigo);
            var servico = new CIOTGerarIdOperacaoTransporte { RetornoWSXML = xml, RetornoWSRawString = "original" };

            Assert.False(servico.Result.Sucesso);
            Assert.Contains("IdOperacaoTransporte válido de 12 dígitos", servico.Result.Mensagem);
            Assert.Equal("original", servico.RetornoWSRawString);
            ValidarDistribuicaoBloqueada(servico);
        }

        private static void ValidarDistribuicaoBloqueada(CIOTGerarIdOperacaoTransporte servico)
        {
            var erro = Assert.Throws<InvalidOperationException>(() => _ = servico.GerarIdOperacaoTransporteProcResult);
            Assert.Contains("XML de distribuição", erro.Message);
            using var stream = new MemoryStream();
            Assert.Throws<InvalidOperationException>(() => servico.GravarXmlDistribuicao(stream));
            Assert.Equal(0, stream.Length);
            Assert.Throws<InvalidOperationException>(() => servico.GravarXmlDistribuicao(Path.GetTempPath()));
        }

        private static GerarIdOperacaoTransporteProducaoFake CriarServicoProducao(string resposta)
        {
            var configuracao = CriarConfiguracao();
            configuracao.TipoAmbiente = TipoAmbiente.Producao;
            configuracao.Definida = true;
            return new GerarIdOperacaoTransporteProducaoFake(
                new GerarIdOperacaoTransporte { CpfCnpj = "12345678000199" }, configuracao) { Resposta = resposta };
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

        private static T LerXML<T>(string arquivo) where T : XMLBase, new()
        {
            Assert.True(File.Exists(arquivo), "Arquivo " + arquivo + " não foi localizado.");

            var doc = new XmlDocument();
            doc.Load(arquivo);

            return new T().LerXML<T>(doc);
        }

        private static void ValidarSchema(string arquivo)
        {
            var doc = new XmlDocument();
            doc.Load(arquivo);

            ValidarSchema(doc, true);
        }

        private static void ValidarSchemaInvalido(string conteudoXML)
        {
            var doc = new XmlDocument();
            doc.LoadXml(conteudoXML);

            ValidarSchema(doc, false);
        }

        private static void ValidarSchema(XmlDocument doc, bool sucessoEsperado)
        {
            var validar = new ValidarSchema();
            validar.Validar(doc, "CIOT.gerarIdOperacaoTransporte_v1.00.xsd", "http://www.antt.gov.br/ciot");

            Assert.Equal(sucessoEsperado, validar.Success);
        }

        private static void ValidarSucesso(RetGerarIdOperacaoTransporte retorno, string ciot = "560000088376")
        {
            Assert.Equal("1.00", retorno.Versao);
            Assert.True(retorno.Sucesso);
            Assert.Equal(ciot, retorno.IdOperacaoTransporte);
            Assert.Equal("110", retorno.Codigo);
            Assert.Equal("CIOT gerado com sucesso", retorno.Mensagem);
        }

        private sealed class TransporteHomologacaoFake(
            string resposta, string mediaType = "application/json", HttpStatusCode status = HttpStatusCode.OK) : IApiTransportExecutor
        {
            public TransportResponse Execute(TransportRequest request) => new()
            {
                StatusCode = status,
                HttpResponseMessage = new HttpResponseMessage(status)
                {
                    Content = new StringContent(resposta, Encoding.UTF8, mediaType)
                }
            };
        }

        private sealed class GerarIdOperacaoTransporteProducaoFake : CIOTGerarIdOperacaoTransporte
        {
            public GerarIdOperacaoTransporteProducaoFake(GerarIdOperacaoTransporte xml, Configuracao configuracao) : base(xml, configuracao) { }

            public string CpfCnpjRecebido { get; private set; }
            public string Resposta { get; set; } = SucessoANTT;
            public Exception FalhaComponente { get; set; }

            protected override string GerarCIOTProducao(string cpfCnpj)
            {
                CpfCnpjRecebido = cpfCnpj;
                return FalhaComponente == null ? Resposta : throw FalhaComponente;
            }
        }
    }
}
