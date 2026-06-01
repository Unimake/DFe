using Newtonsoft.Json.Linq;
using System;
using System.IO;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;
using Xunit;
using CIOTGerarIdOperacaoTransporte = Unimake.Business.DFe.Servicos.CIOT.GerarIdOperacaoTransporte;

namespace Unimake.DFe.Test.CIOT
{
    /// <summary>
    /// Testar o serviço auxiliar de geração do identificador da operação de transporte.
    /// </summary>
    public class GerarIdOperacaoTransporteTest
    {
        private const string ArquivoEnvio = @"..\..\..\CIOT\Resources\gerarIdOperacaoTransporte.xml";
        private const string ArquivoRetorno = @"..\..\..\CIOT\Resources\retGerarIdOperacaoTransporte.xml";

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
                IdOperacaoTransporte = "560000088376",
                Codigo = "110",
                Mensagem = "IdOperacaoTransporte gerado com sucesso."
            }.GerarXML();

            Assert.Equal("RetGerarIdOperacaoTransporte", xml.DocumentElement.Name);
            Assert.Equal("1.00", xml.DocumentElement.GetAttribute("versao"));
            Assert.Equal("560000088376", xml.DocumentElement["IdOperacaoTransporte", "http://www.antt.gov.br/ciot"].InnerText);
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
            Assert.Equal("560000088376", xml.IdOperacaoTransporte);
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
            ValidarSchemaInvalido("<RetGerarIdOperacaoTransporte xmlns=\"http://www.antt.gov.br/ciot\" versao=\"1.00\"><IdOperacaoTransporte>123</IdOperacaoTransporte><Codigo>110</Codigo><Mensagem>Erro</Mensagem></RetGerarIdOperacaoTransporte>");
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
            Assert.Equal("41942626000102", json["CpfCnpj"]);
            Assert.Equal("https://appservices-hml.antt.gov.br/pefServices/api/gerar", servico.Configuracoes.RequestURI);
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
        /// Processar retorno JSON com o campo IdOperacaoTransporte.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void ProcessarJsonIdOperacaoTransporte()
        {
            ValidarSucesso(new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT(@"{""IdOperacaoTransporte"":""560000088376""}"));
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

        /// <summary>
        /// Rejeitar retorno JSON sem conteúdo útil.
        /// </summary>
        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitarJsonNulo()
        {
            Assert.Throws<InvalidOperationException>(() => new CIOTGerarIdOperacaoTransporte().ProcessarRetornoANTT("null"));
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

        private static void ValidarSucesso(RetGerarIdOperacaoTransporte retorno)
        {
            Assert.Equal("1.00", retorno.Versao);
            Assert.Equal("560000088376", retorno.IdOperacaoTransporte);
            Assert.Equal("110", retorno.Codigo);
            Assert.Equal("IdOperacaoTransporte gerado com sucesso.", retorno.Mensagem);
        }
    }
}
