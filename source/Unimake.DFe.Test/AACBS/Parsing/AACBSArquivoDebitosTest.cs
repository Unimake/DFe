using System;
using System.Net;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.AACBS;
using Unimake.DFe.Test.AACBS.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Parsing
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSArquivoDebitosTest
    {
        private const string JsonArquivoVazio = """
            {
              "tiqueteSolicitacao": "TÍQUETE teste/+=?%",
              "ni": "00000001",
              "niConsumidor": "00000001000001",
              "geradoEm": "2028-02-29T23:59:59.123456799-03:00",
              "apuracao": []
            }
            """;

        [Theory]
        [InlineData("55.0", true)]
        [InlineData("5.5e1", true)]
        [InlineData("55.000000000000000000000000000001", false)]
        [InlineData("5.500000000000000000000000000001e1", false)]
        public void DocumentoRespeitaInteiroMatematicoDoSchemaSemArredondamento(string numero, bool valido)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivo.Replace("\"documento\": 55,", "\"documento\": " + numero + ",", StringComparison.Ordinal));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            if (valido)
            {
                baixar.Executar();
                Assert.Equal(55, Assert.Single(baixar.Result.ArquivoDebitos.Apuracao).Debitos[0].Documento);
                ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            }
            else
            {
                Assert.Throws<FormatException>(baixar.Executar);
                Assert.Null(baixar.RetornoWSXML);
            }
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("\"documento\": 55,", "\"documento\": 99,", "$.apuracao[].debitos[].documento")]
        [InlineData("\"origem\": 0", "\"origem\": 999", "$.apuracao[].debitos[].origem")]
        [InlineData("02/2028", "13/2028", "$.apuracao[].pa")]
        [InlineData("00000001000001", "SENTINELA-IDENTIFICACAO-INVALIDA", "$.niConsumidor")]
        public void DiagnosticoInformaFamiliaECaminhoSemEcoarValores(string trecho, string substituicao, string caminho)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivo.Replace(trecho, substituicao, StringComparison.Ordinal));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            var erro = Assert.Throws<FormatException>(baixar.Executar);
            Assert.Contains("DEBITOS", erro.Message);
            Assert.Contains(caminho, erro.Message);
            Assert.DoesNotContain("SENTINELA-IDENTIFICACAO-INVALIDA", erro.ToString());
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            Assert.Null(baixar.RetornoWSXML);
        }

        [Theory]
        [InlineData("https://storage.example.test/f?a=%41%7e%2f%2B")]
        [InlineData("https://storage.example.test/a/../f?sig=TESTE")]
        public void UrlQueTransporteNormalizariaFalhaAntesDaRede(string url)
        {
            var pedido = CriarPedidoDownload();
            pedido.UrlAssinada = url;
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            Assert.NotNull(Record.Exception(() =>
            {
                using var baixar = new ServicosAACBS.BaixarArquivoRetorno(pedido, CriarConfiguracao());
                baixar.Executar();
            }));
            Assert.Empty(transporte.Requisicoes);
        }

        [Theory]
        [InlineData(Cnpj)]
        [InlineData("00000001000001")]
        [InlineData("00000001000002")]
        public void AceitaApuracaoVaziaComNiDaMesmaRaizEPreservaTextoNoXml(string ni)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivoVazioComNi("\"" + ni + "\""));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            baixar.Executar();

            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            Assert.Equal(Cnpj, baixar.Envio.Cnpj);
            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            Assert.Equal(ni, AACBSFixture.Procurar(baixar.RetornoWSXML, "retArquivoDebitos/ni").InnerText);
            var arquivo = baixar.Result.ArquivoDebitos;
            Assert.Equal(ni, arquivo.Ni);
            Assert.Equal("00000001000001", arquivo.NiConsumidor);
            Assert.Empty(arquivo.Apuracao);

            var xmlArquivo = arquivo.GerarXML();
            Assert.Equal(ni, AACBSFixture.Procurar(xmlArquivo, "ni").InnerText);
            var validacao = AACBSFixture.ValidarSchemaDe(xmlArquivo, "retArquivoDebitos");
            Assert.True(validacao.Success, validacao.ErrorMessage);
            Assert.Equal(ni, XMLUtility.Deserializar<RetArquivoDebitos>(xmlArquivo.OuterXml).Ni);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("\"00000002\"", Cnpj)]
        [InlineData("\"00000002000001\"", Cnpj)]
        [InlineData("\"000000010\"", Cnpj)]
        [InlineData("\"00000001000\"", Cnpj)]
        [InlineData("\"0000000100000\"", Cnpj)]
        [InlineData("\"000000010000001\"", Cnpj)]
        [InlineData("\"0000000100000\u0661\"", Cnpj)]
        [InlineData("\"0000000100000 \"", Cnpj)]
        [InlineData("10000000", "10000000")]
        [InlineData("10000000000001", "10000000")]
        [InlineData("null", Cnpj)]
        public void NiInvalidoIdentificaCampoSemProduzirRetorno(string niJson, string cnpj)
        {
            var pedido = CriarPedidoDownload();
            pedido.Cnpj = cnpj;
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivoVazioComNi(niJson));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(pedido, CriarConfiguracao());

            var erro = Assert.Throws<FormatException>(baixar.Executar);

            Assert.Contains("DEBITOS", erro.Message);
            Assert.Contains("$.ni.", erro.Message);
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            Assert.Null(baixar.RetornoWSXML);
            Assert.Null(baixar.Result.ArquivoDebitos);
            Assert.Equal("SEM_RETORNO", baixar.Result.ErroLocal.Codigo);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("\"ni\": \"00000001\"", "\"ni\": \"00000002\"")]
        [InlineData("TÍQUETE teste/+=?%", "OUTRO-TIQUETE")]
        [InlineData("\"debitos\":", "\"creditos\":")]
        [InlineData("\"saldoDevedor\": 0,", "\"saldoDevedor\": 0, \"saldoDevedor\": 0,")]
        [InlineData("\"saldoDevedor\": 0,", "\"saldoDevedor\": 0, \"campoFiscalNovo\": 0,")]
        [InlineData("\"excedente\": 0", "\"excedente\": null")]
        [InlineData("9999999999999999.99", "9999999999999999.991")]
        [InlineData("9999999999999999.99", "\"9999999999999999.99\"")]
        [InlineData("2028-02-29T23:59:59.123456799-03:00", "2028-02-29T23:59:59.123456799")]
        public void RejeitaCorrelacaoOuContratoInvalidoSemEmitirArquivo(string trecho, string substituicao)
        {
            Assert.Contains(trecho, JsonArquivo);
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonArquivo.Replace(trecho, substituicao, StringComparison.Ordinal));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            Assert.NotNull(Record.Exception(baixar.Executar));
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", baixar.Result.ErroLocal.Codigo);
            Assert.Null(baixar.Result.ArquivoDebitos);
            Assert.Null(baixar.RetornoWSXML);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("http://storage.example.test/arq", null)]
        [InlineData("https://conta:senha@storage.example.test/arq", null)]
        [InlineData("https://storage.example.test/arq#fragmento", null)]
        [InlineData(UrlAssinada, "2026-11-24T11:59:59Z")]
        public void UrlInvalidaOuVencidaFalhaAntesDeGet(string url, string expiraEm)
        {
            var pedido = CriarPedidoDownload();
            pedido.UrlAssinada = url;
            pedido.UrlAssinadaExpiraEmField = expiraEm;
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            Assert.NotNull(Record.Exception(() =>
            {
                using var baixar = new ServicosAACBS.BaixarArquivoRetorno(pedido, CriarConfiguracao())
                {
                    RelogioUtc = () => new DateTimeOffset(2026, 11, 24, 12, 0, 0, TimeSpan.Zero)
                };
                baixar.Executar();
            }));
            Assert.Empty(transporte.Requisicoes);
        }

        [Theory]
        [InlineData(HttpStatusCode.TemporaryRedirect)]
        [InlineData(HttpStatusCode.Forbidden)]
        [InlineData(HttpStatusCode.PartialContent)]
        public void RespostaNaoIntegralNaoProduzXmlNemDisparaNovoGet(HttpStatusCode status)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, JsonArquivo);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedidoDownload(), CriarConfiguracao());
            Assert.NotNull(Record.Exception(baixar.Executar));
            Assert.Equal(status, baixar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", baixar.Result.ErroLocal.Codigo);
            var requisicao = Assert.Single(transporte.Requisicoes);
            Assert.False(requisicao.Transporte.AllowAutoRedirect);
        }

        private static string JsonArquivoVazioComNi(string niJson) =>
            JsonArquivoVazio.Replace("\"ni\": \"00000001\"", "\"ni\": " + niJson, StringComparison.Ordinal);
    }
}
