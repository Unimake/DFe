using System;
using System.Linq;
using System.Net;
using System.Text;
using System.Xml.Linq;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml;
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
    public class AACBSArquivosConsultasTest
    {
        private readonly ITestOutputHelper saida;

        public AACBSArquivosConsultasTest(ITestOutputHelper saida) => this.saida = saida;

        internal const string TiqueteArquivo = "TÍQUETE-SINTÉTICO-01";
        private const string CreditoCompleto = """
            {
              "origem": 0, "documento": 55, "chave": "CHAVE-SINTETICA-2",
              "emissao": "2028-02-29T12:00:00Z", "registro": "2028-02-29T12:00:00.0100+00:00",
              "atualizacao": "2028-02-29T23:59:59.123456799-03:00",
              "cbs": {
                "apropriacao": {
                  "utilizacao": {
                    "naoUtilizado": {"pedidoRessarcimento": 0, "saldoCredor": 0},
                    "restabelecido": 8.07, "utilizado": 7.06, "inutilizavel": 6.05
                  },
                  "apropriado": 5.04, "aApropriar": 4.03, "prescrito": 3.02, "suspenso": 2.01,
                  "inapropriavel": 0
                },
                "apurado": 99.99, "excedente": 0
              }
            }
            """;

        private static readonly string Creditos = $$"""
            [
              {"creditos": [
                {{CreditoCompleto}},
                {
                  "origem": 31, "documento": 62, "chave": "CHAVE-SINTETICA-2",
                  "emissao": "2028-02-29T12:00:00Z", "registro": "2028-02-29T12:00:00.0100+00:00",
                  "atualizacao": "2028-02-29T23:59:59.123456799-03:00",
                  "cbs": {"apurado": 0, "apropriacao": {"utilizacao": {"naoUtilizado": {"saldoCredor": 0} } } }
                }
              ], "pa": "02/2028"},
              {"creditos": [{{CreditoCompleto}}], "pa": "03/2028"}
            ]
            """;

        private const string PagamentoPcont = """
            {
              "composicao": [
                {"sequencial": 1, "pa": "02/2028", "vencimento": "2028-03-20",
                 "principal": 10.34, "multa": 0, "juros": 0.01, "total": 7.25},
                {"sequencial": 2, "pa": "02/2028", "vencimento": "2028-03-20",
                 "principal": 0, "multa": 0, "juros": 0.01, "total": 7.25}
              ],
              "niContribuinte": "00000001000001", "tipo": 2, "numeroDARF": "00000000000000002"
            }
            """;

        private static readonly string Pagamentos = $$"""
            [
              {"pagamentos": [
                {
                  "composicao": [
                    {"sequencial": 1, "pa": "02/2028", "vencimento": "2028-03-20",
                     "niContribuinte": "00000001000001", "chaveDFE": "CHAVE-SINTETICA-3",
                     "principal": 9.99, "multa": 0, "juros": 0.01, "total": 7.25},
                    {"sequencial": 99999999, "pa": "02/2028", "vencimento": "2028-03-20T00:00:00.0100-03:00",
                     "principal": 0, "multa": 0, "juros": 0.01, "total": 7.25}
                  ],
                  "niAdquirente": "00000001000001", "tipo": 1, "numeroDARF": "00000000000000001"
                },
                {{PagamentoPcont}}
              ], "dataArrecadacao": "2028-02-29"},
              {"pagamentos": [{{PagamentoPcont}}], "dataArrecadacao": "2028-03-01"}
            ]
            """;

        private static string Recolhimento(string numeroDarf) => $$"""
            {
              "composicao": [
                {"sequencial": 1, "pa": "02/2028", "vencimento": "2028-03-20",
                 "niFornecedor": "00000001000001", "chaveDFE": "CHAVE-SINTETICA-4",
                 "principal": 9.99, "multa": 0, "juros": 0.01, "total": 7.25},
                {"sequencial": 2, "pa": "02/2028", "vencimento": "2028-03-20",
                 "niFornecedor": "00000001000001", "principal": 0, "multa": 0, "juros": 0.01, "total": 7.25}
              ],
              "tipo": 1, "numeroDARF": "{{numeroDarf}}"
            }
            """;

        private static readonly string Recolhimentos = $$"""
            [
              {"pagamentos": [{{Recolhimento("00000000000000001")}}, {{Recolhimento("00000000000000002")}}],
               "dataArrecadacao": "2028-02-29"},
              {"pagamentos": [{{Recolhimento("00000000000000003")}}], "dataArrecadacao": "2028-03-01"}
            ]
            """;

        internal static string ArquivoJson(TipoConsultaAACBS familia, bool vazio = false)
        {
            var apuracao = vazio ? "[]" : familia switch
            {
                TipoConsultaAACBS.Creditos => Creditos,
                TipoConsultaAACBS.Pagamentos => Pagamentos,
                TipoConsultaAACBS.Recolhimentos => Recolhimentos,
                _ => throw new ArgumentOutOfRangeException(nameof(familia))
            };
            var consumidor = vazio || familia != TipoConsultaAACBS.Creditos ? "00000001000001" : "00000000001";
            return $$"""
                {
                  "apuracao": {{apuracao}},
                  "geradoEm": "{{Instante}}",
                  "niConsumidor": "{{consumidor}}",
                  "ni": "{{Cnpj}}",
                  "tiqueteSolicitacao": "{{TiqueteArquivo}}"
                }
                """;
        }

        internal static void ConferirArquivo(ServicosAACBS.BaixarArquivoRetorno baixar, TipoConsultaAACBS familia)
        {
            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var raiz = "retArquivo" + familia;
            var objeto = SelecionarArquivo(baixar.Result, familia);
            var esperado = AACBSFixture.Carregar(raiz);
            var arquivo = objeto.GerarXML();
            var validacao = AACBSFixture.ValidarSchemaDe(arquivo, raiz);
            Assert.True(validacao.Success, validacao.ErrorMessage);
            CompararXml(esperado.DocumentElement.OuterXml, arquivo.DocumentElement.OuterXml);
            var filho = AACBSFixture.Procurar(baixar.RetornoWSXML, raiz);
            CompararXml(esperado.DocumentElement.OuterXml, filho.OuterXml);
            var roundTrip = AACBSFixture.Gerar(arquivo, objeto.GetType());
            CompararXml(arquivo.DocumentElement.OuterXml, roundTrip.DocumentElement.OuterXml);
#if INTEROP
            if (objeto is RetArquivoCreditos creditos)
            {
                Assert.Equal(2, creditos.GetApuracaoCount());
                var periodo = creditos.GetApuracao(0);
                Assert.Equal(2, periodo.GetCreditosCount());
                Assert.Equal("0", periodo.GetCreditos(0).Cbs.ExcedenteField);
                Assert.Null(periodo.GetCreditos(1).Cbs.ExcedenteField);
            }
#endif
        }

        internal static void CompararXml(string esperado, string atual)
        {
            static object[] Estrutura(string xml) => XElement.Parse(xml).DescendantsAndSelf()
                .Select(elemento => (object)(elemento.Name, (string)elemento.Attribute("versao"),
                    elemento.Elements().Count(), elemento.HasElements ? null : elemento.Value))
                .ToArray();

            Assert.Equal(Estrutura(esperado), Estrutura(atual));
        }

        private static XMLBase SelecionarArquivo(RetBaixarArquivoRetorno result, TipoConsultaAACBS familia)
        {
            Assert.Null(result.ErroLocal);
            var itens = new XMLBase[] { result.ArquivoDebitos, result.ArquivoCreditos, result.ArquivoPagamentos, result.ArquivoRecolhimentos };
            XMLBase selecionado = familia switch
            {
                TipoConsultaAACBS.Creditos => Assert.IsType<RetArquivoCreditos>(result.ArquivoCreditos),
                TipoConsultaAACBS.Pagamentos => Assert.IsType<RetArquivoPagamentos>(result.ArquivoPagamentos),
                TipoConsultaAACBS.Recolhimentos => Assert.IsType<RetArquivoRecolhimentos>(result.ArquivoRecolhimentos),
                _ => throw new ArgumentOutOfRangeException(nameof(familia))
            };
            Assert.Same(selecionado, Assert.Single(itens.OfType<XMLBase>()));
            return selecionado;
        }

        private static BaixarArquivoRetorno CriarPedido(TipoConsultaAACBS familia)
        {
            var pedido = CriarPedidoDownload();
            pedido.TipoConsulta = familia;
            pedido.TiqueteSolicitacao = TiqueteArquivo;
            return pedido;
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos)]
        [InlineData(TipoConsultaAACBS.Pagamentos)]
        [InlineData(TipoConsultaAACBS.Recolhimentos)]
        public void ApuracaoVaziaUsaFamiliaDoPedidoEDistingueAusenciaDeLista(TipoConsultaAACBS familia)
        {
            var transporte = new AACBSTransporteControlado();
            var json = ArquivoJson(familia, vazio: true);
            transporte.Enfileirar(HttpStatusCode.OK, json);
            transporte.Enfileirar(HttpStatusCode.OK, json.Replace("\"apuracao\": [],", "", StringComparison.Ordinal));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedido(familia), CriarConfiguracao());

            baixar.Executar();

            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var raiz = "retArquivo" + familia;
            var arquivo = SelecionarArquivo(baixar.Result, familia).GerarXML();
            Assert.Empty(AACBSFixture.Procurar(arquivo, "apuracao").ChildNodes);
            Assert.True(AACBSFixture.ValidarSchemaDe(arquivo, raiz).Success);
            Assert.Single(transporte.Requisicoes);

            Assert.Throws<FormatException>(baixar.Executar);
            Assert.Equal("SEM_RETORNO", baixar.Result.ErroLocal.Codigo);
            Assert.Null(baixar.RetornoWSXML);
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        [Theory]
        [InlineData(Cnpj, false)]
        [InlineData(Cnpj, true)]
        [InlineData("00000001000002", false)]
        [InlineData("00000001000002", true)]
        public void CreditosPreservamNiDaMesmaRaizComApuracaoVaziaOuPreenchida(string ni, bool vazio)
        {
            const TipoConsultaAACBS familia = TipoConsultaAACBS.Creditos;
            const string raiz = "retArquivoCreditos";
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, ArquivoJsonComNi(familia, $"\"{ni}\"", vazio));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedido(familia), CriarConfiguracao());

            baixar.Executar();

            Assert.Equal(Cnpj, baixar.Envio.Cnpj);
            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var arquivo = Assert.IsType<RetArquivoCreditos>(SelecionarArquivo(baixar.Result, familia));
            Assert.Equal(ni, arquivo.Ni);
            Assert.Equal(vazio ? 0 : 2, arquivo.Apuracao.Count);
            Assert.Equal(vazio ? 0 : 3, arquivo.Apuracao.Sum(periodo => periodo.Creditos.Count));

            var xml = arquivo.GerarXML();
            var validacao = AACBSFixture.ValidarSchemaDe(xml, raiz);
            Assert.True(validacao.Success, validacao.ErrorMessage);
            Assert.Equal(ni, AACBSFixture.Procurar(xml, "ni").InnerText);
            Assert.Equal(ni, AACBSFixture.Procurar(baixar.RetornoWSXML, raiz + "/ni").InnerText);
            var roundTrip = AACBSFixture.Gerar(xml, typeof(RetArquivoCreditos));
            CompararXml(xml.DocumentElement.OuterXml, roundTrip.DocumentElement.OuterXml);
            Assert.Single(transporte.Requisicoes);
        }

        [Theory]
        [InlineData("\"00000002\"")]
        [InlineData("\"00000002000001\"")]
        [InlineData("\"000000010\"")]
        [InlineData("\"0000000100\"")]
        [InlineData("\"00000001000\"")]
        [InlineData("\"000000010000\"")]
        [InlineData("\"0000000100000\"")]
        [InlineData("\"000000010000001\"")]
        [InlineData("\"0000000100000\u0661\"")]
        [InlineData("\"00000001+00001\"")]
        [InlineData("\"00000001-00001\"")]
        [InlineData("\"0000000100000A\"")]
        [InlineData("\"00000001 \"")]
        [InlineData("10000000000001")]
        [InlineData("null")]
        public void CreditosRejeitamNiInvalidoOuComRaizDivergente(string niJson)
        {
            const TipoConsultaAACBS familia = TipoConsultaAACBS.Creditos;
            RejeitarArquivo(familia, ArquivoJsonComNi(familia, niJson, vazio: true), "$.ni");
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Pagamentos)]
        [InlineData(TipoConsultaAACBS.Recolhimentos)]
        public void PagamentosERecolhimentosRejeitamNiDeQuatorzeDigitos(TipoConsultaAACBS familia)
        {
            RejeitarArquivo(familia, ArquivoJsonComNi(familia, "\"00000001000001\""), "$.ni");
        }

        private static string ArquivoJsonComNi(TipoConsultaAACBS familia, string niJson, bool vazio = false) =>
            ArquivoJson(familia, vazio).Replace($"\"ni\": \"{Cnpj}\"", "\"ni\": " + niJson, StringComparison.Ordinal);

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, TipoConsultaAACBS.Debitos)]
        [InlineData(TipoConsultaAACBS.Pagamentos, TipoConsultaAACBS.Recolhimentos)]
        [InlineData(TipoConsultaAACBS.Recolhimentos, TipoConsultaAACBS.Pagamentos)]
        public void ArquivoPreenchidoDeOutraFamiliaERecusado(TipoConsultaAACBS esperada, TipoConsultaAACBS recebida) =>
            RejeitarArquivo(esperada, recebida == TipoConsultaAACBS.Debitos
                ? JsonArquivo.Replace(Tiquete, TiqueteArquivo, StringComparison.Ordinal)
                : ArquivoJson(recebida), "$.apuracao");

        [Theory]
        [InlineData(TipoConsultaAACBS.Creditos, "\"saldoCredor\": 0", "\"saldoCredor\": null", ".saldoCredor")]
        [InlineData(TipoConsultaAACBS.Creditos, "\"excedente\": 0", "\"excedentes\": 0", ".cbs")]
        [InlineData(TipoConsultaAACBS.Creditos, "\"excedente\": 0", "\"excedente\": 0, \"excedente\": 1", ".excedente")]
        [InlineData(TipoConsultaAACBS.Creditos, "\"origem\": 31", "\"origem\": 1", ".origem")]
        [InlineData(TipoConsultaAACBS.Creditos, "\"documento\": 62", "\"documento\": 62.000000000000000000000000000001", ".documento")]
        [InlineData(TipoConsultaAACBS.Pagamentos, "\"sequencial\": 99999999", "\"sequencial\": 100000000", ".sequencial")]
        [InlineData(TipoConsultaAACBS.Pagamentos, "\"niAdquirente\": \"00000001000001\"", "\"ni_contribuinte\": \"00000001000001\"", ".pagamentos[]")]
        [InlineData(TipoConsultaAACBS.Pagamentos, "\"vencimento\": \"2028-03-20\"", "\"vencimento\": \"2028-03-20T00:00:00-03:00\"", ".vencimento")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "\"niFornecedor\": \"00000001000001\",", "", ".niFornecedor")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "\"tipo\": 1", "\"tipo\": 2", ".tipo")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "\"total\": 7.25", "\"total\": 7.251", ".total")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "\"total\": 7.25", "\"total\": 10000000000000000.00", ".total")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, "\"total\": 7.25", "\"total\": \"7.25\"", ".total")]
        [InlineData(TipoConsultaAACBS.Recolhimentos, TiqueteArquivo, "SENTINELA-VALOR-REMOTO", ".tiqueteSolicitacao")]
        public void ValidaEstruturaCodigoDataEPrecisaoPorFamilia(
            TipoConsultaAACBS familia, string original, string substituicao, string caminho)
        {
            var json = ArquivoJson(familia);
            Assert.Contains(original, json);
            RejeitarArquivo(familia, json.Replace(original, substituicao, StringComparison.Ordinal), caminho);
        }

        [Theory]
        [InlineData(TipoConsultaAACBS.Pagamentos)]
        [InlineData(TipoConsultaAACBS.Recolhimentos)]
        public void PreservaExtremosMonetariosEAceitaExpoenteExato(TipoConsultaAACBS familia)
        {
            var json = ArquivoJson(familia).Replace("\"principal\": 9.99", "\"principal\": -9999999999999999.99", StringComparison.Ordinal)
                .Replace("\"total\": 7.25", "\"total\": 999999999999999999e-2", StringComparison.Ordinal);
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, json);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedido(familia), CriarConfiguracao());

            baixar.Executar();

            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var composicao = "retArquivo" + familia + "/apuracao/item[1]/pagamentos/item[1]/composicao/item[1]";
            Assert.Equal("-9999999999999999.99", AACBSFixture.Procurar(baixar.RetornoWSXML, composicao + "/principal").InnerText);
            Assert.Equal("9999999999999999.99", AACBSFixture.Procurar(baixar.RetornoWSXML, composicao + "/total").InnerText);
            Assert.Single(transporte.Requisicoes);
        }

        [Fact]
        public void MilCreditosMantemContagemEContratoNoRoundTrip()
        {
            const int quantidade = 1000;
            var itens = string.Join(",", Enumerable.Repeat(CreditoCompleto, quantidade));
            var apuracao = $$"""[{"pa":"02/2028","creditos":[{{itens}}]}]""";
            var json = ArquivoJson(TipoConsultaAACBS.Creditos, vazio: true)
                .Replace("\"apuracao\": []", "\"apuracao\": " + apuracao, StringComparison.Ordinal);
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, json);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedido(TipoConsultaAACBS.Creditos), CriarConfiguracao());

            baixar.Executar();

            ValidarRetorno(baixar, "retBaixarArquivoRetorno");
            var result = baixar.Result.ArquivoCreditos;
            var creditos = Assert.Single(result.Apuracao).Creditos;
            Assert.Equal(quantidade, creditos.Count);
            Assert.All(creditos, credito => Assert.Equal("CHAVE-SINTETICA-2", credito.Chave));
            var xml = result.GerarXML();
            var validacao = AACBSFixture.ValidarSchemaDe(xml, "retArquivoCreditos");
            Assert.True(validacao.Success, validacao.ErrorMessage);
            var roundTrip = AACBSFixture.Gerar(xml, typeof(RetArquivoCreditos));
            CompararXml(xml.DocumentElement.OuterXml, roundTrip.DocumentElement.OuterXml);
            Assert.Single(transporte.Requisicoes);
            saida.WriteLine($"Créditos: {quantidade}. JSON UTF-8: {Encoding.UTF8.GetByteCount(json)} bytes. " +
                $"Envelope XML UTF-8: {Encoding.UTF8.GetByteCount(baixar.RetornoWSString)} bytes.");
        }

        private static void RejeitarArquivo(TipoConsultaAACBS familia, string json, string caminho)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, json);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var baixar = new ServicosAACBS.BaixarArquivoRetorno(CriarPedido(familia), CriarConfiguracao());

            var erro = Assert.Throws<FormatException>(baixar.Executar);

            Assert.Contains(familia.ToString().ToUpperInvariant(), erro.Message);
            Assert.Contains(caminho, erro.Message);
            Assert.DoesNotContain("SENTINELA-VALOR-REMOTO", erro.ToString());
            Assert.DoesNotContain("excedentes", erro.Message);
            Assert.DoesNotContain("ni_contribuinte", erro.Message);
            Assert.Equal(HttpStatusCode.OK, baixar.HttpStatusCode);
            Assert.Equal("SEM_RETORNO", baixar.Result.ErroLocal.Codigo);
            Assert.Null(baixar.RetornoWSXML);
            Assert.Single(transporte.Requisicoes);
        }
    }
}
