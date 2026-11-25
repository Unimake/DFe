using System;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Text.Json;
using Unimake.Business.DFe.ConsumirServico.Transport;
using Unimake.Business.DFe.Servicos;
using Unimake.DFe.Test.AACBS.Validacao;
using Unimake.Exceptions;
using Xunit;
using ServicosAACBS = Unimake.Business.DFe.Servicos.AACBS;
using XmlAACBS = Unimake.Business.DFe.Xml.AACBS;
using static Unimake.DFe.Test.AACBS.Parsing.AACBSArquivosConsultasTest;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoConsultasTest;
using static Unimake.DFe.Test.AACBS.Servicos.AACBSFluxoDebitosTest;

namespace Unimake.DFe.Test.AACBS.Servicos
{
    [Collection(AACBSExecucaoCollection.Nome)]
    [Trait("DFe", "AACBS")]
    public class AACBSDarfTest
    {
        private const string CnpjCompleto = "00000001000001";
        private const string Total = "PAGAMENTO_TOTAL";
        private const string Parcial = "PAGAMENTO_PARCIAL";
        private const string Data = "2028-02-29";
        private const string DataMaxima = "dataMaximaPagamento";
        private const string Vencimento = "dataVencimento";
        private const string JsonDadosDarf = """
            {"linkDarfPDF":"https://example.com/a%2Fb?teste=a%2Bb&v=2","chavePIX":"PIX-SINTETICO","codigoBarras":"00000000000000000001"}
            """;
        private const string ValoresRad = """ "valorDarfPrincipal":9999999999999999.99,"valorDarfTotal":0 """;
        private const string ValoresPcont = """ "valorDarfTotal":45.67,"valorDarfMulta":0,"valorDarfJuros":0.01,"valorDarfPrincipal":12.34 """;

        [Theory]
        [InlineData(DataMaxima, false)]
        [InlineData(Vencimento, true)]
        public void RadEnviaArrayMantemLoteERecebeUmUnicoDarf(string nomeData, bool lote)
        {
            var chaveLimite = new string('0', 100);
            var entrada = new XmlAACBS.GerarDarfAdquirente
            {
                Cnpj = CnpjCompleto,
                Solicitacoes = new() { new() { ChaveDFE = chaveLimite, OpcaoPagamento = Parcial, ValorDarfParcial = 0m } }
            };
            if (lote)
            {
                entrada.Solicitacoes.Add(new() { ChaveDFE = chaveLimite, OpcaoPagamento = Total, ValorDarfParcial = 12.34m });
                entrada.Solicitacoes.Add(new() { ChaveDFE = "0", OpcaoPagamento = Parcial, ValorDarfParcial = 9999999999999999.99m });
                entrada.Solicitacoes.Add(new() { ChaveDFE = "0", OpcaoPagamento = Total });
            }
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, JsonToken);
            transporte.Enfileirar(HttpStatusCode.OK, Sucesso(adquirente: true, nomeData));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = new ServicosAACBS.GerarDarfAdquirente(entrada, CriarConfiguracao());

            ExecutarEmPtBr(servico);

            Assert.Equal(HttpStatusCode.OK, servico.HttpStatusCode);
            var resultado = servico.Result;
            Assert.Equal("DARF_GERADO", resultado.CdRetorno);
            Assert.Equal(9999999999999999.99m, resultado.InformacoesPagamento.ValorDarfPrincipal);
            Assert.Equal(0m, resultado.InformacoesPagamento.ValorDarfTotal);
            Assert.Equal(nomeData == DataMaxima ? Data : null, resultado.InformacoesPagamento.DataMaximaPagamento);
            Assert.Equal(nomeData == Vencimento ? Data : null, resultado.InformacoesPagamento.DataVencimento);
            ConferirSucesso(servico, resultado, "retGerarDarfAdquirente", DataMaxima, nomeData);

            Assert.Equal(2, transporte.Requisicoes.Count);
            Assert.Equal(EndpointToken, transporte.Requisicoes[0].Transporte.RequestUri);
            var post = transporte.Requisicoes[1];
            ConferirPostFiscal(post, "rad", CnpjCompleto);
            using var json = JsonDocument.Parse(post.Corpo);
            var itens = json.RootElement;
            Assert.Equal(JsonValueKind.Array, itens.ValueKind);
            Assert.Equal(lote ? 4 : 1, itens.GetArrayLength());
            ConferirItemRad(itens[0], chaveLimite, Parcial, 0m);
            if (lote)
            {
                ConferirItemRad(itens[1], chaveLimite, Total, null);
                ConferirItemRad(itens[2], "0", Parcial, 9999999999999999.99m);
                ConferirItemRad(itens[3], "0", Total, null);
            }
        }

        [Theory]
        [InlineData(Vencimento, Parcial, "0", "0")]
        [InlineData(DataMaxima, Parcial, "9999999999999999.99", "9999999999999999.99")]
        [InlineData(Vencimento, Total, null, null)]
        [InlineData(DataMaxima, Total, "12.34", null)]
        public void PcontEnviaObjetoComValorCondicionalEPreservaDadosRemotos(
            string nomeData, string opcao, string valorXml, string valorJson)
        {
            var entrada = new XmlAACBS.GerarDarfContribuinte
            {
                Cnpj = CnpjCompleto,
                Pa = "02/2028",
                OpcaoPagamento = opcao,
                ValorDarfParcial = valorXml == null ? null : decimal.Parse(valorXml, CultureInfo.InvariantCulture)
            };
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, Sucesso(adquirente: false, nomeData));
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = new ServicosAACBS.GerarDarfContribuinte(entrada.GerarXML().OuterXml, ConfiguracaoComToken());

            ExecutarEmPtBr(servico);

            Assert.Equal(HttpStatusCode.OK, servico.HttpStatusCode);
            var resultado = servico.Result;
            Assert.Equal("DARF_GERADO", resultado.CdRetorno);
            Assert.Equal(12.34m, resultado.InformacoesPagamento.ValorDarfPrincipal);
            Assert.Equal(0m, resultado.InformacoesPagamento.ValorDarfMulta);
            Assert.Equal(0.01m, resultado.InformacoesPagamento.ValorDarfJuros);
            Assert.Equal(45.67m, resultado.InformacoesPagamento.ValorDarfTotal);
            Assert.Equal(nomeData == DataMaxima ? Data : null, resultado.InformacoesPagamento.DataMaximaPagamento);
            Assert.Equal(nomeData == Vencimento ? Data : null, resultado.InformacoesPagamento.DataVencimento);
            ConferirSucesso(servico, resultado, "retGerarDarfContribuinte", Vencimento, nomeData);

            var post = Assert.Single(transporte.Requisicoes);
            ConferirPostFiscal(post, "pcont", CnpjCompleto);
            using var json = JsonDocument.Parse(post.Corpo);
            var objeto = json.RootElement;
            Assert.Equal(JsonValueKind.Object, objeto.ValueKind);
            Assert.Equal("02/2028", objeto.GetProperty("pa").GetString());
            Assert.Equal(opcao, objeto.GetProperty("opcaoPagamento").GetString());
            var esperado = valorJson == null ? (decimal?)null : decimal.Parse(valorJson, CultureInfo.InvariantCulture);
            ConferirValorParcial(objeto, esperado);
        }

        public static TheoryData<bool, string, HttpStatusCode> ErrosPublicados => new()
        {
            { true, "DFE_NAO_ENCONTRADO", HttpStatusCode.NotFound },
            { true, "VALOR_PARCIAL_SUPERIOR_DFE", HttpStatusCode.BadRequest },
            { true, "CNPJ_NAO_ADQUIRENTE", HttpStatusCode.Unauthorized },
            { true, "DADOS_DE_ENTRADA_INVALIDOS", HttpStatusCode.InternalServerError },
            { false, "DFE_NAO_ENCONTRADO", HttpStatusCode.NotFound },
            { false, "VALOR_PARCIAL_SUPERIOR_DFE", HttpStatusCode.BadRequest },
            { false, "CNPJ_NAO_ADQUIRENTE", HttpStatusCode.Unauthorized },
            { false, "DADOS_DE_ENTRADA_INVALIDOS", HttpStatusCode.InternalServerError },
            { true, "DADOS_DE_ENTRADA_INVALIDOS", HttpStatusCode.OK },
            { false, "DADOS_DE_ENTRADA_INVALIDOS", HttpStatusCode.OK }
        };

        [Theory]
        [MemberData(nameof(ErrosPublicados))]
        public void ErroAplicacaoPreservaCodigoMensagemListaEContexto(bool adquirente, string codigo, HttpStatusCode status)
        {
            const string mensagem = "Erro sintético preservado";
            var contemDados = codigo != "DFE_NAO_ENCONTRADO";
            var dados = contemDados ? new[] { "opcaoPagamento", "valorDarfParcial" } : null;
            var retorno = contemDados
                ? JsonSerializer.Serialize(new { cdRetorno = codigo, mensagem, dadosInvalidos = dados })
                : JsonSerializer.Serialize(new { cdRetorno = codigo, mensagem });
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, retorno);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = CriarDarf(adquirente, ConfiguracaoComToken());

            servico.Executar();

            Assert.Equal(status, servico.HttpStatusCode);
            ValidarRetorno(servico, "ret" + Operacao(adquirente));
            switch (Resultado(servico))
            {
                case XmlAACBS.RetGerarDarfAdquirente rad:
                    Assert.Equal(codigo, rad.CdRetorno);
                    Assert.Equal(mensagem, rad.Mensagem);
                    Assert.Equal(dados, rad.DadosInvalidos);
                    Assert.Null(rad.Darf);
                    Assert.Null(rad.InformacoesPagamento);
#if INTEROP
                    Assert.Equal(dados?.Length ?? 0, rad.GetDadosInvalidosCount());
#endif
                    break;
                case XmlAACBS.RetGerarDarfContribuinte pcont:
                    Assert.Equal(codigo, pcont.CdRetorno);
                    Assert.Equal(mensagem, pcont.Mensagem);
                    Assert.Equal(dados, pcont.DadosInvalidos);
                    Assert.Null(pcont.Darf);
                    Assert.Null(pcont.InformacoesPagamento);
#if INTEROP
                    Assert.Equal(dados?.Length ?? 0, pcont.GetDadosInvalidosCount());
#endif
                    break;
            }
            Assert.Null(Resultado(servico).ErroLocal);
            ConferirPostFiscal(Assert.Single(transporte.Requisicoes), adquirente ? "rad" : "pcont", CnpjCompleto);
        }

        public static TheoryData<bool, string> RespostasIncompativeis => new()
        {
            { true, Sucesso(adquirente: false, Vencimento) },
            { false, Sucesso(adquirente: true, Vencimento) },
            { true, Sucesso(adquirente: true, Vencimento).Replace(ValoresRad, "\"dataMaximaPagamento\":\"2028-02-29\"," + ValoresRad, StringComparison.Ordinal) },
            { false, Sucesso(adquirente: false, Vencimento).Replace("\"dataVencimento\":\"2028-02-29\",", "", StringComparison.Ordinal) },
            { true, Sucesso(adquirente: true, Vencimento).Replace(Data, "2028-02-30", StringComparison.Ordinal) },
            { false, Sucesso(adquirente: false, Vencimento).Replace(Data, "2028-02-29T00:00:00Z", StringComparison.Ordinal) },
            { false, Sucesso(adquirente: false, Vencimento).Replace("45.67", "45.671", StringComparison.Ordinal) },
            { true, Sucesso(adquirente: true, Vencimento).Replace("9999999999999999.99", "9999999999999999.99000000000000000001", StringComparison.Ordinal) },
            { false, Sucesso(adquirente: false, Vencimento).Replace("\"valorDarfMulta\":0", "\"valorDarfMulta\":0,\"valorDarfMulta\":0", StringComparison.Ordinal) },
            { true, Sucesso(adquirente: true, Vencimento).Replace("\"chavePIX\"", "\"campoNovoDarF\"", StringComparison.Ordinal) },
            { true, "[" + Sucesso(adquirente: true, Vencimento) + "]" },
            { false, """{"cdRetorno":"CODIGO_NOVO","mensagem":"SENTINELA-CORPO-REMOTO"}""" },
            { false, """{"cdRetorno":"DFE_NAO_ENCONTRADO","mensagem":""}""" },
            { true, """{"cdRetorno":"DADOS_DE_ENTRADA_INVALIDOS","mensagem":"Teste"}""" },
            { false, """{"cdRetorno":"DADOS_DE_ENTRADA_INVALIDOS","mensagem":"Teste","dadosInvalidos":[]}""" },
            { true, """{"cdRetorno":"DADOS_DE_ENTRADA_INVALIDOS","mensagem":"Teste","dadosInvalidos":null}""" },
            { false, """{"cdRetorno":"DADOS_DE_ENTRADA_INVALIDOS","mensagem":"Teste","dadosInvalidos":["pa",null]}""" }
        };

        [Theory]
        [MemberData(nameof(RespostasIncompativeis))]
        public void RespostaIncompativelFalhaComStatusESemResultadoParcial(bool adquirente, string retorno) =>
            RejeitarResposta(adquirente, HttpStatusCode.OK, retorno, "application/json");

        [Theory]
        [InlineData(true, HttpStatusCode.Unauthorized, "", "text/plain")]
        [InlineData(true, HttpStatusCode.Forbidden, "SENTINELA-CORPO-REMOTO", "text/plain")]
        [InlineData(false, HttpStatusCode.NotFound, "", "text/plain")]
        [InlineData(false, HttpStatusCode.TooManyRequests, "<p>SENTINELA-CORPO-REMOTO</p>", "text/html")]
        [InlineData(true, HttpStatusCode.InternalServerError, "<p>SENTINELA-CORPO-REMOTO</p>", "text/html")]
        public void HttpComCorpoNaoFiscalFalhaSemRepetirPost(bool adquirente, HttpStatusCode status, string corpo, string tipoMidia) =>
            RejeitarResposta(adquirente, status, corpo, tipoMidia);

        [Theory]
        [InlineData(true, "<cnpj>00000001000001</cnpj>", "<cnpj>00000001</cnpj>")]
        [InlineData(false, "<pa>02/2028</pa>", "<pa>13/2028</pa>")]
        [InlineData(true, "<valorDarfParcial>0</valorDarfParcial>", "")]
        [InlineData(false, "<valorDarfParcial>0</valorDarfParcial>", "")]
        [InlineData(true, "<valorDarfParcial>0</valorDarfParcial>", "<valorDarfParcial>0.001</valorDarfParcial>")]
        [InlineData(false, "<valorDarfParcial>0</valorDarfParcial>", "<valorDarfParcial>10000000000000000.00</valorDarfParcial>")]
        public void DarfComXmlInvalidoFalhaAntesDeAutenticarOuEmitir(bool adquirente, string trecho, string substituicao)
        {
            var xml = AACBSFixture.Carregar(RaizEnvio(adquirente)).DocumentElement.OuterXml;
            Assert.Contains(trecho, xml);
            var transporte = new AACBSTransporteControlado();
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);

            Assert.Throws<ValidarXMLException>(() =>
            {
                using var servico = CriarDarf(adquirente, CriarConfiguracao(), xml.Replace(trecho, substituicao, StringComparison.Ordinal));
                servico.Executar();
            });

            Assert.Empty(transporte.Requisicoes);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void TimeoutDepoisDeSucessoLimpaResultadoESemReplay(bool adquirente)
        {
            var falha = new TimeoutException("Falha sintética de transporte.");
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(HttpStatusCode.OK, Sucesso(adquirente, Vencimento));
            transporte.EnfileirarFalha(falha);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = CriarDarf(adquirente, ConfiguracaoComToken());
            servico.Executar();
            ValidarRetorno(servico, "ret" + Operacao(adquirente));
            Assert.Single(transporte.Requisicoes);

            var erro = Assert.Throws<TimeoutException>(servico.Executar);
            Assert.Contains(Operacao(adquirente), erro.Message);
            Assert.DoesNotContain(falha.Message, erro.ToString());

            Assert.Equal((HttpStatusCode)0, servico.HttpStatusCode);
            Assert.Null(servico.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", Resultado(servico).ErroLocal.Codigo);
            Assert.Equal(2, transporte.Requisicoes.Count);
        }

        private static string Sucesso(bool adquirente, string nomeData) => $$"""
            {
              "informacoesPagamento": {"{{nomeData}}":"{{Data}}",{{(adquirente ? ValoresRad : ValoresPcont)}} },
              "darf": {{JsonDadosDarf}},
              "cdRetorno": "DARF_GERADO"
            }
            """;

        private static void ConferirItemRad(JsonElement item, string chave, string opcao, decimal? valor)
        {
            Assert.Equal(JsonValueKind.Object, item.ValueKind);
            Assert.Equal(chave, item.GetProperty("chaveDFE").GetString());
            Assert.Equal(opcao, item.GetProperty("opcaoPagamento").GetString());
            ConferirValorParcial(item, valor);
        }

        private static void ConferirValorParcial(JsonElement item, decimal? valor)
        {
            Assert.Equal(valor.HasValue ? 3 : 2, item.EnumerateObject().Count());
            Assert.Equal(valor.HasValue, item.TryGetProperty("valorDarfParcial", out var campo));
            if (valor.HasValue)
            {
                Assert.Equal(JsonValueKind.Number, campo.ValueKind);
                Assert.Equal(valor.Value, campo.GetDecimal());
            }
        }

        private static void ConferirSucesso(ServicosAACBS.ServicoBase servico, XmlAACBS.RetornoAACBSBase resultado,
            string raiz, string dataDaMassa, string nomeData)
        {
            ValidarRetorno(servico, raiz);
            Assert.Null(resultado.ErroLocal);
            var xmlEsperado = AACBSFixture.Carregar(raiz).DocumentElement.OuterXml.Replace(dataDaMassa, nomeData, StringComparison.Ordinal);
            CompararXml(xmlEsperado, servico.RetornoWSXML.DocumentElement.OuterXml);
            CompararXml(xmlEsperado, resultado.GerarXML().DocumentElement.OuterXml);
        }

        private static void RejeitarResposta(bool adquirente, HttpStatusCode status, string corpo, string tipoMidia)
        {
            var transporte = new AACBSTransporteControlado();
            transporte.Enfileirar(status, corpo, tipoMidia);
            using var executor = ApiTransportExecutorFactory.Override(() => transporte);
            using var servico = CriarDarf(adquirente, ConfiguracaoComToken());

            var erro = Assert.Throws<FormatException>(servico.Executar);

            Assert.Contains(Operacao(adquirente), erro.Message);
            Assert.DoesNotContain("SENTINELA-CORPO-REMOTO", erro.ToString());
            Assert.Equal(status, servico.HttpStatusCode);
            Assert.Null(servico.RetornoWSXML);
            Assert.Equal("SEM_RETORNO", Resultado(servico).ErroLocal.Codigo);
            ConferirPostFiscal(Assert.Single(transporte.Requisicoes), adquirente ? "rad" : "pcont", CnpjCompleto);
        }

        private static Configuracao ConfiguracaoComToken()
        {
            var configuracao = CriarConfiguracao();
            configuracao.AACBSToken = Token;
            return configuracao;
        }

        private static void ExecutarEmPtBr(ServicosAACBS.ServicoBase servico)
        {
            var cultura = CultureInfo.CurrentCulture;
            try
            {
                CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("pt-BR");
                servico.Executar();
            }
            finally
            {
                CultureInfo.CurrentCulture = cultura;
            }
        }

        private static ServicosAACBS.ServicoBase CriarDarf(bool adquirente, Configuracao configuracao, string xml = null)
        {
            xml ??= AACBSFixture.Carregar(RaizEnvio(adquirente)).DocumentElement.OuterXml;
            return adquirente
                ? new ServicosAACBS.GerarDarfAdquirente(xml, configuracao)
                : new ServicosAACBS.GerarDarfContribuinte(xml, configuracao);
        }

        private static string Operacao(bool adquirente) => adquirente ? "GerarDarfAdquirente" : "GerarDarfContribuinte";
        private static string RaizEnvio(bool adquirente) => adquirente ? "gerarDarfAdquirente" : "gerarDarfContribuinte";

        private static XmlAACBS.RetornoAACBSBase Resultado(ServicosAACBS.ServicoBase servico) => servico switch
        {
            ServicosAACBS.GerarDarfAdquirente rad => rad.Result,
            ServicosAACBS.GerarDarfContribuinte pcont => pcont.Result,
            _ => throw new ArgumentOutOfRangeException(nameof(servico))
        };
    }
}
