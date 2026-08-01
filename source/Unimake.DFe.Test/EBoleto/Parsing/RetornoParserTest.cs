using System;
using System.IO;
using System.Net;
using System.Net.Http;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto.Parsing
{
    public class RetornoParserTest
    {
        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveMapearRetornoDeRegistroDeBoleto()
        {
            const string json = @"{
    ""message"": ""Boleto registrado"",
    ""codigoBarraNumerico"": ""61885413104039157298878833105511271184934708"",
    ""numeroNoBanco"": ""123456789"",
    ""linhaDigitavel"": ""55666538375117619420465569876583610990625736333"",
    ""pdfContent"": {
        ""success"": true,
        ""content"": ""JVBERi0xLjQK""
    },
    ""pdfPath"": ""d:\\testenfe\\Retorno\\ret-BoletoRegistrar.pdf"",
    ""qrCodeContent"": {
        ""success"": false,
        ""image"": """",
        ""text"": """"
    }
}";

            var retorno = ExecutarParser<retBoletoRegistrar>(Servico.EBoletoRegistrar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(0, retorno.Status);
            Assert.Equal("Boleto registrado", retorno.Motivo);
            Assert.Equal("123456789", retorno.NumeroNoBanco);
            Assert.True(retorno.PdfContentSuccess);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveMapearRetornoDeConsultaDeBoleto()
        {
            const string json = @"{
    ""message"": ""Boletos encontrados"",
    ""items"": [
        {
            ""dataEmissao"": ""20-09-2018"",
            ""dataVencimento"": ""20-09-2018"",
            ""numeroNaEmpresa"": ""1235512"",
            ""numeroNoBanco"": ""123456789"",
            ""pagador"": {
                ""email"": ""pagador@dominio.com.br"",
                ""tipoInscricao"": 0,
                ""endereco"": {
                    ""logradouro"": ""Rua 87 Quadra 1 Lote 1 casa 1"",
                    ""bairro"": ""Santa Rosa"",
                    ""cidade"": ""Luziânia"",
                    ""uf"": ""DF"",
                    ""cep"": ""72320000""
                }
            },
            ""pdfContent"": {
                ""success"": false
            },
            ""qrCodeContent"": {
                ""text"": ""00020101021226950014br.gov.bcb.pix2573pix.sicoob.com.br"",
                ""success"": false
            },
            ""situacao"": 9,
            ""tipoLiquidacao"": 0,
            ""valor"": ""156.23"",
            ""valorLiquidado"": ""156.23""
        }
    ]
}";

            var retorno = ExecutarParser<retBoletoConsultar>(Servico.EBoletoConsultar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(0, retorno.Status);
            Assert.Equal("Boletos encontrados", retorno.Motivo);
            Assert.Single(retorno.BoletoResponse);
            Assert.Equal("123456789", retorno.BoletoResponse[0].NumeroNoBanco);
            Assert.Equal("pagador@dominio.com.br", retorno.BoletoResponse[0].Pagador.Email);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarContratoLegadoDoRegistroDeBoleto()
        {
            const string json = @"{
    ""message"": ""Registro concluído pela API"",
    ""codigoBarraNumerico"": ""61885413104039157298878833105511271184934708"",
    ""numeroNoBanco"": ""123456789"",
    ""linhaDigitavel"": ""55666538375117619420465569876583610990625736333"",
    ""pdfContent"": {
        ""success"": true,
        ""message"": null,
        ""content"": ""JVBERi0xLjQK""
    },
    ""pixPagamentoDetalhe"": {
        ""dataPagamento"": ""2026-08-01T10:11:12.345Z"",
        ""txId"": ""PIX-REGISTRO-001"",
        ""valorAbatimento"": 1.2,
        ""valorDesconto"": null,
        ""valorJuros"": 3,
        ""valorLiquidado"": 100,
        ""valorMulta"": 4.567,
        ""valorOriginal"": 101.2
    },
    ""qrCodeContent"": {
        ""success"": false,
        ""image"": """",
        ""text"": """"
    }
}";

            var xml = ExecutarTratamentoCompleto(Servico.EBoletoRegistrar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal("0", xml.SelectSingleNode("/BoletoRegistrarResponse/Status").InnerText);
            Assert.Equal("Boleto registrado", xml.SelectSingleNode("/BoletoRegistrarResponse/Motivo").InnerText);
            Assert.Equal("True", xml.SelectSingleNode("/BoletoRegistrarResponse/PdfContentSuccess").InnerText);
            Assert.NotNull(xml.SelectSingleNode("/BoletoRegistrarResponse/PdfContentMessage"));
            Assert.NotNull(xml.SelectSingleNode("/BoletoRegistrarResponse/PdfPath"));
            Assert.Equal("2026-08-01T10:11:12.345Z", xml.SelectSingleNode("//PixPagamentoDetalhe/DataPagamento").InnerText);
            Assert.Equal("PIX-REGISTRO-001", xml.SelectSingleNode("//PixPagamentoDetalhe/TxId").InnerText);
            Assert.Equal("1.20", xml.SelectSingleNode("//PixPagamentoDetalhe/ValorAbatimento").InnerText);
            Assert.Equal(string.Empty, xml.SelectSingleNode("//PixPagamentoDetalhe/ValorDesconto").InnerText);
            Assert.Equal("3.00", xml.SelectSingleNode("//PixPagamentoDetalhe/ValorJuros").InnerText);
            Assert.Equal("100.00", xml.SelectSingleNode("//PixPagamentoDetalhe/ValorLiquidado").InnerText);
            Assert.Equal("4.57", xml.SelectSingleNode("//PixPagamentoDetalhe/ValorMulta").InnerText);
            Assert.Equal("101.20", xml.SelectSingleNode("//PixPagamentoDetalhe/ValorOriginal").InnerText);
            Assert.Null(xml.SelectSingleNode("/BoletoRegistrarResponse/QRCodeContent"));
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarTraceIdNoErroDoRegistroDeBoleto()
        {
            const string json = @"{
    ""errors"": [""Não foi possível registrar o boleto.\r\nDetalhe adicional.""],
    ""status"": 400,
    ""traceId"": ""TRACE-REGISTRO-001""
}";

            var xml = ExecutarTratamentoCompleto(Servico.EBoletoRegistrar, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal("999", xml.SelectSingleNode("/BoletoRegistrarResponse/Status").InnerText);
            Assert.Equal("Não foi possível registrar o boleto.Detalhe adicional.", xml.SelectSingleNode("/BoletoRegistrarResponse/Motivo").InnerText);
            Assert.Equal("TRACE-REGISTRO-001", xml.SelectSingleNode("/BoletoRegistrarResponse/TraceId").InnerText);
            Assert.Null(xml.SelectSingleNode("/BoletoRegistrarResponse/PdfContentSuccess"));
            Assert.Null(xml.SelectSingleNode("/BoletoRegistrarResponse/QRCodeContent"));
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarContratoLegadoDaConsultaDeBoleto()
        {
            const string json = @"[
    {
        ""dataEmissao"": ""2018-09-20T10:30:00-03:00"",
        ""dataLiquidacao"": ""2018-09-21T10:30:00-03:00"",
        ""dataVencimento"": ""2018-09-22T10:30:00-03:00"",
        ""numeroNoBanco"": ""123456789"",
        ""pdfContent"": { ""success"": false },
        ""pixPagamentoDetalhe"": {
            ""dataPagamento"": ""2018-09-21T10:30:00-03:00"",
            ""valorLiquidado"": 156.2,
            ""valorOriginal"": ""156.2""
        },
        ""qrCodeContent"": { ""success"": false },
        ""valor"": 156.2
    }
]";

            var xml = ExecutarTratamentoCompleto(Servico.EBoletoConsultar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal("0", xml.SelectSingleNode("/BoletoConsultarResponse/Status").InnerText);
            Assert.Equal("Boletos encontrados", xml.SelectSingleNode("/BoletoConsultarResponse/Motivo").InnerText);
            Assert.Equal("20-09-2018", xml.SelectSingleNode("//BoletoResponse/DataEmissao").InnerText);
            Assert.Equal("21-09-2018", xml.SelectSingleNode("//BoletoResponse/DataLiquidacao").InnerText);
            Assert.Equal("22-09-2018", xml.SelectSingleNode("//BoletoResponse/DataVencimento").InnerText);
            Assert.Equal("156.20", xml.SelectSingleNode("//BoletoResponse/Valor").InnerText);
            Assert.Equal("0.00", xml.SelectSingleNode("//BoletoResponse/ValorAbatimento").InnerText);
            Assert.Equal("False", xml.SelectSingleNode("//BoletoResponse/PdfContent/Success").InnerText);
            Assert.Equal("False", xml.SelectSingleNode("//BoletoResponse/QrCodeContent/Success").InnerText);
            Assert.Equal("21-09-2018", xml.SelectSingleNode("//PIXPagamentoDetalhe/DataPagamento").InnerText);
            Assert.Equal("156.20", xml.SelectSingleNode("//PIXPagamentoDetalhe/ValorLiquidado").InnerText);
            Assert.NotNull(xml.SelectSingleNode("//BoletoResponse/PdfContent/Content"));
            Assert.NotNull(xml.SelectSingleNode("//BoletoResponse/QrCodeContent/Text"));
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarRetornoQuandoNenhumBoletoForEncontrado()
        {
            const string json = @"{ ""items"": [] }";

            var retorno = ExecutarParser<retBoletoConsultar>(Servico.EBoletoConsultar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Nenhum boleto encontrado", retorno.Motivo);
            Assert.Empty(retorno.BoletoResponse);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveMapearRetornoDeErroDoEBoleto()
        {
            const string json = @"{
    ""errors"": [
        ""A configuração 'TESTE-55LTDXKYYC,TESTE-55LTDXKYYC' não é valida para este contexto.""
    ],
    ""status"": 400,
    ""title"": ""Consultar"",
    ""traceId"": ""0HNMB2BOI7LB7-00000001"",
    ""type"": ""InvalidBankCredentialsException""
}";

            var retorno = ExecutarParser<retBoletoConsultar>(Servico.EBoletoConsultar, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal(999, retorno.Status);
            Assert.Equal("A configuração 'TESTE-55LTDXKYYC,TESTE-55LTDXKYYC' não é valida para este contexto.", retorno.Motivo);
            Assert.Equal("0HNMB2BOI7LB7-00000001", retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarRetornoDeSucessoAoInformarPagamento()
        {
            const string json = @"{}";

            var retornoOk = ExecutarParser<retBoletoInformarPagto>(Servico.EBoletoInformarPagt, json, HttpStatusCode.OK, "application/json");
            var retornoAccepted = ExecutarParser<retBoletoInformarPagto>(Servico.EBoletoInformarPagt, json, HttpStatusCode.Accepted, "application/json");

            Assert.Equal(0, retornoOk.Status);
            Assert.Equal("Instrução para marcar o boleto como pago enviado com sucesso", retornoOk.Motivo);
            Assert.Equal(Info.VersaoDLL, retornoOk.DLLVersao);
            Assert.Equal(0, retornoAccepted.Status);
            Assert.Equal("Instrução para marcar o boleto como pago enviado com sucesso", retornoAccepted.Motivo);
            Assert.Equal(Info.VersaoDLL, retornoAccepted.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DevePreservarRetornoDeErroAoInformarPagamento()
        {
            const string json = @"{
    ""codigo"": ""BOLETO_INVALIDO"",
    ""mensagem"": ""Boleto não localizado."",
    ""traceId"": ""TRACE-PAGAMENTO-001""
}";

            var retorno = ExecutarParser<retBoletoInformarPagto>(Servico.EBoletoInformarPagt, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Não foi possível marcar o boleto como pago. Tente novamente mais tarde. (Status Code: 400) - (Erro: BOLETO_INVALIDO - Boleto não localizado.)", retorno.Motivo);
            Assert.Equal("TRACE-PAGAMENTO-001", retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveManterBoletoInformarPagtoResponseQuandoApiRetornarErroInterno()
        {
            const string json = @"{
    ""codigo"": ""ERRO_INTERNO"",
    ""mensagem"": ""Falha temporária no eBoleto."",
    ""traceId"": ""TRACE-PAGAMENTO-500""
}";

            var xml = ExecutarTratamentoCompleto(Servico.EBoletoInformarPagt, json, HttpStatusCode.InternalServerError, "application/problem+json");

            Assert.Equal("BoletoInformarPagtoResponse", xml.DocumentElement.Name);

            var retorno = XMLUtility.Deserializar<retBoletoInformarPagto>(xml);
            Assert.Equal(1, retorno.Status);
            Assert.Equal("Não foi possível marcar o boleto como pago. Tente novamente mais tarde. (Status Code: 500) - (Erro: ERRO_INTERNO - Falha temporária no eBoleto.)", retorno.Motivo);
            Assert.Equal("TRACE-PAGAMENTO-500", retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveRetornarStatusUmETraceIdQuandoCancelamentoForRejeitado()
        {
            const string json = @"{
    ""errors"": [""Não foi possível cancelar o boleto.""],
    ""status"": 400,
    ""traceId"": ""TRACE-CANCELAMENTO-001""
}";

            var retorno = ExecutarParser<retBoletoCancelar>(Servico.EBoletoCancelar, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Não foi possível cancelar o boleto.", retorno.Motivo);
            Assert.Equal("TRACE-CANCELAMENTO-001", retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveManterBoletoCancelarResponseQuandoApiRetornarErroInterno()
        {
            const string json = @"{
    ""title"": ""Erro interno no eBoleto"",
    ""status"": 500,
    ""traceId"": ""TRACE-CANCELAMENTO-500""
}";

            var retorno = ExecutarTratamentoCompleto(Servico.EBoletoCancelar, json, HttpStatusCode.InternalServerError, "application/problem+json");

            Assert.Equal("BoletoCancelarResponse", retorno.DocumentElement.Name);

            var cancelamento = XMLUtility.Deserializar<retBoletoCancelar>(retorno);
            Assert.Equal(1, cancelamento.Status);
            Assert.Equal("Erro interno no eBoleto", cancelamento.Motivo);
            Assert.Equal("TRACE-CANCELAMENTO-500", cancelamento.TraceId);
            Assert.Equal(Info.VersaoDLL, cancelamento.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveManterSucessoAoAlterarVencimento()
        {
            const string json = @"{}";

            var retorno = ExecutarParser<retBoletoAlterarVencto>(Servico.EBoletoAlterarVencto, json, HttpStatusCode.Accepted, "application/json");

            Assert.Equal(0, retorno.Status);
            Assert.Equal("Vencimento alterado com sucesso", retorno.Motivo);
            Assert.Null(retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveRetornarStatusUmETraceIdQuandoAlteracaoDeVencimentoForRejeitada()
        {
            const string json = @"{
    ""errors"": [""Não foi possível alterar o vencimento.""],
    ""status"": 400,
    ""traceId"": ""TRACE-BOLETO-001""
}";

            var retorno = ExecutarParser<retBoletoAlterarVencto>(Servico.EBoletoAlterarVencto, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Não foi possível alterar o vencimento.", retorno.Motivo);
            Assert.Equal("TRACE-BOLETO-001", retorno.TraceId);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        private static T ExecutarParser<T>(Servico servico, string conteudo, HttpStatusCode statusCode, string mediaType)
            where T : class, new()
        {
            var assembly = typeof(APIConfig).Assembly;
            var parserType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Parsers.ApiResponseContentParser", true);
            var contextType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Parsers.ApiResponseContext", true);

            var parser = Activator.CreateInstance(parserType, true);
            var context = Activator.CreateInstance(contextType, true);

            var config = new APIConfig
            {
                ResponseMediaType = mediaType,
                Servico = servico
            };

            var response = new HttpResponseMessage(statusCode)
            {
                Content = new StringContent(conteudo)
            };
            response.Content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue(mediaType);

            contextType.GetProperty("Config", BindingFlags.Instance | BindingFlags.Public).SetValue(context, config);
            contextType.GetProperty("Response", BindingFlags.Instance | BindingFlags.Public).SetValue(context, response);
            contextType.GetProperty("ResponseContent", BindingFlags.Instance | BindingFlags.Public).SetValue(context, conteudo);

            var metodo = parserType.GetMethod("Parse", BindingFlags.Instance | BindingFlags.Public);
            var parametros = new[] { context };
            var xmlRetorno = (XmlDocument)metodo.Invoke(parser, parametros);

            return XMLUtility.Deserializar<T>(xmlRetorno);
        }

        private static XmlDocument ExecutarTratamentoCompleto(Servico servico, string conteudo, HttpStatusCode statusCode, string mediaType)
        {
            var config = new APIConfig
            {
                ResponseMediaType = mediaType,
                Servico = servico
            };

            using (var response = new HttpResponseMessage(statusCode))
            {
                response.Content = new StringContent(conteudo);
                response.Content.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue(mediaType);

                Stream stream = null;
                return TratarRetornoAPI.ReceberRetorno(ref config, response, ref stream);
            }
        }
    }
}
