using System;
using System.Net;
using System.Net.Http;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EBoleto;
using Xunit;

namespace Unimake.DFe.Test.EBoleto
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
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveMapearRetornoSimplesDeInformarPagamento()
        {
            const string json = @"{
    ""status"": 1,
    ""message"": ""Não foi possível marcar o boleto como pago. Tente novamente mais tarde. (Status Code: 202)""
}";

            var retorno = ExecutarParser<retBoletoInformarPagto>(Servico.EBoletoInformarPagt, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Não foi possível marcar o boleto como pago. Tente novamente mais tarde. (Status Code: 202)", retorno.Motivo);
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
    }
}
