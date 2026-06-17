using System;
using System.Net;
using System.Net.Http;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.PIX;
using Xunit;

namespace Unimake.DFe.Test.PIX
{
    public class RetornoParserTest
    {
        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveMapearRetornoDeCriacaoDeCobranca()
        {
            const string json = @"{
    ""status"": ""ATIVA"",
    ""pixCopiaECola"": ""000201010212pix-copia-cola"",
    ""imageQRCode"": ""C:\\Retorno\\pix.png""
}";

            var retorno = ExecutarParser<retPIXCobrancaCriar>(Servico.PIXCobrancaCriar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(0, retorno.Status);
            Assert.Equal("PIX Ativo (Cobrança gerada)", retorno.Motivo);
            Assert.Equal("000201010212pix-copia-cola", retorno.PixCopiaECola);
            Assert.Equal(@"C:\Retorno\pix.png", retorno.ImageQRCode);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveMapearRetornoDeConsultaDeCobranca()
        {
            const string json = @"{
    ""pix"": [
        {
            ""txid"": ""TX-001"",
            ""valor"": ""75.36"",
            ""horario"": ""2023-10-31T16:29:41"",
            ""pagador"": {
                ""nome"": ""Teste nome pagador 1"",
                ""cnpj"": ""12345678901234""
            }
        },
        {
            ""txid"": ""TX-002"",
            ""valor"": ""66.22"",
            ""horario"": ""2023-10-31T16:29:41"",
            ""pagador"": {
                ""nome"": ""Teste nome pagador 2"",
                ""cpf"": ""11111111111""
            }
        }
    ]
}";

            var retorno = ExecutarParser<retPIXCobrancaConsultar>(Servico.PIXCobrancaConsultar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Movimentos PIX localizados.", retorno.Motivo);
            Assert.Equal(2, retorno.Items.Count);
            Assert.Equal("1", retorno.Items[0].Id);
            Assert.Equal("TX-001", retorno.Items[0].TxId);
            Assert.Equal("Teste nome pagador 1", retorno.Items[0].Pagador.Nome);
            Assert.Equal("12345678901234", retorno.Items[0].Pagador.Inscricao);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveMapearRetornoDeConsultaDePix()
        {
            const string json = @"{
    ""txid"": ""12345678901234567890123456"",
    ""valor"": {
        ""original"": ""0.00""
    },
    ""horario"": {
        ""liquidacao"": ""2023-05-23T10:42:05""
    },
    ""pagador"": {
        ""nome"": ""Teste nome pagador"",
        ""cnpj"": ""12345678901234""
    }
}";

            var retorno = ExecutarParser<retPIXConsultar>(Servico.PIXConsultar, json, HttpStatusCode.OK, "application/json");

            Assert.Equal(1, retorno.Status);
            Assert.Equal("Movimento PIX Localizado.", retorno.Motivo);
            Assert.Equal("12345678901234567890123456", retorno.TxId);
            Assert.Equal("0.00", retorno.Valor);
            Assert.Equal("Teste nome pagador", retorno.Pagador.Nome);
            Assert.Equal("12345678901234", retorno.Pagador.Inscricao);
            Assert.Equal(Info.VersaoDLL, retorno.DLLVersao);
        }

        [Fact]
        [Trait("DFe", "PIX")]
        public void DeveMapearRetornoDeErroDoPix()
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

            var retorno = ExecutarParser<retPIXConsultar>(Servico.PIXConsultar, json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Equal(999, retorno.Status);
            Assert.Equal("A configuração 'TESTE-55LTDXKYYC,TESTE-55LTDXKYYC' não é valida para este contexto.", retorno.Motivo);
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
