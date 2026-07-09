using System;
using System.Net;
using System.Net.Http;
using System.Reflection;
using System.Xml;
using Unimake.Business.DFe;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.UMessenger;
using Xunit;

namespace Unimake.DFe.Test.UMessenger.Parsing
{
    public class RetornoParserTest
    {
        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveMapearRetornoDeSucesso()
        {
            const string json = @"{
    ""messageId"": ""MSG-001"",
    ""messagingService"": 0,
    ""recipient"": {
        ""destination"": ""5511999999999@s.whatsapp.net""
    }
}";

            var retorno = ExecutarParser(json, HttpStatusCode.OK, "application/json");

            Assert.Single(retorno.Mensagem);
            Assert.Equal(1, retorno.Mensagem[0].Status);
            Assert.Equal("Mensagem enviada com sucesso.", retorno.Mensagem[0].Motivo);
            Assert.Equal("MSG-001", retorno.Mensagem[0].MessageID);
            Assert.Equal("MSG-001", retorno.MessageId);
            Assert.Equal(Info.VersaoDLL, retorno.Mensagem[0].DLLVersao);
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DevePreservarRawResponseELocalIdNoRetornoCompativel()
        {
            const string json = @"{
    ""messageId"": ""MSG-LOCAL-001"",
    ""localId"": ""LOCAL-123"",
    ""messagingService"": 0
}";

            var xml = new XmlDocument();
            xml.LoadXml(@"<uMessengerResponse><Mensagem><Status>1</Status><Motivo>Mensagem enviada com sucesso.</Motivo><messageID>MSG-LOCAL-001</messageID><DLLVersao>20260616.1001</DLLVersao></Mensagem></uMessengerResponse>");

            var metodo = typeof(Business.DFe.Servicos.UMessenger.PublishUMessenger).GetMethod("CriarRetornoCompativel", BindingFlags.Static | BindingFlags.NonPublic);
            var retorno = (retUMessengerPublish)metodo.Invoke(null, new object[] { xml, json });

            Assert.Equal("MSG-LOCAL-001", retorno.MessageId);
            Assert.Equal("LOCAL-123", retorno.LocalId);
            Assert.Equal(json, retorno.RawResponse);
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveMapearRetornoDeErroComErrorsComoArray()
        {
            const string json = @"{
    ""errors"": [
        ""Mensagem de erro de teste.""
    ],
    ""helpLink"": ""https://example.test/help"",
    ""status"": 400,
    ""title"": ""Publish"",
    ""traceId"": ""trace-array"",
    ""type"": ""MessageBrokerException""
}";

            var retorno = ExecutarParser(json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Single(retorno.Mensagem);
            Assert.Equal(0, retorno.Mensagem[0].Status);
            Assert.Equal("Mensagem de erro de teste.", retorno.Mensagem[0].Motivo);
            Assert.Equal("trace-array", retorno.Mensagem[0].TraceId);
            Assert.Equal("https://example.test/help", retorno.Mensagem[0].HelpLink);
            Assert.Equal("MessageBrokerException", retorno.Mensagem[0].ErrorType);
            Assert.Equal("Publish", retorno.Mensagem[0].ErrorTitle);
            Assert.Equal(Info.VersaoDLL, retorno.Mensagem[0].DLLVersao);
        }

        [Fact]
        [Trait("DFe", "UMessenger")]
        public void DeveMapearRetornoDeErroComErrorsComoObjeto()
        {
            const string json = @"{
    ""errors"": {
        ""Text"": [
            ""The Text field is required.""
        ]
    },
    ""status"": 400,
    ""title"": ""One or more validation errors occurred."",
    ""type"": ""ValidationException"",
    ""helpLink"": ""https://example.test/validation-help"",
    ""traceId"": ""trace-object""
}";

            var retorno = ExecutarParser(json, HttpStatusCode.BadRequest, "application/problem+json");

            Assert.Single(retorno.Mensagem);
            Assert.Equal(0, retorno.Mensagem[0].Status);
            Assert.Equal("The Text field is required.", retorno.Mensagem[0].Motivo);
            Assert.Equal("trace-object", retorno.Mensagem[0].TraceId);
            Assert.Equal("https://example.test/validation-help", retorno.Mensagem[0].HelpLink);
            Assert.Equal("ValidationException", retorno.Mensagem[0].ErrorType);
            Assert.Equal("One or more validation errors occurred.", retorno.Mensagem[0].ErrorTitle);
            Assert.Equal(Info.VersaoDLL, retorno.Mensagem[0].DLLVersao);
        }

        private static retUMessengerPublish ExecutarParser(string conteudo, HttpStatusCode statusCode, string mediaType)
        {
            var assembly = typeof(APIConfig).Assembly;
            var parserType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Parsers.ApiResponseContentParser", true);
            var contextType = assembly.GetType("Unimake.Business.DFe.ConsumirServico.Parsers.ApiResponseContext", true);

            var parser = Activator.CreateInstance(parserType, true);
            var context = Activator.CreateInstance(contextType, true);

            var config = new APIConfig
            {
                ResponseMediaType = mediaType,
                Servico = Servico.UMessengerPublish
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

            return XMLUtility.Deserializar<retUMessengerPublish>(xmlRetorno);
        }
    }
}
