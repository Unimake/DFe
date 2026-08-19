using System.Net;
using Unimake.Business.DFe;
using Unimake.Business.DFe.ConsumirServico.Contracts;
using Unimake.Business.DFe.ConsumirServico.Parsers;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.Utility.Xml
{
    public class SoapResponseParserTest
    {
        [Fact]
        [Trait("Utility", "Xml")]
        public void Parse_TagRetornoSemConteudo_DeveGerarExcecaoDescritiva()
        {
            var context = CriarContexto(
                "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\">" +
                "<soap:Body><nfcomResultMsg xmlns=\"http://www.portalfiscal.inf.br/nfcom/wsdl/NFComRecepcao\" />" +
                "</soap:Body></soap:Envelope>");

            var exception = Assert.Throws<ValidarXMLRetornoException>(() => new SoapResponseParser().Parse(context));

            Assert.Contains("<nfcomResultMsg>", exception.Message);
            Assert.Contains("sem conteúdo", exception.Message);
        }

        [Fact]
        [Trait("Utility", "Xml")]
        public void Parse_TagRetornoComConteudo_DeveExtrairXmlFiscal()
        {
            var context = CriarContexto(
                "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\">" +
                "<soap:Body><nfcomResultMsg xmlns=\"http://www.portalfiscal.inf.br/nfcom/wsdl/NFComRecepcao\">" +
                "<retNFCom xmlns=\"http://www.portalfiscal.inf.br/nfcom\"><cStat>213</cStat></retNFCom>" +
                "</nfcomResultMsg></soap:Body></soap:Envelope>");

            var result = new SoapResponseParser().Parse(context);

            Assert.Equal("retNFCom", result.RetornoServicoXml.DocumentElement.LocalName);
            Assert.Equal("213", result.RetornoServicoXml.GetElementsByTagName("cStat")[0].InnerText);
        }

        private static SoapResponseContext CriarContexto(string conteudo)
        {
            return new SoapResponseContext
            {
                ConteudoRetorno = conteudo,
                Soap = new WSSoap { TagRetorno = "nfcomResultMsg" },
                TransportResponse = new TransportResponse { StatusCode = HttpStatusCode.OK },
                TratarScapeRetorno = false
            };
        }
    }
}
