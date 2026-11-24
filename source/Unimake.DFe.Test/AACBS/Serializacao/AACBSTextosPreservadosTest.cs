using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.AACBS;
using Xunit;

namespace Unimake.DFe.Test.AACBS.Serializacao
{
    [Trait("DFe", "AACBS")]
    public class AACBSTextosPreservadosTest
    {
        [Theory]
        [InlineData("A\rB")]
        [InlineData("A\r\nB")]
        [InlineData("A\rB\r\nC\nD")]
        public void TicketEMensagemMantemQuebrasDeLinhaNoRoundTrip(string texto)
        {
            var consulta = new ConsultarSituacaoSolicitacao { TiqueteSolicitacao = texto };
            var retorno = new RetConsultarDebitos { CodigoErro = "ERRO_SINTETICO", MensagemErro = texto };

            Assert.Equal(texto, XMLUtility.Deserializar<ConsultarSituacaoSolicitacao>(consulta.GerarXML()).TiqueteSolicitacao);
            Assert.Equal(texto, XMLUtility.Deserializar<RetConsultarDebitos>(retorno.GerarXML()).MensagemErro);
            Assert.Equal(texto, XMLUtility.DeserializarEstrito<ConsultarSituacaoSolicitacao>(consulta.GerarXML()).TiqueteSolicitacao);
            Assert.Equal(texto, XMLUtility.DeserializarEstrito<RetConsultarDebitos>(retorno.GerarXML()).MensagemErro);
            var xmlComReferenciaCr = GravarComoTexto(consulta.GerarXML());
            Assert.Contains("&#xD;", xmlComReferenciaCr, StringComparison.OrdinalIgnoreCase);
            Assert.Equal(texto, XMLUtility.Deserializar<ConsultarSituacaoSolicitacao>(xmlComReferenciaCr).TiqueteSolicitacao);
            Assert.Equal(texto, XMLUtility.Deserializar<RetConsultarDebitos>(GravarComoTexto(retorno.GerarXML())).MensagemErro);
#if INTEROP
            Assert.Equal(texto, XMLUtility.Deserializar<ConsultarSituacaoSolicitacao>(consulta.GerarXMLString()).TiqueteSolicitacao);
            Assert.Equal(texto, XMLUtility.Deserializar<RetConsultarDebitos>(retorno.GerarXMLString()).MensagemErro);
#endif
        }

        private static string GravarComoTexto(XmlDocument document)
        {
            using (var text = new StringWriter())
            {
                using (var writer = XmlWriter.Create(text, new XmlWriterSettings { NewLineHandling = NewLineHandling.Entitize }))
                    document.WriteTo(writer);
                return text.ToString();
            }
        }
    }
}
