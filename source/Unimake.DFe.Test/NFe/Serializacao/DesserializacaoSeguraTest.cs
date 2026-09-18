using Unimake.Business.DFe.Utility;
using Unimake.Exceptions;
using Xunit;
using NFeXml = Unimake.Business.DFe.Xml.NFe.NFe;

namespace Unimake.DFe.Test.NFe.Serializacao
{
    /// <summary>
    /// Testes da proteção contra perda silenciosa de dados durante a desserialização da NFe.
    /// </summary>
    [Trait("DFe", "NFe")]
    public class DesserializacaoSeguraTest
    {
        private const string NamespaceNFe = "http://www.portalfiscal.inf.br/nfe";

        /// <summary>
        /// Deve interromper a desserialização quando um elemento fora do leiaute faz elementos mapeados serem ignorados.
        /// </summary>
        [Fact]
        public void DeveFalharQuandoElementoInvalidoProvocaPerdaDeDados()
        {
            var xml = CriarNFe("<vDif>0.00</vDif><pDevTrib>0.00</pDevTrib><vDevTrib>0.00</vDevTrib><vCBS>0.07</vCBS><vCredPres>0.00</vCredPres><vCredPresCondSus>0.00</vCredPresCondSus>");

            var exception = Assert.Throws<DesserializacaoXMLException>(() => XMLUtility.Deserializar<NFeXml>(xml));

            Assert.Contains("<pDevTrib>", exception.Message);
            Assert.Contains("<vCBS>", exception.Message);
            Assert.Contains("conteúdo mapeado que seria perdido", exception.Message);
        }

        /// <summary>
        /// Deve preservar a compatibilidade quando uma tag genuinamente desconhecida não causa perda de dados mapeados.
        /// </summary>
        [Fact]
        public void DeveTolerarElementoDesconhecidoSemPerdaNoModoPadrao()
        {
            var xml = CriarNFe("<vDif>0.00</vDif><vDevTrib>0.00</vDevTrib><vCBS>0.07</vCBS><vCredPres>0.00</vCredPres><vCredPresCondSus>0.00</vCredPresCondSus><campoFuturo>1</campoFuturo>");

            var nfe = XMLUtility.Deserializar<NFeXml>(xml);

            Assert.Equal(0.07, nfe.InfNFeField.Total.IBSCBSTot.GCBS.VCBS);
        }

        /// <summary>
        /// Deve rejeitar qualquer elemento desconhecido quando o consumidor solicitar desserialização estrita.
        /// </summary>
        [Fact]
        public void DeveRejeitarElementoDesconhecidoNoModoEstrito()
        {
            var xml = CriarNFe("<vDif>0.00</vDif><vDevTrib>0.00</vDevTrib><vCBS>0.07</vCBS><vCredPres>0.00</vCredPres><vCredPresCondSus>0.00</vCredPresCondSus><campoFuturo>1</campoFuturo>");

            var exception = Assert.Throws<DesserializacaoXMLException>(() => XMLUtility.DeserializarEstrito<NFeXml>(xml));

            Assert.Contains("<campoFuturo>", exception.Message);
            Assert.Contains("sem mapeamento", exception.Message);
        }

        /// <summary>
        /// Deve rejeitar atributos desconhecidos quando o consumidor solicitar desserialização estrita.
        /// </summary>
        [Fact]
        public void DeveRejeitarAtributoDesconhecidoNoModoEstrito()
        {
            var xml = CriarNFe("<vDif>0.00</vDif><vDevTrib>0.00</vDevTrib><vCBS>0.07</vCBS><vCredPres>0.00</vCredPres><vCredPresCondSus>0.00</vCredPresCondSus>")
                .Replace("<NFe ", "<NFe atributoFuturo=\"1\" ");

            var exception = Assert.Throws<DesserializacaoXMLException>(() => XMLUtility.DeserializarEstrito<NFeXml>(xml));

            Assert.Contains("Atributo <atributoFuturo>", exception.Message);
        }

        private static string CriarNFe(string conteudoGCBS) =>
            $"<NFe xmlns=\"{NamespaceNFe}\"><infNFe versao=\"4.00\"><total><IBSCBSTot><vBCIBSCBS>7.83</vBCIBSCBS><gCBS>{conteudoGCBS}</gCBS></IBSCBSTot><vNFTot>10.07</vNFTot></total></infNFe></NFe>";
    }
}
