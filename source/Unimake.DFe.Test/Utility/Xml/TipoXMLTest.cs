using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.Utility.Xml
{
    public class TipoXMLTest
    {
        [Fact]
        [Trait("Utility", "TipoXML")]
        public void ObterTipoXmlConsultaSituacaoBPe()
        {
            var doc = new XmlDocument();
            doc.LoadXml("<consSitBPe versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/bpe\"><tpAmb>2</tpAmb><xServ>CONSULTAR</xServ><chBPe>41260899999999000191630010000000011000000014</chBPe></consSitBPe>");

            Assert.Equal(TipoXML.BPeConsultaSituacao, XMLUtility.DetectXMLType(doc));
        }

        /// <summary>
        /// Receber um arquivo de XML do eSocial e devolver o tipo dele
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("Utility", "TipoXML")]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsTabela-esocial-considevt.xml", TipoXML.ESocialConsultaEvtsTabela)]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsEmpregador-esocial-considevt.xml", TipoXML.ESocialConsultaEvtsEmpregador)]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsTrabalhador-esocial-considevt.xml", TipoXML.ESocialConsultaEvtsTrabalhador)]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaLoteEventos-esocial-consloteevt.xml", TipoXML.ESocialConsultaLoteAssincrono)]
        [InlineData(@"..\..\..\ESocial\Resources\DownloadEventosPorId-esocial-downevt.xml", TipoXML.ESocialDownloadPorID)]
        [InlineData(@"..\..\..\ESocial\Resources\DownloadEventosPorNrRec-esocial-downevt.xml", TipoXML.ESocialDownloadPorNrRec)]
        [InlineData(@"..\..\..\ESocial\Resources\EnvioLoteEventos-esocial-loteevt.xml",TipoXML.ESocialEnvioLoteEventos)]
        public void ObterTipoXmlESocial(string arqXML, TipoXML tipoEsperado)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste de obter o tipo do XML.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            TipoXML tipoXml = XMLUtility.DetectXMLType(doc);

            Assert.Equal(tipoEsperado, tipoXml);
        }

        [Theory]
        [Trait("Utility", "TipoXML")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S1000-reinf-cons.xml", TipoXML.EFDReinfConsultaReciboEvento)]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaLoteAssincrono-Reinf-consloteevt.xml", TipoXML.EFDReinfConsultaLoteAssincrono)]
        [InlineData(@"..\..\..\EFDReinf\Resources\loteEventosAssincrono-Reinf-loteevt.xml", TipoXML.EFDReinfEnvioLoteEventos)]
        [InlineData(@"..\..\..\EFDReinf\Resources\1000_evtInfoContri-Reinf-evt.xml", TipoXML.EFDReinfEvento)]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaResultadoFechamento2099-reinf-cons.xml", TipoXML.EFDReinfConsultaFechamento2099)]
        public void ObterTipoXmlEFDReinf(string arqXML, TipoXML tipoEsperado)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste de obter o tipo do XML.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            TipoXML tipoXml = XMLUtility.DetectXMLType(doc);

            Assert.Equal(tipoEsperado, tipoXml);
        }

        [Theory]
        [Trait("Utility", "TipoXML")]
        [InlineData(@"..\..\..\Utility\Resources\xmlNaoIdentificado.xml", TipoXML.NaoIdentificado)]
        public void ObterTipoXmlNaoIdentificado(string arqXML, TipoXML tipoEsperado)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste de obter o tipo do XML.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            TipoXML tipoXml = XMLUtility.DetectXMLType(doc);

            Assert.Equal(tipoEsperado, tipoXml);
        }
    }
}
