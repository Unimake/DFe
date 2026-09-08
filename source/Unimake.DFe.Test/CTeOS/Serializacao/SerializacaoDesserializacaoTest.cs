using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Xunit;

namespace Unimake.DFe.Test.CTeOS.Serializacao
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do CTeOS
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML CTeOS
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(@"..\..\..\CTeOS\Resources\CTeOS_ModalRodoOS.xml")]
        [InlineData(@"..\..\..\CTeOS\Resources\4_00_CTeOS_ModalRodoOS.xml")]
        public void SerializacaoDesserializacaoCTeOS(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Unimake.Business.DFe.Xml.CTeOS.CTeOS();
            xml = xml.LerXML<Unimake.Business.DFe.Xml.CTeOS.CTeOS>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar os campos de antecipação, SUFRAMA e CBS adicionados ao leiaute 4.00.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTeOS")]
        public void SerializacaoDesserializacaoCTeOSNovosCamposRTC()
        {
            const string arqXML = @"..\..\..\CTeOS\Resources\4_00_CTeOS_ModalRodoOS.xml";
            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = Business.DFe.Utility.XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTeOS.CTeOS>(doc);

            Assert.Equal(TipoPagamentoAntecipadoCTe.FornecimentoPagamentoRealizadoAnteriormente, xml.InfCTe.Ide.TpPagAnt);
            Assert.Equal(2, xml.InfCTe.Ide.GPagAntecipado.ChDFePagAnt.Count);
            Assert.Equal("12345678", xml.InfCTe.Emit.ISUFEmit);
            Assert.Equal(10d, xml.InfCTe.Imp.IBSCBS.GIBSCBS.GCBS.GDevTrib.PDevTrib);
            Assert.Equal(2.5d, xml.InfCTe.Imp.IBSCBS.GIBSCBS.GCBS.GALCZFMCBS.PAliqEfetRegCBS);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CteOSProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(@"..\..\..\CTeOS\Resources\cteOSProc.xml")]
        public void SerializacaoDesserializacaoCTeOSProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Business.DFe.Xml.CTeOS.CteOSProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
