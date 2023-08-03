using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML procEventoNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\1000_evtInfoContri-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf1000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf1000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}