using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFe;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFCe
{
    /// <summary>
    /// Testar serialização e deserialização do XML de download de NFCe
    /// </summary>
    public class NFCeDownloadXMLTest
    {
        /// <summary>
        /// Testar a deserialização e serialização do XML NFCeDownloadXML
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFCe\NFCe SP\NFCeDownloadXML.xml")]
        public void SerializarDeserializarNFCeDownloadXML(string arqXML)
        {
            Assert.True(File.Exists(arqXML), $"Arquivo {arqXML} não encontrado.");

            var conteudoXML = File.ReadAllText(arqXML);
            var xml = XMLUtility.Deserializar<NFCeDownloadXML>(conteudoXML);

            Assert.NotNull(xml);
            Assert.Equal("1.00", xml.Versao);
            Assert.Equal(TipoAmbiente.Homologacao, xml.TpAmb);
            Assert.Equal("12345678901234567890123456789012345678901234", xml.ChNFCe);

            var xmlDoc = xml.GerarXML();
            Assert.NotNull(xmlDoc);

            var xmlString = xmlDoc.OuterXml;
            Assert.Contains("nfceDownloadXML", xmlString);
            Assert.Contains("versao=\"1.00\"", xmlString);
            Assert.Contains("<tpAmb>2</tpAmb>", xmlString); 
            Assert.Contains("<chNFCe>12345678901234567890123456789012345678901234", xmlString);
        }
    }
}
