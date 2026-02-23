using System;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFCe;
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

        /// <summary>
        /// Testar a comunicação com o serviço de download de NFCe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao, "35240906117473000150650010000000011000000001")]
        public void ConsultarDownloadXMLNFCe(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string chaveNFCe)
        {
            var xml = new NFCeDownloadXML
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                ChNFCe = chaveNFCe
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoUF = (int)ufBrasil
            };

            var downloadXML = new DownloadXML(xml, configuracao);
            downloadXML.Executar();

            Assert.NotNull(downloadXML.Result);
            Assert.NotNull(downloadXML.Result.CStat);
            Assert.NotNull(downloadXML.Result.XMotivo);
            Assert.Equal(tipoAmbiente, downloadXML.Result.TpAmb);
            
            Console.WriteLine($"Status: {downloadXML.Result.CStat} - {downloadXML.Result.XMotivo}");
        }

        /// <summary>
        /// Testar a comunicação com o serviço usando string XML
        /// </summary>
        [Theory]
        [Trait("DFe", "NFCe")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao, "35240906117473000150650010000000011000000001")]
        public void ConsultarDownloadXMLNFCeComString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string chaveNFCe)
        {
            var xml = new NFCeDownloadXML
            {
                Versao = "1.00",
                TpAmb = tipoAmbiente,
                ChNFCe = chaveNFCe
            };

            var xmlString = xml.GerarXML().OuterXml;

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFCe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                CodigoUF = (int)ufBrasil
            };

            var downloadXML = new DownloadXML(xmlString, configuracao);
            downloadXML.Executar();

            Assert.NotNull(downloadXML.Result);
            Assert.NotNull(downloadXML.Result.CStat);
            Assert.Equal(tipoAmbiente, downloadXML.Result.TpAmb);
            
            Console.WriteLine($"Status: {downloadXML.Result.CStat} - {downloadXML.Result.XMotivo}");
        }
    }
}
