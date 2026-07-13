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
