using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do CTe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviCTe
        /// </summary>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalAereo.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalDutoviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalMultiModal.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalRodoviario.xml")]
        public void SerializacaoDesserializacaoEnviCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new EnviCTe();
            xml = xml.LerXML<EnviCTe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Unimake.Business.DFe.Servicos.CTe.Autorizacao(xml, configuracao);

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnviCTe
        /// </summary>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalAereo.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalDutoviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalMultiModal.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml")]
        public void SerializacaoDesserializacaoCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Unimake.Business.DFe.Xml.CTe.CTe();
            xml = xml.LerXML<Unimake.Business.DFe.Xml.CTe.CTe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Unimake.Business.DFe.Servicos.CTe.AutorizacaoSinc(xml, configuracao);

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }


        /// <summary>
        /// Testar a serialização e desserialização do XML CTeOS
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(@"..\..\..\CTe\Resources\CTeOS_ModalRodoOS.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTeOS_ModalRodoOS.xml")]
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

            var autorizacao = new Unimake.Business.DFe.Servicos.CTeOS.Autorizacao(xml, configuracao);

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CteProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\99999999999999999999999999999999999999999999-procCTe.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_99999999999999999999999999999999999999999999-procCTe.xml")]
        public void SerializacaoDesserializacaoCTeProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new CteProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML retConsSitCTe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_1.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_retConsSitCTe.xml")]
        public void SerializacaoDesserializacaoRetConsSitCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitCTe>(doc);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EventoCTe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110180.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110190.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110191.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_610111.xml")]
        public void SerializacaoDesserializacaoEventoCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EventoCTe>(doc);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}