using Diag = System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CTe;
using Unimake.Business.DFe.Xml.CTeOS;
using Xunit;
using Unimake.Business.DFe.Utility;

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
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

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

            Diag.Debug.Assert(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CTeOS
        /// </summary>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\CTeOS_ModalRodoOS.xml")]
        public void SerializacaoDesserializacaoCTeOS(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new CTeOS();
            xml = xml.LerXML<CTeOS>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Unimake.Business.DFe.Servicos.CTeOS.Autorizacao(xml, configuracao);

            Diag.Debug.Assert(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CteProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\99999999999999999999999999999999999999999999-procCTe.xml")]
        public void SerializacaoDesserializacaoCTeProc(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new CteProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML retConsSitCTe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe.xml")]
        public void SerializacaoDesserializacaoRetConsSitCTe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitCTe>(doc);
            var xmlSerializado = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}