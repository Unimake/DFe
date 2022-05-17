using Diag = System.Diagnostics;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.MDFe;
using Xunit;
using Unimake.Business.DFe.Utility;

namespace Unimake.DFe.Test.MDFe
{
    /// <summary>
    /// Testar a serialização e deserialização dos XMLs do MDFe
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e deserialização do XML EnviMDFe
        /// </summary>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalAereo.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalRodoviario.xml")]
        public void SerializacaoDeserializacaoEnviMDFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new EnviMDFe();
            xml = xml.LerXML<EnviMDFe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.MDFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Unimake.Business.DFe.Servicos.MDFe.Autorizacao(xml, configuracao);

            Diag.Debug.Assert(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }


        /// <summary>
        /// Testar a serialização e deserialização do XML MdfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\99999999999999999999999999999999999999999999-procMDFe.xml")]
        public void SerializacaoDeserializacaoMdfeProc(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MdfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MdfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\EventoMDFePagamentoOperacaoMDFe.xml")]
        public void SerializacaoDeserializacaoEventoMDFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EventoMDFe>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}