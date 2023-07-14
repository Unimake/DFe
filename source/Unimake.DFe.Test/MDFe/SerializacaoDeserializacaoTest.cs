using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Xunit;

namespace Unimake.DFe.Test.MDFe
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do MDFe
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviMDFe
        /// </summary>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalAereo.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\enviMDFe_ModalRodoviario.xml")]
        public void SerializacaoDeserializacaoEnviMDFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

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

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }


        /// <summary>
        /// Testar a serialização e desserialização do XML MdfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\99999999999999999999999999999999999999999999-procMDFe.xml")]
        public void SerializacaoDeserializacaoMdfeProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MdfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML MdfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\EventoMDFePagamentoOperacaoMDFe.xml")]
        public void SerializacaoDeserializacaoEventoMDFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EventoMDFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML retConsSitMDFe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\retConsSitMDFe-com-evento-de-cancelamento.xml")]
        [InlineData(@"..\..\..\MDFe\Resources\retConsSitMDFe-com-evento-de-encerramento.xml")]
        public void SerializacaoDeserializacaoRetConsSitMDFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitMDFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML retConsSitMDFe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "MDFe")]
        [InlineData(@"..\..\..\MDFe\Resources\procEventoMDFe_Encerramento_110112_01.xml")]
        public void SerializacaoDeserializacaoProcEventoMDFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ProcEventoMDFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

    }
}