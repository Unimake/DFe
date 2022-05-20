using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;
using Diag = System.Diagnostics;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar a serialização e deserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e deserialização do XML EnviNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\enviNFe.xml")]
        public void SerializacaoDeserializacaoEnviNFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new EnviNFe();
            xml = xml.LerXML<EnviNFe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Business.DFe.Servicos.NFe.Autorizacao(xml, configuracao);

            Diag.Debug.Assert(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML NfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml")]
        public void SerializacaoDeserializacaoNfeProc(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new NfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML ResEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resEvento.xml")]
        public void SerializacaoDeserializacaoResEvento(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResEvento>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML ResNFe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resNFe.xml")]
        public void SerializacaoDeserializacaoResNFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResNFe>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML EnvEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\EnvEventoEPEC.xml")]
        public void SerializacaoDeserializacaoEnvEvento(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EnvEvento>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

        }

        /// <summary>
        /// Testar a serialização e deserialização do XML DistDFeInt
        /// </summary>
        /// <param name="arqXML">Arquivo a ser deserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ChNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CNPJ.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CPF.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ComUFAutor.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_NSU.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_SemUFAutor.xml")]
        public void SerializacaoDeserializacaoDistDFeInt(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<DistDFeInt>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}