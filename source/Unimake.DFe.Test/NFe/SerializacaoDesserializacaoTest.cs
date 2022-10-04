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
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\enviNFe.xml")]
        public void SerializacaoDesserializacaoEnviNFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

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
        /// Testar a serialização e desserialização do XML NfeProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\99999999999999999999999999999999999999999999-procNFe.xml")]
        public void SerializacaoDesserializacaoNfeProc(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new NfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ResEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resEvento.xml")]
        public void SerializacaoDesserializacaoResEvento(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResEvento>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ResNFe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\resNFe.xml")]
        public void SerializacaoDesserializacaoResNFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResNFe>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnvEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\EnvEventoEPEC.xml")]
        public void SerializacaoDesserializacaoEnvEvento(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EnvEvento>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

        }

        /// <summary>
        /// Testar a serialização e desserialização do XML DistDFeInt
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ChNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CNPJ.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_CPF.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_ComUFAutor.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_NSU.xml")]
        [InlineData(@"..\..\..\NFe\Resources\DistDFeInt_SemUFAutor.xml")]
        public void SerializacaoDesserializacaoDistDFeInt(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<DistDFeInt>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
        
        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsSitNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe2.xml")] //XML retornado pela SEFAZ MG com falha na tag Signature, vou manter este teste, só para garantir o funcionamento para este estado. Eles estão fugindo o padrão.
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe3.xml")] //XML retornado pela SEFAZ MG com falha na tag Signature, vou manter este teste, só para garantir o funcionamento para este estado. Eles estão fugindo o padrão.
        public void SerializacaoDesserializacaoRetConsSitNFe(string arqXML)
        {
            Diag.Debug.Assert(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitNFe>(doc);
            var doc2 = xml.GerarXML();

            Diag.Debug.Assert(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}