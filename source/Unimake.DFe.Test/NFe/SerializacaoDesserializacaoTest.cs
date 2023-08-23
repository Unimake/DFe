using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

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
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

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

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML NFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe"), Trait("DFe", "NFCe")]
        [InlineData(@"..\..\..\NFe\Resources\NFe1.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe2.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe3.xml")]
        [InlineData(@"..\..\..\NFe\Resources\NFe4.xml")]
        public void SerializacaoDesserializacaoNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var enviNFe = new EnviNFe
            {
                IdLote = "000000000000001",
                Versao = "4.00",
                NFe = new System.Collections.Generic.List<Business.DFe.Xml.NFe.NFe>
            {
                XMLUtility.Deserializar<Business.DFe.Xml.NFe.NFe>(doc.OuterXml)
            }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Business.DFe.Servicos.NFe.Autorizacao(enviNFe, configuracao);

            Assert.True(doc.InnerText == autorizacao.ConteudoXMLOriginal.GetElementsByTagName("NFe")[0].InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
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
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new NfeProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
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
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResEvento>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
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
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ResNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnvEvento
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110140.xml")]
        [InlineData(@"..\..\..\NFe\Resources\envEvento_110130.xml")]
        public void SerializacaoDesserializacaoEnvEvento(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EnvEvento>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
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
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<DistDFeInt>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsSitNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe.xml")]
        [InlineData(@"..\..\..\NFe\Resources\retConsSitNFe2.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML procEventoNFe
        /// </summary>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_610130.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_610131.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_790700.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_630690.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_990900.xml")]
        [InlineData(@"..\..\..\NFe\Resources\procEventoNFe_990910.xml")]
        public void SerializacaoDesserializacaoProcEventoNFe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ProcEventoNFe>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}