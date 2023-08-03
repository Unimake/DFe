using System.IO;
using System.Xml;
using Unimake.Business.DFe.Xml.SNCM;
using Xunit;

namespace Unimake.DFe.Test.SNCM
{
    /// <summary>
    /// Testar a serialização e deserialização dos XMLs do SNCM
    /// </summary>
    public class SerializacaoDeserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e deserialização do XML MsgParam
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\msgParam-consparam.xml")]
        public void SerializacaoDeserializacaoMsgParam(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MsgParam();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgParam
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\retMbtAgtMgmt.xml")]
        public void SerializacaoDeserializacaoRetMbtAgtMgmt(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetMbtAgtMgmt();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgParam
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\retEvtIn.xml")]
        public void SerializacaoDeserializacaoRetEvtIn(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetEvtIn();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgMbtAgtMgmt
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\msgMbtAgtMgmt.xml")]
        public void SerializacaoDeserializacaoMsgMbtAgtMgmt(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MsgMbtAgtMgmt();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML RetMdataMAH
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\retMdataMAH.xml")]
        public void SerializacaoDeserializacaoRetMdataMAH(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetMdataMAH();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgMdataMAH
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\msgMdataMAH.xml")]
        public void SerializacaoDeserializacaoMsgMdataMAH(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MsgMdataMAH();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgMdataMAH
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\retParam.xml")]
        public void SerializacaoDeserializacaoRetParam(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetParam();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML MsgEvtIn
        /// </summary>
        [Theory]
        [Trait("DFe", "SNCM")]
        [InlineData(@"..\..\..\SNCM\Resources\msgEvtIn-RegEvento.xml")]
        public void SerializacaoDeserializacaoMsgEvtIn(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new MsgEvtIn();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}