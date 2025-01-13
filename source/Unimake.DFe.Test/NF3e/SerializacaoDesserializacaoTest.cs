using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.NF3e;
using Xunit;

namespace Unimake.DFe.Test.NF3e
{
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML NF3e
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\nota_energia-nf3e.xml")]
        public void SerializacaoDesserializacaoNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.NF3e>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML NF3e
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\consStatServ-ped-sta.xml")]
        public void SerializacaoDesserializacaoNF3eConsStatServ(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.NF3e.ConsStatServ>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ConsSitNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\consSitNF3e.xml")]
        public void SerializacaoDesserializacaoConsSitNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ConsSitNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsSitNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\retConsSitNF3e.xml")]
        public void SerializacaoDesserializacaoRetConsSitNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ConsReciNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\consReciNF3e.xml")]
        public void SerializacaoDesserializacaoConsReciNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ConsReciNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsReciNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\retConsReciNF3e.xml")]
        public void SerializacaoDesserializacaoRetConsReciNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsReciNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML RetConsStatServNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\retConsStatServNF3e.xml")]
        public void SerializacaoDesserializacaoRetConsStatServNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsStatServNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ProtNF3e
        /// </summary>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\protNF3e.xml")]
        public void SerializacaoDesserializacaoProtNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<ProtNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML NF3eProc
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\procNF3e.xml")]
        public void SerializacaoDeserializacaoNF3eProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<NF3eProc>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML NF3eProc
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\retNF3e.xml")]
        public void SerializacaoDeserializacaoRetNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

        }
        
        /// <summary>
        /// Testar a serialização e deserialização do XML retEventoNF3e
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\retEventoNF3e.xml")]
        public void SerializacaoDeserializacaoRetEventoNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetEventoNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
        
        /// <summary>
        /// Testar a serialização e deserialização do XML Evento Cancelamento
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\eventoCancNF3e.xml")]
        public void SerializacaoDeserializacaoEventoCancNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EvCancNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML Evento Cancelamento
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(@"..\..\..\NF3e\Resources\eventoNF3e.xml")]
        public void SerializacaoDeserializacaoEventoNF3e(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/deserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EventoNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
