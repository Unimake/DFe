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

            var nf3e = new Unimake.Business.DFe.Xml.NF3e.NF3e();
            var xml = nf3e.LerXML<Unimake.Business.DFe.Xml.NF3e.NF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e deserialização do XML RetNF3e
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

            var retornoNF3e = new RetNF3e();
            var xml = retornoNF3e.LerXML<RetNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML ConsStatServ
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

            var consultaStatus = new ConsStatServNF3e();
            var xml = consultaStatus.LerXML<ConsStatServNF3e>(doc);

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

            var retornoConsultaStatus = new RetConsStatServNF3e();
            var xml = retornoConsultaStatus.LerXML<RetConsStatServNF3e>(doc);

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

            var consultaSituacao = new ConsSitNF3e();
            var xml = consultaSituacao.LerXML<ConsSitNF3e>(doc);

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

            var retornoConsultaSituacao = new RetConsSitNF3e();
            var xml = retornoConsultaSituacao.LerXML<RetConsSitNF3e>(doc);

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

            var consultaRecibo = new ConsReciNF3e();
            var xml = consultaRecibo.LerXML<ConsReciNF3e>(doc);

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

            var retornoConsultaRecibo = new RetConsReciNF3e();
            var xml = retornoConsultaRecibo.LerXML<RetConsReciNF3e>(doc);

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

            var protocoloNF3e = new ProtNF3e();
            var xml = protocoloNF3e.LerXML<ProtNF3e>(doc);

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

            var procNF3e = new NF3eProc();
            var xml = procNF3e.LerXML<NF3eProc>(doc);

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

            var eventoNF3e = new EventoNF3e();
            var xml = eventoNF3e.LerXML<EventoNF3e>(doc);
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

            var retornoEvento = new RetEventoNF3e();
            var xml = retornoEvento.LerXML<RetEventoNF3e>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
