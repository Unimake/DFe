using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class SerializacaoDesserializacaoConsultasTest
    {
        /// <summary>
        /// Testar a serialização e desserialização da consulta de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsTabela-esocial-considevt.xml")]
        public void SerializacaoDesserializacaoConsultaTabelaESocial(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTabelaESocial>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização da consulta de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsEmpregador-esocial-considevt.xml")]
        public void SerializacaoDesserializacaoConsultaEmpregadorESocial(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsEmpregadorESocial>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização da consulta de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaEvtsTrabalhador-esocial-considevt.xml")]
        public void SerializacaoDesserializacaoConsultaTrabalhadorESocial(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarEvtsTrabalhadorESocial>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização da consulta de lotes de Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\ConsultaLoteEventos-esocial-consloteevt.xml")]
        public void SerializacaoDesserializacaoConsultaLoteEventosESocial(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ConsultarLoteEventos>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
