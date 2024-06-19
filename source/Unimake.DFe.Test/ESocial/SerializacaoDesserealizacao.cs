using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserealizacao
    {
        /// <summary>
        /// Testar a serialização e desserialização do Evento 1000 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1000_evtInfoEmpregador-esocial-evt .xml")]
        public void SerializacaoDesserializacaoESocial1000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1020 ESocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1005_evtTabEstab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1005(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1005>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1010 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1010_evtTabRubrica-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1010(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1010>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1020 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1020_evtTabLotacao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1020(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1020>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1070 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1070_evtTabProcesso-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1070(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1070>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1200_evtRemun-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1200(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1200>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1202_evtRmnRPPS-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1202(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1202>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1207 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1207_evtBenPrRP-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1207(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1207>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1210 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1210_evtPgtos-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1210(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1210>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1260 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1260_evtComProd-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1260(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1260>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1270 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1270_evtContratAvNP-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1270(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1270>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1280 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1280_evtInfoComplPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1280(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1280>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1298 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1298_evtReabreEvPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1298(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1298>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }


        /// <summary>
        /// Testar a serialização e desserialização do Evento 1299 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\1299_evtFechaEvPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1299(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial1299>(doc);
            var doc2 = xml.GerarXML();

             Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2190 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2190_evtAdmPrelim-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2190(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2190>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2200_evtAdmissao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2200(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2200>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2205 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2205_evtAltCadastral-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2205(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2205>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2206 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2206_evtAltContratual-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2206(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2206>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2210 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2210_evtCAT-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2210(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2210>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2220 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2220_evtMonit-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2220(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2220>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2230 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2230_evtAfastTemp-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2230(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2230>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2231 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2231_evtCessao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2231(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2231>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2240 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2240_evtExpRisco-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2240(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2240>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2298 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2298_evtReintegr-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2298(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2298>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2299 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\2299_evtDeslig-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2299(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.ESocial.ESocial2299>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
