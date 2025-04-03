using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserealizacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do Evento 1000 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1000_evtInfoEmpregador-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1000();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1020 ESocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1005_evtTabEstab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1005(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1005();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1005>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1010 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1010_evtTabRubrica-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1010(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1010();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1010>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1020 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1020_evtTabLotacao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1020(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1020();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1020>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1070 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "eSocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1070_evtTabProcesso-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1070(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1070();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1070>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1200_evtRemun-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1200(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1200();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1200>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1202 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1202_evtRmnRPPS-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1202(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1202();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1202>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1207 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1207_evtBenPrRP-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1207(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1207();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1207>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1210 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1210_evtPgtos-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1210(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1210();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1210>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1260 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1260_evtComProd-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1260(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1260();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1260>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1270 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1270_evtContratAvNP-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1270(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1270();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1270>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1280 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1280_evtInfoComplPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1280(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1280();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1280>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1298 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1298_evtReabreEvPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1298(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1298();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1298>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }


        /// <summary>
        /// Testar a serialização e desserialização do Evento 1299 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\1299_evtFechaEvPer-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial1299(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial1299();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial1299>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2190 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2190_evtAdmPrelim-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2190(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2190();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2190>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2200_evtAdmissao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2200(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2200();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2200>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2205 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2205_evtAltCadastral-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2205(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2205();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2205>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2206 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2206_evtAltContratual-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2206(string arqXML    )
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2206();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2206>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2210 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2210_evtCAT-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2210(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2210();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2210>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2220 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2220_evtMonit-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2220(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2220();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2220>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2220 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2221_evtToxic-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2221(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2221();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2221>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2230 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2230_evtAfastTemp-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2230(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2230();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2230>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2231 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2231_evtCessao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2231(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2231();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2231>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2240 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2240_evtExpRisco-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2240(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2240();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2240>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2298 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2298_evtReintegr-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2298(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2298();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2298>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2299 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2299_evtDeslig-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2299(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2299();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2299>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2300 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2300_evtTSVInicio-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2300(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2300();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2300>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2306 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2306_evtTSVAltContr-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2306(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2306();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2306>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2400 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2399_evtTSVTermino-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2399(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2399();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2399>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2400 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2400_evtCdBenefIn-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2400(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2400();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2400>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2400 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2405_evtCdBenefAlt-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2405(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2405();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2405>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2410 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2410_evtCdBenIn-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2410(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2410();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2410>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2416 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2416_evtCdBenAlt-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2416(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2416();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2416>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2418 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2418_evtReativBen-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2418(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2418();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2418>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2420 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2420_evtCdBenTerm-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2420(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2420();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2420>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2500 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2500_evtProcTrab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2500(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2500();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2500>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2501 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2501_evtContProc-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2501(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2501();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2501>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2501 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\2555_evtConsolidContProc-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial2555(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial2555();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial2555>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 3000 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\3000_evtExclusao-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial3000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial3000();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial3000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 3500 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\3500_evtExcProcTrab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial3500(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial3500();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial3500>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5001 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5001_evtBasesTrab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5001(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5001();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5001>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5002 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5002_evtIrrfBenef-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5002(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5002();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5002>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5003 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5003_evtBasesFGTS-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5003(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5003();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5003>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5011 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5011_evtCS-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5011(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5011();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5011>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5012 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5012_evtIrrf-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5012(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5012();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5012>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5013 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5013_evtFGTS-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5013(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5013();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5013>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5501 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5501_evtTribProcTrab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5501(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5501();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5501>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 5503 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\5503-evtFGTSProcTrab-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial5503(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial5503();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial5503>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 8200 eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\S_01_03_00\8200_evtAnotJud-esocial-evt.xml")]
        public void SerializacaoDesserializacaoESocial8200(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocial8200();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocial8200>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnvioLoteEventos
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\EnvioLoteEventos-esocial-loteevt.xml")]
        public void SerializacaoDesserializacaoESocialEnvioLoteEventos(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnvioLoteEventos
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\retorno_erro_protocolo-ret-esocial-loteevt.xml")]
        [InlineData(@"..\..\..\ESocial\Resources\retorno_positivo_protocolo-ret-esocial-loteevt.xml")]
        public void SerializacaoDesserializacaoESocialRetornoEnvioLote(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLote();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.Retorno.RetornoEnvioLote>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do retorno negativo da consulta lote Eventos eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(@"..\..\..\ESocial\Resources\retorno_negativo-ret-esocial-consloteevt.xml")]
        [InlineData(@"..\..\..\ESocial\Resources\retornoProcessamentoLoteEventos1.xml")]
        [InlineData(@"..\..\..\ESocial\Resources\retornoProcessamentoLoteEventos_tots.xml")]
        public void SerializacaoDesserializacaoRetornoProcessamentoLoteEventos(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var eSocial = new Business.DFe.Xml.ESocial.Retorno.RetornoEventoProcessado();
            var xml = eSocial.LerXML<Business.DFe.Xml.ESocial.Retorno.RetornoEventoProcessado>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}