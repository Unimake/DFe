using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do Evento 1000 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\1000_evtInfoContri-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf1000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf1000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1050 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\1050_evtTabLig-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf1050(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf1050>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 1070 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\1070_evtTabProcesso-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf1070(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf1070>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2010 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2010_evtServTom-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2010(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2010>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2020 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2020_evtServPrest-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2020(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2020>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2030 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2030_evtAssocDespRec-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2030(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2030>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2040 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2040_evtAssocDespRep-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2040(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2040>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2050 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2050_evtComProd-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2050(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2050>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2055 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2055_evtAqProd-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2055(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2055>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2060 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2060_evtCPRB-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2060(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2060>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2098 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2098_evtReabreEvPer-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2098(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2098>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 2099 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\2099_evtFechaEvPer-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf2099(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf2099>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 3010 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\3010_evtEspDesportivo-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf3010(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf3010>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 4010 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\4010_evtRetPF-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf4010(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf4010>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 4020 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\4020_evtRetPJ-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf4020(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf4020>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 4040 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\4040_evtBenefNId-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf4040(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf4040>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 4080 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\4080_evtRetRec-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf4080(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf4080>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 4099 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\4099_evtFech-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf4099(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf4099>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 9000 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\9000_evtExclusao-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf9000(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf9000>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 9001 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\9001_evtTotal-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf9001(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf9001>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 9005 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\9005_evtRet-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf9005(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf9005>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 9011 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\9011_evtTotalContrib-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf9011(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf9011>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do Evento 9015 Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\9015_evtRetCons-Reinf-evt.xml")]
        public void SerializacaoDesserializacaoReinf9015(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<Reinf9015>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização das consultas Evento Reinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S1000-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S1070-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2010-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2020-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2030-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2040-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2050-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2055-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2060-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2098-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S2099-reinf-cons.xml")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaReciboEvento_S3010-reinf-cons.xml")]
        public void SerializacaoDesserializacaoConsultasReinf(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<ReinfConsultas>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização da Consulta Resultado Fechamento 2099 EFDReinf.
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaResultadoFechamento2099-reinf-cons.xml")]
        public void SerializacaoDesserializacaoFechamento2099(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<ReinfConsultaFechamento2099>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

        }

        /// <summary>
        /// Testar a serialização e desserialização da Consulta Resultado Fechamento 2099 EFDReinf.
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\ConsultaLoteAssincrono-Reinf-consloteevt.xml")]
        public void SerializacaoDesserializacaoConsultaLoteAssincronoReinf(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<ReinfConsultaLoteAssincrono>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

        }

        /// <summary>
        /// Testar a serialização e desserialização para o Envio lote Assincrono EFDReinf.
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\loteEventosAssincrono-Reinf-loteevt.xml")]
        public void SerializacaoDesserializacaoReinfAssincrono(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = XMLUtility.Deserializar<ReinfEnvioLoteEventos>(doc);
            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}