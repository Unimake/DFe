using System.IO;
using System.Xml;
using Unimake.Business.DFe.Utility;
using Xunit;

namespace Unimake.DFe.Test.NFCom
{
    public class SerializacaoDesserializacaoTest
    {
        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\nfcom_meio_completa-nfcom.xml")]
        [InlineData(@"..\..\..\NFCom\Resources\nfcom_completa-nfcom.xml")]
        public void SerializacaoDesserializacaoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.NFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.NFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\nfcom_completa_rtc.xml")]
        public void SerializacaoDesserializacaoNFComRTCNovosCampos(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.NFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.NFCom>(doc);

            Assert.NotNull(xml.InfNFCom.PgtoVinc);
            Assert.Equal("12345678", xml.InfNFCom.Emit.ISUFEmit);
            Assert.Equal("06117473000150", xml.InfNFCom.Det[0].Prod.CNPJCobrTerc);
            Assert.NotNull(xml.InfNFCom.Det[0].Prod.GPagAntecipado);
            Assert.Equal(Unimake.Business.DFe.Servicos.IndicadorDoacao.OperacaoDoacao, xml.InfNFCom.Det[0].Imposto.IBSCBS.IndDoacao);
            Assert.Equal("03", xml.InfNFCom.PgtoVinc.Pgto[0].TpMeioPgto);
            Assert.NotNull(xml.InfNFCom.Det[0].Imposto.IBSCBS.GIBSCBS.GCBS.GALCZFMCBS);

            var doc2 = xml.GerarXML();

            Assert.True(doc2.GetElementsByTagName("tpPagAnt").Count > 0);
            Assert.True(doc2.GetElementsByTagName("ISUFEmit").Count > 0);
            Assert.True(doc2.GetElementsByTagName("CNPJCobrTerc").Count > 0);
            Assert.True(doc2.GetElementsByTagName("gPagAntecipado").Count > 0);
            Assert.True(doc2.GetElementsByTagName("indDoacao").Count > 0);
            Assert.True(doc2.GetElementsByTagName("pDevTrib").Count > 0);
            Assert.True(doc2.GetElementsByTagName("gALCZFMCBS").Count > 0);
            Assert.True(doc2.GetElementsByTagName("pgtoVinc").Count > 0);
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retNFCom.xml")]
        public void SerializacaoDesserializacaoRetNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, $"XML gerado pela DLL está diferente do conteúdo do arquivo serializado.\nOriginal: {doc.InnerText}\nGerado: {doc2.InnerText}");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\protNFCom.xml")]
        public void SerializacaoDesserializacaoProtNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ProtNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ProtNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\eventoNFCom-110111.xml")]
        [InlineData(@"..\..\..\NFCom\Resources\eventoNFCom-110300.xml")]
        [InlineData(@"..\..\..\NFCom\Resources\eventoNFCom-110301.xml")]
        public void SerializacaoDesserializacaoEventoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.EventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.EventoNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retEventoNFCom.xml")]
        public void SerializacaoDesserializacaoRetEventoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetEventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetEventoNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\procEventoNFCom.xml")]
        public void SerializacaoDesserializacaoProcEventoNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ProcEventoNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ProcEventoNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retConsSitNFCom.xml")]
        public void SerializacaoDesserializacaoRetConsSitNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetConsSitNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetConsSitNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\consSitNFCom-ped-sit.xml")]
        public void SerializacaoDesserializacaoConsSitNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ConsSitNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ConsSitNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\NFComProc.xml")]
        public void SerializacaoDesserializacaoNFComProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.NFComProc();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.NFComProc>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\consStatServNFCom-ped-sta.xml")]
        public void SerializacaoDesserializacaoConsStatServNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.ConsStatServNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        [Theory]
        [Trait("DFe", "NFCom")]
        [InlineData(@"..\..\..\NFCom\Resources\retConsStatServNFCom.xml")]
        public void SerializacaoDesserializacaoRetConsStatServNFCom(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var nfCom = new Unimake.Business.DFe.Xml.NFCom.RetConsStatServNFCom();
            var xml = nfCom.LerXML<Unimake.Business.DFe.Xml.NFCom.RetConsStatServNFCom>(doc);

            var doc2 = xml.GerarXML();

            Assert.True(doc.InnerText == doc2.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
