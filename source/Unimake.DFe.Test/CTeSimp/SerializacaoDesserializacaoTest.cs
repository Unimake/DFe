using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.CTeSimp;
using Xunit;

namespace Unimake.DFe.Test.CTeSimp
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do NFe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML retCteSimp
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\retCteSimp.xml")]
        public void SerializacaoDesserializacaoRetCteSimp(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new RetCTeSimp();
            xml = xml.LerXML<RetCTeSimp>(doc);

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CTeSimp
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\CTeSimp.xml")]
        public void SerializacaoDesserializacaoCTeSimp(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Business.DFe.Xml.CTeSimp.CTeSimp();
            xml = xml.LerXML<Business.DFe.Xml.CTeSimp.CTeSimp>(doc);

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CTeSimp com emissão pelo PAA.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTeSimp")]
        public void SerializacaoDesserializacaoCTeSimpProcEmiPAA()
        {
            const string arqXML = @"..\..\..\CTeSimp\Resources\CTeSimp.xml";
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);
            doc.GetElementsByTagName("procEmi")[0].InnerText = "4";

            var xml = new Business.DFe.Xml.CTeSimp.CTeSimp();
            xml = xml.LerXML<Business.DFe.Xml.CTeSimp.CTeSimp>(doc);

            Assert.Equal(ProcessoEmissao.ProvedorAutorizacaoAssinatura, xml.InfCTe.Ide.ProcEmi);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização do XML cteSimpProc
        /// </summary>
        [Theory]
        [Trait("DFe", "CTeSimp")]
        [InlineData(@"..\..\..\CTeSimp\Resources\CTeSimp.xml", @"..\..\..\CTeSimp\Resources\retCteSimp.xml")]
        public void SerializacaoCteSimpProc(string arqCTeSimp, string arqRetCteSimp)
        {
            Assert.True(File.Exists(arqCTeSimp), "Arquivo " + arqCTeSimp + " não foi localizado para a realização da serialização.");
            Assert.True(File.Exists(arqRetCteSimp), "Arquivo " + arqRetCteSimp + " não foi localizado para a realização da serialização.");

            var docCTeSimp = new XmlDocument();
            docCTeSimp.Load(arqCTeSimp);

            var docRetCteSimp = new XmlDocument();
            docRetCteSimp.Load(arqRetCteSimp);

            var cteSimp = new Business.DFe.Xml.CTeSimp.CTeSimp().LerXML<Business.DFe.Xml.CTeSimp.CTeSimp>(docCTeSimp);
            var retCteSimp = new RetCTeSimp().LerXML<RetCTeSimp>(docRetCteSimp);
            var cteSimpProc = new CteSimpProc
            {
                Versao = cteSimp.InfCTe.Versao,
                CTeSimp = cteSimp,
                ProtCTe = retCteSimp.ProtCTe
            };

            var xml = cteSimpProc.GerarXML();

            Assert.Equal(1, xml.GetElementsByTagName("cteSimpProc").Count);
            Assert.Equal(1, xml.GetElementsByTagName("CTeSimp").Count);
            Assert.Equal(1, xml.GetElementsByTagName("protCTe").Count);
        }
    }
}
