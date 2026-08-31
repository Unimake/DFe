using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe.Serializacao
{
    /// <summary>
    /// Testar a serialização e desserialização dos XMLs do CTe
    /// </summary>
    public class SerializacaoDesserializacaoTest
    {
        /// <summary>
        /// Testar a serialização e desserialização do XML EnviCTe
        /// </summary>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalAereo.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalDutoviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalMultiModal.xml")]
        [InlineData(@"..\..\..\CTe\Resources\enviCTe_ModalRodoviario.xml")]
        public void SerializacaoDesserializacaoEnviCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new EnviCTe();
            xml = xml.LerXML<EnviCTe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EnviCTe
        /// </summary>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalAereo.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalAquaviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalDutoviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalFerroviario.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalMultiModal.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml")]
        public void SerializacaoDesserializacaoCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new Unimake.Business.DFe.Xml.CTe.CTe();
            xml = xml.LerXML<Unimake.Business.DFe.Xml.CTe.CTe>(doc);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CTe com emissão pelo PAA.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTe")]
        public void SerializacaoDesserializacaoCTeProcEmiPAA()
        {
            const string arqXML = @"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml";
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);
            doc.GetElementsByTagName("procEmi")[0].InnerText = "4";

            var xml = new Unimake.Business.DFe.Xml.CTe.CTe();
            xml = xml.LerXML<Unimake.Business.DFe.Xml.CTe.CTe>(doc);

            Assert.Equal(ProcessoEmissao.ProvedorAutorizacaoAssinatura, xml.InfCTe.Ide.ProcEmi);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do grupo de compras governamentais.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTe")]
        public void SerializacaoDesserializacaoCTeGCompraGov()
        {
            const string arqXML = @"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml";
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(doc);
            var gCompraGov = xml.InfCTe.Ide.GCompraGov;

            Assert.Equal(TipoEnteGovernamental.Uniao, gCompraGov.TpEnteGov);
            Assert.Equal(1d, gCompraGov.PRedutor);
            Assert.Equal(TipoOperacaoEnteGovernamental.FornecimentoPagamentoJaRealizado, gCompraGov.TpOperGov);
            Assert.Equal(2, gCompraGov.RefDFeAnt.Count);
            Assert.Equal("99999999999999999999999999999999999999999999", gCompraGov.RefDFeAnt[0]);
            Assert.Equal("88888888888888888888888888888888888888888888", gCompraGov.RefDFeAnt[1]);
            Assert.Equal(IndicadorDoacao.OperacaoDoacao, xml.InfCTe.Imp.IBSCBS.IndDoacao);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar os campos de antecipação, SUFRAMA e CBS adicionados ao leiaute 4.00.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTe")]
        public void SerializacaoDesserializacaoCTeNovosCamposRTC()
        {
            const string arqXML = @"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml";
            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(doc);

            Assert.Equal(TipoPagamentoAntecipadoCTe.FornecimentoPagamentoRealizadoAnteriormente, xml.InfCTe.Ide.TpPagAnt);
            Assert.Equal(2, xml.InfCTe.Ide.GPagAntecipado.ChDFePagAnt.Count);
            Assert.Equal("12345678", xml.InfCTe.Emit.ISUFEmit);
            Assert.Equal(10d, xml.InfCTe.Imp.IBSCBS.GIBSCBS.GCBS.GDevTrib.PDevTrib);
            Assert.Equal(2.5d, xml.InfCTe.Imp.IBSCBS.GIBSCBS.GCBS.GALCZFMCBS.PAliqEfetRegCBS);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");

            xml.InfCTe.Imp.IBSCBS.GIBSCBS.GCBS.GDevTrib.PDevTrib = 0;
            Assert.Equal("0.0000", xml.GerarXML().GetElementsByTagName("pDevTrib")[0].InnerText);
        }

        /// <summary>
        /// Testar a serialização e desserialização da assinatura do Provedor de Assinatura e Autorização.
        /// </summary>
        [Fact]
        [Trait("DFe", "CTe")]
        public void SerializacaoDesserializacaoCTeInfPAA()
        {
            const string arqXML = @"..\..\..\CTe\Resources\4_00_CTe_ModalRodoviario.xml";
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<Unimake.Business.DFe.Xml.CTe.CTe>(doc);
            var infPAA = xml.InfCTe.InfPAA;

            Assert.Equal("00000000000000", infPAA.CNPJPAA);
            Assert.Equal(new byte[] { 1, 2, 3, 4 }, infPAA.PAASignature.SignatureValue);
            Assert.Equal(new byte[] { 1, 2, 3 }, infPAA.PAASignature.RSAKeyValue.Modulus);
            Assert.Equal(new byte[] { 1, 0, 1 }, infPAA.PAASignature.RSAKeyValue.Exponent);
            Assert.True(doc.InnerText == xml.GerarXML().InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML CteProc
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\99999999999999999999999999999999999999999999-procCTe.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_99999999999999999999999999999999999999999999-procCTe.xml")]
        public void SerializacaoDesserializacaoCTeProc(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = new CteProc();
            xml = xml.LoadFromFile(arqXML);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML retConsSitCTe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_1.xml")]
        [InlineData(@"..\..\..\CTe\Resources\4_00_retConsSitCTe.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_2.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_3.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_4.xml")]
        [InlineData(@"..\..\..\CTe\Resources\retConsSitCTe_5.xml")]
        public void SerializacaoDesserializacaoRetConsSitCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<RetConsSitCTe>(doc);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }

        /// <summary>
        /// Testar a serialização e desserialização do XML EventoCTe
        /// </summary>
        /// <param name="arqXML">Arquivo a ser desserializado</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110180.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110190.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110191.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110300.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_110301.xml")]
        [InlineData(@"..\..\..\CTe\Resources\eventoCTe_610111.xml")]
        public void SerializacaoDesserializacaoEventoCTe(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var xml = XMLUtility.Deserializar<EventoCTe>(doc);
            var xmlSerializado = xml.GerarXML();

            Assert.True(doc.InnerText == xmlSerializado.InnerText, "XML gerado pela DLL está diferente do conteúdo do arquivo serializado.");
        }
    }
}
