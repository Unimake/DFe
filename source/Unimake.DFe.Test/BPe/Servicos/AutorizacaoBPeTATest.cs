using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeAutorizacaoBPeTA = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de autorização do BPe TA
    /// </summary>
    public class AutorizacaoBPeTATest : BPeServicoTestBase
    {
        /// <summary>
        /// Autorizar BPe TA.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTA")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTA_minimo.xml")]
        public void AutorizacaoBPeTA(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTA.BPeTA().LerXML<Business.DFe.Xml.BPeTA.BPeTA>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPeTA(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal((int)bpeObjeto.InfBPe.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(bpeObjeto.InfBPe.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetBPe>(autorizacao.Result);
        }

        /// <summary>
        /// Gerar QRCode automaticamente no XML do BPe TA.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTA")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTA_minimo.xml")]
        public void GerarQRCodeBPeTA(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTA.BPeTA().LerXML<Business.DFe.Xml.BPeTA.BPeTA>(xml);
            bpeObjeto.Signature = null;
            bpeObjeto.InfBPeSupl = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPeTA(bpeObjeto, configuracao);

            AssertQRCodeBPe(autorizacao.ConteudoXMLAssinado, bpeObjeto.InfBPe.Chave, bpeObjeto.InfBPe.Ide.TpAmb);
        }

        /// <summary>
        /// Gerar XML de distribuição do BPe TA.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTA")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTA_minimo.xml")]
        public void GerarXmlDistribuicaoBPeTA(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTA.BPeTA().LerXML<Business.DFe.Xml.BPeTA.BPeTA>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPeTA(bpeObjeto, configuracao);
            var chave = bpeObjeto.InfBPe.Chave;
            autorizacao.RetornoWSString =
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>" +
                "<retBPe versao=\"1.00\" xmlns=\"http://www.portalfiscal.inf.br/bpe\">" +
                "<tpAmb>2</tpAmb>" +
                "<cUF>35</cUF>" +
                "<verAplic>SP_BPE_1.00</verAplic>" +
                "<cStat>100</cStat>" +
                "<xMotivo>Autorizado o uso do BP-e</xMotivo>" +
                "<protBPe versao=\"1.00\">" +
                "<infProt Id=\"ID" + chave + "\">" +
                "<tpAmb>2</tpAmb>" +
                "<verAplic>SP_BPE_1.00</verAplic>" +
                "<chBPe>" + chave + "</chBPe>" +
                "<dhRecbto>2026-07-06T15:01:00-03:00</dhRecbto>" +
                "<nProt>135260000000001</nProt>" +
                "<digVal>MTIzNDU2Nzg5MDEyMzQ1Njc4OTA=</digVal>" +
                "<cStat>100</cStat>" +
                "<xMotivo>Autorizado o uso do BP-e</xMotivo>" +
                "</infProt>" +
                "</protBPe>" +
                "</retBPe>";

            var bpeTAProc = autorizacao.BPeTAProcResults[chave];
            var xmlDistribuicao = bpeTAProc.GerarXML();

            Assert.Equal("bpeTAProc", xmlDistribuicao.DocumentElement.Name);
            Assert.Equal(chave + "-procBPeTA.xml", bpeTAProc.NomeArquivoDistribuicao);
            Assert.Contains("<BPeTA", xmlDistribuicao.OuterXml);
            Assert.Contains("<protBPe", xmlDistribuicao.OuterXml);
        }
    }
}
