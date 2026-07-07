using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeAutorizacaoBPe = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPe;
using BPeAutorizacaoBPeTA = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTA;
using BPeAutorizacaoBPeTM = Unimake.Business.DFe.Servicos.BPe.AutorizacaoBPeTM;
using BPeConsultaProtocolo = Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo;
using BPeRecepcaoEvento = Unimake.Business.DFe.Servicos.BPe.RecepcaoEvento;
using BPeStatusServico = Unimake.Business.DFe.Servicos.BPe.StatusServico;

namespace Unimake.DFe.Test.BPe
{
    /// <summary>
    /// Testar os serviços do BPe
    /// </summary>
    public class ServicosTest
    {
        /// <summary>
        /// Consultar status do serviço do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "StatusServico")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void StatusServico(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsStatServBPe
            {
                Versao = "1.00",
                XServ = "STATUS",
                TpAmb = tipoAmbiente
            };

            var configuracao = CriarConfiguracao(ufBrasil);

            var statusServico = new BPeStatusServico(xml, configuracao);
            statusServico.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, statusServico.Result.TpAmb);
        }

        /// <summary>
        /// Consultar protocolo do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "ConsultaProtocolo")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void ConsultaProtocolo(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsSitBPe
            {
                Versao = "1.00",
                XServ = "CONSULTAR",
                TpAmb = tipoAmbiente,
                ChBPe = ((int)ufBrasil).ToString() + "260512345678000195630010000000110000001000"
            };

            var configuracao = CriarConfiguracao(ufBrasil);

            var consultaProtocolo = new BPeConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, consultaProtocolo.Result.TpAmb);
        }

        /// <summary>
        /// Enviar evento do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "RecepcaoEvento")]
        [InlineData(@"..\..\..\BPe\Resources\eventoBPe-110111-cancelamento.xml")]
        public void RecepcaoEvento(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var eventoObjeto = new EventoBPe().LerXML<EventoBPe>(xml);
            var configuracao = CriarConfiguracao((UFBrasil)eventoObjeto.InfEvento.COrgao);
            configuracao.Servico = Servico.BPeRecepcaoEvento;
            configuracao.TipoAmbiente = eventoObjeto.InfEvento.TpAmb;

            var recepcaoEvento = new BPeRecepcaoEvento(eventoObjeto, configuracao);
            recepcaoEvento.Executar();

            Assert.Equal((int)eventoObjeto.InfEvento.COrgao, configuracao.CodigoUF);
            Assert.Equal(eventoObjeto.InfEvento.TpAmb, recepcaoEvento.Result.InfEvento.TpAmb);
        }

        /// <summary>
        /// Autorizar BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void AutorizacaoBPe(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPe.BPe().LerXML<Business.DFe.Xml.BPe.BPe>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPe(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal((int)bpeObjeto.InfBPe.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(bpeObjeto.InfBPe.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetBPe>(autorizacao.Result);
        }

        /// <summary>
        /// Gerar QRCode automaticamente no XML do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPe")]
        [InlineData(@"..\..\..\BPe\Resources\bpe_minimo.xml")]
        public void GerarQRCodeBPe(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPe.BPe().LerXML<Business.DFe.Xml.BPe.BPe>(xml);
            bpeObjeto.Signature = null;
            bpeObjeto.InfBPeSupl = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPe(bpeObjeto, configuracao);

            AssertQRCodeBPe(autorizacao.ConteudoXMLAssinado, bpeObjeto.InfBPe.Chave, bpeObjeto.InfBPe.Ide.TpAmb);
        }

        /// <summary>
        /// Autorizar BPe TM.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "AutorizacaoBPeTM")]
        [InlineData(@"..\..\..\BPe\Resources\bpeTM_minimo.xml")]
        public void AutorizacaoBPeTM(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var bpeObjeto = new Business.DFe.Xml.BPeTM.BPeTM().LerXML<Business.DFe.Xml.BPeTM.BPeTM>(xml);
            bpeObjeto.Signature = null;
            var configuracao = CriarConfiguracao(bpeObjeto.InfBPe.Ide.CUF);

            var autorizacao = new BPeAutorizacaoBPeTM(bpeObjeto, configuracao);
            autorizacao.Executar();

            Assert.Equal((int)bpeObjeto.InfBPe.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(bpeObjeto.InfBPe.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetBPe>(autorizacao.Result);
        }

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

        private static Configuracao CriarConfiguracao(UFBrasil ufBrasil)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.BPe,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)ufBrasil,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }

        private static void AssertQRCodeBPe(XmlDocument xml, string chave, TipoAmbiente tipoAmbiente)
        {
            var qrCodBPe = xml.GetElementsByTagName("qrCodBPe");
            Assert.Equal(1, qrCodBPe.Count);

            var qrCode = qrCodBPe[0].InnerText;
            Assert.StartsWith("https://", qrCode);
            Assert.Contains("?chBPe=", qrCode);
            Assert.Contains("&tpAmb=" + ((int)tipoAmbiente).ToString(), qrCode);

            var infBPeSupl = xml.GetElementsByTagName("infBPeSupl");
            Assert.Equal(1, infBPeSupl.Count);
            Assert.NotNull(infBPeSupl[0].NextSibling);
            Assert.Equal("Signature", infBPeSupl[0].NextSibling.LocalName);
        }
    }
}
