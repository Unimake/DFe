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
    }
}
