using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFGas;
using Xunit;
using NFGasAutorizacaoSinc = Unimake.Business.DFe.Servicos.NFGas.AutorizacaoSinc;
using NFGasConsultaProtocolo = Unimake.Business.DFe.Servicos.NFGas.ConsultaProtocolo;
using NFGasRecepcaoEvento = Unimake.Business.DFe.Servicos.NFGas.RecepcaoEvento;
using NFGasStatusServico = Unimake.Business.DFe.Servicos.NFGas.StatusServico;

namespace Unimake.DFe.Test.NFGas.Servicos
{
    /// <summary>
    /// Testar os serviços da NFGas
    /// </summary>
    public class ServicosTest
    {
        /// <summary>
        /// Consultar status do serviço da NFGas.
        /// </summary>
        [Theory()]
        [Trait("DFe", "NFGas")]
        [Trait("Servico", "StatusServico")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void StatusServico(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsStatServNFGas
            {
                Versao = "1.00",
                XServ = "STATUS",
                TpAmb = tipoAmbiente
            };
            
            var configuracao = CriarConfiguracao(ufBrasil);

            var statusServico = new NFGasStatusServico(xml, configuracao);
            statusServico.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, statusServico.Result.TpAmb);
        }

        /// <summary>
        /// Consultar protocolo da NFGas.
        /// </summary>
        [Theory()]
        [Trait("DFe", "NFGas")]
        [Trait("Servico", "ConsultaProtocolo")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void ConsultaProtocolo(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsSitNFGas
            {
                Versao = "1.00",
                XServ = "CONSULTAR",
                TpAmb = tipoAmbiente,
                ChNFGas = ((int)ufBrasil).ToString() + "260512345678000195760010000000110000001000"
            };

            var configuracao = CriarConfiguracao(ufBrasil);

            var consultaProtocolo = new NFGasConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, consultaProtocolo.Result.TpAmb);
        }

        /// <summary>
        /// Enviar evento da NFGas.
        /// </summary>
        [Theory()]
        [Trait("DFe", "NFGas")]
        [Trait("Servico", "RecepcaoEvento")]
        [InlineData(@"..\..\..\NFGas\Resources\eventoNFGas-110111.xml")]
        public void RecepcaoEvento(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var eventoObjeto = new EventoNFGas().LerXML<EventoNFGas>(xml);
            var configuracao = CriarConfiguracao((UFBrasil)eventoObjeto.InfEvento.COrgao);
            configuracao.Servico = Servico.NFGasRecepcaoEvento;
            configuracao.TipoAmbiente = (TipoAmbiente)eventoObjeto.InfEvento.TpAmb;

            var recepcaoEvento = new NFGasRecepcaoEvento(eventoObjeto, configuracao);
            recepcaoEvento.Executar();

            Assert.Equal(eventoObjeto.InfEvento.COrgao, configuracao.CodigoUF);
            Assert.Equal(eventoObjeto.InfEvento.TpAmb, recepcaoEvento.Result.InfEvento.TpAmb);
        }

        /// <summary>
        /// Autorizar NFGas em modo síncrono.
        /// </summary>
        [Theory()]
        [Trait("DFe", "NFGas")]
        [Trait("Servico", "AutorizacaoSinc")]
        [InlineData(@"..\..\..\NFGas\Resources\nfgas.xml")]
        public void AutorizacaoSinc(string arqXML)
        {
            var xml = new XmlDocument();
            xml.Load(arqXML);

            var nfgasObjeto = new Business.DFe.Xml.NFGas.NFGas().LerXML<Business.DFe.Xml.NFGas.NFGas>(xml);
            var configuracao = CriarConfiguracao(nfgasObjeto.InfNFGas.Ide.CUF);

            var autorizacaoSinc = new NFGasAutorizacaoSinc(nfgasObjeto, configuracao);
            autorizacaoSinc.Executar();

            Assert.Equal((int)nfgasObjeto.InfNFGas.Ide.CUF, configuracao.CodigoUF);
            Assert.Equal(nfgasObjeto.InfNFGas.Ide.TpAmb, configuracao.TipoAmbiente);
            Assert.IsType<RetNFGas>(autorizacaoSinc.Result);
        }

        private static Configuracao CriarConfiguracao(UFBrasil ufBrasil)
        {
            return new Configuracao
            {
                TipoDFe = TipoDFe.NFGas,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)ufBrasil,
                CertificadoDigital = PropConfig.CertificadoDigital
            };
        }
    }
}
