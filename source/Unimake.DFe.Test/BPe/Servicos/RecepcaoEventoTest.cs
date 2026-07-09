using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeRecepcaoEvento = Unimake.Business.DFe.Servicos.BPe.RecepcaoEvento;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de recepção de evento do BPe
    /// </summary>
    public class RecepcaoEventoTest : BPeServicoTestBase
    {
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
    }
}
