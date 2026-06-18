#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Consultar situação do transportador no RNTRC
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.None)]
    [ComDefaultInterface(typeof(IConsultarSituacaoTransportadorInterop))]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador")]
    [ComVisible(true)]
#endif
    public class ConsultarSituacaoTransportador : ServicoBase<Xml.CIOT.ConsultarSituacaoTransportador, RetConsultarSituacaoTransportador>, IInteropService<Xml.CIOT.ConsultarSituacaoTransportador>
#if INTEROP
        , IConsultarSituacaoTransportadorInterop
#endif
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarSituacaoTransportador;

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarSituacaoTransportador() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarSituacaoTransportador(Xml.CIOT.ConsultarSituacaoTransportador xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarSituacaoTransportador(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.ConsultarSituacaoTransportador xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
