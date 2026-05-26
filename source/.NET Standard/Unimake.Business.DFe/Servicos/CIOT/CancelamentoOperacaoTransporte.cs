#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Cancelar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.CancelamentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class CancelamentoOperacaoTransporte : ServicoBase<Xml.CIOT.CancelamentoOperacaoTransporte, RetCancelamentoOperacaoTransporte>, IInteropService<Xml.CIOT.CancelamentoOperacaoTransporte>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTCancelamentoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte(Xml.CIOT.CancelamentoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public CancelamentoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.CancelamentoOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
