#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Encerrar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.EncerramentoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class EncerramentoOperacaoTransporte : ServicoBase<Xml.CIOT.EncerramentoOperacaoTransporte, RetEncerramentoOperacaoTransporte>, IInteropService<Xml.CIOT.EncerramentoOperacaoTransporte>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTEncerramentoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte(Xml.CIOT.EncerramentoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public EncerramentoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.EncerramentoOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
