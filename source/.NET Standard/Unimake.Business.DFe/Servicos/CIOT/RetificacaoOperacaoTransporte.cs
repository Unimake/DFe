#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Retificar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.RetificacaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class RetificacaoOperacaoTransporte : ServicoBase<Xml.CIOT.RetificacaoOperacaoTransporte, RetRetificacaoOperacaoTransporte>, IInteropService<Xml.CIOT.RetificacaoOperacaoTransporte>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTRetificacaoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public RetificacaoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public RetificacaoOperacaoTransporte(Xml.CIOT.RetificacaoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public RetificacaoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.RetificacaoOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
