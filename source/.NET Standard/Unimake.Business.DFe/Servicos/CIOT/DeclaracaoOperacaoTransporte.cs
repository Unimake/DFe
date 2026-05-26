#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Declarar operação de transporte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.DeclaracaoOperacaoTransporte")]
    [ComVisible(true)]
#endif
    public class DeclaracaoOperacaoTransporte : ServicoBase<Xml.CIOT.DeclaracaoOperacaoTransporte, RetDeclaracaoOperacaoTransporte>, IInteropService<Xml.CIOT.DeclaracaoOperacaoTransporte>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTDeclaracaoOperacaoTransporte;

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte(Xml.CIOT.DeclaracaoOperacaoTransporte xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public DeclaracaoOperacaoTransporte(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.DeclaracaoOperacaoTransporte xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
