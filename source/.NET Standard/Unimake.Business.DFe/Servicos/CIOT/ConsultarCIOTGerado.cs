#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Consultar CIOT gerado
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ConsultarCIOTGerado")]
    [ComVisible(true)]
#endif
    public class ConsultarCIOTGerado : ServicoBase<Xml.CIOT.ConsultarCIOTGerado, RetConsultarCIOTGerado>, IInteropService<Xml.CIOT.ConsultarCIOTGerado>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarCIOTGerado;

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarCIOTGerado() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarCIOTGerado(Xml.CIOT.ConsultarCIOTGerado xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarCIOTGerado(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.ConsultarCIOTGerado xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
