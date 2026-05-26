#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Consultar frota do transportador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ConsultarFrotaTransportador")]
    [ComVisible(true)]
#endif
    public class ConsultarFrotaTransportador : ServicoBase<Xml.CIOT.ConsultarFrotaTransportador, RetConsultarFrotaTransportador>, IInteropService<Xml.CIOT.ConsultarFrotaTransportador>
    {
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarFrotaTransportador;

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarFrotaTransportador() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarFrotaTransportador(Xml.CIOT.ConsultarFrotaTransportador xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarFrotaTransportador(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.CIOT.ConsultarFrotaTransportador xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
