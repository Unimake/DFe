#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
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
    public class ConsultarFrotaTransportador : ServicoBase, IInteropService<Xml.CIOT.ConsultarFrotaTransportador>
    {
        private Xml.CIOT.ConsultarFrotaTransportador envio;

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.ConsultarFrotaTransportador Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetConsultarFrotaTransportador Result => ObterResult<RetConsultarFrotaTransportador>();

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarFrotaTransportador;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetConsultarFrotaTransportador);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

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
        public void Executar(Xml.CIOT.ConsultarFrotaTransportador xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
