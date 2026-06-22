#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Consultar situação do transportador no RNTRC
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ConsultarSituacaoTransportador")]
    [ComVisible(true)]
#endif
    public class ConsultarSituacaoTransportador : ServicoBase, IInteropService<Xml.CIOT.ConsultarSituacaoTransportador>
    {
        private Xml.CIOT.ConsultarSituacaoTransportador envio;

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.ConsultarSituacaoTransportador Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetConsultarSituacaoTransportador Result => ObterResult<RetConsultarSituacaoTransportador>();

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarSituacaoTransportador;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetConsultarSituacaoTransportador);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

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
