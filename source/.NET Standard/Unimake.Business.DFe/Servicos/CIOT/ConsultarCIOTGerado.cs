#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
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
    public class ConsultarCIOTGerado : ServicoBase, IInteropService<Xml.CIOT.ConsultarCIOTGerado>
    {
        private Xml.CIOT.ConsultarCIOTGerado envio;

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.ConsultarCIOTGerado Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetConsultarCIOTGerado Result => ObterResult<RetConsultarCIOTGerado>();

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarCIOTGerado;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetConsultarCIOTGerado);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

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
        public void Executar(Xml.CIOT.ConsultarCIOTGerado xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
