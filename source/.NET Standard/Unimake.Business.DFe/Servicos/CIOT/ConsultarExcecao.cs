#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>
    /// Consultar exceção do transportador
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.ConsultarExcecao")]
    [ComVisible(true)]
#endif
    public class ConsultarExcecao : ServicoBase, IInteropService<Xml.CIOT.ConsultarExcecao>
    {
        private Xml.CIOT.ConsultarExcecao envio;

        /// <summary>
        /// Objeto do XML de envio
        /// </summary>
        public Xml.CIOT.ConsultarExcecao Envio => ObterEnvio(ref envio);

        /// <summary>
        /// Resultado do serviço
        /// </summary>
        public RetConsultarExcecao Result => ObterResult<RetConsultarExcecao>();

        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTConsultarExcecao;

        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetConsultarExcecao);

        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarExcecao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarExcecao(Xml.CIOT.ConsultarExcecao xml, Configuracao configuracao) : this()
        {
            InicializarServico(xml, configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultarExcecao(string conteudoXML, Configuracao configuracao) : this()
        {
            InicializarServico(conteudoXML, configuracao);
        }

        /// <inheritdoc />
        protected override void DefinirConfiguracao()
        {
            base.DefinirConfiguracao();

            Configuracoes.RequestURIHomologacao = Configuracoes.RequestURIHomologacao?.Replace("{CpfCnpjTransportador}", Envio.CpfCnpjTransportador);
            Configuracoes.RequestURIProducao = Configuracoes.RequestURIProducao?.Replace("{CpfCnpjTransportador}", Envio.CpfCnpjTransportador);
            Configuracoes.RequestURI = Configuracoes.RequestURI?.Replace("{CpfCnpjTransportador}", Envio.CpfCnpjTransportador);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço
        /// </summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.ConsultarExcecao xml, Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
