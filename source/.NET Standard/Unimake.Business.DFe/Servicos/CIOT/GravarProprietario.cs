#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>Grava ou atualiza um proprietário na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.GravarProprietario")]
    [ComVisible(true)]
#endif
    public class GravarProprietario : ServicoBase, IInteropService<Xml.CIOT.GravarProprietario>
    {
        private Xml.CIOT.GravarProprietario envio;
        /// <summary>Objeto do XML de envio.</summary>
        public Xml.CIOT.GravarProprietario Envio => ObterEnvio(ref envio);
        /// <summary>Resultado do serviço.</summary>
        public RetGravarProprietario Result => ObterResult<RetGravarProprietario>();
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGravarProprietario;
        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetGravarProprietario);
        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;
        /// <summary>Construtor.</summary>
        public GravarProprietario() : base() { }
        /// <summary>Construtor.</summary>
        public GravarProprietario(Xml.CIOT.GravarProprietario xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);
        /// <summary>Construtor.</summary>
        public GravarProprietario(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
#if INTEROP
        /// <summary>Executa o serviço via COM.</summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.GravarProprietario xml, Configuracao configuracao) { InicializarServico(xml, configuracao); Executar(); }
#endif
    }
}
