#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>Grava ou atualiza um motorista na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.GravarMotorista")]
    [ComVisible(true)]
#endif
    public class GravarMotorista : ServicoBase, IInteropService<Xml.CIOT.GravarMotorista>
    {
        private Xml.CIOT.GravarMotorista envio;
        /// <summary>Objeto do XML de envio.</summary>
        public Xml.CIOT.GravarMotorista Envio => ObterEnvio(ref envio);
        /// <summary>Resultado do serviço.</summary>
        public RetGravarMotorista Result => ObterResult<RetGravarMotorista>();
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGravarMotorista;
        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetGravarMotorista);
        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;
        /// <summary>Construtor.</summary>
        public GravarMotorista() : base() { }
        /// <summary>Construtor.</summary>
        public GravarMotorista(Xml.CIOT.GravarMotorista xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);
        /// <summary>Construtor.</summary>
        public GravarMotorista(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
#if INTEROP
        /// <summary>Executa o serviço via COM.</summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.GravarMotorista xml, Configuracao configuracao) { InicializarServico(xml, configuracao); Executar(); }
#endif
    }
}
