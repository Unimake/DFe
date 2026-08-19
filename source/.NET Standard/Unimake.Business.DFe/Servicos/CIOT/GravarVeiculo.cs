#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml;
using Unimake.Business.DFe.Xml.CIOT;

namespace Unimake.Business.DFe.Servicos.CIOT
{
    /// <summary>Grava ou atualiza um veículo na eFrete.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.CIOT.GravarVeiculo")]
    [ComVisible(true)]
#endif
    public class GravarVeiculo : ServicoBase, IInteropService<Xml.CIOT.GravarVeiculo>
    {
        private Xml.CIOT.GravarVeiculo envio;
        /// <summary>Objeto do XML de envio.</summary>
        public Xml.CIOT.GravarVeiculo Envio => ObterEnvio(ref envio);
        /// <summary>Resultado do serviço.</summary>
        public RetGravarVeiculo Result => ObterResult<RetGravarVeiculo>();
        /// <inheritdoc />
        protected override Servico ServicoCIOT => Servico.CIOTGravarVeiculo;
        /// <inheritdoc />
        protected override string NomeRootRetorno => nameof(RetGravarVeiculo);
        /// <inheritdoc />
        protected override XMLBase XmlEnvio => Envio;
        /// <summary>Construtor.</summary>
        public GravarVeiculo() : base() { }
        /// <summary>Construtor.</summary>
        public GravarVeiculo(Xml.CIOT.GravarVeiculo xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);
        /// <summary>Construtor.</summary>
        public GravarVeiculo(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
#if INTEROP
        /// <summary>Executa o serviço via COM.</summary>
        [ComVisible(true)]
        public void Executar(Xml.CIOT.GravarVeiculo xml, Configuracao configuracao) { InicializarServico(xml, configuracao); Executar(); }
#endif
    }
}
