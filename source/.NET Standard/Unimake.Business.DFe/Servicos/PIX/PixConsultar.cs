#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Servicos.PIX
{
    /// <summary>
    /// Consultar PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.PIX.PixConsultar")]
    [ComVisible(true)]
#endif
    public class PixConsultar : ServicoBase<Xml.PIX.PixConsultar>
    {
        /// <inheritdoc />
        protected override Servico ServicoPIX => Servico.PIXConsultar;

        /// <inheritdoc />
        protected override string SchemaArquivoPIX => "PIXGetRequest_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public PixConsultar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public PixConsultar(Xml.PIX.PixConsultar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public PixConsultar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
    }
}
