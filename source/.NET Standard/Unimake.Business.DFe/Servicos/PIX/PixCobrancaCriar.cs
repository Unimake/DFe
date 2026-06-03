#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Servicos.PIX
{
    /// <summary>
    /// Criar cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.PIX.PixCobrancaCriar")]
    [ComVisible(true)]
#endif
    public class PixCobrancaCriar : ServicoBase<Xml.PIX.PixCobrancaCriar>
    {
        /// <inheritdoc />
        protected override Servico ServicoPIX => Servico.PIXCobrancaCriar;

        /// <inheritdoc />
        protected override string SchemaArquivoPIX => "PIXCobrancaCreateRequest_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar(Xml.PIX.PixCobrancaCriar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaCriar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);
    }
}
