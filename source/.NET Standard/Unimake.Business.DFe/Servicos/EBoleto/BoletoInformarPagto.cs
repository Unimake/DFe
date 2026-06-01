#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Informar pagamento do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoInformarPagto")]
    [ComVisible(true)]
#endif
    public class BoletoInformarPagto : ServicoBase<Xml.EBoleto.BoletoInformarPagto>
    {
        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoInformarPagt;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoInformarPagto_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoInformarPagto() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoInformarPagto(Xml.EBoleto.BoletoInformarPagto xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoInformarPagto(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoInformarPagto xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
