#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.EBoleto;

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Registrar boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoRegistrar")]
    [ComVisible(true)]
#endif
    public class BoletoRegistrar : ServicoBase<Xml.EBoleto.BoletoRegistrar>
    {
        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoRegistrar;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoRegistrar_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoRegistrar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoRegistrar(Xml.EBoleto.BoletoRegistrar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoRegistrar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoRegistrar xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
