#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Alterar vencimento do boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoAlterarVencto")]
    [ComVisible(true)]
#endif
    public class BoletoAlterarVencto : ServicoBase<Xml.EBoleto.BoletoAlterarVencto>
    {
        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoAlterarVencto;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoAlterarVencto_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoAlterarVencto() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoAlterarVencto(Xml.EBoleto.BoletoAlterarVencto xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoAlterarVencto(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoAlterarVencto xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
