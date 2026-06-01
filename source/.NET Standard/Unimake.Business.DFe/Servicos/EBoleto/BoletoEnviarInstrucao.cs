#if INTEROP
using System.Runtime.InteropServices;
#endif

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Enviar instrução para boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoEnviarInstrucao")]
    [ComVisible(true)]
#endif
    public class BoletoEnviarInstrucao : ServicoBase<Xml.EBoleto.BoletoEnviarInstrucao>
    {
        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoEnviarInstrucao;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoEnviarInstrucao_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoEnviarInstrucao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoEnviarInstrucao(Xml.EBoleto.BoletoEnviarInstrucao xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoEnviarInstrucao(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoEnviarInstrucao xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
