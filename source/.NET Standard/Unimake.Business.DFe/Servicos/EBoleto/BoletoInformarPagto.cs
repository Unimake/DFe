#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EBoleto;

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
        /// <summary>
        /// Resultado do retorno da informação de pagamento de boleto
        /// </summary>
        public retBoletoInformarPagto Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retBoletoInformarPagto>(RetornoWSXML)
            : new retBoletoInformarPagto
            {
                Status = 999,
                Motivo = "Ocorreu um erro ao tentar obter o objeto no retorno da API",
                DLLVersao = Info.VersaoDLL
            };

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
