#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EBoleto;

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Cancelar/baixar boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoCancelar")]
    [ComVisible(true)]
#endif
    public class BoletoCancelar : ServicoBase<Xml.EBoleto.BoletoCancelar>
    {
        /// <summary>
        /// Resultado do retorno do cancelamento de boleto
        /// </summary>
        public retBoletoCancelar Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retBoletoCancelar>(RetornoWSXML)
            : new retBoletoCancelar
            {
                Status = 999,
                Motivo = "Ocorreu um erro ao tentar obter o objeto no retorno da API",
                DLLVersao = Info.VersaoDLL
            };

        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoCancelar;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoCancelar_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoCancelar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoCancelar(Xml.EBoleto.BoletoCancelar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoCancelar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoCancelar xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
