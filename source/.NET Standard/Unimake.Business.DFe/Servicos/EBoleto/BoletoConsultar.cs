#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EBoleto;

namespace Unimake.Business.DFe.Servicos.EBoleto
{
    /// <summary>
    /// Consultar boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EBoleto.BoletoConsultar")]
    [ComVisible(true)]
#endif
    public class BoletoConsultar : ServicoBase<Xml.EBoleto.BoletoConsultar>
    {
        /// <summary>
        /// Resultado do retorno da consulta de boleto
        /// </summary>
        public retBoletoConsultar Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retBoletoConsultar>(RetornoWSXML)
            : new retBoletoConsultar
            {
                Status = 999,
                Motivo = "Ocorreu um erro ao tentar obter o objeto no retorno da API",
                DLLVersao = Info.VersaoDLL
            };

        /// <inheritdoc />
        protected override Servico ServicoEBoleto => Servico.EBoletoConsultar;

        /// <inheritdoc />
        protected override string SchemaArquivoEBoleto => "BoletoConsultar_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoConsultar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoConsultar(Xml.EBoleto.BoletoConsultar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public BoletoConsultar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

#if INTEROP
        /// <summary>
        /// Executa o serviço via COM
        /// </summary>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] Xml.EBoleto.BoletoConsultar xml, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            InicializarServico(xml, configuracao);
            Executar();
        }
#endif
    }
}
