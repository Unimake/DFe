#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Collections.Generic;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.PIX;

namespace Unimake.Business.DFe.Servicos.PIX
{
    /// <summary>
    /// Consultar cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.PIX.PixCobrancaConsultar")]
    [ComVisible(true)]
#endif
    public class PixCobrancaConsultar : ServicoBase<Xml.PIX.PixCobrancaConsultar>
    {
        /// <summary>
        /// Resultado do retorno da consulta de cobrança PIX
        /// </summary>
        public retPIXCobrancaConsultar Result => RetornoWSXML != null
            ? XMLUtility.Deserializar<retPIXCobrancaConsultar>(RetornoWSXML)
            : new retPIXCobrancaConsultar
            {
                Status = 999,
                Motivo = "Ocorreu um erro ao tentar obter o objeto no retorno da API",
                DLLVersao = Info.VersaoDLL
            };

        /// <inheritdoc />
        protected override Servico ServicoPIX => Servico.PIXCobrancaConsultar;

        /// <inheritdoc />
        protected override string SchemaArquivoPIX => "PIXConsultaRequest_1_00.xsd";

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaConsultar() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaConsultar(Xml.PIX.PixCobrancaConsultar xml, Configuracao configuracao) : this() => InicializarServico(xml, configuracao);

        /// <summary>
        /// Construtor
        /// </summary>
        public PixCobrancaConsultar(string conteudoXML, Configuracao configuracao) : this() => InicializarServico(conteudoXML, configuracao);

        /// <inheritdoc />
        protected override void ConfigurarRequestURI()
        {
            AdicionarQueryString(new Dictionary<string, string>
            {
                { "StartDate", Envio.StartDateField },
                { "EndDate", Envio.EndDateField },
                { "Beneficiario.Inscricao", Envio.Beneficiario?.Inscricao },
                { "Beneficiario.Nome", Envio.Beneficiario?.Nome },
                { "Beneficiario.Conta.Agencia", Envio.Beneficiario?.Conta?.Agencia },
                { "Beneficiario.Conta.Numero", Envio.Beneficiario?.Conta?.Numero },
                { "Beneficiario.Conta.Banco", Envio.Beneficiario?.Conta?.Banco },
                { "ConfigurationId", Envio.ConfigurationId },
                { "Testing", Envio.Testing.ToString().ToLowerInvariant() },
                { "configurationId", Envio.ConfigurationId }
            });
        }
    }
}
