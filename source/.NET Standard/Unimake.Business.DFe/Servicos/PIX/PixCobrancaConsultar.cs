#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Collections.Generic;

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
                { "StartDate", GetPropertyString("StartDate") },
                { "EndDate", GetPropertyString("EndDate") },
                { "Beneficiario.Inscricao", Envio.Beneficiario?.Inscricao },
                { "Beneficiario.Nome", Envio.Beneficiario?.Nome },
                { "Beneficiario.Conta.Agencia", Envio.Beneficiario?.Conta?.Agencia },
                { "Beneficiario.Conta.Numero", Envio.Beneficiario?.Conta?.Numero },
                { "Beneficiario.Conta.Banco", Envio.Beneficiario?.Conta?.Banco },
                { "ConfigurationId", GetPropertyString("ConfigurationId") },
                { "Testing", GetPropertyString("Testing") },
                { "configurationId", GetPropertyString("ConfigurationId") }
            });
        }
    }
}
