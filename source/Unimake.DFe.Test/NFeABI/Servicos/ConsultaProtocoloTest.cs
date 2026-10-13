using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;
using Xunit;
using NFeABIConsultaProtocolo = Unimake.Business.DFe.Servicos.NFeABI.ConsultaProtocolo;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    /// <summary>
    /// Testa o serviço de consulta de protocolo da NFeABI em homologação.
    /// </summary>
    public class ConsultaProtocoloTest : NFeABIServicoTestBase
    {
        private const string ChaveNFeABI = "41260912345678000123770010000000011012345670";

        /// <summary>
        /// Consulta a situação da NFeABI pelo pipeline real.
        /// </summary>
        [Fact]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "ConsultaProtocolo")]
        public void ConsultaProtocolo()
        {
            var xml = new ConsSitNFeABI
            {
                Versao = "1.00",
                TpAmb = TipoAmbiente.Homologacao,
                XServ = "CONSULTAR",
                ChNFeABI = ChaveNFeABI
            };
            var configuracao = CriarConfiguracao(UFBrasil.PR);
            var consulta = new NFeABIConsultaProtocolo(xml, configuracao);

            consulta.Executar();

            Assert.Equal(Servico.NFeABIConsultaProtocolo, configuracao.Servico);
            Assert.Equal(TipoAmbiente.Homologacao, configuracao.TipoAmbiente);
            Assert.Equal(ModeloDFe.NFeABI, configuracao.Modelo);
            Assert.Contains("<chNFeABI>" + ChaveNFeABI + "</chNFeABI>", xml.GerarXML().OuterXml);
            Assert.IsType<RetConsSitNFeABI>(consulta.Result);
            Assert.NotEqual(0, consulta.Result.CStat);
        }
    }
}
