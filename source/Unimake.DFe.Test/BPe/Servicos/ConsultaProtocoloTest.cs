using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeConsultaProtocolo = Unimake.Business.DFe.Servicos.BPe.ConsultaProtocolo;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de consulta de protocolo do BPe
    /// </summary>
    public class ConsultaProtocoloTest : BPeServicoTestBase
    {
        /// <summary>
        /// Consultar protocolo do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "ConsultaProtocolo")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void ConsultaProtocolo(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsSitBPe
            {
                Versao = "1.00",
                XServ = "CONSULTAR",
                TpAmb = tipoAmbiente,
                ChBPe = ((int)ufBrasil).ToString() + "260512345678000195630010000000110000001000"
            };

            var configuracao = CriarConfiguracao(ufBrasil);

            var consultaProtocolo = new BPeConsultaProtocolo(xml, configuracao);
            consultaProtocolo.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, consultaProtocolo.Result.TpAmb);
        }
    }
}
