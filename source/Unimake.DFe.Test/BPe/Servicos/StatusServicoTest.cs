using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.BPe;
using Xunit;
using BPeStatusServico = Unimake.Business.DFe.Servicos.BPe.StatusServico;

namespace Unimake.DFe.Test.BPe.Servicos
{
    /// <summary>
    /// Testar o serviço de status do BPe
    /// </summary>
    public class StatusServicoTest : BPeServicoTestBase
    {
        /// <summary>
        /// Consultar status do serviço do BPe.
        /// </summary>
        [Theory()]
        [Trait("DFe", "BPe")]
        [Trait("Servico", "StatusServico")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void StatusServico(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsStatServBPe
            {
                Versao = "1.00",
                XServ = "STATUS",
                TpAmb = tipoAmbiente
            };

            var configuracao = CriarConfiguracao(ufBrasil);

            var statusServico = new BPeStatusServico(xml, configuracao);
            statusServico.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(tipoAmbiente, statusServico.Result.TpAmb);
        }
    }
}
