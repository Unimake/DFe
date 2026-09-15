using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.NFeABI;
using Xunit;
using NFeABIStatusServico = Unimake.Business.DFe.Servicos.NFeABI.StatusServico;

namespace Unimake.DFe.Test.NFeABI.Servicos
{
    /// <summary>
    /// Testa o serviço de status da NFeABI em homologação.
    /// </summary>
    public class StatusServicoTest : NFeABIServicoTestBase
    {
        /// <summary>
        /// Consulta o status do serviço NFeABI pelo pipeline real.
        /// </summary>
        /// <param name="ufBrasil">Unidade federativa.</param>
        /// <param name="tipoAmbiente">Ambiente de homologação.</param>
        [Theory]
        [Trait("DFe", "NFeABI")]
        [Trait("Servico", "StatusServico")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void StatusServico(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsStatServNFeABI
            {
                Versao = "1.00",
                XServ = "STATUS",
                TpAmb = tipoAmbiente,
                CUF = ufBrasil
            };

            var configuracao = CriarConfiguracao(ufBrasil);
            var statusServico = new NFeABIStatusServico(xml, configuracao);

            statusServico.Executar();

            Assert.Equal((int)ufBrasil, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.IsType<RetConsStatServNFeABI>(statusServico.Result);
            Assert.Equal(tipoAmbiente, statusServico.Result.TpAmb);
        }
    }
}
