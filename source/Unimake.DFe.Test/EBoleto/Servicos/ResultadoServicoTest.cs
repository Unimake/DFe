using Unimake.Business.DFe.Servicos.EBoleto;
using Unimake.Business.DFe.Utility;
using Xunit;
using RetornoBasico = Unimake.Business.DFe.Xml.EBoleto.retEBoletoRetornoBasico;

namespace Unimake.DFe.Test.EBoleto.Servicos
{
    public class ResultadoServicoTest
    {
        private const string MensagemSemResposta = "Ocorreu um erro ao tentar obter o objeto no retorno da API";

        [Fact]
        [Trait("DFe", "EBoleto")]
        public void DeveManterRetornoPadraoQuandoApiNaoRetornarXml()
        {
            var registrar = new BoletoRegistrar().Result;
            var cancelar = new BoletoCancelar().Result;
            var consultar = new BoletoConsultar().Result;
            var alterarVencto = new BoletoAlterarVencto().Result;
            var enviarInstrucao = new BoletoEnviarInstrucao().Result;
#pragma warning disable CS0618
            var informarPagto = new BoletoInformarPagto().Result;
#pragma warning restore CS0618

            ValidarResultado(registrar, registrar.DLLVersao);
            ValidarResultado(cancelar, cancelar.DLLVersao);
            ValidarResultado(consultar, consultar.DLLVersao);
            ValidarResultado(alterarVencto, alterarVencto.DLLVersao);
            ValidarResultado(enviarInstrucao, enviarInstrucao.DLLVersao);
            ValidarResultado(informarPagto, informarPagto.DLLVersao);
        }

        private static void ValidarResultado(RetornoBasico resultado, string dllVersao)
        {
            Assert.Equal(999, resultado.Status);
            Assert.Equal(MensagemSemResposta, resultado.Motivo);
            Assert.Equal(Info.VersaoDLL, dllVersao);
        }
    }
}
