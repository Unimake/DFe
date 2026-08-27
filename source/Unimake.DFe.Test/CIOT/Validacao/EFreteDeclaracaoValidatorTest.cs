using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete;
using Unimake.Business.DFe.Xml.CIOT;
using Unimake.Exceptions;
using Xunit;

namespace Unimake.DFe.Test.CIOT.Validacao
{
    public class EFreteDeclaracaoValidatorTest
    {
        [Fact]
        [Trait("DFe", "CIOT")]
        public void AceitaDeclaracaoLotacaoAderenteAoManual81()
        {
            EFreteValidator.Validar(LerDeclaracao(), Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao());
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitaTipoEmbalagemForaDoContrato()
        {
            var declaracao = LerDeclaracao();
            declaracao.TipoEmbalagem = "Volumes";

            var excecao = Assert.Throws<ValidarXMLException>(() => EFreteValidator.Validar(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Contains("TipoEmbalagem", excecao.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitaPagamentoSemValorParcela()
        {
            var declaracao = LerDeclaracao();
            declaracao.InfPagamento[0].ValorParcela = 0;

            var excecao = Assert.Throws<ValidarXMLException>(() => EFreteValidator.Validar(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Contains("ValorParcela", excecao.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitaTotaisDaViagemIncoerentes()
        {
            var declaracao = LerDeclaracao();
            declaracao.OrigemDestino[0].Valores.TotalDeQuitacao = 0;

            var excecao = Assert.Throws<ValidarXMLException>(() => EFreteValidator.Validar(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Contains("TotalViagem", excecao.Message);
            Assert.Contains("TotalDeAdiantamento", excecao.Message);
            Assert.Contains("TotalDeQuitacao", excecao.Message);
        }

        [Fact]
        [Trait("DFe", "CIOT")]
        public void RejeitaTipoPagamentoForaDoContratoEFrete()
        {
            var declaracao = LerDeclaracao();
            declaracao.InfPagamento[0].TipoPagamentoEFrete = "Pix";

            var excecao = Assert.Throws<ValidarXMLException>(() => EFreteValidator.Validar(declaracao, Servico.CIOTDeclaracaoOperacaoTransporte, CriarConfiguracao()));

            Assert.Contains("TipoPagamentoEFrete", excecao.Message);
        }

        private static Configuracao CriarConfiguracao() => new Configuracao
        {
            EFreteIntegrador = "INTEGRADOR-TESTE",
            EFreteToken = "TOKEN-TESTE",
            ProvedorCIOT = ProvedorCIOT.EFrete,
            TipoAmbiente = TipoAmbiente.Homologacao
        };

        private static DeclaracaoOperacaoTransporte LerDeclaracao()
        {
            var documento = new XmlDocument();
            documento.Load(Path.Combine(@"..\..\..\CIOT\Resources", "efrete-declaracao-carga-lotacao-completa.xml"));
            return new DeclaracaoOperacaoTransporte().LerXML<DeclaracaoOperacaoTransporte>(documento);
        }
    }
}
