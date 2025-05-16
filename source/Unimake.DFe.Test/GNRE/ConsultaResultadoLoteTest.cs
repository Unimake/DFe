using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.GNRE;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Consultar uma chave do MDFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
    /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
    /// </summary>
    public class ConsultaResultadoLoteTest
    {
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultarResultadoLoteGNRE(TipoAmbiente tipoAmbiente)
        {
            var xml = new TConsLoteGNRE
            {
                Ambiente = tipoAmbiente,
                IncluirArquivoPagamento = SimNaoLetra.Sim,
                IncluirNoticias = SimNaoLetra.Sim,
                IncluirPDFGuias = SimNaoLetra.Sim,
                NumeroRecibo = "4112345123"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = 41,
                Servico = Servico.GNREConsultaResultadoLote
            };

            var consultaResultadoLote = new ConsultaResultadoLote(xml, configuracao);
            consultaResultadoLote.Executar();

            Assert.True(consultaResultadoLote.Result != null);
            Assert.True(consultaResultadoLote.Result.Ambiente.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(consultaResultadoLote.Result.SituacaoProcess.Codigo.Equals("100") || consultaResultadoLote.Result.SituacaoProcess.Codigo.Equals("102") || consultaResultadoLote.Result.SituacaoProcess.Codigo.Equals("602"), "Código retornado não era esperado: " + consultaResultadoLote.Result.SituacaoProcess.Codigo + "-" + consultaResultadoLote.Result.SituacaoProcess.Descricao);
        }

        ///<summary>
        ///Teste serviço consulta de lote GNRE construtor simplificado
        /// </summary>
        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultarResultadoLoteConstrutor(TipoAmbiente tipoAmbiente)
        {            
            if (PropConfig.CertificadoDigital == null)
            {
                return;
            }

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.GNREConsultaResultadoLote,
                CodigoUF = 41, // Adicione o código UF como no primeiro teste
                UsaCertificadoDigital = PropConfig.CertificadoDigital != null, 
            };

            var consultaResultadoLote = new ConsultaResultadoLote("12345678901234567890", configuracao);
            consultaResultadoLote.Executar();

            Assert.NotNull(consultaResultadoLote.Result);
            Assert.Equal(tipoAmbiente, consultaResultadoLote.Result.Ambiente);
        }
    }
}
