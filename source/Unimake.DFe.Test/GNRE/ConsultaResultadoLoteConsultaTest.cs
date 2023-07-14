using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.GNRE;
using Unimake.Business.DFe.Xml.GNRE;
using Xunit;

namespace Unimake.DFe.Test.GNRE
{
    /// <summary>
    /// Testar o serviço consulta processamento de tote de consulta de GNRE
    /// </summary>
    public class ConsultaResultadoLoteConsultaTest
    {
        /// <summary>
        /// Testar o serviço consulta processamento de tote de consulta de GNRE
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta</param>

        [Theory]
        [Trait("DFe", "GNRE")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]

        public void ConsultaResultadoLoteConsulta(TipoAmbiente tipoAmbiente)
        {
            var xml = new TConsLoteConsGNRE
            {
                Ambiente = tipoAmbiente,
                NumeroRecibo = "1234567891"
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.GNRE,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaResultadoLoteConsulta = new ConsultaResultadoLoteConsulta(xml, configuracao);
            consultaResultadoLoteConsulta.Executar();

            Assert.True(consultaResultadoLoteConsulta.Result != null);
            Assert.True(consultaResultadoLoteConsulta.Result.Ambiente.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
        }
    }
}