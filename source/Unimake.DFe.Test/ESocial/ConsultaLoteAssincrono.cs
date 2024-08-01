using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class ConsultaLoteAssincrono
    {
        /// <summary>
        /// Testar o Download de Eventos Por ID
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialConsultaLoteAssincrono(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.ConsultarLoteEventos
            {
                ConsultaLoteEventos = new ConsultaLoteEventos
                {
                    ProtocoloEnvio = "1.8.11111111111111111111",
                }
            };

            var consultaLoteAssincrono = new Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono(conteudoXML, configuracao);
            consultaLoteAssincrono.Executar();
        }
    }
}