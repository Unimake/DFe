using System;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Teste para consultar Lote Eventos Reinf
    /// </summary>
    public class ConsultaLoteEventosAssincronoTest
    {
        /// <summary>
        /// Testar a consulta lote assincrono do EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaLoteReinf(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsultaLoteAssincrono
            {
                Versao = "1.05.01",

                ConsultaLoteAssincrono = new ConsultaLoteAssincrono
                {
                    NumeroProtocolo = "9.999999.999999"
                }
            };

            var consultaLoteReinf = new Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono(xml, configuracao);
            consultaLoteReinf.Executar();
        }
    }
}
