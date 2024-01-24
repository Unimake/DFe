using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Teste da consulta recibo evento
    /// </summary>
    public class ConsultaReciboEventoTest
    {
        /// <summary>
        /// Testar a consulta recibo do Evento 1000 EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaReciboEvento1000(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaReciboEvento,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsulta
            {
                Versao = "1.05.01",
                ConsultaReciboEvento = new ConsultaReciboEvento
                {
                    TipoEvento = "1000",
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "00000000",
                }
            };

            var consultaReciboEvento = new Business.DFe.Servicos.EFDReinf.ConsultaReciboEvento(xml, configuracao);
            consultaReciboEvento.Executar();

            //Assert.True(consultaReciboEvento.re, "UF definida nas configurações diferente de " + ufBrasil.ToString());
        }
    }
}
