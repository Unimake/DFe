using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.EFDReinf;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Teste para consultar o fechamento do evento 2099 do EFD Reinf
    /// </summary>
    public class ConsultaFechamento2099Test
    {
        /// <summary>
        /// Testar a consulta fechamento com objeto
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaFechamento2099(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaFechamento2099,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xmlObjeto = new ReinfConsultaFechamento2099
            {
                ConsultaResultadoFechamento2099 = new ConsultaResultadoFechamento2099
                {
                    TpInsc = TiposInscricao.CNPJ,
                    NrInsc = "06117473",
                    NumeroProtocoloFechamento = "000000000000000"
                }
            };

            var consultaFechamento = new ConsultaFechamento2099(xmlObjeto, configuracao);
            consultaFechamento.Executar();
        }

        /// <summary>
        /// Testar a consulta fechamento com objeto
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao, @"..\..\..\EFDReinf\Resources\ConsultaResultadoFechamento2099-reinf-cons.xml")]
        [InlineData(TipoAmbiente.Producao, @"..\..\..\EFDReinf\Resources\ConsultaResultadoFechamento2099-reinf-cons.xml")]
        public void ConsultaFechamento2099String(TipoAmbiente tipoAmbiente, string arqXml)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaFechamento2099,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var consultaFechamento = new ConsultaFechamento2099(arqXml, configuracao);
            consultaFechamento.Executar();
        }
    }
}
