using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CCG;
using Unimake.Business.DFe.Xml.CCG;
using Xunit;

namespace Unimake.DFe.Test.CCG
{
    /// <summary>
    /// Testar o serviço de consulta protocolo da NFe
    /// </summary>
    public class CcgConsGTINTest
    {
        /// <summary>
        /// Consultar o código GTIN somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
        /// </summary>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta</param>
        [Theory]
        [Trait("DFe", "CCG")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaGTIN(TipoAmbiente tipoAmbiente)
        {
            var xml = new ConsGTIN
            {
                Versao = "1.00",
                GTIN = "7894900019896" // "7894900019896" //Código da Coca Cola
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CCG,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var ccgConsGTIN = new CcgConsGTIN(xml, configuracao);
            ccgConsGTIN.Executar();

            Assert.True(configuracao.TipoAmbiente.Equals(TipoAmbiente.Producao), "Tipo de ambiente definido não pode ser diferente de produção. Consulta GTIN só tem endereço de produção.");
            Assert.True(ccgConsGTIN.Result.CStat.Equals(9490), "Não encontrou o GTIN consultado, deveria ter encontrado, pois se trata do GTIN da Coca Cola.");
            Assert.True(ccgConsGTIN.Result.TpGTIN.Equals(TipoCodigoGTIN.GTIN13), "Tipo do GTIN retornado está incorreto.");
            Assert.True(ccgConsGTIN.Result.NCM.Equals("22021000"), "NCM da coca cola retornado está incorreto.");
            Assert.True(ccgConsGTIN.Result.GTIN.Equals(xml.GTIN), "NCM da coca cola retornado está incorreto.");
        }
    }
}
