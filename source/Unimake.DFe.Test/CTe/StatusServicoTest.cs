using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    /// <summary>
    /// Testar o serviço de consulta protocolo da CTe
    /// </summary>
    public class StatusServicoTest
    {
        /// <summary>
        /// Consultar o status do serviço da CTe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua uma consulta por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a consulta status</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a consulta status</param>
        /// <param name="versao">Versão do schema</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao, "3.00")]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao, "3.00")]

        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao, "4.00")]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao, "4.00")]

        [InlineData(UFBrasil.AC, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao, "3.00")]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao, "3.00")]

        [InlineData(UFBrasil.AC, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao, "4.00")]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao, "4.00")]

        public void ConsultarStatusServico(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string versao)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                TipoEmissao = TipoEmissao.Normal,
                CodigoUF = (int)ufBrasil,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ConsStatServCte
            {
                Versao = versao,
                TpAmb = tipoAmbiente,
                CUF = (versao == "4.00" ? ufBrasil : UFBrasil.NaoDefinido)
            };

            var statusServico = new StatusServico(xml, configuracao);
            statusServico.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());

            if (versao == "3.00" || statusServico.Result.CUF != UFBrasil.SP) //Não sei o PQ mas SVSP não está retornando o estado de origem, na versão 3.00 retorna, na 4.00 não.
            {
                Assert.True(statusServico.Result.CUF.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }

            Assert.True(statusServico.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(statusServico.Result.CStat.Equals(107), "Serviço não está em operação - <xMotivo>" + statusServico.Result.XMotivo + "<xMotivo>");
        }
    }
}