using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTe;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    /// <summary>
    /// Testar o serviço de recepção de eventos da CTe
    /// </summary>
    public class RecepcaoEventoTest
    {
        /// <summary>
        /// Enviar um evento de da CTe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Enviar um evento por estado + ambiente para garantir que todos estão funcionando.
        /// Evento utilizado no teste é o cancelamento, pois tem em todos estados e também no ambiente nacional.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
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
        public void RecepcaoEventoEstados(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string versao)
        {
            var xml = new EventoCTe
            {
                Versao = versao,
                InfEvento = new InfEvento(new DetEventoCanc
                {
                    NProt = (ufBrasil != UFBrasil.AN ? (int)ufBrasil : (int)UFBrasil.PR) + "0000000000000",
                    VersaoEvento = versao,
                    XJust = "Justificativa para cancelamento da CTe de teste"
                })
                {
                    COrgao = ufBrasil,
                    ChCTe = (int)ufBrasil + "200206117473000150570010000005671227070615",
                    CNPJ = "06117473000150",
                    DhEvento = DateTime.Now,
                    TpEvento = TipoEventoCTe.Cancelamento,
                    NSeqEvento = 1,
                    TpAmb = tipoAmbiente
                }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var recepcaoEvento = new RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.CStat.Equals(128) || recepcaoEvento.Result.InfEvento.CStat.Equals(217) || recepcaoEvento.Result.InfEvento.CStat.Equals(236), "Serviço não está em operação - <xMotivo>" + recepcaoEvento.Result.InfEvento.XMotivo + "<xMotivo>");
        }
    }
}