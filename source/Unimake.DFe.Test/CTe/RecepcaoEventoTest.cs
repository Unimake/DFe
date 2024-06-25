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
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void RecepcaoEventoEstados(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new EventoCTe
            {
                Versao = "4.00",
                InfEvento = new InfEvento(new DetEventoCanc
                {
                    NProt = (ufBrasil != UFBrasil.AN ? (int)ufBrasil : (int)UFBrasil.PR) + "0000000000000",
                    VersaoEvento = "4.00",
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
            Assert.True(recepcaoEvento.Result.InfEvento.CStat.Equals(213) || recepcaoEvento.Result.InfEvento.CStat.Equals(128) || recepcaoEvento.Result.InfEvento.CStat.Equals(217) || recepcaoEvento.Result.InfEvento.CStat.Equals(236), "Serviço não está em operação - <cStat>"+ recepcaoEvento.Result.InfEvento.CStat + "</cStat><xMotivo>" + recepcaoEvento.Result.InfEvento.XMotivo + "</xMotivo>");
        }

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
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void RecepcaoEventoEstadosString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new EventoCTe
            {
                Versao = "4.00",
                InfEvento = new InfEvento(new DetEventoCanc
                {
                    NProt = (ufBrasil != UFBrasil.AN ? (int)ufBrasil : (int)UFBrasil.PR) + "0000000000000",
                    VersaoEvento = "4.00",
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

            var recepcaoEvento = new RecepcaoEvento(xml.GerarXML().OuterXml, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.InfEvento.CStat.Equals(128) || recepcaoEvento.Result.InfEvento.CStat.Equals(217) || recepcaoEvento.Result.InfEvento.CStat.Equals(236), "Serviço não está em operação - <xMotivo>" + recepcaoEvento.Result.InfEvento.XMotivo + "<xMotivo>");
        }

    }
}