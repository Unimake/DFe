using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NFe;
using Unimake.Business.DFe.Xml.NFe;
using Xunit;

namespace Unimake.DFe.Test.NFe
{
    /// <summary>
    /// Testar o serviço de recepção de eventos da NFe
    /// </summary>
    public class RecepcaoEventoTest
    {
        /// <summary>
        /// Enviar um evento de da NFe somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Enviar um evento por estado + ambiente para garantir que todos estão funcionando.
        /// Evento utilizado no teste é o cancelamento, pois tem em todos estados e também no ambiente nacional.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AL, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AP, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AM, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.AN, TipoAmbiente.Homologacao)] //Ambiente nacional
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
        [InlineData(UFBrasil.AN, TipoAmbiente.Producao)] //Ambiente nacional
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
            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoCanc
                            {
                                NProt = (ufBrasil != UFBrasil.AN ? (int)ufBrasil : (int)UFBrasil.PR)  + "0000000000000",
                                Versao = "1.00",
                                XJust = "Justificativa para cancelamento da NFe de teste"
                            })
                            {
                                COrgao = ufBrasil,
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.Cancelamento,
                                NSeqEvento = 1,
                                VerEvento = "1.00",
                                TpAmb = tipoAmbiente
                            }
                        }
                    }
            };

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NFe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var recepcaoEvento = new RecepcaoEvento(xml, configuracao);
            recepcaoEvento.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());

            if (ufBrasil == UFBrasil.MA)
            {
                Assert.True(recepcaoEvento.Result.COrgao.Equals(UFBrasil.DF), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }
            else
            {
                Assert.True(recepcaoEvento.Result.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            }

            Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217), "Serviço não está em operação - <xMotivo>" + recepcaoEvento.Result.XMotivo + "<xMotivo>");
        }
    }
}