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

        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void RecepcaoEventoEstadosString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
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

            var recepcaoEvento = new RecepcaoEvento(xml.GerarXML().OuterXml, configuracao);
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

        /// <summary>
        /// Testar o envio do evento de insucesso na entrega da NF-e, pois é tudo enviando para o SVRS, independente do estado, e tenho que garantir isso.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.TO)]
        public void RecepcaoEventoInsucessoEntrega(UFBrasil ufBrasil)
        {
            var tipoAmbiente = TipoAmbiente.Producao;

            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoInsucessoEntregaNFe
                            {
                                Versao = "1.00",
                                COrgaoAutor = ufBrasil,
                                DhTentativaEntrega = DateTime.Now,
                                LatGPS = "37.774929",
                                LongGPS= "122.419418",
                                NTentativa = 1,
                                VerAplic = "uninfeteste1.0",
                                TpMotivo = TipoMotivoInsucessoEntrega.Outros,
                                XJustMotivo = "Teste do evento de insucesso na entrega.",
                                HashTentativaEntrega = "RX2tvfSSZWhNsjSwmWQOzZM71hI=",
                                DhHashTentativaEntrega = DateTime.Now
                            })
                            {
                                COrgao = UFBrasil.SVRS, //Sempre 92 para Insucesso na entrega
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.InsucessoEntregaNFe,
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

            Assert.Contains("SVRS", recepcaoEvento.Result.VerAplic); //Quem tem que responder é sempre o SVRS, se não for, tem coisa errada. Só SVRS autoriza o evento de Insucesso
            Assert.True(configuracao.CodigoUF.Equals(92), "UF definida nas configurações diferente de 92=SVRS");
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            //Assert.True(recepcaoEvento.Result.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217) || recepcaoEvento.Result.CStat.Equals(215), "Serviço não está em operação - <cStat>" + recepcaoEvento.Result.CStat + "</cStat><xMotivo>" + recepcaoEvento.Result.XMotivo + "<xMotivo>");
        }

        /// <summary>
        /// Testar o envio do evento de cancelamento do insucesso na entrega da NF-e, pois é tudo enviando para o SVRS, independente do estado, e tenho que garantir isso.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.TO)]
        public void RecepcaoEventoCancelamentoInsucessoEntrega(UFBrasil ufBrasil)
        {
            var tipoAmbiente = TipoAmbiente.Producao;

            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoCancelamentoInsucessoEntregaNFe
                            {
                                Versao = "1.00",
                                COrgaoAutor = ufBrasil,
                                VerAplic = "uninfeteste1.0",
                                NProtEvento = "123456789012345"
                            })
                            {
                                COrgao = UFBrasil.SVRS, //Sempre 92 para Insucesso na entrega
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.CancelamentoInsucessoEntregaNFe,
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

            Assert.Contains("SVRS", recepcaoEvento.Result.VerAplic); //Quem tem que responder é sempre o SVRS, se não for, tem coisa errada. Só SVRS autoriza o evento de Insucesso
            Assert.True(configuracao.CodigoUF.Equals(92), "UF definida nas configurações diferente de 92=SVRS");
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            //Assert.True(recepcaoEvento.Result.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217) || recepcaoEvento.Result.CStat.Equals(215), "Serviço não está em operação - <cStat>" + recepcaoEvento.Result.CStat + "</cStat><xMotivo>" + recepcaoEvento.Result.XMotivo + "<xMotivo>");
        }

        /// <summary>
        /// Testar o envio do evento de cancelamento do insucesso na entrega da NF-e, pois é tudo enviando para o SVRS, independente do estado, e tenho que garantir isso.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.TO)]
        public void RecepcaoEventoAtorInteressadoNFe(UFBrasil ufBrasil)
        {
            var tipoAmbiente = TipoAmbiente.Producao;

            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoAtorInteressadoNFe
                            {
                                Versao = "1.00",
                                COrgaoAutor = ufBrasil,
                                VerAplic = "uninfeteste1.0",
                                TpAutor = TipoAutorGeradorEvento.EmpresaEmitente,
                                TpAutorizacao = TipoAutorizacao.Permite,
                                AutXML = new AutXML
                                {
                                    CNPJ = "00000000000000"
                                }
                            })
                            {
                                COrgao = UFBrasil.AN, //Sempre 91 para este evento
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.AtorInteressadoNFe,
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

            Assert.Contains("AN_", recepcaoEvento.Result.VerAplic); //Quem tem que responder é sempre o AN_, se não for tem coisa errada. Só AN_ autoriza este evento.
            Assert.True(configuracao.CodigoUF.Equals(91), "UF definida nas configurações diferente de 91=AN");
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.COrgao.Equals(UFBrasil.AN), "Webservice retornou uma UF e está diferente de " + UFBrasil.AN.ToString());
            Assert.True(recepcaoEvento.Result.RetEvento[0].InfEvento.COrgaoAutor.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217), "Serviço não está em operação - <xMotivo>" + recepcaoEvento.Result.XMotivo + "<xMotivo>");
        }

        /// <summary>
        /// Testar o envio do evento de cancelamento do insucesso na entrega da NF-e, pois é tudo enviando para o SVRS, independente do estado, e tenho que garantir isso.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.TO)]
        public void RecepcaoEventoConciliacaoFinanceira(UFBrasil ufBrasil)
        {
            var tipoAmbiente = TipoAmbiente.Producao;

            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoConciliacaoFinanceira
                            {
                                Versao = "1.00",
                                VerAplic = "uninfeteste1.0",
                                DescEvento = "ECONF",
                                DetPag = new List<DetPagECONF>
                                {
                                    new DetPagECONF
                                    {
                                        IndPag = IndicadorPagamento.PagamentoVista,
                                        TPag = MeioPagamento.CartaoCredito,
                                        VPag = 11.11,
                                        DPag = DateTime.Now.Date,
                                        CNPJPag = "11111111111111",
                                        UFPag = UFBrasil.PR,
                                        CNPJIF = "22222222222222",
                                        TBand = BandeiraOperadoraCartao.Visa,
                                        CAut = "000000000000000",
                                        CNPJReceb = "33333333333333",
                                        UFReceb = UFBrasil.MG
                                    },
                                    new DetPagECONF
                                    {
                                        IndPag = IndicadorPagamento.PagamentoVista,
                                        TPag = MeioPagamento.CartaoCredito,
                                        VPag = 22.22,
                                        DPag = DateTime.Now.Date,
                                        CNPJPag = "11111111111111",
                                        UFPag = UFBrasil.PR,
                                        CNPJIF = "22222222222222",
                                        TBand = BandeiraOperadoraCartao.Visa,
                                        CAut = "000000000000000",
                                        CNPJReceb = "33333333333333",
                                        UFReceb = UFBrasil.MG
                                    },                                }
                            })
                            {
                                COrgao = ufBrasil,
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.ConciliacaoFinanceira,
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

            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            if (ufBrasil != UFBrasil.BA) //BA retornando coisas fora do padrão
            {
                if (ufBrasil != UFBrasil.MA)
                {
                    Assert.True(recepcaoEvento.Result.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil);
                }
                Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
                Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(999) || recepcaoEvento.Result.CStat.Equals(215) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217), "<cStat>" + recepcaoEvento.Result.CStat + "</cStat><xMotivo>" + recepcaoEvento.Result.XMotivo + "</xMotivo>");
            }
        }

        /// <summary>
        /// Testar o envio do evento de cancelamento do insucesso na entrega da NF-e, pois é tudo enviando para o SVRS, independente do estado, e tenho que garantir isso.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado xml</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado o xml</param>
        [Theory]
        [Trait("DFe", "NFe")]
        [InlineData(UFBrasil.AC)]
        [InlineData(UFBrasil.AL)]
        [InlineData(UFBrasil.AP)]
        [InlineData(UFBrasil.AM)]
        [InlineData(UFBrasil.BA)]
        [InlineData(UFBrasil.CE)]
        [InlineData(UFBrasil.DF)]
        [InlineData(UFBrasil.ES)]
        [InlineData(UFBrasil.GO)]
        [InlineData(UFBrasil.MA)]
        [InlineData(UFBrasil.MT)]
        [InlineData(UFBrasil.MS)]
        [InlineData(UFBrasil.MG)]
        [InlineData(UFBrasil.PA)]
        [InlineData(UFBrasil.PB)]
        [InlineData(UFBrasil.PR)]
        [InlineData(UFBrasil.PE)]
        [InlineData(UFBrasil.PI)]
        [InlineData(UFBrasil.RJ)]
        [InlineData(UFBrasil.RN)]
        [InlineData(UFBrasil.RS)]
        [InlineData(UFBrasil.RO)]
        [InlineData(UFBrasil.RR)]
        [InlineData(UFBrasil.SC)]
        [InlineData(UFBrasil.SP)]
        [InlineData(UFBrasil.SE)]
        [InlineData(UFBrasil.TO)]
        public void RecepcaoEventoCancelamentoConciliacaoFinanceira(UFBrasil ufBrasil)
        {
            var tipoAmbiente = TipoAmbiente.Producao;

            var xml = new EnvEvento
            {
                Versao = "1.00",
                IdLote = "000000000000001",
                Evento = new List<Evento> {
                        new Evento
                        {
                            Versao = "1.00",
                            InfEvento = new InfEvento(new DetEventoCancelamentoConciliacaoFinanceira
                            {
                                Versao = "1.00",
                                VerAplic = "uninfeteste1.0",
                                NProtEvento = "000000000000000",
                            })
                            {
                                COrgao = ufBrasil,
                                ChNFe = (int)ufBrasil + "190806117473000150550010000579131943463890",
                                CNPJ = "06117473000150",
                                DhEvento = DateTime.Now,
                                TpEvento = TipoEventoNFe.CancelamentoConciliacaoFinanceira,
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

            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            if (ufBrasil != UFBrasil.BA && ufBrasil != UFBrasil.MT) //BA e MT retornando coisas fora do padrão
            {
                if (ufBrasil != UFBrasil.MA)
                {
                    Assert.True(recepcaoEvento.Result.COrgao.Equals(ufBrasil), "Webservice retornou uma UF e está diferente de " + ufBrasil);
                }
                Assert.True(recepcaoEvento.Result.TpAmb.Equals(tipoAmbiente), "Webservice retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
                Assert.True(recepcaoEvento.Result.CStat.Equals(128) || recepcaoEvento.Result.CStat.Equals(999) || recepcaoEvento.Result.CStat.Equals(215) || recepcaoEvento.Result.CStat.Equals(656) || recepcaoEvento.Result.CStat.Equals(217), "<cStat>" + recepcaoEvento.Result.CStat + "</cStat><xMotivo>" + recepcaoEvento.Result.XMotivo + "</xMotivo>");
            }
        }
    }
}