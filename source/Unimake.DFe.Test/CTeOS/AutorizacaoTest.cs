﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTeOS;
using Unimake.Business.DFe.Xml.CTeOS;
using Xunit;

namespace Unimake.DFe.Test.CTeOS
{
    /// <summary>
    /// Testar o serviço de envio da CTe Síncrono
    /// </summary>
    public class AutorizacaoTest
    {
        /// <summary>
        /// Enviar um CTeOS no modo síncrono somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a CTe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a CTe</param>
        /// <param name="versao">Versão do schema</param>
        [Theory]
        [Trait("DFe", "CTeOS")]
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
        public void EnviarCTeOSSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = MontarXML(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Autorizacao(xml, configuracao);
            autorizacao.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            //Assert.True(autorizacao.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacao.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CStat.Equals(753) || autorizacao.Result.CStat.Equals(213) || autorizacao.Result.CStat.Equals(539) || autorizacao.Result.CStat.Equals(712), "Lote não foi recebido - <xMotivo> = " + autorizacao.Result.XMotivo);
        }

        private Business.DFe.Xml.CTeOS.CTeOS MontarXML(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new Business.DFe.Xml.CTeOS.CTeOS
            {
                Versao = "4.00",

                InfCTe = new InfCTe
                {
                    Versao = "4.00",

                    Ide = new Ide
                    {
                        CUF = ufBrasil,
                        CCT = "01722067",
                        CFOP = "6352",
                        NatOp = "PREST.SERV.TRANSP.INDUSTR",
                        Mod = ModeloDFe.CTeOS,
                        Serie = 1,
                        NCT = 861,
                        DhEmi = DateTime.Now,
                        TpImp = FormatoImpressaoDACTE.NormalPaisagem,
                        TpEmis = TipoEmissao.Normal,
                        TpAmb = tipoAmbiente,
                        TpCTe = TipoCTe.Normal,
                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                        VerProc = "UNICO V8.0",
                        CMunEnv = "4118402",
                        XMunEnv = "PARANAVAI",
                        UFEnv = ufBrasil,
                        Modal = ModalidadeTransporteCTe.Rodoviario,
                        TpServ = TipoServicoCTeOS.TransportePessoas,
                        CMunIni = "4118402",
                        XMunIni = "PARANAVAI",
                        UFIni = UFBrasil.PR,
                        CMunFim = "3305109",
                        XMunFim = "SAO JOAO DE MERITI",
                        UFFim = UFBrasil.RJ,
                        IndIEToma = IndicadorIEDestinatario.ContribuinteICMS,
                    },
                    Emit = new Emit
                    {
                        CNPJ = "31905001000109",
                        IE = "9079649730",
                        XNome = "EXATUS MOVEIS EIRELI",
                        XFant = "EXATUS MOVEIS",
                        EnderEmit = new Unimake.Business.DFe.Xml.CTeOS.EnderEmit
                        {
                            XLgr = "RUA JOAQUIM F. DE SOUZA",
                            Nro = "01112",
                            XBairro = "VILA TEREZINHA",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87706675",
                            UF = ufBrasil,
                            Fone = "04434237530",
                        },
                        CRT = CRT.SimplesNacional
                    },
                    Toma = new Toma
                    {
                        CNPJ = "31905001000109",
                        IE = "9079649730",
                        XNome = "EXATUS MOVEIS EIRELI",
                        XFant = "EXATUS MOVEIS",
                        EnderToma = new Unimake.Business.DFe.Xml.CTeOS.EnderToma
                        {
                            XLgr = "RUA JOAQUIM F. DE SOUZA",
                            Nro = "01112",
                            XBairro = "VILA TEREZINHA",
                            CMun = 4118402,
                            XMun = "PARANAVAI",
                            CEP = "87706675",
                            UF = ufBrasil,
                        }
                    },
                    VPrest = new VPrest
                    {
                        VTPrest = 50.00,
                        VRec = 50.00,
                        Comp = new List<Comp>
                                    {
                                        new Comp
                                        {
                                            XNome = "FRETE VALOR",
                                            VComp = 50.00,
                                        },
                                    },
                    },
                    Imp = new Imp
                    {
                        ICMS = new ICMS
                        {
                            ICMSSN = new ICMSSN
                            {
                                CST = "90",
                                IndSN = SimNao.Sim,
                            }
                        }
                    },
                    InfCTeNorm = new InfCTeNorm
                    {
                        InfServico = new InfServico
                        {
                            XDescServ = "TRANSPORTE DE PASSAGEIROS",
                            InfQ = new InfQ
                            {
                                QCarga = 45.0000
                            },
                        },
                        Seg = new List<Seg>
                            {
                                new Seg
                                {
                                    RespSeg = ResponsavelSeguroCTeOS.EmitenteCTeOS
                                }
                            },
                        InfModal = new InfModal
                        {
                            VersaoModal = "4.00",
                            RodoOS = new RodoOS
                            {
                                TAF = "999999999999",
                                Veic = new Veic
                                {
                                    Placa = "XXX999",
                                    RENAVAM = "999999999",
                                    UF = UFBrasil.SP
                                },
                            },
                        },
                    },
                    InfRespTec = new InfRespTec
                    {
                        CNPJ = "06117473000150",
                        XContato = "Wandrey Mundin Ferreira",
                        Email = "wandrey@unimake.com.br",
                        Fone = "04431414900",
                    },
                },
            };

            return xml;
        }

        /// <summary>
        /// Enviar um CTeOS no modo síncrono somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a CTe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a CTe</param>
        /// <param name="versao">Versão do schema</param>
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        public void EnviarCTeOSSincronoString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = MontarXML(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Autorizacao(xml.GerarXML().OuterXml, configuracao);
            autorizacao.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacao.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CStat.Equals(753) || autorizacao.Result.CStat.Equals(213) || autorizacao.Result.CStat.Equals(539) || autorizacao.Result.CStat.Equals(712), "Lote não foi recebido - <xMotivo> = " + autorizacao.Result.XMotivo);
        }

        /// <summary>
        /// Testa a validação do XML de CTeOS com as tags da reforma tributária
        /// </summary>
        /// <param name="arqXML"></param>
        [Theory]
        [Trait("DFe", "CTeOS")]
        [InlineData(@"..\..\..\CTeOS\Resources\RTC\CTeOS_CST000.xml")]
        public void ValidarCTeOSComReformaTributaria(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTeOS,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacao = new Autorizacao(doc.OuterXml, configuracao);
        }
    }
}