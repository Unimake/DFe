using System;
using System.Collections.Generic;
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

        public void EnviarCTeOSSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente, string versao)
        {
            #region CriarCTeOS

            var xml = new Business.DFe.Xml.CTeOS.CTeOS
            {
                Versao = versao,

                InfCTe = new InfCTe
                {
                    Versao = versao,

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
                            VersaoModal = versao,
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

            #endregion CriarCTeOS

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
            Assert.True(autorizacao.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacao.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacao.Result.CStat.Equals(753) || autorizacao.Result.CStat.Equals(213) || autorizacao.Result.CStat.Equals(539) || autorizacao.Result.CStat.Equals(712), "Lote não foi recebido - <xMotivo> = " + autorizacao.Result.XMotivo);
        }
    }
}