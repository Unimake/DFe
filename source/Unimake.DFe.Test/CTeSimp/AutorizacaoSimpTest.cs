using System.Collections.Generic;
using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.CTe;
using Unimake.Business.DFe.Xml.CTeSimp;
using Xunit;

namespace Unimake.DFe.Test.CTe
{
    /// <summary>
    /// Testar o serviço de envio da CTe Simplificado Síncrono
    /// </summary>
    public class AutorizacaoSimpTest
    {
        /// <summary>
        /// Enviar uma CTe Simplificado no modo síncrono somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o web-service correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a CTe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a CTe</param>
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
        //[InlineData(UFBrasil.AC, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.AL, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.AP, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.AM, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.BA, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.CE, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.DF, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.ES, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.GO, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.MA, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.MT, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.MS, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.MG, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.PA, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.PB, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.PR, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.PE, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.PI, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.RJ, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.RN, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.RS, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.RO, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.RR, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.SC, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        //[InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void EnviarCTeSimp(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = MontarXmlCTeSimp(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSimp = new AutorizacaoSimp(xml, configuracao);
            autorizacaoSimp.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            //Assert.True(autorizacaoSimp.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacaoSimp.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacaoSimp.Result.CStat.Equals(225) || autorizacaoSimp.Result.CStat.Equals(215) || autorizacaoSimp.Result.CStat.Equals(213) || autorizacaoSimp.Result.CStat.Equals(539) || autorizacaoSimp.Result.CStat.Equals(712), "Lote não foi recebido - <xMotivo> = " + autorizacaoSimp.Result.XMotivo + " - <cStat> = " + autorizacaoSimp.Result.CStat); 
        }

        /// <summary>
        /// Enviar uma CTe no modo síncrono somente para saber se a conexão com o web-service está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a CTe</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a CTe</param>
        /// <param name="versao">Versão do schema</param>
        [Theory]
        [Trait("DFe", "CTe")]
        [InlineData(UFBrasil.PR, TipoAmbiente.Homologacao)]
        public void EnviarCTeSimpString(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = MontarXmlCTeSimp(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.CTe,
                TipoEmissao = TipoEmissao.Normal,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSimp = new AutorizacaoSimp(xml.GerarXML().OuterXml, configuracao);
            autorizacaoSimp.Executar();

            Assert.True(configuracao.CodigoUF.Equals((int)ufBrasil), "UF definida nas configurações diferente de " + ufBrasil.ToString());
            Assert.True(configuracao.TipoAmbiente.Equals(tipoAmbiente), "Tipo de ambiente definido nas configurações diferente de " + tipoAmbiente.ToString());
            Assert.True(autorizacaoSimp.Result.CUF.Equals(ufBrasil), "Web-service retornou uma UF e está diferente de " + ufBrasil.ToString());
            Assert.True(autorizacaoSimp.Result.TpAmb.Equals(tipoAmbiente), "Web-service retornou um Tipo de ambiente diferente " + tipoAmbiente.ToString());
            Assert.True(autorizacaoSimp.Result.CStat.Equals(225) || autorizacaoSimp.Result.CStat.Equals(215) || autorizacaoSimp.Result.CStat.Equals(213) || autorizacaoSimp.Result.CStat.Equals(539) || autorizacaoSimp.Result.CStat.Equals(712), "Lote não foi recebido - <xMotivo> = " + autorizacaoSimp.Result.XMotivo + " - <cStat> = " + autorizacaoSimp.Result.CStat);
        }

        private Business.DFe.Xml.CTeSimp.CTeSimp MontarXmlCTeSimp(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var cteSimp = new Business.DFe.Xml.CTeSimp.CTeSimp
            {
                InfCTe = new InfCTe
                {
                    Versao = "4.00",
                    Ide = new Ide
                    {
                        CUF = ufBrasil,
                        CCT = "09999999",
                        CFOP = "6352",
                        NatOp = "PREST.SERV.TRANSP.INDUSTR",
                        Mod = ModeloDFe.CTe,
                        Serie = 1,
                        NCT = 861,
                        DhEmi = DateTime.Now,
                        TpImp = FormatoImpressaoDACTE.NormalRetrato,
                        TpEmis = TipoEmissao.Normal,
                        CDV = 9,
                        TpAmb = tipoAmbiente,
                        TpCTe = TipoCTeSimp.CTeSimplificado,
                        ProcEmi = ProcessoEmissao.AplicativoContribuinte,
                        VerProc = "UNICO V8.0",
                        CMunEnv = "4118402",
                        XMunEnv = "PARANAVAI",
                        UFEnv = ufBrasil,
                        Modal = ModalidadeTransporteCTe.Rodoviario,
                        TpServ = TipoServicoCTe.Normal,
                        UFIni = ufBrasil,
                        UFFim = ufBrasil,
                        Retira = SimNao.Nao,
                        XDetRetira = "xDetRetira1"                        
                    },
                    Compl = new Compl
                    {
                        XCaracAd = "xCaracAd1",
                        XCaracSer = "xCaracSer1",
                        Fluxo = new Business.DFe.Xml.CTe.Fluxo
                        {
                            XOrig = "xOrig1",
                            Pass = new List<Business.DFe.Xml.CTe.Pass>
                            {
                                new Business.DFe.Xml.CTe.Pass { XPass = "xPass1" },
                                new Business.DFe.Xml.CTe.Pass { XPass = "xPass2" },
                                new Business.DFe.Xml.CTe.Pass { XPass = "xPass3" }
                            },
                            XDest = "xDest1",
                            XRota = "xRota1"
                        },
                        XObs = "xObs1",
                        ObsCont = new List<Business.DFe.Xml.CTe.ObsCont>
                        {
                            new Business.DFe.Xml.CTe.ObsCont { XCampo = "xCampo1", XTexto = "xTexto1" },
                            new Business.DFe.Xml.CTe.ObsCont { XCampo = "xCampo2", XTexto = "xTexto2" },
                            new Business.DFe.Xml.CTe.ObsCont { XCampo = "xCampo3", XTexto = "xTexto3" }
                        },
                        ObsFisco = new List<Business.DFe.Xml.CTe.ObsFisco>
                        {
                            new Business.DFe.Xml.CTe.ObsFisco { XCampo = "xCampo1", XTexto = "xTexto1" },
                            new Business.DFe.Xml.CTe.ObsFisco { XCampo = "xCampo2", XTexto = "xTexto2" },
                            new Business.DFe.Xml.CTe.ObsFisco { XCampo = "xCampo3", XTexto = "xTexto3" }
                        }
                    },
                    Emit = new Business.DFe.Xml.CTe.Emit
                    {
                        CNPJ = "00000000000000",
                        IE = "0000000000",
                        XNome = "xNome1",
                        XFant = "xFant1",
                        EnderEmit = new Business.DFe.Xml.CTe.EnderEmit
                        {
                            XLgr = "xLgr1",
                            Nro = "nro1",
                            XCpl = "xCpl1",
                            XBairro = "xBairro1",
                            CMun = 4118402,
                            XMun = "xMun1",
                            CEP = "88888888",
                            UF = ufBrasil,
                            Fone = "44991444444"
                        },
                        CRT = CRT.SimplesNacional
                    },
                    Toma = new Toma
                    {
                        TomaServico = TomadorServicoCTe.Remetente,
                        IndIEToma = IndicadorIEDestinatario.ContribuinteICMS,
                        CNPJ = "00000000000000",
                        IE = "000000000",
                        XNome = "xNome1",
                        Fone = "44991444444",
                        EnderToma = new Business.DFe.Xml.CTe.EnderToma
                        {
                            XLgr = "xLgr1",
                            Nro = "nro1",
                            XCpl = "xCpl1",
                            XBairro = "xBairro1",
                            CMun = 4118402,
                            XMun = "xMun1",
                            CEP = "88888888",
                            UF = ufBrasil,
                            CPais = 1058,
                            XPais = "xPais1"
                        },
                        Email = "email@gmail.com"
                    },
                    InfCarga = new Business.DFe.Xml.CTe.InfCarga
                    {
                        VCarga = 1.00,
                        ProPred = "proPred1",
                        XOutCat = "xOutCat1",
                        InfQ = new List<Business.DFe.Xml.CTe.InfQ>
                        {
                            new Business.DFe.Xml.CTe.InfQ { CUnid = CodigoUnidadeMedidaCTe.KG, TpMed = "00", QCarga = 1.0000 },
                            new Business.DFe.Xml.CTe.InfQ { CUnid = CodigoUnidadeMedidaCTe.KG, TpMed = "01", QCarga = 2.0000 },
                            new Business.DFe.Xml.CTe.InfQ { CUnid = CodigoUnidadeMedidaCTe.KG, TpMed = "02", QCarga = 3.0000 }
                        },
                        VCargaAverb = 1.00
                    },
                    Det = new List<Det>
                    {
                        new Det
                        {
                            NItem = "1",
                            CMunIni = "3305109",
                            XMunIni = "xMunIni1",
                            CMunFim = "3305109",
                            XMunFim = "xMunFim1",
                            VPrest = 1.00,
                            VRec = 1.00,
                            Comp = new List<Business.DFe.Xml.CTe.Comp>
                            {
                                new Business.DFe.Xml.CTe.Comp { XNome = "xNome1", VComp = 1.00 },
                                new Business.DFe.Xml.CTe.Comp { XNome = "xNome2", VComp = 2.00 },
                                new Business.DFe.Xml.CTe.Comp { XNome = "xNome3", VComp = 3.00 }
                            },
                            InfNFe = new List<InfNFe>
                            {
                                new InfNFe
                                {
                                    ChNFe = "50230500000000000000570010000008611099999999",
                                    PIN = "123",
                                    DPrev = DateTime.Parse("2024-07-19"),
                                    InfUnidCarga = new List<Business.DFe.Xml.CTe.InfUnidCarga>
                                    {
                                        new Business.DFe.Xml.CTe.InfUnidCarga
                                        {
                                            TpUnidCarga = TipoUnidadeCarga.Container,
                                            IdUnidCarga = "123456",
                                            LacUnidCarga = new List<Business.DFe.Xml.CTe.LacUnidCarga>
                                            {
                                                new Business.DFe.Xml.CTe.LacUnidCarga { NLacre = "nLacre1" },
                                                new Business.DFe.Xml.CTe.LacUnidCarga { NLacre = "nLacre2" },
                                                new Business.DFe.Xml.CTe.LacUnidCarga { NLacre = "nLacre3" }
                                            },
                                            QtdRat = 1.00
                                        },
                                    }
                                },
                            }
                        }
                    },
                    InfModal = new Business.DFe.Xml.CTe.InfModal
                    {
                        VersaoModal = "4.00",
                        Rodo = new Business.DFe.Xml.CTe.Rodo
                        {
                            RNTRC = "12345678",
                            Occ = new List<Business.DFe.Xml.CTe.Occ>
                            {
                                new Business.DFe.Xml.CTe.Occ
                                {
                                    Serie = "1",
                                    NOcc = 1,
                                    DEmi = DateTime.Now,
                                    EmiOcc = new Business.DFe.Xml.CTe.EmiOcc
                                    {
                                        CNPJ = "00000000000000",
                                        CInt = "cInt1",
                                        IE = "000000000",
                                        UF = ufBrasil,
                                        Fone = "44999999999"
                                    }
                                }
                            }
                        }
                    },
                    Imp = new Business.DFe.Xml.CTe.Imp
                    {
                        ICMS = new Business.DFe.Xml.CTe.ICMS
                        {
                            ICMS00 = new Business.DFe.Xml.CTe.ICMS00
                            {
                                CST = "00",
                                VBC = 1.00,
                                PICMS = 1.00,
                                VICMS = 1.00
                            }
                        },
                        VTotTrib = 1.00,
                        ICMSUFFim = new Business.DFe.Xml.CTe.ICMSUFFim
                        {
                            VBCUFFim = 1.00,
                            PFCPUFFim = 1.00,
                            PICMSUFFim = 1.00,
                            PICMSInter = 1.00,
                            VFCPUFFim = 1.00,
                            VICMSUFFim = 1.00,
                            VICMSUFIni = 1.00
                        },
                        InfAdFisco = "infAdFisco1"
                    },
                    Total = new Total
                    {
                        VTPrest = 1.00,
                        VTRec = 1.00
                    },
                    InfRespTec = new Business.DFe.Xml.CTe.InfRespTec
                    {
                        CNPJ = "00000000000000",
                        XContato = "xContato1",
                        Email = "email1@gmail.com",
                        Fone = "44991444444"
                    },
                    InfSolicNFF = new Business.DFe.Xml.CTe.InfSolicNFF
                    {
                        XSolic = "xSolic1"
                    }
                }
            };

            return cteSimp;
        }
    }
}