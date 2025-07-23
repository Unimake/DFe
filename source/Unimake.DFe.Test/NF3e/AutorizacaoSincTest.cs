using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.NF3e;
using Unimake.Business.DFe.Xml.NF3e;
using Xunit;

namespace Unimake.DFe.Test.NF3e
{
    /// <summary>
    /// Testasr o serviço de envio da NF3e
    /// </summary>
    public class AutorizacaoSincTest
    {
        /// <summary>
        /// Enviar uma NF3e no modo síncrono somente para saber se a conexão com o webservice está ocorrendo corretamente e se quem está respondendo é o webservice correto.
        /// Efetua o envio por estado + ambiente para garantir que todos estão funcionando.
        /// </summary>
        /// <param name="ufBrasil">UF para onde deve ser enviado a NF3e</param>
        /// <param name="tipoAmbiente">Ambiente para onde deve ser enviado a NF3e</param>
        [Theory]
        [Trait("DFe", "NF3e")]
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
        [InlineData(UFBrasil.MG, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MT, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.MS, TipoAmbiente.Homologacao)]
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
        [InlineData(UFBrasil.SE, TipoAmbiente.Homologacao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Homologacao)]
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
        [InlineData(UFBrasil.SE, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.SP, TipoAmbiente.Producao)]
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void EnviarNF3eSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var nf3eObjeto = MontarXMLNF3e(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.ContingenciaOfflineNF3eNFCom,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSincNF3e = new AutorizacaoSinc(nf3eObjeto, configuracao);
            autorizacaoSincNF3e.Executar();
        }

        /// <summary>
        /// Auxiliar para montar o XML do NF3e
        /// </summary>
        /// <param name="ufBrasil">UF</param>
        /// <param name="tipoAmbiente">Ambiente</param>
        /// <returns></returns>
        private Business.DFe.Xml.NF3e.NF3e MontarXMLNF3e(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var xml = new Business.DFe.Xml.NF3e.NF3e
            {
                InfNF3e = new InfNF3e
                {
                    Versao = "1.00",
                    Ide = new Ide
                    {
                        CUF = ufBrasil,
                        TpAmb = tipoAmbiente,
                        Mod = ModeloDFe.NF3e,
                        Serie = 123,
                        NNF = 1,
                        CNF = "1489657",
                        DhEmi = DateTime.Now,
                        TpEmis = TipoEmissao.Normal,
                        NSiteAutoriz = "0",
                        CMunFG = "1234567",
                        FinNF3e = FinalidadeNF3e.Normal,
                        VerProc = "verProc1",
                        GCompraGov = new GCompraGov
                        {
                            TpEnteGov = TipoEnteGovernamental.Uniao,
                            PRedutor = 1.00
                        }
                    },
                    Emit = new Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "9032000301",
                        XNome = "Unimake Solucoes Corporativas",
                        XFant = "Unimake Software",
                        EnderEmit = new EnderEmit
                        {
                            XLgr = "Rua Paulo Antonio da Costa",
                            Nro = "575",
                            XBairro = "Jardim Simara",
                            CMun = "4118402",
                            XMun = "Paranavai",
                            CEP = "87777000",
                            UF = UFBrasil.PR,
                            Email = "teste@teste.com.br"
                        }
                    },
                    Dest = new Dest
                    {
                        XNome = "Empresa Teste",
                        CNPJ = "12345678901234",
                        IndIEDest = IndicadorIEDestinatario.NaoContribuinte,
                        IE = "9876543",
                        EnderDest = new EnderDest
                        {
                            XLgr = "Rua da Silva",
                            Nro = "1",
                            XBairro = "Jardim Ipe",
                            CMun = "1234567",
                            XMun = "Outro",
                            CEP = "87777001",
                            UF = UFBrasil.PR,
                            Email = "1teste1@1teste1.com.br"
                        }
                    },
                    Acessante = new Acessante
                    {
                        IdAcesso = "1",
                        TpAcesso = TipoAcessante.Cativo,
                        XNomeUC = "nome aleatorio",
                        TpClasse = TipoClasseConsumidora.ConsumoProprio,
                        TpSubClasse = TipoSubClasseConsumidora.Residencial,
                        TpFase = TipoLigacao.Monofasico,
                        TpGrpTensao = GrupoSubGrupoTensao.A1AltaTensao230kVMais,
                        TpModTar = ModalidadeTarifaria.HorariaBranca,
                        LatGPS = "45.123456",
                        LongGPS = "12.345678",
                        CodRoteiroLeitura = "85589"
                    },
                    GJudic = new GJudic
                    {
                        ChNF3e = "12345678901234567890123456789012345678901234"
                    },
                    GMed = new List<GMed>
                    {
                        new GMed
                        {
                            NMed = "01",
                            IdMedidor = "AB1234",
                            DMedAnt = DateTime.Now,
                            DMedAtu = DateTime.Now,
                        },
                        new GMed
                        {
                            NMed = "02",
                            IdMedidor = "DC98765",
                            DMedAnt = DateTime.Now,
                            DMedAtu = DateTime.Now,
                        }
                    },
                    GSCEE = new GSCEE
                    {
                        TpPartComp = TipoParticipacaoCompensacao.AutoconsumoRemoto,
                        GConsumidor = new List<GConsumidor>
                        {
                            new GConsumidor
                            {
                                IdAcessGer = "AB1234",
                                VPotInst = 12789.014,
                                TpFonteEnergia = TipoFonteEnergia.Hidraulica,
                                EnerAloc = 14.19,
                                TpPosTar = TipoPostoTarifario.Unico,
                                EnerInjet = 117.45,
                                TpPosTarInjet = TipoPostoTarifario.Intermediario
                            },
                            new GConsumidor
                            {
                                IdAcessGer = "FVD9785",
                                VPotInst = 174.17,
                                TpFonteEnergia = TipoFonteEnergia.Eolica,
                                EnerAloc = 189.256,
                                TpPosTar = TipoPostoTarifario.ForaPonta
                            }
                        },
                        GSaldoCred = new List<GSaldoCred>
                        {
                            new GSaldoCred
                            {
                                TpPosTar = TipoPostoTarifario.Ponta,
                                VSaldAnt = 123.14,
                                VCredExpirado = 14.589,
                                VSaldAtual = 1.478
                            }
                        }
                    },
                    NFdet = new List<NFdet>
                    {
                        new NFdet
                        {
                            Det = new List<Det>
                            {
                                new Det
                                {
                                    NItem = "1",
                                    DetItemAnt = new DetItemAnt
                                    {
                                        NItemAnt = "12",
                                        VItem = 123456.12M,
                                        QFaturada = 123.4,
                                        VProd = 1.02M,
                                        CClass = "1234567",
                                        VBC = 1.34,
                                        PICMS = 2.3,
                                        VFCP = 2.44,
                                        VBCST = 3.45,
                                        VICMSST = 4.55,
                                        VFCPST = 2.33,
                                        VPIS = 2.3,
                                        VPISEfet = 3.4,
                                        VCOFINS = 3.22,
                                        VCOFINSEfet = 4.5,
                                        RetTribNF3e = new RetTribNF3e
                                        {
                                            VRetPIS = 123.45M,
                                            VRetCofins = 44.42M,
                                            VRetCSLL = 33M,
                                            VIRRF = 33.42M
                                        },
                                        IndDevolucao = SimNao.Sim
                                    }
                                },
                                new Det
                                {
                                    NItem = "2",
                                    DetItem = new DetItem
                                    {
                                        NItemAnt = "1",
                                        GTarif = new GTarif
                                        {
                                            DIniTarif = DateTime.Now,
                                            DFimTarif = DateTime.Now,
                                            TpAto = TipoAto.Despacho,
                                            NAto = "1342",
                                            AnoAto = "2024",
                                            TpTarif = TipoTarifa.TUSD,
                                            CPosTarif = TipoPostoTarifario.Intermediario,
                                            UMed = UnidadeMedidaEnergia.KW,
                                            VTarifHom = 0
                                        },
                                        GAdBand = new GAdBand
                                        {
                                            DIniAdBand = DateTime.Now,
                                            DFimAdBand = DateTime.Now,
                                            TpBand = TipoBandeira.EscassezHidrica,
                                            VAdBand = 33.44M,
                                            VAdBandAplic = 2.11,
                                            MotDifBand = MotivoTarifaDiferente.DescontoTarifario
                                        },
                                        Prod = new Prod
                                        {
                                            IndOrigemQtd = IndicadorOrigemQuantidadeFaturada.Medido,
                                            GMedicao = new GMedicao
                                            {
                                                NMed = "02",
                                                NContrat = "03",
                                                GMedida = new GMedida
                                                {
                                                    TpGrMed = TipoGrandezaMedida.Demanda,
                                                    CPosTarif = TipoPostoTarifario.ForaPonta,
                                                    UMed = UnidadeMedidaEnergia.KVArh,
                                                    VMedAnt = 23.33,
                                                    VMedAtu = 34.3,
                                                    VConst = 333,
                                                    VMed = 44,
                                                    PPerdaTran = 22.344,
                                                    VMedPerdaTran = 33,
                                                    VMedPerdaTec = 3455
                                                }
                                            },
                                            CProd = "12345",
                                            XProd = "produto teste",
                                            CClass = "1234567",
                                            CFOP = "1234",
                                            UMed = UnidadeMedidaEnergia.KW,
                                            QFaturada = 2345.333,
                                            VItem = 123.45M,
                                            VProd = 23,
                                            IndDevolucao = SimNao.Sim,
                                            IndPrecoACL = SimNao.Sim
                                        },
                                        Imposto = new Imposto
                                        {
                                            ICMS00 = new ICMS00
                                            {
                                                CST = "00",
                                                VBC = 123,
                                                PICMS = 12.13,
                                                VICMS = 23.33,
                                                PFCP = 22.333,
                                                VFCP = 33.4
                                            },
                                            PIS = new PIS
                                            {
                                                CST = CSTPisCofins.AliquotaBasica,
                                                VBC = 22.22,
                                                PPIS = 3,
                                                VPIS = 4.44
                                            },
                                            COFINS = new COFINS
                                            {
                                                CST = CSTPisCofins.AliquotaBasica,
                                                VBC = 22.22,
                                                PCOFINS = 5.55,
                                                VCOFINS = 33
                                            },
                                            RetTrib = new RetTribNF3e
                                            {
                                                VRetPIS = 23.13M,
                                                VRetCofins = 33M,
                                                VRetCSLL = 33,
                                                VBCIRRF = 25,
                                                VIRRF = 234.55M
                                            },
                                            IBSCBS = new IBSCBS
                                            {
                                                CST = "123",
                                                CClassTrib = "123456",
                                                GIBSCBS = new GIBSCBS
                                                {
                                                    VBC = 11.11,
                                                    GIBSUF = new GIBSUF
                                                    {
                                                        PIBSUF = 1.113,
                                                        GDif = new GDif
                                                        {
                                                            PDif = 1.1154,
                                                            VDif = 11.11
                                                        },
                                                        GDevTrib = new GDevTrib
                                                        {
                                                            VDevTrib = 115.80
                                                        },
                                                        GRed = new GRed
                                                        {
                                                            PRedAliq = 1.1154,
                                                            PAliqEfet = 1.448
                                                        },
                                                        VIBSUF = 111.58
                                                    },
                                                    GIBSMun = new GIBSMun
                                                    {
                                                        PIBSMun = 11.4844,
                                                        GDif = new GDif
                                                        {
                                                            PDif = 1.1154,
                                                            VDif = 11.11
                                                        },
                                                        GDevTrib = new GDevTrib
                                                        {
                                                            VDevTrib = 115.80
                                                        },
                                                        GRed = new GRed
                                                        {
                                                            PRedAliq = 1.1154,
                                                            PAliqEfet = 1.448
                                                        },
                                                        VIBSMun = 169.89
                                                    },
                                                    GCBS = new GCBS
                                                    {
                                                        PCBS = 11.454,
                                                        GDif = new GDif
                                                        {
                                                            PDif = 1.1154,
                                                            VDif = 11.11
                                                        },
                                                        GDevTrib = new GDevTrib
                                                        {
                                                            VDevTrib = 115.80
                                                        },
                                                        GRed = new GRed
                                                        {
                                                            PRedAliq = 1.1154,
                                                            PAliqEfet = 1.448
                                                        },
                                                        VCBS = 1587.56
                                                    },
                                                    GTribRegular = new GTribRegular
                                                    {
                                                        CSTReg = "041",
                                                        CClassTribReg = "876543",
                                                        PAliqEfetRegIBSUF = 1.6000,
                                                        VTribRegIBSUF = 16.00,
                                                        PAliqEfetRegIBSMun = 0.8000,
                                                        VTribRegIBSMun = 8.00,
                                                        PAliqEfetRegCBS = 2.7000,
                                                        VTribRegCBS = 27.00
                                                    },
                                                    GIBSCredPres = new GIBSCredPres
                                                    {
                                                        CCredPres = "1111",
                                                        PCredPres = 5.0000,
                                                        VCredPresCondSus = 50.00,
                                                    },
                                                    GCBSCredPres = new GCBSCredPres
                                                    {
                                                        CCredPres = "2222",
                                                        PCredPres = 3.0000,
                                                        VCredPres = 30.00,
                                                    },
                                                    GTribCompraGov = new GTribCompraGov
                                                    {
                                                        PAliqIBSUF = 1.0000,
                                                        VTribIBSUF = 1.11,
                                                        PAliqIBSMun = 2.0000,
                                                        VTribIBSMun = 2.22,
                                                        PAliqCBS = 3.0000,
                                                        VTribCBS = 3.33
                                                    }
                                                }
                                            }
                                        },
                                        GProcRef = new GProcRef
                                        {
                                            VItem = 2.33M,
                                            QFaturada = 123.45,
                                            VProd = 23.33M,
                                            IndDevolucao = SimNao.Sim,
                                            VBC = 23.2,
                                            PICMS = 22.2,
                                            VICMS = 2,
                                            PFCP = 123.4,
                                            VFCP = 22.22,
                                            VBCST = 2.22,
                                            PICMSST = 2.222,
                                            VICMSST = 22.33,
                                            PFCPST = 22.2,
                                            VFCPST = 22.22,
                                            VPIS = 2.2,
                                            VPISEfet = 2.12,
                                            VCOFINS = 2.22,
                                            VCOFINSEfet = 2,
                                            GProc = new List<GProc>
                                            {
                                                new GProc
                                                {
                                                    TpProc = TipoProcessoNF3eNFCom.JusticaEstadual,
                                                    NProcesso = "1222"
                                                }
                                            }
                                        },
                                        InfAdProd = "TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE"
                                    }
                                }
                            }
                        }
                    },
                    Total = new Total
                    {
                        VProd = 23151515.22,
                        ICMSTot = new ICMSTot
                        {
                            VBC = 123.45,
                            VICMS = 12.22,
                            VICMSDeson = 1,
                            VFCP = 22,
                            VBCST = 123.45,
                            VST = 1234.5,
                            VFCPST = 11.1
                        },
                        VRetTribTot = new VRetTribTot
                        {
                            VRetPIS = 123.45,
                            VRetCofins = 123.45,
                            VRetCSLL = 11.11,
                            VIRRF = 11.44
                        },
                        VCOFINS = 11.1,
                        VCOFINSEfet = 11.1,
                        VPIS = 14.11,
                        VPISEfet = 14.11,
                        VNF = 111111.45,
                        IBSCBSTot = new IBSCBSTot
                        {
                            VBCIBSCBS = 1.00,
                            GIBS = new GIBS
                            {
                                GIBSUF = new GIBSUFTot
                                {
                                    VDif = 1.22,
                                    VDevTrib = 1.29,
                                    VIBSUF = 11.58
                                },
                                GIBSMun = new GIBSMunTot
                                {
                                    VDif = 1.77,
                                    VDevTrib = 1.99,
                                    VIBSMun = 155.89
                                },
                                VIBS = 1.17,
                                VCredPres = 117.40,
                                VCredPresCondSus = 1.59
                            },
                            GCBS = new GCBSTot
                            {
                                VDif = 22.22,
                                VDevTrib = 23.33,
                                VCBS = 44.44,
                                VCredPres = 55.55,
                                VCredPresCondSus = 66.66
                            }
                        },
                        VTotDFe = 1000.55
                    },
                    GFat = new GFat
                    {
                        CompetFat = "202411",
                        DVencFat = DateTime.Now,
                        DApresFat = DateTime.Now,
                        DProxLeitura = DateTime.Now,
                        CodBarras = "12345678901234567890",
                        CodDebAuto = "222",

                        EnderCorresp = new EnderCorresp
                        {
                            XLgr = "Rua da Silva",
                            Nro = "1",
                            XBairro = "Jardim Ipe",
                            CMun = "1234567",
                            XMun = "Outro",
                            CEP = "87777001",
                            UF = UFBrasil.PR,
                            Email = "1teste1@1teste1.com.br"
                        }
                    },
                    GANEEL = new GANEEL
                    {
                        GHistFat = new List<GHistFat>
                        {
                            new GHistFat
                            {
                                XGrandFat = "TESTE UNICO",
                                GGrandFat = new List<GGrandFat>
                                {
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "01"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "02"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "03"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "04"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "05"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "06"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "07"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "08"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "09"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "10"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "11"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "12"
                                    },
                                    new GGrandFat
                                    {
                                        CompetFat = "202411",
                                        VFat = 333.3,
                                        UMed = UnidadeMedidaEnergia.KW,
                                        QtdDias = "13"
                                    }
                                }
                            }
                        }
                    },
                    AutXML = new List<AutXML>
                    {
                        new AutXML
                        {
                            CNPJ = "06117473000150"
                        },
                        new AutXML
                        {
                            CPF = "12345678901"
                        }
                    },
                    InfAdic = new InfAdicNF3e
                    {
                        InfAdFisco = "TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE TESTE",
                        InfCpl = new List<string>
                        {
                            "TESTE do InfCpl",
                            "TESTE2 do InfCpl"
                        }
                    },
                    GRespTec = new GRespTec
                    {
                        CNPJ = "06117473000150",
                        XContato = "Wandrey Mundin Ferreria",
                        Email = "wandrey@unimake.com.br",
                        Fone = "04431414900"
                    }
                }
            };

            return xml;
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(TipoAmbiente.Homologacao, @"..\..\..\NF3e\Resources\nota_energia-nf3e.xml")]
        [InlineData(TipoAmbiente.Producao, @"..\..\..\NF3e\Resources\nota_energia-nf3e.xml")]
        public void EnviarNF3eSincronoXml(TipoAmbiente tipoAmbiente, string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var nf3eSincrono = new Business.DFe.Xml.NF3e.NF3e();
            var nf3eSincronoObjeto = nf3eSincrono.LerXML<Business.DFe.Xml.NF3e.NF3e>(doc);

            var autorizarNf3eSincrono = new AutorizacaoSinc(nf3eSincronoObjeto, configuracao);
            autorizarNf3eSincrono.Executar();
        }

        [Theory]
        [Trait("DFe", "NF3e")]
        [InlineData(TipoAmbiente.Homologacao, @"..\..\..\NF3e\Resources\NF3e_RTC.xml")]
        public void ValidarNF3eReformaTributaria(TipoAmbiente tipoAmbiente, string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");
            
            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var nf3eSincrono = new Business.DFe.Xml.NF3e.NF3e();
            var nf3eSincronoObjeto = nf3eSincrono.LerXML<Business.DFe.Xml.NF3e.NF3e>(doc);

            var autorizarNf3eSincrono = new AutorizacaoSinc(nf3eSincronoObjeto, configuracao);
        }
    }
}
