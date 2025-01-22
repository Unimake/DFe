using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        [InlineData(UFBrasil.TO, TipoAmbiente.Producao)]
        public void EnviarNF3eSincrono(UFBrasil ufBrasil, TipoAmbiente tipoAmbiente)
        {
            var conteudoXML = MontarXMLNF3e(ufBrasil, tipoAmbiente);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.NF3e,
                TipoEmissao = TipoEmissao.ContingenciaOfflineNF3e,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var autorizacaoSincNF3e = new AutorizacaoSinc(conteudoXML, configuracao);
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
                        VerProc = "verProc1"
                    },
                    Emit = new Emit
                    {
                        CNPJ = "06117473000150",
                        IE = "9032000301",
                        XNome = "Unimake Solucoes Corporativas",
                        XFant = "Unimake Software",
                        EnderEmit = new EnderEmit
                        {
                            XLgr = "Rua Paulo Antônio da Costa",
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
                            XBairro = "Jardim Ipê",
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
                                TpPosTar = TipoPostoTarifario.Unico
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
                        VNF = 111111.45
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
                            XBairro = "Jardim Ipê",
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
    }
}
