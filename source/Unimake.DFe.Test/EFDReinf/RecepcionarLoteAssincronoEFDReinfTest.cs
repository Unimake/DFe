using System;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    public class RecepcionarLoteAssincronoEFDReinfTest
    {
        /// <summary>
        /// Testar a consulta lote assincrono do EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678945687"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                            {
                                new EventoReinf
                            {
                                ID = "ID1000000000000002021052608080800001",
                                Reinf1000 = new Reinf1000
                                {
                                    EvtInfoContri = new EvtInfoContri
                                    {
                                        ID = "ID1000000000000002021052608080654321",
                                        IdeEvento = new IdeEvento
                                        {
                                            TpAmb = TipoAmbiente.Homologacao,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "150"
                                        },
                                        IdeContri = new IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678945687"
                                        },
                                        InfoContri = new InfoContri
                                        {
                                            Inclusao = new Inclusao1000
                                            {
                                                IdePeriodo = new IdePeriodo
                                                {
                                                    IniValid = "2021-05",
                                                },
                                                InfoCadastro = new InfoCadastro
                                                {
                                                    ClassTrib = ClassificacaoTributaria.PessoaJuridica,
                                                    IndEscrituracao = IndicativoEscrituracao.Obrigada,
                                                    IndDesoneracao = IndicativoDesoneracao.Aplicavel,
                                                    IndAcordoIsenMulta = IndicativoIsencaoMulta.ComAcordo,
                                                    IndSitPJ = IndicativoSituacaoPJ.Extincao,
                                                    IndUniao = IndicativoUniao.NaoAplicavel,
                                                    DtTransfFinsLucr = DateTime.Parse("2021-01-01"),
                                                    DtObito = DateTime.Parse("2021-01-01"),
                                                    Contato = new Contato
                                                    {
                                                        NmCtt = "NMCTT1",
                                                        CpfCtt = "12345678954",
                                                        FoneFixo = "4412347894",
                                                        FoneCel = "44912347894",
                                                        Email = "email@email.com"
                                                    },
                                                    SoftHouse = new List<SoftHouse>
                                                    {
                                                        new SoftHouse
                                                        {
                                                            CnpjSoftHouse = "12345678945687",
                                                            NmRazao = "nomeContribuinte",
                                                            NmCont = "Nome Contato",
                                                            Telefone = "4498749874",
                                                            Email = "email@email.com"
                                                        }
                                                    },
                                                    InfoEFR = new InfoEFR
                                                    {
                                                        IdeEFR = SimNaoLetra.Sim,
                                                        CnpjEFR = "11122233344488"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var recepcionarLoteAssincReinf = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            recepcionarLoteAssincReinf.Executar();
        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf2050
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf2050(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678901234"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1000000000000002021061608080800001",
                                Reinf2050 = new Reinf2050
                                {
                                    EvtComProd = new EvtComProd
                                    {
                                        ID = "ID1000000000000002021061608080800001",
                                        IdeEvento = new IdeEvento2050
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            PerApur = DateTime.Parse("2021-05"),
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "1.0"
                                        },
                                        IdeContri = new IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678901234"
                                        },
                                        InfoComProd = new InfoComProd
                                        {
                                            IdeEstab = new IdeEstab2050
                                            {
                                                TpInscEstab = TipoInscricaoEstabelecimento.CNPJ,
                                                NrInscEstab = "12345678901234",
                                                VlrRecBrutaTotal = 1000.00,
                                                VlrCPApur = 100.00,
                                                VlrRatApur = 50.00,
                                                VlrSenarApur = 25.00,
                                                VlrCPSuspTotal = 0.00,
                                                VlrRatSuspTotal = 0.00,
                                                VlrSenarSuspTotal = 0.00,
                                                TipoCom = new List<TipoCom>
                                                {
                                                    new TipoCom
                                                    {
                                                        IndCom = IndicativoComercializacao.ProdutorRuralPJ,
                                                        VlrRecBruta = 1000.00,
                                                        InfoProc = new List<InfoProc2050>
                                                        {
                                                            new InfoProc2050
                                                            {
                                                                TpProc = TipoProcesso.Judicial,
                                                                NrProc = "123456789012345678901",
                                                                CodSusp = "12345678901234",
                                                                VlrCPSusp = 0.00,
                                                                VlrRatSusp = 0.00,
                                                                VlrSenarSusp = 0.00
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();
        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf2055
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf2055(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678901234"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1000000000000002021052608080800001",
                                Reinf2055 = new Reinf2055
                                {
                                    EvtAqProd = new EvtAqProd
                                    {
                                        ID = "ID1000000000000002021052608080800001",
                                        IdeEvento = new IdeEvento2055
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            PerApur = DateTime.Parse("2021-06"),
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "1.0"
                                        },
                                        IdeContri = new IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678901234"
                                        },
                                        InfoAquisProd = new InfoAquisProd
                                        {
                                            IdeEstabAdquir = new IdeEstabAdquir
                                            {
                                                TpInscAdq = TipoInscricaoAdquirente.CNPJ,
                                                NrInscAdq = "12345678901234",
                                                IdeProdutor = new IdeProdutor
                                                {
                                                    TpInscProd = TiposInscricao.CNPJ,
                                                    NrInscProd = "12345678901234",
                                                    IndOpcCP = "S",
                                                    DetAquis = new List<DetAquis>
                                                    {
                                                        new DetAquis
                                                        {
                                                            IndAquis = IndAquis.AquisicaoIsentaPjPaa,
                                                            VlrBruto = 1000.00,
                                                            VlrCPDescPR = 100.00,
                                                            VlrRatDescPR = 50.00,
                                                            VlrSenarDesc = 25.00,
                                                            InfoProcJud = new List<InfoProcJud>
                                                            {
                                                                new InfoProcJud
                                                                {
                                                                    NrProcJud = "123456789012345678901",
                                                                    CodSusp = "12345678901234",
                                                                    VlrCPNRet = 0.00,
                                                                    VlrRatNRet = 0.00,
                                                                    VlrSenarNRet = 0.00
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();
        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf2098
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf2098(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678945687"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                        new EventoReinf
                        {
                            ID = "ID1000000000000002025061600000000001",
                            Reinf2098 = new Reinf2098
                            {
                                EvtReabreEvPer = new EvtReabreEvPer
                                {
                                    ID = "ID1000000000000002025061600000000002",
                                    IdeEvento = new IdeEvento2098
                                    {
                                        PerApur = DateTime.Parse("2021-06"),
                                        TpAmb = tipoAmbiente,
                                        ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                        VerProc = "150"
                                    },
                                    IdeContri = new IdeContri
                                    {
                                        TpInsc = TiposInscricao.CNPJ,
                                        NrInsc = "12345678945687"
                                    }   
                                }
                            }
                        }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();

        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf2099
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf2099(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678945687"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1000000000000002025061600000000001",
                                Reinf2099 = new Reinf2099
                                {
                                    EvtFechaEvPer = new EvtFechaEvPer
                                    {
                                        ID = "ID1000000000000002025061600000000002",
                                        IdeEvento = new IdeEvento2099
                                        {
                                            PerApur = DateTime.Parse("2025-06"),
                                            TpAmb   = tipoAmbiente,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "150"
                                        },
                                        IdeContri = new IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678945687"
                                        },
                                        IdeRespInf = new IdeRespInf2099
                                        {
                                            NmResp   = "Fulano de Tal",
                                            CpfResp  = "12345678901",
                                            Telefone = "41999999999",
                                            Email    = "fulano@example.com"
                                        },
                                        InfoFech = new InfoFech
                                        {
                                            EvtServTm     = SimNaoLetra.Sim,
                                            EvtServPr     = SimNaoLetra.Sim,
                                            EvtAssDespRec = SimNaoLetra.Nao,
                                            EvtAssDespRep = SimNaoLetra.Nao,
                                            EvtComProd    = SimNaoLetra.Sim,
                                            EvtCPRB       = SimNaoLetra.Sim,
                                            EvtAquis      = SimNaoLetra.Sim
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();

        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf4010
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf4010(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678945687"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1183132710000002024110716402200002",
                                Reinf4010 = new Reinf4010
                                {
                                    EvtRetPF = new EvtRetPF
                                    {
                                        ID = "ID1183132710000002024110716402200002",
                                        IdeEvento = new IdeEvento4010
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal, 
                                            PerApur   = DateTime.Parse("2024-11"),
                                            TpAmb     = tipoAmbiente,                       
                                            ProcEmi   = ProcessoEmissaoReinf.AplicativoContribuinte, 
                                            VerProc   = "UNICO V8.0"
                                        },
                                        IdeContri = new IdeContri4010
                                        {
                                            TpInsc = TiposInscricao.CNPJ,   
                                            NrInsc = "18313271"
                                        },
                                        IdeEstab = new IdeEstab4010
                                        {
                                            TpInscEstab = TipoInscricaoEstabelecimento.CNPJ,      
                                            NrInscEstab = "18313271000162",
                                            IdeBenef    = new IdeBenef
                                            {
                                                CpfBenef = "15831382850",
                                                IdePgto = new List<IdePgto>
                                                {
                                                    new IdePgto
                                                    {
                                                        NatRend = "16005",
                                                        InfoPgto = new List<InfoPgto>
                                                        {
                                                            new InfoPgto
                                                            {
                                                                DtFG         = DateTime.Parse("2024-11-06"),
                                                                VlrRendTrib  = 200.00,
                                                                PercSCP      = 0.0
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();

        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf4020
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf4020(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1000000000000002021052608080800001", 
                                Reinf4020 = new Reinf4020
                                {
                                    EvtRetPJ = new EvtRetPJ
                                    {
                                        ID = "ID1000000000000002021052608080800001",
                                        IdeEvento = new IdeEvento4020
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            PerApur  = DateTime.Parse("2022-05"),
                                            TpAmb    = tipoAmbiente,                      
                                            ProcEmi  = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc  = "verProc1"
                                        },
                                        IdeContri = new IdeContri4020
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473000150",
                                            InfoComplContri = new InfoComplContri
                                            {
                                                NatJur = "1123"
                                            }
                                        },
                                        IdeEstab = new IdeEstab4020
                                        {
                                            TpInscEstab = TipoInscricaoEstabelecimento.CNPJ,
                                            NrInscEstab = "00000000000000",
                                            IdeBenef = new IdeBenef4020
                                            {
                                                CnpjBenef   = "00000000000000",
                                                NmBenef     = "nmBenef1",
                                                IsenImun    = (IsencaoEImunidade)2,
                                                IdePgto = new List<IdePgto4020>
                                                {
                                                    new IdePgto4020
                                                    {
                                                        NatRend = "10001",
                                                        Observ  = "observ1",
                                                        InfoPgto = new List<InfoPgto4020>
                                                        {
                                                            new InfoPgto4020
                                                            {
                                                                DtFG         = DateTime.Parse("2022-01-01"),
                                                                VlrBruto     = 1.00,
                                                                IndFciScp    = IndicativoFundoDeInvestimento.FCI,
                                                                NrInscFciScp = "00000000000000",
                                                                PercSCP      = 10.0,
                                                                IndJud       = SimNaoLetra.Sim,
                                                                PaisResidExt = "001",
                                                                Retencoes = new List<Retencoes>
                                                                {
                                                                    new Retencoes
                                                                    {
                                                                        VlrBaseIR     = 1.00,
                                                                        VlrIR         = 1.00,
                                                                        VlrBaseAgreg  = 1.00,
                                                                        VlrAgreg      = 1.00,
                                                                        VlrBaseCSLL   = 1.00,
                                                                        VlrCSLL       = 1.00,
                                                                        VlrBaseCofins = 1.00,
                                                                        VlrCofins     = 1.00,
                                                                        VlrBasePP     = 1.00,
                                                                        VlrPP         = 1.00
                                                                    }
                                                                },
                                                                InfoProcRet = new List<InfoProcRet4020>
                                                                {
                                                                    new InfoProcRet4020
                                                                    {
                                                                        TpProcRet       = TipoProcesso.Administrativo,
                                                                        NrProcRet       = "12345678901234",
                                                                        CodSusp         = "123456",
                                                                        VlrBaseSuspIR   = 1.00,
                                                                        VlrNIR          = 1.00,
                                                                        VlrDepIR        = 1.00,
                                                                        VlrBaseSuspCSLL = 1.00,
                                                                        VlrNCSLL        = 1.00,
                                                                        VlrDepCSLL      = 1.00,
                                                                        VlrBaseSuspCofins = 1.00,
                                                                        VlrNCofins      = 1.00,
                                                                        VlrDepCofins    = 1.00,
                                                                        VlrBaseSuspPP   = 1.00,
                                                                        VlrNPP          = 1.00,
                                                                        VlrDepPP        = 1.00
                                                                    }
                                                                },
                                                                InfoProcJud = new InfoProcJud4020
                                                                {
                                                                    NrProc           = "12345678901234",
                                                                    IndOrigRec       = IndicativoOrigemRecursos.ProprioDeclarante,
                                                                    CnpjOrigRecurso  = "00000000000000",
                                                                    Desc             = "desc1",
                                                                    DespProcJud      = new DespProcJud
                                                                    {
                                                                        VlrDespCustas    = 1.00,
                                                                        VlrDespAdvogados = 1.00,
                                                                        IdeAdv = new List<IdeAdv>
                                                                        {
                                                                            new IdeAdv
                                                                            {
                                                                                TpInscAdv = TiposInscricao.CNPJ,
                                                                                NrInscAdv = "00000000000000",
                                                                                VlrAdv    = 1.00
                                                                            }
                                                                        }
                                                                    }
                                                                },
                                                                InfoPgtoExt = new InfoPgtoExt4020
                                                                {
                                                                    IndNIF    = IndicativoNIF.BeneficiarioComNIF,
                                                                    NifBenef  = "nifBenef1",
                                                                    RelFontPg = (RelacaoFontePagadora)500,
                                                                    FrmTribut = (FormaDeTributacao)10,
                                                                    EndExt    = new EndExt
                                                                    {
                                                                        DscLograd = "dscLograd1",
                                                                        NrLograd  = "nrLograd1",
                                                                        Complem   = "complem1",
                                                                        Bairro    = "bairro1",
                                                                        Cidade    = "cidade1",
                                                                        Estado    = "estado1",
                                                                        CodPostal = "00000000",
                                                                        Telef     = "44999999999"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();

        }

        /// <summary>
        /// Testar a consulta lote assíncrono do EFDReinf4099
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinf4099(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ReinfEnvioLoteEventos
            {
                EnvioLoteEventos = new EnvioLoteEventosReinf
                {
                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678000199"
                    },
                    Eventos = new EventosReinf
                    {
                        Evento = new List<EventoReinf>
                        {
                            new EventoReinf
                            {
                                ID = "ID1000000000000002025061600000000001",
                                Reinf4099 = new Reinf4099
                                {
                                    EvtFech = new EvtFech
                                    {
                                        ID = "ID1000000000000002025061600000000002",
                                        IdeEvento = new IdeEvento4099
                                        {
                                            PerApur = DateTime.Parse("2025-06"),
                                            TpAmb   = tipoAmbiente,
                                            ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                            VerProc = "1.0.0"
                                        },
                                        IdeContri = new IdeContri
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "12345678000199"
                                        },
                                        IdeRespInf = new IdeRespInf4099
                                        {
                                            NmResp   = "Fulano da Silva",
                                            CpfResp  = "12345678901",
                                            Telefone = "41999999999",
                                            Email    = "fulano@example.com"
                                        },
                                        InfoFech = new InfoFech4099
                                        {
                                            FechRet = FechamentoRetencao.Fechamento
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            };

            var serv = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            serv.Executar();

        }



        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void RecepcionarLoteAssincReinfXml(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfRecepcionarLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new XmlDocument();
            conteudoXML.Load(@"..\..\..\EFDReinf\Resources\loteEventosAssincrono-Reinf-loteevt.xml");

            var envioObjeto = new Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos();
            var xml = envioObjeto.LerXML<ReinfEnvioLoteEventos>(conteudoXML);

            var recepcionarLoteAssincReinf = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(xml, configuracao);
            recepcionarLoteAssincReinf.Executar();
        }
    }
}
