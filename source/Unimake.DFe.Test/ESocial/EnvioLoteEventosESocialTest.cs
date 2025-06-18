using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Servicos.ESocial;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class EnvioLoteEventosESocialTest
    {
        /// <summary>
        /// Testar o envio do evento S-1000
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao, "v_S_01_03_00")]
        [InlineData(TipoAmbiente.Homologacao, "v_S_01_03_00")]
        public void ESocialEnvioLoteEventosS1000(TipoAmbiente tipoAmbiente, string versaoSchema)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ESocialEnvioLoteEventos
            {
                Versao = "1.1.0",
                VersaoSchema = versaoSchema,
                EnvioLoteEventos = new EnvioLoteEventosESocial
                {
                    Grupo = "1",
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473"
                    },

                    IdeTransmissor = new IdeTransmissor
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },

                    Eventos = new EventosESocial
                    {
                        Evento = new List<EventoESocial>
                        {
                            new()
                            {
                                ID = "ID1061174730000002024081911021200001",
                                ESocial1000 = new ESocial1000
                                {
                                    EvtInfoEmpregador = new EvtInfoEmpregador
                                    {
                                        ID = "ID1061174730000002024081911021200001",
                                        IdeEvento = new IdeEvento1000
                                        {
                                            TpAmb = TipoAmbiente.Homologacao,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "1.1.0"
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473"
                                        },
                                        InfoEmpregador = new InfoEmpregador
                                        {
                                            Inclusao = new InclusaoE1000
                                            {
                                                IdePeriodo = new IdePeriodo
                                                {
                                                    IniValid = DateTime.Parse("2021-01-01"),
                                                    FimValid = DateTime.Parse("2021-01-02"),
                                                },
                                                InfoCadastro = new InfoCadastro
                                                {
                                                    ClassTrib = ClassificacaoTributaria.AdministracaoDiretaUniao,
                                                    IndCoop = IndCoop.NaoCooperativa,
                                                    IndConstr = IndConstr.EmpresaConstrutora,
                                                    IndDesFolha = IndDesFolha.NaoAplicavel,
                                                    IndOptRegEletron = IndOptRegEletron.NaoOptou,
                                                    DadosIsencao = new DadosIsencao
                                                    {
                                                        IdeMinLei = "str1234",
                                                        NrCertif = "56156178151",
                                                        DtEmisCertif = DateTime.Parse("2021-01-01"),
                                                        DtVencCertif = DateTime.Parse("2021-12-31"),
                                                        NrProtRenov = "1565458",
                                                        DtProtRenov = DateTime.Parse("2021-06-20"),
                                                        DtDou = DateTime.Parse("2021-05-20"),
                                                        PagDou = "123"
                                                    },
                                                    InfoOrgInternacional = new Business.DFe.Xml.ESocial.InfoOrgInternacional
                                                    {
                                                        IndAcordoIsenMulta = IndAcordoIsenMulta.ComAcordo
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

            var enviarLoteEventosESocial = new Business.DFe.Servicos.ESocial.EnviarLoteEventosESocial(conteudoXML, configuracao);
            enviarLoteEventosESocial.Executar();
        }

        /// <summary>
        /// Testar o envio do evento S-1200
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialEnvioLoteEventosS1200(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ESocialEnvioLoteEventos
            {
                Versao = "1.1.0",
                EnvioLoteEventos = new EnvioLoteEventosESocial
                {
                    Grupo = "3",
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473"
                    },

                    IdeTransmissor = new IdeTransmissor
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },

                    Eventos = new EventosESocial
                    {
                        Evento = new List<EventoESocial>
                        {
                            new() {
                                ID = "ID1061174730000002017102608080800001",
                                ESocial1200 = new ESocial1200
                                {
                                    EvtRemun = new EvtRemun
                                    {
                                        ID = "ID1061174730000002017102608080800001",
                                        IdeEvento = new IdeEvento1200
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            IndApuracao = IndApuracao.Mensal,
                                            PerApur = "2017-10",
                                            TpAmb = TipoAmbiente.Homologacao,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "1.0",
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473"
                                        },
                                        IdeTrabalhador =  new IdeTrabalhador
                                        {
                                            CpfTrab = "11111111111",
                                            InfoMV = new InfoMV1200
                                            {
                                                IndMV = IndMV.DescontoSobreRemuneracao,
                                                RemunOutrEmpr = new List<RemunOutrEmpr>
                                                {
                                                    new()
                                                    {
                                                        TpInsc = TiposInscricao.CNPJ,
                                                        NrInsc = "06117473",
                                                        CodCateg = CodCateg.EmpregadoAprendiz,
                                                        VlrRemunOE = 123.45
                                                    }
                                                }
                                            },
                                            InfoComplem = new InfoComplem
                                            {
                                                NmTrab = "nome",
                                                DtNascto = DateTime.Parse("2012-12-13")
                                            },
                                            ProcJudTrab = new List<ProcJudTrab1200>
                                            {
                                               new()
                                               {
                                                   TpTrib = TpTrib.IRRF,
                                                   NrProcJud = "00000000000000000001",
                                                   CodSusp = "1234"
                                               }
                                            }
                                        },
                                        DmDev = new List<DmDev>
                                        {
                                            new()
                                            {
                                                IdeDmDev = "teste",
                                                CodCateg = CodCateg.EmpregadoAprendiz,
                                                InfoPerApur = new InfoPerApur
                                                {
                                                    IdeEstabLot = new List<IdeEstabLot>
                                                    {
                                                        new IdeEstabLot()
                                                        {
                                                            TpInsc = TpInsc.CNPJ,
                                                            NrInsc = "12345678901234",
                                                            Codlotacao = "148",
                                                            QtdDiasAv = "1",
                                                            RemunPerApur = new List<RemunPerApur1200>
                                                            {
                                                                new()
                                                                {
                                                                    Matricula = "teste1",
                                                                    ItensRemun = new List<ItensRemun1200>
                                                                    {
                                                                        new()
                                                                        {
                                                                            CodRubr = "123",
                                                                            IdeTabRubr = "teste1",
                                                                            VrRubr = 123.45
                                                                        }
                                                                    },
                                                                    InfoAgNocivo = new InfoAgNocivo1200
                                                                    {
                                                                        GrauExp = "123"
                                                                    }
                                                                }
                                                            }
                                                        },
                                                        new IdeEstabLot()
                                                        {
                                                            TpInsc = TpInsc.CNPJ,
                                                            NrInsc = "12345678901234",
                                                            Codlotacao = "205",
                                                            QtdDiasAv = "1",
                                                            RemunPerApur = new List<RemunPerApur1200>
                                                            {
                                                                new()
                                                                {
                                                                    Matricula = "teste1",
                                                                    ItensRemun = new List<ItensRemun1200>
                                                                    {
                                                                        new()
                                                                        {
                                                                            CodRubr = "123",
                                                                            IdeTabRubr = "teste1",
                                                                            VrRubr = 123.45
                                                                        }
                                                                    },
                                                                    InfoAgNocivo = new InfoAgNocivo1200
                                                                    {
                                                                        GrauExp = "123"
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                },
                                                InfoComplCont = new InfoComplCont
                                                {
                                                    CodCBO = "14",
                                                    NatAtividade = NatAtividade.TrabalhoUrbano,
                                                    QtdDiasTrab = "2"
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

            var enviarLoteEventosESocial = new EnviarLoteEventosESocial(conteudoXML, configuracao);
            enviarLoteEventosESocial.Executar();
        }

        /// <summary>
        /// Testar o envio do evento S-1005
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialEnvioLoteEventosXML(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var xml = new XmlDocument();
            xml.Load("..\\..\\..\\ESocial\\Resources\\EnvioLoteEventos-esocial-loteevt.xml");

            var eSocialEnvioLoteEventos = new ESocialEnvioLoteEventos();
            var conteudoXML = eSocialEnvioLoteEventos.LerXML<ESocialEnvioLoteEventos>(xml);

            var enviarLoteEventosESocial = new EnviarLoteEventosESocial(conteudoXML, configuracao);
            enviarLoteEventosESocial.Executar();
        }

        /// <summary>
        /// Testar o envio do evento S-1005
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao, "..\\..\\..\\ESocial\\Resources\\EnvioLoteEventos-esocial-loteevt.xml")]
        [InlineData(TipoAmbiente.Homologacao, "..\\..\\..\\ESocial\\Resources\\S_01_03_00\\EnvioLoteEventos-esocial-loteevt.xml")]
        public void ESocialEnvioLoteEventosLoadFrom(TipoAmbiente tipoAmbiente, string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização do teste.");

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var eSocialEnvioLoteEventos = new ESocialEnvioLoteEventos();
            eSocialEnvioLoteEventos = eSocialEnvioLoteEventos.LoadFromFile(arqXML);

            var enviarLoteEventosESocial = new EnviarLoteEventosESocial(eSocialEnvioLoteEventos, configuracao);
            enviarLoteEventosESocial.Executar();

            var xmlAssinado = enviarLoteEventosESocial.ConteudoXMLAssinado;
        }

        /// <summary>
        /// Testar o envio do lote de eventos S2400
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialEnvioLoteEventosS2400(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoLoteEvento2400 = new ESocialEnvioLoteEventos
            {
                Versao = "1.1.0",
                EnvioLoteEventos = new EnvioLoteEventosESocial
                {
                    Grupo = "2",
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    IdeTransmissor = new IdeTransmissor
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    Eventos = new EventosESocial
                    {
                        Evento = new List<EventoESocial>
                        {
                            new()
                            {
                                ID = "ID1000000000000002017102608080800001",
                                ESocial2400 = new ESocial2400
                                {
                                    EvtCdBenefIn = new EvtCdBenefIn
                                    {
                                        ID = "ID1000000000000002017102608080800001",
                                        IdeEvento = new IdeEvento2400
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            NrRecibo = "1.1.11111111111111",
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "Versao Teste"
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473000150"
                                        },
                                        Beneficiario = new Beneficiario
                                        {
                                            CpfBenef = "12345678901",
                                            NmBenefic = "Joao da Silva",
                                            DtNascto = DateTime.ParseExact("1990-02-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                            DtInicio = DateTime.ParseExact("2025-01-01", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                            Sexo = TipoSexo.Masculino,
                                            RacaCor = RacaCor.Branca,
                                            EstCiv = EstadoCivil.Viuvo,
                                            IncFisMen = SimNaoLetra.Nao,
                                            DtIncFisMen = DateTime.Now,
                                            Endereco = new Endereco2400
                                            {
                                                Brasil = new Brasil
                                                {
                                                    TpLograd = "Rua",
                                                    DscLograd = "De terra",
                                                    NrLograd = "1",
                                                    Complemento = "Fundo",
                                                    Bairro = "Graao",
                                                    Cep = "12345678",
                                                    CodMunic = "1234567",
                                                    Uf = "PR"
                                                }
                                            },
                                            Dependente = new List<Dependente2400>
                                            {
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Jose dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
                                                },
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Maria dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    SexoDep = TipoSexo.Feminino,
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
                                                },
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Jorge dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
                                                }
                                            }
                                        }
                                    }
                                }
                            },
                            new()
                            {
                                ID = "ID1000000000000002017102608080800002",
                                ESocial2400 = new ESocial2400
                                {
                                    EvtCdBenefIn = new EvtCdBenefIn
                                    {
                                        ID = "ID1000000000000002017102608080800002",
                                        IdeEvento = new IdeEvento2400
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            NrRecibo = "1.1.11111111111111",
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "Versao Teste"
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473000150"
                                        },
                                        Beneficiario = new Beneficiario
                                        {
                                            CpfBenef = "12345678901",
                                            NmBenefic = "Joao da Silva",
                                            DtNascto = DateTime.ParseExact("1990-02-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                            DtInicio = DateTime.ParseExact("2025-01-01", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                            Sexo = TipoSexo.Masculino,
                                            RacaCor = RacaCor.Branca,
                                            EstCiv = EstadoCivil.Viuvo,
                                            IncFisMen = SimNaoLetra.Nao,
                                            DtIncFisMen = DateTime.Now,
                                            Endereco = new Endereco2400
                                            {
                                                Brasil = new Brasil
                                                {
                                                    TpLograd = "Rua",
                                                    DscLograd = "De terra",
                                                    NrLograd = "1",
                                                    Complemento = "Fundo",
                                                    Bairro = "Graao",
                                                    Cep = "12345678",
                                                    CodMunic = "1234567",
                                                    Uf = "PR"
                                                }
                                            },
                                            Dependente = new List<Dependente2400>
                                            {
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Jose dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
                                                },
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Maria dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    SexoDep = TipoSexo.Feminino,
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
                                                },
                                                new Dependente2400
                                                {
                                                    TpDep = TiposDeDependente.FilhoOuEnteado,
                                                    NmDep = "Jorge dos Santos",
                                                    DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                    CpfDep = "987654321098",
                                                    DepIRRF = SimNaoLetra.Sim,
                                                    IncFisMen = SimNaoLetra.Sim
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

            var enviarLoteEventosESocial = new EnviarLoteEventosESocial(conteudoLoteEvento2400, configuracao);
            enviarLoteEventosESocial.Executar();
        }

        /// <summary>
        /// Testar o envio do lote de eventos S2405
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialEnvioLoteEventosS2405(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoLoteEvento2405 = new ESocialEnvioLoteEventos
            {
                Versao = "1.1.0",
                EnvioLoteEventos = new EnvioLoteEventosESocial
                {
                    Grupo = "2",
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    IdeTransmissor = new IdeTransmissor
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    Eventos = new EventosESocial
                    {
                        Evento = new List<EventoESocial>
                        {
                            new()
                            {
                                ID = "ID1000000000000002017102608080800001",
                                ESocial2405 = new ESocial2405
                                {
                                    EvtCdBenefAlt = new EvtCdBenefAlt
                                    {
                                        ID = "ID1000000000000002017102608080800001",
                                        IdeEvento = new IdeEvento2405
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            NrRecibo = "1.1.11111111111111",
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "Versao Teste"
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473000150"
                                        },
                                        IdeBenef = new IdeBenef2405
                                        {
                                            CpfBenef = "12345678901"
                                        },
                                        Alteracao = new Alteracao2405
                                        {
                                            DtAlteracao = DateTime.Now,
                                            DadosBenef = new DadosBenef
                                            {
                                                NmBenefic = "Maria de Souza",
                                                Sexo = TipoSexo.Feminino,
                                                RacaCor = RacaCor.Branca,
                                                EstCiv = EstadoCivil.Viuvo,
                                                IncFisMen = SimNaoLetra.Nao,
                                                Endereco = new Endereco2405
                                                {
                                                    Brasil = new Brasil
                                                    {
                                                        TpLograd = "Rua",
                                                        DscLograd = "De terra",
                                                        NrLograd = "1",
                                                        Complemento = "Fundo",
                                                        Bairro = "Graao",
                                                        Cep = "12345678",
                                                        CodMunic = "1234567",
                                                        Uf = "PR"
                                                    }
                                                },
                                                Dependente = new List<Dependente>
                                                {
                                                    new Dependente
                                                    {
                                                        TpDep = TiposDeDependente.MenorPobreGuardaJudicial,
                                                        NmDep = "Mario Joao",
                                                        DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                        CpfDep = "11111111151",
                                                        SexoDep = TipoSexo.Feminino,
                                                        DepIRRF = SimNaoLetra.Nao,
                                                        IncFisMen = SimNaoLetra.Nao,
                                                        DescrDep = "Teste"
                                                    },
                                                    new Dependente
                                                    {
                                                        TpDep = TiposDeDependente.MenorPobreGuardaJudicial,
                                                        NmDep = "Mario Jorge",
                                                        DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                        CpfDep = "11111111155",
                                                        SexoDep = TipoSexo.Masculino,
                                                        DepIRRF = SimNaoLetra.Nao,
                                                        IncFisMen = SimNaoLetra.Nao,
                                                        DescrDep = "Teste"
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            },
                            new()
                            {
                                ID = "ID1000000000000002017102608080800002",
                                ESocial2405 = new ESocial2405
                                {
                                    EvtCdBenefAlt = new EvtCdBenefAlt
                                    {
                                        ID = "ID1000000000000002017102608080800002",
                                        IdeEvento = new IdeEvento2405
                                        {
                                            IndRetif = IndicativoRetificacao.ArquivoOriginal,
                                            NrRecibo = "1.1.11111111111111",
                                            TpAmb = tipoAmbiente,
                                            ProcEmi = ProcEmiESocial.AppDoEmpregador,
                                            VerProc = "Versao Teste"
                                        },
                                        IdeEmpregador = new IdeEmpregador
                                        {
                                            TpInsc = TiposInscricao.CNPJ,
                                            NrInsc = "06117473000150"
                                        },
                                        IdeBenef = new IdeBenef2405
                                        {
                                            CpfBenef = "12345678901"
                                        },
                                        Alteracao = new Alteracao2405
                                        {
                                            DtAlteracao = DateTime.Now,
                                            DadosBenef = new DadosBenef
                                            {
                                                NmBenefic = "Maria de Souza",
                                                Sexo = TipoSexo.Feminino,
                                                RacaCor = RacaCor.Branca,
                                                EstCiv = EstadoCivil.Viuvo,
                                                IncFisMen = SimNaoLetra.Nao,
                                                Endereco = new Endereco2405
                                                {
                                                    Brasil = new Brasil
                                                    {
                                                        TpLograd = "Rua",
                                                        DscLograd = "De terra",
                                                        NrLograd = "1",
                                                        Complemento = "Fundo",
                                                        Bairro = "Graao",
                                                        Cep = "12345678",
                                                        CodMunic = "1234567",
                                                        Uf = "PR"
                                                    }
                                                },
                                                Dependente = new List<Dependente>
                                                {
                                                    new Dependente
                                                    {
                                                        TpDep = TiposDeDependente.MenorPobreGuardaJudicial,
                                                        NmDep = "Mario Joao",
                                                        DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                        CpfDep = "11111111151",
                                                        SexoDep = TipoSexo.Masculino,
                                                        DepIRRF = SimNaoLetra.Nao,
                                                        IncFisMen = SimNaoLetra.Nao,
                                                        DescrDep = "Teste"
                                                    },
                                                    new Dependente
                                                    {
                                                        TpDep = TiposDeDependente.MenorPobreGuardaJudicial,
                                                        NmDep = "Mario Jorge",
                                                        DtNascto = DateTime.ParseExact("2024-05-05", "yyyy-MM-dd", CultureInfo.InvariantCulture),
                                                        CpfDep = "11111111155",
                                                        SexoDep = TipoSexo.Masculino,
                                                        DepIRRF = SimNaoLetra.Nao,
                                                        IncFisMen = SimNaoLetra.Nao,
                                                        DescrDep = "Teste"
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

            var enviarLoteEventosESocial = new EnviarLoteEventosESocial(conteudoLoteEvento2405, configuracao);
            enviarLoteEventosESocial.Executar();
        }

    }
}
