using System;
using System.Collections.Generic;
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
                                                    IdeEstabLot = new IdeEstabLot
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
    }
}
