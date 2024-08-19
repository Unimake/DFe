using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class EnvioLoteEventosESocialTest
    {
        /// <summary>
        /// Testar a consulta lote assincrono do eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialEnvioLoteEventos(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialEnviarLoteEventos,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.ESocialEnvioLoteEventos
            {
                Versao = "1.1.0",
                EnvioLoteEventos = new Business.DFe.Xml.ESocial.EnvioLoteEventosESocial
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
                        new EventoESocial
                        {
                            ID = "ID1061174730000002024081911021200001",
                            ESocial1000 = new ESocial1000
                            {
                                EvtInfoEmpregador = new EvtInfoEmpregador
                                {
                                    ID = "ID1061174730000002024081911021200001",
                                    IdeEvento = new IdeEvento
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
    }
}
