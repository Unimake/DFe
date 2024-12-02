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
