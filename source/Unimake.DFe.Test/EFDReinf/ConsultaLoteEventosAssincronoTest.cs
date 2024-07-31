using System;
using System.IO;
using System.Collections.Generic;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    /// <summary>
    /// Teste para consultar Lote Eventos Reinf
    /// </summary>
    public class ConsultaLoteEventosAssincronoTest
    {
        /// <summary>
        /// Testar a consulta lote assincrono do EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultaLoteReinf(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.EFDReinfConsultaLoteAssincrono,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsultaLoteAssincrono
            {
                ConsultaLoteAssincrono = new ConsultaLoteAssincrono
                {
                    NumeroProtocolo = "2.202402.5467550"
                }
            };

            var consultaLoteReinf = new Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono(xml, configuracao);
            consultaLoteReinf.Executar();
        }

        /// <summary>
        /// Testar a serialização e desserialização da Consulta
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(@"..\..\..\EFDReinf\Resources\Consulta_Lote-ret-reinf-consloteevt.xml")]
        public void GerarXmlDistribuicaoReinf(string arqXML)
        {
            Assert.True(File.Exists(arqXML), "Arquivo " + arqXML + " não foi localizado para a realização da serialização/desserialização.");

            var doc = new XmlDocument();
            doc.Load(arqXML);

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            var xml = new ReinfConsultaLoteAssincrono
            {
                Versao = "1.05.01",

                ConsultaLoteAssincrono = new Business.DFe.Xml.EFDReinf.ConsultaLoteAssincrono
                {
                    NumeroProtocolo = "2.202402.5467550"
                }
            };

            var teste = new Unimake.Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono(xml, configuracao);
            var reinfEnvio = reinfTeste;
            teste.ReinfEnvioLoteEventos = reinfEnvio;
            teste.Executar();
            teste.RetornoWSXML = doc;
            teste.RetornoWSString = teste.RetornoWSXML.OuterXml;
            teste.GravarXmlDistribuicao("C:\\Projetos");
        }

        public ReinfEnvioLoteEventos reinfTeste = new Business.DFe.Xml.EFDReinf.ReinfEnvioLoteEventos
        {
            EnvioLoteEventos = new EnvioLoteEventosReinf
            {
                Eventos = new EventosReinf
                {
                    Evento = new List<EventoReinf>
                    {
                        new EventoReinf
                    {
                        Reinf1000 = new Reinf1000
                        {
                            EvtInfoContri = new EvtInfoContri
                                {
                                    ID = "ID1061174730000002024052915244400001",
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
                                        Inclusao = new InclusaoReinf1000
                                        {
                                            IdePeriodo = new IdePeriodo
                                            {
                                                IniValid = "202105",
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
                            },
                        }
                    }
                }
            }
        };
    }
}
