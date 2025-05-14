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

            var consultaLote = new Unimake.Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono(xml, configuracao);
            var reinfEnvio = reinfTeste;
            consultaLote.ReinfEnvioLoteEventos = reinfEnvio;
            consultaLote.Executar();
            consultaLote.RetornoWSXML = doc;
            consultaLote.RetornoWSString = consultaLote.RetornoWSXML.OuterXml;
            consultaLote.GravarXmlDistribuicao(@"..\..\..\EFDReinf\Resources", null);
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
                                        VerProc = "UNICO V8.0"
                                    },
                                    IdeContri = new IdeContri
                                    {
                                        TpInsc = TiposInscricao.CNPJ,
                                        NrInsc = "06117473"
                                    },
                                    InfoContri = new InfoContri
                                    {
                                        Inclusao = new Inclusao1000
                                        {
                                           IdePeriodo = new IdePeriodo
                                           {
                                               IniValid = "2024-03",
                                               FimValid = "2024-03"
                                           },
                                           InfoCadastro = new InfoCadastro
                                           {
                                               ClassTrib = ClassificacaoTributaria.Agroindustria,
                                               IndEscrituracao = IndicativoEscrituracao.Obrigada,
                                               IndDesoneracao = IndicativoDesoneracao.NaoAplicavel,
                                               IndAcordoIsenMulta = IndicativoIsencaoMulta.SemAcordo,
                                               IndSitPJ = IndicativoSituacaoPJ.Normal,
                                               Contato = new Contato
                                               {

                                                   NmCtt = "TESTE TESTE TESTE",
                                                   CpfCtt = "12345678901",
                                                   FoneFixo = "123456789012",
                                                   FoneCel = "123456789012",
                                                   Email = "TESTE@GMAIL.COM"
                                               },
                                               SoftHouse = new List<SoftHouse>
                                               {
                                                   new SoftHouse
                                                   {
                                                       CnpjSoftHouse = "06117473000150",
                                                       NmRazao = "UNIMAKE SOLUCOES CORPORATIVAS LTDA",
                                                       NmCont = "wandrey",
                                                       Telefone = "4431421010",
                                                       Email = "wandrey@unimake.com.br"
                                                   }
                                               }
                                           }
                                        }
                                    }
                                }
                            },
                        },
                        new EventoReinf
                        {
                            Reinf1050 = new Reinf1050
                            {
                                EvtTabLig = new EvtTabLig
                                {
                                    ID = "ID1000000000000002021052608080800002",
                                    IdeEvento = new IdeEvento
                                    {
                                        TpAmb = TipoAmbiente.Homologacao,
                                        ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                        VerProc = "verProc1"
                                    },
                                    IdeContri = new IdeContri
                                    {
                                        TpInsc =  TiposInscricao.CNPJ,
                                        NrInsc = "00000000000000"
                                    },
                                    InfoLig = new InfoLig
                                    {
                                        Inclusao = new Inclusao1050
                                        {
                                            IdeEntLig = new IdeEntLig
                                            {
                                                TpEntLig = TipoEntidadeLigada.FundoDeInvestimento,
                                                CnpjLig = "00000000000000",
                                                IniValid = "2022-01",
                                                FimValid = "2022-01"
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        new EventoReinf
                        {
                            Reinf2099 = new Reinf2099
                            {
                                EvtFechaEvPer = new EvtFechaEvPer
                                {
                                    ID = "ID1000000000000002021052608080800003",
                                    IdeEvento = new IdeEvento2099
                                    {
                                        PerApur = DateTime.Now,
                                        TpAmb = TipoAmbiente.Homologacao,
                                        ProcEmi = ProcessoEmissaoReinf.AplicativoContribuinte,
                                        VerProc = "verProc1"
                                    },
                                    IdeContri = new IdeContri
                                    {
                                        TpInsc = TiposInscricao.CNPJ,
                                        NrInsc = "00000000000000"
                                    },
                                    IdeRespInf = new IdeRespInf2099
                                    {
                                        NmResp = "Responsavel Nome Silva",
                                        CpfResp = "00000000000",
                                        Telefone = "044999999999",
                                        Email = "teste@teste.com"
                                    },
                                    InfoFech = new InfoFech
                                    {
                                        EvtAquis = SimNaoLetra.Sim,
                                        EvtComProd = SimNaoLetra.Sim
                                    }
                                }
                            }
                        }
                    },
                }
            }
        };

        /// <summary>
        /// Testar construtor simplificado para API
        ///</summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Homologacao)]
        [InlineData(TipoAmbiente.Producao)]
        public void ConsultarLoteAssincronoConstrutor(TipoAmbiente tipoAmbiente)
        {
            var protocolo = "2.202402.5467550";

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.EFDReinf,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital
            };

            // Utilizando o novo construtor simplificado para API
            var consultaLoteReinf = new Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono(protocolo, tipoAmbiente, configuracao);
            consultaLoteReinf.Executar();

            // Verificar se as configurações foram definidas corretamente
            Assert.Equal(Servico.EFDReinfConsultaLoteAssincrono, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.Equal(protocolo, configuracao.NumeroProtocolo);

        }
    }
}
