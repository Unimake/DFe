using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.EFDReinf;
using Xunit;

namespace Unimake.DFe.Test.EFDReinf
{
    public class RecepcionarLoteAssincronoEFDReinf
    {
        /// <summary>
        /// Testar a consulta lote assincrono do EFDReinf
        /// </summary>
        [Theory]
        [Trait("DFe", "EFDReinf")]
        [InlineData(TipoAmbiente.Producao)]
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
                Versao = "1.05.01",
                EnvioLoteEventos = new EnvioLoteEventos
                {

                    IdeContribuinte = new IdeContribuinte
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "12345678945687"
                    },
                    Eventos = new Eventos
                    {
                        Evento = new List<Evento>
                            {
                                new Evento
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
                                }
                            }
                        }
                    }
                }
            };

            var recepcionarLoteAssincReinf = new Business.DFe.Servicos.EFDReinf.RecepcionarLoteAssincrono(conteudoXML, configuracao);
            recepcionarLoteAssincReinf.Executar();
        }
    }
}
