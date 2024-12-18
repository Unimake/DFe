using System;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class ConsultaEventosESocialTest
    {
        /// <summary>
        /// Testar a consulta lote assíncrono do eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialConsultaEventosEmpregador(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialConsultaEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ConsultarEvtsEmpregadorESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    ConsultaEvtsEmpregador = new ConsultaEvtsEmpregador
                    {
                        PerApurField = "2024-01",
                        TpEvt = "S-1020"
                    }
                }
            };

            var consultaEvtsEmpregador = new Business.DFe.Servicos.ESocial.ConsultarEvtsEmpregador(conteudoXML, configuracao);
            consultaEvtsEmpregador.Executar();
        }

        /// <summary>
        /// Testar a consulta lote assíncrono do eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialConsultaEventosTrabalhador(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialConsultaEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ConsultarEvtsTrabalhadorESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    ConsultaEvtsTrabalhador = new ConsultaEvtsTrabalhador
                    {
                        CpfTrab = "07303304940",
                        DtIniField = DateTime.Now.ToString(),
                        DtFimField = DateTime.Now.AddDays(15).ToString(),
                    }
                }
            };

            var consultaEvtsTrabalhador = new Business.DFe.Servicos.ESocial.ConsultarEvtsTrabalhador(conteudoXML, configuracao);
            consultaEvtsTrabalhador.Executar();
        }

        /// <summary>
        /// Testar a consulta lote assíncrono do eSocial
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialConsultaEventosTabela(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialConsultaEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new ConsultarEvtsTabelaESocial
            {
                ConsultaIdentificadoresEvts = new ConsultaIdentificadoresEvts
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    ConsultaEvtsTabela = new ConsultaEvtsTabela
                    {
                        ChEvt = "123123",
                        TpEvt = "S-1200",
                        DtIniField = DateTime.Now.ToString(),
                        DtFimField = DateTime.Now.AddDays(15).ToString(),
                    }
                }
            };

            var consultaEvtsTabela = new Business.DFe.Servicos.ESocial.ConsultarEvtsTabela(conteudoXML, configuracao);
            consultaEvtsTabela.Executar();
        }
    }
}