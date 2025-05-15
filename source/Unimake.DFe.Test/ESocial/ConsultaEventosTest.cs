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

        /// <summary>
        /// Teste construtor simplificado ConsutaEvtsTrabalhador
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultaEvtsTrabalhadorConstrutor(TipoAmbiente tipoAmbiente)
        {
            var tpInsc = TiposInscricao.CNPJ;
            var nrInsc = "06117473000150";
            var cpfTrab = "07303304940";

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialConsultaEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var consultaEvtsTrabalhador = new Business.DFe.Servicos.ESocial.ConsultarEvtsTrabalhador(tpInsc, nrInsc, cpfTrab, configuracao);
            consultaEvtsTrabalhador.Executar();

            Assert.Equal(Servico.ESocialConsultaEvts, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.NotNull(consultaEvtsTrabalhador.RetornoWSXML);
            Assert.NotNull(consultaEvtsTrabalhador.RetornoWSString);

        }

        /// <summary>
        /// Teste construtor simplificado ConsutaEvtsTabela
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultaEvtsTabelaConstrutor(TipoAmbiente tipoAmbiente)
        {
            var tpInsc = TiposInscricao.CNPJ;
            var nrInsc = "06117473000150";
            var tpEvt = "S-1200";
            var chEvt = "123123";
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialConsultaEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };
            var consultaEvtsTabela = new Business.DFe.Servicos.ESocial.ConsultarEvtsTabela(tpInsc, nrInsc, tpEvt, chEvt, configuracao);
            consultaEvtsTabela.Executar();

            Assert.Equal(Servico.ESocialConsultaEvts, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.NotNull(consultaEvtsTabela.RetornoWSXML);
            Assert.NotNull(consultaEvtsTabela.RetornoWSString);
        }

        /// <summary>
        /// Teste construtor simplificado ConsutaEvtsEmpregador
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ConsultaEvtsEmpregadorConstrutor(TipoAmbiente tipoAmbiente)
        {
            var tpInsc = TiposInscricao.CNPJ;
            var nrInsc = "06117473000150";
            var perApur = "2024-01";
            var tpEvt = "S-1020";

            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var consultaEvtsEmpregador = new Business.DFe.Servicos.ESocial.ConsultarEvtsEmpregador(
                tpInsc,
                nrInsc,
                perApur,
                tpEvt,
                configuracao);

            consultaEvtsEmpregador.Executar();

            Assert.Equal(Servico.ESocialConsultaEvts, configuracao.Servico);
            Assert.Equal((int)UFBrasil.AN, configuracao.CodigoUF);
            Assert.Equal(tipoAmbiente, configuracao.TipoAmbiente);
            Assert.NotNull(consultaEvtsEmpregador.ConteudoXMLOriginal);
            var xmlContent = consultaEvtsEmpregador.ConteudoXMLOriginal.OuterXml;

        }
    }
}