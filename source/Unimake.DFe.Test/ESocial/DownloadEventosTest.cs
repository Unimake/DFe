using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Xml.ESocial;
using Xunit;

namespace Unimake.DFe.Test.ESocial
{
    public class DownloadEventosESocial
    {
        /// <summary>
        /// Testar o Download de Eventos Por ID
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialDownloadEventosPorID(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.DownloadEventosPorID
            {
                Download = new Download
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    SolicitacaoDownloadPorId = new SolicitacaoDownloadPorId
                    {
                        Id = "ID123908129312894812"
                    }
                }
            };

            var DownloadEvtsID = new Business.DFe.Servicos.ESocial.DownloadPorID(conteudoXML, configuracao);
            DownloadEvtsID.Executar();
        }

        /// <summary>
        /// Testar o Download de Eventos Por NrRec
        /// </summary>
        [Theory]
        [Trait("DFe", "ESocial")]
        [InlineData(TipoAmbiente.Producao)]
        [InlineData(TipoAmbiente.Homologacao)]
        public void ESocialDownloadEventosPorNrRec(TipoAmbiente tipoAmbiente)
        {
            var configuracao = new Configuracao
            {
                TipoDFe = TipoDFe.ESocial,
                TipoEmissao = TipoEmissao.Normal,
                TipoAmbiente = tipoAmbiente,
                Servico = Servico.ESocialDownloadEvts,
                CertificadoDigital = PropConfig.CertificadoDigital,
            };

            var conteudoXML = new Business.DFe.Xml.ESocial.DownloadEventosPorNrRec
            {
                Download = new Download
                {
                    IdeEmpregador = new IdeEmpregador
                    {
                        TpInsc = TiposInscricao.CNPJ,
                        NrInsc = "06117473000150"
                    },
                    SolicitacaoDownloadPorNrRec = new SolicitacaoDownloadPorNrRec
                    {
                        NrRec = "12312312"
                    }

                }
            };

            var DownloadEvtsNrRec = new Business.DFe.Servicos.ESocial.DownloadPorNrRec(conteudoXML, configuracao);
            DownloadEvtsNrRec.Executar();
        }

    }
}