#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Exceptions;
using System.IO;
using System.Text;
using System.Xml;
using System.Collections.Generic;
using System.Linq;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML de consulta recibo Evento EFDReinf para o web-service
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.EFDReinf.ConsultaLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class ConsultaLoteAssincrono : ServicoBase, IInteropService<ReinfConsultaLoteAssincrono>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfConsultaLoteAssincrono();
            xml = xml.LerXML<ReinfConsultaLoteAssincrono>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfConsultaLoteAssincrono;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaLoteAssincrono() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfConsultaLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaLoteAssincrono(ReinfConsultaLoteAssincrono reinfConsultaLoteAssinc, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }
            configuracao.NumeroProtocolo = reinfConsultaLoteAssinc.ConsultaLoteAssincrono.NumeroProtocolo;

            Inicializar(reinfConsultaLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaLoteAssinc)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public ReinfRetornoLoteAssincrono Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<ReinfRetornoLoteAssincrono>(RetornoWSXML);
                }

                return new ReinfRetornoLoteAssincrono
                {
                    RetornoLoteEventosAssincrono = new RetornoLoteEventosAssincrono()
                    {
                        Status = new Status()
                        {
                            CdResposta = 0,
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                        }
                    }
                };
            }
        }

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfConsultaLoteAssinc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfConsultaLoteAssincrono reinfConsultaLoteAssinc, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfConsultaLoteAssinc?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaLoteAssinc)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                Exceptions.ThrowHelper.Instance.Throw(ex);
            }
            catch (CertificadoDigitalException ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

#endif
        /// <summary>
        /// Desserialize nesta propriedade o XML do lote de eventos já assinado que foi enviado.
        /// </summary>
        public ReinfEnvioLoteEventos ReinfEnvioLoteEventos { get; set; }


        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta)");
        }

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(Stream stream, string value, Encoding encoding = null)
        {
            throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta)");
        }

        /// <summary>
        /// Gravar XML de distribuição dos eventos enviados/autorizados em uma pasta específica
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado os XMLs de distribuição dos eventos autorizados</param>
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                ReinfProc reinfProc = null;

                if (Result.RetornoLoteEventosAssincrono.Status.CdResposta != 0)
                {
                    var mensagemRetorno = Result.GerarXML();
                    base.GravarXmlDistribuicao(pasta, "testeteste.xml", mensagemRetorno.OuterXml);
                }

                foreach (var evento in ReinfEnvioLoteEventos.EnvioLoteEventos.Eventos.Evento)
                {

                    #region Reinf1000

                    if (evento.Reinf1000 != null)
                    {
                        reinfProc = new ReinfProc();
                        reinfProc.Reinf1000 = evento.Reinf1000;
                        reinfProc.ID = evento.Reinf1000.EvtInfoContri.ID;

                        foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                        {
                            if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                            {
                                reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                            }

                            base.GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                        }
                        #endregion

                        #region Reinf1050

                        if (evento.Reinf1050 != null)
                        {
                            foreach (var reinf1050 in evento.Reinf1050)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf1050 = reinf1050;
                                reinfProc.ID = reinf1050.EvtTabLig.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf1070

                        if (evento.Reinf1070 != null)
                        {
                            foreach (var reinf1070 in evento.Reinf1070)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf1070 = reinf1070;
                                reinfProc.ID = reinf1070.EvtTabProcesso.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {

                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }

                        #endregion

                        #region Reinf2010

                        if (evento.Reinf2010 != null)
                        {
                            foreach (var reinf2010 in evento.Reinf2010)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2010 = reinf2010;
                                reinfProc.ID = reinf2010.EvtServTom.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {

                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2020

                        if (evento.Reinf2020 != null)
                        {
                            foreach (var reinf2020 in evento.Reinf2020)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2020 = reinf2020;
                                reinfProc.ID = reinf2020.EvtServPrest.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2030

                        if (evento.Reinf2030 != null)
                        {
                            foreach (var reinf2030 in evento.Reinf2030)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2030 = reinf2030;
                                reinfProc.ID = reinf2030.EvtAssocDespRec.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }

                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2040

                        if (evento.Reinf2040 != null)
                        {
                            foreach (var reinf2040 in evento.Reinf2040)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2040 = reinf2040;
                                reinfProc.ID = reinf2040.EvtAssocDespRep.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2050

                        if (evento.Reinf2050 != null)
                        {
                            foreach (var reinf2050 in evento.Reinf2050)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2050 = reinf2050;
                                reinfProc.ID = reinf2050.EvtComProd.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2055

                        if (evento.Reinf2055 != null)
                        {
                            foreach (var reinf2055 in evento.Reinf2055)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2055 = reinf2055;
                                reinfProc.ID = reinf2055.EvtAqProd.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2060

                        if (evento.Reinf2060 != null)
                        {
                            foreach (var reinf2060 in evento.Reinf2060)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2060 = reinf2060;
                                reinfProc.ID = reinf2060.EvtCPRB.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }
                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2098

                        if (evento.Reinf2098 != null)
                        {
                            foreach (var reinf2098 in evento.Reinf2098)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2098 = reinf2098;
                                reinfProc.ID = reinf2098.EvtReabreEvPer.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf2099

                        if (evento.Reinf2099 != null)
                        {
                            foreach (var reinf2099 in evento.Reinf2099)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf2099 = reinf2099;
                                reinfProc.ID = reinf2099.EvtFechaEvPer.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9011.FirstOrDefault()?.EvtTotalContrib.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf3010

                        if (evento.Reinf3010 != null)
                        {
                            foreach (var reinf3010 in evento.Reinf3010)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf3010 = reinf3010;
                                reinfProc.ID = reinf3010.EvtEspDesportivo.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9001.FirstOrDefault()?.EvtTotal.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf4010

                        if (evento.Reinf4010 != null)
                        {
                            foreach (var reinf4010 in evento.Reinf4010)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf4010 = reinf4010;
                                reinfProc.ID = reinf4010.EvtRetPF.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9005.FirstOrDefault()?.EvtRet.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;

                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf4020

                        if (evento.Reinf4020 != null)
                        {
                            foreach (var reinf4020 in evento.Reinf4020)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf4020 = reinf4020;
                                reinfProc.ID = reinf4020.EvtRetPJ.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9005.FirstOrDefault()?.EvtRet.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf4040

                        if (evento.Reinf4040 != null)
                        {
                            foreach (var reinf4040 in evento.Reinf4040)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf4040 = reinf4040;
                                reinfProc.ID = reinf4040.EvtBenefNId.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {
                                    if (retornoEvento.Evento.RetornoEvento.Reinf9005.FirstOrDefault()?.EvtRet.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }

                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf4080

                        if (evento.Reinf4080 != null)
                        {
                            foreach (var reinf4080 in evento.Reinf4080)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf4080 = reinf4080;
                                reinfProc.ID = reinf4080.EvtRetRec.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {

                                    if (retornoEvento.Evento.RetornoEvento.Reinf9005.FirstOrDefault()?.EvtRet.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion

                        #region Reinf4099

                        if (evento.Reinf4099 != null)
                        {
                            foreach (var reinf4099 in evento.Reinf4099)
                            {
                                reinfProc = new ReinfProc();
                                reinfProc.Reinf4099 = reinf4099;
                                reinfProc.ID = reinf4099.EvtFech.ID;

                                foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos)
                                {

                                    if (retornoEvento.Evento.RetornoEvento.Reinf9015.FirstOrDefault()?.EvtRetCons.InfoRecEv.IdEv == reinfProc.ID)
                                    {
                                        reinfProc.RetornoEvento = retornoEvento.Evento.RetornoEvento;
                                    }
                                }

                                GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            }
                        }
                        #endregion
                    }
                }
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }


        #endregion Public Methods
    }
}