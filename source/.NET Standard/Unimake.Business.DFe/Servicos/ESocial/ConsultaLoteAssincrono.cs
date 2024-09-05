#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Exceptions;
#endif
using System;
using System.IO;
using System.Text;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o xml para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.ConsultaLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class ConsultaLoteAssincrono : ServicoBase, IInteropService<ConsultarLoteEventos>
    {
        /// <summary>
        /// 
        /// </summary>
        public ConsultaLoteAssincrono(ConsultarLoteEventos consulta, Configuracao configuracao)
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consulta?.GerarXML() ?? throw new ArgumentNullException(nameof(consulta)), configuracao);
        }

#if INTEROP
        /// <summary>
        /// 
        /// </summary>
        /// <param name="ConsultarLoteEventos"></param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ConsultarLoteEventos ConsultarLoteEventos, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(ConsultarLoteEventos?.GerarXML() ?? throw new ArgumentNullException(nameof(ConsultarLoteEventos)), configuracao);
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

        #region Result

        /// <summary>
        /// Conteúdo retornado pelo web-service depois do envio do XML
        /// </summary>
        public RetornoConsultaLoteEvts Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetornoConsultaLoteEvts>(RetornoWSXML);
                }

                return new RetornoConsultaLoteEvts
                {
                    RetornoProcessamentoLoteEventos = new RetornoProcessamentoLoteEventos
                    {
                        StatusRetorno = new StatusRetorno
                        {
                            CdResposta = 0,
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornoado"
                        }
                    }

                };
            }
        }
        #endregion

        /// <summary>
        /// Desserialize nesta propriedade o XML do lote de eventos já assinado que foi enviado.
        /// </summary>
        public ESocialEnvioLoteEventos ESocialEnvioLoteEventos { get; set; }

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
                var esocialProc = new ESocialProc();

                if (Result.RetornoProcessamentoLoteEventos.StatusRetorno.CdResposta != 201)
                {
                    var mensagemRetorno = Result.GerarXML();
                    base.GravarXmlDistribuicao(pasta, "erro.xml", mensagemRetorno.OuterXml);
                    return;
                }

                foreach (var evento in ESocialEnvioLoteEventos.EnvioLoteEventos.Eventos.Evento)
                {
                    #region ESocial1000

                    if (evento.ESocial1000 != null)
                    {
                        esocialProc.ESocial1000 = evento.ESocial1000;
                        esocialProc.ID = evento.ESocial1000.EvtInfoEmpregador.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;

                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }                            
                        }
                    }

                    #endregion ESocial1000

                    #region ESocial1005

                    else if (evento.ESocial1005 != null)
                    {
                        esocialProc.ESocial1005 = evento.ESocial1005;
                        esocialProc.ID = evento.ESocial1005.EvtTabEstab.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;

                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1005

                    #region ESocial1010

                    else if (evento.ESocial1010 != null)
                    {
                        esocialProc.ESocial1010 = evento.ESocial1010;
                        esocialProc.ID = evento.ESocial1010.EvtTabRubrica.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;

                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1010

                    #region ESocial1020

                    else if (evento.ESocial1020 != null)
                    {
                        esocialProc.ESocial1020 = evento.ESocial1020;
                        esocialProc.ID = evento.ESocial1020.evtTabLotacao.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;

                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1020

                    #region ESocial1070

                    else if (evento.ESocial1070 != null)
                    {
                        esocialProc.ESocial1070 = evento.ESocial1070;
                        esocialProc.ID = evento.ESocial1070.EvtTabProcesso.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1070

                    #region ESocial1200

                    else if (evento.ESocial1200 != null)
                    {
                        esocialProc.ESocial1200 = evento.ESocial1200;
                        esocialProc.ID = evento.ESocial1200.EvtRemun.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;

                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1200

                    #region ESocial1202

                    else if (evento.ESocial1202 != null)
                    {
                        esocialProc.ESocial1202 = evento.ESocial1202;
                        esocialProc.ID = evento.ESocial1202.EvtRmnRPPS.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1202

                    #region ESocial1207

                    else if (evento.ESocial1207 != null)
                    {
                        esocialProc.ESocial1207 = evento.ESocial1207;
                        esocialProc.ID = evento.ESocial1207.EvtBenPrRP.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1207

                    #region ESocial1210

                    else if (evento.ESocial1210 != null)
                    {
                        esocialProc.ESocial1210 = evento.ESocial1210;
                        esocialProc.ID = evento.ESocial1210.EvtPgtos.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1210

                    #region ESocial1260

                    else if (evento.ESocial1260 != null)
                    {
                        esocialProc.ESocial1260 = evento.ESocial1260;
                        esocialProc.ID = evento.ESocial1260.EvtComProd.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1260

                    #region ESocial1270

                    else if (evento.ESocial1270 != null)
                    {
                        esocialProc.ESocial1270 = evento.ESocial1270;
                        esocialProc.ID = evento.ESocial1270.EvtContratAvNP.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1270

                    #region ESocial1280

                    else if (evento.ESocial1280 != null)
                    {
                        esocialProc.ESocial1280 = evento.ESocial1280;
                        esocialProc.ID = evento.ESocial1280.EvtInfoComplPer.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1280

                    #region ESocial1298

                    else if (evento.ESocial1298 != null)
                    {
                        esocialProc.ESocial1298 = evento.ESocial1298;
                        esocialProc.ID = evento.ESocial1298.EvtReabreEvPer.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1298

                    #region ESocial1299

                    else if (evento.ESocial1299 != null)
                    {
                        esocialProc.ESocial1299 = evento.ESocial1299;
                        esocialProc.ID = evento.ESocial1299.EvtFechaEvPer.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial1299

                    #region ESocial2190

                    else if (evento.ESocial2190 != null)
                    {
                        esocialProc.ESocial2190 = evento.ESocial2190;
                        esocialProc.ID = evento.ESocial2190.EvtAdmPrelim.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2190

                    #region ESocial2200

                    else if (evento.ESocial2200 != null)
                    {
                        esocialProc.ESocial2200 = evento.ESocial2200;
                        esocialProc.ID = evento.ESocial2200.EvtAdmissao.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2200

                    #region ESocial2205

                    else if (evento.ESocial2205 != null)
                    {
                        esocialProc.ESocial2205 = evento.ESocial2205;
                        esocialProc.ID = evento.ESocial2205.EvtAltCadastral.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2205

                    #region ESocial2206

                    else if (evento.ESocial2206 != null)
                    {
                        esocialProc.ESocial2206 = evento.ESocial2206;
                        esocialProc.ID = evento.ESocial2206.EvtAltContratual.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2206

                    #region ESocial2210

                    else if (evento.ESocial2210 != null)
                    {
                        esocialProc.ESocial2210 = evento.ESocial2210;
                        esocialProc.ID = evento.ESocial2210.EvtCAT.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2210

                    #region ESocial2220

                    else if (evento.ESocial2220 != null)
                    {
                        esocialProc.ESocial2220 = evento.ESocial2220;
                        esocialProc.ID = evento.ESocial2220.EvtMonit.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2220

                    #region ESocial2221

                    else if (evento.ESocial2221 != null)
                    {
                        esocialProc.ESocial2221 = evento.ESocial2221;
                        esocialProc.ID = evento.ESocial2221.EvtToxic.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2221

                    #region ESocial2230

                    else if (evento.ESocial2230 != null)
                    {
                        esocialProc.ESocial2230 = evento.ESocial2230;
                        esocialProc.ID = evento.ESocial2230.EvtAfastTemp.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2230

                    #region ESocial2231

                    else if (evento.ESocial2231 != null)
                    {
                        esocialProc.ESocial2231 = evento.ESocial2231;
                        esocialProc.ID = evento.ESocial2231.EvtCessao.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2231

                    #region ESocial2240

                    else if (evento.ESocial2240 != null)
                    {
                        esocialProc.ESocial2240 = evento.ESocial2240;
                        esocialProc.ID = evento.ESocial2240.EvtExpRisco.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2240

                    #region ESocial2298

                    else if (evento.ESocial2298 != null)
                    {
                        esocialProc.ESocial2298 = evento.ESocial2298;
                        esocialProc.ID = evento.ESocial2298.EvtReintegr.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2298

                    #region ESocial2299

                    else if (evento.ESocial2299 != null)
                    {
                        esocialProc.ESocial2299 = evento.ESocial2299;
                        esocialProc.ID = evento.ESocial2299.EvtDeslig.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2299

                    #region ESocial2306

                    else if (evento.ESocial2306 != null)
                    {
                        esocialProc.ESocial2306 = evento.ESocial2306;
                        esocialProc.ID = evento.ESocial2306.EvtTSVAltContr.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2306

                    #region ESocial2399

                    else if (evento.ESocial2399 != null)
                    {
                        esocialProc.ESocial2399 = evento.ESocial2399;
                        esocialProc.ID = evento.ESocial2399.EvtTSVTermino.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2399

                    #region ESocial2400

                    else if (evento.ESocial2400 != null)
                    {
                        esocialProc.ESocial2400 = evento.ESocial2400;
                        esocialProc.ID = evento.ESocial2400.EvtCdBenefIn.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2400

                    #region ESocial2405

                    else if (evento.ESocial2405 != null)
                    {
                        esocialProc.ESocial2405 = evento.ESocial2405;
                        esocialProc.ID = evento.ESocial2405.EvtCdBenefAlt.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2405

                    #region ESocial2410

                    else if (evento.ESocial2410 != null)
                    {
                        esocialProc.ESocial2410 = evento.ESocial2410;
                        esocialProc.ID = evento.ESocial2410.EvtCdBenIn.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2410

                    #region ESocial2416

                    else if (evento.ESocial2416 != null)
                    {
                        esocialProc.ESocial2416 = evento.ESocial2416;
                        esocialProc.ID = evento.ESocial2416.EvtCdBenAlt.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2416

                    #region ESocial2418

                    else if (evento.ESocial2418 != null)
                    {
                        esocialProc.ESocial2418 = evento.ESocial2418;
                        esocialProc.ID = evento.ESocial2418.EvtReativBen.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2418

                    #region ESocial2420

                    else if (evento.ESocial2420 != null)
                    {
                        esocialProc.ESocial2420 = evento.ESocial2420;
                        esocialProc.ID = evento.ESocial2420.EvtCdBenTerm.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2420

                    #region ESocial2500

                    else if (evento.ESocial2500 != null)
                    {
                        esocialProc.ESocial2500 = evento.ESocial2500;
                        esocialProc.ID = evento.ESocial2500.EvtProcTrab.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2500

                    #region ESocial2501

                    else if (evento.ESocial2501 != null)
                    {
                        esocialProc.ESocial2501 = evento.ESocial2501;
                        esocialProc.ID = evento.ESocial2501.EvtContProc.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial2501

                    #region ESocial3000

                    else if (evento.ESocial3000 != null)
                    {
                        esocialProc.ESocial3000 = evento.ESocial3000;
                        esocialProc.ID = evento.ESocial3000.EvtExclusao.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial3000

                    #region ESocial3500

                    else if (evento.ESocial3500 != null)
                    {
                        esocialProc.ESocial3500 = evento.ESocial3500;
                        esocialProc.ID = evento.ESocial3500.EvtExcProcTrab.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial3500

                    // Os eventos 5001 e 5002 são totalizadores, logo não são enviados e sim recebidos no retorno da consulta lote assíncrono

                    #region ESocial5003

                    else if (evento.ESocial5003 != null)
                    {
                        esocialProc.ESocial5003 = evento.ESocial5003;
                        esocialProc.ID = evento.ESocial5003.EvtBasesFGTS.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial5003

                    // Os eventos 5011 e 5012 são totalizadores, logo não são enviados e sim recebidos no retorno da consulta lote assíncrono

                    #region ESocial5013

                    else if (evento.ESocial5013 != null)
                    {
                        esocialProc.ESocial5013 = evento.ESocial5013;
                        esocialProc.ID = evento.ESocial5013.EvtFGTS.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial5013

                    #region ESocial5501

                    else if (evento.ESocial5501 != null)
                    {
                        esocialProc.ESocial5501 = evento.ESocial5501;
                        esocialProc.ID = evento.ESocial5501.EvtTribProcTrab.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial5501

                    #region ESocial5503

                    else if (evento.ESocial5503 != null)
                    {
                        esocialProc.ESocial5503 = evento.ESocial5503;
                        esocialProc.ID = evento.ESocial5503.EvtFGTSProcTrab.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial5503

                    #region ESocial8200

                    else if (evento.ESocial8200 != null)
                    {
                        esocialProc.ESocial8200 = evento.ESocial8200;
                        esocialProc.ID = evento.ESocial8200.EvtAnotJud.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial8200

                    #region ESocial8299

                    else if (evento.ESocial8299 != null)
                    {
                        esocialProc.ESocial8299 = evento.ESocial8299;
                        esocialProc.ID = evento.ESocial8299.EvtBaixa.ID;

                        foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
                        {
                            if (retornoEvento.ID == esocialProc.ID)
                            {
                                esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                                base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                            }
                        }
                    }

                    #endregion ESocial8299
                }
            } 
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <exception cref="NotImplementedException"></exception>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsultarLoteEventos();
            xml = xml.LerXML<ConsultarLoteEventos>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialConsultaEvts;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

    }
}
