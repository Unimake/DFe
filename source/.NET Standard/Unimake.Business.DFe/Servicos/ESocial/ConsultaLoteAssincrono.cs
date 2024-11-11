#if INTEROP
using System.Runtime.InteropServices;
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
        /// Construtor
        /// </summary>
        public ConsultaLoteAssincrono() : base() { }

        /// <summary>
        /// Construtor
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
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="ConsultarLoteEventos">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
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
        public Xml.ESocial.Retorno.RetornoEventoProcessado Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<Xml.ESocial.Retorno.RetornoEventoProcessado>(RetornoWSXML);
                }

                return new Xml.ESocial.Retorno.RetornoEventoProcessado
                {
                    RetornoProcessamentoLoteEventos = new Xml.ESocial.Retorno.RetornoProcessamentoLoteEventos
                    {
                        Status = new Xml.ESocial.Retorno.Status
                        {
                            CdResposta = 0,
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado"
                        }
                    }
                };
            }
        }
        #endregion

        /// <summary>
        /// Desserializar nesta propriedade o XML do lote de eventos já assinado que foi enviado.
        /// </summary>
        public ESocialEnvioLoteEventos ESocialEnvioLoteEventos { get; set; }

        /// <summary>
        /// Gravar o XML de distribuição
        /// </summary>
        /// <param name="esocialProc">Objeto da estrutura do XML de distribuição contendo o evento e ID</param>
        /// <param name="pasta">Pasta onde será gravado os XMLs de distribuição dos eventos autorizados</param>
        /// <param name="idEvento">Id do evento a ser gravado. Em branco ou nulo vai gravar o XML de distribuição de todos os eventos do lote enviado</param>
        private void GravarXML(ESocialProc esocialProc, string pasta, string idEvento)
        {
            if (!string.IsNullOrWhiteSpace(idEvento))
            {
                if (esocialProc.ID != idEvento)
                {
                    return;
                }
            }

            foreach (var retornoEvento in Result.RetornoProcessamentoLoteEventos.RetornoEventos.Evento)
            {
                if (retornoEvento.ID == esocialProc.ID)
                {
                    if (retornoEvento.RetornoEvento.ESocial.RetornoEvento.Processamento.CdResposta == 201)
                    {
                        esocialProc.RetornoEvento = retornoEvento.RetornoEvento;
                        base.GravarXmlDistribuicao(pasta, esocialProc.NomeArquivoDistribuicao, esocialProc.GerarXML().OuterXml);
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// Gravar XML de distribuição dos eventos enviados/autorizados em uma pasta específica
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado os XMLs de distribuição dos eventos autorizados</param>
        /// <param name="idEvento">Id do evento a ser gravado. Em branco ou nulo vai gravar o XML de distribuição de todos os eventos do lote enviado</param>
        public void GravarXmlDistribuicao(string pasta, string idEvento)
        {
            try
            {
                foreach (var evento in ESocialEnvioLoteEventos.EnvioLoteEventos.Eventos.Evento)
                {
                    #region ESocial1000

                    if (evento.ESocial1000 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1000 = evento.ESocial1000,
                            ID = evento.ESocial1000.EvtInfoEmpregador.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1000

                    #region ESocial1005

                    else if (evento.ESocial1005 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1005 = evento.ESocial1005,
                            ID = evento.ESocial1005.EvtTabEstab.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1005

                    #region ESocial1010

                    else if (evento.ESocial1010 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1010 = evento.ESocial1010,
                            ID = evento.ESocial1010.EvtTabRubrica.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1010

                    #region ESocial1020

                    else if (evento.ESocial1020 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1020 = evento.ESocial1020,
                            ID = evento.ESocial1020.EvtTabLotacao.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1020

                    #region ESocial1070

                    else if (evento.ESocial1070 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1070 = evento.ESocial1070,
                            ID = evento.ESocial1070.EvtTabProcesso.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1070

                    #region ESocial1200

                    else if (evento.ESocial1200 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1200 = evento.ESocial1200,
                            ID = evento.ESocial1200.EvtRemun.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1200

                    #region ESocial1202

                    else if (evento.ESocial1202 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1202 = evento.ESocial1202,
                            ID = evento.ESocial1202.EvtRmnRPPS.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1202

                    #region ESocial1207

                    else if (evento.ESocial1207 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1207 = evento.ESocial1207,
                            ID = evento.ESocial1207.EvtBenPrRP.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1207

                    #region ESocial1210

                    else if (evento.ESocial1210 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1210 = evento.ESocial1210,
                            ID = evento.ESocial1210.EvtPgtos.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1210

                    #region ESocial1260

                    else if (evento.ESocial1260 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1260 = evento.ESocial1260,
                            ID = evento.ESocial1260.EvtComProd.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1260

                    #region ESocial1270

                    else if (evento.ESocial1270 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1270 = evento.ESocial1270,
                            ID = evento.ESocial1270.EvtContratAvNP.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1270

                    #region ESocial1280

                    else if (evento.ESocial1280 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1280 = evento.ESocial1280,
                            ID = evento.ESocial1280.EvtInfoComplPer.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1280

                    #region ESocial1298

                    else if (evento.ESocial1298 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1298 = evento.ESocial1298,
                            ID = evento.ESocial1298.EvtReabreEvPer.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1298

                    #region ESocial1299

                    else if (evento.ESocial1299 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial1299 = evento.ESocial1299,
                            ID = evento.ESocial1299.EvtFechaEvPer.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial1299

                    #region ESocial2190

                    else if (evento.ESocial2190 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2190 = evento.ESocial2190,
                            ID = evento.ESocial2190.EvtAdmPrelim.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2190

                    #region ESocial2200

                    else if (evento.ESocial2200 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2200 = evento.ESocial2200,
                            ID = evento.ESocial2200.EvtAdmissao.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2200

                    #region ESocial2205

                    else if (evento.ESocial2205 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2205 = evento.ESocial2205,
                            ID = evento.ESocial2205.EvtAltCadastral.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2205

                    #region ESocial2206

                    else if (evento.ESocial2206 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2206 = evento.ESocial2206,
                            ID = evento.ESocial2206.EvtAltContratual.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2206

                    #region ESocial2210

                    else if (evento.ESocial2210 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2210 = evento.ESocial2210,
                            ID = evento.ESocial2210.EvtCAT.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2210

                    #region ESocial2220

                    else if (evento.ESocial2220 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2220 = evento.ESocial2220,
                            ID = evento.ESocial2220.EvtMonit.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2220

                    #region ESocial2221

                    else if (evento.ESocial2221 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2221 = evento.ESocial2221,
                            ID = evento.ESocial2221.EvtToxic.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2221

                    #region ESocial2230

                    else if (evento.ESocial2230 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2230 = evento.ESocial2230,
                            ID = evento.ESocial2230.EvtAfastTemp.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2230

                    #region ESocial2231

                    else if (evento.ESocial2231 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2231 = evento.ESocial2231,
                            ID = evento.ESocial2231.EvtCessao.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2231

                    #region ESocial2240

                    else if (evento.ESocial2240 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2240 = evento.ESocial2240,
                            ID = evento.ESocial2240.EvtExpRisco.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2240

                    #region ESocial2298

                    else if (evento.ESocial2298 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2298 = evento.ESocial2298,
                            ID = evento.ESocial2298.EvtReintegr.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2298

                    #region ESocial2299

                    else if (evento.ESocial2299 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2299 = evento.ESocial2299,
                            ID = evento.ESocial2299.EvtDeslig.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2299

                    #region ESocial2306

                    else if (evento.ESocial2306 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2306 = evento.ESocial2306,
                            ID = evento.ESocial2306.EvtTSVAltContr.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2306

                    #region ESocial2399

                    else if (evento.ESocial2399 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2399 = evento.ESocial2399,
                            ID = evento.ESocial2399.EvtTSVTermino.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2399

                    #region ESocial2400

                    else if (evento.ESocial2400 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2400 = evento.ESocial2400,
                            ID = evento.ESocial2400.EvtCdBenefIn.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2400

                    #region ESocial2405

                    else if (evento.ESocial2405 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2405 = evento.ESocial2405,
                            ID = evento.ESocial2405.EvtCdBenefAlt.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2405

                    #region ESocial2410

                    else if (evento.ESocial2410 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2410 = evento.ESocial2410,
                            ID = evento.ESocial2410.EvtCdBenIn.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2410

                    #region ESocial2416

                    else if (evento.ESocial2416 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2416 = evento.ESocial2416,
                            ID = evento.ESocial2416.EvtCdBenAlt.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2416

                    #region ESocial2418

                    else if (evento.ESocial2418 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2418 = evento.ESocial2418,
                            ID = evento.ESocial2418.EvtReativBen.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2418

                    #region ESocial2420

                    else if (evento.ESocial2420 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2420 = evento.ESocial2420,
                            ID = evento.ESocial2420.EvtCdBenTerm.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2420

                    #region ESocial2500

                    else if (evento.ESocial2500 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2500 = evento.ESocial2500,
                            ID = evento.ESocial2500.EvtProcTrab.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2500

                    #region ESocial2501

                    else if (evento.ESocial2501 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial2501 = evento.ESocial2501,
                            ID = evento.ESocial2501.EvtContProc.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial2501

                    #region ESocial3000

                    else if (evento.ESocial3000 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial3000 = evento.ESocial3000,
                            ID = evento.ESocial3000.EvtExclusao.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial3000

                    #region ESocial3500

                    else if (evento.ESocial3500 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial3500 = evento.ESocial3500,
                            ID = evento.ESocial3500.EvtExcProcTrab.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial3500

                    // Os eventos 5001 e 5002 são totalizadores, logo não são enviados e sim recebidos no retorno da consulta lote assíncrono

                    #region ESocial5003

                    else if (evento.ESocial5003 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial5003 = evento.ESocial5003,
                            ID = evento.ESocial5003.EvtBasesFGTS.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial5003

                    // Os eventos 5011 e 5012 são totalizadores, logo não são enviados e sim recebidos no retorno da consulta lote assíncrono

                    #region ESocial5013

                    else if (evento.ESocial5013 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial5013 = evento.ESocial5013,
                            ID = evento.ESocial5013.EvtFGTS.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial5013

                    #region ESocial5501

                    else if (evento.ESocial5501 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial5501 = evento.ESocial5501,
                            ID = evento.ESocial5501.EvtTribProcTrab.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial5501

                    #region ESocial5503

                    else if (evento.ESocial5503 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial5503 = evento.ESocial5503,
                            ID = evento.ESocial5503.EvtFGTSProcTrab.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial5503

                    #region ESocial8200

                    else if (evento.ESocial8200 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial8200 = evento.ESocial8200,
                            ID = evento.ESocial8200.EvtAnotJud.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
                    }

                    #endregion ESocial8200

                    #region ESocial8299

                    else if (evento.ESocial8299 != null)
                    {
                        var esocialProc = new ESocialProc
                        {
                            ESocial8299 = evento.ESocial8299,
                            ID = evento.ESocial8299.EvtBaixa.ID
                        };

                        GravarXML(esocialProc, pasta, idEvento);
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
        /// Gravar XML de distribuição dos eventos enviados/autorizados em uma pasta específica
        /// </summary>
        /// <param name="pasta">Pasta onde será gravado os XMLs de distribuição dos eventos autorizados</param>
#if INTEROP
        [ComVisible(false)]
#endif
        public void GravarXmlDistribuicao(string pasta)
        {
            try
            {
                GravarXmlDistribuicao(pasta, null);
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) => throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta, string idEvento)");

        /// <inheritdoc />
#if INTEROP
        [ComVisible(false)]
#endif
        public override void GravarXmlDistribuicao(Stream stream, string value, Encoding encoding = null) => throw new Exception("Método não implementado! Utilize o GravarXmlDistribuicao(string pasta, string idEvento)");

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
