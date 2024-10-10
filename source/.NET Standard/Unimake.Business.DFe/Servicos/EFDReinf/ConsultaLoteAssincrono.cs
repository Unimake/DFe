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

        /// <summary>
        /// Gravar o XML de distribuição
        /// </summary>
        /// <param name="reinfProc">Objeto da estrutura do XML de distribuição contendo o evento e ID</param>
        /// <param name="pasta">Pasta onde será gravado os XMLs de distribuição dos eventos autorizados</param>
        /// <param name="idEvento">Id do evento a ser gravado. Em branco ou nulo vai gravar o XML de distribuição de todos os eventos do lote enviado</param>
        private void GravarXML(ReinfProc reinfProc, string pasta, string idEvento)
        {
            if (!string.IsNullOrWhiteSpace(idEvento))
            {
                if (reinfProc.ID != idEvento)
                {
                    return;
                }
            }

            foreach (var retornoEvento in Result.RetornoLoteEventosAssincrono.RetornoEventos.Evento)
            {
                if (retornoEvento.ID == reinfProc.ID)
                {
                    if (retornoEvento.RetornoEvento.Reinf9001 != null)
                    {
                        if (retornoEvento.RetornoEvento.Reinf9001.EvtTotal.IdeRecRetorno.IdeStatus.CdRetorno == 0)
                        {
                            reinfProc.RetornoEvento = retornoEvento.RetornoEvento;
                            base.GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            break;
                        }
                    }

                    else if (retornoEvento.RetornoEvento.Reinf9005 != null)
                    {
                        if (retornoEvento.RetornoEvento.Reinf9005.EvtRet.IdeRecRetorno.IdeStatus.CdRetorno == 0)
                        {
                            reinfProc.RetornoEvento = retornoEvento.RetornoEvento;
                            base.GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            break;
                        }
                    }
                    
                    else if (retornoEvento.RetornoEvento.Reinf9011 != null)
                    {
                        if (retornoEvento.RetornoEvento.Reinf9011.EvtTotalContrib.IdeRecRetorno.IdeStatus.CdRetorno == 0)
                        {
                            reinfProc.RetornoEvento = retornoEvento.RetornoEvento;
                            base.GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            break;
                        }
                    }

                    else if (retornoEvento.RetornoEvento.Reinf9015 != null)
                    {
                        if (retornoEvento.RetornoEvento.Reinf9015.EvtRetCons.IdeRecRetorno.IdeStatus.CdRetorno == 0)
                        {
                            reinfProc.RetornoEvento = retornoEvento.RetornoEvento;
                            base.GravarXmlDistribuicao(pasta, reinfProc.NomeArquivoDistribuicao, reinfProc.GerarXML().OuterXml);
                            break;
                        }
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
                foreach (var evento in ReinfEnvioLoteEventos.EnvioLoteEventos.Eventos.Evento)
                {
                    #region Reinf1000

                    if (evento.Reinf1000 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf1000 = evento.Reinf1000,
                            ID = evento.Reinf1000.EvtInfoContri.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf1000

                    #region Reinf1050

                    else if (evento.Reinf1050 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf1050 = evento.Reinf1050,
                            ID = evento.Reinf1050.EvtTabLig.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf1050

                    #region Reinf1070

                    else if (evento.Reinf1070 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf1070 = evento.Reinf1070,
                            ID = evento.Reinf1070.EvtTabProcesso.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf1070

                    #region Reinf2010

                    else if (evento.Reinf2010 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2010 = evento.Reinf2010,
                            ID = evento.Reinf2010.EvtServTom.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2010

                    #region Reinf2020

                    else if (evento.Reinf2020 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2020 = evento.Reinf2020,
                            ID = evento.Reinf2020.EvtServPrest.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2020

                    #region Reinf2030

                    else if (evento.Reinf2030 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2030 = evento.Reinf2030,
                            ID = evento.Reinf2030.EvtAssocDespRec.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2030

                    #region Reinf2040

                    else if (evento.Reinf2040 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2040 = evento.Reinf2040,
                            ID = evento.Reinf2040.EvtAssocDespRep.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2040

                    #region Reinf2050

                    else if (evento.Reinf2050 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2050 = evento.Reinf2050,
                            ID = evento.Reinf2050.EvtComProd.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2050

                    #region Reinf2055

                    else if (evento.Reinf2055 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2055 = evento.Reinf2055,
                            ID = evento.Reinf2055.EvtAqProd.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2055

                    #region Reinf2060

                    else if (evento.Reinf2060 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2060 = evento.Reinf2060,
                            ID = evento.Reinf2060.EvtCPRB.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2060

                    #region Reinf2098

                    else if (evento.Reinf2098 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2098 = evento.Reinf2098,
                            ID = evento.Reinf2098.EvtReabreEvPer.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2098

                    #region Reinf2099

                    else if (evento.Reinf2099 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf2099 = evento.Reinf2099,
                            ID = evento.Reinf2099.EvtFechaEvPer.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf2099

                    #region Reinf3010

                    else if (evento.Reinf3010 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf3010 = evento.Reinf3010,
                            ID = evento.Reinf3010.EvtEspDesportivo.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf3010

                    #region Reinf4010

                    else if (evento.Reinf4010 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf4010 = evento.Reinf4010,
                            ID = evento.Reinf4010.EvtRetPF.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf4010

                    #region Reinf4020

                    else if (evento.Reinf4020 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf4020 = evento.Reinf4020,
                            ID = evento.Reinf4020.EvtRetPJ.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf4020

                    #region Reinf4040

                    else if (evento.Reinf4040 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf4040 = evento.Reinf4040,
                            ID = evento.Reinf4040.EvtBenefNId.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf4040

                    #region Reinf4080

                    else if (evento.Reinf4080 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf4080 = evento.Reinf4080,
                            ID = evento.Reinf4080.EvtRetRec.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf4080

                    #region Reinf4099

                    else if (evento.Reinf4099 != null)
                    {
                        var reinfProc = new ReinfProc
                        {
                            Reinf4099 = evento.Reinf4099,
                            ID = evento.Reinf4099.EvtFech.ID
                        };

                        GravarXML(reinfProc, pasta, idEvento);
                    }

                    #endregion Reinf4099
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

        #endregion Public Methods
    }
}