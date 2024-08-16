#if INTEROP
using System.Runtime.InteropServices;
using Unimake.Exceptions;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.ESocial;

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
        /// 
        /// </summary>
        /// <param name="pasta"></param>
        /// <param name="nomeArquivo"></param>
        /// <param name="conteudoXML"></param>
        /// <exception cref="NotImplementedException"></exception>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            //throw new NotImplementedException();
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
