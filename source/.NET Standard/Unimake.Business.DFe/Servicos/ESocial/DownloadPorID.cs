#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.ESocial;
using Unimake.Exceptions;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Servicos.ESocial
{
    /// <summary>
    /// Enviar o xml para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.ESocial.DownloadPorID")]
    [ComVisible(true)]
#endif
    public class DownloadPorID : ServicoBase, IInteropService<DownloadEventosPorID>
    {
        /// <summary>
        /// Construtor
        /// </summary>
        public DownloadPorID() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        public DownloadPorID(DownloadEventosPorID downloadEventosPorID, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(downloadEventosPorID?.GerarXML() ?? throw new ArgumentNullException(nameof(downloadEventosPorID)), configuracao);
        }

#if INTEROP
        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="downloadEventosPorID">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao"></param>
        /// <exception cref="NotImplementedException"></exception>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] DownloadEventosPorID downloadEventosPorID, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(downloadEventosPorID?.GerarXML() ?? throw new ArgumentNullException(nameof(downloadEventosPorID)), configuracao);
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
                            DescResposta = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornoado",
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
            var xml = new DownloadEventosPorID();
            xml = xml.LerXML<DownloadEventosPorID>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.ESocialDownloadEvts;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

    }
}
