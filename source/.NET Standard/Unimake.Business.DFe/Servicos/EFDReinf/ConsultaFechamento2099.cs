#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Xml.EFDReinf;
using Unimake.Business.DFe.Utility;
using System.Xml;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.EFDReinf
{
    /// <summary>
    /// Enviar o XML de consulta o fechamento do R-2099 do EFD Reinf
    /// </summary>
    public class ConsultaFechamento2099 : ServicoBase, IInteropService<ReinfConsultaFechamento2099>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ReinfConsultaFechamento2099();
            xml = xml.LerXML<ReinfConsultaFechamento2099>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.EFDReinfConsultaFechamento2099;
                Configuracoes.CodigoUF = (int)UFBrasil.AN;
                Configuracoes.SchemaVersao = "1.05.01";
            }

            base.DefinirConfiguracao();
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// 
        /// </summary>
        public ReinfRetornoConsultaFechamento2099 Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<ReinfRetornoConsultaFechamento2099>(RetornoWSXML);
                }

                return new ReinfRetornoConsultaFechamento2099
                {

                    EvtTotalContrib = new EvtTotalContrib
                    {
                        IdeRecRetorno = new IdeRecRetorno9011
                        {
                            IdeStatus = new IdeStatus
                            {
                                CdRetorno = CodigoDoRetorno.Erro,
                                DescRetorno = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ"
                            }
                        }
                    }

                };
            }
        }

        #endregion Public Properties

        #region Public Contructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaFechamento2099() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="reinfConsultaFechamento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public ConsultaFechamento2099(ReinfConsultaFechamento2099 reinfConsultaFechamento, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(reinfConsultaFechamento.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaFechamento)), configuracao);
        }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="conteudoXML">String do XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o webservice</param>
        public ConsultaFechamento2099(string conteudoXML, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            var doc = new XmlDocument();
            doc.Load(conteudoXML);

            Inicializar(doc, configuracao);
        }

        #endregion Public Contructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="reinfConsultaFechamento">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar([MarshalAs(UnmanagedType.IUnknown)] ReinfConsultaFechamento2099 reinfConsultaFechamento, [MarshalAs(UnmanagedType.IUnknown)] Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(reinfConsultaFechamento?.GerarXML() ?? throw new ArgumentNullException(nameof(reinfConsultaFechamento)), configuracao);
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

        /// <inheritdoc />
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML)
        {
            try
            {
                throw new Exception("Não existe XML de distribuição para consulta status do serviço.");
            }
            catch (Exception ex)
            {
                ThrowHelper.Instance.Throw(ex);
            }
        }


        #endregion Public Methods
    }
}
