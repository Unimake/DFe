#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.GNRE;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.GNRE
{
    /// <summary>
    /// Envio do XML de consulta configurações da GNRE por UF para o WebService 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.GNRE.ConsultaConfigUF")]
    [ComVisible(true)]
#endif
    public class ConsultaConfigUF : ServicoBase, IInteropService<TConsultaConfigUf>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new TConsultaConfigUf();
            xml = xml.LerXML<TConsultaConfigUf>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.GNREConsultaConfigUF;
                Configuracoes.CodigoUF = (int)xml.UF;
                Configuracoes.TipoAmbiente = xml.Ambiente;
                Configuracoes.SchemaVersao = "2.00";

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public TConfigUf Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<TConfigUf>(RetornoWSXML);
                }

                return new TConfigUf
                {
                    SituacaoConsulta = new SituacaoConsulta
                    {
                        Codigo = "0",
                        Descricao = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado do WebService."
                    }
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsultaConfigUF() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="tConsultaConfigUf">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsultaConfigUF(TConsultaConfigUf tConsultaConfigUf, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(tConsultaConfigUf?.GerarXML() ?? throw new ArgumentNullException(nameof(tConsultaConfigUf)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="tConsultaConfigUf">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [ComVisible(true)]
        public void Executar(TConsultaConfigUf tConsultaConfigUf, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(tConsultaConfigUf?.GerarXML() ?? throw new ArgumentNullException(nameof(tConsultaConfigUf)), configuracao);
                Executar();
            }
            catch (ValidarXMLException ex)
            {
                ThrowHelper.Instance.Throw(ex);
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
        /// Grava o XML de Distribuição em uma pasta definida - (Para este serviço não tem XML de distribuição).
        /// </summary>
        /// <param name="pasta">Pasta onde é para ser gravado do XML</param>
        /// <param name="nomeArquivo">Nome para o arquivo XML</param>
        /// <param name="conteudoXML">Conteúdo do XML</param>
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) => ThrowHelper.Instance.Throw(new Exception("Não existe XML de distribuição para consulta status do serviço."));

        #endregion Public Methods
    }
}