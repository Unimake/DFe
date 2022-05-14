#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Servicos.Interop;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Envio do XML de consulta MDFe´s não encerrados o WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.ConsNaoEnc")]
    [ComVisible(true)]
#endif
    public class ConsNaoEnc : ServicoBase, IInteropService<ConsMDFeNaoEnc>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsMDFeNaoEnc();
            xml = xml.LerXML<ConsMDFeNaoEnc>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.MDFeConsultaNaoEnc;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetConsMDFeNaoEnc Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsMDFeNaoEnc>(RetornoWSXML);
                }

                return new RetConsMDFeNaoEnc
                {
                    CStat = 0,
                    XMotivo = "Ocorreu uma falha ao tentar criar o objeto a partir do XML retornado da SEFAZ."
                };
            }
        }

        #endregion Public Properties

        #region Public Constructors

        /// <summary>
        /// Construtor
        /// </summary>
        public ConsNaoEnc() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="consMDFeNaoEnc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public ConsNaoEnc(ConsMDFeNaoEnc consMDFeNaoEnc, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consMDFeNaoEnc?.GerarXML() ?? throw new ArgumentNullException(nameof(consMDFeNaoEnc)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="consMDFeNaoEnc">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        public void Executar(ConsMDFeNaoEnc consMDFeNaoEnc, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consMDFeNaoEnc?.GerarXML() ?? throw new ArgumentNullException(nameof(consMDFeNaoEnc)), configuracao);
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
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) => ThrowHelper.Instance.Throw(new System.Exception("Não existe XML de distribuição para consulta status do serviço."));

        #endregion Public Methods
    }
}