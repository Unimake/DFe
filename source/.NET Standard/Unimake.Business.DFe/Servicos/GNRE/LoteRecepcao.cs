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
    /// Envio do XML de lote da GNRE para o WebService
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.GNRE.LoteRecepcao")]
    [ComVisible(true)]
#endif
    public class LoteRecepcao : ServicoBase, IInteropService<TLoteGNRE>
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            //var xml = new TLoteGNRE();
            //xml = xml.LerXML<TLoteGNRE>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.GNRELoteRecepcao;
                Configuracoes.SchemaVersao = "2.00";

                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public TRetLoteGNRE Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<TRetLoteGNRE>(RetornoWSXML);
                }

                return new TRetLoteGNRE
                {
                    SituacaoRecepcao = new SituacaoRecepcao
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
        public LoteRecepcao() : base() { }

        /// <summary>
        /// Construtor
        /// </summary>
        /// <param name="tLoteGNRE">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public LoteRecepcao(TLoteGNRE tLoteGNRE, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(tLoteGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(tLoteGNRE)), configuracao);
        }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o web-service
        /// </summary>
        /// <param name="tLoteGNRE">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o web-service</param>
        [ComVisible(true)]
        public void Executar(TLoteGNRE tLoteGNRE, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(tLoteGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(tLoteGNRE)), configuracao);
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

        /// <summary>
        /// Definir o objeto contendo o XML a ser enviado e configuração de conexão e envio do XML para web-service
        /// </summary>
        /// <param name="tLoteGNRE">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public void SetXMLConfiguracao(TLoteGNRE tLoteGNRE, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(tLoteGNRE?.GerarXML() ?? throw new ArgumentNullException(nameof(tLoteGNRE)), configuracao);
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