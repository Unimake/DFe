#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.MDFe;
using Unimake.Exceptions;

namespace Unimake.Business.DFe.Servicos.MDFe
{
    /// <summary>
    /// Enviar o XML de consulta recibo do lote de MDFe para o webservice
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Servicos.MDFe.RetAutorizacao")]
    [ComVisible(true)]
#endif
    public class RetAutorizacao : ServicoBase
    {
        #region Protected Methods

        /// <summary>
        /// Definir o valor de algumas das propriedades do objeto "Configuracoes"
        /// </summary>
        protected override void DefinirConfiguracao()
        {
            var xml = new ConsReciMDFe();
            xml = xml.LerXML<ConsReciMDFe>(ConteudoXML);

            if (!Configuracoes.Definida)
            {
                Configuracoes.Servico = Servico.MDFeConsultaRecibo;
                Configuracoes.TipoAmbiente = xml.TpAmb;
                Configuracoes.SchemaVersao = xml.Versao;
                Configuracoes.CodigoUF = Convert.ToInt32(xml.NRec.Substring(0, 2));
                base.DefinirConfiguracao();
            }
        }

        #endregion Protected Methods

        #region Public Properties

        /// <summary>
        /// Conteúdo retornado pelo webservice depois do envio do XML
        /// </summary>
        public RetConsReciMDFe Result
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(RetornoWSString))
                {
                    return XMLUtility.Deserializar<RetConsReciMDFe>(RetornoWSXML);
                }

                return new RetConsReciMDFe
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
        /// <param name="consReciMDFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações para conexão e envio do XML para o web-service</param>
        public RetAutorizacao(ConsReciMDFe consReciMDFe, Configuracao configuracao) : this()
        {
            if (configuracao is null)
            {
                throw new ArgumentNullException(nameof(configuracao));
            }

            Inicializar(consReciMDFe?.GerarXML() ?? throw new ArgumentNullException(nameof(consReciMDFe)), configuracao);
        }

        /// <summary>
        /// Inicia uma instância desta classe
        /// </summary>
        public RetAutorizacao() : base() { }

        #endregion Public Constructors

        #region Public Methods

#if INTEROP

        /// <summary>
        /// Executa o serviço: Assina o XML, valida e envia para o webservice
        /// </summary>
        /// <param name="consReciMDFe">Objeto contendo o XML a ser enviado</param>
        /// <param name="configuracao">Configurações a serem utilizadas na conexão e envio do XML para o webservice</param>
        [ComVisible(true)]
        public void Executar(ConsReciMDFe consReciMDFe, Configuracao configuracao)
        {
            try
            {
                if (configuracao is null)
                {
                    throw new ArgumentNullException(nameof(configuracao));
                }

                Inicializar(consReciMDFe?.GerarXML() ?? throw new ArgumentNullException(nameof(consReciMDFe)), configuracao);
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
        public override void GravarXmlDistribuicao(string pasta, string nomeArquivo, string conteudoXML) => ThrowHelper.Instance.Throw(new Exception("Não existe XML de distribuição para consulta do recibo de lote."));

        #endregion Public Methods
    }
}