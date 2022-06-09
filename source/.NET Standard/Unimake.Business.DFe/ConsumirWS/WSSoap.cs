using System.Net;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para definição de parâmetros do SOAP dos Webservices
    /// </summary>
    public class WSSoap
    {
        #region Private Fields

        private string _ActionWeb;
        private string _ContentType;
        private string _EnderecoWeb;
        private string _SoapString;
        private string _TagRetorno;
        private string _EncodingRetorno;
        private string _VersaoSoap;
        private bool _GZIPCompress;
        private IWebProxy _Proxy;
        private int _TimeOutWebServiceConnect;

        #endregion Private Fields

        #region Public Properties

        /// <summary>
        /// Web Action - Endereço com a ação/método que será executado no webservice
        /// </summary>
        public string ActionWeb
        {
            get => _ActionWeb;
            set => _ActionWeb = value;
        }

        /// <summary>
        /// Content Type - Tipo do conteúdo do SOAP
        /// </summary>
        public string ContentType
        {
            get => string.IsNullOrWhiteSpace(_ContentType) ? (_ContentType = "application/soap+xml; charset=utf-8;") : _ContentType;
            set => _ContentType = value;
        }

        /// <summary>
        /// Endereço do Webservice
        /// </summary>
        public string EnderecoWeb
        {
            get => _EnderecoWeb;
            set => _EnderecoWeb = value;
        }

        /// <summary>
        /// String do SOAP
        /// </summary>
        public string SoapString
        {
            get => _SoapString;
            set => _SoapString = value;
        }

        /// <summary>
        /// Nome da tag de retorno de conteúdo que é devolvido pelo WebService
        /// </summary>
        public string TagRetorno
        {
            get => string.IsNullOrWhiteSpace(_TagRetorno) ? (_TagRetorno = "nfeResultMsg") : _TagRetorno;
            set => _TagRetorno = value;
        }

        /// <summary>
        /// Versão do SOAP
        /// </summary>
        public string VersaoSoap
        {
            get => string.IsNullOrWhiteSpace(_VersaoSoap) ? (_VersaoSoap = "soap12") : _VersaoSoap;
            set => _VersaoSoap = value;
        }

        /// <summary>
        /// Dados do servidor de proxy para conexão
        /// </summary>
        public IWebProxy Proxy
        {
            get => _Proxy;
            set => _Proxy = value;
        }

        /// <summary>
        /// Definir o encoding do XML retornado pelo webservice
        /// </summary>
        public string EncodingRetorno
        {
            get => string.IsNullOrWhiteSpace(_EncodingRetorno) ? (_EncodingRetorno = "UTF-8") : _EncodingRetorno;
            set => _EncodingRetorno = value;
        }

        /// <summary>
        /// Definir se a mensagem enviada para o webservice deve ser compactada com GZIP ou não.
        /// </summary>
        public bool GZIPCompress
        {
            get => _GZIPCompress;
            set => _GZIPCompress = value;
        }

        /// <summary>
        /// Tempo, em milissegundos, para aguardar resposta dos WebServices. (default = 60000)
        /// </summary>
        public int TimeOutWebServiceConnect 
        { 
            get
            {
                return _TimeOutWebServiceConnect <= 0 ? (_TimeOutWebServiceConnect = 60000) : _TimeOutWebServiceConnect;
            }
            set => _TimeOutWebServiceConnect = value;
        }

        #endregion Public Properties
    }
}