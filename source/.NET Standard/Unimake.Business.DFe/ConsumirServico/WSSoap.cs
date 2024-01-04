using System.Net;
using Unimake.Business.DFe.Servicos;

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
        private PadraoNFSe _PadraoNFSe;
        private bool _UsaCertificadoDigital;
        private string _TagRetornoHomologacao;
        private TipoAmbiente _TipoAmbiente;
        private bool _ConverteSenhaBase64;
        private string _MunicipioSenha;
        private string _MunicipioUsuario;
        private bool _EncriptaTagAssinatura;
        private string _Token;

        #endregion Private Fields

        #region Public Properties


        /// <summary>
        /// Encriptação de assinatura usada no padrão DSF 1.0
        /// </summary>
        public bool EncriptaTagAssinatura
        {
            get => _EncriptaTagAssinatura;
            set => _EncriptaTagAssinatura = value;
        }

        /// <summary>
        /// Municipio Senha
        /// </summary>
        public string Token
        {
            get => _Token;
            set => _Token = value;
        }

        /// <summary>
        /// Utiliza ou não a conversão Base64
        /// </summary>
        public string MunicipioSenha
        {
            get => _MunicipioSenha;
            set => _MunicipioSenha = value;
        }

        /// <summary>
        /// Municipio Usuario
        /// </summary>
        public string MunicipioUsuario
        {
            get => _MunicipioUsuario;
            set => _MunicipioUsuario = value;
        }

        /// <summary>
        /// Utiliza ou não a conversão Base64
        /// </summary>
        public bool ConverteSenhaBase64
        {
            get => _ConverteSenhaBase64;
            set => _ConverteSenhaBase64 = value;
        }

        /// <summary>
        /// Utiliza ou não o certificado digital
        /// </summary>
        public bool UsaCertificadoDigital
        {
            get => _UsaCertificadoDigital;
            set => _UsaCertificadoDigital = value;
        }

        /// <summary>
        /// Web Action - Endereço com a ação/método que será executado no webservice
        /// </summary>
        public string ActionWeb
        {
            get => _ActionWeb;
            set => _ActionWeb = value;
        }

        /// <summary>
        /// Define o padrão utilizado
        /// </summary>
        public PadraoNFSe PadraoNFSe
        {
            get => _PadraoNFSe;
            set => _PadraoNFSe = value;
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
        /// Nome da tag de retorno de conteúdo que é devolvido pelo WebService
        /// </summary>
        public string TagRetornoHomologacao
        {
            get => _TagRetornoHomologacao;
            set => _TagRetornoHomologacao = value;
        }

        /// <summary>
        /// Tipo ambiente
        /// </summary>
        public TipoAmbiente TipoAmbiente
        {
            get => _TipoAmbiente;
            set => _TipoAmbiente = value;
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
            get => _TimeOutWebServiceConnect <= 0 ? (_TimeOutWebServiceConnect = 60000) : _TimeOutWebServiceConnect;
            set => _TimeOutWebServiceConnect = value;
        }

        #endregion Public Properties
    }
}