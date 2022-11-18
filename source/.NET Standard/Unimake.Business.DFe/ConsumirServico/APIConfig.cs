using System.Dynamic;
using System.Net;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para definição de parâmetros da API
    /// </summary>
    public class APIConfig
    {
        private bool _B64;
        private string _RequestURI;
        private string _ContentType;
        private string _TagRetorno;
        private string _MunicipioUsuario;
        private string _MunicipioSenha;
        private string _WebSoapString;
        private string _MetodoAPI;
        private bool _GZipCompress;
        private string _chAcesso;
        private PadraoNFSe _PadraoNFSe;
        private Servico _Servico;
        private string _WebActionProducao;
        private IWebProxy _Proxy;

        /// <summary>
        /// Configuracões de proxy
        /// </summary>
        public IWebProxy Proxy
        {
            get => _Proxy;
            set => _Proxy = value;
        }

        /// <summary>
        /// Prefixo de envio para solicitação da API
        /// </summary>
        public string WebAction
        {
            get => _WebActionProducao;
            set => _WebActionProducao = value;
        }

        /// <summary>
        /// Confirmação da B64
        /// </summary>
        public bool B64
        {
            get => _B64;
            set => _B64= value;
        }

        /// <summary>
        /// Método de solicitação da API
        /// </summary>
        public string MetodoAPI
        {
            get => _MetodoAPI;
            set => _MetodoAPI = value;
        }

        /// <summary>
        /// String de comunicação com a API
        /// </summary>
        public string WebSoapString
        {
            get => _WebSoapString;
            set => _WebSoapString = value;
        }

        /// <summary>
        /// Serviço selecionado, utilizado para resgatar a chave dentro do arquivo xml, necessário pois há tamanhos diferentes de chaves, conforme o serviço a ser executado
        /// </summary>
        public Servico Servico
        {
            get => _Servico;
            set => _Servico = value;
        }

        /// <summary>
        /// Padrão selecionado da API, necessário para tratar configurações exclusivas de cada API
        /// </summary>
        public PadraoNFSe PadraoNFSeAPI
        {
            get => _PadraoNFSe;
            set => _PadraoNFSe = value;
        }

        /// <summary>
        /// Chave de acesso, resgatada pelo xml, necessário para facilitar a transição da informação pelas camadas da DLL
        /// </summary>
        public string chaveAcesso
        {
            get => _chAcesso;
            set => _chAcesso = value;
        }

        /// <summary>
        /// Verifica se o arquivo precisa ser em GZip
        /// </summary>
        public bool GZipCompress
        {
            get => _GZipCompress;
            set => _GZipCompress = value;
        }

        /// <summary>
        /// Usuario para loguin no sistema do município
        /// </summary>
        public string MunicipioUsuario
        {
            get => _MunicipioUsuario;
            set => _MunicipioUsuario = value;
        }

        /// <summary>
        /// Senha para loguin no sistema do município
        /// </summary>
        public string MunicipioSenha
        {
            get => _MunicipioSenha;
            set => _MunicipioSenha = value;
        }
        /// <summary>
        /// HttpAddress - Endereço da API
        /// </summary>
        public string RequestURI
        {
            get => _RequestURI;
            set => _RequestURI = value;
        }

        /// <summary>
        /// Content Type - Tipo do conteúdo
        /// </summary>
        public string ContentType
        {
            get => string.IsNullOrWhiteSpace(_ContentType) ? (_ContentType = "application/json") : _ContentType;
            set => _ContentType = value;
        }

        /// <summary>
        /// Tag alvo retornada pela API com o conteúdo desejado
        /// </summary>
        public string TagRetorno
        {
            get => _TagRetorno;
            set => _TagRetorno = value;           
        }
    }
}