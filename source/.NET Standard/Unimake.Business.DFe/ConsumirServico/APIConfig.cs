using System.Dynamic;

namespace Unimake.Business.DFe
{
    /// <summary>
    /// Classe para definição de parâmetros da API
    /// </summary>
    public class APIConfig
    {
        private string _RequestURI;
        private string _ContentType;
        private string _TagRetorno;
        private string _MunicipioUsuario;
        private string _MunicipioSenha;
        private bool _GZipCompress;

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
        /// 
        /// </summary>
        public string TagRetorno
        {
            get => _TagRetorno;
            set => _TagRetorno = value;           
        }
    }
}