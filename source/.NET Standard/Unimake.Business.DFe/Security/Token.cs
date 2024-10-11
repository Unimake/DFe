using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;
using Newtonsoft.Json;

namespace Unimake.Business.DFe.Security
{
    /// <summary>
    /// Classe criada para o consumo do padrão AGILI, com possibilidade de generalização de padrões com necessidade de um token para emissão de DFe
    /// </summary>
    internal class Token
    {
        #region Privte Properties

        [JsonProperty(PropertyName = "access_token")]
        public string AccessToken { get; set; }

        [JsonProperty(PropertyName = "token_type")]
        public string TokenType { get; set; }

        [JsonProperty(PropertyName = "refresh_token")]
        public string RefreshToken { get; set; }

        [JsonProperty(PropertyName = "expires_in")]
        public double ExpiresIn { get; set; }

        [JsonProperty(PropertyName = "scope")]
        public string Scope { get; set; }

        #endregion Privte Properties

        #region Public Methods

        /// <summary>
        /// A princípio, apenas para o consumo do padrão AGILI
        /// </summary>
        /// <param name="proxy"></param>
        /// <param name="usuario"></param>
        /// <param name="senha"></param>
        /// <param name="clientID"></param>
        /// <param name="clientSecret"></param>
        /// <returns></returns>
        public static Token GerarToken(IWebProxy proxy, string usuario, string senha, string clientID, string clientSecret)
        {
            var url = "http://agiliblue.agilicloud.com.br/api/";
            string result = string.Empty;
            var tokenResult = new Token();
            senha = Criptografia.descriptografaSenha(senha);

            //var dictionary = new Dictionary<string, string>()
            //{
            //         {"grant_type", "password" },
            //         {"username", usuario },
            //         {"password", GerarMD5(senha).ToUpper() },
            //         {"client_id", clientID},
            //         {"client_secret", clientSecret}
            //};

            var postParameter = "&grant_type=password"
                                + $"username={usuario}"
                                + $"password={GerarMD5(senha).ToUpper()}"
                                + $"client_id={clientID}"
                                + $"client_secret={clientSecret}";

            url += postParameter;
            try
            {
                var request = WebRequest.Create(url);

                request.Method = "POST";
                //request.KeepAlive = true;
                request.Credentials = CredentialCache.DefaultCredentials;

                if (proxy != null)
                {
                    request.UseDefaultCredentials = false;
                    request.Proxy = proxy;
                    request.Proxy.Credentials = proxy.Credentials;
                    request.Credentials = proxy.Credentials;
                }

                //ServicePointManager.Expect100Continue = false;

                var response = default(WebResponse);
                try
                {
                    response = request.GetResponse();

                }
                catch(WebException e)
                {
                    response = e.Response;
                }
                
                var streamDados = response.GetResponseStream();
                var reader = new StreamReader(streamDados);
                result = reader.ReadToEnd();
                streamDados.Close();
                response.Close();
                response.Dispose();


                //SetAllowUnsafeHeaderParsing20() = caso necessite

                //using (POSTRequest post = new POSTRequest
                //{
                //    Proxy = proxy
                //})
                //{
                //    string autorization = Functions.Base64Encode($"{clientID}:{clientSecret}");

                //    IList<string> autorizations = new List<string>()
                //{
                //    $"Authorization: Basic {autorization}"
                //};

                //    result = post.PostForm(Path.Combine(url, "autenticacao/oauth/token"), new Dictionary<string, string> {
                //     {"grant_type", "password" },
                //     {"username", usuario },
                //     {"password", Functions.GerarMD5(senha).ToUpper() },
                //     {"client_id", clientID},
                //     {"client_secret", clientSecret}
                //}, autorizations);
                //}

                var token = JsonConvert.DeserializeObject<Token>(result);

                if (token.AccessToken == null)
                    throw new Exception("O token informado é inválido");

                tokenResult = token;
            }
            catch (Exception ex)
            {
                throw ex;
            }

            return tokenResult;
        }


        #endregion Public Methods
        public static string GerarMD5(string valor)
        {
            // Cria uma nova intância do objeto que implementa o algoritmo para
            // criptografia MD5
            var md5Hasher = System.Security.Cryptography.MD5.Create();

            // Criptografa o valor passado
            var valorCriptografado = md5Hasher.ComputeHash(Encoding.Default.GetBytes(valor));

            // Cria um StringBuilder para passarmos os bytes gerados para ele
            var strBuilder = new StringBuilder();

            // Converte cada byte em um valor hexadecimal e adiciona ao
            // string builder
            // and format each one as a hexadecimal string.
            for (var i = 0; i < valorCriptografado.Length; i++)
            {
                strBuilder.Append(valorCriptografado[i].ToString("x2"));
            }

            // retorna o valor criptografado como string
            return strBuilder.ToString();
        }
    }
}
