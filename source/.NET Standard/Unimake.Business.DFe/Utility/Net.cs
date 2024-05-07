// Ignore Spelling: Interop

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Net;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Utilitários de rede
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public static class Net
    {
        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <param name="proxy">Proxy a ser utilizado para testar a conexão</param>
        /// <returns>true = Tem conexão com a internet</returns>
        public static bool HasInternetConnection(IWebProxy proxy) => HasInternetConnection(proxy, 3);
        //Quando o Marcelo liberar novos NUGET, passar a utilizar este Unimake.Net.Utility.HasInternetConnection(proxy, 3)

        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <returns>true = Tem conexão com a internet</returns>
        public static bool HasInternetConnection() => HasInternetConnection(null);

        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <param name="proxy">Proxy a ser utilizado para testar a conexão</param>
        /// <param name="timeoutInSeconds">Tempo para tentativa de conexão em segundos</param>
        /// <param name="testUrls">URLs a serem testadas, se não informada o método utilizará 5 URLs para o teste, se uma delas funcionar, vai retornar que a conexão está ok</param>
        /// <returns>true = Tem conexão com a internet</returns>
        public static bool HasInternetConnection(IWebProxy proxy, int timeoutInSeconds = 3, string[] testUrls = null)
        {
            if (testUrls == null)
            {
                testUrls = new string[] {
                    "http://clients3.google.com/generate_204",
                    "http://www.microsoft.com",
                    "http://www.cloudflare.com",
                    "http://www.amazon.com",
                    "http://www.unimake.com.br"
                };
            }

            var retorno = true;
            var timeoutMilleSeconds = (timeoutInSeconds * 1000);

            foreach (var url in testUrls)
            {
                var httpWebRequest = (HttpWebRequest)WebRequest.Create(url);
                if (proxy != null)
                {
                    httpWebRequest.Proxy = proxy;
                }

                try
                {
                    retorno = HasInternetConnection(httpWebRequest, timeoutMilleSeconds);
                }
                catch
                {
                    retorno = false;
                }

                if (retorno)
                {
                    break;
                }
            }

            return retorno;
        }

        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <param name="client">Client a ser testado</param>
        /// <param name="timeoutInMilliSeconds">Tempo de retorno em mile segundos</param>
        /// <returns>true=Se tudo ok com a internet</returns>
        /// <exception cref="ArgumentNullException">Quantidade </exception>
        /// <exception cref="ArgumentOutOfRangeException"></exception>
        private static bool HasInternetConnection(HttpWebRequest client, int timeoutInMilliSeconds)
        {
            if (client == null)
            {
                throw new ArgumentNullException("client");
            }

            if (timeoutInMilliSeconds <= 0)
            {
                throw new ArgumentOutOfRangeException("O valor  do parâmetro 'timeoutInSeconds' deve ser maior que zero.");
            }

            try
            {
                client.Timeout = timeoutInMilliSeconds;
                client.ReadWriteTimeout = timeoutInMilliSeconds;

                var statusCode = (client.GetResponse() as HttpWebResponse).StatusCode;

                return statusCode == HttpStatusCode.OK || statusCode == HttpStatusCode.NoContent;
            }
            catch
            {
                return false;
            }
        }
    }

#if INTEROP

    /// <summary>
    /// Utilitários de rede
    /// </summary>
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.NetInterop")]
    [ComVisible(true)]
    public class NetInterop
    {
        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <returns>true = Tem conexão com a internet</returns>
        public bool HasInternetConnection() => Net.HasInternetConnection(null);

        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <param name="server">Endereço de servidor do proxy</param>
        /// <param name="user">Usuário do proxy</param>
        /// <param name="password">Senha de acesso do proxy</param>
        /// <param name="port">Porta</param>
        /// <param name="autoDetect">Se verdadeiro, utiliza o proxy padrão do sistema</param>
        /// <returns>true = Tem conexão com a internet</returns>
        public bool HasInternetConnectionWithProxy(string server, string user, string password, int port, bool autoDetect = false) => Net.HasInternetConnection(Unimake.Net.Utility.GetProxy(server, user, password, port, autoDetect));
    }

#endif
}
