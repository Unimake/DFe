// Ignore Spelling: Interop

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Net;
using System.Net.NetworkInformation;

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
                    "8.8.8.8", //Servidor Primário de DNS do Google
                    "8.8.4.4", //Servidor Secundário de DNS do Google
                    "http://www.microsoft.com",
                    "http://www.cloudflare.com",
                    "1.1.1.1", //Servidor Primário de DNS do Cloudfare
                    "1.0.0.1",  //Servidor Secundário de DNS do Cloudfare
                    "http://www.amazon.com",
                    "9.9.9.9", //Servidor Primário de DNS do Quad 9
                    "149.112.112.112", //Servidor Secundário de DNS do Quad 9
                    "http://www.unimake.com.br",
                    "http://67.205.183.164"
                };
            }

            var retorno = true;
            var timeoutMilleSeconds = (timeoutInSeconds * 1000);

            foreach (var url in testUrls)
            {
                if (url.Substring(0,7).Equals("http://"))
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
                }
                else
                {
                    // Testar conexão com IP direto do Google
                    retorno = TestConnectionToIp(url, timeoutInSeconds);
                }

                if (retorno)
                {
                    break;
                }
            }

            return retorno;
        }

        /// <summary>
        /// Testar para ver o servidor de DNS dá um PING
        /// </summary>
        /// <param name="ipAddress">IP</param>
        /// <param name="timeoutInSeconds">Tempo máximo para ter uma resposta</param>
        /// <returns>Se teve sucesso no PING</returns>
        private static bool TestConnectionToIp(string ipAddress, int timeoutInSeconds)
        {
            try
            {
                Ping ping = new Ping();
                PingReply reply = ping.Send(ipAddress, timeoutInSeconds * 1000);
                return (reply.Status == IPStatus.Success);
            }
            catch (PingException ex)
            {
                // Log ex.Message for debugging if necessary
                return false;
            }
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
