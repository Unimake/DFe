// Ignore Spelling: Interop

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Net;
using System.Net.NetworkInformation;
using System.Security.Cryptography.X509Certificates;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Utilitários de rede e internet
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
            if (timeoutInSeconds <= 0)
            {
                throw new ArgumentOutOfRangeException("O valor  do parâmetro 'timeoutInSeconds' deve ser maior que zero.");
            }

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
                if (url.Substring(0, 7).Equals("http://"))
                {
                    try
                    {
                        retorno = TestHttpConnection(url, null, timeoutInSeconds);
                    }
                    catch
                    {
                        retorno = false;
                    }
                }
                else
                {
                    // Testar conexão com IP direto do Google
                    retorno = PingHost(url, timeoutInSeconds);
                }

                if (retorno)
                {
                    break;
                }
            }

            return retorno;
        }

        /// <summary>
        /// Testar conexão HTTP ou HTTPS
        /// </summary>
        /// <param name="url">URL a ser testada</param>
        /// <param name="certificate">Certificado a ser utilizado para conexões https</param>
        /// <param name="proxy">Configuração de proxy, caso exista</param>
        /// <returns>Se a URL está respondendo, ou não</returns>
        public static bool TestHttpConnection(string url, X509Certificate2 certificate = null, int timeoutInSeconds = 3, IWebProxy proxy = null)
        {
            try
            {
                var httpWebRequest = (HttpWebRequest)WebRequest.Create(url);
                if (proxy != null)
                {
                    httpWebRequest.Proxy = proxy;
                }

                if (certificate != null)
                {
                    httpWebRequest.ClientCertificates.Add(certificate);
                }

                httpWebRequest.Timeout = timeoutInSeconds * 1000;
                httpWebRequest.ReadWriteTimeout = timeoutInSeconds * 1000;

                var statusCode = (httpWebRequest.GetResponse() as HttpWebResponse).StatusCode;
                return statusCode == HttpStatusCode.OK || statusCode == HttpStatusCode.NoContent;
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Testar para ver o servidor de DNS dá um PING
        /// </summary>
        /// <param name="ipAddress">IP</param>
        /// <param name="timeoutInSeconds">Tempo máximo para ter uma resposta</param>
        /// <returns>Se teve sucesso no PING</returns>
        public static bool PingHost(string ipAddress, int timeoutInSeconds)
        {
            try
            {
                var ping = new Ping();
                var reply = ping.Send(ipAddress, timeoutInSeconds * 1000);
                return (reply.Status == IPStatus.Success);
            }
            catch (PingException)
            {
                return false;
            }
        }

        /// <summary>
        /// Extrair o domínio de uma URL
        /// </summary>
        /// <param name="url">URL para extrair o domínio</param>
        /// <returns>Domínio da URL</returns>
        public static string ExtractDomain(string url)
        {
            try
            {
                var uri = new Uri(url);
                return uri.Host;
            }
            catch
            {
                throw;
            }
        }

        /// <summary>
        /// Obter o endereço de IP de um domínio
        /// </summary>
        /// <param name="domain">Domínio que é para obter o IP</param>
        /// <returns>IP do domínio</returns>
        public static string GetIpAddressDomain(string domain)
        {
            try
            {
                var addresses = Dns.GetHostAddresses(domain);
                if (addresses.Length > 0)
                {
                    return addresses[0].ToString();
                }
                else
                {
                    throw new Exception("Nenhum endereço de IP encontrado.");
                }
            }
            catch
            {
                throw;
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
