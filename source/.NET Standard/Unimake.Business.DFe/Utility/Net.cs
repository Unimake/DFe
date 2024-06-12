// Ignore Spelling: Interop

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Net;

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
        public static bool HasInternetConnection(IWebProxy proxy) => Unimake.Net.Utility.HasInternetConnection(proxy, 3);
        //Quando o Marcelo liberar novos NUGET, passar a utilizar este 

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
        public static bool HasInternetConnection(IWebProxy proxy, int timeoutInSeconds = 3, string[] testUrls = null) => Unimake.Net.Utility.HasInternetConnection(proxy, timeoutInSeconds, testUrls);

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
