// Ignore Spelling: Interop

#if INTEROP
using System.Runtime.InteropServices;
#endif

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
        public static bool HasInternetConnection(IWebProxy proxy) => Unimake.Net.Utility.HasInternetConnection(proxy);

        /// <summary>
        /// Verifica a conexão com a internet e retorna verdadeiro se conectado com sucesso
        /// </summary>
        /// <returns>true = Tem conexão com a internet</returns>
        public static bool HasInternetConnection() => HasInternetConnection(null);
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
