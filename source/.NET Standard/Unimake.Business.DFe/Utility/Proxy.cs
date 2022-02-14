#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Net;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Definições para conexão com servidor de proxy
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.Proxy")]
    [ComVisible(true)]
#endif
    public static class Proxy
    {
        /// <summary>
        /// Definir servidor de Proxy
        /// </summary>
        /// <param name="usuario">usuário de conexão do proxy</param>
        /// <param name="senha">senha do usuário</param>
        /// <param name="detectarAutomaticamente">Detectar proxy automaticamente?</param>
        /// <example>
        /// //Detectar os dados de conexão do proxy automaticamente
        /// DefinirProxy(true);
        /// 
        /// //Passar os dados do proxy para conexão manualmente
        /// DefinirProxy(false, "user", "pass");
        /// </example>
        /// <returns></returns>
        public static IWebProxy DefinirServidor(bool detectarAutomaticamente = false, string usuario = "", string senha = "")
        {
            var proxy = (detectarAutomaticamente ? WebRequest.GetSystemWebProxy() : WebRequest.DefaultWebProxy);

            if(proxy != null)
            {
                if(!string.IsNullOrEmpty(usuario) && !string.IsNullOrEmpty(senha))
                {
                    proxy.Credentials = new NetworkCredential(usuario, senha);
                }
            }

            return proxy;
        }
    }
}
