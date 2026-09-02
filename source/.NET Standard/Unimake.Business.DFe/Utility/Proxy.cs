#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
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

            AplicarCredenciais(proxy, usuario, senha, exigirParCompleto: false);

            return proxy;
        }

        /// <summary>
        /// Define o servidor de proxy, utilizando o proxy do sistema ou um endpoint manual.
        /// </summary>
        /// <param name="servidor">Nome, endereço IP ou URI HTTP/HTTPS do proxy manual.</param>
        /// <param name="porta">Porta do proxy manual.</param>
        /// <param name="detectarAutomaticamente">Detectar o proxy pelas configurações do sistema operacional?</param>
        /// <param name="usuario">Usuário de conexão do proxy, quando houver autenticação explícita.</param>
        /// <param name="senha">Senha do usuário, quando houver autenticação explícita.</param>
        /// <returns>Proxy configurado para o transporte HTTP.</returns>
        public static IWebProxy DefinirServidor(string servidor, int porta, bool detectarAutomaticamente = false, string usuario = "", string senha = "")
        {
            IWebProxy proxy;
            if(detectarAutomaticamente)
            {
                proxy = WebRequest.GetSystemWebProxy();
            }
            else
            {
                if(string.IsNullOrWhiteSpace(servidor))
                {
                    throw new ArgumentException("O servidor de proxy deve ser informado.", nameof(servidor));
                }

                if(porta < 1 || porta > 65535)
                {
                    throw new ArgumentOutOfRangeException(nameof(porta), "A porta do proxy deve estar entre 1 e 65535.");
                }

                var endereco = CriarEndereco(servidor, porta);
                proxy = new WebProxy(endereco);
            }

            AplicarCredenciais(proxy, usuario, senha, exigirParCompleto: true);

            return proxy;
        }

        private static void AplicarCredenciais(IWebProxy proxy, string usuario, string senha, bool exigirParCompleto)
        {
            var informouUsuario = !string.IsNullOrWhiteSpace(usuario);
            var informouSenha = !string.IsNullOrWhiteSpace(senha);
            if(exigirParCompleto && informouUsuario != informouSenha)
            {
                throw new ArgumentException("Usuário e senha do proxy devem ser informados em conjunto.");
            }

            if(proxy != null)
            {
                if(informouUsuario || informouSenha)
                {
                    proxy.Credentials = new NetworkCredential(usuario, senha);
                }
            }
        }

        private static Uri CriarEndereco(string servidor, int porta)
        {
            var valor = servidor.Trim();
            Uri endereco;
            if(Uri.TryCreate(valor, UriKind.Absolute, out endereco))
            {
                if(endereco.Scheme != Uri.UriSchemeHttp && endereco.Scheme != Uri.UriSchemeHttps)
                {
                    throw new ArgumentException("O servidor de proxy deve usar HTTP ou HTTPS.", nameof(servidor));
                }

                if(endereco.AbsolutePath != "/" || !string.IsNullOrEmpty(endereco.Query) || !string.IsNullOrEmpty(endereco.Fragment))
                {
                    throw new ArgumentException("O servidor de proxy não pode conter caminho, consulta ou fragmento.", nameof(servidor));
                }

                return new UriBuilder(endereco) { Port = porta }.Uri;
            }

            try
            {
                return new UriBuilder(Uri.UriSchemeHttp, valor, porta).Uri;
            }
            catch(UriFormatException ex)
            {
                throw new ArgumentException("O servidor de proxy informado é inválido.", nameof(servidor), ex);
            }
        }
    }
}
