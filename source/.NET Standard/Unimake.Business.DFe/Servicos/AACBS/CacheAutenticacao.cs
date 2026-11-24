using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Security.Cryptography;
using System.Text;
using Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    internal static class CacheAutenticacao
    {
        // A cota é do par emissor/ClientID. A revisão inclui também ambiente e segredo.
        private sealed class Cliente
        {
            internal readonly Queue<DateTimeOffset> Tentativas = new Queue<DateTimeOffset>();
            internal readonly Dictionary<string, Token> Tokens = new Dictionary<string, Token>();
            internal DateTimeOffset UltimoUso;
            internal int Usuarios;
        }

        private sealed class Token
        {
            internal RetAutenticar Resposta;
            internal DateTimeOffset UtilizavelAte;
            internal Cliente Cliente;
            internal string Revisao;
        }

        private static readonly Dictionary<string, Cliente> Clientes = new Dictionary<string, Cliente>();
        private static readonly ConditionalWeakTable<RetAutenticar, Token> Emitidos = new ConditionalWeakTable<RetAutenticar, Token>();
        private static readonly TimeSpan Janela = TimeSpan.FromHours(1);

        internal static RetAutenticar Obter(string emissor, TipoAmbiente ambiente, string id, string segredo,
            Func<DateTimeOffset> relogio, Func<RetAutenticar> emitir)
        {
            if (string.IsNullOrEmpty(id) || string.IsNullOrEmpty(segredo))
                throw new InvalidOperationException("AACBS: informe ClientID e ClientSecret para autenticar.");

            var identidade = Derivar(emissor, id);
            var revisao = Derivar(identidade, ambiente.ToString(), segredo);
            Cliente cliente;
            lock (Clientes)
            {
                var agora = DateTimeOffset.UtcNow;
                foreach (var expirada in Clientes.Where(p => p.Value.Usuarios == 0 && agora - p.Value.UltimoUso >= Janela)
                    .Select(p => p.Key).ToArray())
                    Clientes.Remove(expirada);

                if (!Clientes.TryGetValue(identidade, out cliente))
                    Clientes.Add(identidade, cliente = new Cliente());
                cliente.Usuarios++;
            }
            try
            {
                lock (cliente)
                {
                    var inicio = relogio();
                    foreach (var antiga in cliente.Tokens.Where(p => p.Value.UtilizavelAte <= inicio).Select(p => p.Key).ToArray())
                        cliente.Tokens.Remove(antiga);

                    if (cliente.Tokens.TryGetValue(revisao, out var existente))
                        return existente.Resposta;

                    while (cliente.Tentativas.Count != 0 && inicio - cliente.Tentativas.Peek() >= Janela)
                        cliente.Tentativas.Dequeue();
                    if (cliente.Tentativas.Count >= 2)
                        throw new InvalidOperationException("AACBS: limite local de duas tentativas de emissão de token em uma hora.");

                    cliente.Tentativas.Enqueue(inicio);
                    var resposta = emitir();
                    if (resposta.Error != null) return resposta;

                    var segundos = Math.Min(resposta.ExpiresIn ?? 3600, 3600);
                    var validade = inicio.AddSeconds(segundos - Math.Min(60, segundos / 2.0));
                    if (relogio() >= validade)
                        throw new InvalidOperationException("AACBS: token recebido fora do prazo de utilização.");
                    var token = new Token { Cliente = cliente, Revisao = revisao, Resposta = resposta, UtilizavelAte = validade };
                    cliente.Tokens[revisao] = token;
                    Emitidos.Add(resposta, token);
                    return resposta;
                }
            }
            finally
            {
                lock (Clientes)
                {
                    cliente.UltimoUso = DateTimeOffset.UtcNow;
                    cliente.Usuarios--;
                }
            }
        }

        internal static void Invalidar(RetAutenticar usado)
        {
            if (!Emitidos.TryGetValue(usado, out var emissao)) return;
            lock (emissao.Cliente)
            {
                if (emissao.Cliente.Tokens.TryGetValue(emissao.Revisao, out var atual) && ReferenceEquals(emissao, atual))
                    emissao.Cliente.Tokens.Remove(emissao.Revisao);
            }
        }

        private static string Derivar(params string[] componentes)
        {
            using (var dados = new MemoryStream())
            using (var escritor = new BinaryWriter(dados, Encoding.UTF8, true))
            using (var hash = SHA256.Create())
            {
                // BinaryWriter prefixa cada componente com seu comprimento.
                foreach (var componente in componentes) escritor.Write(componente);
                escritor.Flush();
                dados.Position = 0;
                return Convert.ToBase64String(hash.ComputeHash(dados));
            }
        }
    }
}
