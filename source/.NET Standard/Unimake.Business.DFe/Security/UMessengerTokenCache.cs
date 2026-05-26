using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Text;

namespace Unimake.Business.DFe.Security
{
    internal static class UMessengerTokenCache
    {
        private sealed class TokenEntry
        {
            public string Token { get; set; }
            public DateTimeOffset ExpiresAtUtc { get; set; }
        }

        private sealed class AuthRequest
        {
            [JsonProperty("appId")]
            public string AppId { get; set; }

            [JsonProperty("secret")]
            public string Secret { get; set; }
        }

        private sealed class AuthResponse
        {
            [JsonProperty("token")]
            public string Token { get; set; }

            [JsonProperty("expiration")]
            public double Expiration { get; set; }

            [JsonProperty("type")]
            public string Type { get; set; }
        }

        private static readonly object CacheLock = new object();
        private static readonly Dictionary<string, TokenEntry> Cache = new Dictionary<string, TokenEntry>(StringComparer.OrdinalIgnoreCase);

        /// <summary>
        /// Retorna um token válido do cache, ou adquire um novo via AppId/Secret.
        /// </summary>
        public static string GetOrAcquireToken(string appId, string secret, bool useHomolog, string loginUrlProducao, string loginUrlHomologacao)
        {
            if (string.IsNullOrWhiteSpace(appId) || string.IsNullOrWhiteSpace(secret))
            {
                throw new Exception("AppId e Secret são obrigatórios para autenticação no uMessenger.");
            }

            var cacheKey = (useHomolog ? "H" : "P") + "|" + appId;

            lock (CacheLock)
            {
                if (Cache.TryGetValue(cacheKey, out var entry) && entry.ExpiresAtUtc > DateTimeOffset.UtcNow)
                {
                    return entry.Token;
                }
            }

            var authUrl = useHomolog ? loginUrlHomologacao : loginUrlProducao;

            if (string.IsNullOrWhiteSpace(authUrl))
            {
                throw new Exception("URL de autenticação do uMessenger não configurada.");
            }

            var payload = JsonConvert.SerializeObject(new AuthRequest { AppId = appId, Secret = secret });
            var bytes = Encoding.UTF8.GetBytes(payload);

            var request = (HttpWebRequest)WebRequest.Create(authUrl);
            request.Method = "POST";
            request.ContentType = "application/json";
            request.Accept = "application/json";
            request.ContentLength = bytes.Length;

            using (var reqStream = request.GetRequestStream())
            {
                reqStream.Write(bytes, 0, bytes.Length);
            }

            string responseStr;
            try
            {
                using (var response = (HttpWebResponse)request.GetResponse())
                using (var reader = new StreamReader(response.GetResponseStream(), Encoding.UTF8))
                {
                    responseStr = reader.ReadToEnd();
                }
            }
            catch (WebException ex) when (ex.Response != null)
            {
                using (var reader = new StreamReader(ex.Response.GetResponseStream(), Encoding.UTF8))
                {
                    var errorBody = reader.ReadToEnd();
                    throw new Exception($"Erro na autenticação do uMessenger: {errorBody}", ex);
                }
            }

            var authResponse = JsonConvert.DeserializeObject<AuthResponse>(responseStr);

            if (authResponse == null || string.IsNullOrWhiteSpace(authResponse.Token))
            {
                throw new Exception("Retorno do Auth inválido para o uMessenger.");
            }

            var expiration = authResponse.Expiration > 0
                ? TimeSpan.FromMilliseconds(authResponse.Expiration)
                : TimeSpan.FromMinutes(15);
            var expiresAt = DateTimeOffset.UtcNow.Add(expiration).AddSeconds(-30);

            lock (CacheLock)
            {
                Cache[cacheKey] = new TokenEntry
                {
                    Token = authResponse.Token,
                    ExpiresAtUtc = expiresAt
                };
            }

            return authResponse.Token;
        }
    }
}
