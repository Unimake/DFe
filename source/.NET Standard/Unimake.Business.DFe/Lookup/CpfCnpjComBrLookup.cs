#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace Unimake.Business.DFe.Lookup
{
    /// <summary>
    /// Implementação de referência de <see cref="IPessoaLookup"/> que consome a API
    /// pública cpfcnpj.com.br (<c>GET https://api.cpfcnpj.com.br/{token}/{pacote}/{documento}</c>).
    /// O token é informado no construtor e fica atrelado ao IP de origem.
    /// A chamada HTTP usa <see cref="HttpClient"/> e a leitura do JSON usa <c>Newtonsoft.Json</c>,
    /// as mesmas dependências já usadas pelo núcleo da biblioteca.
    /// Erros de negócio (campo <c>status</c> diferente de 1) e falhas de transporte
    /// são convertidos em <see cref="PessoaLookupException"/>.
    /// </summary>
    /// <remarks>
    /// Esta classe é totalmente opcional e aditiva: o núcleo da biblioteca não a referencia.
    /// Proxy e timeout são decisões explícitas: a instância compartilhada interna usa conexão direta
    /// (sem herdar o proxy do sistema) e o timeout recomendado pela documentação da API (60 segundos).
    /// Para outra política, use a sobrecarga com timeout e <see cref="IWebProxy"/> (por exemplo o retorno
    /// de <c>Unimake.Business.DFe.Utility.Proxy.DefinirServidor(...)</c>) ou injete o seu próprio
    /// <see cref="HttpClient"/>. Instâncias criadas pela sobrecarga com timeout/proxy possuem o próprio
    /// <see cref="HttpClient"/>: reutilize-as e chame <see cref="Dispose"/> ao descartar.
    /// A API não fornece Inscrição Estadual nos pacotes de endereço (1/3/5/6) nem Inscrição
    /// Municipal; a IE é obtida separadamente pelo pacote 16
    /// (<see cref="ConsultarInscricoesEstaduaisAsync"/>).
    /// </remarks>
#if INTEROP
    [ComVisible(false)]
#endif
    public class CpfCnpjComBrLookup : IPessoaLookup, IDisposable
    {
        private const string BaseUrl = "https://api.cpfcnpj.com.br/";

        /// <summary>
        /// Timeout padrão recomendado pela documentação da API (60 segundos).
        /// </summary>
        private static readonly TimeSpan TimeoutPadrao = TimeSpan.FromSeconds(60);

        private static readonly HttpClient SharedHttpClient = CriarHttpClient(TimeoutPadrao, null);

        private readonly HttpClient _httpClient;
        private readonly bool _httpClientProprio;
        private readonly string _token;
        private bool _disposed;

        /// <summary>
        /// Cria a implementação de consulta.
        /// </summary>
        /// <param name="token">
        /// Token da API cpfcnpj.com.br (atrelado ao IP de origem). Obrigatório.
        /// </param>
        /// <param name="httpClient">
        /// <see cref="HttpClient"/> a ser reutilizado (útil para injeção de dependência e testes).
        /// Quando nulo, usa uma instância compartilhada interna (conexão direta, timeout de 60 segundos).
        /// A instância informada continua sob responsabilidade de quem a criou.
        /// </param>
        /// <exception cref="ArgumentException">Quando o token é nulo ou vazio.</exception>
        public CpfCnpjComBrLookup(string token, HttpClient httpClient = null)
        {
            _token = ValidarToken(token);
            _httpClient = httpClient ?? SharedHttpClient;
            _httpClientProprio = false;
        }

        /// <summary>
        /// Cria a implementação de consulta com timeout e proxy explícitos.
        /// </summary>
        /// <param name="token">Token da API cpfcnpj.com.br (atrelado ao IP de origem). Obrigatório.</param>
        /// <param name="timeoutEmMilissegundos">
        /// Tempo limite da requisição HTTP, em milissegundos. Zero ou negativo usa o padrão de 60 segundos.
        /// </param>
        /// <param name="proxy">
        /// Proxy a utilizar (por exemplo o retorno de <c>Unimake.Business.DFe.Utility.Proxy.DefinirServidor(...)</c>,
        /// que já trata detecção automática, servidor, porta e credenciais). Nulo usa conexão direta.
        /// </param>
        /// <exception cref="ArgumentException">Quando o token é nulo ou vazio.</exception>
        public CpfCnpjComBrLookup(string token, int timeoutEmMilissegundos, IWebProxy proxy = null)
        {
            _token = ValidarToken(token);

            var timeout = timeoutEmMilissegundos > 0
                ? TimeSpan.FromMilliseconds(timeoutEmMilissegundos)
                : TimeoutPadrao;

            _httpClient = CriarHttpClient(timeout, proxy);
            _httpClientProprio = true;
        }

        /// <summary>
        /// Libera o <see cref="HttpClient"/> criado internamente pela sobrecarga com timeout/proxy.
        /// Instâncias compartilhadas ou injetadas não são liberadas.
        /// </summary>
        public void Dispose()
        {
            if(_disposed)
            {
                return;
            }

            _disposed = true;

            if(_httpClientProprio)
            {
                _httpClient.Dispose();
            }
        }

        private void GarantirNaoDescartado()
        {
            if(_disposed)
            {
                throw new ObjectDisposedException(nameof(CpfCnpjComBrLookup));
            }
        }

        private static string ValidarToken(string token)
        {
            if(string.IsNullOrWhiteSpace(token))
            {
                throw new ArgumentException("O token da API cpfcnpj.com.br é obrigatório.", nameof(token));
            }

            return token.Trim();
        }

        private static HttpClient CriarHttpClient(TimeSpan timeout, IWebProxy proxy)
        {
            // Mesma preparação de TLS usada pelos transportes do núcleo: em hosts .NET Framework o padrão
            // do processo pode não incluir TLS 1.2, exigido pela API.
            ServicePointManager.SecurityProtocol |= SecurityProtocolType.Tls12;

            var handler = new HttpClientHandler
            {
                UseProxy = proxy != null,
                Proxy = proxy
            };

            return new HttpClient(handler)
            {
                Timeout = timeout
            };
        }

        /// <inheritdoc/>
        public async Task<ResultadoPessoa> ConsultarCpfAsync(string cpf, bool comEndereco, CancellationToken cancellationToken = default(CancellationToken))
        {
            GarantirNaoDescartado();
            var documento = Normalizar(cpf);
            var pacote = comEndereco ? 3 : 1;
            var resposta = await GetAsync<CpfResposta>(pacote, documento, cancellationToken).ConfigureAwait(false);

            GarantirSucesso(resposta);

            var resultado = new ResultadoPessoa
            {
                Documento = documento,
                PessoaFisica = true,
                Nome = resposta.Nome
            };

            if(comEndereco)
            {
                resultado.Endereco = MapearEnderecoCpf(resposta.Endereco, resposta.Numero, resposta.Complemento,
                    resposta.Bairro, resposta.Cep, resposta.Cidade, resposta.Uf, resposta.Ibge);

                if(resposta.Enderecos != null)
                {
                    foreach(var extra in resposta.Enderecos)
                    {
                        resultado.EnderecosAdicionais.Add(MapearEnderecoCpf(extra.Endereco, extra.Numero, extra.Complemento,
                            extra.Bairro, extra.Cep, extra.Cidade, extra.Uf, extra.Ibge));
                    }
                }
            }

            return resultado;
        }

        /// <inheritdoc/>
        public async Task<ResultadoPessoa> ConsultarCnpjAsync(string cnpj, bool comSimplesNacional, CancellationToken cancellationToken = default(CancellationToken))
        {
            GarantirNaoDescartado();
            var documento = Normalizar(cnpj);
            var pacote = comSimplesNacional ? 6 : 5;
            var resposta = await GetAsync<CnpjResposta>(pacote, documento, cancellationToken).ConfigureAwait(false);

            GarantirSucesso(resposta);

            var resultado = new ResultadoPessoa
            {
                Documento = documento,
                PessoaFisica = false,
                Nome = resposta.Razao,
                NomeFantasia = resposta.Fantasia
            };

            var m = resposta.MatrizEndereco;
            if(m != null)
            {
                var codigoIbge = 0;
                if(resposta.Ibge != null && resposta.Ibge.Cidade != null)
                {
                    codigoIbge = resposta.Ibge.Cidade.IbgeId;
                }

                var uf = m.Uf;
                if(string.IsNullOrWhiteSpace(uf) && resposta.Ibge != null && resposta.Ibge.Estado != null)
                {
                    uf = resposta.Ibge.Estado.Sigla;
                }

                resultado.Endereco = new EnderecoPessoa
                {
                    Logradouro = ComporLogradouro(m.Tipo, m.Logradouro),
                    Numero = m.Numero,
                    Complemento = m.Complemento,
                    Bairro = m.Bairro,
                    Cep = SomenteDigitos(m.Cep),
                    Municipio = m.Cidade,
                    Uf = uf,
                    CodigoMunicipioIbge = codigoIbge,
                    Pais = "Brasil"
                };
            }

            if(comSimplesNacional && resposta.SimplesNacional != null)
            {
                resultado.OptanteSimplesNacional = InterpretarSimNao(resposta.SimplesNacional.Optante);
                resultado.Mei = InterpretarSimNao(resposta.SimplesNacional.Mei);
            }

            if(resposta.Situacao != null)
            {
                resultado.SituacaoCadastral = resposta.Situacao.Nome;
            }

            return resultado;
        }

        /// <inheritdoc/>
        public async Task<IReadOnlyList<InscricaoEstadualInfo>> ConsultarInscricoesEstaduaisAsync(string cnpj, CancellationToken cancellationToken = default(CancellationToken))
        {
            GarantirNaoDescartado();
            var documento = Normalizar(cnpj);
            var resposta = await GetAsync<IeResposta>(16, documento, cancellationToken).ConfigureAwait(false);

            GarantirSucesso(resposta);

            var lista = new List<InscricaoEstadualInfo>();
            if(resposta.InscricoesEstaduais != null)
            {
                foreach(var ie in resposta.InscricoesEstaduais)
                {
                    lista.Add(new InscricaoEstadualInfo
                    {
                        InscricaoEstadual = Normalizar(ie.InscricaoEstadual),
                        Ativo = ie.Ativo,
                        Uf = ie.Estado != null ? ie.Estado.Sigla : null,
                        CodigoEstadoIbge = ie.Estado != null ? ie.Estado.IbgeId : 0
                    });
                }
            }

            return lista;
        }

        private async Task<T> GetAsync<T>(int pacote, string documento, CancellationToken cancellationToken)
        {
            if(string.IsNullOrWhiteSpace(documento))
            {
                throw new PessoaLookupException("Documento sem conteúdo para consulta.");
            }

            var url = BaseUrl + Uri.EscapeDataString(_token) + "/" + pacote + "/" + Uri.EscapeDataString(documento);

            string corpo;
            int statusHttp;

            try
            {
                using(var response = await _httpClient.GetAsync(url, cancellationToken).ConfigureAwait(false))
                {
                    statusHttp = (int)response.StatusCode;
                    corpo = response.Content == null
                        ? null
                        : await response.Content.ReadAsStringAsync().ConfigureAwait(false);
                }
            }
            catch(OperationCanceledException) when(cancellationToken.IsCancellationRequested)
            {
                throw;
            }
            catch(Exception ex)
            {
                throw new PessoaLookupException("Falha ao consultar a API cpfcnpj.com.br (pacote " + pacote + ").", ex);
            }

            if(!PareceJson(corpo))
            {
                throw new PessoaLookupException("A API cpfcnpj.com.br respondeu HTTP " + statusHttp + " sem conteúdo JSON (pacote " + pacote + ").");
            }

            try
            {
                return JsonConvert.DeserializeObject<T>(corpo);
            }
            catch(JsonException ex)
            {
                throw new PessoaLookupException("Não foi possível interpretar a resposta da API cpfcnpj.com.br (HTTP " + statusHttp + ", pacote " + pacote + ").", ex);
            }
        }

        private static bool PareceJson(string corpo)
        {
            if(string.IsNullOrWhiteSpace(corpo))
            {
                return false;
            }

            var primeiro = corpo.TrimStart()[0];
            return primeiro == '{' || primeiro == '[';
        }

        private static void GarantirSucesso(RespostaBase resposta)
        {
            if(resposta == null)
            {
                throw new PessoaLookupException("A API cpfcnpj.com.br devolveu uma resposta vazia.");
            }

            if(resposta.Status == 1)
            {
                return;
            }

            var mensagem = !string.IsNullOrWhiteSpace(resposta.Erro)
                ? resposta.Erro
                : !string.IsNullOrWhiteSpace(resposta.Message)
                    ? resposta.Message
                    : "A API cpfcnpj.com.br retornou status " + resposta.Status + ".";

            var codigo = !string.IsNullOrWhiteSpace(resposta.ErroCodigo)
                ? resposta.ErroCodigo
                : resposta.Code;

            throw new PessoaLookupException(mensagem, codigo);
        }

        private static EnderecoPessoa MapearEnderecoCpf(string logradouro, string numero, string complemento,
            string bairro, string cep, string cidade, string uf, string ibge)
        {
            var codigoIbge = 0;
            if(!string.IsNullOrWhiteSpace(ibge) && int.TryParse(SomenteDigitos(ibge), out var parsed))
            {
                codigoIbge = parsed;
            }

            return new EnderecoPessoa
            {
                Logradouro = logradouro,
                Numero = numero,
                Complemento = complemento,
                Bairro = bairro,
                Cep = SomenteDigitos(cep),
                Municipio = cidade,
                Uf = uf,
                CodigoMunicipioIbge = codigoIbge,
                Pais = "Brasil"
            };
        }

        /// <summary>
        /// Junta o tipo do logradouro (ex.: <c>Rua</c>) ao nome quando ele ainda não faz parte do texto,
        /// para que <c>xLgr</c> saia completo (ex.: <c>Rua A</c>).
        /// </summary>
        private static string ComporLogradouro(string tipo, string logradouro)
        {
            if(string.IsNullOrWhiteSpace(logradouro))
            {
                return logradouro;
            }

            var nome = logradouro.Trim();

            if(string.IsNullOrWhiteSpace(tipo))
            {
                return nome;
            }

            var prefixo = tipo.Trim();

            return nome.StartsWith(prefixo + " ", StringComparison.OrdinalIgnoreCase)
                ? nome
                : prefixo + " " + nome;
        }

        private static bool InterpretarSimNao(string valor)
        {
            if(string.IsNullOrWhiteSpace(valor))
            {
                return false;
            }

            var t = valor.Trim();
            return t.StartsWith("S", StringComparison.OrdinalIgnoreCase) || t == "1";
        }

        private static string Normalizar(string documento)
        {
            if(string.IsNullOrWhiteSpace(documento))
            {
                return "";
            }

            var sb = new StringBuilder(documento.Length);
            foreach(var c in documento)
            {
                if(char.IsLetterOrDigit(c))
                {
                    sb.Append(char.ToUpperInvariant(c));
                }
            }

            return sb.ToString();
        }

        private static string SomenteDigitos(string valor)
        {
            if(string.IsNullOrWhiteSpace(valor))
            {
                return "";
            }

            var sb = new StringBuilder(valor.Length);
            foreach(var c in valor)
            {
                if(char.IsDigit(c))
                {
                    sb.Append(c);
                }
            }

            return sb.ToString();
        }

        #region DTOs internos de desserialização (Newtonsoft.Json)

        /// <summary>
        /// Lê <c>status</c> como inteiro; qualquer texto não numérico (ex.: <c>"error"</c>) vira 0.
        /// </summary>
        private sealed class StatusJsonConverter : JsonConverter
        {
            public override bool CanConvert(Type objectType) => objectType == typeof(int);

            public override object ReadJson(JsonReader reader, Type objectType, object existingValue, JsonSerializer serializer)
            {
                switch(reader.TokenType)
                {
                    case JsonToken.Integer:
                        return Convert.ToInt32(reader.Value, CultureInfo.InvariantCulture);
                    case JsonToken.Float:
                        return (int)Convert.ToDouble(reader.Value, CultureInfo.InvariantCulture);
                    case JsonToken.String:
                        return int.TryParse((string)reader.Value, NumberStyles.Integer, CultureInfo.InvariantCulture, out var valor) ? valor : 0;
                    case JsonToken.Boolean:
                        return (bool)reader.Value ? 1 : 0;
                    default:
                        return 0;
                }
            }

            public override void WriteJson(JsonWriter writer, object value, JsonSerializer serializer) =>
                writer.WriteValue(Convert.ToInt32(value, CultureInfo.InvariantCulture));
        }

        /// <summary>
        /// Envelope comum a todas as respostas. A API devolve <c>status</c> numérico (1 sucesso, 0 falha)
        /// com <c>erro</c>/<c>erroCodigo</c>, mas parâmetros malformados (pacote inexistente, documento
        /// com máscara) chegam com <c>status: "error"</c>, <c>code</c> e <c>message</c>.
        /// </summary>
        private class RespostaBase
        {
            [JsonProperty("status")]
            [JsonConverter(typeof(StatusJsonConverter))]
            public int Status { get; set; }

            [JsonProperty("erro")]
            public string Erro { get; set; }

            [JsonProperty("erroCodigo")]
            public string ErroCodigo { get; set; }

            [JsonProperty("message")]
            public string Message { get; set; }

            [JsonProperty("code")]
            public string Code { get; set; }
        }

        private class CpfResposta : RespostaBase
        {
            [JsonProperty("nome")]
            public string Nome { get; set; }

            [JsonProperty("endereco")]
            public string Endereco { get; set; }

            [JsonProperty("numero")]
            public string Numero { get; set; }

            [JsonProperty("complemento")]
            public string Complemento { get; set; }

            [JsonProperty("bairro")]
            public string Bairro { get; set; }

            [JsonProperty("cep")]
            public string Cep { get; set; }

            [JsonProperty("cidade")]
            public string Cidade { get; set; }

            [JsonProperty("uf")]
            public string Uf { get; set; }

            [JsonProperty("ibge")]
            public string Ibge { get; set; }

            [JsonProperty("enderecos")]
            public List<CpfEndereco> Enderecos { get; set; }
        }

        private class CpfEndereco
        {
            [JsonProperty("endereco")]
            public string Endereco { get; set; }

            [JsonProperty("numero")]
            public string Numero { get; set; }

            [JsonProperty("complemento")]
            public string Complemento { get; set; }

            [JsonProperty("bairro")]
            public string Bairro { get; set; }

            [JsonProperty("cep")]
            public string Cep { get; set; }

            [JsonProperty("cidade")]
            public string Cidade { get; set; }

            [JsonProperty("uf")]
            public string Uf { get; set; }

            [JsonProperty("ibge")]
            public string Ibge { get; set; }
        }

        private class CnpjResposta : RespostaBase
        {
            [JsonProperty("razao")]
            public string Razao { get; set; }

            [JsonProperty("fantasia")]
            public string Fantasia { get; set; }

            [JsonProperty("matrizEndereco")]
            public CnpjEndereco MatrizEndereco { get; set; }

            [JsonProperty("ibge")]
            public CnpjIbge Ibge { get; set; }

            [JsonProperty("simplesNacional")]
            public CnpjSimples SimplesNacional { get; set; }

            [JsonProperty("situacao")]
            public CnpjSituacao Situacao { get; set; }
        }

        private class CnpjEndereco
        {
            [JsonProperty("cep")]
            public string Cep { get; set; }

            [JsonProperty("tipo")]
            public string Tipo { get; set; }

            [JsonProperty("logradouro")]
            public string Logradouro { get; set; }

            [JsonProperty("numero")]
            public string Numero { get; set; }

            [JsonProperty("complemento")]
            public string Complemento { get; set; }

            [JsonProperty("bairro")]
            public string Bairro { get; set; }

            [JsonProperty("cidade")]
            public string Cidade { get; set; }

            [JsonProperty("uf")]
            public string Uf { get; set; }
        }

        private class CnpjIbge
        {
            [JsonProperty("estado")]
            public IbgeEstado Estado { get; set; }

            [JsonProperty("cidade")]
            public IbgeCidade Cidade { get; set; }
        }

        private class IbgeEstado
        {
            [JsonProperty("sigla")]
            public string Sigla { get; set; }

            [JsonProperty("ibge_id")]
            public int IbgeId { get; set; }
        }

        private class IbgeCidade
        {
            [JsonProperty("ibge_id")]
            public int IbgeId { get; set; }
        }

        private class CnpjSimples
        {
            [JsonProperty("optante")]
            public string Optante { get; set; }

            [JsonProperty("mei")]
            public string Mei { get; set; }
        }

        private class CnpjSituacao
        {
            [JsonProperty("nome")]
            public string Nome { get; set; }
        }

        private class IeResposta : RespostaBase
        {
            [JsonProperty("inscricoesEstaduais")]
            public List<IeItem> InscricoesEstaduais { get; set; }
        }

        private class IeItem
        {
            [JsonProperty("inscricao_estadual")]
            public string InscricaoEstadual { get; set; }

            [JsonProperty("ativo")]
            public bool Ativo { get; set; }

            [JsonProperty("estado")]
            public IbgeEstado Estado { get; set; }
        }

        #endregion
    }
}
