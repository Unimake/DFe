using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Xml.AACBS;

namespace Unimake.Business.DFe.Servicos.AACBS
{
    // O caminho vem exclusivamente dos nomes constantes do contrato, nunca das chaves do remetente.
    internal sealed class CampoInvalidoException : FormatException
    {
        internal string Caminho { get; }
        internal CampoInvalidoException(string caminho) => Caminho = caminho;
    }

    internal static partial class RespostaAACBS
    {
        private static readonly HashSet<int> OrigensDebito = new HashSet<int> { 0, 20, 21, 22, 30, 31, 32, 50, 51, 52, 53, 54, 55 };
        private static readonly HashSet<int> DocumentosDebito = new HashSet<int> { 55, 57, 62, 63, 64, 65, 66, 67, 91, 92, 93, 94, 97 };
        private static readonly Regex PeriodoApuracao = new Regex(@"\A(0[1-9]|1[0-2])/[0-9]{4}\z", RegexOptions.CultureInvariant);

        private sealed class Objeto
        {
            private readonly JsonElement json;
            private readonly string caminho;

            internal Objeto(JsonElement json, string caminho)
            {
                if (json.ValueKind != JsonValueKind.Object) throw new CampoInvalidoException(caminho);
                this.json = json;
                this.caminho = caminho;
            }

            internal void Aceitar(params string[] nomes)
            {
                var recebidos = new HashSet<string>(StringComparer.Ordinal);
                foreach (var campo in json.EnumerateObject())
                {
                    var nome = campo.Name;
                    if (!nomes.Contains(nome, StringComparer.Ordinal)) throw new CampoInvalidoException(caminho);
                    if (!recebidos.Add(nome)) throw Invalido(nome);
                }
            }

            internal bool Tem(string nome) => json.TryGetProperty(nome, out _);
            internal CampoInvalidoException Invalido(string nome) => new CampoInvalidoException(caminho + "." + nome);

            private JsonElement Campo(string nome, JsonValueKind tipo)
            {
                if (!json.TryGetProperty(nome, out var campo) || campo.ValueKind != tipo) throw Invalido(nome);
                return campo;
            }

            internal string Texto(string nome)
            {
                var campo = Campo(nome, JsonValueKind.String);
                try { return XmlConvert.VerifyXmlChars(campo.GetString()); }
                catch (XmlException) { throw Invalido(nome); }
                catch (InvalidOperationException) { throw Invalido(nome); }
            }

            internal string TextoOpcional(string nome) => Tem(nome) ? Texto(nome) : null;

            internal string Instante(string nome)
            {
                var texto = Texto(nome);
                try { AACBSFormat.ParseInstant(texto); }
                catch (FormatException) { throw Invalido(nome); }
                return texto;
            }

            internal int Inteiro(string nome)
            {
                if (!Campo(nome, JsonValueKind.Number).TryGetInt32(out var inteiro)) throw Invalido(nome);
                return inteiro;
            }

            internal int InteiroDoSchema(string nome)
            {
                // O JSON Schema define integer pelo valor matemático, inclusive 55.0 e 5.5e1.
                var valor = NumeroExato(nome, 10, 0);
                if (valor < int.MinValue || valor > int.MaxValue) throw Invalido(nome);
                return (int)valor;
            }

            internal Objeto Grupo(string nome) => new Objeto(Campo(nome, JsonValueKind.Object), caminho + "." + nome);

            internal List<T> Lista<T>(string nome, Func<Objeto, T> ler)
            {
                var itens = new List<T>();
                foreach (var item in Campo(nome, JsonValueKind.Array).EnumerateArray())
                    itens.Add(ler(new Objeto(item, caminho + "." + nome + "[]")));
                return itens;
            }

            internal decimal Valor(string nome) => NumeroExato(nome, 16, 2);

            private decimal NumeroExato(string nome, int maxInteiras, int maxDecimais)
            {
                var numero = Campo(nome, JsonValueKind.Number).GetRawText();
                // O parser JSON já validou a sintaxe. Trabalhar com os dígitos originais evita
                // que uma aproximação em decimal/double aceite milésimos próximos de um centavo.
                var partes = numero.TrimStart('-').Split('e', 'E');
                var mantissa = partes[0];
                var ponto = mantissa.IndexOf('.');
                var fracao = ponto < 0 ? 0 : mantissa.Length - ponto - 1;
                var digitos = mantissa.Replace(".", "").TrimStart('0');
                if (digitos.Length == 0) return 0;
                var significativos = digitos.TrimEnd('0');
                var expoente = 0;
                if (partes.Length > 1 && !int.TryParse(partes[1], NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture, out expoente))
                    throw Invalido(nome);
                var escala = (long)fracao - expoente - (digitos.Length - significativos.Length);
                if (escala > maxDecimais || significativos.Length - escala > maxInteiras) throw Invalido(nome);

                var valor = decimal.Parse(significativos, CultureInfo.InvariantCulture);
                while (escala < 0) { valor *= 10; escala++; }
                while (escala > 0) { valor /= 10; escala--; }
                return numero[0] == '-' ? -valor : valor;
            }

            internal decimal? ValorOpcional(string nome) => Tem(nome) ? Valor(nome) : (decimal?)null;
        }

        private static bool FalhaHttp(HttpStatusCode status) => (int)status >= 400 && (int)status <= 599;

        // RFC 6750 §2.1 (b64token). Não interpretar o conteúdo ou inferir identidade/permissão.
        internal static bool TokenValido(string token)
        {
            if (string.IsNullOrEmpty(token)) return false;
            var padding = false;
            foreach (var c in token)
            {
                if (c == '=') { padding = true; continue; }
                if (padding || !(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' ||
                    c >= '0' && c <= '9' || "-._~+/".IndexOf(c) >= 0)) return false;
            }
            return token[0] != '=';
        }

        internal static RetAutenticar Autenticacao(JsonElement json, HttpStatusCode status)
        {
            var obj = new Objeto(json, "$");
            if (obj.Tem("error"))
            {
                if (!FalhaHttp(status)) throw obj.Invalido("error");
                obj.Aceitar("error", "error_description", "error_uri");
                return new RetAutenticar { Error = obj.Texto("error"), ErrorDescription = obj.TextoOpcional("error_description"),
                    ErrorUri = obj.TextoOpcional("error_uri") };
            }
            if (status != HttpStatusCode.OK || obj.Tem("error_description") || obj.Tem("error_uri"))
                throw new CampoInvalidoException("$");
            // RFC 6749 §5.1 exige ignorar extensões desconhecidas no sucesso.
            var token = obj.Texto("access_token");
            if (!TokenValido(token)) throw obj.Invalido("access_token");
            var tipo = obj.Texto("token_type");
            if (!string.Equals(tipo, "Bearer", StringComparison.OrdinalIgnoreCase)) throw obj.Invalido("token_type");
            var prazo = obj.Tem("expires_in") ? obj.Inteiro("expires_in") : (int?)null;
            if (prazo <= 0) throw obj.Invalido("expires_in");
            var escopo = obj.TextoOpcional("scope");
            if (escopo != null && escopo.Length == 0) throw obj.Invalido("scope");
            return new RetAutenticar { AccessToken = token, TokenType = tipo, ExpiresIn = prazo, Scope = escopo };
        }

        internal static RetConsultarDebitos Abertura(JsonElement json, HttpStatusCode status)
        {
            var obj = new Objeto(json, "$");
            if (status == HttpStatusCode.Created)
            {
                obj.Aceitar("tiqueteSolicitacao", "tEASegundos");
                return new RetConsultarDebitos { TiqueteSolicitacao = obj.Texto("tiqueteSolicitacao"), TEASegundos = obj.Inteiro("tEASegundos") };
            }
            if (!FalhaHttp(status)) throw new CampoInvalidoException("$");
            obj.Aceitar("codigoErro", "mensagemErro");
            return new RetConsultarDebitos { CodigoErro = obj.Texto("codigoErro"), MensagemErro = obj.Texto("mensagemErro") };
        }

        internal static RetConsultarSituacaoSolicitacao Situacao(JsonElement json, HttpStatusCode status)
        {
            var obj = new Objeto(json, "$");
            var resultado = new RetConsultarSituacaoSolicitacao();
            switch (obj.Texto("situacao"))
            {
                case "PENDENTE": resultado.Situacao = SituacaoSolicitacaoAACBS.Pendente; break;
                case "EM_PROCESSAMENTO": resultado.Situacao = SituacaoSolicitacaoAACBS.EmProcessamento; break;
                case "CONCLUIDA": resultado.Situacao = SituacaoSolicitacaoAACBS.Concluida; break;
                case "ERRO": resultado.Situacao = SituacaoSolicitacaoAACBS.Erro; break;
                case "EXPIRADA": resultado.Situacao = SituacaoSolicitacaoAACBS.Expirada; break;
                default: throw obj.Invalido("situacao");
            }
            if (resultado.Situacao == SituacaoSolicitacaoAACBS.Erro || resultado.Situacao == SituacaoSolicitacaoAACBS.Expirada)
            {
                if (status != HttpStatusCode.OK && !FalhaHttp(status)) throw obj.Invalido("situacao");
                obj.Aceitar("situacao", "codigoErro", "mensagemErro");
                resultado.CodigoErro = obj.Texto("codigoErro");
                resultado.MensagemErro = obj.Texto("mensagemErro");
            }
            else
            {
                if (status != HttpStatusCode.OK) throw obj.Invalido("situacao");
                obj.Aceitar("situacao");
            }
            return resultado;
        }

        internal static RetConsultarLinkDownload Link(JsonElement json, HttpStatusCode status)
        {
            var obj = new Objeto(json, "$");
            if (status == HttpStatusCode.OK)
            {
                obj.Aceitar("urlAssinada", "urlAssinadaExpiraEm");
                var texto = obj.Texto("urlAssinada");
                if (!Uri.TryCreate(texto, UriKind.Absolute, out var uri) || !uri.IsWellFormedOriginalString() ||
                    uri.Scheme != Uri.UriSchemeHttps || uri.UserInfo.Length != 0 || uri.Fragment.Length != 0)
                    throw obj.Invalido("urlAssinada");
                return new RetConsultarLinkDownload { UrlAssinada = texto,
                    UrlAssinadaExpiraEmField = obj.Tem("urlAssinadaExpiraEm") ? obj.Instante("urlAssinadaExpiraEm") : null };
            }
            if (!FalhaHttp(status)) throw new CampoInvalidoException("$");
            obj.Aceitar("codigoErro", "mensagemErro");
            return new RetConsultarLinkDownload { CodigoErro = obj.Texto("codigoErro"), MensagemErro = obj.Texto("mensagemErro") };
        }

        internal static RetBaixarArquivoRetorno Arquivo(JsonElement json, HttpStatusCode status, Xml.AACBS.BaixarArquivoRetorno envio)
        {
            switch (envio.TipoConsulta)
            {
                case TipoConsultaAACBS.Creditos: return ArquivoCreditos(json, status, envio);
                case TipoConsultaAACBS.Pagamentos: return ArquivoPagamentos(json, status, envio);
                case TipoConsultaAACBS.Recolhimentos: return ArquivoRecolhimentos(json, status, envio);
                case TipoConsultaAACBS.Debitos: break;
                default: throw new CampoInvalidoException("$");
            }
            if (status != HttpStatusCode.OK) throw new CampoInvalidoException("$");
            var obj = new Objeto(json, "$");
            obj.Aceitar("tiqueteSolicitacao", "ni", "niConsumidor", "geradoEm", "apuracao");
            var tiquete = obj.Texto("tiqueteSolicitacao");
            var ni = obj.Texto("ni");
            if (tiquete != envio.TiqueteSolicitacao) throw obj.Invalido("tiqueteSolicitacao");
            // O arquivo de produção também pode identificar o contribuinte pelo CNPJ completo.
            if ((ni.Length != 8 && ni.Length != 14) || ni.Any(c => c < '0' || c > '9') ||
                !ni.StartsWith(envio.Cnpj, StringComparison.Ordinal))
                throw obj.Invalido("ni");
            var consumidor = obj.Texto("niConsumidor");
            if ((consumidor.Length != 11 && consumidor.Length != 14) || consumidor.Any(c => c < '0' || c > '9'))
                throw obj.Invalido("niConsumidor");
            var arquivo = new RetArquivoDebitos { TiqueteSolicitacao = tiquete, Ni = ni, NiConsumidor = consumidor,
                GeradoEmField = obj.Instante("geradoEm"), Apuracao = obj.Lista("apuracao", Periodo) };
            return new RetBaixarArquivoRetorno { ArquivoDebitos = arquivo };
        }

        private static ApuracaoDebitos Periodo(Objeto periodo)
        {
            periodo.Aceitar("pa", "debitos");
            var pa = periodo.Texto("pa");
            if (!PeriodoApuracao.IsMatch(pa)) throw periodo.Invalido("pa");
            var debitos = periodo.Lista("debitos", Debito);
            if (debitos.Count == 0) throw periodo.Invalido("debitos");
            return new ApuracaoDebitos { Pa = pa, Debitos = debitos };
        }

        private static DebitoArquivo Debito(Objeto debito)
        {
            debito.Aceitar("origem", "documento", "chave", "emissao", "registro", "atualizacao", "cbs");
            var origem = debito.InteiroDoSchema("origem");
            var documento = debito.InteiroDoSchema("documento");
            var chave = debito.Texto("chave");
            if (!OrigensDebito.Contains(origem)) throw debito.Invalido("origem");
            if (!DocumentosDebito.Contains(documento)) throw debito.Invalido("documento");
            // Texto já rejeitou substitutos inválidos. Cada par UTF-16 representa um ponto de código.
            if (chave.Length == 0 || chave.Count(c => !char.IsLowSurrogate(c)) > 100) throw debito.Invalido("chave");
            var cbs = debito.Grupo("cbs");
            cbs.Aceitar("excedente", "apurado", "inexigivel", "suspenso", "pgtoAntecipado", "extinto", "saldoDevedor");
            return new DebitoArquivo
            {
                Origem = origem, Documento = documento, Chave = chave,
                EmissaoField = debito.Instante("emissao"), RegistroField = debito.Instante("registro"),
                AtualizacaoField = debito.Instante("atualizacao"),
                Cbs = new CbsDebito
                {
                    Excedente = cbs.ValorOpcional("excedente"), Apurado = cbs.Valor("apurado"),
                    Inexigivel = cbs.ValorOpcional("inexigivel"), Suspenso = cbs.ValorOpcional("suspenso"),
                    PgtoAntecipado = cbs.ValorOpcional("pgtoAntecipado"), Extinto = cbs.ValorOpcional("extinto"),
                    SaldoDevedor = cbs.Valor("saldoDevedor")
                }
            };
        }
    }
}
