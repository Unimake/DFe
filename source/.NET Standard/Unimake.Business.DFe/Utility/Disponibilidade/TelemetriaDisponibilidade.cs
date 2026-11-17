using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class RelogioDisponibilidade
    {
        /// <summary>Função que fornece a data e hora usadas pelo diagnóstico.</summary>
        /// <remarks>É substituível nos testes para simular a passagem do tempo sem esperar de verdade.</remarks>
        internal static Func<DateTime> Agora = () => DateTime.Now;
    }

    /// <summary>Converte respostas e exceções de transporte em classificações seguras.</summary>
    internal static class ClassificadorDisponibilidade
    {
        /// <summary>Classifica um retorno fiscal usando cStat, HTTP e motivo.</summary>
        /// <param name="resultado">Amostra que será preenchida.</param>
        internal static void ClassificarRespostaFiscal(ResultadoSondaDisponibilidade resultado)
        {
            resultado.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
            if (EhIndisponibilidadeFiscal(resultado.CStat))
            {
                resultado.Status = StatusDisponibilidade.Indisponivel;
                resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                resultado.XMotivo = resultado.CStat == 999
                    ? "A autoridade fiscal retornou erro não catalogado, indicando provável instabilidade."
                    : "A autoridade fiscal informou indisponibilidade do serviço.";
            }
            else if (resultado.CStat == 656 || resultado.CStat == 678)
            {
                resultado.Status = StatusDisponibilidade.Degradado;
                resultado.TipoFalha = TipoFalhaDisponibilidade.ConsumoIndevido;
                resultado.XMotivo = "A autoridade fiscal informou consumo indevido.";
            }
            else if (resultado.CStat > 0)
            {
                resultado.Status = StatusDisponibilidade.Operacional;
                resultado.XMotivo = "A mensagem foi processada pela aplicação fiscal.";
            }
            else if (resultado.HttpStatusCode >= 200 && resultado.HttpStatusCode < 300)
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                resultado.XMotivo = "O endpoint respondeu, mas não foi localizado um status fiscal.";
            }
            else
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
            }
        }

        /// <summary>Verifica se o código fiscal representa indisponibilidade atribuída à autoridade fiscal.</summary>
        /// <param name="cStat">Código de status retornado pelo serviço fiscal.</param>
        /// <returns><see langword="true"/> para indisponibilidade declarada ou provável instabilidade fiscal.</returns>
        internal static bool EhIndisponibilidadeFiscal(int cStat) =>
            cStat == 108 || cStat == 109 || cStat == 999;

        /// <summary>Classifica uma exceção de transporte sem expor dados sensíveis.</summary>
        /// <param name="exception">Exceção original da operação.</param>
        /// <param name="resultado">Amostra que receberá o tipo de falha e mensagem sanitizada.</param>
        /// <param name="incluirMensagem">Se permite ler/sanitizar a mensagem externa; desabilitar para observações sem texto.</param>
        internal static void PreencherFalha(Exception exception, ResultadoSondaDisponibilidade resultado, bool incluirMensagem = true)
        {
            PreencherFalhaInterna(exception, resultado, incluirMensagem);
            ClassificarHttp(resultado, false);
        }

        private static void PreencherFalhaInterna(Exception exception, ResultadoSondaDisponibilidade resultado, bool incluirMensagem)
        {
            resultado.Status = StatusDisponibilidade.Inconclusivo;
            resultado.TipoFalha = TipoFalhaDisponibilidade.Desconhecida;
            resultado.Excecao = incluirMensagem ? SanitizarExcecao(exception) : string.Empty;
            if (EncontrarExcecao<Unimake.Exceptions.CertificadoDigitalException>(exception) != null)
            {
                resultado.TipoFalha = TipoFalhaDisponibilidade.Certificado;
                return;
            }
            if (EncontrarExcecao<System.Security.Authentication.AuthenticationException>(exception) != null)
            {
                resultado.TipoFalha = TipoFalhaDisponibilidade.TLS;
                return;
            }
            var webException = EncontrarWebException(exception);
            var permiteClassificarSocket = webException == null ||
                webException.Status == WebExceptionStatus.UnknownError ||
                webException.Status == WebExceptionStatus.ConnectFailure ||
                webException.Status == WebExceptionStatus.Timeout ||
                webException.Status == WebExceptionStatus.NameResolutionFailure;
            var falhaSocket = permiteClassificarSocket
                ? ClassificarSocketException(exception)
                : TipoFalhaDisponibilidade.Desconhecida;
            if (falhaSocket != TipoFalhaDisponibilidade.Desconhecida)
            {
                resultado.TipoFalha = falhaSocket;
                if (falhaSocket == TipoFalhaDisponibilidade.Timeout ||
                    falhaSocket == TipoFalhaDisponibilidade.ConexaoRecusada)
                {
                    resultado.Status = StatusDisponibilidade.Degradado;
                }
                return;
            }
            if (webException == null)
            {
                // HttpClient em .NET 5+ encapsula TimeoutException em OperationCanceledException.
                // Cancelamento sem essa evidência é ambíguo em runtimes antigos; não inferir timeout.
                if (EncontrarExcecao<TimeoutException>(exception) != null)
                {
                    resultado.TipoFalha = TipoFalhaDisponibilidade.Timeout;
                    resultado.Status = StatusDisponibilidade.Degradado;
                }
                else if (EncontrarExcecao<OperationCanceledException>(exception) == null)
                {
                    resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                }
                return;
            }

            var resposta = webException.Response as HttpWebResponse;
            if (resposta != null)
            {
                resultado.HttpStatusCode = (int)resposta.StatusCode;
            }

            var falha = Unimake.Net.Utility.ClassifyWebExceptionStatus(webException.Status);
            switch (falha)
            {
                case Unimake.Net.HttpConnectionFailureType.Dns: resultado.TipoFalha = TipoFalhaDisponibilidade.DNS; break;
                case Unimake.Net.HttpConnectionFailureType.Connection: resultado.TipoFalha = TipoFalhaDisponibilidade.Conexao; break;
                case Unimake.Net.HttpConnectionFailureType.Timeout: resultado.TipoFalha = TipoFalhaDisponibilidade.Timeout; resultado.Status = StatusDisponibilidade.Degradado; break;
                case Unimake.Net.HttpConnectionFailureType.Tls: resultado.TipoFalha = TipoFalhaDisponibilidade.TLS; break;
                case Unimake.Net.HttpConnectionFailureType.Proxy: resultado.TipoFalha = TipoFalhaDisponibilidade.Proxy; break;
                case Unimake.Net.HttpConnectionFailureType.Http:
                    resultado.TipoFalha = TipoFalhaDisponibilidade.HTTP;
                    if (resultado.HttpStatusCode == (int)HttpStatusCode.ServiceUnavailable)
                    {
                        resultado.Status = StatusDisponibilidade.Indisponivel;
                        resultado.XMotivo = "O endpoint fiscal informou indisponibilidade temporária do serviço (HTTP 503).";
                    }
                    else
                    {
                        resultado.Status = resultado.HttpStatusCode >= 500
                            ? StatusDisponibilidade.Degradado
                            : StatusDisponibilidade.Inconclusivo;
                    }
                    break;
            }
        }

        // Não ler mensagens localizadas para inferir autenticação, proxy ou rate limit.
        internal static void ClassificarHttp(ResultadoSondaDisponibilidade resultado, bool respostaReconhecida,
            TipoDFe tipoDFe = TipoDFe.Desconhecido)
        {
            var http = resultado.HttpStatusCode;
            if (http < 400) return;
            // Uma falha objetiva de TLS/certificado não pode ser substituída por HTTP anterior.
            if (resultado.TipoFalha == TipoFalhaDisponibilidade.TLS ||
                resultado.TipoFalha == TipoFalhaDisponibilidade.Certificado) return;
            if (tipoDFe == TipoDFe.EFDReinf && (http == 495 || http == 496))
            {
                resultado.TipoFalha = TipoFalhaDisponibilidade.Certificado;
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.XMotivo = "O serviço recusou o certificado de acesso.";
            }
            else if (http == 401 || http == 403 || http == 407 || http == 429)
            {
                resultado.TipoFalha = http == 407 ? TipoFalhaDisponibilidade.Proxy :
                    http == 429 ? TipoFalhaDisponibilidade.ConsumoIndevido : TipoFalhaDisponibilidade.Autenticacao;
                resultado.Status = http == 429 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Inconclusivo;
                resultado.XMotivo = http == 429 ? "O endpoint limitou temporariamente as consultas." :
                    http == 407 ? "O proxy recusou a autenticação." : "O serviço recusou a autenticação ou a autorização de acesso.";
            }
            else if (!EhIndisponibilidadeFiscal(resultado.CStat) &&
                ((!respostaReconhecida && resultado.CStat == 0) || http >= 500))
            {
                resultado.TipoFalha = TipoFalhaDisponibilidade.HTTP;
                resultado.Status = http == 503 ? StatusDisponibilidade.Indisponivel :
                    http >= 500 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Inconclusivo;
                resultado.XMotivo = http == 503 ? "O endpoint informou indisponibilidade temporária (HTTP 503)." :
                    "O endpoint respondeu com falha HTTP; a causa não foi confirmada.";
            }
        }

        /// <summary>Classifica uma falha de socket encontrada dentro da cadeia de exceções.</summary>
        /// <param name="exception">Exceção externa gerada pelo transporte.</param>
        /// <returns>Categoria de rede correspondente ou <see cref="TipoFalhaDisponibilidade.Desconhecida"/>.</returns>
        internal static TipoFalhaDisponibilidade ClassificarSocketException(Exception exception)
        {
            var socketException = EncontrarSocketException(exception);
            if (socketException == null)
            {
                return TipoFalhaDisponibilidade.Desconhecida;
            }

            switch (socketException.SocketErrorCode)
            {
                case SocketError.ConnectionRefused:
                    return TipoFalhaDisponibilidade.ConexaoRecusada;
                case SocketError.TimedOut:
                    return TipoFalhaDisponibilidade.Timeout;
                case SocketError.HostNotFound:
                case SocketError.NoData:
                case SocketError.TryAgain:
                    return TipoFalhaDisponibilidade.DNS;
                default:
                    return TipoFalhaDisponibilidade.Conexao;
            }
        }

        /// <summary>Cria uma cópia independente de uma amostra para cache ou retorno.</summary>
        /// <param name="origem">Amostra original.</param>
        /// <returns>Cópia com os mesmos dados públicos.</returns>
        internal static ResultadoSondaDisponibilidade Clonar(ResultadoSondaDisponibilidade origem) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = origem.Servico,
                Endpoint = origem.Endpoint,
                IdentidadeEndpoint = origem.IdentidadeEndpoint,
                Protocolo = origem.Protocolo,
                Fonte = origem.Fonte,
                DataHora = origem.DataHora,
                IdadeSegundos = origem.IdadeSegundos,
                Status = origem.Status,
                TipoFalha = origem.TipoFalha,
                DuracaoMilissegundos = origem.DuracaoMilissegundos,
                HttpStatusCode = origem.HttpStatusCode,
                CStat = origem.CStat,
                XMotivo = origem.XMotivo,
                Excecao = origem.Excecao,
                DoCache = origem.DoCache,
                Essencial = origem.Essencial
            };

        /// <summary>Remove usuário, senha, caminho, consulta e fragmento de um endpoint HTTP.</summary>
        /// <param name="endpoint">Endereço potencialmente sensível.</param>
        /// <returns>Somente esquema, host e porta.</returns>
        internal static string SanitizarEndpoint(string endpoint)
        {
            if (string.IsNullOrWhiteSpace(endpoint)) return string.Empty;
            Uri uri;
            if (!Uri.TryCreate(endpoint, UriKind.Absolute, out uri) ||
                (uri.Scheme != Uri.UriSchemeHttp && uri.Scheme != Uri.UriSchemeHttps))
            {
                return string.Empty;
            }
            return uri.GetComponents(UriComponents.SchemeAndServer, UriFormat.UriEscaped);
        }

        internal static string IdentificarEndpoint(string endpoint)
        {
            using (var sha = SHA256.Create())
            {
                return BitConverter.ToString(sha.ComputeHash(Encoding.UTF8.GetBytes(endpoint ?? string.Empty))).Replace("-", string.Empty);
            }
        }

        /// <summary>Sanitiza a mensagem de uma exceção de transporte.</summary>
        /// <param name="exception">Exceção que será resumida.</param>
        /// <returns>Mensagem curta sem URLs, credenciais ou identificadores conhecidos.</returns>
        internal static string SanitizarExcecao(Exception exception) =>
            exception == null ? string.Empty : "Falha técnica (" + exception.GetType().Name + ").";

        /// <summary>Sanitiza texto externo antes de colocá-lo no resultado do diagnóstico.</summary>
        /// <param name="mensagem">Texto vindo do transporte ou do provedor.</param>
        /// <returns>Texto limitado e sem dados sensíveis comuns.</returns>
        internal static string SanitizarMensagem(string mensagem)
        {
            mensagem = mensagem ?? string.Empty;
            var quebra = mensagem.IndexOfAny(new[] { '\r', '\n' });
            if (quebra >= 0) mensagem = mensagem.Substring(0, quebra);
            mensagem = Regex.Replace(mensagem, @"[a-z][a-z0-9+.-]*://[^\s\""']+", m => SanitizarEndpoint(m.Value),
                RegexOptions.IgnoreCase);
            mensagem = Regex.Replace(mensagem, @"(?<!\d)\d{2}\.?\d{3}\.?\d{3}/?\d{4}-?\d{2}(?!\d)", "***");
            mensagem = Regex.Replace(mensagem, @"(?<!\d)\d{3}\.?\d{3}\.?\d{3}-?\d{2}(?!\d)", "***");
            mensagem = Regex.Replace(mensagem, @"\d{6,}", "***");
            mensagem = Regex.Replace(mensagem,
                @"\b(password|senha|token|apikey|api_key|secret|usuario|user)\s*[:=]\s*[^,;\s]+",
                "$1=***", RegexOptions.IgnoreCase);
            mensagem = Regex.Replace(mensagem, @"(?:[a-z]:\\|\\\\)[^'\""\r\n]+", "***",
                RegexOptions.IgnoreCase);
            return mensagem.Length > 300 ? mensagem.Substring(0, 300) : mensagem;
        }

        /// <summary>Procura um tipo de exceção na cadeia e nos ramos de exceções agregadas.</summary>
        /// <typeparam name="T">Tipo de evidência procurada, sem interpretar mensagens localizadas.</typeparam>
        /// <param name="exception">Exceção inicial.</param>
        /// <returns>A evidência encontrada ou <see langword="null"/>.</returns>
        private static T EncontrarExcecao<T>(Exception exception) where T : Exception
        {
            for (var atual = exception; atual != null; atual = atual.InnerException)
            {
                var encontrada = atual as T;
                if (encontrada != null) return encontrada;

                var agregada = atual as AggregateException;
                if (agregada != null)
                {
                    foreach (var interna in agregada.InnerExceptions)
                    {
                        encontrada = EncontrarExcecao<T>(interna);
                        if (encontrada != null) return encontrada;
                    }
                    return null;
                }
            }
            return null;
        }

        /// <summary>Procura uma WebException dentro da cadeia de exceções internas.</summary>
        /// <param name="exception">Exceção inicial.</param>
        /// <returns>A WebException encontrada ou <see langword="null"/>.</returns>
        private static WebException EncontrarWebException(Exception exception) => EncontrarExcecao<WebException>(exception);

        /// <summary>Procura uma <see cref="SocketException"/> dentro da cadeia de exceções internas.</summary>
        /// <param name="exception">Exceção inicial.</param>
        /// <returns>A falha de socket encontrada ou <see langword="null"/>.</returns>
        private static SocketException EncontrarSocketException(Exception exception)
        {
            for (var atual = exception; atual != null; atual = atual.InnerException)
            {
                var socketException = atual as SocketException;
                if (socketException != null)
                {
                    return socketException;
                }

                var aggregateException = atual as AggregateException;
                if (aggregateException != null)
                {
                    foreach (var interna in aggregateException.Flatten().InnerExceptions)
                    {
                        socketException = EncontrarSocketException(interna);
                        if (socketException != null)
                        {
                            return socketException;
                        }
                    }
                }
            }
            return null;
        }
    }

    /// <summary>Combina as sondas em um estado geral e em uma origem provável.</summary>
    /// <remarks>Considera essencialidade, evidência fiscal direta e falhas locais antes de atribuir problema à SEFAZ.</remarks>
    internal static class AgregadorDisponibilidade
    {
        /// <summary>Estado consolidado de um serviço após combinar suas tentativas recentes.</summary>
        private sealed class EstadoServico
        {
            /// <summary>Indica se o serviço é necessário para emissão ou status.</summary>
            public bool Essencial;
            /// <summary>Indica que cStat 108, 109 ou 999 confirmou indisponibilidade fiscal.</summary>
            public bool IndisponibilidadeConfirmada;
            /// <summary>Estado mais representativo do serviço.</summary>
            public StatusDisponibilidade Status;
        }

        /// <summary>Calcula o estado agregado do diagnóstico recebido.</summary>
        /// <param name="resultado">Resultado cujas sondas serão analisadas e atualizado.</param>
        internal static void Agregar(ResultadoDiagnosticoDisponibilidade resultado)
        {
            var itens = resultado.Sondas.Itens.ToList();
            if (itens.Count == 0)
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Indeterminada;
                return;
            }

            if (itens.All(x => x.Status == StatusDisponibilidade.NaoAplicavel))
            {
                resultado.Status = StatusDisponibilidade.NaoAplicavel;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Indeterminada;
                return;
            }

            if (itens.Any(x => x.Essencial &&
                (x.TipoFalha == TipoFalhaDisponibilidade.Certificado ||
                 x.TipoFalha == TipoFalhaDisponibilidade.Configuracao)))
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.AmbienteLocal;
                return;
            }

            if (itens.Any(x => x.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido))
            {
                resultado.Status = StatusDisponibilidade.Degradado;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.ConsumoIndevido;
                return;
            }

            var falhaLocal = itens.Any(EhFalhaLocal);
            var infraestrutura = itens.Where(x => x.Fonte == FonteEvidenciaDisponibilidade.Infraestrutura &&
                x.Status != StatusDisponibilidade.NaoAplicavel).ToList();
            var infraestruturaSaudavel = infraestrutura.Count > 0 && !falhaLocal &&
                infraestrutura.All(x => x.Status == StatusDisponibilidade.Operacional);
            var fiscais = itens.Where(x => x.Fonte != FonteEvidenciaDisponibilidade.Infraestrutura).ToList();
            var infraestruturaCorroboraFalhaRemota = InfraestruturaCorroboraFalhaRemota(
                infraestrutura, fiscais, falhaLocal, infraestruturaSaudavel);
            var estadosServicos = fiscais
                .GroupBy(x => (x.Servico ?? string.Empty) + "|" + (x.IdentidadeEndpoint ?? x.Endpoint ?? string.Empty), StringComparer.OrdinalIgnoreCase)
                .Select(x => ClassificarServico(x, infraestruturaCorroboraFalhaRemota))
                .ToList();
            var indisponiveis = estadosServicos.Where(x => x.Status == StatusDisponibilidade.Indisponivel).ToList();
            var operacionais = estadosServicos.Where(x => x.Status == StatusDisponibilidade.Operacional ||
                x.Status == StatusDisponibilidade.Degradado).ToList();
            var essenciais = estadosServicos.Where(x => x.Essencial).ToList();
            var indisponibilidadeConfirmada = indisponiveis.Any(x => x.IndisponibilidadeConfirmada);

            if (falhaLocal && !indisponibilidadeConfirmada)
            {
                resultado.Status = operacionais.Count > 0
                    ? StatusDisponibilidade.Degradado
                    : StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.AmbienteLocal;
            }
            else if (essenciais.Count > 0 && essenciais.All(x => x.Status == StatusDisponibilidade.Indisponivel))
            {
                resultado.Status = StatusDisponibilidade.Indisponivel;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.AutoridadeFiscal;
            }
            else if (indisponiveis.Count > 0)
            {
                resultado.Status = StatusDisponibilidade.ParcialmenteIndisponivel;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Parcial;
            }
            else if (operacionais.Count > 0)
            {
                resultado.Status = estadosServicos.Any(x => x.Status == StatusDisponibilidade.Degradado)
                    ? StatusDisponibilidade.Degradado
                    : StatusDisponibilidade.Operacional;
                resultado.OrigemProvavel = resultado.Status == StatusDisponibilidade.Degradado &&
                    fiscais.Any(x => EhFalhaAcessoRemoto(x.TipoFalha))
                    ? OrigemProvavelIndisponibilidade.Indeterminada
                    : OrigemProvavelIndisponibilidade.Nenhuma;
            }
            else
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Indeterminada;
            }
        }

        /// <summary>Classifica um serviço usando suas até três amostras mais recentes.</summary>
        /// <param name="grupo">Amostras do mesmo serviço e endpoint.</param>
        /// <param name="infraestruturaCorroboraFalhaRemota">Indica se a infraestrutura permite correlacionar timeouts com o endpoint remoto.</param>
        /// <returns>Estado consolidado do serviço.</returns>
        private static EstadoServico ClassificarServico(IGrouping<string, ResultadoSondaDisponibilidade> grupo,
            bool infraestruturaCorroboraFalhaRemota)
        {
            var ultimas = grupo.OrderByDescending(x => x.DataHora).Take(3).ToList();
            var maisRecente = ultimas[0];
            var estado = new EstadoServico
            {
                Essencial = ultimas.Any(x => x.Essencial),
                IndisponibilidadeConfirmada = EhIndisponibilidadeConfirmada(maisRecente),
                Status = maisRecente.Status
            };
            if (maisRecente.Status == StatusDisponibilidade.Operacional ||
                maisRecente.Status == StatusDisponibilidade.Indisponivel ||
                maisRecente.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
            {
                return estado;
            }

            var falhasHttp = ultimas.Count(x => x.TipoFalha == TipoFalhaDisponibilidade.HTTP && x.HttpStatusCode >= 500);
            if (falhasHttp >= 2)
            {
                estado.Status = StatusDisponibilidade.Indisponivel;
                return estado;
            }

            if (EhFalhaAcessoRemoto(maisRecente.TipoFalha))
            {
                var falhasAcessoRemoto = ultimas.Count(x => EhFalhaAcessoRemoto(x.TipoFalha));
                if (falhasAcessoRemoto >= 2 && infraestruturaCorroboraFalhaRemota)
                {
                    estado.Status = StatusDisponibilidade.Indisponivel;
                }
                else if (!infraestruturaCorroboraFalhaRemota &&
                    !ultimas.Skip(1).Any(x => x.Status == StatusDisponibilidade.Operacional))
                {
                    estado.Status = StatusDisponibilidade.Inconclusivo;
                }
            }

            return estado;
        }

        /// <summary>Identifica evidência direta de indisponibilidade declarada pelo serviço fiscal.</summary>
        /// <param name="amostra">Amostra fiscal mais recente do serviço.</param>
        /// <returns>
        /// <see langword="true"/> para os códigos fiscais de indisponibilidade ou para HTTP 503;
        /// caso contrário, <see langword="false"/>.
        /// </returns>
        private static bool EhIndisponibilidadeConfirmada(ResultadoSondaDisponibilidade amostra) =>
            ClassificadorDisponibilidade.EhIndisponibilidadeFiscal(amostra.CStat) ||
            (amostra.TipoFalha == TipoFalhaDisponibilidade.HTTP &&
             amostra.HttpStatusCode == (int)HttpStatusCode.ServiceUnavailable);

        /// <summary>Verifica se as sondas locais permitem atribuir timeouts repetidos ao endpoint remoto.</summary>
        /// <param name="infraestrutura">Sondas de DNS, TCP, TLS e proxy do diagnóstico atual.</param>
        /// <param name="fiscais">Sondas fiscais que serão correlacionadas.</param>
        /// <param name="falhaLocal">Indica se existe causa local objetiva, como DNS, TLS, proxy ou certificado.</param>
        /// <param name="infraestruturaSaudavel">Indica se todas as sondas locais foram operacionais.</param>
        /// <returns><see langword="true"/> quando a rede está saudável ou quando DNS funcionou e TCP/serviço expiraram no mesmo endpoint.</returns>
        private static bool InfraestruturaCorroboraFalhaRemota(
            IList<ResultadoSondaDisponibilidade> infraestrutura,
            IList<ResultadoSondaDisponibilidade> fiscais,
            bool falhaLocal,
            bool infraestruturaSaudavel)
        {
            if (falhaLocal)
            {
                return false;
            }

            if (infraestruturaSaudavel)
            {
                return true;
            }

            var dnsOperacional = infraestrutura.Any(x =>
                string.Equals(x.Servico, "DNS", StringComparison.OrdinalIgnoreCase) &&
                x.Status == StatusDisponibilidade.Operacional);
            if (!dnsOperacional)
            {
                return false;
            }

            var endpointsComFalhaRemota = new HashSet<string>(
                infraestrutura
                    .Where(x => EhFalhaAcessoRemoto(x.TipoFalha) &&
                        !string.IsNullOrWhiteSpace(x.Endpoint))
                    .Select(x => x.IdentidadeEndpoint ?? x.Endpoint),
                StringComparer.OrdinalIgnoreCase);
            return endpointsComFalhaRemota.Count > 0 && fiscais.Any(x =>
                EhFalhaAcessoRemoto(x.TipoFalha) &&
                !string.IsNullOrWhiteSpace(x.Endpoint) &&
                endpointsComFalhaRemota.Contains(x.IdentidadeEndpoint ?? x.Endpoint));
        }

        /// <summary>Identifica falhas que indicam ausência de resposta ou recusa ativa do endpoint remoto.</summary>
        /// <param name="tipoFalha">Categoria de falha observada.</param>
        /// <returns><see langword="true"/> para timeout ou conexão recusada pelo host remoto.</returns>
        private static bool EhFalhaAcessoRemoto(TipoFalhaDisponibilidade tipoFalha) =>
            tipoFalha == TipoFalhaDisponibilidade.Timeout ||
            tipoFalha == TipoFalhaDisponibilidade.ConexaoRecusada;

        /// <summary>Verifica se uma amostra aponta para a máquina ou configuração local.</summary>
        /// <param name="item">Amostra analisada.</param>
        /// <returns><see langword="true"/> para DNS, conexão, TLS, proxy, certificado ou configuração.</returns>
        private static bool EhFalhaLocal(ResultadoSondaDisponibilidade item) =>
            item.TipoFalha == TipoFalhaDisponibilidade.Autenticacao ||
            item.TipoFalha == TipoFalhaDisponibilidade.DNS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Conexao ||
            item.TipoFalha == TipoFalhaDisponibilidade.TLS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Proxy ||
            item.TipoFalha == TipoFalhaDisponibilidade.Certificado ||
            item.TipoFalha == TipoFalhaDisponibilidade.Configuracao;
    }
}
