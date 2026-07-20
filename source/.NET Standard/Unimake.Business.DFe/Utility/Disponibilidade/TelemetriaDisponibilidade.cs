using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class RelogioDisponibilidade
    {
        internal static Func<DateTime> Agora = () => DateTime.Now;
    }

    internal static class TelemetriaDisponibilidade
    {
        private const int MaximoAmostrasPorChave = 20;
        private const int MaximoChaves = 512;
        private static readonly object SyncRoot = new object();
        private static readonly Dictionary<string, Queue<ResultadoSondaDisponibilidade>> Historico =
            new Dictionary<string, Queue<ResultadoSondaDisponibilidade>>(StringComparer.OrdinalIgnoreCase);

        internal static void Registrar(Configuracao configuracao, string endpoint, string protocolo, long duracao,
            HttpStatusCode httpStatusCode, XmlDocument retorno, Exception exception)
        {
            if (configuracao == null || !configuracao.ColetarTelemetriaDisponibilidade || !EhDFeSuportado(configuracao.TipoDFe))
            {
                return;
            }

            try
            {
                var amostra = new ResultadoSondaDisponibilidade
                {
                    Servico = configuracao.Servico.ToString(),
                    Endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint),
                    Protocolo = protocolo,
                    Fonte = FonteEvidenciaDisponibilidade.TelemetriaPassiva,
                    DataHora = RelogioDisponibilidade.Agora(),
                    DuracaoMilissegundos = duracao,
                    HttpStatusCode = (int)httpStatusCode,
                    Essencial = EhServicoEssencial(configuracao.Servico)
                };

                if (exception != null)
                {
                    ClassificadorDisponibilidade.PreencherFalha(exception, amostra);
                }
                else
                {
                    PreencherRetorno(retorno, amostra);
                    ClassificadorDisponibilidade.ClassificarRespostaFiscal(amostra);
                }

                var chave = CriarChave(configuracao, amostra);
                if (amostra.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
                {
                    CacheStatusDisponibilidade.BloquearContexto(configuracao, amostra.DataHora.AddHours(1));
                }
                lock (SyncRoot)
                {
                    Queue<ResultadoSondaDisponibilidade> fila;
                    if (!Historico.TryGetValue(chave, out fila))
                    {
                        if (Historico.Count >= MaximoChaves)
                        {
                            Historico.Remove(Historico.Keys.First());
                        }
                        fila = new Queue<ResultadoSondaDisponibilidade>();
                        Historico.Add(chave, fila);
                    }

                    fila.Enqueue(amostra);
                    while (fila.Count > MaximoAmostrasPorChave)
                    {
                        fila.Dequeue();
                    }
                }
            }
            catch
            {
                // A telemetria nunca pode alterar o comportamento da operação fiscal observada.
            }
        }

        internal static IList<ResultadoSondaDisponibilidade> Obter(Configuracao configuracao,
            ConfiguracaoDiagnosticoDisponibilidade opcoes)
        {
            var resultados = new List<ResultadoSondaDisponibilidade>();
            var agora = RelogioDisponibilidade.Agora();
            var limite = agora.AddMinutes(-opcoes.JanelaEvidenciaMinutos);
            var prefixoFiscal = CriarPrefixo(configuracao) + "F|*|";
            var prefixoAcesso = CriarPrefixo(configuracao) + "A|" + IdentidadeCertificado(configuracao) + "|";
            var prefixoFiscalNacional = CriarPrefixo(configuracao.TipoDFe, (int)UFBrasil.AN, configuracao.TipoAmbiente) + "F|*|";
            var prefixoAcessoNacional = CriarPrefixo(configuracao.TipoDFe, (int)UFBrasil.AN, configuracao.TipoAmbiente) + "A|" + IdentidadeCertificado(configuracao) + "|";

            lock (SyncRoot)
            {
                foreach (var item in Historico)
                {
                    if (!item.Key.StartsWith(prefixoFiscal, StringComparison.OrdinalIgnoreCase) &&
                        !item.Key.StartsWith(prefixoAcesso, StringComparison.OrdinalIgnoreCase) &&
                        !item.Key.StartsWith(prefixoFiscalNacional, StringComparison.OrdinalIgnoreCase) &&
                        !item.Key.StartsWith(prefixoAcessoNacional, StringComparison.OrdinalIgnoreCase))
                    {
                        continue;
                    }

                    foreach (var amostra in item.Value.Where(x => x.DataHora >= limite))
                    {
                        if (opcoes.AceitaServico(amostra.Servico))
                        {
                            var copia = ClassificadorDisponibilidade.Clonar(amostra);
                            copia.IdadeSegundos = Math.Max(0, (long)(agora - copia.DataHora).TotalSeconds);
                            if (copia.Status == StatusDisponibilidade.Operacional &&
                                copia.DuracaoMilissegundos >= opcoes.LimiteLentidaoMilissegundos)
                            {
                                copia.Status = StatusDisponibilidade.Degradado;
                                copia.XMotivo = "O serviço respondeu acima do limite de lentidão.";
                            }
                            resultados.Add(copia);
                        }
                    }
                }
            }

            return resultados.OrderBy(x => x.DataHora).ToList();
        }

        internal static void Limpar()
        {
            lock (SyncRoot)
            {
                Historico.Clear();
            }
        }

        private static void PreencherRetorno(XmlDocument retorno, ResultadoSondaDisponibilidade amostra)
        {
            if (retorno == null || retorno.DocumentElement == null)
            {
                return;
            }

            var noCStat = retorno.SelectSingleNode("//*[local-name()='cStat']");
            int codigo;
            if (noCStat != null && int.TryParse(noCStat.InnerText, NumberStyles.Integer, CultureInfo.InvariantCulture, out codigo))
            {
                amostra.CStat = codigo;
            }
        }

        private static string CriarPrefixo(Configuracao configuracao) =>
            CriarPrefixo(configuracao.TipoDFe, configuracao.CodigoUF, configuracao.TipoAmbiente);

        private static string CriarPrefixo(TipoDFe tipoDFe, int codigoUF, TipoAmbiente tipoAmbiente) =>
            ((int)tipoDFe).ToString(CultureInfo.InvariantCulture) + "|" +
            codigoUF.ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)tipoAmbiente).ToString(CultureInfo.InvariantCulture) + "|";

        private static string CriarChave(Configuracao configuracao, ResultadoSondaDisponibilidade amostra)
        {
            var acesso = amostra.Servico + "|" + amostra.Endpoint;
            var escopo = "F|*|";
            // TLS pode depender do certificado apresentado; não deve contaminar outros contribuintes no mesmo processo.
            if ((amostra.TipoFalha == TipoFalhaDisponibilidade.TLS || amostra.TipoFalha == TipoFalhaDisponibilidade.Certificado) &&
                configuracao.CertificadoDigital != null)
            {
                escopo = "A|" + IdentidadeCertificado(configuracao) + "|";
            }
            return CriarPrefixo(configuracao) + escopo + acesso;
        }

        private static string IdentidadeCertificado(Configuracao configuracao)
        {
            if (configuracao.CertificadoDigital == null) return "sem-certificado";
            try
            {
                using (var sha = SHA256.Create())
                {
                    var bytes = sha.ComputeHash(Encoding.UTF8.GetBytes(configuracao.CertificadoDigital.Thumbprint ?? string.Empty));
                    return BitConverter.ToString(bytes).Replace("-", string.Empty);
                }
            }
            catch
            {
                return "certificado-indisponivel";
            }
        }

        private static bool EhDFeSuportado(TipoDFe tipoDFe) => tipoDFe == TipoDFe.NFe || tipoDFe == TipoDFe.NFCe ||
            tipoDFe == TipoDFe.CTe || tipoDFe == TipoDFe.MDFe;

        private static bool EhServicoEssencial(Servico servico)
        {
            var nome = servico.ToString();
            return nome.IndexOf("Autorizacao", StringComparison.OrdinalIgnoreCase) >= 0 ||
                   nome.IndexOf("StatusServico", StringComparison.OrdinalIgnoreCase) >= 0;
        }
    }

    internal static class ClassificadorDisponibilidade
    {
        internal static void ClassificarRespostaFiscal(ResultadoSondaDisponibilidade resultado)
        {
            resultado.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
            if (resultado.CStat == 108 || resultado.CStat == 109)
            {
                resultado.Status = StatusDisponibilidade.Indisponivel;
                resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                resultado.XMotivo = "A autoridade fiscal informou indisponibilidade do serviço.";
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

        internal static void PreencherFalha(Exception exception, ResultadoSondaDisponibilidade resultado)
        {
            resultado.Status = StatusDisponibilidade.Inconclusivo;
            resultado.TipoFalha = TipoFalhaDisponibilidade.Desconhecida;
            resultado.Excecao = SanitizarExcecao(exception);
            var webException = EncontrarWebException(exception);
            if (webException == null)
            {
                resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
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
                    resultado.Status = resultado.HttpStatusCode >= 500 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Inconclusivo;
                    break;
            }
        }

        internal static ResultadoSondaDisponibilidade Clonar(ResultadoSondaDisponibilidade origem) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = origem.Servico,
                Endpoint = origem.Endpoint,
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

        internal static string SanitizarEndpoint(string endpoint)
        {
            if (string.IsNullOrWhiteSpace(endpoint)) return string.Empty;
            Uri uri;
            if (!Uri.TryCreate(endpoint, UriKind.Absolute, out uri)) return endpoint.Split('?')[0];
            var seguro = new UriBuilder(uri) { UserName = string.Empty, Password = string.Empty, Query = string.Empty, Fragment = string.Empty };
            return seguro.Uri.GetLeftPart(UriPartial.Path);
        }

        internal static string SanitizarExcecao(Exception exception)
        {
            var mensagem = exception?.Message ?? string.Empty;
            var quebra = mensagem.IndexOfAny(new[] { '\r', '\n' });
            if (quebra >= 0) mensagem = mensagem.Substring(0, quebra);
            mensagem = Regex.Replace(mensagem, @"https?://[^\s\""']+", m => SanitizarEndpoint(m.Value));
            mensagem = Regex.Replace(mensagem, @"\d{6,}", "***");
            return mensagem.Length > 300 ? mensagem.Substring(0, 300) : mensagem;
        }

        private static WebException EncontrarWebException(Exception exception)
        {
            for (var atual = exception; atual != null; atual = atual.InnerException)
            {
                var webException = atual as WebException;
                if (webException != null) return webException;
            }
            return null;
        }
    }

    internal static class AgregadorDisponibilidade
    {
        internal static void Agregar(ResultadoDiagnosticoDisponibilidade resultado)
        {
            var itens = resultado.Sondas.Itens.ToList();
            if (itens.Count == 0)
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Indeterminada;
                return;
            }

            if (itens.Any(x => x.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido))
            {
                resultado.Status = StatusDisponibilidade.Degradado;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.ConsumoIndevido;
                return;
            }

            var fiscais = itens.Where(x => x.Fonte != FonteEvidenciaDisponibilidade.Infraestrutura).ToList();
            var estadosServicos = fiscais
                .GroupBy(x => (x.Servico ?? string.Empty) + "|" + (x.Endpoint ?? string.Empty), StringComparer.OrdinalIgnoreCase)
                .Select(ClassificarServico)
                .ToList();
            var indisponiveis = estadosServicos.Count(x => x == StatusDisponibilidade.Indisponivel);
            var operacionais = estadosServicos.Count(x => x == StatusDisponibilidade.Operacional || x == StatusDisponibilidade.Degradado);
            var falhaLocal = itens.Any(EhFalhaLocal);
            if (indisponiveis > 0 && operacionais > 0)
            {
                resultado.Status = StatusDisponibilidade.ParcialmenteIndisponivel;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Parcial;
            }
            else if (indisponiveis > 0)
            {
                resultado.Status = StatusDisponibilidade.Indisponivel;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.AutoridadeFiscal;
            }
            else if (operacionais > 0)
            {
                resultado.Status = estadosServicos.Any(x => x == StatusDisponibilidade.Degradado) || falhaLocal
                    ? StatusDisponibilidade.Degradado
                    : StatusDisponibilidade.Operacional;
                resultado.OrigemProvavel = falhaLocal ? OrigemProvavelIndisponibilidade.AmbienteLocal : OrigemProvavelIndisponibilidade.Nenhuma;
            }
            else
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = falhaLocal ? OrigemProvavelIndisponibilidade.AmbienteLocal : OrigemProvavelIndisponibilidade.Indeterminada;
            }
        }

        private static StatusDisponibilidade ClassificarServico(IGrouping<string, ResultadoSondaDisponibilidade> grupo)
        {
            var ultimas = grupo.OrderByDescending(x => x.DataHora).Take(3).ToList();
            var maisRecente = ultimas[0];
            if (maisRecente.Status == StatusDisponibilidade.Operacional) return StatusDisponibilidade.Operacional;
            if (maisRecente.Status == StatusDisponibilidade.Indisponivel) return StatusDisponibilidade.Indisponivel;
            if (maisRecente.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido) return StatusDisponibilidade.Degradado;

            var falhasRemotas = ultimas.Count(x => x.TipoFalha == TipoFalhaDisponibilidade.Timeout ||
                (x.TipoFalha == TipoFalhaDisponibilidade.HTTP && x.HttpStatusCode >= 500));
            if (falhasRemotas >= 2) return StatusDisponibilidade.Indisponivel;
            return maisRecente.Status;
        }

        private static bool EhFalhaLocal(ResultadoSondaDisponibilidade item) =>
            item.TipoFalha == TipoFalhaDisponibilidade.DNS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Conexao ||
            item.TipoFalha == TipoFalhaDisponibilidade.TLS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Proxy ||
            item.TipoFalha == TipoFalhaDisponibilidade.Certificado;
    }
}
