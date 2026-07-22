using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    internal static class RelogioDisponibilidade
    {
        /// <summary>Função que fornece a data e hora usadas pelo diagnóstico.</summary>
        /// <remarks>É substituível nos testes para simular a passagem do tempo sem esperar de verdade.</remarks>
        internal static Func<DateTime> Agora = () => DateTime.Now;
    }

    /// <summary>Coleta, em memória, evidências das operações fiscais reais da aplicação.</summary>
    /// <remarks>
    /// A telemetria é passiva: ela observa chamadas que o sistema já faria e não envia mensagens fiscais extras.
    /// O histórico é limitado e temporário para não aumentar o custo da emissão nem criar arquivos no computador.
    /// </remarks>
    internal static class TelemetriaDisponibilidade
    {
        /// <summary>Quantidade máxima de amostras guardadas para cada combinação de serviço e contexto.</summary>
        private const int MaximoAmostrasPorChave = 20;
        /// <summary>Quantidade máxima de combinações diferentes mantidas no processo.</summary>
        private const int MaximoChaves = 512;
        /// <summary>Lock usado somente para proteger o dicionário de histórico.</summary>
        private static readonly object SyncRoot = new object();
        /// <summary>Histórico em memória, separado por documento, UF, ambiente e serviço.</summary>
        private static readonly Dictionary<string, Queue<ResultadoSondaDisponibilidade>> Historico =
            new Dictionary<string, Queue<ResultadoSondaDisponibilidade>>(StringComparer.OrdinalIgnoreCase);

        /// <summary>Verifica se a configuração permite observar as operações atuais.</summary>
        /// <param name="configuracao">Configuração usada pela operação fiscal.</param>
        /// <returns><see langword="true"/> quando a coleta está ligada e o documento é suportado.</returns>
        internal static bool EstaHabilitada(Configuracao configuracao) =>
            configuracao != null && configuracao.ColetarTelemetriaDisponibilidade &&
            EhDFeSuportado(configuracao.TipoDFe);

        /// <summary>Registra uma operação fiscal já executada, sem repetir a chamada.</summary>
        /// <param name="configuracao">Configuração da operação observada.</param>
        /// <param name="endpoint">Endereço utilizado pelo transporte; será sanitizado.</param>
        /// <param name="protocolo">Protocolo de transporte, como SOAP ou REST.</param>
        /// <param name="duracao">Duração da operação em milissegundos.</param>
        /// <param name="httpStatusCode">Status HTTP retornado, quando existir.</param>
        /// <param name="retorno">Resposta fiscal recebida, usada apenas para localizar o cStat.</param>
        /// <param name="exception">Exceção ocorrida, quando a operação falhou.</param>
        /// <remarks>Qualquer erro da própria telemetria é ignorado para nunca interromper a emissão.</remarks>
        internal static void Registrar(Configuracao configuracao, string endpoint, string protocolo, long duracao,
            HttpStatusCode httpStatusCode, XmlDocument retorno, Exception exception)
        {
            if (!EstaHabilitada(configuracao))
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
                var lockAdquirido = false;
                try
                {
                    if (!Monitor.TryEnter(SyncRoot))
                    {
                        return;
                    }
                    lockAdquirido = true;

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
                finally
                {
                    if (lockAdquirido)
                    {
                        Monitor.Exit(SyncRoot);
                    }
                }
            }
            catch
            {
                // A telemetria nunca pode alterar o comportamento da operação fiscal observada.
            }
        }

        /// <summary>Obtém cópias das evidências recentes relevantes para uma configuração.</summary>
        /// <param name="configuracao">Configuração cujo documento, UF, ambiente e certificado serão filtrados.</param>
        /// <param name="opcoes">Janela de tempo e serviços que devem ser considerados.</param>
        /// <returns>Lista ordenada por data, sem executar qualquer chamada externa.</returns>
        internal static IList<ResultadoSondaDisponibilidade> Obter(Configuracao configuracao,
            ConfiguracaoDiagnosticoDisponibilidade opcoes)
        {
            var resultados = new List<ResultadoSondaDisponibilidade>();
            var agora = RelogioDisponibilidade.Agora();
            var limite = agora.AddMinutes(-opcoes.JanelaEvidenciaMinutos);
            var identidadeCertificado = IdentidadeCertificado(configuracao);
            var prefixoFiscal = CriarPrefixo(configuracao) + "F|*|";
            var prefixoAcesso = CriarPrefixo(configuracao) + "A|" + identidadeCertificado + "|";
            var prefixoFiscalNacional = CriarPrefixo(configuracao.TipoDFe, (int)UFBrasil.AN, configuracao.TipoAmbiente) + "F|*|";
            var prefixoAcessoNacional = CriarPrefixo(configuracao.TipoDFe, (int)UFBrasil.AN, configuracao.TipoAmbiente) + "A|" + identidadeCertificado + "|";

            var amostras = new List<ResultadoSondaDisponibilidade>();
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

                    foreach (var amostra in item.Value)
                    {
                        if (amostra.DataHora >= limite)
                        {
                            amostras.Add(amostra);
                        }
                    }
                }
            }

            foreach (var amostra in amostras)
            {
                if (!opcoes.AceitaServico(amostra.Servico))
                {
                    continue;
                }

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

            return resultados.OrderBy(x => x.DataHora).ToList();
        }

        /// <summary>Apaga o histórico passivo mantido em memória.</summary>
        internal static void Limpar()
        {
            lock (SyncRoot)
            {
                Historico.Clear();
            }
        }

        /// <summary>Extrai somente o cStat da resposta, evitando guardar o XML fiscal completo.</summary>
        /// <param name="retorno">Documento XML retornado pelo serviço.</param>
        /// <param name="amostra">Amostra que receberá o código encontrado.</param>
        private static void PreencherRetorno(XmlDocument retorno, ResultadoSondaDisponibilidade amostra)
        {
            if (retorno == null || retorno.DocumentElement == null)
            {
                return;
            }

            var noCStat = retorno.DocumentElement.LocalName == "cStat"
                ? retorno.DocumentElement
                : retorno.DocumentElement.SelectSingleNode("./*[local-name()='cStat']");
            int codigo;
            if (noCStat != null && int.TryParse(noCStat.InnerText, NumberStyles.Integer, CultureInfo.InvariantCulture, out codigo))
            {
                amostra.CStat = codigo;
            }
        }

        /// <summary>Cria o prefixo de chave para a UF da configuração.</summary>
        private static string CriarPrefixo(Configuracao configuracao) =>
            CriarPrefixo(configuracao.TipoDFe, configuracao.CodigoUF, configuracao.TipoAmbiente);

        /// <summary>Cria o prefixo numérico comum usado nas chaves do histórico.</summary>
        private static string CriarPrefixo(TipoDFe tipoDFe, int codigoUF, TipoAmbiente tipoAmbiente) =>
            ((int)tipoDFe).ToString(CultureInfo.InvariantCulture) + "|" +
            codigoUF.ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)tipoAmbiente).ToString(CultureInfo.InvariantCulture) + "|";

        /// <summary>Combina o contexto fiscal, serviço e endpoint em uma chave de histórico.</summary>
        /// <remarks>Falhas dependentes do certificado ficam separadas para não misturar contribuintes.</remarks>
        private static string CriarChave(Configuracao configuracao, ResultadoSondaDisponibilidade amostra)
        {
            var acesso = amostra.Servico + "|" + amostra.Endpoint;
            var escopo = "F|*|";
            // TLS pode depender do certificado apresentado; não deve contaminar outros contribuintes no mesmo processo.
            if (amostra.TipoFalha == TipoFalhaDisponibilidade.TLS ||
                amostra.TipoFalha == TipoFalhaDisponibilidade.Certificado)
            {
                escopo = "A|" + IdentidadeCertificado(configuracao) + "|";
            }
            return CriarPrefixo(configuracao) + escopo + acesso;
        }

        /// <summary>Obtém uma identidade não reversível do certificado configurado.</summary>
        /// <remarks>Senha, certificado em Base64 e outros dados sensíveis nunca são devolvidos no resultado.</remarks>
        private static string IdentidadeCertificado(Configuracao configuracao)
        {
            try
            {
                var certificado = configuracao.CertificadoDigitalCarregado;
                if (certificado != null)
                {
                    return certificado.GetCertHashString();
                }

                string material;
                if (!string.IsNullOrWhiteSpace(configuracao.CertificadoSerialNumberOrThumbPrint))
                {
                    material = "repositorio|" + configuracao.CertificadoSerialNumberOrThumbPrint;
                }
                else if (!string.IsNullOrWhiteSpace(configuracao.CertificadoArquivo))
                {
                    material = "arquivo|" + configuracao.CertificadoArquivo + "|" +
                        (configuracao.CertificadoSenha ?? string.Empty);
                }
                else if (!string.IsNullOrWhiteSpace(configuracao.CertificadoBase64))
                {
                    material = "base64|" + configuracao.CertificadoBase64 + "|" +
                        (configuracao.CertificadoSenha ?? string.Empty);
                }
                else
                {
                    return "sem-certificado";
                }

                using (var sha = SHA256.Create())
                {
                    var bytes = sha.ComputeHash(Encoding.UTF8.GetBytes(material));
                    return BitConverter.ToString(bytes).Replace("-", string.Empty);
                }
            }
            catch
            {
                return "certificado-indisponivel";
            }
        }

        /// <summary>Verifica se o documento possui regras de telemetria implementadas nesta versão.</summary>
        private static bool EhDFeSuportado(TipoDFe tipoDFe) => tipoDFe == TipoDFe.NFe || tipoDFe == TipoDFe.NFCe ||
            tipoDFe == TipoDFe.CTe || tipoDFe == TipoDFe.MDFe;

        /// <summary>Identifica serviços cuja evidência representa diretamente a capacidade de emissão.</summary>
        private static bool EhServicoEssencial(Servico servico)
        {
            var nome = servico.ToString();
            return nome.IndexOf("Autorizacao", StringComparison.OrdinalIgnoreCase) >= 0 ||
                   nome.IndexOf("StatusServico", StringComparison.OrdinalIgnoreCase) >= 0;
        }
    }

    /// <summary>Converte respostas e exceções de transporte em classificações seguras.</summary>
    internal static class ClassificadorDisponibilidade
    {
        /// <summary>Classifica um retorno fiscal usando cStat, HTTP e motivo.</summary>
        /// <param name="resultado">Amostra que será preenchida.</param>
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

        /// <summary>Classifica uma exceção de transporte sem expor dados sensíveis.</summary>
        /// <param name="exception">Exceção original da operação.</param>
        /// <param name="resultado">Amostra que receberá o tipo de falha e mensagem sanitizada.</param>
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

        /// <summary>Cria uma cópia independente de uma amostra para cache ou retorno.</summary>
        /// <param name="origem">Amostra original.</param>
        /// <returns>Cópia com os mesmos dados públicos.</returns>
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

        /// <summary>Remove usuário, senha, consulta e fragmento de um endpoint HTTP.</summary>
        /// <param name="endpoint">Endereço potencialmente sensível.</param>
        /// <returns>Somente esquema, host, porta e caminho seguros.</returns>
        internal static string SanitizarEndpoint(string endpoint)
        {
            if (string.IsNullOrWhiteSpace(endpoint)) return string.Empty;
            Uri uri;
            if (!Uri.TryCreate(endpoint, UriKind.Absolute, out uri) ||
                (uri.Scheme != Uri.UriSchemeHttp && uri.Scheme != Uri.UriSchemeHttps))
            {
                return string.Empty;
            }
            var seguro = new UriBuilder(uri) { UserName = string.Empty, Password = string.Empty, Query = string.Empty, Fragment = string.Empty };
            return seguro.Uri.GetLeftPart(UriPartial.Path);
        }

        /// <summary>Sanitiza a mensagem de uma exceção de transporte.</summary>
        /// <param name="exception">Exceção que será resumida.</param>
        /// <returns>Mensagem curta sem URLs, credenciais ou identificadores conhecidos.</returns>
        internal static string SanitizarExcecao(Exception exception) =>
            SanitizarMensagem(exception?.Message);

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

        /// <summary>Procura uma WebException dentro da cadeia de exceções internas.</summary>
        /// <param name="exception">Exceção inicial.</param>
        /// <returns>A WebException encontrada ou <see langword="null"/>.</returns>
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

    /// <summary>Combina as sondas em um estado geral e em uma origem provável.</summary>
    /// <remarks>Considera essencialidade, evidência fiscal direta e falhas locais antes de atribuir problema à SEFAZ.</remarks>
    internal static class AgregadorDisponibilidade
    {
        /// <summary>Estado consolidado de um serviço após combinar suas tentativas recentes.</summary>
        private sealed class EstadoServico
        {
            /// <summary>Indica se o serviço é necessário para emissão ou status.</summary>
            public bool Essencial;
            /// <summary>Indica que cStat 108 ou 109 confirmou indisponibilidade fiscal.</summary>
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
            var estadosServicos = fiscais
                .GroupBy(x => (x.Servico ?? string.Empty) + "|" + (x.Endpoint ?? string.Empty), StringComparer.OrdinalIgnoreCase)
                .Select(x => ClassificarServico(x, infraestruturaSaudavel))
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
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Nenhuma;
            }
            else
            {
                resultado.Status = StatusDisponibilidade.Inconclusivo;
                resultado.OrigemProvavel = OrigemProvavelIndisponibilidade.Indeterminada;
            }
        }

        /// <summary>Classifica um serviço usando suas até três amostras mais recentes.</summary>
        /// <param name="grupo">Amostras do mesmo serviço e endpoint.</param>
        /// <param name="infraestruturaSaudavel">Indica se a rede local respondeu normalmente.</param>
        /// <returns>Estado consolidado do serviço.</returns>
        private static EstadoServico ClassificarServico(IGrouping<string, ResultadoSondaDisponibilidade> grupo,
            bool infraestruturaSaudavel)
        {
            var ultimas = grupo.OrderByDescending(x => x.DataHora).Take(3).ToList();
            var maisRecente = ultimas[0];
            var estado = new EstadoServico
            {
                Essencial = ultimas.Any(x => x.Essencial),
                IndisponibilidadeConfirmada = maisRecente.CStat == 108 || maisRecente.CStat == 109,
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

            if (maisRecente.TipoFalha == TipoFalhaDisponibilidade.Timeout)
            {
                var timeouts = ultimas.Count(x => x.TipoFalha == TipoFalhaDisponibilidade.Timeout);
                if (timeouts >= 2 && infraestruturaSaudavel)
                {
                    estado.Status = StatusDisponibilidade.Indisponivel;
                }
                else if (!ultimas.Skip(1).Any(x => x.Status == StatusDisponibilidade.Operacional))
                {
                    estado.Status = StatusDisponibilidade.Inconclusivo;
                }
            }

            return estado;
        }

        /// <summary>Verifica se uma amostra aponta para a máquina ou configuração local.</summary>
        /// <param name="item">Amostra analisada.</param>
        /// <returns><see langword="true"/> para DNS, conexão, TLS, proxy, certificado ou configuração.</returns>
        private static bool EhFalhaLocal(ResultadoSondaDisponibilidade item) =>
            item.TipoFalha == TipoFalhaDisponibilidade.DNS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Conexao ||
            item.TipoFalha == TipoFalhaDisponibilidade.TLS ||
            item.TipoFalha == TipoFalhaDisponibilidade.Proxy ||
            item.TipoFalha == TipoFalhaDisponibilidade.Certificado ||
            item.TipoFalha == TipoFalhaDisponibilidade.Configuracao;
    }
}
