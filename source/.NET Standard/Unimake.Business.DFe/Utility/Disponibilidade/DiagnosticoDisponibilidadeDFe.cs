#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Net;
using System.Net.Security;
using System.Net.Sockets;
using System.Security.Authentication;
using System.Security.Cryptography.X509Certificates;
using System.Threading.Tasks;
using Unimake.Business.DFe.ConsumirServico.Compatibility;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Consolida telemetria passiva e testes locais sem criar mensagens fiscais sintéticas.
    /// </summary>
#if INTEROP
    [ComVisible(false)]
#endif
    public class DiagnosticoDisponibilidadeDFe
    {
        private readonly Configuracao configuracao;
        private readonly ConfiguracaoDiagnosticoDisponibilidade opcoes;
        private readonly IExecutorInfraestruturaDisponibilidade executorInfraestrutura;

        /// <summary>Cria o motor de diagnóstico.</summary>
        /// <param name="configuracao">Configuração que identifica DFe, UF, ambiente e certificado.</param>
        /// <param name="opcoes">Opções; quando nulas, são usados os padrões seguros.</param>
        public DiagnosticoDisponibilidadeDFe(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes = null)
            : this(configuracao, opcoes, new ExecutorInfraestruturaDisponibilidade()) { }

        internal DiagnosticoDisponibilidadeDFe(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes,
            IExecutorInfraestruturaDisponibilidade executorInfraestrutura)
        {
            this.configuracao = configuracao ?? throw new ArgumentNullException(nameof(configuracao));
            this.opcoes = opcoes ?? new ConfiguracaoDiagnosticoDisponibilidade();
            this.executorInfraestrutura = executorInfraestrutura ?? throw new ArgumentNullException(nameof(executorInfraestrutura));
        }

        /// <summary>
        /// Obtém as evidências passivas e executa DNS, TCP e TLS com cache, sem enviar mensagem fiscal.
        /// </summary>
        /// <returns>Resultado consolidado.</returns>
        public ResultadoDiagnosticoDisponibilidade Executar()
        {
            opcoes.Validar();
            var resultado = CriarResultado();
            var cronometro = Stopwatch.StartNew();
            AdicionarTelemetria(resultado);

            Configuracao configuracaoStatus;
            string endpoint;
            try
            {
                configuracaoStatus = CriarConfiguracaoStatus();
                endpoint = ObterEndpoint(configuracaoStatus);
                if (string.IsNullOrWhiteSpace(endpoint))
                {
                    resultado.Sondas.Add(CriarNaoAplicavel("Infraestrutura", "Não existe endpoint de status configurado."));
                }
                else
                {
                    var falhaCertificado = ValidarCertificado(configuracaoStatus.CertificadoDigital);
                    if (falhaCertificado != null) resultado.Sondas.Add(falhaCertificado);
                    foreach (var item in CacheInfraestruturaDisponibilidade.ObterOuExecutar(configuracaoStatus, endpoint, opcoes,
                        executorInfraestrutura))
                    {
                        resultado.Sondas.Add(item);
                    }
                }
            }
            catch (Exception ex)
            {
                resultado.Sondas.Add(CriarFalhaConfiguracao(ex));
            }

            Finalizar(resultado, cronometro);
            return resultado;
        }

        /// <summary>Obtém somente a telemetria passiva em memória, sem executar qualquer acesso de rede.</summary>
        /// <returns>Resultado consolidado.</returns>
        public ResultadoDiagnosticoDisponibilidade ObterDiagnosticoPassivo()
        {
            opcoes.Validar();
            var resultado = CriarResultado();
            var cronometro = Stopwatch.StartNew();
            AdicionarTelemetria(resultado);
            Finalizar(resultado, cronometro);
            return resultado;
        }

        /// <summary>
        /// Consulta explicitamente o serviço oficial de status, respeitando cache mínimo de cinco minutos.
        /// Não são enviadas mensagens sintéticas para outros serviços.
        /// </summary>
        /// <returns>Resultado combinado com telemetria, infraestrutura e status.</returns>
        public ResultadoDiagnosticoDisponibilidade ConsultarStatusServico()
        {
            var resultado = Executar();
            var cronometro = Stopwatch.StartNew();
            try
            {
                DateTime bloqueadoAte;
                if (CacheStatusDisponibilidade.ContextoBloqueado(configuracao, out bloqueadoAte))
                {
                    resultado.Sondas.Add(new ResultadoSondaDisponibilidade
                    {
                        Servico = "StatusServico",
                        Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                        DataHora = RelogioDisponibilidade.Agora(),
                        Status = StatusDisponibilidade.Degradado,
                        TipoFalha = TipoFalhaDisponibilidade.ConsumoIndevido,
                        XMotivo = "A consulta de status foi suprimida até " + bloqueadoAte.ToString("s", CultureInfo.InvariantCulture) + " por consumo indevido observado.",
                        DoCache = true,
                        Essencial = true
                    });
                    cronometro.Stop();
                    resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
                    AgregadorDisponibilidade.Agregar(resultado);
                    return resultado;
                }

                var configuracaoStatus = CriarConfiguracaoStatus();
                var endpoint = ObterEndpoint(configuracaoStatus);
                if (string.IsNullOrWhiteSpace(endpoint))
                {
                    resultado.Sondas.Add(CriarNaoAplicavel("StatusServico", "Não existe endpoint de status configurado."));
                }
                else
                {
                    var chave = CriarChaveStatus(configuracaoStatus, endpoint);
                    var status = CacheStatusDisponibilidade.ObterOuExecutar(chave,
                        TimeSpan.FromMinutes(opcoes.IntervaloMinimoStatusMinutos),
                        () => ExecutarStatusServico(configuracaoStatus, endpoint));
                    resultado.Sondas.Add(status);
                }
            }
            catch (Exception ex)
            {
                resultado.Sondas.Add(CriarFalhaConfiguracao(ex));
            }

            cronometro.Stop();
            resultado.DuracaoTotalMilissegundos += cronometro.ElapsedMilliseconds;
            AgregadorDisponibilidade.Agregar(resultado);
            return resultado;
        }

        /// <summary>Limpa telemetria e caches mantidos somente em memória no processo atual.</summary>
        public static void LimparMemoriaDiagnostico()
        {
            TelemetriaDisponibilidade.Limpar();
            CacheInfraestruturaDisponibilidade.Limpar();
            CacheStatusDisponibilidade.Limpar();
        }

        private ResultadoDiagnosticoDisponibilidade CriarResultado() => new ResultadoDiagnosticoDisponibilidade
        {
            TipoDFe = configuracao.TipoDFe,
            UFBrasil = (UFBrasil)configuracao.CodigoUF,
            TipoAmbiente = configuracao.TipoAmbiente,
            Inicio = RelogioDisponibilidade.Agora()
        };

        private void AdicionarTelemetria(ResultadoDiagnosticoDisponibilidade resultado)
        {
            foreach (var item in TelemetriaDisponibilidade.Obter(configuracao, opcoes))
            {
                resultado.Sondas.Add(item);
            }
        }

        private Configuracao CriarConfiguracaoStatus()
        {
            var copia = new Configuracao
            {
                TipoDFe = configuracao.TipoDFe,
                TipoEmissao = configuracao.TipoEmissao,
                CodigoUF = configuracao.CodigoUF,
                CodigoMunicipio = configuracao.CodigoMunicipio,
                TipoAmbiente = configuracao.TipoAmbiente,
                SchemaVersao = ObterVersaoStatus(),
                CertificadoDigital = configuracao.CertificadoDigital,
                HasProxy = configuracao.HasProxy,
                ProxyAutoDetect = configuracao.ProxyAutoDetect,
                ProxyUser = configuracao.ProxyUser,
                ProxyPassword = configuracao.ProxyPassword,
                TimeOutWebServiceConnect = opcoes.TimeoutMilissegundos,
                ColetarTelemetriaDisponibilidade = false
            };
            copia.Load("StatusServico");
            return copia;
        }

        private string ObterVersaoStatus()
        {
            if (configuracao.TipoDFe == TipoDFe.MDFe) return "3.00";
            if (configuracao.TipoDFe == TipoDFe.CTe) return string.IsNullOrWhiteSpace(configuracao.SchemaVersao) ? "4.00" : configuracao.SchemaVersao;
            return "4.00";
        }

        private ResultadoSondaDisponibilidade ExecutarStatusServico(Configuracao configuracaoStatus, string endpoint)
        {
            var resultado = new ResultadoSondaDisponibilidade
            {
                Servico = "StatusServico",
                Endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint),
                Protocolo = "SOAP",
                Fonte = FonteEvidenciaDisponibilidade.StatusServico,
                DataHora = RelogioDisponibilidade.Agora(),
                Essencial = true
            };
            var cronometro = Stopwatch.StartNew();
            try
            {
                using (var consumidor = new ConsumirWS())
                {
                    var soap = new ConfiguracaoWSSoapMapper().Map(configuracaoStatus);
                    consumidor.ExecutarServico(CriarXmlStatus(configuracaoStatus), soap, configuracaoStatus.CertificadoDigital);
                    resultado.HttpStatusCode = (int)consumidor.HttpStatusCode;
                    PreencherCStat(consumidor.RetornoServicoXML, resultado);
                }

                ClassificadorDisponibilidade.ClassificarRespostaFiscal(resultado);
                if (resultado.CStat != 107 && resultado.CStat != 108 && resultado.CStat != 109 &&
                    resultado.CStat != 656 && resultado.CStat != 678)
                {
                    resultado.Status = StatusDisponibilidade.Degradado;
                    resultado.TipoFalha = TipoFalhaDisponibilidade.Protocolo;
                    resultado.XMotivo = "O serviço de status respondeu com código fiscal inesperado.";
                }
            }
            catch (Exception ex)
            {
                ClassificadorDisponibilidade.PreencherFalha(ex, resultado);
            }
            finally
            {
                cronometro.Stop();
                resultado.DuracaoMilissegundos = cronometro.ElapsedMilliseconds;
            }

            if (resultado.Status == StatusDisponibilidade.Operacional && resultado.DuracaoMilissegundos >= opcoes.LimiteLentidaoMilissegundos)
            {
                resultado.Status = StatusDisponibilidade.Degradado;
                resultado.XMotivo = "O serviço de status respondeu acima do limite de lentidão.";
            }
            return resultado;
        }

        private System.Xml.XmlDocument CriarXmlStatus(Configuracao configuracaoStatus)
        {
            switch (configuracaoStatus.TipoDFe)
            {
                case TipoDFe.NFe:
                case TipoDFe.NFCe:
                    return new Xml.NFe.ConsStatServ
                    {
                        Versao = "4.00",
                        TpAmb = configuracaoStatus.TipoAmbiente,
                        CUF = (UFBrasil)configuracaoStatus.CodigoUF
                    }.GerarXML();
                case TipoDFe.CTe:
                    return new Xml.CTe.ConsStatServCte
                    {
                        Versao = configuracaoStatus.SchemaVersao,
                        TpAmb = configuracaoStatus.TipoAmbiente,
                        CUF = (UFBrasil)configuracaoStatus.CodigoUF
                    }.GerarXML();
                case TipoDFe.MDFe:
                    return new Xml.MDFe.ConsStatServMDFe
                    {
                        Versao = "3.00",
                        TpAmb = configuracaoStatus.TipoAmbiente
                    }.GerarXML();
                default:
                    throw new NotSupportedException("O documento não possui consulta de status registrada no diagnóstico.");
            }
        }

        private static void PreencherCStat(System.Xml.XmlDocument retorno, ResultadoSondaDisponibilidade resultado)
        {
            var no = retorno?.SelectSingleNode("//*[local-name()='cStat']");
            int codigo;
            if (no != null && int.TryParse(no.InnerText, NumberStyles.Integer, CultureInfo.InvariantCulture, out codigo))
            {
                resultado.CStat = codigo;
            }
        }

        private static string ObterEndpoint(Configuracao item) => item.IsAPI
            ? (item.TipoAmbiente == TipoAmbiente.Producao ? item.RequestURIProducao : item.RequestURIHomologacao)
            : (item.TipoAmbiente == TipoAmbiente.Producao ? item.WebEnderecoProducao : item.WebEnderecoHomologacao);

        private static ResultadoSondaDisponibilidade ValidarCertificado(X509Certificate2 certificado)
        {
            if (certificado == null) return CriarFalhaCertificado("O certificado digital não foi informado.");
            try
            {
                if (!certificado.HasPrivateKey) return CriarFalhaCertificado("O certificado digital não possui chave privada.");
                var agora = RelogioDisponibilidade.Agora();
                if (agora < certificado.NotBefore || agora > certificado.NotAfter)
                    return CriarFalhaCertificado("O certificado digital está fora do período de validade.");
            }
            catch (Exception ex)
            {
                return CriarFalhaCertificado(ClassificadorDisponibilidade.SanitizarExcecao(ex));
            }
            return null;
        }

        private static ResultadoSondaDisponibilidade CriarFalhaCertificado(string mensagem) => new ResultadoSondaDisponibilidade
        {
            Servico = "CertificadoDigital",
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.Inconclusivo,
            TipoFalha = TipoFalhaDisponibilidade.Certificado,
            Excecao = mensagem,
            Essencial = true
        };

        private static ResultadoSondaDisponibilidade CriarFalhaConfiguracao(Exception ex) => new ResultadoSondaDisponibilidade
        {
            Servico = "Configuracao",
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.Inconclusivo,
            TipoFalha = TipoFalhaDisponibilidade.Configuracao,
            Excecao = ClassificadorDisponibilidade.SanitizarExcecao(ex),
            Essencial = true
        };

        private static ResultadoSondaDisponibilidade CriarNaoAplicavel(string servico, string motivo) => new ResultadoSondaDisponibilidade
        {
            Servico = servico,
            Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
            DataHora = RelogioDisponibilidade.Agora(),
            Status = StatusDisponibilidade.NaoAplicavel,
            TipoFalha = TipoFalhaDisponibilidade.Nenhuma,
            XMotivo = motivo
        };

        private static string CriarChaveStatus(Configuracao item, string endpoint) =>
            ((int)item.TipoDFe).ToString(CultureInfo.InvariantCulture) + "|" + item.CodigoUF.ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)item.TipoAmbiente).ToString(CultureInfo.InvariantCulture) + "|" + ClassificadorDisponibilidade.SanitizarEndpoint(endpoint);

        private static void Finalizar(ResultadoDiagnosticoDisponibilidade resultado, Stopwatch cronometro)
        {
            cronometro.Stop();
            resultado.DuracaoTotalMilissegundos = cronometro.ElapsedMilliseconds;
            AgregadorDisponibilidade.Agregar(resultado);
        }
    }

    internal interface IExecutorInfraestruturaDisponibilidade
    {
        IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos);
    }

    internal sealed class ExecutorInfraestruturaDisponibilidade : IExecutorInfraestruturaDisponibilidade
    {
        public IList<ResultadoSondaDisponibilidade> Executar(Configuracao configuracao, string endpoint, int timeoutMilissegundos)
        {
            var resultados = new List<ResultadoSondaDisponibilidade>();
            Uri uri;
            if (!Uri.TryCreate(endpoint, UriKind.Absolute, out uri))
            {
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.Configuracao, new UriFormatException("Endpoint inválido.")));
                return resultados;
            }

            var soap = new ConfiguracaoWSSoapMapper().Map(configuracao);
            if (soap.Proxy != null)
            {
                var cronometroProxy = Stopwatch.StartNew();
                var teste = Unimake.Net.Utility.TestHttpConnectionDetailed(endpoint, configuracao.CertificadoDigital,
                    Math.Max(1, timeoutMilissegundos / 1000), soap.Proxy, "HEAD");
                cronometroProxy.Stop();
                var proxy = Base("Proxy", endpoint, "HTTP", cronometroProxy.ElapsedMilliseconds);
                proxy.HttpStatusCode = teste.StatusCode;
                proxy.Status = teste.ResponseReceived ? StatusDisponibilidade.Operacional : StatusDisponibilidade.Inconclusivo;
                if (!teste.ResponseReceived)
                {
                    proxy.TipoFalha = ConverterFalha(teste.FailureType);
                    proxy.Excecao = teste.ErrorMessage;
                }
                resultados.Add(proxy);
                return resultados;
            }

            IPAddress[] enderecos;
            var cronometroDns = Stopwatch.StartNew();
            try
            {
                var tarefaDns = Dns.GetHostAddressesAsync(uri.Host);
                if (!tarefaDns.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite ao resolver o host do endpoint.");
                enderecos = tarefaDns.Result;
                cronometroDns.Stop();
                resultados.Add(Sucesso("DNS", endpoint, "DNS", cronometroDns.ElapsedMilliseconds));
            }
            catch (Exception ex)
            {
                cronometroDns.Stop();
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.DNS, ex, cronometroDns.ElapsedMilliseconds));
                return resultados;
            }

            if (enderecos.Length == 0)
            {
                resultados.Add(Falha("DNS", endpoint, TipoFalhaDisponibilidade.DNS, new Exception("O host não retornou endereços IP.")));
                return resultados;
            }

            var porta = uri.IsDefaultPort ? (uri.Scheme == Uri.UriSchemeHttps ? 443 : 80) : uri.Port;
            using (var tcp = new TcpClient())
            {
                var cronometroTcp = Stopwatch.StartNew();
                try
                {
                    var tarefa = tcp.ConnectAsync(uri.Host, porta);
                    if (!tarefa.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite ao conectar ao endpoint.");
                    cronometroTcp.Stop();
                    resultados.Add(Sucesso("TCP", endpoint, "TCP", cronometroTcp.ElapsedMilliseconds));
                }
                catch (Exception ex)
                {
                    cronometroTcp.Stop();
                    var tipo = ex is TimeoutException ? TipoFalhaDisponibilidade.Timeout : TipoFalhaDisponibilidade.Conexao;
                    resultados.Add(Falha("TCP", endpoint, tipo, ex, cronometroTcp.ElapsedMilliseconds));
                    return resultados;
                }

                if (uri.Scheme == Uri.UriSchemeHttps)
                {
                    var cronometroTls = Stopwatch.StartNew();
                    try
                    {
                        tcp.ReceiveTimeout = timeoutMilissegundos;
                        tcp.SendTimeout = timeoutMilissegundos;
                        using (var ssl = new SslStream(tcp.GetStream(), false))
                        {
                            var certificados = new X509CertificateCollection();
                            if (configuracao.CertificadoDigital != null) certificados.Add(configuracao.CertificadoDigital);
                            var tarefaTls = Task.Run(() => ssl.AuthenticateAsClient(uri.Host, certificados, SslProtocols.Tls12, true));
                            if (!tarefaTls.Wait(timeoutMilissegundos)) throw new TimeoutException("Tempo limite durante a negociação TLS.");
                            tarefaTls.GetAwaiter().GetResult();
                        }
                        cronometroTls.Stop();
                        resultados.Add(Sucesso("TLS", endpoint, "TLS", cronometroTls.ElapsedMilliseconds));
                    }
                    catch (Exception ex)
                    {
                        cronometroTls.Stop();
                        resultados.Add(Falha("TLS", endpoint, TipoFalhaDisponibilidade.TLS, ex, cronometroTls.ElapsedMilliseconds));
                    }
                }
            }
            return resultados;
        }

        private static ResultadoSondaDisponibilidade Sucesso(string servico, string endpoint, string protocolo, long duracao)
        {
            var item = Base(servico, endpoint, protocolo, duracao);
            item.Status = StatusDisponibilidade.Operacional;
            item.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
            return item;
        }

        private static ResultadoSondaDisponibilidade Falha(string servico, string endpoint, TipoFalhaDisponibilidade tipo, Exception ex, long duracao = 0)
        {
            var item = Base(servico, endpoint, servico, duracao);
            item.Status = StatusDisponibilidade.Inconclusivo;
            item.TipoFalha = tipo;
            item.Excecao = ClassificadorDisponibilidade.SanitizarExcecao(ex);
            return item;
        }

        private static ResultadoSondaDisponibilidade Base(string servico, string endpoint, string protocolo, long duracao) =>
            new ResultadoSondaDisponibilidade
            {
                Servico = servico,
                Endpoint = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint),
                Protocolo = protocolo,
                Fonte = FonteEvidenciaDisponibilidade.Infraestrutura,
                DataHora = RelogioDisponibilidade.Agora(),
                DuracaoMilissegundos = duracao,
                Essencial = true
            };

        private static TipoFalhaDisponibilidade ConverterFalha(Unimake.Net.HttpConnectionFailureType falha)
        {
            switch (falha)
            {
                case Unimake.Net.HttpConnectionFailureType.Dns: return TipoFalhaDisponibilidade.DNS;
                case Unimake.Net.HttpConnectionFailureType.Connection: return TipoFalhaDisponibilidade.Conexao;
                case Unimake.Net.HttpConnectionFailureType.Timeout: return TipoFalhaDisponibilidade.Timeout;
                case Unimake.Net.HttpConnectionFailureType.Tls: return TipoFalhaDisponibilidade.TLS;
                case Unimake.Net.HttpConnectionFailureType.Proxy: return TipoFalhaDisponibilidade.Proxy;
                case Unimake.Net.HttpConnectionFailureType.Http: return TipoFalhaDisponibilidade.HTTP;
                default: return TipoFalhaDisponibilidade.Desconhecida;
            }
        }
    }

    internal static class CacheInfraestruturaDisponibilidade
    {
        private const int MaximoEntradas = 256;
        private sealed class Entrada
        {
            public readonly object SyncRoot = new object();
            public DateTime DataHora;
            public IList<ResultadoSondaDisponibilidade> Resultados;
        }
        private static readonly object SyncRoot = new object();
        private static readonly Dictionary<string, Entrada> Cache = new Dictionary<string, Entrada>(StringComparer.OrdinalIgnoreCase);

        internal static IList<ResultadoSondaDisponibilidade> ObterOuExecutar(Configuracao configuracao, string endpoint,
            ConfiguracaoDiagnosticoDisponibilidade opcoes, IExecutorInfraestruturaDisponibilidade executor)
        {
            var identidadeCertificado = configuracao.CertificadoDigital == null ? "sem-certificado" : configuracao.CertificadoDigital.GetCertHashString();
            var chave = ClassificadorDisponibilidade.SanitizarEndpoint(endpoint) + "|" + identidadeCertificado + "|" +
                configuracao.HasProxy.ToString(CultureInfo.InvariantCulture) + "|" + configuracao.ProxyAutoDetect.ToString(CultureInfo.InvariantCulture);
            Entrada entrada;
            lock (SyncRoot)
            {
                if (!Cache.TryGetValue(chave, out entrada))
                {
                    if (Cache.Count >= MaximoEntradas)
                    {
                        var antiga = Cache.OrderBy(x => x.Value.DataHora).First().Key;
                        Cache.Remove(antiga);
                    }
                    entrada = new Entrada();
                    Cache.Add(chave, entrada);
                }
            }

            lock (entrada.SyncRoot)
            {
                var agora = RelogioDisponibilidade.Agora();
                if (entrada.Resultados != null && agora - entrada.DataHora < TimeSpan.FromSeconds(opcoes.CacheInfraestruturaSegundos))
                {
                    return entrada.Resultados.Select(x => { var c = ClassificadorDisponibilidade.Clonar(x); c.DoCache = true; c.IdadeSegundos = (long)(agora - entrada.DataHora).TotalSeconds; return c; }).ToList();
                }
                var resultados = executor.Executar(configuracao, endpoint, opcoes.TimeoutMilissegundos);
                entrada.DataHora = agora;
                entrada.Resultados = resultados.Select(ClassificadorDisponibilidade.Clonar).ToList();
                return resultados;
            }
        }

        internal static void Limpar() { lock (SyncRoot) Cache.Clear(); }
    }

    internal static class CacheStatusDisponibilidade
    {
        private const int MaximoEntradas = 256;
        private sealed class Entrada
        {
            public readonly object SyncRoot = new object();
            public DateTime DataHora;
            public DateTime BloqueadoAte;
            public ResultadoSondaDisponibilidade Resultado;
        }

        private static readonly object CacheSync = new object();
        private static readonly Dictionary<string, Entrada> Cache = new Dictionary<string, Entrada>(StringComparer.OrdinalIgnoreCase);
        private static readonly Dictionary<string, DateTime> BloqueiosContexto = new Dictionary<string, DateTime>(StringComparer.OrdinalIgnoreCase);

        internal static void BloquearContexto(Configuracao configuracao, DateTime bloqueadoAte)
        {
            var chave = ChaveContexto(configuracao);
            lock (CacheSync)
            {
                DateTime atual;
                if (!BloqueiosContexto.TryGetValue(chave, out atual) || atual < bloqueadoAte)
                {
                    BloqueiosContexto[chave] = bloqueadoAte;
                }
            }
        }

        internal static bool ContextoBloqueado(Configuracao configuracao, out DateTime bloqueadoAte)
        {
            lock (CacheSync)
            {
                if (BloqueiosContexto.TryGetValue(ChaveContexto(configuracao), out bloqueadoAte))
                {
                    if (RelogioDisponibilidade.Agora() < bloqueadoAte) return true;
                    BloqueiosContexto.Remove(ChaveContexto(configuracao));
                }
                return false;
            }
        }

        internal static ResultadoSondaDisponibilidade ObterOuExecutar(string chave, TimeSpan intervalo,
            Func<ResultadoSondaDisponibilidade> executar)
        {
            Entrada entrada;
            lock (CacheSync)
            {
                if (!Cache.TryGetValue(chave, out entrada))
                {
                    if (Cache.Count >= MaximoEntradas)
                    {
                        var antiga = Cache.OrderBy(x => x.Value.DataHora).First().Key;
                        Cache.Remove(antiga);
                    }
                    entrada = new Entrada();
                    Cache.Add(chave, entrada);
                }
            }

            lock (entrada.SyncRoot)
            {
                var agora = RelogioDisponibilidade.Agora();
                if (entrada.Resultado != null && (agora < entrada.BloqueadoAte || agora - entrada.DataHora < intervalo))
                {
                    var cache = ClassificadorDisponibilidade.Clonar(entrada.Resultado);
                    cache.DoCache = true;
                    cache.IdadeSegundos = Math.Max(0, (long)(agora - entrada.DataHora).TotalSeconds);
                    return cache;
                }

                var resultado = executar();
                entrada.DataHora = agora;
                entrada.Resultado = ClassificadorDisponibilidade.Clonar(resultado);
                if (resultado.TipoFalha == TipoFalhaDisponibilidade.ConsumoIndevido)
                {
                    entrada.BloqueadoAte = agora.AddHours(1);
                }
                return resultado;
            }
        }

        internal static void Limpar()
        {
            lock (CacheSync)
            {
                Cache.Clear();
                BloqueiosContexto.Clear();
            }
        }

        private static string ChaveContexto(Configuracao configuracao) =>
            ((int)configuracao.TipoDFe).ToString(CultureInfo.InvariantCulture) + "|" +
            configuracao.CodigoUF.ToString(CultureInfo.InvariantCulture) + "|" +
            ((int)configuracao.TipoAmbiente).ToString(CultureInfo.InvariantCulture);
    }

    /// <summary>Fachada COM para o diagnóstico.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.DiagnosticoDisponibilidadeDFeInterop")]
    [ComVisible(true)]
#endif
    public class DiagnosticoDisponibilidadeDFeInterop
    {
        /// <summary>Executa telemetria passiva e diagnóstico local, sem mensagem fiscal.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado.</returns>
        public ResultadoDiagnosticoDisponibilidade Executar(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).Executar();

        /// <summary>Obtém somente a telemetria passiva.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado.</returns>
        public ResultadoDiagnosticoDisponibilidade ObterDiagnosticoPassivo(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ObterDiagnosticoPassivo();

        /// <summary>Consulta explicitamente o status oficial com cache e limitação.</summary>
        /// <param name="configuracao">Configuração fiscal.</param>
        /// <param name="opcoes">Opções do diagnóstico.</param>
        /// <returns>Resultado agregado.</returns>
        public ResultadoDiagnosticoDisponibilidade ConsultarStatusServico(Configuracao configuracao, ConfiguracaoDiagnosticoDisponibilidade opcoes) =>
            new DiagnosticoDisponibilidadeDFe(configuracao, opcoes).ConsultarStatusServico();

        /// <summary>Limpa os dados mantidos em memória no processo.</summary>
        public void LimparMemoriaDiagnostico() => DiagnosticoDisponibilidadeDFe.LimparMemoriaDiagnostico();
    }
}
