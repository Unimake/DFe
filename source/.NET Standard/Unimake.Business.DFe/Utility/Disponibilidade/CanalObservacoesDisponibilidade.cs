using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Net;
using System.Threading;
using System.Xml;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>Canal limitado de observações individuais, sem callbacks, disco ou sondas de rede.</summary>
    /// <remarks>O consumidor deve drenar o canal em background. Uma sessão representa uma operação de negócio,
    /// incluindo seus acessos auxiliares, e deve envolver também a construção do serviço quando ela puder autenticar.</remarks>
#if INTEROP
    [System.Runtime.InteropServices.ComVisible(false)]
#endif
    public sealed class CanalObservacoesDisponibilidade
    {
        private readonly ConcurrentQueue<ObservacaoDisponibilidade> fila = new ConcurrentQueue<ObservacaoDisponibilidade>();
        private readonly int capacidade;
        private int pendentes;
        private long descartadas;

        /// <summary>Cria um canal com capacidade fixa e descarte explícito quando cheio.</summary>
        /// <param name="capacidade">Número máximo de observações em memória.</param>
        public CanalObservacoesDisponibilidade(int capacidade = 1024)
        {
            if (capacidade < 1) throw new ArgumentOutOfRangeException(nameof(capacidade));
            this.capacidade = capacidade;
        }

        /// <summary>Quantidade acumulada de observações descartadas por saturação ou sessão não concluída.</summary>
        public long ObservacoesDescartadas => Interlocked.Read(ref descartadas);

        /// <summary>Inicia uma observação opt-in, sem carregar certificado nem executar rede.</summary>
        /// <param name="configuracao">Configuração da operação; mantida apenas durante a sessão, nunca na fila.</param>
        /// <param name="operacaoId">Identidade única da execução, estável na persistência e distinta por tentativa.</param>
        /// <param name="contextoId">Identidade opaca e durável de empresa/host/acesso, definida pelo consumidor.</param>
        /// <returns>Sessão a concluir e descartar em ordem de criação inversa.</returns>
        public SessaoObservacaoDisponibilidade IniciarOperacao(Configuracao configuracao, Guid operacaoId, Guid contextoId)
        {
            if (configuracao == null) throw new ArgumentNullException(nameof(configuracao));
            if (operacaoId == Guid.Empty) throw new ArgumentException("Informe a identidade da operação.", nameof(operacaoId));
            if (contextoId == Guid.Empty) throw new ArgumentException("Informe a identidade do contexto.", nameof(contextoId));
            return new SessaoObservacaoDisponibilidade(this, configuracao, operacaoId, contextoId);
        }

        /// <summary>Retira uma observação; não espera novos itens nem executa I/O.</summary>
        /// <param name="observacao">Observação retirada, ou nula quando vazio.</param>
        /// <returns>Se havia observação disponível.</returns>
        public bool TentarLer(out ObservacaoDisponibilidade observacao)
        {
            if (!fila.TryDequeue(out observacao)) return false;
            Interlocked.Decrement(ref pendentes);
            return true;
        }

        internal void Descartar() => Interlocked.Increment(ref descartadas);

        internal void Publicar(ObservacaoDisponibilidade observacao)
        {
            if (Interlocked.Increment(ref pendentes) > capacidade)
            {
                Interlocked.Decrement(ref pendentes);
                Descartar();
                return;
            }
            fila.Enqueue(observacao);
        }
    }

    /// <summary>Sessão isolada por fluxo assíncrono que observa uma execução sem alterar seu resultado.</summary>
#if INTEROP
    [System.Runtime.InteropServices.ComVisible(false)]
#endif
    public sealed class SessaoObservacaoDisponibilidade : IDisposable
    {
        private static readonly AsyncLocal<SessaoObservacaoDisponibilidade> Atual = new AsyncLocal<SessaoObservacaoDisponibilidade>();
        private readonly SessaoObservacaoDisponibilidade anterior;
        private readonly CanalObservacoesDisponibilidade canal;
        private Configuracao configuracao;
        private readonly Guid operacaoId;
        private readonly Guid contextoId;
        private readonly DateTimeOffset inicioUtc = DateTimeOffset.UtcNow;
        private readonly long inicio = Stopwatch.GetTimestamp();
        private string destino = string.Empty;
        private string protocolo = string.Empty;
        private string etapa = "Preparacao";
        private bool transporte;
        private HttpStatusCode ultimoHttp;
        private long inicioTransporte;
        private long duracaoTransporte = -1;
        private int finalizada;
        private int descartada;

        internal SessaoObservacaoDisponibilidade(CanalObservacoesDisponibilidade canal, Configuracao configuracao,
            Guid operacaoId, Guid contextoId)
        {
            this.canal = canal;
            this.configuracao = configuracao;
            this.operacaoId = operacaoId;
            this.contextoId = contextoId;
            anterior = Atual.Value;
            Atual.Value = this;
        }

        /// <summary>Associa a configuração efetiva antes da primeira entrada no transporte.</summary>
        /// <param name="configuracao">Configuração preparada pelo consumidor; não será publicada nem retida na fotografia.</param>
        /// <remarks>Permite abrir a sessão antes de uma preparação assíncrona e acompanhar logins do construtor.</remarks>
        public void DefinirConfiguracao(Configuracao configuracao)
        {
            if (configuracao == null) throw new ArgumentNullException(nameof(configuracao));
            if (transporte || Volatile.Read(ref finalizada) != 0 || Volatile.Read(ref descartada) != 0)
                throw new InvalidOperationException("A configuração da observação já foi fixada.");
            this.configuracao = configuracao;
        }
        /// <summary>Conclui com o retorno já obtido, sem reexecutar ou reter o XML.</summary>
        /// <param name="retorno">Retorno interpretado da operação principal; não é armazenado.</param>
        /// <param name="httpStatusCode">Código HTTP efetivamente recebido, ou zero quando ausente.</param>
        /// <param name="respostaAplicacaoReconhecida">Verdadeiro somente após reconhecimento do contrato pelo
        /// consumidor, inclusive rejeição normal. HTTP 2xx sozinho não permite informar verdadeiro.</param>
        public void Concluir(XmlDocument retorno, HttpStatusCode httpStatusCode, bool respostaAplicacaoReconhecida = false)
            => Finalizar(retorno, httpStatusCode, null, respostaAplicacaoReconhecida);

        /// <summary>Conclui com uma falha já ocorrida, sem reter a exceção ou sua mensagem.</summary>
        /// <param name="exception">Exceção original, que o chamador continua responsável por propagar.</param>
        /// <param name="httpStatusCode">Código HTTP efetivamente recebido, quando disponível.</param>
        public void Falhar(Exception exception, HttpStatusCode httpStatusCode = 0)
            => Finalizar(null, httpStatusCode, exception, false);

        private void Finalizar(XmlDocument retorno, HttpStatusCode httpStatusCode, Exception exception, bool reconhecida)
        {
            if (Volatile.Read(ref descartada) != 0 || Interlocked.Exchange(ref finalizada, 1) != 0) return;
            try
            {
                if (!configuracao.ColetarTelemetriaDisponibilidade) return;
                if (httpStatusCode == 0) httpStatusCode = ultimoHttp;
                var sonda = new ResultadoSondaDisponibilidade { HttpStatusCode = (int)httpStatusCode };
                if (exception != null)
                {
                    ClassificadorDisponibilidade.PreencherFalha(exception, sonda, incluirMensagem: false);
                    if (exception is Unimake.Exceptions.CertificadoDigitalException)
                        sonda.TipoFalha = TipoFalhaDisponibilidade.Certificado;
                }
                else
                {
                    // Reutiliza a precedência do primeiro cStat, sem guardar corpo ou xMotivo externo.
                    var no = retorno?.DocumentElement?.SelectSingleNode("descendant-or-self::*[local-name()='cStat']");
                    int cStat;
                    if (no != null && int.TryParse(no.InnerText, out cStat)) sonda.CStat = cStat;
                    ClassificadorDisponibilidade.ClassificarRespostaFiscal(sonda);
                    if (reconhecida && transporte && (int)httpStatusCode >= 200 && (int)httpStatusCode < 300 &&
                        sonda.CStat == 0)
                    {
                        sonda.Status = StatusDisponibilidade.Operacional;
                        sonda.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
                    }
                }
                if (sonda.CStat == 0 && sonda.HttpStatusCode >= 400)
                {
                    sonda.TipoFalha = TipoFalhaDisponibilidade.HTTP;
                    sonda.Status = sonda.HttpStatusCode == 503 ? StatusDisponibilidade.Indisponivel
                        : sonda.HttpStatusCode >= 500 ? StatusDisponibilidade.Degradado : StatusDisponibilidade.Inconclusivo;
                }
                // Ausência de metadados do componente não é defeito de protocolo da operação bem-sucedida.
                if (protocolo == "ComponenteExterno" && exception == null && sonda.CStat == 0 &&
                    sonda.HttpStatusCode == 0)
                {
                    sonda.Status = StatusDisponibilidade.Inconclusivo;
                    sonda.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
                }
                if (!transporte && exception == null)
                {
                    sonda.Status = StatusDisponibilidade.Inconclusivo;
                    sonda.TipoFalha = TipoFalhaDisponibilidade.Nenhuma;
                }
                canal.Publicar(new ObservacaoDisponibilidade(operacaoId, contextoId, inicioUtc, DateTimeOffset.UtcNow,
                    (long)((Stopwatch.GetTimestamp() - inicio) * 1000.0 / Stopwatch.Frequency),
                    configuracao, destino, protocolo, etapa, transporte,
                    transporte ? duracaoTransporte >= 0 ? duracaoTransporte :
                        (long)((Stopwatch.GetTimestamp() - inicioTransporte) * 1000.0 / Stopwatch.Frequency) : 0, sonda));
            }
            catch
            {
                canal.Descartar();
                // A falha do diagnóstico não substitui o retorno ou a exceção da operação observada.
            }
        }

        internal static void InformarTransporte(string endpoint, string protocolo, string etapa = "Servico")
        {
            try
            {
                var sessao = Atual.Value;
                if (sessao == null || sessao.configuracao == null || Volatile.Read(ref sessao.finalizada) != 0 ||
                    !sessao.configuracao.ColetarTelemetriaDisponibilidade) return;
                Uri uri;
                // URLs de integrações podem conter documento, chave ou token no próprio path.
                sessao.destino = Uri.TryCreate(endpoint, UriKind.Absolute, out uri) &&
                    (uri.Scheme == Uri.UriSchemeHttp || uri.Scheme == Uri.UriSchemeHttps)
                    ? uri.GetComponents(UriComponents.SchemeAndServer, UriFormat.UriEscaped) : string.Empty;
                sessao.inicioTransporte = Stopwatch.GetTimestamp();
                sessao.duracaoTransporte = -1;
                sessao.ultimoHttp = 0;
                sessao.protocolo = protocolo;
                sessao.etapa = !string.IsNullOrWhiteSpace(endpoint) &&
                    (endpoint == sessao.configuracao.RequestURILoginProducao ||
                     endpoint == sessao.configuracao.RequestURILoginHomologacao) ? "Autenticacao" : etapa;
                sessao.transporte = true;
            }
            catch
            {
                // Nenhuma falha de instrumentação pode alterar o transporte.
            }
        }

        internal static void InformarRespostaHttp(HttpStatusCode codigo)
        {
            var sessao = Atual.Value;
            if (sessao != null && Volatile.Read(ref sessao.finalizada) == 0)
            {
                sessao.ultimoHttp = codigo;
                sessao.duracaoTransporte = (long)((Stopwatch.GetTimestamp() - sessao.inicioTransporte) *
                    1000.0 / Stopwatch.Frequency);
            }
        }

        /// <summary>Restaura o contexto anterior; uma sessão não concluída é contabilizada como perda.</summary>
        public void Dispose()
        {
            if (Interlocked.Exchange(ref descartada, 1) != 0) return;
            if (ReferenceEquals(Atual.Value, this)) Atual.Value = anterior;
            if (Volatile.Read(ref finalizada) == 0 && configuracao.ColetarTelemetriaDisponibilidade) canal.Descartar();
            configuracao = null;
        }
    }

    /// <summary>Fotografia imutável e sanitizada de uma operação; não contém configuração, XML ou exceção.</summary>
#if INTEROP
    [System.Runtime.InteropServices.ComVisible(false)]
#endif
    public sealed class ObservacaoDisponibilidade
    {
        /// <summary>Identidade única da execução, fornecida pelo consumidor.</summary>
        public Guid OperacaoId { get; }
        /// <summary>Identidade opaca do contexto de acesso; não contém credencial.</summary>
        public Guid ContextoId { get; }
        /// <summary>Início UTC da operação observada.</summary>
        public DateTimeOffset InicioUtc { get; }
        /// <summary>Conclusão UTC da observação.</summary>
        public DateTimeOffset ConclusaoUtc { get; }
        /// <summary>Duração da operação completa, incluindo acessos auxiliares, em milissegundos.</summary>
        public long DuracaoMilissegundos { get; }
        /// <summary>Duração até a resposta HTTP do último transporte ou até sua falha; não inclui preparação anterior.</summary>
        public long DuracaoTransporteMilissegundos { get; }
        /// <summary>Documento ou integração.</summary>
        public TipoDFe TipoDFe { get; }
        /// <summary>Serviço principal solicitado.</summary>
        public Servico Servico { get; }
        /// <summary>Ambiente fiscal.</summary>
        public TipoAmbiente TipoAmbiente { get; }
        /// <summary>Código da UF informado na configuração.</summary>
        public int CodigoUF { get; }
        /// <summary>Código IBGE quando aplicável.</summary>
        public int CodigoMunicipio { get; }
        /// <summary>Padrão municipal quando aplicável.</summary>
        public PadraoNFSe PadraoNFSe { get; }
        /// <summary>Somente esquema, host e porta, sem usuário, path, query ou fragmento.</summary>
        public string Destino { get; }
        /// <summary>SOAP, REST ou ComponenteExterno conforme a última chamada observada.</summary>
        public string Protocolo { get; }
        /// <summary>Etapa observada: Preparacao, Servico ou Autenticacao.</summary>
        public string Etapa { get; }
        /// <summary>Indica entrada no transporte; não garante que o servidor recebeu a requisição.</summary>
        public bool TransporteObservado { get; }
        /// <summary>Status técnico da observação; não é a análise temporal do PRO.</summary>
        public StatusDisponibilidade Status { get; }
        /// <summary>Categoria técnica de falha.</summary>
        public TipoFalhaDisponibilidade TipoFalha { get; }
        /// <summary>Código HTTP efetivamente observado.</summary>
        public int HttpStatusCode { get; }
        /// <summary>Primeiro cStat da resposta principal, quando presente.</summary>
        public int CStat { get; }

        internal ObservacaoDisponibilidade(Guid operacaoId, Guid contextoId, DateTimeOffset inicioUtc,
            DateTimeOffset conclusaoUtc, long duracao, Configuracao configuracao, string destino,
            string protocolo, string etapa, bool transporte, long duracaoTransporte, ResultadoSondaDisponibilidade sonda)
        {
            OperacaoId = operacaoId;
            ContextoId = contextoId;
            InicioUtc = inicioUtc;
            ConclusaoUtc = conclusaoUtc;
            DuracaoMilissegundos = duracao;
            DuracaoTransporteMilissegundos = duracaoTransporte;
            TipoDFe = configuracao.TipoDFe;
            Servico = configuracao.ServicoObservacaoDisponibilidade;
            TipoAmbiente = configuracao.TipoAmbiente;
            CodigoUF = configuracao.CodigoUF;
            CodigoMunicipio = configuracao.CodigoMunicipio;
            PadraoNFSe = configuracao.PadraoNFSe;
            Destino = destino;
            Protocolo = protocolo;
            Etapa = etapa;
            TransporteObservado = transporte;
            Status = sonda.Status;
            TipoFalha = sonda.TipoFalha;
            HttpStatusCode = sonda.HttpStatusCode;
            CStat = sonda.CStat;
        }
    }
}