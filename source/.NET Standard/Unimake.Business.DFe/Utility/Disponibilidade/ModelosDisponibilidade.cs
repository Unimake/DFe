#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Representa o estado final que o motor conseguiu observar para um serviço
    /// ou para o conjunto de serviços analisados.
    /// </summary>
    /// <remarks>
    /// O desenvolvedor normalmente deve usar este valor junto com
    /// <see cref="OrigemProvavelIndisponibilidade"/> e <see cref="ResultadoDiagnosticoDisponibilidade.Descricao"/>.
    /// O estado não deve ser interpretado sozinho como prova absoluta de que a
    /// SEFAZ está fora do ar, pois problemas locais podem impedir a observação.
    /// </remarks>
    public enum StatusDisponibilidade
    {
        /// <summary>O serviço respondeu normalmente e apresentou evidência válida de funcionamento.</summary>
        Operacional = 0,
        /// <summary>Há lentidão, oscilação, resposta inesperada ou limitação temporária de consumo.</summary>
        Degradado = 1,
        /// <summary>Somente parte dos serviços observados está indisponível.</summary>
        ParcialmenteIndisponivel = 2,
        /// <summary>Todos os serviços essenciais observados estão indisponíveis.</summary>
        Indisponivel = 3,
        /// <summary>Não existem evidências suficientes para afirmar se o problema é local ou fiscal.</summary>
        Inconclusivo = 4,
        /// <summary>O documento, ambiente, UF ou provedor ainda não possui uma sonda aplicável.</summary>
        NaoAplicavel = 5
    }

    /// <summary>
    /// Identifica a categoria técnica da falha encontrada durante uma sonda.
    /// </summary>
    /// <remarks>
    /// Este enum ajuda o desenvolvedor a tomar uma ação específica, mas para
    /// mostrar uma mensagem ao usuário final prefira a propriedade
    /// <see cref="ResultadoDiagnosticoDisponibilidade.Descricao"/>.
    /// </remarks>
    public enum TipoFalhaDisponibilidade
    {
        /// <summary>Nenhuma falha.</summary>
        Nenhuma = 0,
        /// <summary>O computador não conseguiu transformar o endereço do serviço em um endereço IP.</summary>
        DNS = 1,
        /// <summary>A conexão TCP não foi criada ou foi encerrada antes da resposta.</summary>
        Conexao = 2,
        /// <summary>O serviço ou a rede não respondeu dentro do tempo configurado.</summary>
        Timeout = 3,
        /// <summary>O canal HTTPS/TLS não pôde ser estabelecido ou validado.</summary>
        TLS = 4,
        /// <summary>O proxy configurado ou detectado não conseguiu encaminhar a requisição.</summary>
        Proxy = 5,
        /// <summary>O servidor respondeu com um código HTTP fora da faixa de sucesso.</summary>
        HTTP = 6,
        /// <summary>A resposta chegou, mas não pôde ser interpretada como resposta fiscal esperada.</summary>
        Protocolo = 7,
        /// <summary>O certificado não foi informado, expirou ou não possui chave privada.</summary>
        Certificado = 8,
        /// <summary>A configuração necessária para localizar ou acessar o serviço está inválida.</summary>
        Configuracao = 9,
        /// <summary>A autoridade fiscal informou excesso de consultas ou consumo indevido.</summary>
        ConsumoIndevido = 10,
        /// <summary>Ocorreu uma falha que ainda não pôde ser enquadrada em outra categoria.</summary>
        Desconhecida = 11
    }

    /// <summary>
    /// Indica quem ou o que provavelmente causou o estado observado.
    /// </summary>
    /// <remarks>
    /// A origem é uma inferência baseada nas sondas disponíveis. Ela não
    /// substitui a análise do suporte quando o estado for inconclusivo.
    /// </remarks>
    public enum OrigemProvavelIndisponibilidade
    {
        /// <summary>Nenhum problema foi observado nas sondas aplicáveis.</summary>
        Nenhuma = 0,
        /// <summary>Há indícios de problema no computador, rede, proxy, certificado ou configuração local.</summary>
        AmbienteLocal = 1,
        /// <summary>Há resposta fiscal ou padrão de falhas que aponta para a autoridade fiscal.</summary>
        AutoridadeFiscal = 2,
        /// <summary>Os serviços observados apresentam estados diferentes entre si.</summary>
        Parcial = 3,
        /// <summary>As evidências não permitem atribuir a causa com segurança.</summary>
        Indeterminada = 4,
        /// <summary>O contribuinte está temporariamente limitado por consumo indevido informado pela SEFAZ.</summary>
        ConsumoIndevido = 5
    }

    /// <summary>
    /// Informa de onde veio cada evidência apresentada no resultado.
    /// </summary>
    /// <remarks>
    /// A fonte permite distinguir uma operação fiscal real, um teste local de
    /// rede e uma consulta explícita ao serviço de status.
    /// </remarks>
    public enum FonteEvidenciaDisponibilidade
    {
        /// <summary>Resultado de uma operação fiscal real observada sem criar uma requisição adicional.</summary>
        TelemetriaPassiva = 0,
        /// <summary>Resultado de uma verificação local de DNS, TCP, TLS ou proxy, sem mensagem fiscal.</summary>
        Infraestrutura = 1,
        /// <summary>Resultado de uma consulta controlada ao serviço oficial de status.</summary>
        StatusServico = 2
    }

    /// <summary>
    /// Define como o diagnóstico de disponibilidade deve trabalhar.
    /// </summary>
    /// <remarks>
    /// Use esta classe quando precisar ajustar timeout, janela de histórico,
    /// cache ou limitar o diagnóstico a determinados serviços. Os valores
    /// padrão são conservadores para evitar excesso de consultas à SEFAZ.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ConfiguracaoDiagnosticoDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ConfiguracaoDiagnosticoDisponibilidade
    {
        /// <summary>Lista interna dos nomes de serviços selecionados pelo desenvolvedor.</summary>
        private readonly List<string> servicos = new List<string>();

        /// <summary>Cria as opções com padrões conservadores e seguros para uso normal.</summary>
        public ConfiguracaoDiagnosticoDisponibilidade()
        {
            TimeoutMilissegundos = 10000;
            LimiteLentidaoMilissegundos = 3000;
            JanelaEvidenciaMinutos = 15;
            CacheInfraestruturaSegundos = 60;
            IntervaloMinimoStatusMinutos = 5;
        }

        /// <summary>Tempo máximo, em milissegundos, permitido para cada etapa de infraestrutura.</summary>
        /// <remarks>Valores menores deixam o diagnóstico mais rápido, mas aumentam a chance de classificar uma rede lenta como inconclusiva.</remarks>
        public int TimeoutMilissegundos { get; set; }

        /// <summary>Tempo, em milissegundos, a partir do qual uma resposta válida é considerada lenta.</summary>
        public int LimiteLentidaoMilissegundos { get; set; }

        /// <summary>Quantidade de minutos de histórico passivo que pode ser usado no diagnóstico.</summary>
        public int JanelaEvidenciaMinutos { get; set; }

        /// <summary>Quantidade de segundos durante os quais o resultado de DNS/TCP/TLS pode ser reutilizado.</summary>
        public int CacheInfraestruturaSegundos { get; set; }

        /// <summary>Intervalo mínimo, em minutos, entre consultas explícitas ao StatusServico; o mínimo permitido é cinco.</summary>
        public int IntervaloMinimoStatusMinutos { get; set; }

        /// <summary>Informa quantos serviços foram selecionados para um diagnóstico direcionado.</summary>
        public int ServicosCount => servicos.Count;

        /// <summary>Adiciona um serviço à seleção do diagnóstico, evitando nomes repetidos.</summary>
        /// <param name="nomeServico">Nome do serviço.</param>
        public void AddServico(string nomeServico)
        {
            if (!string.IsNullOrWhiteSpace(nomeServico) && !servicos.Contains(nomeServico))
            {
                servicos.Add(nomeServico);
            }
        }

        /// <summary>Remove a seleção de serviços e faz o diagnóstico voltar a considerar todos os serviços.</summary>
        public void ClearServicos() => servicos.Clear();

        /// <summary>Obtém o nome do serviço selecionado em um índice iniciado em zero.</summary>
        /// <param name="index">Índice iniciado em zero.</param>
        /// <returns>Nome do serviço.</returns>
        public string GetServico(int index) => servicos[index];

        /// <summary>Verifica se um serviço está incluído no filtro atual.</summary>
        /// <param name="servico">Nome do serviço que será comparado.</param>
        /// <returns><see langword="true"/> quando não há filtro ou quando o serviço foi selecionado.</returns>
        internal bool AceitaServico(string servico) => servicos.Count == 0 || servicos.Contains(servico);

        /// <summary>Confere se os limites da configuração têm valores seguros antes da execução.</summary>
        /// <exception cref="ArgumentOutOfRangeException">Lançada quando um limite é zero, negativo ou menor que o mínimo permitido.</exception>
        internal void Validar()
        {
            if (TimeoutMilissegundos <= 0) throw new ArgumentOutOfRangeException(nameof(TimeoutMilissegundos));
            if (LimiteLentidaoMilissegundos <= 0) throw new ArgumentOutOfRangeException(nameof(LimiteLentidaoMilissegundos));
            if (JanelaEvidenciaMinutos <= 0) throw new ArgumentOutOfRangeException(nameof(JanelaEvidenciaMinutos));
            if (CacheInfraestruturaSegundos <= 0) throw new ArgumentOutOfRangeException(nameof(CacheInfraestruturaSegundos));
            if (IntervaloMinimoStatusMinutos < 5) throw new ArgumentOutOfRangeException(nameof(IntervaloMinimoStatusMinutos));
        }
    }

    /// <summary>
    /// Guarda todos os detalhes de uma única sonda de disponibilidade.
    /// </summary>
    /// <remarks>
    /// Uma sonda pode vir de uma operação fiscal real, de uma verificação local
    /// ou de uma consulta de status. O conjunto de sondas fica disponível em
    /// <see cref="ResultadoDiagnosticoDisponibilidade.Sondas"/>.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ResultadoSondaDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ResultadoSondaDisponibilidade
    {
        /// <summary>Nome do serviço fiscal ou da etapa local, como DNS, TCP ou StatusServico.</summary>
        public string Servico { get; set; }
        /// <summary>Endereço sanitizado do serviço, sem usuário, senha, query string ou fragmento.</summary>
        public string Endpoint { get; set; }
        /// <summary>Protocolo usado na evidência, normalmente SOAP, REST, DNS, TCP ou TLS.</summary>
        public string Protocolo { get; set; }
        /// <summary>Origem da evidência: operação real, infraestrutura ou consulta de status.</summary>
        public FonteEvidenciaDisponibilidade Fonte { get; set; }
        /// <summary>Data e hora em que a sonda foi observada.</summary>
        public DateTime DataHora { get; set; }
        /// <summary>Tempo, em segundos, entre a observação e a leitura atual do diagnóstico.</summary>
        public long IdadeSegundos { get; set; }
        /// <summary>Estado encontrado especificamente nesta sonda.</summary>
        public StatusDisponibilidade Status { get; set; }
        /// <summary>Categoria técnica da falha, quando houver.</summary>
        public TipoFalhaDisponibilidade TipoFalha { get; set; }
        /// <summary>Tempo gasto pela operação observada, em milissegundos.</summary>
        public long DuracaoMilissegundos { get; set; }
        /// <summary>Código HTTP recebido, ou zero quando não houve resposta HTTP.</summary>
        public int HttpStatusCode { get; set; }
        /// <summary>Código fiscal <c>cStat</c> recebido, ou zero quando não foi encontrado.</summary>
        public int CStat { get; set; }
        /// <summary>Motivo fiscal ou explicação criada pelo diagnóstico, sem dados sensíveis.</summary>
        public string XMotivo { get; set; }
        /// <summary>Resumo sanitizado da exceção técnica, sem credenciais ou conteúdo fiscal.</summary>
        public string Excecao { get; set; }
        /// <summary>Indica que a sonda veio do cache e não executou uma nova tentativa.</summary>
        public bool DoCache { get; set; }
        /// <summary>Indica que a sonda representa um serviço essencial para o estado agregado.</summary>
        public bool Essencial { get; set; }
    }

    /// <summary>
    /// Coleção de sondas com acesso seguro para C#, Object Pascal, Lazarus e COM.
    /// </summary>
    /// <remarks>
    /// Use <see cref="Count"/> e <see cref="GetItem(int)"/> para percorrer os
    /// itens em linguagens que não trabalham bem com enumeradores .NET.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ColecaoResultadoSondaDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ColecaoResultadoSondaDisponibilidade
    {
        /// <summary>Lista privada que mantém as sondas na ordem em que foram adicionadas.</summary>
        private readonly List<ResultadoSondaDisponibilidade> itens = new List<ResultadoSondaDisponibilidade>();
        /// <summary>Quantidade de sondas disponíveis na coleção.</summary>
        public int Count => itens.Count;
        /// <summary>Obtém uma sonda pelo índice iniciado em zero.</summary>
        /// <param name="index">Índice do item.</param>
        /// <returns>Sonda armazenada no índice solicitado.</returns>
        public ResultadoSondaDisponibilidade GetItem(int index) => itens[index];
        /// <summary>Fornece a lista interna para o motor e os agregadores.</summary>
        internal IList<ResultadoSondaDisponibilidade> Itens => itens;
        /// <summary>Adiciona uma sonda produzida internamente pelo motor.</summary>
        /// <param name="item">Sonda que será incluída no resultado.</param>
        internal void Add(ResultadoSondaDisponibilidade item) => itens.Add(item);
    }

    /// <summary>
    /// Resultado completo e amigável do diagnóstico de disponibilidade.
    /// </summary>
    /// <remarks>
    /// Use <see cref="Status"/> para regras automáticas,
    /// <see cref="OrigemProvavel"/> para orientar o suporte e
    /// <see cref="Descricao"/> para mostrar uma mensagem simples ao usuário.
    /// </remarks>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ResultadoDiagnosticoDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ResultadoDiagnosticoDisponibilidade
    {
        /// <summary>Cria um resultado vazio e inicializa a coleção de sondas.</summary>
        public ResultadoDiagnosticoDisponibilidade() => Sondas = new ColecaoResultadoSondaDisponibilidade();
        /// <summary>Tipo de documento fiscal usado na análise, como NFe, NFCe, CTe ou MDFe.</summary>
        public TipoDFe TipoDFe { get; set; }
        /// <summary>UF ou órgão nacional associado à configuração analisada.</summary>
        public UFBrasil UFBrasil { get; set; }
        /// <summary>Ambiente fiscal analisado, normalmente homologação ou produção.</summary>
        public TipoAmbiente TipoAmbiente { get; set; }
        /// <summary>Data e hora em que o motor começou a reunir as evidências.</summary>
        public DateTime Inicio { get; set; }
        /// <summary>Tempo total gasto para montar este resultado, em milissegundos.</summary>
        public long DuracaoTotalMilissegundos { get; set; }
        /// <summary>Estado geral calculado a partir de todas as sondas aplicáveis.</summary>
        public StatusDisponibilidade Status { get; set; }
        /// <summary>Origem provável do problema ou da indisponibilidade observada.</summary>
        public OrigemProvavelIndisponibilidade OrigemProvavel { get; set; }
        /// <summary>Mensagem em português simples, pronta para ser exibida ao usuário do sistema.</summary>
        /// <remarks>Evita que cada aplicação precise traduzir códigos técnicos de rede ou da SEFAZ.</remarks>
        public string Descricao => DescricaoDisponibilidade.Obter(this);
        /// <summary>Lista detalhada das evidências usadas para chegar ao estado agregado.</summary>
        public ColecaoResultadoSondaDisponibilidade Sondas { get; private set; }
    }

    /// <summary>
    /// Converte um resultado técnico em uma mensagem curta e compreensível.
    /// </summary>
    /// <remarks>A aplicação consumidora deve usar a propriedade <see cref="ResultadoDiagnosticoDisponibilidade.Descricao"/>.</remarks>
    internal static class DescricaoDisponibilidade
    {
        /// <summary>Obtém a mensagem adequada para o estado e a origem informados.</summary>
        /// <param name="resultado">Resultado que será convertido em texto.</param>
        /// <returns>Mensagem simples ou texto vazio quando o resultado for nulo.</returns>
        internal static string Obter(ResultadoDiagnosticoDisponibilidade resultado)
        {
            if (resultado == null)
            {
                return string.Empty;
            }

            if (resultado.OrigemProvavel == OrigemProvavelIndisponibilidade.ConsumoIndevido)
            {
                return "A SEFAZ limitou temporariamente as consultas por excesso de consumo. Aguarde antes de tentar novamente.";
            }

            if (resultado.OrigemProvavel == OrigemProvavelIndisponibilidade.AmbienteLocal)
            {
                if (PossuiFalha(resultado, TipoFalhaDisponibilidade.Certificado))
                {
                    return "Não foi possível acessar a SEFAZ porque o certificado digital não está disponível ou precisa ser revisado.";
                }

                if (PossuiFalha(resultado, TipoFalhaDisponibilidade.Configuracao))
                {
                    return "Não foi possível verificar a SEFAZ porque a configuração do sistema precisa ser revisada.";
                }

                return "Há indícios de um problema na conexão deste computador com a SEFAZ. Verifique a internet e a rede local.";
            }

            switch (resultado.Status)
            {
                case StatusDisponibilidade.Operacional:
                    return "Os serviços da SEFAZ estão funcionando normalmente.";

                case StatusDisponibilidade.Degradado:
                    return "Os serviços da SEFAZ estão respondendo, mas apresentam lentidão ou instabilidade.";

                case StatusDisponibilidade.ParcialmenteIndisponivel:
                    return "Alguns serviços da SEFAZ estão indisponíveis, enquanto outros continuam funcionando.";

                case StatusDisponibilidade.Indisponivel:
                    return resultado.OrigemProvavel == OrigemProvavelIndisponibilidade.AutoridadeFiscal
                        ? "Há indícios de indisponibilidade nos serviços da SEFAZ. Tente novamente mais tarde."
                        : "O serviço não está disponível no momento, mas não foi possível determinar a origem do problema.";

                case StatusDisponibilidade.NaoAplicavel:
                    return "Este diagnóstico não se aplica ao documento, ambiente ou local configurado.";

                default:
                    return "Ainda não há informações suficientes para determinar se os serviços da SEFAZ estão disponíveis.";
            }
        }

        /// <summary>Verifica se alguma sonda possui a categoria de falha informada.</summary>
        /// <param name="resultado">Resultado cujas sondas serão consultadas.</param>
        /// <param name="tipoFalha">Categoria procurada.</param>
        /// <returns><see langword="true"/> quando a categoria existir; caso contrário, <see langword="false"/>.</returns>
        private static bool PossuiFalha(ResultadoDiagnosticoDisponibilidade resultado, TipoFalhaDisponibilidade tipoFalha)
        {
            for (var i = 0; i < resultado.Sondas.Count; i++)
            {
                if (resultado.Sondas.GetItem(i).TipoFalha == tipoFalha)
                {
                    return true;
                }
            }

            return false;
        }
    }
}
