#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>Estado aferido para uma evidência ou para o diagnóstico agregado.</summary>
    public enum StatusDisponibilidade
    {
        /// <summary>O serviço respondeu normalmente.</summary>
        Operacional = 0,
        /// <summary>Há lentidão, oscilação ou limitação de consumo.</summary>
        Degradado = 1,
        /// <summary>Somente parte dos serviços está indisponível.</summary>
        ParcialmenteIndisponivel = 2,
        /// <summary>O serviço está indisponível.</summary>
        Indisponivel = 3,
        /// <summary>Não existem evidências suficientes.</summary>
        Inconclusivo = 4,
        /// <summary>O diagnóstico não se aplica à configuração.</summary>
        NaoAplicavel = 5
    }

    /// <summary>Categoria técnica da falha observada.</summary>
    public enum TipoFalhaDisponibilidade
    {
        /// <summary>Nenhuma falha.</summary>
        Nenhuma = 0,
        /// <summary>Falha de resolução de nomes.</summary>
        DNS = 1,
        /// <summary>Falha de conexão TCP.</summary>
        Conexao = 2,
        /// <summary>Tempo limite excedido.</summary>
        Timeout = 3,
        /// <summary>Falha de TLS ou canal seguro.</summary>
        TLS = 4,
        /// <summary>Falha atribuída ao proxy.</summary>
        Proxy = 5,
        /// <summary>Resposta HTTP de erro.</summary>
        HTTP = 6,
        /// <summary>Resposta SOAP ou fiscal inválida.</summary>
        Protocolo = 7,
        /// <summary>Certificado local ausente ou inválido.</summary>
        Certificado = 8,
        /// <summary>Configuração local inválida.</summary>
        Configuracao = 9,
        /// <summary>Consumo indevido informado pelo autorizador.</summary>
        ConsumoIndevido = 10,
        /// <summary>Falha sem classificação específica.</summary>
        Desconhecida = 11
    }

    /// <summary>Origem provável do estado observado.</summary>
    public enum OrigemProvavelIndisponibilidade
    {
        /// <summary>Nenhum problema observado.</summary>
        Nenhuma = 0,
        /// <summary>Problema provável no ambiente local.</summary>
        AmbienteLocal = 1,
        /// <summary>Indisponibilidade declarada ou demonstrada pela autoridade fiscal.</summary>
        AutoridadeFiscal = 2,
        /// <summary>Serviços com estados diferentes.</summary>
        Parcial = 3,
        /// <summary>Origem indeterminada.</summary>
        Indeterminada = 4,
        /// <summary>O contribuinte está limitado por consumo indevido.</summary>
        ConsumoIndevido = 5
    }

    /// <summary>Origem da evidência utilizada pelo diagnóstico.</summary>
    public enum FonteEvidenciaDisponibilidade
    {
        /// <summary>Execução fiscal real observada sem tráfego adicional.</summary>
        TelemetriaPassiva = 0,
        /// <summary>Diagnóstico local de DNS, TCP, TLS ou proxy.</summary>
        Infraestrutura = 1,
        /// <summary>Consulta explícita e controlada de status.</summary>
        StatusServico = 2
    }

    /// <summary>Opções do diagnóstico de disponibilidade.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ConfiguracaoDiagnosticoDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ConfiguracaoDiagnosticoDisponibilidade
    {
        private readonly List<string> servicos = new List<string>();

        /// <summary>Cria as opções com padrões conservadores.</summary>
        public ConfiguracaoDiagnosticoDisponibilidade()
        {
            TimeoutMilissegundos = 10000;
            LimiteLentidaoMilissegundos = 3000;
            JanelaEvidenciaMinutos = 15;
            CacheInfraestruturaSegundos = 60;
            IntervaloMinimoStatusMinutos = 5;
        }

        /// <summary>Tempo limite dos testes locais.</summary>
        public int TimeoutMilissegundos { get; set; }

        /// <summary>Duração a partir da qual uma resposta é considerada lenta.</summary>
        public int LimiteLentidaoMilissegundos { get; set; }

        /// <summary>Janela das evidências passivas consideradas no resultado.</summary>
        public int JanelaEvidenciaMinutos { get; set; }

        /// <summary>Validade do cache de infraestrutura.</summary>
        public int CacheInfraestruturaSegundos { get; set; }

        /// <summary>Intervalo mínimo entre consultas explícitas de status. O mínimo aceito é cinco minutos.</summary>
        public int IntervaloMinimoStatusMinutos { get; set; }

        /// <summary>Quantidade de serviços selecionados explicitamente.</summary>
        public int ServicosCount => servicos.Count;

        /// <summary>Adiciona um serviço à seleção.</summary>
        /// <param name="nomeServico">Nome do serviço.</param>
        public void AddServico(string nomeServico)
        {
            if (!string.IsNullOrWhiteSpace(nomeServico) && !servicos.Contains(nomeServico))
            {
                servicos.Add(nomeServico);
            }
        }

        /// <summary>Limpa a seleção de serviços.</summary>
        public void ClearServicos() => servicos.Clear();

        /// <summary>Obtém o serviço no índice informado.</summary>
        /// <param name="index">Índice iniciado em zero.</param>
        /// <returns>Nome do serviço.</returns>
        public string GetServico(int index) => servicos[index];

        internal bool AceitaServico(string servico) => servicos.Count == 0 || servicos.Contains(servico);

        internal void Validar()
        {
            if (TimeoutMilissegundos <= 0) throw new ArgumentOutOfRangeException(nameof(TimeoutMilissegundos));
            if (LimiteLentidaoMilissegundos <= 0) throw new ArgumentOutOfRangeException(nameof(LimiteLentidaoMilissegundos));
            if (JanelaEvidenciaMinutos <= 0) throw new ArgumentOutOfRangeException(nameof(JanelaEvidenciaMinutos));
            if (CacheInfraestruturaSegundos <= 0) throw new ArgumentOutOfRangeException(nameof(CacheInfraestruturaSegundos));
            if (IntervaloMinimoStatusMinutos < 5) throw new ArgumentOutOfRangeException(nameof(IntervaloMinimoStatusMinutos));
        }
    }

    /// <summary>Resultado de uma evidência de disponibilidade.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ResultadoSondaDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ResultadoSondaDisponibilidade
    {
        /// <summary>Nome do serviço ou etapa de infraestrutura.</summary>
        public string Servico { get; set; }
        /// <summary>Endpoint sem credenciais, consulta ou fragmento.</summary>
        public string Endpoint { get; set; }
        /// <summary>Protocolo observado.</summary>
        public string Protocolo { get; set; }
        /// <summary>Fonte da evidência.</summary>
        public FonteEvidenciaDisponibilidade Fonte { get; set; }
        /// <summary>Data e hora da observação.</summary>
        public DateTime DataHora { get; set; }
        /// <summary>Idade da observação em segundos.</summary>
        public long IdadeSegundos { get; set; }
        /// <summary>Estado aferido.</summary>
        public StatusDisponibilidade Status { get; set; }
        /// <summary>Categoria da falha.</summary>
        public TipoFalhaDisponibilidade TipoFalha { get; set; }
        /// <summary>Duração em milissegundos.</summary>
        public long DuracaoMilissegundos { get; set; }
        /// <summary>Status HTTP, ou zero quando ausente.</summary>
        public int HttpStatusCode { get; set; }
        /// <summary>Status fiscal, ou zero quando ausente.</summary>
        public int CStat { get; set; }
        /// <summary>Motivo sanitizado ou gerado pelo diagnóstico.</summary>
        public string XMotivo { get; set; }
        /// <summary>Resumo sanitizado da exceção.</summary>
        public string Excecao { get; set; }
        /// <summary>Indica que o resultado foi reutilizado do cache.</summary>
        public bool DoCache { get; set; }
        /// <summary>Indica serviço essencial para agregação.</summary>
        public bool Essencial { get; set; }
    }

    /// <summary>Coleção de resultados compatível com COM.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ColecaoResultadoSondaDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ColecaoResultadoSondaDisponibilidade
    {
        private readonly List<ResultadoSondaDisponibilidade> itens = new List<ResultadoSondaDisponibilidade>();
        /// <summary>Quantidade de itens.</summary>
        public int Count => itens.Count;
        /// <summary>Obtém um item pelo índice iniciado em zero.</summary>
        /// <param name="index">Índice do item.</param>
        /// <returns>Resultado armazenado.</returns>
        public ResultadoSondaDisponibilidade GetItem(int index) => itens[index];
        internal IList<ResultadoSondaDisponibilidade> Itens => itens;
        internal void Add(ResultadoSondaDisponibilidade item) => itens.Add(item);
    }

    /// <summary>Resultado agregado do diagnóstico.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.ResultadoDiagnosticoDisponibilidade")]
    [ComVisible(true)]
#endif
    public class ResultadoDiagnosticoDisponibilidade
    {
        /// <summary>Cria o resultado e sua coleção.</summary>
        public ResultadoDiagnosticoDisponibilidade() => Sondas = new ColecaoResultadoSondaDisponibilidade();
        /// <summary>Documento fiscal diagnosticado.</summary>
        public TipoDFe TipoDFe { get; set; }
        /// <summary>UF diagnosticada.</summary>
        public UFBrasil UFBrasil { get; set; }
        /// <summary>Ambiente diagnosticado.</summary>
        public TipoAmbiente TipoAmbiente { get; set; }
        /// <summary>Início do diagnóstico.</summary>
        public DateTime Inicio { get; set; }
        /// <summary>Duração total em milissegundos.</summary>
        public long DuracaoTotalMilissegundos { get; set; }
        /// <summary>Estado agregado.</summary>
        public StatusDisponibilidade Status { get; set; }
        /// <summary>Origem provável.</summary>
        public OrigemProvavelIndisponibilidade OrigemProvavel { get; set; }
        /// <summary>Descrição não técnica do resultado, pronta para apresentação ao usuário.</summary>
        public string Descricao => DescricaoDisponibilidade.Obter(this);
        /// <summary>Evidências utilizadas.</summary>
        public ColecaoResultadoSondaDisponibilidade Sondas { get; private set; }
    }

    internal static class DescricaoDisponibilidade
    {
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
