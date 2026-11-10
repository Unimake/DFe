using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>Capacidades implementadas na DLL, sem executar rede ou carregar certificado.</summary>
    /// <remarks>Capacidade não comprova funcionamento. Configuração, ambiente, município, versão e provedor
    /// ainda podem impedir uma execução. Consulte também a limitação de cada observação efetiva.</remarks>
#if INTEROP
    [System.Runtime.InteropServices.ComVisible(false)]
#endif
    public sealed class CapacidadesDiagnosticoDisponibilidade
    {
        private readonly bool historicoLegado;
        /// <summary>Documento ou integração avaliada.</summary>
        public TipoDFe TipoDFe { get; }
        /// <summary>Há serviços remotos implementados que podem ser acompanhados por uma sessão opt-in.</summary>
        public bool ObservacaoPassiva { get; }
        /// <summary>Compatibilidade da API anterior; não deve orientar cobertura ou coleta do consumidor.</summary>
        [System.Obsolete("Use ObservacaoPassiva e o canal de sessões. O histórico anterior existe somente para compatibilidade de transição.")]
        public bool HistoricoLegado => historicoLegado;
        /// <summary>Há estratégia implementada de consulta oficial de status, com cache e bloqueio.</summary>
        public bool ConsultaOficialStatus { get; }
        /// <summary>O diagnóstico manual pode sondar infraestrutura do endpoint de status configurado.</summary>
        public bool InfraestruturaEndpointStatus => ConsultaOficialStatus;
        /// <summary>Limite da capacidade; não contém dados da configuração.</summary>
        public string Limitacao { get; }

        /// <summary>Obtém capacidades do documento sem iniciar operações remotas.</summary>
        /// <param name="tipoDFe">Tipo de documento ou integração.</param>
        public CapacidadesDiagnosticoDisponibilidade(TipoDFe tipoDFe)
        {
            TipoDFe = tipoDFe;
            ObservacaoPassiva = tipoDFe != TipoDFe.Desconhecido && tipoDFe != TipoDFe.SAT &&
                tipoDFe != TipoDFe.CFe && tipoDFe != TipoDFe.SNCM &&
                System.Enum.IsDefined(typeof(TipoDFe), tipoDFe);
            historicoLegado = TelemetriaDisponibilidade.SuportaDocumento(tipoDFe);
            ConsultaOficialStatus = DiagnosticoDisponibilidadeDFe.PossuiConsultaOficialStatus(tipoDFe);
            Limitacao = !ObservacaoPassiva ? "Não há serviço remoto implementado para este tipo na biblioteca; utilitários externos são identificados separadamente." :
                ConsultaOficialStatus ? "Status não confirma autorização, eventos, downloads ou recuperação de outro serviço. A configuração efetiva pode não possuir endpoint." :
                "Sem estratégia oficial de status registrada. Use evidências das operações reais; ausência de evidência permanece inconclusiva.";
        }
    }
}
