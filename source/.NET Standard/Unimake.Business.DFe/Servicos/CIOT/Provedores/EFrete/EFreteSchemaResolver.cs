using System.Collections.Generic;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal static class EFreteSchemaResolver
    {
        private static readonly IReadOnlyDictionary<Servico, string> Schemas = new Dictionary<Servico, string>
        {
            { Servico.CIOTDeclaracaoOperacaoTransporte, "CIOT.EFrete.declaracaoOperacaoTransporteEFrete_v1.00.xsd" },
            { Servico.CIOTConsultarCIOTGerado, "CIOT.EFrete.consultarCIOTGeradoEFrete_v1.00.xsd" },
            { Servico.CIOTCancelamentoOperacaoTransporte, "CIOT.EFrete.cancelamentoOperacaoTransporteEFrete_v1.00.xsd" },
            { Servico.CIOTEncerramentoOperacaoTransporte, "CIOT.EFrete.encerramentoOperacaoTransporteEFrete_v1.00.xsd" },
            { Servico.CIOTConsultarSituacaoTransportador, "CIOT.EFrete.consultarSituacaoTransportadorEFrete_v1.00.xsd" },
            { Servico.CIOTConsultarFrotaTransportador, "CIOT.EFrete.consultarFrotaTransportadorEFrete_v1.00.xsd" }
        };

        internal static string ObterSchemaArquivo(Servico servico)
        {
            string schema;
            return Schemas.TryGetValue(servico, out schema) ? schema : null;
        }
    }
}
