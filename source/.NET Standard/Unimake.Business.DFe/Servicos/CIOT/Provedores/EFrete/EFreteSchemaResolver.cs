using System.Collections.Generic;

namespace Unimake.Business.DFe.Servicos.CIOT.Provedores.EFrete
{
    internal static class EFreteSchemaResolver
    {
        private const string SchemaEFrete = "CIOT.EFrete.ciotEFrete_v1.00.xsd";

        private static readonly IReadOnlyDictionary<Servico, string> Schemas = new Dictionary<Servico, string>
        {
            { Servico.CIOTDeclaracaoOperacaoTransporte, SchemaEFrete },
            { Servico.CIOTConsultarCIOTGerado, SchemaEFrete },
            { Servico.CIOTCancelamentoOperacaoTransporte, SchemaEFrete },
            { Servico.CIOTEncerramentoOperacaoTransporte, SchemaEFrete },
            { Servico.CIOTConsultarSituacaoTransportador, SchemaEFrete },
            { Servico.CIOTConsultarFrotaTransportador, SchemaEFrete }
        };

        internal static string ObterSchemaArquivo(Servico servico)
        {
            string schema;
            return Schemas.TryGetValue(servico, out schema) ? schema : null;
        }
    }
}
