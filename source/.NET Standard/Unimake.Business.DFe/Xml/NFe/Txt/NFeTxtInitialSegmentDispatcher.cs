using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Despacha os segmentos iniciais de identificação, emitente, nota avulsa e destinatário.
    /// </summary>
    internal sealed class NFeTxtInitialSegmentDispatcher
    {
        private readonly Dictionary<string, Action<int>> acoes;

        internal NFeTxtInitialSegmentDispatcher(Dictionary<string, Action<int>> acoes)
        {
            this.acoes = acoes;
        }

        internal bool TentarDespachar(string codigoSegmento, int quantidadePipes)
        {
            Action<int> acao;
            if (!acoes.TryGetValue(codigoSegmento, out acao)) return false;

            acao(quantidadePipes);
            return true;
        }
    }
}
