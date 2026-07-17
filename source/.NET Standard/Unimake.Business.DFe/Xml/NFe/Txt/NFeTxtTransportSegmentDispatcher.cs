using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Despacha os segmentos do grupo de transporte da nota.
    /// </summary>
    internal sealed class NFeTxtTransportSegmentDispatcher
    {
        private readonly Dictionary<string, Action<int, int>> acoes;

        internal NFeTxtTransportSegmentDispatcher(Dictionary<string, Action<int, int>> acoes)
        {
            this.acoes = acoes;
        }

        internal bool TentarDespachar(string codigoSegmento, int numeroProduto, int quantidadePipes)
        {
            Action<int, int> acao;
            if (!acoes.TryGetValue(codigoSegmento, out acao)) return false;

            acao(numeroProduto, quantidadePipes);
            return true;
        }
    }
}
