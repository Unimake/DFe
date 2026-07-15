using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Despacha os segmentos de produto, complementos e impostos do item da nota.
    /// </summary>
    internal sealed class NFeTxtDetailSegmentDispatcher
    {
        private readonly Dictionary<string, Action<int, int>> acoes;

        internal NFeTxtDetailSegmentDispatcher(Dictionary<string, Action<int, int>> acoes)
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
