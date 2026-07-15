using System;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Despacha os segmentos de totais da nota, incluindo os grupos complementares de IBS e CBS.
    /// </summary>
    internal sealed class NFeTxtTotalSegmentDispatcher
    {
        private readonly Dictionary<string, Action<int, int>> acoes;

        internal NFeTxtTotalSegmentDispatcher(Dictionary<string, Action<int, int>> acoes)
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
