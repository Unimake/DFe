using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.NFe.Txt
{
    /// <summary>
    /// Mantem o estado transitório compartilhado entre a leitura e o processamento do TXT.
    /// </summary>
    internal sealed class NFeTxtConversionContext
    {
        internal NFeTxtConversionContext()
        {
            ConteudoPorNota = new Dictionary<int, List<string>>();
        }

        internal Dictionary<int, List<string>> ConteudoPorNota { get; set; }

        internal int LinhaLida { get; set; }

        internal void Reiniciar()
        {
            ConteudoPorNota.Clear();
            LinhaLida = 0;
        }
    }
}
