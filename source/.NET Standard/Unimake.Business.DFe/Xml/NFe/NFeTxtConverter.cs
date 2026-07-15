using System;
using Unimake.Business.DFe.Xml.NFe.Txt;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Converte arquivos TXT no layout 4.00 para XML de NFe ou NFCe.
    /// A conversão apenas lê o arquivo informado e mantém o resultado em memória.
    /// </summary>
    public sealed class NFeTxtConverter
    {
        /// <summary>
        /// Converte todas as NFe ou NFCe presentes no arquivo TXT informado.
        /// </summary>
        /// <param name="caminhoArquivo">Caminho completo do arquivo TXT de origem.</param>
        /// <returns>Resultado com os XMLs produzidos ou a mensagem de erro de compatibilidade.</returns>
        public NFeTxtConversaoResultado Converter(string caminhoArquivo)
        {
            if (string.IsNullOrWhiteSpace(caminhoArquivo))
            {
                throw new ArgumentException("O caminho do arquivo TXT deve ser informado.", nameof(caminhoArquivo));
            }

            return new NFeTxtLegacyConverter().Converter(caminhoArquivo);
        }
    }
}
