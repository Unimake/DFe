using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Business.DFe.Xml.NFe.Txt;

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Converte arquivos TXT no layout 4.00 para XML de NFe ou NFCe.
    /// A conversão apenas lê o arquivo informado e mantém o resultado em memória.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFeTxtConverter")]
    [ComVisible(true)]
#endif
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

            return new NFeTxtConversionEngine().Converter(caminhoArquivo);
        }
    }
}
