using System;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe para compactação e descompactação de objetos, strings, etc...
    /// </summary>
    public static class Compress
    {
        /// <summary>
        /// Descompactador padrão GZIP
        /// </summary>
        /// <param name="input">Conteúdo a ser descompactado</param>
        /// <returns>Retorna o conteúdo descompactado</returns>
        public static string GZIPDecompress(string input)
        {
            var enc = input.ToCharArray();
            var dec = Convert.FromBase64CharArray(enc, 0, enc.Length);

            var encodedDataAsBytes = Convert.FromBase64String(input);
            using (System.IO.Stream comp = new System.IO.MemoryStream(encodedDataAsBytes))
            {
                using (System.IO.Stream decomp = new System.IO.Compression.GZipStream(comp, System.IO.Compression.CompressionMode.Decompress, false))
                {
                    using (var sr = new System.IO.StreamReader(decomp))
                    {
                        return sr.ReadToEnd();
                    }
                }
            }
        }
    }
}
