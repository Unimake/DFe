#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Xml;

namespace Unimake.Business.DFe.Utility
{
    /// <summary>
    /// Classe para compactação e descompactação de objetos, strings, etc...
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Utility.Compress")]
    [ComVisible(true)]
#endif
    public static class Compress
    {
        /// <summary>
        /// Compactar XMLDocument com GZIP
        /// </summary>
        /// <param name="conteudoXML">XML a ser compactado</param>
        /// <returns>String do conteúdo compactado</returns>
        public static string GZIPCompress(XmlDocument conteudoXML) => GZIPCompress(conteudoXML.InnerXml);

        /// <summary>
        /// Compactar string com GZIP
        /// </summary>
        /// <param name="conteudoXML">Conteúdo a ser compactado</param>
        /// <returns>String do conteúdo compactado</returns>
        public static string GZIPCompress(string conteudoXML)
        {
            var value = conteudoXML;

            var buffer = Encoding.UTF8.GetBytes(value);
            var ms = new MemoryStream();
            using(var zip = new GZipStream(ms, CompressionMode.Compress))
            {
                zip.Write(buffer, 0, buffer.Length);
            }

            return Convert.ToBase64String(ms.GetBuffer());
        }


        /// <summary>
        /// Descompactador string padrão GZIP
        /// </summary>
        /// <param name="input">Conteúdo a ser descompactado</param>
        /// <returns>Retorna uma string com o conteúdo descompactado</returns>
        public static string GZIPDecompress(string input)
        {
            //var enc = input.ToCharArray();
            //var dec = Convert.FromBase64CharArray(enc, 0, enc.Length);

            var encodedDataAsBytes = Convert.FromBase64String(input);
            using(Stream comp = new MemoryStream(encodedDataAsBytes))
            {
                using(Stream decomp = new GZipStream(comp, CompressionMode.Decompress, false))
                {
                    using(var sr = new StreamReader(decomp))
                    {
                        return sr.ReadToEnd();
                    }
                }
            }
        }
    }
}
