#if INTEROP
using System.Runtime.InteropServices;
#endif
using Unimake.Compression;
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
    public class Compress
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
        public static string GZIPCompress(string conteudoXML) => CompressionHelper.GZIPCompress(conteudoXML);


        /// <summary>
        /// Descompactador string padrão GZIP
        /// </summary>
        /// <param name="input">Conteúdo a ser descompactado</param>
        /// <returns>Retorna uma string com o conteúdo descompactado</returns>
        public static string GZIPDecompress(string input) => CompressionHelper.GZIPDecompress(input);
    }
}
