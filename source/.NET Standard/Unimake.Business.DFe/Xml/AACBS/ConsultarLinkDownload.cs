using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Consulta do link para baixar o arquivo de uma solicitação da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ConsultarLinkDownload")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "ConsultarLinkDownloadTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("consultarLinkDownload", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class ConsultarLinkDownload : AACBSBase
    {
        /// <summary>
        /// Tíquete que identifica a solicitação.
        /// </summary>
        [XmlElement("tiqueteSolicitacao")]
        public string TiqueteSolicitacao { get; set; }
    }
}
