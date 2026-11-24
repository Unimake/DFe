using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Consulta da situação de uma solicitação da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ConsultarSituacaoSolicitacao")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "ConsultarSituacaoSolicitacaoTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("consultarSituacaoSolicitacao", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class ConsultarSituacaoSolicitacao : AACBSBase
    {
        /// <summary>
        /// Tíquete que identifica a solicitação.
        /// </summary>
        [XmlElement("tiqueteSolicitacao")]
        public string TiqueteSolicitacao { get; set; }
    }
}
