using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Abertura de solicitação de consulta de créditos da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.ConsultarCreditos")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "ConsultarCreditosTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("consultarCreditos", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class ConsultarCreditos : AACBSBase
    {
        /// <summary>
        /// CNPJ base da matriz, com oito dígitos.
        /// </summary>
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// URL HTTPS opcional para receber a notificação de conclusão.
        /// </summary>
        [XmlElement("urlRetorno")]
        public string UrlRetorno { get; set; }
    }
}
