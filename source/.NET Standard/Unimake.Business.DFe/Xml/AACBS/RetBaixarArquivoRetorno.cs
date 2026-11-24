using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Envelope com um arquivo de retorno da AACBS ou um erro local.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetBaixarArquivoRetorno")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetBaixarArquivoRetornoTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retBaixarArquivoRetorno", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetBaixarArquivoRetorno : RetornoAACBSBase
    {
        /// <summary>
        /// Arquivo de retorno da consulta de débitos.
        /// </summary>
        [XmlElement("retArquivoDebitos")]
        public RetArquivoDebitos ArquivoDebitos { get; set; }

        /// <summary>
        /// Arquivo de retorno da consulta de créditos.
        /// </summary>
        [XmlElement("retArquivoCreditos")]
        public RetArquivoCreditos ArquivoCreditos { get; set; }

        /// <summary>
        /// Arquivo de retorno da consulta de pagamentos.
        /// </summary>
        [XmlElement("retArquivoPagamentos")]
        public RetArquivoPagamentos ArquivoPagamentos { get; set; }

        /// <summary>
        /// Arquivo de retorno da consulta de recolhimentos.
        /// </summary>
        [XmlElement("retArquivoRecolhimentos")]
        public RetArquivoRecolhimentos ArquivoRecolhimentos { get; set; }
    }
}
