using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Resultado da consulta do link para baixar o arquivo da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetConsultarLinkDownload")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetConsultarLinkDownloadTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retConsultarLinkDownload", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetConsultarLinkDownload : RetornoAACBSBase
    {
        /// <summary>
        /// URL assinada para acesso ao arquivo. O endereço pode conter parâmetros sensíveis.
        /// </summary>
        [XmlElement("urlAssinada", DataType = "anyURI")]
        public string UrlAssinada { get; set; }

        /// <summary>
        /// Instante de expiração da URL assinada.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public DateTimeOffset? UrlAssinadaExpiraEm
        {
            get => UrlAssinadaExpiraEmField == null ? (DateTimeOffset?)null : AACBSFormat.ParseInstant(UrlAssinadaExpiraEmField);
            set => UrlAssinadaExpiraEmField = value.HasValue ? AACBSFormat.FormatInstant(value.Value) : null;
        }

        /// <summary>
        /// Representação XML do instante, preservando a fração de segundo e o fuso informados.
        /// </summary>
        [XmlElement("urlAssinadaExpiraEm")]
        public string UrlAssinadaExpiraEmField { get; set; }

        /// <summary>
        /// Código de erro informado pela Receita Federal.
        /// </summary>
        [XmlElement("codigoErro")]
        public string CodigoErro { get; set; }

        /// <summary>
        /// Mensagem de erro informada pela Receita Federal.
        /// </summary>
        [XmlElement("mensagemErro")]
        public string MensagemErro { get; set; }
    }
}
