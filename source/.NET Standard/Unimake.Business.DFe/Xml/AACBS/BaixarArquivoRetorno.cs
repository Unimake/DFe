using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Solicitação de download do arquivo de retorno da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.BaixarArquivoRetorno")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "BaixarArquivoRetornoTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("baixarArquivoRetorno", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class BaixarArquivoRetorno : AACBSBase
    {
        /// <summary>
        /// Família do arquivo a ser baixado.
        /// </summary>
        [XmlElement("tipoConsulta")]
        public TipoConsultaAACBS TipoConsulta { get; set; }

        /// <summary>
        /// Indica se a propriedade TipoConsulta foi informada.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        public bool ShouldSerializeTipoConsulta() => TipoConsulta != 0;

        /// <summary>
        /// CNPJ base da matriz, com oito dígitos.
        /// </summary>
        [XmlElement("cnpj")]
        public string Cnpj { get; set; }

        /// <summary>
        /// Tíquete que identifica a solicitação.
        /// </summary>
        [XmlElement("tiqueteSolicitacao")]
        public string TiqueteSolicitacao { get; set; }

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
    }
}
