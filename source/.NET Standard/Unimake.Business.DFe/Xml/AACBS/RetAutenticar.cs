using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Resultado da autenticação da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetAutenticar")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetAutenticarTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retAutenticar", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetAutenticar : RetornoAACBSBase
    {
        /// <summary>
        /// Token opaco de acesso. Seu conteúdo é sensível.
        /// </summary>
        [XmlElement("access_token")]
        public string AccessToken { get; set; }

        /// <summary>
        /// Tipo do token de acesso, com a grafia informada pelo autorizador.
        /// </summary>
        [XmlElement("token_type")]
        public string TokenType { get; set; }

        /// <summary>
        /// Prazo de validade informado na emissão do token, em segundos.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public int? ExpiresIn { get; set; }

        /// <summary>
        /// Representação inteira em XML. Ausência é representada por null, inclusive no INTEROP.
        /// </summary>
        [XmlElement("expires_in")]
        public string ExpiresInField
        {
            get => ExpiresIn.HasValue ? XmlConvert.ToString(ExpiresIn.Value) : null;
            set => ExpiresIn = value == null ? (int?)null : XmlConvert.ToInt32(value);
        }

        /// <summary>
        /// Escopo informado na resposta de autenticação.
        /// </summary>
        [XmlElement("scope")]
        public string Scope { get; set; }

        /// <summary>
        /// Código de erro OAuth.
        /// </summary>
        [XmlElement("error")]
        public string Error { get; set; }

        /// <summary>
        /// Descrição informativa do erro OAuth.
        /// </summary>
        [XmlElement("error_description")]
        public string ErrorDescription { get; set; }

        /// <summary>
        /// URI informativa do erro OAuth.
        /// </summary>
        [XmlElement("error_uri", DataType = "anyURI")]
        public string ErrorUri { get; set; }
    }
}
