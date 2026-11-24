using System;
#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.AACBS
{
    /// <summary>
    /// Resultado da abertura da consulta de débitos da AACBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.AACBS.RetConsultarDebitos")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(TypeName = "RetConsultarDebitosTipo", Namespace = AACBSFormat.Namespace)]
    [XmlRoot("retConsultarDebitos", Namespace = AACBSFormat.Namespace, IsNullable = false)]
    public class RetConsultarDebitos : RetornoAACBSBase
    {
        /// <summary>
        /// Tíquete que identifica a solicitação.
        /// </summary>
        [XmlElement("tiqueteSolicitacao")]
        public string TiqueteSolicitacao { get; set; }

        /// <summary>
        /// Estimativa de atendimento em segundos. O valor -1 indica estimativa indisponível.
        /// </summary>
#if INTEROP
        [ComVisible(false)]
#endif
        [XmlIgnore]
        public int? TEASegundos { get; set; }

        /// <summary>
        /// Representação inteira em XML. Ausência é representada por null, inclusive no INTEROP.
        /// </summary>
        [XmlElement("tEASegundos")]
        public string TEASegundosField
        {
            get => TEASegundos.HasValue ? XmlConvert.ToString(TEASegundos.Value) : null;
            set => TEASegundos = value == null ? (int?)null : XmlConvert.ToInt32(value);
        }

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
