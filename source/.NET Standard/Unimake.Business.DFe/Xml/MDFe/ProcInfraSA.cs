#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.MDFe
{
    /// <summary>
    /// Grupo de informações do compartilhamento do MDFe com InfraSA para geração do DTe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ProcInfraSA")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    [XmlRoot("procInfraSA", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class ProcInfraSA : XMLBase
    {
        /// <summary>
        /// Número do Protocolo de Geração do DTe
        /// </summary>
        [XmlElement("nProtDTe")]
        public string NProtDTe { get; set; }

        /// <summary>
        /// Data e hora de geração do protocolo, no formato AAAA-MM-DDTHH:MM:SS TZD.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhProt { get; set; }
#else
        public DateTimeOffset DhProt { get; set; }
#endif

        /// <summary>
        /// Auxiliar da propriedade DhProt para geração do XML. Atualize o valor da tag DhProt, não esta.
        /// </summary>
        [XmlElement("dhProt")]
        public string DhProtField
        {
            get => DhProt.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProt = DateTime.Parse(value);
#else
            set => DhProt = DateTimeOffset.Parse(value);
#endif
        }
    }
}
