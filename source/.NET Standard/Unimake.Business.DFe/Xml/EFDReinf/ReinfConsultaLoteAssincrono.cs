#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ReinfConsultaLoteAssincrono")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "", IsNullable = false)]
    public class ReinfConsultaLoteAssincrono : XMLBase
    {
        [XmlIgnore]
        public string Versao { get; set; } = "1.00.00";

        [XmlElement("ConsultaLoteAssincrono")]
        public ConsultaLoteAssincrono ConsultaLoteAssincrono { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.ConsultaLoteAssincrono")]
    [ComVisible(true)]
#endif
    public class ConsultaLoteAssincrono
    {
        [XmlElement("numeroProtocolo")]
        public string NumeroProtocolo { get; set; }
    }
}