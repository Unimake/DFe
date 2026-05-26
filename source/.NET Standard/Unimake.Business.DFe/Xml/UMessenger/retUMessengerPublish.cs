#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.UMessenger
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.retUMessengerPublish")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("retUMessenger", IsNullable = false)]
    public class retUMessengerPublish : XMLBase
    {
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";

        [XmlElement("MessageId")]
        public string MessageId { get; set; }

        [XmlElement("LocalId")]
        public string LocalId { get; set; }

        [XmlElement("RawResponse")]
        public string RawResponse { get; set; }
    }
}
