#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.UMessenger
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.retUMessengerPublish")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("uMessengerResponse", IsNullable = false)]
    public class retUMessengerPublish : XMLBase
    {
        [XmlElement("Mensagem")]
        public List<retUMessengerMensagem> Mensagem { get; set; } = new List<retUMessengerMensagem>();
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.retUMessengerMensagem")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class retUMessengerMensagem
    {
        [XmlAttribute("Id")]
        public string Id { get; set; }

        [XmlElement("Status")]
        public int Status { get; set; }

        [XmlElement("Motivo")]
        public string Motivo { get; set; }

        [XmlElement("messageID")]
        public string MessageID { get; set; }

        [XmlElement("DLLVersao")]
        public string DLLVersao { get; set; }
    }
}
