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
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.uMessengerSendTextMessage")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("uMessenger", IsNullable = false)]
    public class uMessengerSendTextMessage : XMLBase
    {
        [XmlAttribute("versao")]
        public string Versao { get; set; } = "1.00";

        [XmlElement("SendTextMessage")]
        public List<SendTextMessageContent> SendTextMessage { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.SendTextMessageContent")]
    [ComVisible(true)]
#endif
    public class SendTextMessageContent
    {
        [XmlAttribute("Id")]
        public string Id { get; set; }

        [XmlElement("InstanceName")]
        public string InstanceName { get; set; }

        [XmlElement("To")]
        public string To { get; set; }

        [XmlElement("Text")]
        public string Text { get; set; }

        [XmlElement("Testing")]
        public bool Testing { get; set; }

        [XmlElement("UseHomologServer")]
        public bool UseHomologServer { get; set; }

        [XmlIgnore]
        public bool UseHomologServerSpecified { get; set; }

        [XmlElement("Files")]
        public SendTextMessageFiles Files { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.SendTextMessageFiles")]
    [ComVisible(true)]
#endif
    public class SendTextMessageFiles
    {
        [XmlElement("File")]
        public List<SendTextMessageFile> File { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.UMessenger.SendTextMessageFile")]
    [ComVisible(true)]
#endif
    public class SendTextMessageFile
    {
        [XmlElement("FullPath")]
        public string FullPath { get; set; }

        [XmlElement("Description")]
        public string Description { get; set; }

        [XmlElement("MediaType")]
        public int MediaType { get; set; }

        [XmlIgnore]
        public bool MediaTypeSpecified { get; set; }
    }
}
