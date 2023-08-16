#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Collections.Generic;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1070")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt1050TabLig/v2_01_02", IsNullable = false)]
    public class Reinf1070 : XMLBase
    {
        [XmlElement("evtTabProcesso")]
        public EvtTabProcesso EvtTabProcesso { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTabProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtTabProcesso : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoContri")]
        public InfoContri InfoContri { get; set; }

        [XmlElement("infoProcesso")]
        public InfoProcesso InfoProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcesso
    {
        [XmlElement("inclusao")]
        public Reinf1070Inclusao Reinf1070Inclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1070Inclusao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf1070Inclusao
    {
        [XmlElement("ideProcesso")]
        public IdeProcesso IdeProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeProcesso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeProcesso
    {

    }

}
