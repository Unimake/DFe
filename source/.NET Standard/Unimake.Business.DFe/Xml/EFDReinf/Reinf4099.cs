#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// R-4099 - Fechamento/reabertura dos eventos da série R-4000
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf4099")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt4099FechamentoDirf/v2_01_02", IsNullable = false)]
    public class Reinf4099 : XMLBase
    {
        /// <summary>
        /// Evento de fechamento
        /// </summary>
        [XmlElement("evtFech")]
        public EvtFech EvtFech { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento de fechamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtFech")]
    [ComVisible(true)]
#endif
    public class EvtFech : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento4099 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRespInf")]
        public IdeRespInf4099 IdeRespInf { get; set; }

        [XmlElement("infoFech")]
        public InfoFech4099 InfoFech { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento4099")]
    [ComVisible(true)]
#endif
    public class IdeEvento4099 : IdeEvento2099 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRespInf4099")]
    [ComVisible(true)]
#endif
    public class IdeRespInf4099 : IdeRespInf2099 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoFech4099")]
    [ComVisible(true)]
#endif
    public class InfoFech4099
    {
        [XmlElement("fechRet")]
        public FechamentoRetencao FechRet { get; set; }
    }
}
