#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2099")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtFechamento/v2_01_02", IsNullable = false)]
    public class Reinf2099 : XMLBase
    {
        [XmlElement("evtFechaEvPer")]
        public EvtFechaEvPer EvtFechaEvPer { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtFechaEvPer")]
    [ComVisible(true)]
#endif
    public class EvtFechaEvPer : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEventoReinf2099 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRespInf")]
        public IdeRespInf IdeRespInf { get; set; }

        [XmlElement("infoFech")]
        public InfoFech InfoFech { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEventoReinf2099")]
    [ComVisible(true)]
#endif
    public class IdeEventoReinf2099 : IdeEventoReinf2098 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRespInf")]
    [ComVisible(true)]
#endif
    public class IdeRespInf
    {
        [XmlElement("nmResp")]
        public string NmResp { get; set; }

        [XmlElement("cpfResp")]
        public string CpfResp { get; set; }

        [XmlElement("telefone")]
        public string Telefone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeTelefone() => !string.IsNullOrEmpty(Telefone);

        public bool ShouldSereializeEmail() => !string.IsNullOrEmpty(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoFech")]
    [ComVisible(true)]
#endif
    public class InfoFech
    {
        [XmlElement("evtServTm")]
        public SimNaoLetra EvtServTm { get; set; }

        [XmlElement("evtServPr")]
        public SimNaoLetra EvtServPr { get; set; }

        [XmlElement("evtAssDespRec")]
        public SimNaoLetra EvtAssDespRec { get; set; }

        [XmlElement("evtAssDespRep")]
        public SimNaoLetra EvtAssDespRep { get; set; }

        [XmlElement("evtComProd")]
        public SimNaoLetra EvtComProd { get; set; }

        [XmlElement("evtCPRB")]
        public SimNaoLetra EvtCPRB { get; set; }

        [XmlElement("evtAquis")]
        public SimNaoLetra EvtAquis { get; set; }
    }
}
