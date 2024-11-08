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
    /// R-2099 - Fechamento dos eventos da série R-2000
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf2099")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtFechamento/v2_01_02", IsNullable = false)]
    public class Reinf2099 : XMLBase
    {
        /// <summary>
        /// Evento de fechamento 
        /// </summary>
        [XmlElement("evtFechaEvPer")]
        public EvtFechaEvPer EvtFechaEvPer { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento de fechamento 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtFechaEvPer")]
    [ComVisible(true)]
#endif
    public class EvtFechaEvPer : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento2099 IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("ideRespInf")]
        public IdeRespInf2099 IdeRespInf { get; set; }

        [XmlElement("infoFech")]
        public InfoFech InfoFech { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento2099")]
    [ComVisible(true)]
#endif
    public class IdeEvento2099 : IdeEvento2098 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeRespInf2099")]
    [ComVisible(true)]
#endif
    public class IdeRespInf2099
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

        public bool ShouldSerializeTelefone() => !string.IsNullOrEmpty(Telefone);

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);

        #endregion ShouldSerialize
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
