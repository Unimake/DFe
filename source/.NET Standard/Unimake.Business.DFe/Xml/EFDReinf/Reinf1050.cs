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
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1050")]
    [ComVisible(true)]
#endif

    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evtTabProcesso/v2_01_02", IsNullable = false)]
    public class Reinf1050 : XMLBase
    {
        [XmlElement("evtTabLig")]
        public EvtTabLig EvtTabLig { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EvtTabLig")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class EvtTabLig : ReinfEventoBase
    {
        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        [XmlElement("ideContri")]
        public IdeContri IdeContri { get; set; }

        [XmlElement("infoLig")]
        public InfoLig InfoLig { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoLig")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoLig
    {
        [XmlElement("inclusao")]
        public Reinf1050Inclusao Inclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1050Inclusao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf1050Inclusao
    {
        [XmlElement("ideEntLig")]
        public IdeEntLig IdeEntLig { get; set; }

        [XmlElement("alteracao")]
        public Reinf1050Alteracao Alteracao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEntLig")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEntLig
    {
        [XmlElement("tpEntLig")]
        public TipoEntidadeLigada TpEntLig { get; set; }

        [XmlElement("cnpjLig")]
        public string CnpjLig { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        [XmlElement("exclusao")]
        public Reinf1050Exclusao Exclusao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Alteracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf1050Alteracao
    {
        [XmlElement("tpEntLig")]
        public TipoEntidadeLigada TpEntLig { get; set; }

        [XmlElement("cnpjLig")]
        public string CnpjLig { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        [XmlElement("novaValidade")]
        public NovaValidade NovaValidade { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NovaValidade")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class NovaValidade
    {
        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Exclusao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Reinf1050Exclusao
    {
        [XmlElement("cnpjLig")]
        public string CnpjLig { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion
    }
}
