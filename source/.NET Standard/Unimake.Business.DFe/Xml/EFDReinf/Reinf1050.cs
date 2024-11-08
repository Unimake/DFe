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
    /// R-1050 - Tabela de entidades ligadas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Reinf1050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("Reinf", Namespace = "http://www.reinf.esocial.gov.br/schemas/evt1050TabLig/v2_01_02", IsNullable = false)]
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
        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
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
        public Inclusao1050 Inclusao { get; set; }

        [XmlElement("alteracao")]
        public Alteracao1050 Alteracao { get; set; }

        [XmlElement("exclusao")]
        public Exclusao1050 Exclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Inclusao1050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Inclusao1050
    {
        [XmlElement("ideEntLig")]
        public IdeEntLig IdeEntLig { get; set; }
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

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Alteracao1050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Alteracao1050
    {
        [XmlElement("ideEntLig")]
        public IdeEntLig IdeEntLig { get; set; }

        [XmlElement("novaValidade")]
        public NovaValidade1050 NovaValidade { get; set; }
    }

    /// <summary>
    /// Novo período de validade das informações que
    /// estão sendo alteradas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.NovaValidade1050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class NovaValidade1050
    {
        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.Exclusao1050")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class Exclusao1050
    {
        [XmlElement("ideEntLig")]
        public IdeEntLigExclusao IdeEntLig { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEntLigExclusao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEntLigExclusao
    {
        [XmlElement("cnpjLig")]
        public string CnpjLig { get; set; }

        [XmlElement("iniValid")]
        public string IniValid { get; set; }

        [XmlElement("fimValid")]
        public string FimValid { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeFimValid() => !string.IsNullOrEmpty(FimValid);

        #endregion ShouldSerialize
    }
}
