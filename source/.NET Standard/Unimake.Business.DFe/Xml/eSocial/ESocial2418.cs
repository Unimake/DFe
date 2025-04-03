#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2418 - Reativação de Benefício - Entes Públicos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2418")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReativBen/v_S_01_03_00", IsNullable = false)]
    public class ESocial2418 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Reativação de Benefício
        /// </summary>
        [XmlElement("evtReativBen")]
        public EvtReativBen EvtReativBen { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Reativação de Benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtReativBen")]
    [ComVisible(true)]
#endif
    public class EvtReativBen
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Informações de identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2418 IdeEvento { get; set; }

        /// <summary>
        /// Informações de identificação do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        /// Identificação do beneficiário e do benefício
        /// </summary>
        [XmlElement("ideBeneficio")]
        public IdeBeneficio2418 IdeBeneficio { get; set; }

        /// <summary>
        /// Informações da reativação do benefício
        /// </summary>
        [XmlElement("infoReativ")]
        public InfoReativ InfoReativ { get; set; }
    }

    /// <summary>
    /// Informações de identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2418")]
    [ComVisible(true)]
#endif
    public class IdeEvento2418 : IdeEvento2205 { }

    /// <summary>
    /// Identificação do beneficiário e do benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2418")]
    [ComVisible(true)]
#endif
    public class IdeBeneficio2418 : IdeBeneficio2416 { }

    /// <summary>
    /// Informações da reativação do benefício
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReativ")]
    [ComVisible(true)]
#endif
    public class InfoReativ
    {
        /// <summary>
        /// Informar a data da efetiva reativação do benefício
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEfetReativ { get; set; }
#else
        public DateTimeOffset DtEfetReativ { get; set; }
#endif

        [XmlElement("dtEfetReativ")]
        public string DtEfetReativField
        {
            get => DtEfetReativ.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEfetReativ = DateTime.Parse(value);
#else
            set => DtEfetReativ = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Data de início dos efeitos financeiros da reativação do benefício
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DtEfeito { get; set; }
#else
        public DateTimeOffset DtEfeito { get; set; }
#endif

        [XmlElement("dtEfeito")]
        public string DtEfeitoField
        {
            get => DtEfeito.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEfeito = DateTime.Parse(value);
#else
            set => DtEfeito = DateTimeOffset.Parse(value);
#endif
        }
    }
}
