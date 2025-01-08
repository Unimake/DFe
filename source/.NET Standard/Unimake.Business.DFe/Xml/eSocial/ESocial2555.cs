#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using System.Xml;

namespace Unimake.Business.DFe.Xml.ESocial
{
    /// <summary>
    /// S-2555 - Solicitação de Consolidação das Informações de Tributos Decorrentes de Processo Trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2555")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtConsolidContProc/v_S_01_02_00", IsNullable = false)]
    public class ESocial2555 : XMLBaseESocial
    {
        /// <summary>
        /// Evento Solicitação de Consolidação das Informações de Tributos Decorrentes de Processo Trabalhista
        /// </summary>
        [XmlElement("evtConsolidContProc")]
        public EvtConsolidContProc EvtConsolidContProc { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Evento Solicitação de Consolidação das Informações de Tributos Decorrentes de Processo Trabalhista
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtConsolidContProc")]
    [ComVisible(true)]
#endif
    public class EvtConsolidContProc
    {
        /// <summary>
        /// ID
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        /// <summary>
        /// Identificação do evento
        /// </summary>
        [XmlElement("ideEvento")]
        public IdeEvento2555 IdeEvento { get; set; }

        /// <summary>
        /// Informações do empregador
        /// </summary>
        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        /// <summary>
        ///  Identificação do processo
        /// </summary>
        [XmlElement("ideProc")]
        public IdeProc2255 IdeProc { get; set; }
    }

    /// <summary>
    /// Identificação do evento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeEvento2555")]
    [ComVisible(true)]
#endif
    public class IdeEvento2555 : IdeEvento { }

    /// <summary>
    /// Identificação do processo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeProc2255")]
    [ComVisible(true)]
#endif
    public class IdeProc2255
    {
        /// <summary>
        /// Número do processo trabalhista, da ata ou número de identificação da conciliação
        /// </summary>
        [XmlElement("nrProcTrab")]
        public string NrProcTrab { get; set; }

        /// <summary>
        /// Mês/ano em que é devida a obrigação de pagar a parcela prevista no acordo/sentença
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime PerApurPgto { get; set; }
#else
        public DateTimeOffset PerApurPgto { get; set; }
#endif

        /// <summary>
        /// Mês/ano em que é devida a obrigação de pagar a parcela prevista no acordo/sentença.
        /// </summary>
        [XmlElement("perApurPgto")]
        public string PerApurPgtoField
        {
            get => PerApurPgto.ToString("yyyy-MM");
#if INTEROP
            set => PerApurPgto = DateTime.Parse(value);
#else
            set => PerApurPgto = DateTimeOffset.Parse(value);
#endif
        }
    }
}
