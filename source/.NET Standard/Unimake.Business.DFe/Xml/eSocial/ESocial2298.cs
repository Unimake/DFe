#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial2298")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtReintegr/v_S_01_02_00", IsNullable = false)]
    public class ESocial2298 : XMLBase
    {
        [XmlElement("evtReintegr")]
        public EvtReintegr EvtReintegr { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtReintegr")]
    [ComVisible(true)]
#endif
    public class EvtReintegr
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEventoESocial2205 IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("ideVinculo")]
        public IdeVinculo IdeVinculo { get; set; }

        [XmlElement("infoReintegr")]
        public InfoReintegr InfoReintegr { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoReintegr")]
    [ComVisible(true)]
#endif
    public class InfoReintegr
    {
        [XmlElement("tpReint")]
        public TpReint TpReint { get; set; }

        [XmlElement("nrProcJud")]
        public string NrProcJud { get; set; }

        [XmlElement("nrLeiAnistia")]
        public string NrLeiAnistia { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DtEfetRetorno { get; set; }
#else
        public DateTimeOffset DtEfetRetorno { get; set; }
#endif

        [XmlElement("dtEfetRetorno")]
        public string DtEfetRetornoField
        {
            get => DtEfetRetorno.ToString("yyyy-MM-dd");
#if INTEROP
            set => DtEfetRetorno = DateTime.Parse(value);
#else
            set => DtEfetRetorno = DateTimeOffset.Parse(value);
#endif
        }

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

        #region ShouldSerialize

        public bool ShouldSerializeNrProcJudField() => !string.IsNullOrEmpty(NrProcJud);
     
        public bool ShouldSerializeNrLeiAnistiaField() => !string.IsNullOrEmpty(NrLeiAnistia);

        #endregion
    }
}
