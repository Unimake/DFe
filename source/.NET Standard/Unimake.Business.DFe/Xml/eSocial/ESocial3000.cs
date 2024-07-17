#pragma warning disable CS1591

using System;
using System.Runtime.InteropServices;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.ESocial
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.ESocial3000")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("eSocial", Namespace = "http://www.esocial.gov.br/schema/evt/evtExclusao/v_S_01_02_00", IsNullable = false)]
    public class ESocial3000 : XMLBase
    {
        [XmlElement("evtExclusao")]
        public EvtExclusao EvtExclusao { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.EvtExclusao")]
    [ComVisible(true)]
#endif
    public class EvtExclusao
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string ID { get; set; }

        [XmlElement("ideEvento")]
        public IdeEvento IdeEvento { get; set; }

        [XmlElement("ideEmpregador")]
        public IdeEmpregador IdeEmpregador { get; set; }

        [XmlElement("infoExclusao")]
        public InfoExclusao InfoExclusao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.InfoExclusao")]
    [ComVisible(true)]
#endif
    public class InfoExclusao
    {
        [XmlElement("tpEvento")]
        public string TpEvento { get; set; }

        [XmlElement("nrRecEvt")]
        public string NrRecEvt { get; set; }

        [XmlElement("ideTrabalhador")]
        public IdeTrabalhadorESocial3000 IdeTrabalhador { get; set; }

        [XmlElement("ideFolhaPagto")]
        public IdeFolhaPagto IdeFolhaPagto { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeTrabalhadorESocial3000")]
    [ComVisible(true)]
#endif
    public class IdeTrabalhadorESocial3000
    {
        [XmlElement("cpfTrab")]
        public string CpfTrab { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.ESocial.IdeFolhaPagto")]
    [ComVisible(true)]
#endif
    public class IdeFolhaPagto
    {
        [XmlElement("indApuracao")]
#if INTEROP
        public IndApuracao IndApuracao { get; set; } = (IndApuracao)(-1);
#else
        public IndApuracao? IndApuracao { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime PerApur { get; set; }
#else
        public DateTimeOffset PerApur { get; set; }
#endif

        [XmlElement("perApur")]
        public string PerApurField
        {
            get => PerApur.ToString("yyyy-MM");
#if INTEROP
            set => PerApur = DateTime.Parse(value);
#else
            set => PerApur = DateTimeOffset.Parse(value);
#endif
        }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndApuracao() => IndApuracao != (IndApuracao)(-1);
#else
        public bool ShouldSerializeIndApuracao() => IndApuracao != null;
#endif

        #endregion
    }
}
