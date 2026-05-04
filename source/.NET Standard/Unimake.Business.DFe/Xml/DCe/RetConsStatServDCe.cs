#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.DCe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.RetConsStatServDCe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retConsStatServDCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class RetConsStatServDCe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tMed")]
        public string TMed { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRetorno { get; set; }
#else
        public DateTimeOffset DhRetorno { get; set; }
#endif

        [XmlElement("dhRetorno")]
        public string DhRetornoField
        {
            get => DhRetorno.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRetorno = DateTime.Parse(value);
#else
            set => DhRetorno = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xObs")]
        public string XObs { get; set; }

        public bool ShouldSerializeTMed() => !string.IsNullOrEmpty(TMed);
        public bool ShouldSerializeDhRetornoField() => DhRetorno > DateTime.MinValue;
        public bool ShouldSerializeXObs() => !string.IsNullOrEmpty(XObs);
    }
}
