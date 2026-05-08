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
    [ProgId("Unimake.Business.DFe.Xml.DCe.RetEventoDCe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retEventoDCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class RetEventoDCe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infEvento")]
        public InfEventoRet InfEvento { get; set; }
    }

    public class InfEventoRet
    {
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic")]
        public string VerAplic { get; set; }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao")]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cStat")]
        public int CStat { get; set; }

        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }

        [XmlElement("chDCe")]
        public string ChDCe { get; set; }

        [XmlElement("tpEvento")]
        public TipoEventoDCe TpEvento { get; set; }

        [XmlElement("xEvento")]
        public string XEvento { get; set; }

        [XmlElement("nSeqEvento")]
        public int NSeqEvento { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        [XmlElement("dhRegEvento")]
        public string DhRegEventoField
        {
            get => DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nProt")]
        public string NProt { get; set; }

        public bool ShouldSerializeId() => !string.IsNullOrEmpty(Id);
        public bool ShouldSerializeChDCe() => !string.IsNullOrEmpty(ChDCe);
        public bool ShouldSerializeTpEvento() => TpEvento != 0;
        public bool ShouldSerializeXEvento() => !string.IsNullOrEmpty(XEvento);
        public bool ShouldSerializeNSeqEvento() => NSeqEvento > 0;
        public bool ShouldSerializeDhRegEventoField() => DhRegEvento > DateTime.MinValue;
        public bool ShouldSerializeNProt() => !string.IsNullOrEmpty(NProt);
    }
}
