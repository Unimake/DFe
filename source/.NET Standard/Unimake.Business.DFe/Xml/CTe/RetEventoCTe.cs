#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetEventoCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot("retEventoCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetEventoCTe
    {
        [XmlElement("infEvento", Order = 0)]
        public RetEventoCTeInfEvento InfEvento { get; set; }

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetEventoCTeInfEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class RetEventoCTeInfEvento
    {
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id { get; set; }

        [XmlElement("tpAmb", Order = 0)]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verAplic", Order = 1)]
        public string VerAplic { get; set; }

        [XmlIgnore]
        public UFBrasil COrgao { get; set; }

        [XmlElement("cOrgao", Order = 2)]
        public int COrgaoField
        {
            get => (int)COrgao;
            set => COrgao = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cStat", Order = 3)]
        public int CStat { get; set; }

        [XmlElement("xMotivo", Order = 4)]
        public string XMotivo { get; set; }

        [XmlElement("chCTe", Order = 5)]
        public string ChCTe { get; set; }

        [XmlElement("tpEvento", Order = 6)]
        public TipoEventoCTe TpEvento { get; set; }

        [XmlElement("xEvento", Order = 7)]
        public string XEvento { get; set; }

        [XmlElement("nSeqEvento", Order = 8)]
        public int NSeqEvento { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhRegEvento { get; set; }
#else
        public DateTimeOffset DhRegEvento { get; set; }
#endif

        [XmlElement("dhRegEvento", Order = 12)]
        public string DhRegEventoField
        {
            get => DhRegEvento.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRegEvento = DateTime.Parse(value);
#else
            set => DhRegEvento = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElementAttribute("nProt", Order = 13)]
        public string NProt { get; set; }
    }
}
