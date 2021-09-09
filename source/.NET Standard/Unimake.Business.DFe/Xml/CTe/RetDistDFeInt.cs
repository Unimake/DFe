#pragma warning disable CS1591

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
    [XmlRoot("retDistDFeInt", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetDistDFeInt : XMLBase
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
        public DateTime DhResp { get; set; }

        [XmlElement("dhResp")]
        public string DhRespField
        {
            get => DhResp.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhResp = DateTime.Parse(value);
        }

        [XmlElement("ultNSU", DataType = "token")]
        public string UltNSU { get; set; }

        [XmlElement("maxNSU", DataType = "token")]
        public string MaxNSU { get; set; }

        [XmlElement("loteDistDFeInt")]
        public LoteDistDFeInt LoteDistDFeInt { get; set; }
    }

    /// <remarks/>
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LoteDistDFeInt
    {
        [XmlElement("docZip")]
        public List<DocZip> DocZip { get; set; }
    }

    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public partial class DocZip
    {
        [XmlAttribute("NSU", DataType = "token")]
        public string NSU { get; set; }

        [XmlAttribute("schema")]
        public string Schema { get; set; }

        [XmlText(DataType = "base64Binary")]
        public byte[] Value { get; set; }
    }
}
