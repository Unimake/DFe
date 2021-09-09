#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
    [XmlRoot("retConsStatServCte", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetConsStatServCte : XMLBase
    {
        private const string FormatDate = "yyyy-MM-ddTHH:mm:sszzz";

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
        public DateTime DhRecbto { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString(FormatDate);
            set => DhRecbto = DateTime.Parse(value);
        }

        [XmlElement("tMed")]
        public int TMed { get; set; }

        [XmlIgnore]
        public DateTime DhRetorno { get; set; }

        [XmlElement("dhRetorno")]
        public string DhRetornoField
        {
            get => DhRetorno.ToString(FormatDate);
            set => DhRetorno = DateTime.Parse(value);
        }

        [XmlElement("xObs")]
        public string XObs { get; set; }

    }
}
