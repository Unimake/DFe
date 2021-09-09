#pragma warning disable CS1591

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{

    [XmlRoot("retInutCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class RetInutCTe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement(ElementName = "infInut")]
        public InfInut InfInut = new InfInut();
    }

    public class InfInut
    {
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

        [XmlElement("ano")]
        public string Ano { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlIgnore]
        public ModeloDFe Mod { get; set; }

        [XmlElement("mod")]
        public int ModField
        {
            get => (int)Mod;
            set => Mod = (ModeloDFe)Enum.Parse(typeof(ModeloDFe), value.ToString());
        }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nNFIni")]
        public string NNFIni { get; set; }

        [XmlElement("nNFFin")]
        public string NNFFin { get; set; }

        [XmlIgnore]
        public DateTime DhRecbto { get; set; }

        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhRecbto = DateTime.Parse(value);
        }

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("Id")]
        public string Id { get; set; }
    }
}
