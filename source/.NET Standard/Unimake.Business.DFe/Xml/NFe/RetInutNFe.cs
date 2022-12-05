#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetInutNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retInutNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetInutNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infInut")]
        public InfInut InfInut { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfInut")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType("infInut", AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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

        [XmlElement("nProt")]
        public string NProt { get; set; }

        [XmlElement("Id")]
        public string Id { get; set; }
    }
}
