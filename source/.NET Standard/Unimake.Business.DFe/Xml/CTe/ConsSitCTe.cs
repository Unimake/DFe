#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ConsSitCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consSitCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ConsSitCTe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        [XmlElement("chCTe")]
        public string ChCTe { get; set; }
    }
}