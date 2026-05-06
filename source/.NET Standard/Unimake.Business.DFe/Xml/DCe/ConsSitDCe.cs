#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.DCe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.ConsSitDCe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consSitDCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class ConsSitDCe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        [XmlElement("chDCe")]
        public string ChDCe { get; set; }
    }
}
