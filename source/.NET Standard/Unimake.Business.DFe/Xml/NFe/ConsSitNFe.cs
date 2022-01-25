#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ConsSitNFe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consSitNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsSitNFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("xServ")]
        public string XServ { get; set; } = "CONSULTAR";

        [XmlElement("chNFe")]
        public string ChNFe { get; set; }
    }
}