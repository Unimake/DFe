#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ConsReciCTe")]
    [ComVisible(true)]
#endif
    [XmlRoot("consReciCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class ConsReciCTe: XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("nRec")]
        public string NRec { get; set; }
    }
}
