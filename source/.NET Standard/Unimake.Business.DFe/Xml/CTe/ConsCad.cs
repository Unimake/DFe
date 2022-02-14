#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ConsCad")]
    [ComVisible(true)]
#endif
    [XmlRoot("ConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class ConsCad : NFe.ConsCadBase
    {

    }
}