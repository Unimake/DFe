#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.NFCom
{
    /// <summary>
    /// NFCom - Portal da Nota Fiscal Fatura de Serviço de Comunicação Eletrônica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.NFCom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfcom")]
    [XmlRoot("NFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class NFCom
    {
    }
}
