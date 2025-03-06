#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// Classe de retorno do pedido de consulta de cadastro de contribuintes
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RetConsCad")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("retConsCad", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class RetConsCad : NFe.RetConsCad
    {
    }
}