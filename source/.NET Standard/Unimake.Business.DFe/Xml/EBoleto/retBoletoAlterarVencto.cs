#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Retorno da alteração de vencimento de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoAlterarVencto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoAlterarVenctoResponse", IsNullable = false)]
    public class retBoletoAlterarVencto : retEBoletoRetornoBasico
    {
    }
}
