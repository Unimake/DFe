#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Retorno da informação de pagamento de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoInformarPagto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoInformarPagtoResponse", IsNullable = false)]
    public class retBoletoInformarPagto : retEBoletoRetornoBasico
    {
    }
}
