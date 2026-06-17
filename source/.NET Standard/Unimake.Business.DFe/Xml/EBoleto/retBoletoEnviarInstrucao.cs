#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Retorno do envio de instrução de boleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retBoletoEnviarInstrucao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BoletoEnviarInstrucaoResponse", IsNullable = false)]
    public class retBoletoEnviarInstrucao : retEBoletoRetornoBasico
    {
    }
}
