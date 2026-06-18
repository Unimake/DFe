#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.EBoleto
{
    /// <summary>
    /// Classe base para os retornos do eBoleto
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EBoleto.retEBoletoRetornoBasico")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public abstract class retEBoletoRetornoBasico : XMLBase
    {
        /// <summary>
        /// Status do retorno
        /// </summary>
        [XmlElement]
        public int Status { get; set; }

        /// <summary>
        /// Motivo do retorno
        /// </summary>
        [XmlElement]
        public string Motivo { get; set; }

    }
}
