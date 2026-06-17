#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;

namespace Unimake.Business.DFe.Xml.PIX
{
    /// <summary>
    /// Retorno da criação de cobrança PIX
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.PIX.retPIXCobrancaCriar")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("PIXCobrancaCreateResponse", IsNullable = false)]
    public class retPIXCobrancaCriar : XMLBase
    {
        /// <summary>
        /// Status do retorno
        /// </summary>
        [XmlElement(Order = 0)]
        public int Status { get; set; }

        /// <summary>
        /// Motivo do retorno
        /// </summary>
        [XmlElement(Order = 1)]
        public string Motivo { get; set; }

        /// <summary>
        /// PIX copia e cola
        /// </summary>
        [XmlElement(Order = 2)]
        public string PixCopiaECola { get; set; }

        /// <summary>
        /// Caminho da imagem do QRCode
        /// </summary>
        [XmlElement(Order = 3)]
        public string ImageQRCode { get; set; }

        /// <summary>
        /// Versão da DLL
        /// </summary>
        [XmlElement(Order = 4)]
        public string DLLVersao { get; set; }
    }
}
