#if INTEROP
using System.Runtime.InteropServices;
#endif
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFCom
{
    /// <summary>
    /// Valores de IBS e CBS a considerar sem a aplicação do processo judicial referenciado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSCBSProcRef")]
    [ComVisible(true)]
#endif
    public class GIBSCBSProcRef
    {
        /// <summary>
        /// Base de cálculo do IBS e da CBS.
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VBC.
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS estadual.
        /// </summary>
        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VIBSUF.
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS municipal.
        /// </summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VIBSMun.
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS.
        /// </summary>
        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VCBS.
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }
}
