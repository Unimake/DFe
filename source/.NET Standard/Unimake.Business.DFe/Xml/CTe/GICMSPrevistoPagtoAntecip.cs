#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
    /// <summary>
    /// ICMS previsto para o fornecimento futuro em pagamento antecipado.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GICMSPrevistoPagtoAntecip")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GICMSPrevistoPagtoAntecip
    {
        /// <summary>
        /// Valor do ICMS a incidir no fornecimento futuro; informar zero quando não devido.
        /// </summary>
        [XmlIgnore]
        public double VICMSPrevisto { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VICMSPrevisto.
        /// </summary>
        [XmlElement("vICMSPrevisto")]
        public string VICMSPrevistoField
        {
            get => VICMSPrevisto.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSPrevisto = Converter.ToDouble(value);
        }

    }

}
