#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTeSimp
{
    /// <summary>
    /// Totais da tributação de IBS e CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.IBSCBSTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IBSCBSTot
    {
        /// <summary>
        /// Total da base de cálculo de IBS e CBS.
        /// </summary>
        [XmlIgnore]
        public double VBCIBSCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VBCIBSCBS.
        /// </summary>
        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBSField
        {
            get => VBCIBSCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIBSCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Totalização do IBS.
        /// </summary>
        [XmlElement("gIBS")]
        public GIBSTot GIBSTot { get; set; }

        /// <summary>
        /// Totalização da CBS.
        /// </summary>
        [XmlElement("gCBS")]
        public GCBSTot GCBSTot { get; set; }

        /// <summary>
        /// Totalização do estorno de crédito.
        /// </summary>
        [XmlElement("gEstornoCred")]
        public CTe.GEstornoCred GEstornoCred { get; set; }

    }

    /// <summary>
    /// Totais do IBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.GIBSTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSTot
    {
        /// <summary>
        /// Totalização do IBS estadual.
        /// </summary>
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUFTot { get; set; }

        /// <summary>
        /// Totalização do IBS municipal.
        /// </summary>
        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMunTot { get; set; }

        /// <summary>
        /// Valor total do IBS.
        /// </summary>
        [XmlIgnore]
        public double VIBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VIBS.
        /// </summary>
        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => VIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBS = Converter.ToDouble(value);
        }

    }

    /// <summary>
    /// Totais do IBS estadual.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.GIBSUFTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSUFTot
    {
        /// <summary>
        /// Total do diferimento.
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDif.
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de devoluções de tributos.
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDevTrib.
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do IBS estadual.
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

    }

    /// <summary>
    /// Totais do IBS municipal.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.GIBSMunTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSMunTot
    {
        /// <summary>
        /// Total do diferimento.
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDif.
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de devoluções de tributos.
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDevTrib.
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do IBS municipal.
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

    }

    /// <summary>
    /// Totais da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.GCBSTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GCBSTot
    {
        /// <summary>
        /// Total do diferimento.
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDif.
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de devoluções de tributos.
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização de VDevTrib.
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da CBS.
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
