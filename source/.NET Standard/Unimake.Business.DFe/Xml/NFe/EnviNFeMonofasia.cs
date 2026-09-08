#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFe
{
    internal static class MonofasiaSerialization
    {
        internal static string Format2(double value) => value.ToString("F2", CultureInfo.InvariantCulture);

        internal static string Format4(double value) => value.ToString("F4", CultureInfo.InvariantCulture);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMonoAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSMonoAdRem
    {
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoIBSAdRem GMonoPadrao { get; set; }

        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenIBSAdRem GMonoReten { get; set; }

        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetIBS GMonoRet { get; set; }

        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaIBS GPBioDiferenca { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMonoAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSMonoAdValorem
    {
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoIBSAdValorem GMonoPadrao { get; set; }

        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenIBSAdValorem GMonoReten { get; set; }

        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetIBS GMonoRet { get; set; }

        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaIBS GPBioDiferenca { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSMonoAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCBSMonoAdRem
    {
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoCBSAdRem GMonoPadrao { get; set; }

        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenCBSAdRem GMonoReten { get; set; }

        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetCBS GMonoRet { get; set; }

        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaCBS GPBioDiferenca { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSMonoAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCBSMonoAdValorem
    {
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoCBSAdValorem GMonoPadrao { get; set; }

        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenCBSAdValorem GMonoReten { get; set; }

        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetCBS GMonoRet { get; set; }

        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaCBS GPBioDiferenca { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoIBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoIBSAdRem
    {
        [XmlIgnore]
        public double QBCMono { get; set; }

        [XmlElement("qBCMono", Order = 0)]
        public string QBCMonoField
        {
            get => MonofasiaSerialization.Format4(QBCMono);
            set => QBCMono = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double AdRemIBS { get; set; }

        [XmlElement("adRemIBS", Order = 1)]
        public string AdRemIBSField
        {
            get => MonofasiaSerialization.Format4(AdRemIBS);
            set => AdRemIBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMono { get; set; }

        [XmlElement("vIBSMono", Order = 2)]
        public string VIBSMonoField
        {
            get => MonofasiaSerialization.Format2(VIBSMono);
            set => VIBSMono = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenIBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenIBSAdRem
    {
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        [XmlElement("qBCMonoReten", Order = 0)]
        public string QBCMonoRetenField
        {
            get => MonofasiaSerialization.Format4(QBCMonoReten);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double AdRemIBSReten { get; set; }

        [XmlElement("adRemIBSReten", Order = 1)]
        public string AdRemIBSRetenField
        {
            get => MonofasiaSerialization.Format4(AdRemIBSReten);
            set => AdRemIBSReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        [XmlElement("vIBSMonoReten", Order = 2)]
        public string VIBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoReten);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoIBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoIBSAdValorem
    {
        [XmlIgnore]
        public double VBCMono { get; set; }

        [XmlElement("vBCMono", Order = 0)]
        public string VBCMonoField
        {
            get => MonofasiaSerialization.Format2(VBCMono);
            set => VBCMono = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqMonoUF { get; set; }

        [XmlElement("pAliqMonoUF", Order = 1)]
        public string PAliqMonoUFField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoUF);
            set => PAliqMonoUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMonoUF { get; set; }

        [XmlElement("vIBSMonoUF", Order = 2)]
        public string VIBSMonoUFField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoUF);
            set => VIBSMonoUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqMonoMun { get; set; }

        [XmlElement("pAliqMonoMun", Order = 3)]
        public string PAliqMonoMunField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoMun);
            set => PAliqMonoMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMonoMun { get; set; }

        [XmlElement("vIBSMonoMun", Order = 4)]
        public string VIBSMonoMunField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoMun);
            set => VIBSMonoMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMono { get; set; }

        [XmlElement("vIBSMono", Order = 5)]
        public string VIBSMonoField
        {
            get => MonofasiaSerialization.Format2(VIBSMono);
            set => VIBSMono = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenIBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenIBSAdValorem
    {
        [XmlIgnore]
        public double VBCMonoReten { get; set; }

        [XmlElement("vBCMonoReten", Order = 0)]
        public string VBCMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VBCMonoReten);
            set => VBCMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqMonoReten { get; set; }

        [XmlElement("pAliqMonoReten", Order = 1)]
        public string PAliqMonoRetenField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoReten);
            set => PAliqMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        [XmlElement("vIBSMonoReten", Order = 2)]
        public string VIBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoReten);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetIBS
    {
        [XmlIgnore]
        public double VIBSMonoRet { get; set; }

        [XmlElement("vIBSMonoRet", Order = 0)]
        public string VIBSMonoRetField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoRet);
            set => VIBSMonoRet = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPBioDiferencaIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPBioDiferencaIBS
    {
        [XmlIgnore]
        public double QBCBioComb { get; set; }

        [XmlElement("qBCBioComb", Order = 0)]
        public string QBCBioCombField
        {
            get => MonofasiaSerialization.Format4(QBCBioComb);
            set => QBCBioComb = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSDiferenca { get; set; }

        [XmlElement("vIBSDiferenca", Order = 1)]
        public string VIBSDiferencaField
        {
            get => MonofasiaSerialization.Format2(VIBSDiferenca);
            set => VIBSDiferenca = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoCBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoCBSAdRem
    {
        [XmlIgnore]
        public double QBCMono { get; set; }

        [XmlElement("qBCMono", Order = 0)]
        public string QBCMonoField
        {
            get => MonofasiaSerialization.Format4(QBCMono);
            set => QBCMono = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double AdRemCBS { get; set; }

        [XmlElement("adRemCBS", Order = 1)]
        public string AdRemCBSField
        {
            get => MonofasiaSerialization.Format4(AdRemCBS);
            set => AdRemCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSMono { get; set; }

        [XmlElement("vCBSMono", Order = 2)]
        public string VCBSMonoField
        {
            get => MonofasiaSerialization.Format2(VCBSMono);
            set => VCBSMono = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenCBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenCBSAdRem
    {
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        [XmlElement("qBCMonoReten", Order = 0)]
        public string QBCMonoRetenField
        {
            get => MonofasiaSerialization.Format4(QBCMonoReten);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double AdRemCBSReten { get; set; }

        [XmlElement("adRemCBSReten", Order = 1)]
        public string AdRemCBSRetenField
        {
            get => MonofasiaSerialization.Format4(AdRemCBSReten);
            set => AdRemCBSReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        [XmlElement("vCBSMonoReten", Order = 2)]
        public string VCBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoReten);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoCBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoCBSAdValorem
    {
        [XmlIgnore]
        public double VBCMono { get; set; }

        [XmlElement("vBCMono", Order = 0)]
        public string VBCMonoField
        {
            get => MonofasiaSerialization.Format2(VBCMono);
            set => VBCMono = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqMonoCBS { get; set; }

        [XmlElement("pAliqMonoCBS", Order = 1)]
        public string PAliqMonoCBSField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoCBS);
            set => PAliqMonoCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSMono { get; set; }

        [XmlElement("vCBSMono", Order = 2)]
        public string VCBSMonoField
        {
            get => MonofasiaSerialization.Format2(VCBSMono);
            set => VCBSMono = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenCBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenCBSAdValorem
    {
        [XmlIgnore]
        public double VBCMonoReten { get; set; }

        [XmlElement("vBCMonoReten", Order = 0)]
        public string VBCMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VBCMonoReten);
            set => VBCMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqMonoReten { get; set; }

        [XmlElement("pAliqMonoReten", Order = 1)]
        public string PAliqMonoRetenField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoReten);
            set => PAliqMonoReten = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        [XmlElement("vCBSMonoReten", Order = 2)]
        public string VCBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoReten);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetCBS
    {
        [XmlIgnore]
        public double VCBSMonoRet { get; set; }

        [XmlElement("vCBSMonoRet", Order = 0)]
        public string VCBSMonoRetField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoRet);
            set => VCBSMonoRet = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPBioDiferencaCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPBioDiferencaCBS
    {
        [XmlIgnore]
        public double QBCBioComb { get; set; }

        [XmlElement("qBCBioComb", Order = 0)]
        public string QBCBioCombField
        {
            get => MonofasiaSerialization.Format4(QBCBioComb);
            set => QBCBioComb = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSDiferenca { get; set; }

        [XmlElement("vCBSDiferenca", Order = 1)]
        public string VCBSDiferencaField
        {
            get => MonofasiaSerialization.Format2(VCBSDiferenca);
            set => VCBSDiferenca = Converter.ToDouble(value);
        }
    }
}
