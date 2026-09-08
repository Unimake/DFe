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

    /// <summary>
    /// Grupo de informações da tributação monofásica Ad Rem do IBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMonoAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSMonoAdRem
    {
        /// <summary>
        /// Grupo de tributação monofásica padrão do IBS.
        /// </summary>
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoIBSAdRem GMonoPadrao { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica do IBS sujeita à retenção.
        /// </summary>
        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenIBSAdRem GMonoReten { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica do IBS retida anteriormente.
        /// </summary>
        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetIBS GMonoRet { get; set; }

        /// <summary>
        /// Grupo da diferença decorrente da mistura de biocombustível.
        /// </summary>
        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaIBS GPBioDiferenca { get; set; }
    }

    /// <summary>
    /// Grupo de informações da tributação monofásica Ad Valorem do IBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMonoAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSMonoAdValorem
    {
        /// <summary>
        /// Grupo de tributação monofásica padrão do IBS.
        /// </summary>
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoIBSAdValorem GMonoPadrao { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica do IBS sujeita à retenção.
        /// </summary>
        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenIBSAdValorem GMonoReten { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica do IBS retida anteriormente.
        /// </summary>
        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetIBS GMonoRet { get; set; }

        /// <summary>
        /// Grupo da diferença decorrente da mistura de biocombustível.
        /// </summary>
        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaIBS GPBioDiferenca { get; set; }
    }

    /// <summary>
    /// Grupo de informações da tributação monofásica Ad Rem da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSMonoAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCBSMonoAdRem
    {
        /// <summary>
        /// Grupo de tributação monofásica padrão da CBS.
        /// </summary>
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoCBSAdRem GMonoPadrao { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica da CBS sujeita à retenção.
        /// </summary>
        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenCBSAdRem GMonoReten { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica da CBS retida anteriormente.
        /// </summary>
        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetCBS GMonoRet { get; set; }

        /// <summary>
        /// Grupo da diferença decorrente da mistura de biocombustível.
        /// </summary>
        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaCBS GPBioDiferenca { get; set; }
    }

    /// <summary>
    /// Grupo de informações da tributação monofásica Ad Valorem da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSMonoAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCBSMonoAdValorem
    {
        /// <summary>
        /// Grupo de tributação monofásica padrão da CBS.
        /// </summary>
        [XmlElement("gMonoPadrao", Order = 0)]
        public GMonoPadraoCBSAdValorem GMonoPadrao { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica da CBS sujeita à retenção.
        /// </summary>
        [XmlElement("gMonoReten", Order = 1)]
        public GMonoRetenCBSAdValorem GMonoReten { get; set; }

        /// <summary>
        /// Grupo de tributação monofásica da CBS retida anteriormente.
        /// </summary>
        [XmlElement("gMonoRet", Order = 2)]
        public GMonoRetCBS GMonoRet { get; set; }

        /// <summary>
        /// Grupo da diferença decorrente da mistura de biocombustível.
        /// </summary>
        [XmlElement("gpBioDiferenca", Order = 3)]
        public GPBioDiferencaCBS GPBioDiferenca { get; set; }
    }

    /// <summary>
    /// Informações da tributação monofásica padrão Ad Rem do IBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoIBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoIBSAdRem
    {
        /// <summary>
        /// Quantidade tributada na monofasia.
        /// </summary>
        [XmlIgnore]
        public double QBCMono { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCMono"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCMono", Order = 0)]
        public string QBCMonoField
        {
            get => MonofasiaSerialization.Format4(QBCMono);
            set => QBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Rem do IBS.
        /// </summary>
        [XmlIgnore]
        public double AdRemIBS { get; set; }

        /// <summary>
        /// Valor de <see cref="AdRemIBS"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("adRemIBS", Order = 1)]
        public string AdRemIBSField
        {
            get => MonofasiaSerialization.Format4(AdRemIBS);
            set => AdRemIBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico.
        /// </summary>
        [XmlIgnore]
        public double VIBSMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMono", Order = 2)]
        public string VIBSMonoField
        {
            get => MonofasiaSerialization.Format2(VIBSMono);
            set => VIBSMono = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica Ad Rem do IBS sujeita à retenção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenIBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenIBSAdRem
    {
        /// <summary>
        /// Quantidade tributada sujeita à retenção na monofasia.
        /// </summary>
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCMonoReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCMonoReten", Order = 0)]
        public string QBCMonoRetenField
        {
            get => MonofasiaSerialization.Format4(QBCMonoReten);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Rem do IBS sujeito à retenção.
        /// </summary>
        [XmlIgnore]
        public double AdRemIBSReten { get; set; }

        /// <summary>
        /// Valor de <see cref="AdRemIBSReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("adRemIBSReten", Order = 1)]
        public string AdRemIBSRetenField
        {
            get => MonofasiaSerialization.Format4(AdRemIBSReten);
            set => AdRemIBSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico sujeito à retenção.
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMonoReten", Order = 2)]
        public string VIBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoReten);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica padrão Ad Valorem do IBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoIBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoIBSAdValorem
    {
        /// <summary>
        /// Valor da base de cálculo tributada na monofasia.
        /// </summary>
        [XmlIgnore]
        public double VBCMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VBCMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vBCMono", Order = 0)]
        public string VBCMonoField
        {
            get => MonofasiaSerialization.Format2(VBCMono);
            set => VBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Valorem do IBS estadual.
        /// </summary>
        [XmlIgnore]
        public double PAliqMonoUF { get; set; }

        /// <summary>
        /// Valor de <see cref="PAliqMonoUF"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("pAliqMonoUF", Order = 1)]
        public string PAliqMonoUFField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoUF);
            set => PAliqMonoUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico estadual.
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoUF { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMonoUF"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMonoUF", Order = 2)]
        public string VIBSMonoUFField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoUF);
            set => VIBSMonoUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Valorem do IBS municipal.
        /// </summary>
        [XmlIgnore]
        public double PAliqMonoMun { get; set; }

        /// <summary>
        /// Valor de <see cref="PAliqMonoMun"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("pAliqMonoMun", Order = 3)]
        public string PAliqMonoMunField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoMun);
            set => PAliqMonoMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico municipal.
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoMun { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMonoMun"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMonoMun", Order = 4)]
        public string VIBSMonoMunField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoMun);
            set => VIBSMonoMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do IBS monofásico.
        /// </summary>
        [XmlIgnore]
        public double VIBSMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMono", Order = 5)]
        public string VIBSMonoField
        {
            get => MonofasiaSerialization.Format2(VIBSMono);
            set => VIBSMono = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica Ad Valorem do IBS sujeita à retenção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenIBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenIBSAdValorem
    {
        /// <summary>
        /// Valor da base de cálculo sujeito à retenção na monofasia.
        /// </summary>
        [XmlIgnore]
        public double VBCMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VBCMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vBCMonoReten", Order = 0)]
        public string VBCMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VBCMonoReten);
            set => VBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Valorem do IBS sujeito à retenção.
        /// </summary>
        [XmlIgnore]
        public double PAliqMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="PAliqMonoReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("pAliqMonoReten", Order = 1)]
        public string PAliqMonoRetenField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoReten);
            set => PAliqMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico sujeito à retenção.
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMonoReten", Order = 2)]
        public string VIBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoReten);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações do IBS monofásico retido anteriormente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetIBS
    {
        /// <summary>
        /// Valor do IBS retido anteriormente.
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoRet { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSMonoRet"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSMonoRet", Order = 0)]
        public string VIBSMonoRetField
        {
            get => MonofasiaSerialization.Format2(VIBSMonoRet);
            set => VIBSMonoRet = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da diferença de IBS decorrente da mistura de biocombustível.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPBioDiferencaIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPBioDiferencaIBS
    {
        /// <summary>
        /// Quantidade de biocombustível a recolher ou ressarcir.
        /// </summary>
        [XmlIgnore]
        public double QBCBioComb { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCBioComb"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCBioComb", Order = 0)]
        public string QBCBioCombField
        {
            get => MonofasiaSerialization.Format4(QBCBioComb);
            set => QBCBioComb = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da diferença do IBS em relação ao percentual obrigatório de biocombustível.
        /// </summary>
        [XmlIgnore]
        public double VIBSDiferenca { get; set; }

        /// <summary>
        /// Valor de <see cref="VIBSDiferenca"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vIBSDiferenca", Order = 1)]
        public string VIBSDiferencaField
        {
            get => MonofasiaSerialization.Format2(VIBSDiferenca);
            set => VIBSDiferenca = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica padrão Ad Rem da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoCBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoCBSAdRem
    {
        /// <summary>
        /// Quantidade tributada na monofasia.
        /// </summary>
        [XmlIgnore]
        public double QBCMono { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCMono"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCMono", Order = 0)]
        public string QBCMonoField
        {
            get => MonofasiaSerialization.Format4(QBCMono);
            set => QBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Rem da CBS.
        /// </summary>
        [XmlIgnore]
        public double AdRemCBS { get; set; }

        /// <summary>
        /// Valor de <see cref="AdRemCBS"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("adRemCBS", Order = 1)]
        public string AdRemCBSField
        {
            get => MonofasiaSerialization.Format4(AdRemCBS);
            set => AdRemCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS monofásica.
        /// </summary>
        [XmlIgnore]
        public double VCBSMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSMono", Order = 2)]
        public string VCBSMonoField
        {
            get => MonofasiaSerialization.Format2(VCBSMono);
            set => VCBSMono = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica Ad Rem da CBS sujeita à retenção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenCBSAdRem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenCBSAdRem
    {
        /// <summary>
        /// Quantidade tributada sujeita à retenção na monofasia.
        /// </summary>
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCMonoReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCMonoReten", Order = 0)]
        public string QBCMonoRetenField
        {
            get => MonofasiaSerialization.Format4(QBCMonoReten);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Rem da CBS sujeita à retenção.
        /// </summary>
        [XmlIgnore]
        public double AdRemCBSReten { get; set; }

        /// <summary>
        /// Valor de <see cref="AdRemCBSReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("adRemCBSReten", Order = 1)]
        public string AdRemCBSRetenField
        {
            get => MonofasiaSerialization.Format4(AdRemCBSReten);
            set => AdRemCBSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS monofásica sujeita à retenção.
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSMonoReten", Order = 2)]
        public string VCBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoReten);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica padrão Ad Valorem da CBS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoPadraoCBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoPadraoCBSAdValorem
    {
        /// <summary>
        /// Valor da base de cálculo tributada na monofasia.
        /// </summary>
        [XmlIgnore]
        public double VBCMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VBCMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vBCMono", Order = 0)]
        public string VBCMonoField
        {
            get => MonofasiaSerialization.Format2(VBCMono);
            set => VBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Valorem da CBS.
        /// </summary>
        [XmlIgnore]
        public double PAliqMonoCBS { get; set; }

        /// <summary>
        /// Valor de <see cref="PAliqMonoCBS"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("pAliqMonoCBS", Order = 1)]
        public string PAliqMonoCBSField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoCBS);
            set => PAliqMonoCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS monofásica.
        /// </summary>
        [XmlIgnore]
        public double VCBSMono { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSMono"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSMono", Order = 2)]
        public string VCBSMonoField
        {
            get => MonofasiaSerialization.Format2(VCBSMono);
            set => VCBSMono = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da tributação monofásica Ad Valorem da CBS sujeita à retenção.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetenCBSAdValorem")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetenCBSAdValorem
    {
        /// <summary>
        /// Valor da base de cálculo sujeito à retenção na monofasia.
        /// </summary>
        [XmlIgnore]
        public double VBCMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VBCMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vBCMonoReten", Order = 0)]
        public string VBCMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VBCMonoReten);
            set => VBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Ad Valorem da CBS sujeita à retenção.
        /// </summary>
        [XmlIgnore]
        public double PAliqMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="PAliqMonoReten"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("pAliqMonoReten", Order = 1)]
        public string PAliqMonoRetenField
        {
            get => MonofasiaSerialization.Format4(PAliqMonoReten);
            set => PAliqMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS monofásica sujeita à retenção.
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSMonoReten"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSMonoReten", Order = 2)]
        public string VCBSMonoRetenField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoReten);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da CBS monofásica retida anteriormente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMonoRetCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMonoRetCBS
    {
        /// <summary>
        /// Valor da CBS retida anteriormente.
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoRet { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSMonoRet"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSMonoRet", Order = 0)]
        public string VCBSMonoRetField
        {
            get => MonofasiaSerialization.Format2(VCBSMonoRet);
            set => VCBSMonoRet = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da diferença da CBS decorrente da mistura de biocombustível.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GPBioDiferencaCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GPBioDiferencaCBS
    {
        /// <summary>
        /// Quantidade de biocombustível a recolher ou ressarcir.
        /// </summary>
        [XmlIgnore]
        public double QBCBioComb { get; set; }

        /// <summary>
        /// Valor de <see cref="QBCBioComb"/> formatado para serialização XML com quatro casas decimais.
        /// </summary>
        [XmlElement("qBCBioComb", Order = 0)]
        public string QBCBioCombField
        {
            get => MonofasiaSerialization.Format4(QBCBioComb);
            set => QBCBioComb = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da diferença da CBS em relação ao percentual obrigatório de biocombustível.
        /// </summary>
        [XmlIgnore]
        public double VCBSDiferenca { get; set; }

        /// <summary>
        /// Valor de <see cref="VCBSDiferenca"/> formatado para serialização XML com duas casas decimais.
        /// </summary>
        [XmlElement("vCBSDiferenca", Order = 1)]
        public string VCBSDiferencaField
        {
            get => MonofasiaSerialization.Format2(VCBSDiferenca);
            set => VCBSDiferenca = Converter.ToDouble(value);
        }
    }
}
