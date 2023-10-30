#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using System.Globalization;

namespace Unimake.Business.DFe.Xml.EFDReinf
{
    /// <summary>
    /// Configurações Cultura para o REINF
    /// </summary>
    public static class CultureInfoReinf
    {
        private static CultureInfo InfoField = new CultureInfo("pt-BR");

        public static CultureInfo Info 
        {
            get
            {
                InfoField.NumberFormat.NumberDecimalSeparator = ",";

                return InfoField;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeEvento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeEvento
    {
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissaoReinf ProcEmi { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.IdeContri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class IdeContri
    {
        [XmlElement("tpInsc")]
        public TiposInscricao TpInsc { get; set; }

        [XmlElement("nrInsc")]
        public string NrInsc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoTpServ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoTpServ
    {
        [XmlElement("tpServico")]
        public string TpServico { get; set; }

        [XmlIgnore]
        public double VlrBaseRet { get; set; }

        [XmlElement("vlrBaseRet")]
        public string VlrBaseRetField
        {
            get => VlrBaseRet.ToString("F2", CultureInfoReinf.Info);
            set => VlrBaseRet = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetencao { get; set; }

        [XmlElement("vlrRetencao")]
        public string VlrRetencaoField
        {
            get => VlrRetencao.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetencao = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrRetSub { get; set; }

        [XmlElement("vlrRetSub")]
        public string VlrRetSubField
        {
            get => VlrRetSub.ToString("F2", CultureInfoReinf.Info);
            set => VlrRetSub = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNRetPrinc { get; set; }

        [XmlElement("vlrNRetPrinc")]
        public string VlrNRetPrincField
        {
            get => VlrNRetPrinc.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRetPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos15 { get; set; }

        [XmlElement("vlrServicos15")]
        public string VlrServicos15Field
        {
            get => VlrServicos15.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos15 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos20 { get; set; }

        [XmlElement("vlrServicos20")]
        public string VlrServicos20Field
        {
            get => VlrServicos20.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos20 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrServicos25 { get; set; }

        [XmlElement("vlrServicos25")]
        public string VlrServicos25Field
        {
            get => VlrServicos25.ToString("F2", CultureInfoReinf.Info);
            set => VlrServicos25 = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrAdicional { get; set; }

        [XmlElement("vlrAdicional")]
        public string VlrAdicionalField
        {
            get => VlrAdicional.ToString("F2", CultureInfoReinf.Info);
            set => VlrAdicional = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        [XmlIgnore]
        public double VlrNRetAdic { get; set; }

        [XmlElement("vlrNRetAdic")]
        public string vlrNRetAdicField
        {
            get => VlrNRetAdic.ToString("F2", CultureInfoReinf.Info);
            set => VlrNRetAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region Should Serialize

        public bool ShouldSerializeVlrRetSubField() => VlrRetSub > 0;

        public bool ShouldSerializeVlrNRetPrincField() => VlrNRetPrinc > 0;

        public bool ShouldSerializeVlrServicos15Field() => VlrServicos15 > 0;

        public bool ShouldSerializeVlrServicos20Field() => VlrServicos20 > 0;

        public bool ShouldSerializeVlrServicos25Field() => VlrServicos25 > 0;

        public bool ShouldSerializeVlrAdicionalField() => VlrAdicional > 0;

        public bool ShouldSerializeVlrNRetAdicField() => VlrNRetAdic > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetPr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcRetPr
    {
        [XmlElement("tpProcRetPrinc")]
        public TipoProcesso TpProcRetPrinc { get; set; }


        [XmlElement("nrProcRetPrinc")]
        public string NrProcRetPrinc { get; set; }


        [XmlElement("codSuspPrinc")]
        public string CodSuspPrinc { get; set; }


        [XmlIgnore]
        public double ValorPrinc { get; set; }

        [XmlElement("valorPrinc")]
        public string ValorPrincField
        {
            get => ValorPrinc.ToString("F2", CultureInfoReinf.Info);
            set => ValorPrinc = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspPrinc() => !string.IsNullOrEmpty(CodSuspPrinc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.InfoProcRetAd")]
    [ComVisible(true)]
#endif
    [Serializable()]
    public class InfoProcRetAd
    {
        [XmlElement("tpProcRetAdic")]
        public TipoProcesso TpProcRetAdic { get; set; }

        [XmlElement("nrProcRetAdic")]
        public string NrProcRetAdic { get; set; }

        [XmlElement("codSuspAdic")]
        public string CodSuspAdic { get; set; }

        [XmlIgnore]
        public double ValorAdic { get; set; }

        [XmlElement("valorAdic")]
        public string ValorAdicField
        {
            get => ValorAdic.ToString("F2", CultureInfoReinf.Info);
            set => ValorAdic = double.Parse(value.ToString(), CultureInfoReinf.Info);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCodSuspAdic() => !string.IsNullOrEmpty(CodSuspAdic);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.EFDReinf.EndExt")]
    [ComVisible(true)]
#endif
    public class EndExt
    {
        [XmlElement("dscLograd")]
        public string DscLograd { get; set; }

        [XmlElement("nrLograd")]
        public string NrLograd { get; set; }

        [XmlElement("complem")]
        public string Complem { get; set; }

        [XmlElement("bairro")]
        public string Bairro { get; set; }

        [XmlElement("cidade")]
        public string Cidade { get; set; }

        [XmlElement("estado")]
        public string Estado { get; set; }

        [XmlElement("codPostal")]
        public string CodPostal { get; set; }

        [XmlElement("telef")]
        public string Telef { get; set; }

        #region ShouldSerialize

        public bool ShouldSereializeDscLograd() => !string.IsNullOrEmpty(DscLograd);

        public bool ShouldSereializeNrLograd() => !string.IsNullOrEmpty(NrLograd);

        public bool ShouldSereializeComplem() => !string.IsNullOrEmpty(Complem);

        public bool ShouldSereializeBairro() => !string.IsNullOrEmpty(Bairro);

        public bool ShouldSereializeCidade() => !string.IsNullOrEmpty(Cidade);

        public bool ShouldSereializeEstado() => !string.IsNullOrEmpty(Estado);

        public bool ShouldSereializeCodPostal() => !string.IsNullOrEmpty(CodPostal);

        public bool ShouldSereializeTelef() => !string.IsNullOrEmpty(Telef);

        #endregion
    }

}
