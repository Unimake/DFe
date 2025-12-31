#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFSe.NACIONAL
{
    /// <summary>
    /// Retorno da NFS-e – Padrão Nacional
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.NFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType(Namespace = Nfse.Ns)]
    [XmlRoot("NFSe", Namespace = Nfse.Ns, IsNullable = false)]
    public class NFSe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML de retorno da NFS-e.
        /// </summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Informações da NFS-e gerada.
        /// </summary>
        [XmlElement("infNFSe", Namespace = Nfse.Ns)]
        public InfNFSeRet InfNFSe { get; set; }

        /// <summary>
        /// Assinatura digital.
        /// </summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    #region InfNFSeRet

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.InfNFSeRet")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("infNFSe", Namespace = Nfse.Ns)]
    public class InfNFSeRet
    {
        /// <summary>
        /// Identificador da NFS-e
        /// </summary>
        [XmlAttribute("Id", DataType = "token")]
        public string Id { get; set; }

        /// <summary>
        /// Município de localização do emitente.
        /// </summary>
        [XmlElement("xLocEmi", Namespace = Nfse.Ns)]
        public string XLocEmi { get; set; }

        /// <summary>
        /// Município de localização da prestação.
        /// </summary>
        [XmlElement("xLocPrestacao", Namespace = Nfse.Ns)]
        public string XLocPrestacao { get; set; }

        /// <summary>
        /// Número da NFS-e.
        /// </summary>
        [XmlElement("nNFSe", Namespace = Nfse.Ns)]
        public long NNFSe { get; set; }

        /// <summary>
        /// Código do município de incidência do imposto.
        /// </summary>
        [XmlElement("cLocIncid", Namespace = Nfse.Ns)]
        public long CLocIncid { get; set; }

        /// <summary>
        /// Descrição do município de incidência.
        /// </summary>
        [XmlElement("xLocIncid", Namespace = Nfse.Ns)]
        public string XLocIncid { get; set; }

        /// <summary>
        /// Tributação nacional.
        /// </summary>
        [XmlElement("xTribNac", Namespace = Nfse.Ns)]
        public string XTribNac { get; set; }

        /// <summary>
        /// Tributação municipal.
        /// </summary>
        [XmlElement("xTribMun", Namespace = Nfse.Ns)]
        public string XTribMun { get; set; }

        /// <summary>
        /// Nomenclatura Brasileira de Serviços.
        /// </summary>
        [XmlElement("xNBS", Namespace = Nfse.Ns)]
        public string XNBS { get; set; }

        /// <summary>
        /// Versão do aplicativo que gerou a NFS-e.
        /// </summary>
        [XmlElement("verAplic", Namespace = Nfse.Ns)]
        public string VerAplic { get; set; }

        /// <summary>
        /// Ambiente de geração: 1=Produção, 2=Homologação.
        /// </summary>
        [XmlElement("ambGer", Namespace = Nfse.Ns)]
        public int AmbGer { get; set; }

        /// <summary>
        /// Tipo de emissão: 1=Normal.
        /// </summary>
        [XmlElement("tpEmis", Namespace = Nfse.Ns)]
        public int TpEmis { get; set; }

        /// <summary>
        /// Processo de emissão: 1=Aplicativo do contribuinte.
        /// </summary>
        [XmlElement("procEmi", Namespace = Nfse.Ns)]
        public int ProcEmi { get; set; }

        /// <summary>
        /// Código do status da NFS-e.
        /// </summary>
        [XmlElement("cStat", Namespace = Nfse.Ns)]
        public int CStat { get; set; }

        /// <summary>
        /// Data e hora de processamento.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhProc { get; set; }
#else
        public DateTimeOffset DhProc { get; set; }
#endif

        [XmlElement("dhProc", Namespace = Nfse.Ns)]
        public string DhProcField
        {
            get => DhProc.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhProc = DateTime.Parse(value);
#else
            set => DhProc = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Número do Documento Fiscal de Serviços Eletrônico.
        /// </summary>
        [XmlElement("nDFSe", Namespace = Nfse.Ns)]
        public long NDFSe { get; set; }

        /// <summary>
        /// Dados do emitente.
        /// </summary>
        [XmlElement("emit", Namespace = Nfse.Ns)]
        public EmitNFSe Emit { get; set; }

        /// <summary>
        /// Valores da NFS-e.
        /// </summary>
        [XmlElement("valores", Namespace = Nfse.Ns)]
        public ValoresNFSe Valores { get; set; }

        /// <summary>
        /// Informações do IBS/CBS (Reforma Tributária).
        /// </summary>
        [XmlElement("IBSCBS", Namespace = Nfse.Ns)]
        public IBSCBSNFSe IBSCBS { get; set; }

        /// <summary>
        /// DPS que gerou a NFS-e.
        /// </summary>
        [XmlElement("DPS", Namespace = Nfse.Ns)]
        public DPS DPS { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCLocIncid() => CLocIncid > 0;
        public bool ShouldSerializeXLocIncid() => !string.IsNullOrWhiteSpace(XLocIncid);
        public bool ShouldSerializeXTribMun() => !string.IsNullOrWhiteSpace(XTribMun);
        public bool ShouldSerializeXNBS() => !string.IsNullOrWhiteSpace(XNBS);
        public bool ShouldSerializeProcEmi() => ProcEmi > 0;
        #endregion
    }

    #endregion

    #region Emitente

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.EmitNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("emit", Namespace = Nfse.Ns)]
    public class EmitNFSe
    {
        /// <summary>
        /// CNPJ do emitente.
        /// </summary>
        [XmlElement("CNPJ", Namespace = Nfse.Ns)]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente.
        /// </summary>
        [XmlElement("CPF", Namespace = Nfse.Ns)]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Municipal.
        /// </summary>
        [XmlElement("IM", Namespace = Nfse.Ns)]
        public string IM { get; set; }

        /// <summary>
        /// Razão social ou nome empresarial.
        /// </summary>
        [XmlElement("xNome", Namespace = Nfse.Ns)]
        public string XNome { get; set; }

        /// <summary>
        /// Nome fantasia.
        /// </summary>
        [XmlElement("xFant", Namespace = Nfse.Ns)]
        public string XFant { get; set; }

        /// <summary>
        /// Endereço nacional do emitente.
        /// </summary>
        [XmlElement("enderNac", Namespace = Nfse.Ns)]
        public EnderNac EnderNac { get; set; }

        /// <summary>
        /// Telefone.
        /// </summary>
        [XmlElement("fone", Namespace = Nfse.Ns)]
        public string Fone { get; set; }

        /// <summary>
        /// E-mail.
        /// </summary>
        [XmlElement("email", Namespace = Nfse.Ns)]
        public string Email { get; set; }

        #region Should Serialize
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.EnderNac")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("enderNac", Namespace = Nfse.Ns)]
    public class EnderNac
    {
        /// <summary>
        /// Logradouro.
        /// </summary>
        [XmlElement("xLgr", Namespace = Nfse.Ns)]
        public string XLgr { get; set; }

        /// <summary>
        /// Número.
        /// </summary>
        [XmlElement("nro", Namespace = Nfse.Ns)]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento.
        /// </summary>
        [XmlElement("xCpl", Namespace = Nfse.Ns)]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro.
        /// </summary>
        [XmlElement("xBairro", Namespace = Nfse.Ns)]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município (IBGE).
        /// </summary>
        [XmlElement("cMun", Namespace = Nfse.Ns)]
        public long CMun { get; set; }

        /// <summary>
        /// Sigla da UF.
        /// </summary>
        [XmlElement("UF", Namespace = Nfse.Ns)]
        public string UF { get; set; }

        /// <summary>
        /// CEP.
        /// </summary>
        [XmlElement("CEP", Namespace = Nfse.Ns)]
        public string CEP { get; set; }

        #region Should Serialize
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);
        #endregion
    }

    #endregion

    #region Valores

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ValoresNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class ValoresNFSe
    {
        [XmlIgnore]
        public double VCalcDR { get; set; }

        /// <summary>
        /// Valor de dedução/redução da BC do ISSQN.
        /// </summary>
        [XmlElement("vCalcDR", Namespace = Nfse.Ns)]
        public string VCalcDRField
        {
            get => VCalcDR.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcDR = Converter.ToDouble(value);
        }

        /// <summary>
        /// Tipo Benefício Municipal.
        /// </summary>
        [XmlElement("tpBM", Namespace = Nfse.Ns)]
        public int TpBM { get; set; }

        [XmlIgnore]
        public double VCalcBM { get; set; }

        /// <summary>
        /// Valor do cálculo do benefício municipal.
        /// </summary>
        [XmlElement("vCalcBM", Namespace = Nfse.Ns)]
        public string VCalcBMField
        {
            get => VCalcBM.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcBM = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Base de cálculo.
        /// </summary>
        [XmlElement("vBC", Namespace = Nfse.Ns)]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqAplic { get; set; }

        /// <summary>
        /// Alíquota aplicada.
        /// </summary>
        [XmlElement("pAliqAplic", Namespace = Nfse.Ns)]
        public string PAliqAplicField
        {
            get => PAliqAplic.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqAplic = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VISSQN { get; set; }

        /// <summary>
        /// Valor do ISSQN.
        /// </summary>
        [XmlElement("vISSQN", Namespace = Nfse.Ns)]
        public string VISSQNField
        {
            get => VISSQN.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSQN = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTotalRet { get; set; }

        /// <summary>
        /// Valor total de retenções.
        /// </summary>
        [XmlElement("vTotalRet", Namespace = Nfse.Ns)]
        public string VTotalRetField
        {
            get => VTotalRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotalRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VLiq { get; set; }

        /// <summary>
        /// Valor líquido da NFS-e.
        /// </summary>
        [XmlElement("vLiq", Namespace = Nfse.Ns)]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Converter.ToDouble(value);
        }

        /// <summary>
        /// Outras informações (uso da Administração Tributária Municipal).
        /// </summary>
        [XmlElement("xOutInf", Namespace = Nfse.Ns)]
        public string XOutInf { get; set; }

        #region Should Serialize
        public bool ShouldSerializeVCalcDRField() => VCalcDR > 0;
        public bool ShouldSerializeTpBM() => TpBM > 0;
        public bool ShouldSerializeVCalcBMField() => VCalcBM > 0;
        public bool ShouldSerializeVBCField() => VBC > 0;
        public bool ShouldSerializePAliqAplicField() => PAliqAplic > 0;
        public bool ShouldSerializeVISSQNField() => VISSQN > 0;
        public bool ShouldSerializeVTotalRetField() => VTotalRet > 0;
        public bool ShouldSerializeXOutInf() => !string.IsNullOrWhiteSpace(XOutInf);
        #endregion
    }

    #endregion

    #region IBSCBS

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.IBSCBSNFSe")]
    [ComVisible(true)]
#endif
    [Serializable]
    public class IBSCBSNFSe
    {
        /// <summary>
        /// Código da localidade de incidência.
        /// </summary>
        [XmlElement("cLocalidadeIncid", Namespace = Nfse.Ns)]
        public long CLocalidadeIncid { get; set; }

        /// <summary>
        /// Descrição da localidade de incidência.
        /// </summary>
        [XmlElement("xLocalidadeIncid", Namespace = Nfse.Ns)]
        public string XLocalidadeIncid { get; set; }

        [XmlIgnore]
        public double PRedutor { get; set; }

        /// <summary>
        /// Percentual de redução em compra governamental.
        /// </summary>
        [XmlElement("pRedutor", Namespace = Nfse.Ns)]
        public string PRedutorField
        {
            get => PRedutor.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedutor = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valores calculados de IBS/CBS.
        /// </summary>
        [XmlElement("valores", Namespace = Nfse.Ns)]
        public ValoresIBSCBS Valores { get; set; }

        /// <summary>
        /// Totalizadores de IBS/CBS.
        /// </summary>
        [XmlElement("totCIBS", Namespace = Nfse.Ns)]
        public TotCIBS TotCIBS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.ValoresIBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("valores", Namespace = Nfse.Ns)]
    public class ValoresIBSCBS
    {
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Base de cálculo.
        /// </summary>
        [XmlElement("vBC", Namespace = Nfse.Ns)]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCalcReeRepRes { get; set; }

        /// <summary>
        /// Valor de reembolso/repasse/ressarcimento.
        /// </summary>
        [XmlElement("vCalcReeRepRes", Namespace = Nfse.Ns)]
        public string VCalcReeRepResField
        {
            get => VCalcReeRepRes.ToString("F2", CultureInfo.InvariantCulture);
            set => VCalcReeRepRes = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valores da UF.
        /// </summary>
        [XmlElement("uf", Namespace = Nfse.Ns)]
        public IBSCBSUF UF { get; set; }

        /// <summary>
        /// Valores do município.
        /// </summary>
        [XmlElement("mun", Namespace = Nfse.Ns)]
        public IBSCBSMun Mun { get; set; }

        /// <summary>
        /// Valores federais.
        /// </summary>
        [XmlElement("fed", Namespace = Nfse.Ns)]
        public IBSCBSFed Fed { get; set; }

        #region Should Serialize
        public bool ShouldSerializeVCalcReeRepResField() => VCalcReeRepRes > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.IBSCBSUF")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("uf", Namespace = Nfse.Ns)]
    public class IBSCBSUF
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        /// <summary>
        /// Percentual IBS UF.
        /// </summary>
        [XmlElement("pIBSUF", Namespace = Nfse.Ns)]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqUF { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota UF.
        /// </summary>
        [XmlElement("pRedAliqUF", Namespace = Nfse.Ns)]
        public string PRedAliqUFField
        {
            get => PRedAliqUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetUF { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva UF.
        /// </summary>
        [XmlElement("pAliqEfetUF", Namespace = Nfse.Ns)]
        public string PAliqEfetUFField
        {
            get => PAliqEfetUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetUF = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqUFField() => PRedAliqUF > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.IBSCBSMun")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("mun", Namespace = Nfse.Ns)]
    public class IBSCBSMun
    {
        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Percentual IBS Municipal.
        /// </summary>
        [XmlElement("pIBSMun", Namespace = Nfse.Ns)]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqMun { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota municipal.
        /// </summary>
        [XmlElement("pRedAliqMun", Namespace = Nfse.Ns)]
        public string PRedAliqMunField
        {
            get => PRedAliqMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetMun { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva municipal.
        /// </summary>
        [XmlElement("pAliqEfetMun", Namespace = Nfse.Ns)]
        public string PAliqEfetMunField
        {
            get => PAliqEfetMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetMun = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqMunField() => PRedAliqMun > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.IBSCBSFed")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("fed", Namespace = Nfse.Ns)]
    public class IBSCBSFed
    {
        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Percentual CBS.
        /// </summary>
        [XmlElement("pCBS", Namespace = Nfse.Ns)]
        public string PCBSField
        {
            get => PCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedAliqCBS { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota CBS.
        /// </summary>
        [XmlElement("pRedAliqCBS", Namespace = Nfse.Ns)]
        public string PRedAliqCBSField
        {
            get => PRedAliqCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAliqCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetCBS { get; set; }

        /// <summary>
        /// Percentual alíquota efetiva CBS.
        /// </summary>
        [XmlElement("pAliqEfetCBS", Namespace = Nfse.Ns)]
        public string PAliqEfetCBSField
        {
            get => PAliqEfetCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfetCBS = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializePRedAliqCBSField() => PRedAliqCBS > 0;
        #endregion
    }

    #endregion

    #region Totalizadores

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.TotCIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("totCIBS", Namespace = Nfse.Ns)]
    public class TotCIBS
    {
        [XmlIgnore]
        public double VTotNF { get; set; }

        /// <summary>
        /// Valor total da nota fiscal.
        /// </summary>
        [XmlElement("vTotNF", Namespace = Nfse.Ns)]
        public string VTotNFField
        {
            get => VTotNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotNF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de totalizadores do IBS.
        /// </summary>
        [XmlElement("gIBS", Namespace = Nfse.Ns)]
        public GIBS GIBS { get; set; }

        /// <summary>
        /// Grupo de totalizadores do CBS.
        /// </summary>
        [XmlElement("gCBS", Namespace = Nfse.Ns)]
        public GCBS GCBS { get; set; }

        /// <summary>
        /// Grupo de tributação regular (opcional).
        /// </summary>
        [XmlElement("gTribRegular", Namespace = Nfse.Ns)]
        public GTribRegularRet GTribRegular { get; set; }

        /// <summary>
        /// Grupo de compra governamental (opcional).
        /// </summary>
        [XmlElement("gTribCompraGov", Namespace = Nfse.Ns)]
        public GTribCompraGov GTribCompraGov { get; set; }

        #region Should Serialize
        public bool ShouldSerializeGTribRegular() => GTribRegular != null;
        public bool ShouldSerializeGTribCompraGov() => GTribCompraGov != null;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GIBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gIBS", Namespace = Nfse.Ns)]
    public class GIBS
    {
        [XmlIgnore]
        public double VIBSTot { get; set; }

        /// <summary>
        /// Valor total do IBS.
        /// </summary>
        [XmlElement("vIBSTot", Namespace = Nfse.Ns)]
        public string VIBSTotField
        {
            get => VIBSTot.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSTot = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de crédito presumido IBS (opcional).
        /// </summary>
        [XmlElement("gIBSCredPres", Namespace = Nfse.Ns)]
        public GIBSCredPres GIBSCredPres { get; set; }

        /// <summary>
        /// Grupo de totalização IBS UF.
        /// </summary>
        [XmlElement("gIBSUFTot", Namespace = Nfse.Ns)]
        public GIBSUFTot GIBSUFTot { get; set; }

        /// <summary>
        /// Grupo de totalização IBS Municipal.
        /// </summary>
        [XmlElement("gIBSMunTot", Namespace = Nfse.Ns)]
        public GIBSMunTot GIBSMunTot { get; set; }

        #region Should Serialize
        public bool ShouldSerializeGIBSCredPres() => GIBSCredPres != null;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GIBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gIBSCredPres", Namespace = Nfse.Ns)]
    public class GIBSCredPres
    {
        [XmlIgnore]
        public double PCredPresIBS { get; set; }

        /// <summary>
        /// Alíquota do crédito presumido para IBS.
        /// </summary>
        [XmlElement("pCredPresIBS", Namespace = Nfse.Ns)]
        public string PCredPresIBSField
        {
            get => PCredPresIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCredPresIBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresIBS { get; set; }

        /// <summary>
        /// Valor do crédito presumido para IBS.
        /// </summary>
        [XmlElement("vCredPresIBS", Namespace = Nfse.Ns)]
        public string VCredPresIBSField
        {
            get => VCredPresIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresIBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GIBSUFTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gIBSUFTot", Namespace = Nfse.Ns)]
    public class GIBSUFTot
    {
        [XmlIgnore]
        public double VDifUF { get; set; }

        /// <summary>
        /// Valor de diferimento UF.
        /// </summary>
        [XmlElement("vDifUF", Namespace = Nfse.Ns)]
        public string VDifUFField
        {
            get => VDifUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Valor IBS UF.
        /// </summary>
        [XmlElement("vIBSUF", Namespace = Nfse.Ns)]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GIBSMunTot")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gIBSMunTot", Namespace = Nfse.Ns)]
    public class GIBSMunTot
    {
        [XmlIgnore]
        public double VDifMun { get; set; }

        /// <summary>
        /// Valor de diferimento Municipal.
        /// </summary>
        [XmlElement("vDifMun", Namespace = Nfse.Ns)]
        public string VDifMunField
        {
            get => VDifMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Valor IBS Municipal.
        /// </summary>
        [XmlElement("vIBSMun", Namespace = Nfse.Ns)]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GCBS")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gCBS", Namespace = Nfse.Ns)]
    public class GCBS
    {
        /// <summary>
        /// Grupo de crédito presumido CBS (opcional).
        /// </summary>
        [XmlElement("gCBSCredPres", Namespace = Nfse.Ns)]
        public GCBSCredPres GCBSCredPres { get; set; }

        [XmlIgnore]
        public double VDifCBS { get; set; }

        /// <summary>
        /// Valor de diferimento CBS.
        /// </summary>
        [XmlElement("vDifCBS", Namespace = Nfse.Ns)]
        public string VDifCBSField
        {
            get => VDifCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VDifCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Valor CBS.
        /// </summary>
        [XmlElement("vCBS", Namespace = Nfse.Ns)]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeGCBSCredPres() => GCBSCredPres != null;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GCBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gCBSCredPres", Namespace = Nfse.Ns)]
    public class GCBSCredPres
    {
        [XmlIgnore]
        public double PCredPresCBS { get; set; }

        /// <summary>
        /// Alíquota do crédito presumido para CBS.
        /// </summary>
        [XmlElement("pCredPresCBS", Namespace = Nfse.Ns)]
        public string PCredPresCBSField
        {
            get => PCredPresCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCredPresCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCBS { get; set; }

        /// <summary>
        /// Valor do crédito presumido para CBS.
        /// </summary>
        [XmlElement("vCredPresCBS", Namespace = Nfse.Ns)]
        public string VCredPresCBSField
        {
            get => VCredPresCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GTribRegularRet")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gTribRegular", Namespace = Nfse.Ns)]
    public class GTribRegularRet
    {
        [XmlIgnore]
        public double PAliqEfeRegIBSUF { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular do IBS estadual.
        /// </summary>
        [XmlElement("pAliqEfeRegIBSUF", Namespace = Nfse.Ns)]
        public string PAliqEfeRegIBSUFField
        {
            get => PAliqEfeRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSUF { get; set; }

        /// <summary>
        /// Valor da tributação regular do IBS estadual.
        /// </summary>
        [XmlElement("vTribRegIBSUF", Namespace = Nfse.Ns)]
        public string VTribRegIBSUFField
        {
            get => VTribRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfeRegIBSMun { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular do IBS municipal.
        /// </summary>
        [XmlElement("pAliqEfeRegIBSMun", Namespace = Nfse.Ns)]
        public string PAliqEfeRegIBSMunField
        {
            get => PAliqEfeRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSMun { get; set; }

        /// <summary>
        /// Valor da tributação regular do IBS municipal.
        /// </summary>
        [XmlElement("vTribRegIBSMun", Namespace = Nfse.Ns)]
        public string VTribRegIBSMunField
        {
            get => VTribRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfeRegCBS { get; set; }

        /// <summary>
        /// Alíquota efetiva de tributação regular da CBS.
        /// </summary>
        [XmlElement("pAliqEfeRegCBS", Namespace = Nfse.Ns)]
        public string PAliqEfeRegCBSField
        {
            get => PAliqEfeRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PAliqEfeRegCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        /// <summary>
        /// Valor da tributação regular da CBS.
        /// </summary>
        [XmlElement("vTribRegCBS", Namespace = Nfse.Ns)]
        public string VTribRegCBSField
        {
            get => VTribRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFSe.NACIONAL.GTribCompraGov")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlType("gTribCompraGov", Namespace = Nfse.Ns)]
    public class GTribCompraGov
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        /// <summary>
        /// Alíquota do IBS de competência do Estado.
        /// </summary>
        [XmlElement("pIBSUF", Namespace = Nfse.Ns)]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS da UF calculado.
        /// </summary>
        [XmlElement("vIBSUF", Namespace = Nfse.Ns)]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Alíquota do IBS de competência do Município.
        /// </summary>
        [XmlElement("pIBSMun", Namespace = Nfse.Ns)]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Valor do Tributo do IBS do Município calculado.
        /// </summary>
        [XmlElement("vIBSMun", Namespace = Nfse.Ns)]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Alíquota da CBS.
        /// </summary>
        [XmlElement("pCBS", Namespace = Nfse.Ns)]
        public string PCBSField
        {
            get => PCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Valor do Tributo da CBS calculado.
        /// </summary>
        [XmlElement("vCBS", Namespace = Nfse.Ns)]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }

    #endregion
}
