#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.BPe
{
    /// <summary>
    /// BP-e TM - Bilhete de Passagem Eletronico Transporte Metropolitano
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.BPeTM")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BPeTM", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class BPeTM : XMLBase
    {
        /// <summary>
        /// Informacoes do BP-e TM
        /// </summary>
        [XmlElement("infBPe")]
        public InfBPe InfBPe { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfBPe")]
    [ComVisible(true)]
#endif
    public class InfBPe
    {
        private string idField;
        private string chaveField;

        /// <summary>
        /// Versao do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificador da tag a ser assinada
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => string.IsNullOrWhiteSpace(idField) ? "BPe" + Chave : idField;
            set => idField = value;
        }

        /// <summary>
        /// Chave de acesso do BP-e
        /// </summary>
        [XmlIgnore]
        public string Chave
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(chaveField))
                {
                    return chaveField;
                }

                if (!string.IsNullOrWhiteSpace(idField) && idField.StartsWith("BPe", StringComparison.OrdinalIgnoreCase))
                {
                    chaveField = idField.Substring(3);
                    return chaveField;
                }

                if (Ide == null)
                {
                    throw new NullReferenceException("A propriedade 'Ide' esta nula.");
                }

                if (Emit == null)
                {
                    throw new NullReferenceException("A propriedade 'Emit' esta nula.");
                }

                if (string.IsNullOrWhiteSpace(Emit.CNPJ))
                {
                    throw new NullReferenceException("Emit.CNPJ nao foi informado.");
                }

                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = Ide.CUF,
                    AnoEmissao = Ide.DhEmi.ToString("yy"),
                    MesEmissao = Ide.DhEmi.ToString("MM"),
                    CNPJCPFEmissor = Emit.CNPJ.PadLeft(14, '0'),
                    Modelo = Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NBP,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    CodigoNumerico = Ide.CBP
                };

                chaveField = XMLUtility.MontarChaveBPe(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

                return chaveField;
            }

            set => chaveField = value;
        }

        /// <summary>
        /// Identificacao do BP-e TM
        /// </summary>
        [XmlElement("ide")]
        public Ide Ide { get; set; }

        /// <summary>
        /// Identificacao do emitente
        /// </summary>
        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>
        /// Grupo de informacoes detalhadas do BP-e TM
        /// </summary>
        [XmlElement("detBPeTM")]
        public List<DetBPeTM> DetBPeTM { get; set; }

#if INTEROP
        public void AddDetBPeTM(DetBPeTM item) => (DetBPeTM ?? (DetBPeTM = new List<DetBPeTM>())).Add(item);
        public DetBPeTM GetDetBPeTM(int index) => (DetBPeTM?.Count ?? 0) == 0 ? default : DetBPeTM[index];
        public int GetDetBPeTMCount => DetBPeTM != null ? DetBPeTM.Count : 0;
#endif

        /// <summary>
        /// Totais do BP-e TM
        /// </summary>
        [XmlElement("total")]
        public Total Total { get; set; }

        /// <summary>
        /// Informacoes da vinculacao com transacao de pagamento
        /// </summary>
        [XmlElement("pgtoVinc")]
        public PgtoVinc PgtoVinc { get; set; }

        /// <summary>
        /// Autorizados para download do XML do DF-e
        /// </summary>
        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

#if INTEROP
        public void AddAutXML(AutXML item) => (AutXML ?? (AutXML = new List<AutXML>())).Add(item);
        public AutXML GetAutXML(int index) => (AutXML?.Count ?? 0) == 0 ? default : AutXML[index];
        public int GetAutXMLCount => AutXML != null ? AutXML.Count : 0;
#endif

        /// <summary>
        /// Informacoes adicionais
        /// </summary>
        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        /// <summary>
        /// Informacoes do responsavel tecnico
        /// </summary>
        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        public bool ShouldSerializePgtoVinc() => PgtoVinc != null;
        public bool ShouldSerializeAutXML() => AutXML?.Count > 0;
        public bool ShouldSerializeInfAdic() => InfAdic != null;
        public bool ShouldSerializeInfRespTec() => InfRespTec != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Ide")]
    [ComVisible(true)]
#endif
    public class Ide
    {
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nBP")]
        public int NBP { get; set; }

        [XmlElement("cBP")]
        public string CBP { get; set; }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("modal")]
        public ModalidadeTransporteBPe Modal { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value);
#else
            set => DhEmi = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
        public DateTime DCompet { get; set; }

        [XmlElement("dCompet")]
        public string DCompetField
        {
            get => DCompet.ToString("yyyy-MM-dd");
            set => DCompet = DateTime.Parse(value);
        }

        [XmlElement("tpEmis")]
        public TipoEmissaoBPe TpEmis { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("tpBPe")]
        public TipoBPe TpBPe { get; set; }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset? DhCont { get; set; }
#endif

        [XmlElement("dhCont")]
        public string DhContField
        {
#if INTEROP
            get => DhCont.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhCont = DateTime.Parse(value);
#else
            get => DhCont.HasValue ? DhCont.Value.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
            set => DhCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xJust")]
        public string XJust { get; set; }

        [XmlElement("gCompraGov")]
        public CompraGovReduzido GCompraGov { get; set; }

#if INTEROP
        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;
#else
        public bool ShouldSerializeDhContField() => DhCont != null;
#endif
        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);
        public bool ShouldSerializeGCompraGov() => GCompraGov != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Emit")]
    [ComVisible(true)]
#endif
    public class Emit
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IEST")]
        public string IEST { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("CNAE")]
        public string CNAE { get; set; }

        [XmlElement("CRT")]
        public CRT CRT { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        [XmlElement("TAR")]
        public string TAR { get; set; }

        [XmlElement("ISUFEmit")]
        public string ISUFEmit { get; set; }

        public bool ShouldSerializeIEST() => !string.IsNullOrWhiteSpace(IEST);
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);
        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);
        public bool ShouldSerializeCNAE() => !string.IsNullOrWhiteSpace(CNAE);
        public bool ShouldSerializeTAR() => !string.IsNullOrWhiteSpace(TAR);
        public bool ShouldSerializeISUFEmit() => !string.IsNullOrWhiteSpace(ISUFEmit);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.EnderEmit")]
    [ComVisible(true)]
#endif
    public class EnderEmit
    {
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        [XmlElement("nro")]
        public string Nro { get; set; }

        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        [XmlElement("cMun")]
        public string CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.CompraGovReduzido")]
    [ComVisible(true)]
#endif
    public class CompraGovReduzido
    {
        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamental TpEnteGov { get; set; }

        [XmlIgnore]
        public double PRedutor { get; set; }

        [XmlElement("pRedutor")]
        public string PRedutorField
        {
            get => BPeFormat.Format4(PRedutor);
            set => PRedutor = Converter.ToDouble(value);
        }

        [XmlElement("tpOperGov")]
        public TipoOperacaoEnteGovernamental TpOperGov { get; set; }

        [XmlElement("refDFeAnt")]
        public List<string> RefDFeAnt { get; set; }

#if INTEROP
        public void AddRefDFeAnt(string item) => (RefDFeAnt ?? (RefDFeAnt = new List<string>())).Add(item);
        public string GetRefDFeAnt(int index) => (RefDFeAnt?.Count ?? 0) == 0 ? default : RefDFeAnt[index];
        public int GetRefDFeAntCount => RefDFeAnt != null ? RefDFeAnt.Count : 0;
#endif

        public bool ShouldSerializeRefDFeAnt() => RefDFeAnt?.Count > 0;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.DetBPeTM")]
    [ComVisible(true)]
#endif
    public class DetBPeTM
    {
        [XmlAttribute(AttributeName = "idEqpCont")]
        public int IdEqpCont { get; set; }

        [XmlElement("UFIniViagem")]
        public UFBrasil UFIniViagem { get; set; }

        [XmlElement("UFFimViagem")]
#if INTEROP
        public UFBrasil UFFimViagem { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? UFFimViagem { get; set; }
#endif

        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("prefixo")]
        public string Prefixo { get; set; }

        [XmlElement("det")]
        public List<Det> Det { get; set; }

#if INTEROP
        public void AddDet(Det item) => (Det ?? (Det = new List<Det>())).Add(item);
        public Det GetDet(int index) => (Det?.Count ?? 0) == 0 ? default : Det[index];
        public int GetDetCount => Det != null ? Det.Count : 0;
#endif

#if INTEROP
        public bool ShouldSerializeUFFimViagem() => UFFimViagem != (UFBrasil)(-1);
#else
        public bool ShouldSerializeUFFimViagem() => UFFimViagem != null;
#endif
        public bool ShouldSerializePlaca() => !string.IsNullOrWhiteSpace(Placa);
        public bool ShouldSerializePrefixo() => !string.IsNullOrWhiteSpace(Prefixo);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Det")]
    [ComVisible(true)]
#endif
    public class Det
    {
        [XmlAttribute(AttributeName = "nViagem")]
        public int NViagem { get; set; }

        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        [XmlElement("nContInicio")]
        public string NContInicio { get; set; }

        [XmlElement("nContFim")]
        public string NContFim { get; set; }

        [XmlElement("qPass")]
#if INTEROP
        public string QPass { get; set; }
#else
        public long QPass { get; set; }
#endif

        [XmlIgnore]
        public double VBP { get; set; }

        [XmlElement("vBP")]
        public string VBPField
        {
            get => BPeFormat.Format2(VBP);
            set => VBP = Converter.ToDouble(value);
        }

        [XmlElement("imp")]
        public Imp Imp { get; set; }

        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

#if INTEROP
        public void AddComp(Comp item) => (Comp ?? (Comp = new List<Comp>())).Add(item);
        public Comp GetComp(int index) => (Comp?.Count ?? 0) == 0 ? default : Comp[index];
        public int GetCompCount => Comp != null ? Comp.Count : 0;
#endif

        public bool ShouldSerializeCMunFim() => !string.IsNullOrWhiteSpace(CMunFim);
        public bool ShouldSerializeNContInicio() => !string.IsNullOrWhiteSpace(NContInicio);
        public bool ShouldSerializeNContFim() => !string.IsNullOrWhiteSpace(NContFim);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Imp")]
    [ComVisible(true)]
#endif
    public class Imp
    {
        [XmlElement("ICMS")]
        public ICMS ICMS { get; set; }

        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);
        public bool ShouldSerializeIBSCBS() => IBSCBS != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMS")]
    [ComVisible(true)]
#endif
    public class ICMS
    {
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        [XmlElement("ICMS45")]
        public ICMS45 ICMS45 { get; set; }

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("ICMSSN")]
        public ICMSSN ICMSSN { get; set; }

        public bool ShouldSerializeICMS00() => ICMS00 != null;
        public bool ShouldSerializeICMS20() => ICMS20 != null;
        public bool ShouldSerializeICMS45() => ICMS45 != null;
        public bool ShouldSerializeICMS90() => ICMS90 != null;
        public bool ShouldSerializeICMSSN() => ICMSSN != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMS00")]
    [ComVisible(true)]
#endif
    public class ICMS00
    {
        [XmlElement("CST")]
        public CSTICMSBPe CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => BPeFormat.Format2(VBC);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => BPeFormat.Format2(PICMS);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => BPeFormat.Format2(VICMS);
            set => VICMS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMS20")]
    [ComVisible(true)]
#endif
    public class ICMS20
    {
        [XmlElement("CST")]
        public CSTICMSBPe CST { get; set; }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => BPeFormat.Format2(PRedBC);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => BPeFormat.Format2(VBC);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => BPeFormat.Format2(PICMS);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => BPeFormat.Format2(VICMS);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => BPeFormat.Format2(VICMSDeson);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson != null;
        public bool ShouldSerializeCBenef() => !string.IsNullOrWhiteSpace(CBenef);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMS45")]
    [ComVisible(true)]
#endif
    public class ICMS45
    {
        [XmlElement("CST")]
        public CSTICMSBPe CST { get; set; }

        [XmlIgnore]
        public double? VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => BPeFormat.Format2(VICMSDeson);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson != null;
        public bool ShouldSerializeCBenef() => !string.IsNullOrWhiteSpace(CBenef);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMS90")]
    [ComVisible(true)]
#endif
    public class ICMS90
    {
        [XmlElement("CST")]
        public CSTICMSBPe CST { get; set; }

        [XmlIgnore]
        public double? PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => BPeFormat.Format2(PRedBC);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => BPeFormat.Format2(VBC);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => BPeFormat.Format2(PICMS);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => BPeFormat.Format2(VICMS);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VCred { get; set; }

        [XmlElement("vCred")]
        public string VCredField
        {
            get => BPeFormat.Format2(VCred);
            set => VCred = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => BPeFormat.Format2(VICMSDeson);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        public bool ShouldSerializePRedBCField() => PRedBC != null;
        public bool ShouldSerializeVCredField() => VCred != null;
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson != null;
        public bool ShouldSerializeCBenef() => !string.IsNullOrWhiteSpace(CBenef);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMSSN")]
    [ComVisible(true)]
#endif
    public class ICMSSN
    {
        [XmlElement("CST")]
        public CSTICMSBPe CST { get; set; }

        [XmlElement("indSN")]
        public IndicadorSimplesNacionalBPe IndSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.IBSCBS")]
    [ComVisible(true)]
#endif
    public class IBSCBS
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        [XmlElement("indDoacao")]
#if INTEROP
        public IndicadorDoacao IndDoacao { get; set; } = (IndicadorDoacao)(-1);
#else
        public IndicadorDoacao? IndDoacao { get; set; }
#endif

        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }

        [XmlElement("gEstornoCred")]
        public GEstornoCred GEstornoCred { get; set; }

#if INTEROP
        public bool ShouldSerializeIndDoacao() => IndDoacao != (IndicadorDoacao)(-1);
#else
        public bool ShouldSerializeIndDoacao() => IndDoacao != null;
#endif

        public bool ShouldSerializeGEstornoCred() => GEstornoCred != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBSCBS")]
    [ComVisible(true)]
#endif
    public class GIBSCBS
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => BPeFormat.Format2(VBC);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }

        [XmlIgnore]
        public double VIBS { get; set; }

        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => BPeFormat.Format2(VIBS);
            set => VIBS = Converter.ToDouble(value);
        }

        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }

        public bool ShouldSerializeGTribRegular() => GTribRegular != null;
        public bool ShouldSerializeGTribCompraGov() => GTribCompraGov != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBSUF")]
    [ComVisible(true)]
#endif
    public class GIBSUF
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => BPeFormat.Format4(PIBSUF);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => BPeFormat.Format2(VIBSUF);
            set => VIBSUF = Converter.ToDouble(value);
        }

        public bool ShouldSerializeGDif() => GDif != null;
        public bool ShouldSerializeGDevTrib() => GDevTrib != null;
        public bool ShouldSerializeGRed() => GRed != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBSMun")]
    [ComVisible(true)]
#endif
    public class GIBSMun
    {
        [XmlIgnore]
        public double PIBSMun { get; set; }

        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => BPeFormat.Format4(PIBSMun);
            set => PIBSMun = Converter.ToDouble(value);
        }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => BPeFormat.Format2(VIBSMun);
            set => VIBSMun = Converter.ToDouble(value);
        }

        public bool ShouldSerializeGDif() => GDif != null;
        public bool ShouldSerializeGDevTrib() => GDevTrib != null;
        public bool ShouldSerializeGRed() => GRed != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GCBS")]
    [ComVisible(true)]
#endif
    public class GCBS
    {
        [XmlIgnore]
        public double PCBS { get; set; }

        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => BPeFormat.Format4(PCBS);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlElement("gALCZFMCBS")]
        public GALCZFMCBS GALCZFMCBS { get; set; }

        [XmlIgnore]
        public double VCBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => BPeFormat.Format2(VCBS);
            set => VCBS = Converter.ToDouble(value);
        }

        public bool ShouldSerializeGDif() => GDif != null;
        public bool ShouldSerializeGDevTrib() => GDevTrib != null;
        public bool ShouldSerializeGRed() => GRed != null;
        public bool ShouldSerializeGALCZFMCBS() => GALCZFMCBS != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GDif")]
    [ComVisible(true)]
#endif
    public class GDif
    {
        [XmlIgnore]
        public double PDif { get; set; }

        [XmlElement("pDif")]
        public string PDifField
        {
            get => BPeFormat.Format4(PDif);
            set => PDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => BPeFormat.Format2(VDif);
            set => VDif = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GDevTrib")]
    [ComVisible(true)]
#endif
    public class GDevTrib
    {
        [XmlIgnore]
        public double? PDevTrib { get; set; }

        [XmlElement("pDevTrib")]
        public string PDevTribField
        {
            get => BPeFormat.Format4(PDevTrib);
            set => PDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => BPeFormat.Format2(VDevTrib);
            set => VDevTrib = Converter.ToDouble(value);
        }

        public bool ShouldSerializePDevTribField() => PDevTrib != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GRed")]
    [ComVisible(true)]
#endif
    public class GRed
    {
        [XmlIgnore]
        public double PRedAliq { get; set; }

        [XmlElement("pRedAliq")]
        public string PRedAliqField
        {
            get => BPeFormat.Format4(PRedAliq);
            set => PRedAliq = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfet { get; set; }

        [XmlElement("pAliqEfet")]
        public string PAliqEfetField
        {
            get => BPeFormat.Format4(PAliqEfet);
            set => PAliqEfet = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GEstornoCred")]
    [ComVisible(true)]
#endif
    public class GEstornoCred
    {
        [XmlIgnore]
        public double VIBSEstCred { get; set; }

        [XmlElement("vIBSEstCred")]
        public string VIBSEstCredField
        {
            get => BPeFormat.Format2(VIBSEstCred);
            set => VIBSEstCred = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBSEstCred { get; set; }

        [XmlElement("vCBSEstCred")]
        public string VCBSEstCredField
        {
            get => BPeFormat.Format2(VCBSEstCred);
            set => VCBSEstCred = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GALCZFMCBS")]
    [ComVisible(true)]
#endif
    public class GALCZFMCBS
    {
        [XmlIgnore]
        public double PAliqEfetRegCBS { get; set; }

        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBSField
        {
            get => BPeFormat.Format4(PAliqEfetRegCBS);
            set => PAliqEfetRegCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => BPeFormat.Format2(VTribRegCBS);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GTribRegular")]
    [ComVisible(true)]
#endif
    public class GTribRegular
    {
        [XmlElement("CSTReg")]
        public string CSTReg { get; set; }

        [XmlElement("cClassTribReg")]
        public string CClassTribReg { get; set; }

        [XmlIgnore]
        public double PAliqEfetRegIBSUF { get; set; }

        [XmlElement("pAliqEfetRegIBSUF")]
        public string PAliqEfetRegIBSUFField
        {
            get => BPeFormat.Format4(PAliqEfetRegIBSUF);
            set => PAliqEfetRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSUF { get; set; }

        [XmlElement("vTribRegIBSUF")]
        public string VTribRegIBSUFField
        {
            get => BPeFormat.Format2(VTribRegIBSUF);
            set => VTribRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetRegIBSMun { get; set; }

        [XmlElement("pAliqEfetRegIBSMun")]
        public string PAliqEfetRegIBSMunField
        {
            get => BPeFormat.Format4(PAliqEfetRegIBSMun);
            set => PAliqEfetRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegIBSMun { get; set; }

        [XmlElement("vTribRegIBSMun")]
        public string VTribRegIBSMunField
        {
            get => BPeFormat.Format2(VTribRegIBSMun);
            set => VTribRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetRegCBS { get; set; }

        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBSField
        {
            get => BPeFormat.Format4(PAliqEfetRegCBS);
            set => PAliqEfetRegCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => BPeFormat.Format2(VTribRegCBS);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GTribCompraGov")]
    [ComVisible(true)]
#endif
    public class GTribCompraGov
    {
        [XmlIgnore]
        public double PAliqIBSUF { get; set; }

        [XmlElement("pAliqIBSUF")]
        public string PAliqIBSUFField
        {
            get => BPeFormat.Format4(PAliqIBSUF);
            set => PAliqIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribIBSUF { get; set; }

        [XmlElement("vTribIBSUF")]
        public string VTribIBSUFField
        {
            get => BPeFormat.Format2(VTribIBSUF);
            set => VTribIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqIBSMun { get; set; }

        [XmlElement("pAliqIBSMun")]
        public string PAliqIBSMunField
        {
            get => BPeFormat.Format4(PAliqIBSMun);
            set => PAliqIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribIBSMun { get; set; }

        [XmlElement("vTribIBSMun")]
        public string VTribIBSMunField
        {
            get => BPeFormat.Format2(VTribIBSMun);
            set => VTribIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqCBS { get; set; }

        [XmlElement("pAliqCBS")]
        public string PAliqCBSField
        {
            get => BPeFormat.Format4(PAliqCBS);
            set => PAliqCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribCBS { get; set; }

        [XmlElement("vTribCBS")]
        public string VTribCBSField
        {
            get => BPeFormat.Format2(VTribCBS);
            set => VTribCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Comp")]
    [ComVisible(true)]
#endif
    public class Comp
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("qComp")]
        public string QComp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Total")]
    [ComVisible(true)]
#endif
    public class Total
    {
        [XmlElement("qPass")]
        public int QPass { get; set; }

        [XmlIgnore]
        public double VBP { get; set; }

        [XmlElement("vBP")]
        public string VBPField
        {
            get => BPeFormat.Format2(VBP);
            set => VBP = Converter.ToDouble(value);
        }

        [XmlElement("ICMSTot")]
        public ICMSTot ICMSTot { get; set; }

        [XmlElement("IBSCBSTot")]
        public IBSCBSTot IBSCBSTot { get; set; }

        [XmlIgnore]
        public double? VTotDFe { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => BPeFormat.Format2(VTotDFe);
            set => VTotDFe = Converter.ToDouble(value);
        }

        public bool ShouldSerializeIBSCBSTot() => IBSCBSTot != null;
        public bool ShouldSerializeVTotDFeField() => VTotDFe != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.ICMSTot")]
    [ComVisible(true)]
#endif
    public class ICMSTot
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => BPeFormat.Format2(VBC);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => BPeFormat.Format2(VICMS);
            set => VICMS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.IBSCBSTot")]
    [ComVisible(true)]
#endif
    public class IBSCBSTot
    {
        [XmlIgnore]
        public double VBCIBSCBS { get; set; }

        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBSField
        {
            get => BPeFormat.Format2(VBCIBSCBS);
            set => VBCIBSCBS = Converter.ToDouble(value);
        }

        [XmlElement("gIBS")]
        public GIBS GIBS { get; set; }

        [XmlElement("gCBS")]
        public GCBSTot GCBS { get; set; }

        [XmlElement("gEstornoCred")]
        public GEstornoCred GEstornoCred { get; set; }

        public bool ShouldSerializeGEstornoCred() => GEstornoCred != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBS")]
    [ComVisible(true)]
#endif
    public class GIBS
    {
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUF { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMun { get; set; }

        [XmlIgnore]
        public double VIBS { get; set; }

        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => BPeFormat.Format2(VIBS);
            set => VIBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBSUFTot")]
    [ComVisible(true)]
#endif
    public class GIBSUFTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => BPeFormat.Format2(VDif);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => BPeFormat.Format2(VDevTrib);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => BPeFormat.Format2(VIBSUF);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GIBSMunTot")]
    [ComVisible(true)]
#endif
    public class GIBSMunTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => BPeFormat.Format2(VDif);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => BPeFormat.Format2(VDevTrib);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => BPeFormat.Format2(VIBSMun);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.GCBSTot")]
    [ComVisible(true)]
#endif
    public class GCBSTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => BPeFormat.Format2(VDif);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => BPeFormat.Format2(VDevTrib);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => BPeFormat.Format2(VCBS);
            set => VCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.PgtoVinc")]
    [ComVisible(true)]
#endif
    public class PgtoVinc
    {
        [XmlElement("pgto")]
        public List<Pgto> Pgto { get; set; }

#if INTEROP
        public void AddPgto(Pgto item) => (Pgto ?? (Pgto = new List<Pgto>())).Add(item);
        public Pgto GetPgto(int index) => (Pgto?.Count ?? 0) == 0 ? default : Pgto[index];
        public int GetPgtoCount => Pgto != null ? Pgto.Count : 0;
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.Pgto")]
    [ComVisible(true)]
#endif
    public class Pgto
    {
        [XmlAttribute(AttributeName = "nPag")]
        public string NPag { get; set; }

        [XmlAttribute(AttributeName = "idTransacao")]
        public string IdTransacao { get; set; }

        [XmlElement("tpMeioPgto")]
        public MeioPagamento TpMeioPgto { get; set; }

        [XmlElement("CNPJReceb")]
        public string CNPJReceb { get; set; }

        [XmlElement("CNPJBasePSP")]
        public string CNPJBasePSP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.AutXML")]
    [ComVisible(true)]
#endif
    public class AutXML
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfAdic")]
    [ComVisible(true)]
#endif
    public class InfAdic
    {
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("infCpl")]
        public string InfCpl { get; set; }

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);
        public bool ShouldSerializeInfCpl() => !string.IsNullOrWhiteSpace(InfCpl);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPe.InfRespTec")]
    [ComVisible(true)]
#endif
    public class InfRespTec
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xContato")]
        public string XContato { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("idCSRT")]
        public string IdCSRT { get; set; }

        [XmlElement("hashCSRT")]
        public string HashCSRT { get; set; }

        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);
        public bool ShouldSerializeHashCSRT() => !string.IsNullOrWhiteSpace(HashCSRT);
    }

    internal static class BPeFormat
    {
        public static string Format2(double value) => value.ToString("F2", CultureInfo.InvariantCulture);

        public static string Format2(double? value) => value.HasValue ? value.Value.ToString("F2", CultureInfo.InvariantCulture) : null;

        public static string Format4(double value) => value.ToString("F4", CultureInfo.InvariantCulture);

        public static string Format4(double? value) => value.HasValue ? value.Value.ToString("F4", CultureInfo.InvariantCulture) : null;
    }
}
