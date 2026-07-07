#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using Unimake.Business.DFe.Xml.BPeTM;

namespace Unimake.Business.DFe.Xml.BPeTA
{
    /// <summary>
    /// BP-e TA - Bilhete de Passagem Eletronico Transporte Aereo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.BPeTA")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("BPeTA", Namespace = "http://www.portalfiscal.inf.br/bpe", IsNullable = false)]
    public class BPeTA : XMLBase
    {
        [XmlElement("infBPe")]
        public InfBPe InfBPe { get; set; }

        [XmlElement("infBPeSupl")]
        public InfBPeSupl InfBPeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfBPe")]
    [ComVisible(true)]
#endif
    public class InfBPe
    {
        private string idField;
        private string chaveField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => string.IsNullOrWhiteSpace(idField) ? "BPe" + Chave : idField;
            set => idField = value;
        }

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

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("comp")]
        public Comp Comp { get; set; }

        [XmlElement("agencia")]
        public Agencia Agencia { get; set; }

        [XmlElement("infBPeSub")]
        public InfBPeSub InfBPeSub { get; set; }

        [XmlElement("infPassagem")]
        public InfPassagem InfPassagem { get; set; }

        [XmlElement("infViagem")]
        public List<InfViagem> InfViagem { get; set; }

#if INTEROP
        public void AddInfViagem(InfViagem item) => (InfViagem ?? (InfViagem = new List<InfViagem>())).Add(item);
        public InfViagem GetInfViagem(int index) => (InfViagem?.Count ?? 0) == 0 ? default : InfViagem[index];
        public int GetInfViagemCount => InfViagem != null ? InfViagem.Count : 0;
#endif

        [XmlElement("infValorBPe")]
        public InfValorBPe InfValorBPe { get; set; }

        [XmlElement("imp")]
        public Imp Imp { get; set; }

        [XmlElement("pag")]
        public List<Pag> Pag { get; set; }

#if INTEROP
        public void AddPag(Pag item) => (Pag ?? (Pag = new List<Pag>())).Add(item);
        public Pag GetPag(int index) => (Pag?.Count ?? 0) == 0 ? default : Pag[index];
        public int GetPagCount => Pag != null ? Pag.Count : 0;
#endif

        [XmlElement("pgtoVinc")]
        public PgtoVinc PgtoVinc { get; set; }

        [XmlElement("infBPeVinculado")]
        public InfBPeVinculado InfBPeVinculado { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

#if INTEROP
        public void AddAutXML(AutXML item) => (AutXML ?? (AutXML = new List<AutXML>())).Add(item);
        public AutXML GetAutXML(int index) => (AutXML?.Count ?? 0) == 0 ? default : AutXML[index];
        public int GetAutXMLCount => AutXML != null ? AutXML.Count : 0;
#endif

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        public bool ShouldSerializeComp() => Comp != null;
        public bool ShouldSerializeAgencia() => Agencia != null;
        public bool ShouldSerializeInfBPeSub() => InfBPeSub != null;
        public bool ShouldSerializePag() => Pag?.Count > 0;
        public bool ShouldSerializePgtoVinc() => PgtoVinc != null;
        public bool ShouldSerializeInfBPeVinculado() => InfBPeVinculado != null;
        public bool ShouldSerializeAutXML() => AutXML?.Count > 0;
        public bool ShouldSerializeInfAdic() => InfAdic != null;
        public bool ShouldSerializeInfRespTec() => InfRespTec != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Ide")]
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
        public ModalidadeTransporteBPeTA Modal { get; set; }

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

        [XmlElement("tpEmis")]
        public TipoEmissaoBPe TpEmis { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("tpBPe")]
        public TipoBPe TpBPe { get; set; }

        [XmlElement("tpCompra")]
        public TipoCompraBPeTA TpCompra { get; set; }

        [XmlElement("indPres")]
        public IndicadorPresenca IndPres { get; set; }

        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; }

        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        [XmlElement("nroBPIATA")]
        public string NroBPIATA { get; set; }

        [XmlElement("gCompraGov")]
        public CompraGovReduzido GCompraGov { get; set; }

        public bool ShouldSerializeNroBPIATA() => !string.IsNullOrWhiteSpace(NroBPIATA);
        public bool ShouldSerializeGCompraGov() => GCompraGov != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Comp")]
    [ComVisible(true)]
#endif
    public class Comp
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("enderComp")]
        public EnderComp EnderComp { get; set; }

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Agencia")]
    [ComVisible(true)]
#endif
    public class Agencia
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("enderAgencia")]
        public EnderAgencia EnderAgencia { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.EnderComp")]
    [ComVisible(true)]
#endif
    public class EnderComp
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
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.EnderAgencia")]
    [ComVisible(true)]
#endif
    public class EnderAgencia : EnderComp
    {
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfBPeSub")]
    [ComVisible(true)]
#endif
    public class InfBPeSub
    {
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }

        [XmlElement("tpSub")]
        public TipoSubstituicaoTABPe TpSub { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfPassagem")]
    [ComVisible(true)]
#endif
    public class InfPassagem
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmb { get; set; }
#else
        public DateTimeOffset DhEmb { get; set; }
#endif

        [XmlElement("dhEmb")]
        public string DhEmbField
        {
            get => DhEmb.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmb = DateTime.Parse(value);
#else
            set => DhEmb = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhValidade { get; set; }
#else
        public DateTimeOffset DhValidade { get; set; }
#endif

        [XmlElement("dhValidade")]
        public string DhValidadeField
        {
            get => DhValidade.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhValidade = DateTime.Parse(value);
#else
            set => DhValidade = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("infPassageiro")]
        public InfPassageiro InfPassageiro { get; set; }

        public bool ShouldSerializeInfPassageiro() => InfPassageiro != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfPassageiro")]
    [ComVisible(true)]
#endif
    public class InfPassageiro
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("tpDoc")]
        public TipoDocumentoPassageiroBPe TpDoc { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlElement("xDoc")]
        public string XDoc { get; set; }

        [XmlIgnore]
        public DateTime? DNasc { get; set; }

        [XmlElement("dNasc")]
        public string DNascField
        {
            get => DNasc.HasValue ? DNasc.Value.ToString("yyyy-MM-dd") : null;
            set => DNasc = DateTime.Parse(value);
        }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeXDoc() => !string.IsNullOrWhiteSpace(XDoc);
        public bool ShouldSerializeDNascField() => DNasc.HasValue;
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfViagem")]
    [ComVisible(true)]
#endif
    public class InfViagem
    {
        [XmlElement("nroVoo")]
        public string NroVoo { get; set; }

        [XmlElement("SiglaCiaOperVoo")]
        public string SiglaCiaOperVoo { get; set; }

        [XmlElement("tpViagem")]
        public TipoViagemBPeTA TpViagem { get; set; }

        [XmlElement("cAeroOrig")]
        public string CAeroOrig { get; set; }

        [XmlElement("cAeroDest")]
        public string CAeroDest { get; set; }

        [XmlElement("tpServ")]
        public TipoServicoBPeTA TpServ { get; set; }

        [XmlElement("tpAcomodacao")]
        public TipoAcomodacaoBPeTA TpAcomodacao { get; set; }

        [XmlElement("tpTrecho")]
        public TipoTrechoBPeTA TpTrecho { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhViagem { get; set; }
#else
        public DateTimeOffset DhViagem { get; set; }
#endif

        [XmlElement("dhViagem")]
        public string DhViagemField
        {
            get => DhViagem.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhViagem = DateTime.Parse(value);
#else
            set => DhViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DhConexao { get; set; }
#else
        public DateTimeOffset? DhConexao { get; set; }
#endif

        [XmlElement("dhConexao")]
        public string DhConexaoField
        {
#if INTEROP
            get => DhConexao.ToString("yyyy-MM-ddTHH:mm:sszzz");
            set => DhConexao = DateTime.Parse(value);
#else
            get => DhConexao.HasValue ? DhConexao.Value.ToString("yyyy-MM-ddTHH:mm:sszzz") : null;
            set => DhConexao = DateTimeOffset.Parse(value);
#endif
        }

#if INTEROP
        public bool ShouldSerializeDhConexaoField() => DhConexao > DateTime.MinValue;
#else
        public bool ShouldSerializeDhConexaoField() => DhConexao != null;
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfValorBPe")]
    [ComVisible(true)]
#endif
    public class InfValorBPe
    {
        [XmlIgnore]
        public double VBP { get; set; }

        [XmlElement("vBP")]
        public string VBPField
        {
            get => BPeFormat.Format2(VBP);
            set => VBP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDesconto { get; set; }

        [XmlElement("vDesconto")]
        public string VDescontoField
        {
            get => BPeFormat.Format2(VDesconto);
            set => VDesconto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPgto { get; set; }

        [XmlElement("vPgto")]
        public string VPgtoField
        {
            get => BPeFormat.Format2(VPgto);
            set => VPgto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTroco { get; set; }

        [XmlElement("vTroco")]
        public string VTrocoField
        {
            get => BPeFormat.Format2(VTroco);
            set => VTroco = Converter.ToDouble(value);
        }

        [XmlElement("tpDesconto")]
        public TipoDescontoBPe? TpDesconto { get; set; }

        [XmlElement("xDesconto")]
        public string XDesconto { get; set; }

        [XmlElement("cDesconto")]
        public string CDesconto { get; set; }

        [XmlElement("Comp")]
        public List<CompValor> Comp { get; set; }

#if INTEROP
        public void AddComp(CompValor item) => (Comp ?? (Comp = new List<CompValor>())).Add(item);
        public CompValor GetComp(int index) => (Comp?.Count ?? 0) == 0 ? default : Comp[index];
        public int GetCompCount => Comp != null ? Comp.Count : 0;
#endif

        public bool ShouldSerializeTpDesconto() => TpDesconto.HasValue;
        public bool ShouldSerializeXDesconto() => !string.IsNullOrWhiteSpace(XDesconto);
        public bool ShouldSerializeCDesconto() => !string.IsNullOrWhiteSpace(CDesconto);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.CompValor")]
    [ComVisible(true)]
#endif
    public class CompValor
    {
        [XmlElement("tpComp")]
        public TipoComponenteValorBPe TpComp { get; set; }

        [XmlIgnore]
        public double VComp { get; set; }

        [XmlElement("vComp")]
        public string VCompField
        {
            get => BPeFormat.Format2(VComp);
            set => VComp = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Imp")]
    [ComVisible(true)]
#endif
    public class Imp
    {
        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        [XmlIgnore]
        public double? VINSSRet { get; set; }

        [XmlElement("vINSSRet")]
        public string VINSSRetField
        {
            get => VINSSRet.HasValue ? BPeFormat.Format2(VINSSRet.Value) : null;
            set => VINSSRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VTotTrib { get; set; }

        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.HasValue ? BPeFormat.Format2(VTotTrib.Value) : null;
            set => VTotTrib = Converter.ToDouble(value);
        }

        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlIgnore]
        public double VTotDFe { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => BPeFormat.Format2(VTotDFe);
            set => VTotDFe = Converter.ToDouble(value);
        }

        public bool ShouldSerializeVINSSRetField() => VINSSRet.HasValue;
        public bool ShouldSerializeVTotTribField() => VTotTrib.HasValue;
        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Pag")]
    [ComVisible(true)]
#endif
    public class Pag
    {
        [XmlElement("tPag")]
        public MeioPagamentoBPe TPag { get; set; }

        [XmlElement("xPag")]
        public string XPag { get; set; }

        [XmlElement("nDocPag")]
        public string NDocPag { get; set; }

        [XmlIgnore]
        public double VPag { get; set; }

        [XmlElement("vPag")]
        public string VPagField
        {
            get => BPeFormat.Format2(VPag);
            set => VPag = Converter.ToDouble(value);
        }

        [XmlElement("card")]
        public Card Card { get; set; }

        public bool ShouldSerializeXPag() => !string.IsNullOrWhiteSpace(XPag);
        public bool ShouldSerializeNDocPag() => !string.IsNullOrWhiteSpace(NDocPag);
        public bool ShouldSerializeCard() => Card != null;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.Card")]
    [ComVisible(true)]
#endif
    public class Card
    {
        [XmlElement("tpIntegra")]
        public TipoIntegracaoPagamento TpIntegra { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("tBand")]
        public BandeiraOperadoraCartao? TBand { get; set; }

        [XmlElement("xBand")]
        public string XBand { get; set; }

        [XmlElement("cAut")]
        public string CAut { get; set; }

        [XmlElement("nsuTrans")]
        public string NsuTrans { get; set; }

        [XmlElement("nsuHost")]
        public string NsuHost { get; set; }

        [XmlElement("nParcelas")]
        public string NParcelas { get; set; }

        [XmlElement("infAdCard")]
        public string InfAdCard { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeTBand() => TBand.HasValue;
        public bool ShouldSerializeXBand() => !string.IsNullOrWhiteSpace(XBand);
        public bool ShouldSerializeCAut() => !string.IsNullOrWhiteSpace(CAut);
        public bool ShouldSerializeNsuTrans() => !string.IsNullOrWhiteSpace(NsuTrans);
        public bool ShouldSerializeNsuHost() => !string.IsNullOrWhiteSpace(NsuHost);
        public bool ShouldSerializeNParcelas() => !string.IsNullOrWhiteSpace(NParcelas);
        public bool ShouldSerializeInfAdCard() => !string.IsNullOrWhiteSpace(InfAdCard);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfBPeVinculado")]
    [ComVisible(true)]
#endif
    public class InfBPeVinculado
    {
        [XmlElement("chBPeMultiplo")]
        public string ChBPeMultiplo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.BPeTA.InfBPeSupl")]
    [ComVisible(true)]
#endif
    public class InfBPeSupl
    {
        [XmlElement("qrCodBPe")]
        public string QrCodBPe { get; set; }
    }
}
