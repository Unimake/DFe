#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFGas
{
    /// <summary>
    /// NFGas - Nota Fiscal Eletrônica do Gás
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.NFGas")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfgas")]
    [XmlRoot("NFGas", Namespace = "http://www.portalfiscal.inf.br/nfgas", IsNullable = false)]
    public class NFGas : XMLBase
    {
        /// <summary>
        /// Informações da NFGas
        /// </summary>
        [XmlElement("infNFGas")]
        public InfNFGas InfNFGas { get; set; }

        /// <summary>
        /// Informações suplementares da NFGas
        /// </summary>
        [XmlElement("infNFGasSupl")]
        public InfNFGasSupl InfNFGasSupl { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    /// <summary>
    /// Informações da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.InfNFGas")]
    [ComVisible(true)]
#endif
    public class InfNFGas
    {
        private string idField;
        private string chaveField;

        /// <summary>
        /// Versão do leiaute
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificador da NFGas
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => string.IsNullOrWhiteSpace(idField) ? "NFGas" + Chave : idField;
            set => idField = value;
        }

        /// <summary>
        /// Chave de acesso da NFGas
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

                if (!string.IsNullOrWhiteSpace(idField) && idField.StartsWith("NFGas", StringComparison.OrdinalIgnoreCase))
                {
                    chaveField = idField.Substring(5);
                    return chaveField;
                }

                if (Ide == null) throw new NullReferenceException("A propriedade 'Ide' está nula.");
                if (Emit == null) throw new NullReferenceException("A propriedade 'Emit' está nula.");
                if (string.IsNullOrWhiteSpace(Emit.CNPJ)) throw new NullReferenceException("Emit.CNPJ não foi informado.");

                var dhEmi = DateTimeOffset.Parse(Ide.DhEmi);
                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = Ide.CUF,
                    AnoEmissao = dhEmi.ToString("yy"),
                    MesEmissao = dhEmi.ToString("MM"),
                    CNPJCPFEmissor = Emit.CNPJ.PadLeft(14, '0'),
                    Modelo = Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NNF,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    NSiteAutoriz = Ide.NSiteAutoriz,
                    CodigoNumerico = Ide.CNF
                };

                chaveField = XMLUtility.MontarChaveNFGas(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

                return chaveField;
            }

            set => chaveField = value;
        }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("instalacao")]
        public Instalacao Instalacao { get; set; }

        [XmlElement("gSub")]
        public GSub GSub { get; set; }

        [XmlElement("gVolContrat")]
        public List<GVolContrat> GVolContrat { get; set; }

        [XmlElement("gMed")]
        public List<GMed> GMed { get; set; }

        [XmlElement("det")]
        public List<Det> Det { get; set; }

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("pgtoVinc")]
        public PgtoVinc PgtoVinc { get; set; }

        [XmlElement("gFat")]
        public GFat GFat { get; set; }

        [XmlElement("gAgencia")]
        public GAgencia GAgencia { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("gRespTec")]
        public GRespTec GRespTec { get; set; }

        public bool ShouldSerializeGVolContrat() => GVolContrat?.Count > 0;

        public bool ShouldSerializeGMed() => GMed?.Count > 0;

        public bool ShouldSerializeAutXML() => AutXML?.Count > 0;
    }

    /// <summary>
    /// Informações suplementares da NFGas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFGas.InfNFGasSupl")]
    [ComVisible(true)]
#endif
    public class InfNFGasSupl
    {
        /// <summary>
        /// Texto com o QR-Code para consulta da NFGas
        /// </summary>
        [XmlElement("qrCodNFGas")]
        public string QrCodNFGas { get; set; }
    }

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

        [XmlElement("nNF")]
        public int NNF { get; set; }

        [XmlElement("cNF")]
        public string CNF { get; set; }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("dhEmi")]
        public string DhEmi { get; set; }

        [XmlElement("tpEmis")]
        public TipoEmissaoNFGas TpEmis { get; set; }

        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz { get; set; }

        [XmlElement("cMunFG")]
        public string CMunFG { get; set; }

        [XmlElement("finNFGas")]
        public FinalidadeNFGas FinNFGas { get; set; }

        [XmlElement("tpFat")]
        public TipoFaturamentoNFGas TpFat { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("dhCont")]
        public string DhCont { get; set; }

        [XmlElement("xJust")]
        public string XJust { get; set; }

        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        [XmlElement("tpPagAnt")]
        public TipoPagamentoAntecipadoNFCom TpPagAnt { get; set; }

        public bool ShouldSerializeDhCont() => !string.IsNullOrEmpty(DhCont);

        public bool ShouldSerializeXJust() => !string.IsNullOrEmpty(XJust);

        public bool ShouldSerializeTpPagAnt() => TpPagAnt != (TipoPagamentoAntecipadoNFCom)0;
    }

    public class GCompraGov
    {
        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamentalNFGas TpEnteGov { get; set; }

        [XmlElement("pRedutor")]
        public string PRedutor { get; set; }

        [XmlElement("tpOperGov")]
        public TipoOperacaoGovernamentalNFGas TpOperGov { get; set; }

        [XmlElement("refDFeAnt")]
        public List<string> RefDFeAnt { get; set; }

        public bool ShouldSerializeRefDFeAnt() => RefDFeAnt?.Count > 0;
    }

    public class Emit
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        [XmlElement("ISUFEmit")]
        public string ISUFEmit { get; set; }

        public bool ShouldSerializeXFant() => !string.IsNullOrEmpty(XFant);

        public bool ShouldSerializeISUFEmit() => !string.IsNullOrEmpty(ISUFEmit);
    }

    public class Dest
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("idOutros")]
        public string IdOutros { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("cNIS")]
        public string CNIS { get; set; }

        [XmlElement("NB")]
        public string NB { get; set; }

        [XmlElement("xNomeAdicional")]
        public string XNomeAdicional { get; set; }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);

        public bool ShouldSerializeIdOutros() => !string.IsNullOrEmpty(IdOutros);

        public bool ShouldSerializeIE() => !string.IsNullOrEmpty(IE);

        public bool ShouldSerializeIM() => !string.IsNullOrEmpty(IM);

        public bool ShouldSerializeCNIS() => !string.IsNullOrEmpty(CNIS);

        public bool ShouldSerializeNB() => !string.IsNullOrEmpty(NB);

        public bool ShouldSerializeXNomeAdicional() => !string.IsNullOrEmpty(XNomeAdicional);
    }

    public class EnderEmit : EnderDest
    {

    }

    public class EnderDest
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

        public bool ShouldSerializeXCpl() => !string.IsNullOrEmpty(XCpl);

        public bool ShouldSerializeFone() => !string.IsNullOrEmpty(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);
    }

    public class Instalacao
    {
        [XmlElement("idInstalacao")]
        public string IdInstalacao { get; set; }

        [XmlElement("idCodCliente")]
        public string IdCodCliente { get; set; }

        [XmlElement("tpInstalacao")]
        public TipoInstalacaoNFGas TpInstalacao { get; set; }

        [XmlElement("nContrato")]
        public string NContrato { get; set; }

        [XmlElement("tpClasse")]
        public ClasseConsumoNFGas TpClasse { get; set; }

        [XmlElement("xClasse")]
        public string XClasse { get; set; }

        [XmlElement("latGPS")]
        public string LatGPS { get; set; }

        [XmlElement("longGPS")]
        public string LongGPS { get; set; }

        [XmlElement("codRoteiroLeitura")]
        public string CodRoteiroLeitura { get; set; }
    }

    public class GSub
    {
        [XmlElement("chNFGas")]
        public string ChNFGas { get; set; }

        [XmlElement("gNF")]
        public GNF GNF { get; set; }

        [XmlElement("motSub")]
        public MotivoSubstituicaoNFGas MotSub { get; set; }
    }

    public class GNF
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nNF")]
        public int NNF { get; set; }

        [XmlElement("CompetEmis")]
        public string CompetEmis { get; set; }

        [XmlElement("CompetApur")]
        public string CompetApur { get; set; }

        [XmlElement("hash115")]
        public string Hash115 { get; set; }
    }

    public class GVolContrat
    {
        [XmlAttribute("nContrat")]
        public string NContrat { get; set; }

        [XmlElement("tpVolContrat")]
        public GrandezaContratadaNFGas TpVolContrat { get; set; }

        [XmlElement("qUnidContrat")]
        public string QUnidContrat { get; set; }
    }

    public class GMed
    {
        [XmlAttribute("nMed")]
        public string NMed { get; set; }

        [XmlElement("idEqp")]
        public string IdEqp { get; set; }

        [XmlElement("dMedAnt")]
        public string DMedAnt { get; set; }

        [XmlElement("vMedAnt")]
        public string VMedAnt { get; set; }

        [XmlElement("dMedAtu")]
        public string DMedAtu { get; set; }

        [XmlElement("vMedAtu")]
        public string VMedAtu { get; set; }

        [XmlElement("tpEqp")]
        public string TpEqp { get; set; }

        [XmlElement("tpMedidor")]
        public string TpMedidor { get; set; }
    }

    public class GTarif
    {
        [XmlElement("dIniTarif")]
        public string DIniTarif { get; set; }

        [XmlElement("dFimTarif")]
        public string DFimTarif { get; set; }

        [XmlElement("nAto")]
        public string NAto { get; set; }

        [XmlElement("anoAto")]
        public string AnoAto { get; set; }

        [XmlElement("tpFaixaCons")]
        public string TpFaixaCons { get; set; }

        [XmlElement("vTarifAplic")]
        public string VTarifAplic { get; set; }
    }

    public class GMedicao
    {
        [XmlElement("nMed")]
        public string NMed { get; set; }

        [XmlElement("nContrat")]
        public string NContrat { get; set; }

        [XmlElement("gMedida")]
        public GMedida GMedida { get; set; }

        [XmlElement("tpMotNaoLeitura")]
        public string TpMotNaoLeitura { get; set; }

        [XmlElement("xMotNaoLeitura")]
        public string XMotNaoLeitura { get; set; }

        public bool ShouldSerializeNContrat() => !string.IsNullOrEmpty(NContrat);

        public bool ShouldSerializeXMotNaoLeitura() => !string.IsNullOrEmpty(XMotNaoLeitura);
    }

    public class GMedida
    {
        [XmlElement("uMed")]
        public UnidadeMedidaNFGas UMed { get; set; }

        [XmlElement("vMed")]
        public string VMed { get; set; }
    }

    public class Det
    {
        [XmlAttribute("nItem")]
        public int NItem { get; set; }

        [XmlAttribute("chNFGasAnt")]
        public string ChNFGasAnt { get; set; }

        [XmlAttribute("nItemAnt")]
        public string NItemAnt { get; set; }

        [XmlElement("gNormal")]
        public GNormal GNormal { get; set; }

        [XmlElement("gAgregadora")]
        public GAgregadora GAgregadora { get; set; }

        public bool ShouldSerializeChNFGasAnt() => !string.IsNullOrEmpty(ChNFGasAnt);

        public bool ShouldSerializeNItemAnt() => !string.IsNullOrEmpty(NItemAnt);
    }

    public class GNormal
    {
        [XmlElement("gTarif")]
        public List<GTarif> GTarif { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        [XmlElement("gProcRef")]
        public GProcRef GProcRef { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd { get; set; }

        public bool ShouldSerializeGTarif() => GTarif?.Count > 0;

        public bool ShouldSerializeInfAdProd() => !string.IsNullOrEmpty(InfAdProd);
    }

    public class Prod
    {
        [XmlElement("indOrigemQtd")]
        public OrigemQuantidadeFaturadaNFGas IndOrigemQtd { get; set; }

        [XmlElement("gMedicao")]
        public GMedicao GMedicao { get; set; }

        [XmlElement("cProd")]
        public string CProd { get; set; }

        [XmlElement("xProd")]
        public string XProd { get; set; }

        [XmlElement("cClass")]
        public string CClass { get; set; }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlElement("uMed")]
        public UnidadeMedidaNFGas UMed { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturada { get; set; }

        [XmlElement("vItem")]
        public string VItem { get; set; }

        [XmlElement("fatorPCS")]
        public string FatorPCS { get; set; }

        [XmlElement("fatorPTZ")]
        public string FatorPTZ { get; set; }

        [XmlElement("fatorP")]
        public string FatorP { get; set; }

        [XmlElement("fatorT")]
        public string FatorT { get; set; }

        [XmlElement("vProd")]
        public string VProd { get; set; }

        [XmlElement("indDevolucao")]
        public string IndDevolucao { get; set; }

        [XmlElement("gPagAntecipado")]
        public GPagAntecipado GPagAntecipado { get; set; }

        public bool ShouldSerializeCFOP() => !string.IsNullOrEmpty(CFOP);

        public bool ShouldSerializeGMedicao() => GMedicao != null;

        public bool ShouldSerializeFatorPCS() => !string.IsNullOrEmpty(FatorPCS);

        public bool ShouldSerializeFatorPTZ() => !string.IsNullOrEmpty(FatorPTZ);

        public bool ShouldSerializeFatorP() => !string.IsNullOrEmpty(FatorP);

        public bool ShouldSerializeFatorT() => !string.IsNullOrEmpty(FatorT);

        public bool ShouldSerializeIndDevolucao() => !string.IsNullOrEmpty(IndDevolucao);
    }

    public class GPagAntecipado
    {
        [XmlElement("chDFePagAnt")]
        public string ChDFePagAnt { get; set; }

        [XmlElement("nItemPagAnt")]
        public int NItemPagAnt { get; set; }

        public bool ShouldSerializeNItemPagAnt() => NItemPagAnt > 0;
    }

    public class Imposto
    {
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; } = (OrigemMercadoria)(-1);

        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("ICMS10")]
        public ICMS10 ICMS10 { get; set; }

        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        [XmlElement("ICMS40")]
        public ICMS40 ICMS40 { get; set; }

        [XmlElement("ICMS51")]
        public ICMS51 ICMS51 { get; set; }

        [XmlElement("ICMS60")]
        public ICMS60 ICMS60 { get; set; }

        [XmlElement("ICMS70")]
        public ICMS70 ICMS70 { get; set; }

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("indSemCST")]
        public string IndSemCST { get; set; }

        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        [XmlElement("retTrib")]
        public RetTrib RetTrib { get; set; }

        [XmlElement("TxReg")]
        public TxReg TxReg { get; set; }

        public bool ShouldSerializeOrig() => Orig != (OrigemMercadoria)(-1);

        public bool ShouldSerializeIndSemCST() => !string.IsNullOrEmpty(IndSemCST);
    }

    public class ICMS00
    {
        [XmlIgnore]
        public OrigemMercadoria Orig { get; set; } = (OrigemMercadoria)(-1);

        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }

        public bool ShouldSerializeOrig() => Orig != (OrigemMercadoria)(-1);
    }

    public class ICMS10
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("modBC")]
        public string ModBC { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("pFCP")]
        public string PFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }

        [XmlElement("modBCST")]
        public string ModBCST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVAST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPST { get; set; }
    }

    public class ICMS20 : ICMS00
    {
        [XmlElement("modBC")]
        public string ModBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBC { get; set; }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }
    }

    public class ICMS40
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDeson { get; set; }

        [XmlElement("motDesICMS")]
        public string MotDesICMS { get; set; }

        [XmlElement("indDeduzDeson")]
        public string IndDeduzDeson { get; set; }
    }

    public class ICMS51
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("pDif")]
        public string PDif { get; set; }

        [XmlElement("vICMSOp")]
        public string VICMSOp { get; set; }

        [XmlElement("vICMSDif")]
        public string VICMSDif { get; set; }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        public bool ShouldSerializeCBenef() => !string.IsNullOrEmpty(CBenef);
    }

    public class ICMS60
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRet { get; set; }

        [XmlElement("pICMSSTRet")]
        public string PICMSSTRet { get; set; }

        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstituto { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRet { get; set; }

        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRet { get; set; }

        [XmlElement("pFCPSTRet")]
        public string PFCPSTRet { get; set; }

        [XmlElement("vFCPSTRet")]
        public string VFCPSTRet { get; set; }

        [XmlElement("pRedBCEfet")]
        public string PRedBCEfet { get; set; }

        [XmlElement("vBCEfet")]
        public string VBCEfet { get; set; }

        [XmlElement("pICMSEfet")]
        public string PICMSEfet { get; set; }

        [XmlElement("vICMSEfet")]
        public string VICMSEfet { get; set; }
    }

    public class ICMS70
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("modBC")]
        public string ModBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBC { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("vBCFCP")]
        public string VBCFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }

        [XmlElement("modBCST")]
        public string ModBCST { get; set; }

        [XmlElement("pMVAST")]
        public string PMVAST { get; set; }

        [XmlElement("pRedBCST")]
        public string PRedBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSST { get; set; }

        [XmlElement("vBCFCPST")]
        public string VBCFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPST { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDeson { get; set; }

        [XmlElement("motDesICMS")]
        public string MotDesICMS { get; set; }

        [XmlElement("indDeduzDeson")]
        public string IndDeduzDeson { get; set; }

        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDeson { get; set; }

        [XmlElement("motDesICMSST")]
        public string MotDesICMSST { get; set; }
    }

    public class ICMS90
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDeson { get; set; }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        [XmlElement("pFCP")]
        public string PFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }
    }

    public class PIS
    {
        [XmlElement("CST")]
        public CSTPisCofins CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pPIS")]
        public string PPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPIS { get; set; }
    }

    public class COFINS
    {
        [XmlElement("CST")]
        public CSTPisCofins CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pCOFINS")]
        public string PCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINS { get; set; }
    }

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

        [XmlIgnore]
        public GIBSCBSMono GIBSCBSMono { get; set; }

        [XmlIgnore]
        public GTransfCred GTransfCred { get; set; }

        [XmlIgnore]
        public GAjusteCompet GAjusteCompet { get; set; }

        [XmlIgnore]
        public GCredPresOper GCredPresOper { get; set; }

        [XmlIgnore]
        public GCredPresIBSZFM GCredPresIBSZFM { get; set; }

        [XmlElement("gEstornoCred")]
        public GEstornoCred GEstornoCred { get; set; }

        [XmlIgnore]
        public string RefDFe { get; set; }

#if INTEROP
        public bool ShouldSerializeIndDoacao() => IndDoacao != (IndicadorDoacao)(-1);
#else
        public bool ShouldSerializeIndDoacao() => IndDoacao != null;
#endif
    }

    public class GAgregadora
    {
        [XmlElement("cClass")]
        public string CClass { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFe { get; set; }
    }

    public class IS
    {
        [XmlElement("CSTIS")]
        public string CSTIS { get; set; }

        [XmlElement("cClassTribIS")]
        public string CClassTribIS { get; set; }

        [XmlElement("vBCIS")]
        public string VBCIS { get; set; }

        [XmlElement("pIS")]
        public string PIS { get; set; }

        [XmlElement("adRemIS")]
        public string AdRemIS { get; set; }

        [XmlIgnore]
        public string PISEspec
        {
            get => AdRemIS;
            set => AdRemIS = value;
        }

        [XmlElement("uTrib")]
        public string UTrib { get; set; }

        [XmlElement("qTrib")]
        public string QTrib { get; set; }

        [XmlElement("vIS")]
        public string VIS { get; set; }
    }

    public class GIBSCBS
    {
        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }

        [XmlElement("vIBS")]
        public string VIBS { get; set; }

        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }
    }

    public class GIBSUF
    {
        [XmlElement("pIBSUF")]
        public string PIBSUF { get; set; }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUF { get; set; }
    }

    public class GIBSMun
    {
        [XmlElement("pIBSMun")]
        public string PIBSMun { get; set; }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlElement("vIBSMun")]
        public string VIBSMun { get; set; }
    }

    public class GCBS
    {
        [XmlElement("pCBS")]
        public string PCBS { get; set; }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlElement("gALCZFMCBS")]
        public GALCZFMCBS GALCZFMCBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBS { get; set; }
    }

    public class GDif
    {
        [XmlElement("pDif")]
        public string PDif { get; set; }

        [XmlElement("vDif")]
        public string VDif { get; set; }
    }

    public class GDevTrib
    {
        [XmlElement("pDevTrib")]
        public string PDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTrib { get; set; }

        public bool ShouldSerializePDevTrib() => !string.IsNullOrEmpty(PDevTrib);
    }

    public class GRed
    {
        [XmlElement("pRedAliq")]
        public string PRedAliq { get; set; }

        [XmlElement("pAliqEfet")]
        public string PAliqEfet { get; set; }
    }

    public class GALCZFMCBS
    {
        [XmlElement("tpALCZFMCBS")]
        public TipoAplicacaoAliquotaZeroCBS TpALCZFMCBS { get; set; }

        [XmlElement("nProcSuframa")]
        public string NProcSuframa { get; set; }

        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBS { get; set; }

        [XmlElement("vTribRegCBS")]
        public string VTribRegCBS { get; set; }

        public bool ShouldSerializeTpALCZFMCBS() => TpALCZFMCBS > 0;

        public bool ShouldSerializeNProcSuframa() => !string.IsNullOrWhiteSpace(NProcSuframa);
    }

    public class GTribRegular
    {
        [XmlElement("CSTReg")]
        public string CSTReg { get; set; }

        [XmlElement("cClassTribReg")]
        public string CClassTribReg { get; set; }

        [XmlElement("pAliqEfetRegIBSUF")]
        public string PAliqEfetRegIBSUF { get; set; }

        [XmlElement("vTribRegIBSUF")]
        public string VTribRegIBSUF { get; set; }

        [XmlElement("pAliqEfetRegIBSMun")]
        public string PAliqEfetRegIBSMun { get; set; }

        [XmlElement("vTribRegIBSMun")]
        public string VTribRegIBSMun { get; set; }

        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBS { get; set; }

        [XmlElement("vTribRegCBS")]
        public string VTribRegCBS { get; set; }
    }

    public class GTribCompraGov
    {
        [XmlElement("pAliqIBSUF")]
        public string PAliqIBSUF { get; set; }

        [XmlElement("vTribIBSUF")]
        public string VTribIBSUF { get; set; }

        [XmlElement("pAliqIBSMun")]
        public string PAliqIBSMun { get; set; }

        [XmlElement("vTribIBSMun")]
        public string VTribIBSMun { get; set; }

        [XmlElement("pAliqCBS")]
        public string PAliqCBS { get; set; }

        [XmlElement("vTribCBS")]
        public string VTribCBS { get; set; }
    }

    public class GIBSCBSMono
    {
        [XmlElement("gMono")]
        public GMono GMono { get; set; }

        [XmlElement("gMonoPadrao")]
        public GMonoPadrao GMonoPadrao { get; set; }

        [XmlElement("gMonoReten")]
        public GMonoReten GMonoReten { get; set; }

        [XmlElement("gMonoRet")]
        public GMonoRet GMonoRet { get; set; }

        [XmlElement("gMonoDif")]
        public GMonoDif GMonoDif { get; set; }

        [XmlElement("vTotIBSMonoItem")]
        public string VTotIBSMonoItem { get; set; }

        [XmlElement("vTotCBSMonoItem")]
        public string VTotCBSMonoItem { get; set; }
    }

    public class GMono
    {
        [XmlElement("vIBSMono")]
        public string VIBSMono { get; set; }

        [XmlElement("vCBSMono")]
        public string VCBSMono { get; set; }

        [XmlElement("vIBSMonoReten")]
        public string VIBSMonoReten { get; set; }

        [XmlElement("vCBSMonoReten")]
        public string VCBSMonoReten { get; set; }

        [XmlElement("vIBSMonoRet")]
        public string VIBSMonoRet { get; set; }

        [XmlElement("vCBSMonoRet")]
        public string VCBSMonoRet { get; set; }
    }

    public class GMonoPadrao
    {
        [XmlElement("qBCMono")]
        public string QBCMono { get; set; }

        [XmlElement("adRemIBS")]
        public string AdRemIBS { get; set; }

        [XmlElement("adRemCBS")]
        public string AdRemCBS { get; set; }
    }

    public class GMonoReten
    {
        [XmlElement("qBCMonoReten")]
        public string QBCMonoReten { get; set; }

        [XmlElement("adRemIBSReten")]
        public string AdRemIBSReten { get; set; }

        [XmlElement("adRemCBSReten")]
        public string AdRemCBSReten { get; set; }
    }

    public class GMonoRet
    {
        [XmlElement("qBCMonoRet")]
        public string QBCMonoRet { get; set; }

        [XmlElement("adRemIBSRet")]
        public string AdRemIBSRet { get; set; }

        [XmlElement("adRemCBSRet")]
        public string AdRemCBSRet { get; set; }
    }

    public class GMonoDif
    {
        [XmlElement("pDifIBS")]
        public string PDifIBS { get; set; }

        [XmlElement("vIBSMonoDif")]
        public string VIBSMonoDif { get; set; }

        [XmlElement("pDifCBS")]
        public string PDifCBS { get; set; }

        [XmlElement("vCBSMonoDif")]
        public string VCBSMonoDif { get; set; }
    }

    public class GTransfCred
    {
        [XmlElement("vIBS")]
        public string VIBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBS { get; set; }
    }

    public class GAjusteCompet
    {
        [XmlElement("competApur")]
        public string CompetApur { get; set; }

        [XmlElement("vIBS")]
        public string VIBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBS { get; set; }
    }

    public class GCredPresOper
    {
        [XmlElement("vBCCredPres")]
        public string VBCCredPres { get; set; }

        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        [XmlElement("pCredPres")]
        public string PCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPres { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSus { get; set; }

        [XmlElement("gIBSCredPres")]
        public GCredPres GIBSCredPres { get; set; }

        [XmlElement("gCBSCredPres")]
        public GCredPres GCBSCredPres { get; set; }
    }

    public class GCredPres
    {
        [XmlElement("pCredPres")]
        public string PCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPres { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSus { get; set; }
    }

    public class GCredPresIBSZFM
    {
        [XmlElement("competApur")]
        public string CompetApur { get; set; }

        [XmlElement("tpCredPresIBSZFM")]
        public TipoCreditoPresumidoIBSZFM TpCredPresIBSZFM { get; set; }

        [XmlElement("vCredPresIBSZFM")]
        public string VCredPresIBSZFM { get; set; }
    }

    public class TribItemSN
    {
        [XmlAttribute("nItem")]
        public string NItem { get; set; }

        [XmlElement("vRBSNItem")]
        public string VRBSNItem { get; set; }

        [XmlElement("tpRBSN")]
        public TipoReceitaBrutaSimplesNacional TpRBSN { get; set; }

        [XmlElement("pIBSSN")]
        public string PIBSSN { get; set; }

        [XmlElement("vIBSSN")]
        public string VIBSSN { get; set; }

        [XmlElement("pCBSSN")]
        public string PCBSSN { get; set; }

        [XmlElement("vCBSSN")]
        public string VCBSSN { get; set; }

        [XmlElement("vIBSPendSusp")]
        public string VIBSPendSusp { get; set; }

        [XmlElement("vCBSPendSusp")]
        public string VCBSPendSusp { get; set; }

        public bool ShouldSerializePIBSSN() => !string.IsNullOrEmpty(PIBSSN);

        public bool ShouldSerializeVIBSSN() => !string.IsNullOrEmpty(VIBSSN);

        public bool ShouldSerializePCBSSN() => !string.IsNullOrEmpty(PCBSSN);

        public bool ShouldSerializeVCBSSN() => !string.IsNullOrEmpty(VCBSSN);

        public bool ShouldSerializeVIBSPendSusp() => !string.IsNullOrEmpty(VIBSPendSusp);

        public bool ShouldSerializeVCBSPendSusp() => !string.IsNullOrEmpty(VCBSPendSusp);
    }

    public class TotalSN
    {
        [XmlElement("vRBSNTot")]
        public string VRBSNTot { get; set; }

        [XmlElement("vIBSSNTot")]
        public string VIBSSNTot { get; set; }

        [XmlElement("vIBSSNtotPendSusp")]
        public string VIBSSNtotPendSusp { get; set; }

        [XmlElement("vCBSSNTot")]
        public string VCBSSNTot { get; set; }

        [XmlElement("vCBSSNtotPendSusp")]
        public string VCBSSNtotPendSusp { get; set; }

        public bool ShouldSerializeVIBSSNTot() => !string.IsNullOrEmpty(VIBSSNTot);

        public bool ShouldSerializeVIBSSNtotPendSusp() => !string.IsNullOrEmpty(VIBSSNtotPendSusp);

        public bool ShouldSerializeVCBSSNTot() => !string.IsNullOrEmpty(VCBSSNTot);

        public bool ShouldSerializeVCBSSNtotPendSusp() => !string.IsNullOrEmpty(VCBSSNtotPendSusp);
    }

    public class Total
    {
        [XmlElement("vProd")]
        public string VProd { get; set; }

        [XmlElement("ICMSTot")]
        public ICMSTot ICMSTot { get; set; }

        [XmlElement("vRetTribTot")]
        public VRetTribTot VRetTribTot { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINS { get; set; }

        [XmlElement("vPIS")]
        public string VPIS { get; set; }

        [XmlElement("vTxReg")]
        public string VTxReg { get; set; }

        [XmlElement("vNF")]
        public string VNF { get; set; }

        [XmlElement("IBSCBSTot")]
        public IBSCBSTot IBSCBSTot { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFe { get; set; }
    }

    public class ICMSTot
    {
        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDeson { get; set; }

        [XmlElement("vFCP")]
        public string VFCP { get; set; }

        [XmlElement("vBCST")]
        public string VBCST { get; set; }

        [XmlElement("vST")]
        public string VST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPST { get; set; }

        [XmlIgnore]
        public string VFCPSTRet { get; set; }
    }

    public class VRetTribTot
    {
        [XmlElement("vRetPIS")]
        public string VRetPIS { get; set; }

        [XmlElement("vRetCofins")]
        public string VRetCofins { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLL { get; set; }

        [XmlElement("vIRRF")]
        public string VIRRF { get; set; }
    }

    public class IBSCBSTot
    {
        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBS { get; set; }

        [XmlElement("gIBS")]
        public GIBSTot GIBS { get; set; }

        [XmlElement("gCBS")]
        public GCBSTot GCBS { get; set; }

        [XmlElement("gEstornoCred")]
        public GEstornoCred GEstornoCred { get; set; }

        [XmlIgnore]
        public string VTotIBSMonoItem { get; set; }

        [XmlIgnore]
        public string VTotCBSMonoItem { get; set; }
    }

    public class GIBSTot
    {
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUF { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMun { get; set; }

        [XmlElement("vIBS")]
        public string VIBS { get; set; }
    }

    public class GIBSUFTot
    {
        [XmlElement("vDif")]
        public string VDif { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTrib { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUF { get; set; }
    }

    public class GIBSMunTot
    {
        [XmlElement("vDif")]
        public string VDif { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTrib { get; set; }

        [XmlElement("vIBSMun")]
        public string VIBSMun { get; set; }
    }

    public class GCBSTot
    {
        [XmlElement("vDif")]
        public string VDif { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTrib { get; set; }

        [XmlElement("vCBS")]
        public string VCBS { get; set; }
    }

    public class GEstornoCred
    {
        [XmlElement("vIBSEstCred")]
        public string VIBSEstCred { get; set; }

        [XmlElement("vCBSEstCred")]
        public string VCBSEstCred { get; set; }
    }

    public class PgtoVinc
    {
        [XmlElement("pgto")]
        public List<Pgto> Pgto { get; set; }

        public bool ShouldSerializePgto() => Pgto?.Count > 0;
    }

    public class RetTrib
    {
        [XmlElement("vRetPIS")]
        public string VRetPIS { get; set; }

        [XmlElement("vRetCofins")]
        public string VRetCofins { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLL { get; set; }

        [XmlElement("vIRRF")]
        public string VIRRF { get; set; }
    }

    public class TxReg
    {
        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pTaxa")]
        public string PTaxa { get; set; }

        [XmlElement("vTaxa")]
        public string VTaxa { get; set; }
    }

    public class GFat
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlElement("dVencFat")]
        public string DVencFat { get; set; }

        [XmlElement("dApresFat")]
        public string DApresFat { get; set; }

        [XmlElement("dProxLeitura")]
        public string DProxLeitura { get; set; }

        [XmlElement("nFat")]
        public string NFat { get; set; }

        [XmlElement("codBarras")]
        public string CodBarras { get; set; }

        [XmlElement("codDebAuto")]
        public string CodDebAuto { get; set; }

        [XmlElement("codBanco")]
        public string CodBanco { get; set; }

        [XmlElement("codAgencia")]
        public string CodAgencia { get; set; }

        [XmlElement("enderCorresp")]
        public EnderCorresp EnderCorresp { get; set; }

        [XmlElement("gPIX")]
        public GPix GPix { get; set; }

        [XmlElement("infAdFat")]
        public string InfAdFat { get; set; }

        [XmlIgnore]
        public string TpMeioPgto { get; set; }

        [XmlIgnore]
        public string CNPJReceb { get; set; }

        [XmlIgnore]
        public string CNPJBasePSP { get; set; }

        [XmlIgnore]
        public string NPag { get; set; }

        [XmlIgnore]
        public string IdTransacao { get; set; }

        public bool ShouldSerializeDApresFat() => !string.IsNullOrEmpty(DApresFat);

        public bool ShouldSerializeNFat() => !string.IsNullOrEmpty(NFat);

        public bool ShouldSerializeCodDebAuto() => !string.IsNullOrEmpty(CodDebAuto);

        public bool ShouldSerializeCodBanco() => !string.IsNullOrEmpty(CodBanco);

        public bool ShouldSerializeCodAgencia() => !string.IsNullOrEmpty(CodAgencia);

        public bool ShouldSerializeInfAdFat() => !string.IsNullOrEmpty(InfAdFat);

        public bool ShouldSerializeTpMeioPgto() => !string.IsNullOrEmpty(TpMeioPgto);

        public bool ShouldSerializeCNPJReceb() => !string.IsNullOrEmpty(CNPJReceb);

        public bool ShouldSerializeCNPJBasePSP() => !string.IsNullOrEmpty(CNPJBasePSP);

        public bool ShouldSerializeNPag() => !string.IsNullOrEmpty(NPag);

        public bool ShouldSerializeIdTransacao() => !string.IsNullOrEmpty(IdTransacao);
    }

    public class EnderCorresp : EnderDest { }

    public class GPix
    {
        [XmlElement("urlQRCodePIX")]
        public string UrlQRCodePIX { get; set; }
    }

    public class GAgencia
    {
        [XmlElement("nomeAgenciaAtend")]
        public string NomeAgenciaAtend { get; set; }

        [XmlElement("enderAgenciaAtend")]
        public string EnderAgenciaAtend { get; set; }

        [XmlElement("sitioAgenciaAtend")]
        public string SitioAgenciaAtend { get; set; }

        [XmlElement("gHistCons")]
        public List<GHistCons> GHistCons { get; set; }

        [XmlElement("infAdReg")]
        public string InfAdReg { get; set; }

        public bool ShouldSerializeGHistCons() => GHistCons?.Count > 0;

        public bool ShouldSerializeInfAdReg() => !string.IsNullOrEmpty(InfAdReg);
    }

    public class GHistCons
    {
        [XmlElement("xHistorico")]
        public string XHistorico { get; set; }

        [XmlElement("gCons")]
        public List<GCons> GCons { get; set; }

        [XmlElement("medMensal")]
        public string MedMensal { get; set; }

        public bool ShouldSerializeGCons() => GCons?.Count > 0;
    }

    public class GCons
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlElement("uMed")]
        public string UMed { get; set; }

        [XmlElement("qtdDias")]
        public int QtdDias { get; set; }

        [XmlElement("medDiaria")]
        public string MedDiaria { get; set; }

        [XmlElement("consumo")]
        public string Consumo { get; set; }

        [XmlElement("vFat")]
        public string VFat { get; set; }

        public bool ShouldSerializeMedDiaria() => !string.IsNullOrEmpty(MedDiaria);

        public bool ShouldSerializeConsumo() => !string.IsNullOrEmpty(Consumo);
    }

    public class AutXML
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);
    }

    public class InfAdic
    {
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("infCpl")]
        public List<string> InfCpl { get; set; }

        [XmlIgnore]
        public GProcRef GProcRef { get; set; }

        [XmlIgnore]
        public InfFisco InfFisco { get; set; }

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrEmpty(InfAdFisco);

        public bool ShouldSerializeInfCpl() => InfCpl?.Count > 0;
    }

    public class GProcRef
    {
        [XmlElement("vItem")]
        public string VItem { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturada { get; set; }

        [XmlElement("vProd")]
        public string VProd { get; set; }

        [XmlElement("indDevolucao")]
        public string IndDevolucao { get; set; }

        [XmlElement("gProc")]
        public List<GProc> GProc { get; set; }

        public bool ShouldSerializeIndDevolucao() => !string.IsNullOrEmpty(IndDevolucao);

        public bool ShouldSerializeGProc() => GProc?.Count > 0;
    }

    public class GProc
    {
        [XmlElement("tpProc")]
        public string TpProc { get; set; }

        [XmlElement("nProcesso")]
        public string NProcesso { get; set; }
    }

    public class InfFisco
    {
        [XmlElement("cMsg")]
        public string CMsg { get; set; }

        [XmlElement("xMsg")]
        public string XMsg { get; set; }
    }

    public class GRespTec
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

        public bool ShouldSerializeIdCSRT() => !string.IsNullOrEmpty(IdCSRT);

        public bool ShouldSerializeHashCSRT() => !string.IsNullOrEmpty(HashCSRT);
    }
}
