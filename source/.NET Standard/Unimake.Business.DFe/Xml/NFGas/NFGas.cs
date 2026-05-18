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
                    UFEmissor = (UFBrasil)Ide.CUF,
                    AnoEmissao = dhEmi.ToString("yy"),
                    MesEmissao = dhEmi.ToString("MM"),
                    CNPJCPFEmissor = Emit.CNPJ.PadLeft(14, '0'),
                    Modelo = (ModeloDFe)Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NNF,
                    TipoEmissao = (TipoEmissao)Ide.TpEmis,
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
        [XmlElement("cUF")]
        public int CUF { get; set; }

        [XmlElement("tpAmb")]
        public int TpAmb { get; set; }

        [XmlElement("mod")]
        public int Mod { get; set; }

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
        public int TpEmis { get; set; }

        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz { get; set; }

        [XmlElement("cMunFG")]
        public string CMunFG { get; set; }

        [XmlElement("finNFGas")]
        public int FinNFGas { get; set; }

        [XmlElement("tpFat")]
        public int TpFat { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("dhCont")]
        public string DhCont { get; set; }

        [XmlElement("xJust")]
        public string XJust { get; set; }

        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        public bool ShouldSerializeDhCont() => !string.IsNullOrEmpty(DhCont);

        public bool ShouldSerializeXJust() => !string.IsNullOrEmpty(XJust);
    }

    public class GCompraGov
    {
        [XmlElement("tpEnteGov")]
        public string TpEnteGov { get; set; }

        [XmlElement("pRedutor")]
        public string PRedutor { get; set; }

        [XmlElement("tpOperGov")]
        public string TpOperGov { get; set; }

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
        public EnderNFGas EnderEmit { get; set; }

        public bool ShouldSerializeXFant() => !string.IsNullOrEmpty(XFant);
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

        [XmlElement("indIEDest")]
        public int IndIEDest { get; set; }

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
        public EnderNFGas EnderDest { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);

        public bool ShouldSerializeIdOutros() => !string.IsNullOrEmpty(IdOutros);

        public bool ShouldSerializeIE() => !string.IsNullOrEmpty(IE);

        public bool ShouldSerializeIM() => !string.IsNullOrEmpty(IM);

        public bool ShouldSerializeCNIS() => !string.IsNullOrEmpty(CNIS);

        public bool ShouldSerializeNB() => !string.IsNullOrEmpty(NB);

        public bool ShouldSerializeXNomeAdicional() => !string.IsNullOrEmpty(XNomeAdicional);
    }

    public class EnderNFGas
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
        public string UF { get; set; }

        [XmlElement("cPais")]
        public string CPais { get; set; }

        [XmlElement("xPais")]
        public string XPais { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        public bool ShouldSerializeXCpl() => !string.IsNullOrEmpty(XCpl);

        public bool ShouldSerializeCPais() => !string.IsNullOrEmpty(CPais);

        public bool ShouldSerializeXPais() => !string.IsNullOrEmpty(XPais);

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
        public string TpInstalacao { get; set; }

        [XmlElement("nContrato")]
        public string NContrato { get; set; }

        [XmlElement("tpClasse")]
        public string TpClasse { get; set; }

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
        public string MotSub { get; set; }
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
        public string TpVolContrat { get; set; }

        [XmlElement("qUnidContrat")]
        public string QUnidContrat { get; set; }
    }

    public class GMed
    {
        [XmlElement("nMed")]
        public string NMed { get; set; }

        [XmlElement("gMedGas")]
        public GMedGas GMedGas { get; set; }
    }

    public class GMedGas
    {
        [XmlElement("tpGrMed")]
        public string TpGrMed { get; set; }

        [XmlElement("nMedidor")]
        public string NMedidor { get; set; }
    }

    public class Det
    {
        [XmlAttribute("nItem")]
        public int NItem { get; set; }

        [XmlAttribute("chNFGasAnt")]
        public string ChNFGasAnt { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd { get; set; }

        [XmlElement("gAgregadora")]
        public GAgregadora GAgregadora { get; set; }

        public bool ShouldSerializeChNFGasAnt() => !string.IsNullOrEmpty(ChNFGasAnt);

        public bool ShouldSerializeInfAdProd() => !string.IsNullOrEmpty(InfAdProd);
    }

    public class Prod
    {
        [XmlElement("cProd")]
        public string CProd { get; set; }

        [XmlElement("xProd")]
        public string XProd { get; set; }

        [XmlElement("cClass")]
        public string CClass { get; set; }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlElement("uMed")]
        public string UMed { get; set; }

        [XmlElement("vItem")]
        public string VItem { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturada { get; set; }

        [XmlElement("vProd")]
        public string VProd { get; set; }
    }

    public class Imposto
    {
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }
    }

    public class ICMS00
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("vBC")]
        public string VBC { get; set; }

        [XmlElement("pICMS")]
        public string PICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMS { get; set; }
    }

    public class PIS
    {
        [XmlElement("CST")]
        public string CST { get; set; }

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
        public string CST { get; set; }

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
    }

    public class GAgregadora
    {
        [XmlElement("cClass")]
        public string CClass { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFe { get; set; }
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
        public EnderNFGas EnderCorresp { get; set; }

        [XmlElement("gPIX")]
        public GPix GPix { get; set; }

        [XmlElement("infAdFat")]
        public string InfAdFat { get; set; }

        public bool ShouldSerializeDApresFat() => !string.IsNullOrEmpty(DApresFat);

        public bool ShouldSerializeNFat() => !string.IsNullOrEmpty(NFat);

        public bool ShouldSerializeCodDebAuto() => !string.IsNullOrEmpty(CodDebAuto);

        public bool ShouldSerializeCodBanco() => !string.IsNullOrEmpty(CodBanco);

        public bool ShouldSerializeCodAgencia() => !string.IsNullOrEmpty(CodAgencia);

        public bool ShouldSerializeInfAdFat() => !string.IsNullOrEmpty(InfAdFat);
    }

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

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrEmpty(InfAdFisco);

        public bool ShouldSerializeInfCpl() => InfCpl?.Count > 0;
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
