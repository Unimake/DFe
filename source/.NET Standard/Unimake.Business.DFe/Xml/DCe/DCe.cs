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

namespace Unimake.Business.DFe.Xml.DCe
{
    /// <summary>
    /// DCe - Declaração de Conteúdo Eletrônica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.DCe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("DCe", Namespace = "http://www.portalfiscal.inf.br/dce", IsNullable = false)]
    public class DCe : XMLBase
    {
        /// <summary>
        /// Informações da DCe
        /// </summary>
        [XmlElement("infDCe")]
        public InfDCe InfDCe { get; set; }

        /// <summary>
        /// Informações suplementares da DCe
        /// </summary>
        [XmlElement("infDCeSupl")]
        public InfDCeSupl InfDCeSupl { get; set; }

        /// <summary>
        /// Assinatura digital
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.InfDCe")]
    [ComVisible(true)]
#endif
    public class InfDCe
    {
        private string idField;
        private string chaveField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get => string.IsNullOrWhiteSpace(idField) ? "DCe" + Chave : idField;
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

                if (!string.IsNullOrWhiteSpace(idField) && idField.StartsWith("DCe", StringComparison.OrdinalIgnoreCase))
                {
                    chaveField = idField.Substring(3);
                    return chaveField;
                }

                if (Ide == null) throw new NullReferenceException("A propriedade 'Ide' está nula.");
                if (Emit == null) throw new NullReferenceException("A propriedade 'Emit' está nula.");

                var cnpjCpf = Emit.CNPJ;
                if (string.IsNullOrWhiteSpace(cnpjCpf))
                {
                    cnpjCpf = Emit.CPF;
                }

                if (string.IsNullOrWhiteSpace(cnpjCpf))
                {
                    cnpjCpf = Emit.IdOutros;
                }

                if (string.IsNullOrWhiteSpace(cnpjCpf)) throw new NullReferenceException("Emit.CNPJ, Emit.CPF ou Emit.IdOutros não foi informado.");

                var chaveSemDV = ((int)Ide.CUF).ToString() +
                    Ide.DhEmi.ToString("yy") +
                    Ide.DhEmi.ToString("MM") +
                    cnpjCpf.PadLeft(12, '0') +
                    ((int)Ide.Mod).ToString().PadLeft(2, '0') +
                    Ide.Serie.ToString().PadLeft(3, '0') +
                    Ide.NDC.ToString().PadLeft(9, '0') +
                    ((int)Ide.TpEmis).ToString() +
                    Ide.NSiteAutoriz +
                    Ide.CDC;

                Ide.CDV = CalcularDVChaveDCe(chaveSemDV);
                chaveField = chaveSemDV + Ide.CDV.ToString();

                return chaveField;
            }

            set => chaveField = value;
        }

        private static int CalcularDVChaveDCe(string chave)
        {
            const string peso = "4329876543298765432987654329876543298765432";
            var soma = 0;

            for (var i = 0; i < chave.Length; i++)
            {
                soma += (chave[i] - 48) * (peso[i] - 48);
            }

            var resto = soma % 11;
            return resto < 2 ? 0 : 11 - resto;
        }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("Fisco")]
        public Fisco Fisco { get; set; }

        [XmlElement("Marketplace")]
        public Marketplace Marketplace { get; set; }

        [XmlElement("Transportadora")]
        public Transportadora Transportadora { get; set; }

        [XmlElement("EmpEmisProp")]
        public EmpEmisProp EmpEmisProp { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

#if INTEROP
        public void AddAutXML(AutXML item) => (AutXML ?? (AutXML = new List<AutXML>())).Add(item);
        public AutXML GetAutXML(int index) => (AutXML?.Count ?? 0) == 0 ? default : AutXML[index];
        public int GetAutXMLCount => AutXML != null ? AutXML.Count : 0;
#endif

        [XmlElement("det")]
        public List<Det> Det { get; set; }

#if INTEROP
        public void AddDet(Det item) => (Det ?? (Det = new List<Det>())).Add(item);
        public Det GetDet(int index) => (Det?.Count ?? 0) == 0 ? default : Det[index];
        public int GetDetCount => Det != null ? Det.Count : 0;
#endif

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("transp")]
        public Transp Transp { get; set; }

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("obsCont")]
        public List<ObsCont> ObsCont { get; set; }

#if INTEROP
        public void AddObsCont(ObsCont item) => (ObsCont ?? (ObsCont = new List<ObsCont>())).Add(item);
        public ObsCont GetObsCont(int index) => (ObsCont?.Count ?? 0) == 0 ? default : ObsCont[index];
        public int GetObsContCount => ObsCont != null ? ObsCont.Count : 0;
#endif

        [XmlElement("obsMarketplace")]
        public List<ObsMarketplace> ObsMarketplace { get; set; }

#if INTEROP
        public void AddObsMarketplace(ObsMarketplace item) => (ObsMarketplace ?? (ObsMarketplace = new List<ObsMarketplace>())).Add(item);
        public ObsMarketplace GetObsMarketplace(int index) => (ObsMarketplace?.Count ?? 0) == 0 ? default : ObsMarketplace[index];
        public int GetObsMarketplaceCount => ObsMarketplace != null ? ObsMarketplace.Count : 0;
#endif

        [XmlElement("infDec")]
        public InfDec InfDec { get; set; }

        public bool ShouldSerializeAutXML() => AutXML?.Count > 0;
        public bool ShouldSerializeDet() => Det?.Count > 0;
        public bool ShouldSerializeObsCont() => ObsCont?.Count > 0;
        public bool ShouldSerializeObsMarketplace() => ObsMarketplace?.Count > 0;
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Ide")]
    [ComVisible(true)]
#endif
    public class Ide
    {
        private string cdcField;
        private string nSiteAutorizField;

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cDC")]
        public string CDC
        {
            get => string.IsNullOrWhiteSpace(cdcField) ? XMLUtility.GerarCodigoNumerico(NDC, 6).ToString("000000") : cdcField;
            set => cdcField = value;
        }

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; } = ModeloDFe.DCe;

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nDC")]
        public int NDC { get; set; }

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
        public TipoEmissao TpEmis { get; set; } = TipoEmissao.Normal;

        [XmlElement("tpEmit")]
        public string TpEmit { get; set; }

        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz
        {
            get => nSiteAutorizField;
            set
            {
                if (!string.IsNullOrEmpty(value) && !"0123456789".Contains(value))
                {
                    throw new Exception("A tag <nSiteAutoriz> só aceita os valores 0, 1, 2, 3, 4, 5, 6, 7, 8 e 9.");
                }

                nSiteAutorizField = value;
            }
        }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Emit")]
    [ComVisible(true)]
#endif
    public class Emit
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("idOutros")]
        public string IdOutros { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);
        public bool ShouldSerializeIdOutros() => !string.IsNullOrEmpty(IdOutros);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.EnderEmit")]
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

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("cPais")]
        public string CPais { get; set; }

        [XmlElement("xPais")]
        public string XPais { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        public bool ShouldSerializeXCpl() => !string.IsNullOrEmpty(XCpl);
        public bool ShouldSerializeFone() => !string.IsNullOrEmpty(Fone);
    }

    public class EnderDest : EnderEmit
    {
        [XmlElement("email")]
        public string Email { get; set; }

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);
        public bool ShouldSerializeCPais() => !string.IsNullOrEmpty(CPais);
        public bool ShouldSerializeXPais() => !string.IsNullOrEmpty(XPais);
    }

    public class Fisco
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xOrgao")]
        public string XOrgao { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }
    }

    public class Marketplace
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("Site")]
        public string Site { get; set; }
    }

    public class Transportadora
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }
    }

    public class EmpEmisProp : Transportadora { }

    public class Dest
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("idOutros")]
        public string IdOutros { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);
        public bool ShouldSerializeIdOutros() => !string.IsNullOrEmpty(IdOutros);
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

    public class Det
    {
        [XmlAttribute(AttributeName = "nItem")]
        public int NItem { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd { get; set; }

        public bool ShouldSerializeInfAdProd() => !string.IsNullOrEmpty(InfAdProd);
    }

    public class Prod
    {
        [XmlElement("xProd")]
        public string XProd { get; set; }

        [XmlElement("NCM")]
        public string NCM { get; set; }

        [XmlIgnore]
        public double QCom { get; set; }

        [XmlElement("qCom")]
        public string QComField
        {
            get => QCom.ToString("F4", CultureInfo.InvariantCulture);
            set => QCom = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VUnCom { get; set; }

        [XmlElement("vUnCom")]
        public string VUnComField
        {
            get => VUnCom.ToString("F2", CultureInfo.InvariantCulture);
            set => VUnCom = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VProd { get; set; }

        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }
    }

    public class Total
    {
        [XmlIgnore]
        public double VDC { get; set; }

        [XmlElement("vDC")]
        public string VDCField
        {
            get => VDC.ToString("F2", CultureInfo.InvariantCulture);
            set => VDC = Converter.ToDouble(value);
        }
    }

    public class Transp
    {
        [XmlElement("modTrans")]
        public string ModTrans { get; set; }

        [XmlElement("CNPJTransp")]
        public string CNPJTransp { get; set; }
    }

    public class InfAdic
    {
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("infCpl")]
        public string InfCpl { get; set; }

        [XmlElement("infAdMarketPlace")]
        public string InfAdMarketPlace { get; set; }

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrEmpty(InfAdFisco);
        public bool ShouldSerializeInfCpl() => !string.IsNullOrEmpty(InfCpl);
        public bool ShouldSerializeInfAdMarketPlace() => !string.IsNullOrEmpty(InfAdMarketPlace);
    }

    public class ObsCont
    {
        [XmlElement("xCampo")]
        public string XCampo { get; set; }

        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        public bool ShouldSerializeXCampo() => !string.IsNullOrEmpty(XCampo);
        public bool ShouldSerializeXTexto() => !string.IsNullOrEmpty(XTexto);
    }

    public class ObsMarketplace : ObsCont { }

    public class InfDec
    {
        [XmlElement("xObs1")]
        public string XObs1 { get; set; }

        [XmlElement("xObs2")]
        public string XObs2 { get; set; }
    }

    public class InfDCeSupl
    {
        [XmlElement("qrCodDCe")]
        public string QrCodDCe { get; set; }

        [XmlElement("urlChave")]
        public string UrlChave { get; set; }
    }
}
