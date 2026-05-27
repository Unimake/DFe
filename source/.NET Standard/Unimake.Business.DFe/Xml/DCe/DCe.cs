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
            get
            {
                idField = "DCe" + Chave;
                return idField;
            }
            set => idField = value;
        }

        [XmlIgnore]
        public string Chave
        {
            get
            {
                if (!string.IsNullOrWhiteSpace(idField) && idField.StartsWith("DCe", StringComparison.OrdinalIgnoreCase))
                {
                    chaveField = idField.Substring(3);
                    return chaveField;
                }

                if (Ide == null) throw new NullReferenceException("A propriedade 'Ide' está nula.");
                if (Emit == null) throw new NullReferenceException("A propriedade 'Emit' está nula.");

                var cnpjCpf = "";
                switch (Ide.TpEmit)
                {
                    case TipoEmitenteDCe.AppFisco:
                        cnpjCpf = Fisco?.CNPJ;
                        break;

                    case TipoEmitenteDCe.Marketplace:
                        cnpjCpf = Marketplace?.CNPJ;
                        break;

                    case TipoEmitenteDCe.EmissorProprio:
                        cnpjCpf = Emit.CNPJ;

                        if (string.IsNullOrWhiteSpace(cnpjCpf))
                        {
                            cnpjCpf = Emit.CPF;
                        }

                        if (string.IsNullOrWhiteSpace(cnpjCpf))
                        {
                            cnpjCpf = Emit.IdOutros;
                        }
                        break;

                    case TipoEmitenteDCe.Transportadora:
                        cnpjCpf = Transportadora?.CNPJ;

                        if (string.IsNullOrWhiteSpace(cnpjCpf))
                        {
                            cnpjCpf = EmpEmisProp?.CNPJ;
                        }
                        break;
                }

                if (string.IsNullOrWhiteSpace(cnpjCpf))
                {
                    var mensagemErro = "Não foi possível identificar o CNPJ/CPF para montagem da chave da DCe conforme o tipo de emitente informado.";

                    switch (Ide.TpEmit)
                    {
                        case TipoEmitenteDCe.AppFisco:
                            mensagemErro = "Fisco.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = AppFisco.";
                            break;

                        case TipoEmitenteDCe.Marketplace:
                            mensagemErro = "Marketplace.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = Marketplace.";
                            break;

                        case TipoEmitenteDCe.EmissorProprio:
                            mensagemErro = "Emit.CNPJ, Emit.CPF ou Emit.IdOutros não foi informado para montar a chave da DCe quando Ide.TpEmit = EmissorProprio.";
                            break;

                        case TipoEmitenteDCe.Transportadora:
                            mensagemErro = "Transportadora.CNPJ não foi informado para montar a chave da DCe quando Ide.TpEmit = Transportadora.";
                            break;
                    }

                    throw new NullReferenceException(mensagemErro);
                }

                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = (UFBrasil)(int)Ide.CUF,
                    AnoEmissao = Ide.DhEmi.ToString("yy"),
                    MesEmissao = Ide.DhEmi.ToString("MM"),
                    CNPJCPFEmissor = cnpjCpf.PadLeft(14, '0'),
                    Modelo = (ModeloDFe)(int)Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NDC,
                    TipoEmissao = Ide.TpEmis,
                    TipoEmitenteDCe = Ide.TpEmit,
                    NSiteAutoriz = Ide.NSiteAutoriz,
                    CodigoNumerico = Ide.CDC
                };

                chaveField = XMLUtility.MontarChaveDCe(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

                return chaveField;
            }

            set => chaveField = value;
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

        /// <summary>
        /// Identificação da ECT (Correios).
        /// </summary>
        [XmlElement("ECT")]
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

        /// <summary>
        /// Observações de uso livre do emitente.
        /// </summary>
        [XmlElement("obsEmit")]
        public List<ObsCont> ObsEmit { get; set; }

        /// <summary>
        /// Campo mantido por compatibilidade, representando as observações do emitente.
        /// </summary>
        [XmlIgnore]
        public List<ObsCont> ObsCont
        {
            get => ObsEmit;
            set => ObsEmit = value;
        }

#if INTEROP
        public void AddObsCont(ObsCont item) => (ObsEmit ?? (ObsEmit = new List<ObsCont>())).Add(item);
        public ObsCont GetObsCont(int index) => (ObsEmit?.Count ?? 0) == 0 ? default : ObsEmit[index];
        public int GetObsContCount => ObsEmit != null ? ObsEmit.Count : 0;
#endif

        /// <summary>
        /// Observações de uso livre do fisco.
        /// </summary>
        [XmlElement("obsFisco")]
        public List<ObsCont> ObsFisco { get; set; }

#if INTEROP
        public void AddObsFisco(ObsCont item) => (ObsFisco ?? (ObsFisco = new List<ObsCont>())).Add(item);
        public ObsCont GetObsFisco(int index) => (ObsFisco?.Count ?? 0) == 0 ? default : ObsFisco[index];
        public int GetObsFiscoCount => ObsFisco != null ? ObsFisco.Count : 0;
#endif

        [XmlElement("obsMarketplace")]
        public List<ObsMarketplace> ObsMarketplace { get; set; }

#if INTEROP
        public void AddObsMarketplace(ObsMarketplace item) => (ObsMarketplace ?? (ObsMarketplace = new List<ObsMarketplace>())).Add(item);
        public ObsMarketplace GetObsMarketplace(int index) => (ObsMarketplace?.Count ?? 0) == 0 ? default : ObsMarketplace[index];
        public int GetObsMarketplaceCount => ObsMarketplace != null ? ObsMarketplace.Count : 0;
#endif

        /// <summary>
        /// Observações de uso livre da ECT (Correios).
        /// </summary>
        [XmlElement("obsECT")]
        public List<ObsCont> ObsECT { get; set; }

#if INTEROP
        public void AddObsECT(ObsCont item) => (ObsECT ?? (ObsECT = new List<ObsCont>())).Add(item);
        public ObsCont GetObsECT(int index) => (ObsECT?.Count ?? 0) == 0 ? default : ObsECT[index];
        public int GetObsECTCount => ObsECT != null ? ObsECT.Count : 0;
#endif

        [XmlElement("infDec")]
        public InfDec InfDec { get; set; }

        public bool ShouldSerializeAutXML() => AutXML?.Count > 0;
        public bool ShouldSerializeDet() => Det?.Count > 0;
        /// <summary>
        /// Verifica se existem observações do emitente para serializar.
        /// </summary>
        /// <returns>True quando houver observações de emitente.</returns>
        public bool ShouldSerializeObsEmit() => ObsEmit?.Count > 0;

        /// <summary>
        /// Verifica se existem observações do fisco para serializar.
        /// </summary>
        /// <returns>True quando houver observações do fisco.</returns>
        public bool ShouldSerializeObsFisco() => ObsFisco?.Count > 0;

        /// <summary>
        /// Verifica se existem observações do marketplace para serializar.
        /// </summary>
        /// <returns>True quando houver observações do marketplace.</returns>
        public bool ShouldSerializeObsMarketplace() => ObsMarketplace?.Count > 0;

        /// <summary>
        /// Verifica se existem observações da ECT para serializar.
        /// </summary>
        /// <returns>True quando houver observações da ECT.</returns>
        public bool ShouldSerializeObsECT() => ObsECT?.Count > 0;
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
        public TipoEmitenteDCe TpEmit { get; set; } = TipoEmitenteDCe.EmissorProprio;

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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.EnderDest")]
    [ComVisible(true)]
#endif
    public class EnderDest : EnderEmit
    {
        [XmlElement("email")]
        public string Email { get; set; }

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);
        public bool ShouldSerializeCPais() => !string.IsNullOrEmpty(CPais);
        public bool ShouldSerializeXPais() => !string.IsNullOrEmpty(XPais);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Fisco")]
    [ComVisible(true)]
#endif
    public class Fisco
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xOrgao")]
        public string XOrgao { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Marketplace")]
    [ComVisible(true)]
#endif
    public class Marketplace
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("Site")]
        public string Site { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Transportadora")]
    [ComVisible(true)]
#endif
    public class Transportadora
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }
    }

    /// <summary>
    /// Identificação da ECT (Correios).
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.ECT")]
    [ComVisible(true)]
#endif
    public class ECT : Transportadora { }

    /// <summary>
    /// Classe mantida para compatibilidade e mapeada para o grupo ECT.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.EmpEmisProp")]
    [ComVisible(true)]
#endif
    public class EmpEmisProp : ECT { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Dest")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.AutXML")]
    [ComVisible(true)]
#endif
    public class AutXML
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Det")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Prod")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Total")]
    [ComVisible(true)]
#endif
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

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.Transp")]
    [ComVisible(true)]
#endif
    public class Transp
    {
        [XmlElement("modTrans")]
        public ModalidadeTransporteDCe ModTrans { get; set; }

        [XmlElement("CNPJTransp")]
        public string CNPJTransp { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.InfAdic")]
    [ComVisible(true)]
#endif
    public class InfAdic
    {
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("infCpl")]
        public string InfCpl { get; set; }

        /// <summary>
        /// Informações adicionais do marketplace.
        /// </summary>
        [XmlElement("infAdMarketplace")]
        public string InfAdMarketPlace { get; set; }

        /// <summary>
        /// Informações adicionais da ECT (Correios).
        /// </summary>
        [XmlElement("infAdECT")]
        public string InfAdECT { get; set; }

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrEmpty(InfAdFisco);
        public bool ShouldSerializeInfCpl() => !string.IsNullOrEmpty(InfCpl);
        public bool ShouldSerializeInfAdMarketPlace() => !string.IsNullOrEmpty(InfAdMarketPlace);
        public bool ShouldSerializeInfAdECT() => !string.IsNullOrEmpty(InfAdECT);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.ObsCont")]
    [ComVisible(true)]
#endif
    public class ObsCont
    {
        /// <summary>
        /// Identificação do campo de observação.
        /// </summary>
        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }

        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        public bool ShouldSerializeXCampo() => !string.IsNullOrEmpty(XCampo);
        public bool ShouldSerializeXTexto() => !string.IsNullOrEmpty(XTexto);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.ObsMarketplace")]
    [ComVisible(true)]
#endif
    public class ObsMarketplace : ObsCont { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.InfDec")]
    [ComVisible(true)]
#endif
    public class InfDec
    {
        [XmlElement("xObs1")]
        public string XObs1 { get; set; }

        [XmlElement("xObs2")]
        public string XObs2 { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.DCe.InfDCeSupl")]
    [ComVisible(true)]
#endif
    public class InfDCeSupl
    {
        [XmlElement("qrCodDCe")]
        public string QrCodDCe { get; set; }

        [XmlElement("urlChave")]
        public string UrlChave { get; set; }
    }
}
