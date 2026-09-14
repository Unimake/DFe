#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;

namespace Unimake.Business.DFe.Xml.NFeABI
{
/// <summary>Representa o grupo Endereco do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Endereco")]
    [ComVisible(true)]
#endif
    public class Endereco
    {
        /// <summary>Obtém ou define o conteúdo da tag xLgr da NFeABI.</summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag nro da NFeABI.</summary>
        [XmlElement("nro")]
        public string Nro { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xCpl da NFeABI.</summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xBairro da NFeABI.</summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cMun da NFeABI.</summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xMun da NFeABI.</summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag UF da NFeABI.</summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CEP da NFeABI.</summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cPais da NFeABI.</summary>
        [XmlElement("cPais")]
        public int CPais { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xPais da NFeABI.</summary>
        [XmlElement("xPais")]
        public string XPais { get; set; }
        /// <summary>Telefone do endereço.</summary>
        [XmlElement("fone")]
        public string Fone { get; set; }
        /// <summary>Indica se o membro CPais deve ser serializado.</summary>
        public bool ShouldSerializeCPais() => CPais > 0;
    }

/// <summary>Representa o grupo GIntermedCorret do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIntermedCorret")]
    [ComVisible(true)]
#endif
    public class GIntermedCorret
    {
        /// <summary>Obtém ou define o atributo nCorretagem da NFeABI.</summary>
        [XmlAttribute("nCorretagem")]
        public int NCorretagem { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CPF da NFeABI.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xNome da NFeABI.</summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vCorretagem da NFeABI.</summary>
        [XmlIgnore]
        public double VCorretagem { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vCorretagem da NFeABI.</summary>
        [XmlElement("vCorretagem")]
        public string VCorretagemField { get => NFeABIFormat.Money(VCorretagem); set => VCorretagem = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Indica se o membro CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        /// <summary>Indica se o membro CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);
    }

/// <summary>Representa o grupo GRed do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GRed")]
    [ComVisible(true)]
#endif
    public class GRed
    {
        /// <summary>Obtém ou define o conteúdo da tag pRedAliq da NFeABI.</summary>
        [XmlIgnore]
        public double PRedAliq { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pRedAliq da NFeABI.</summary>
        [XmlElement("pRedAliq")]
        public string PRedAliqField { get => NFeABIFormat.Rate(PRedAliq); set => PRedAliq = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag pAliqEfet da NFeABI.</summary>
        [XmlIgnore]
        public double PAliqEfet { get; set; }
        /// <summary>Alíquota efetiva.</summary>
        [XmlElement("pAliqEfet")]
        public string PAliqEfetField { get => NFeABIFormat.Rate(PAliqEfet); set => PAliqEfet = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo GIBSUF do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIBSUF")]
    [ComVisible(true)]
#endif
    public class GIBSUF
    {
        /// <summary>Obtém ou define o conteúdo da tag pIBSUF da NFeABI.</summary>
        [XmlIgnore]
        public double PIBSUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pIBSUF da NFeABI.</summary>
        [XmlElement("pIBSUF")]
        public string PIBSUFField { get => NFeABIFormat.Rate(PIBSUF); set => PIBSUF = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag gRed da NFeABI.</summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vIBSUF da NFeABI.</summary>
        [XmlIgnore]
        public double VIBSUF { get; set; }
        /// <summary>Valor do IBS estadual.</summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField { get => NFeABIFormat.Money(VIBSUF); set => VIBSUF = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo GIBSMun do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIBSMun")]
    [ComVisible(true)]
#endif
    public class GIBSMun
    {
        /// <summary>Obtém ou define o conteúdo da tag pIBSMun da NFeABI.</summary>
        [XmlIgnore]
        public double PIBSMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pIBSMun da NFeABI.</summary>
        [XmlElement("pIBSMun")]
        public string PIBSMunField { get => NFeABIFormat.Rate(PIBSMun); set => PIBSMun = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag gRed da NFeABI.</summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vIBSMun da NFeABI.</summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }
        /// <summary>Valor do IBS municipal.</summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField { get => NFeABIFormat.Money(VIBSMun); set => VIBSMun = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo GCBS do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GCBS")]
    [ComVisible(true)]
#endif
    public class GCBS
    {
        /// <summary>Obtém ou define o conteúdo da tag pCBS da NFeABI.</summary>
        [XmlIgnore]
        public double PCBS { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pCBS da NFeABI.</summary>
        [XmlElement("pCBS")]
        public string PCBSField { get => NFeABIFormat.Rate(PCBS); set => PCBS = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag gRed da NFeABI.</summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vCBS da NFeABI.</summary>
        [XmlIgnore]
        public double VCBS { get; set; }
        /// <summary>Valor da CBS.</summary>
        [XmlElement("vCBS")]
        public string VCBSField { get => NFeABIFormat.Money(VCBS); set => VCBS = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo GTribCompraGov do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GTribCompraGov")]
    [ComVisible(true)]
#endif
    public class GTribCompraGov
    {
        /// <summary>Obtém ou define o conteúdo da tag pAliqIBSUF da NFeABI.</summary>
        [XmlIgnore]
        public double PAliqIBSUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pAliqIBSUF da NFeABI.</summary>
        [XmlElement("pAliqIBSUF")]
        public string PAliqIBSUFField { get => NFeABIFormat.Rate(PAliqIBSUF); set => PAliqIBSUF = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vTribIBSUF da NFeABI.</summary>
        [XmlIgnore]
        public double VTribIBSUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTribIBSUF da NFeABI.</summary>
        [XmlElement("vTribIBSUF")]
        public string VTribIBSUFField { get => NFeABIFormat.Money(VTribIBSUF); set => VTribIBSUF = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag pAliqIBSMun da NFeABI.</summary>
        [XmlIgnore]
        public double PAliqIBSMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pAliqIBSMun da NFeABI.</summary>
        [XmlElement("pAliqIBSMun")]
        public string PAliqIBSMunField { get => NFeABIFormat.Rate(PAliqIBSMun); set => PAliqIBSMun = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vTribIBSMun da NFeABI.</summary>
        [XmlIgnore]
        public double VTribIBSMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTribIBSMun da NFeABI.</summary>
        [XmlElement("vTribIBSMun")]
        public string VTribIBSMunField { get => NFeABIFormat.Money(VTribIBSMun); set => VTribIBSMun = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag pAliqCBS da NFeABI.</summary>
        [XmlIgnore]
        public double PAliqCBS { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pAliqCBS da NFeABI.</summary>
        [XmlElement("pAliqCBS")]
        public string PAliqCBSField { get => NFeABIFormat.Rate(PAliqCBS); set => PAliqCBS = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vTribCBS da NFeABI.</summary>
        [XmlIgnore]
        public double VTribCBS { get; set; }
        /// <summary>Valor da CBS na compra governamental.</summary>
        [XmlElement("vTribCBS")]
        public string VTribCBSField { get => NFeABIFormat.Money(VTribCBS); set => VTribCBS = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo GIBSCBS do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIBSCBS")]
    [ComVisible(true)]
#endif
    public class GIBSCBS
    {
        /// <summary>Obtém ou define o conteúdo da tag vOperacIndiv da NFeABI.</summary>
        [XmlIgnore]
        public double VOperacIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vOperacIndiv da NFeABI.</summary>
        [XmlElement("vOperacIndiv")]
        public string VOperacIndivField { get => NFeABIFormat.Money(VOperacIndiv); set => VOperacIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vTornaIndiv da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VTornaIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTornaIndiv da NFeABI.</summary>
        [XmlElement("vTornaIndiv")]
        public string VTornaIndivField { get => NFeABIFormat.Money(VTornaIndiv); set => VTornaIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteIndiv da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VRedAjusteIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteIndiv da NFeABI.</summary>
        [XmlElement("vRedAjusteIndiv")]
        public string VRedAjusteIndivField { get => NFeABIFormat.Money(VRedAjusteIndiv); set => VRedAjusteIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vRedSocialIndiv da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VRedSocialIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedSocialIndiv da NFeABI.</summary>
        [XmlElement("vRedSocialIndiv")]
        public string VRedSocialIndivField { get => NFeABIFormat.Money(VRedSocialIndiv); set => VRedSocialIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vBC da NFeABI.</summary>
        [XmlIgnore]
        public double VBC { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vBC da NFeABI.</summary>
        [XmlElement("vBC")]
        public string VBCField { get => NFeABIFormat.Money(VBC); set => VBC = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag gIBSUF da NFeABI.</summary>
        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gIBSMun da NFeABI.</summary>
        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gCBS da NFeABI.</summary>
        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gTribCompraGov da NFeABI.</summary>
        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }
        /// <summary>Indica se o membro VTornaIndivField deve ser serializado.</summary>
        public bool ShouldSerializeVTornaIndivField() => VTornaIndiv.HasValue;
        /// <summary>Indica se o membro VRedAjusteIndivField deve ser serializado.</summary>
        public bool ShouldSerializeVRedAjusteIndivField() => VRedAjusteIndiv.HasValue;
        /// <summary>Indica se o membro VRedSocialIndivField deve ser serializado.</summary>
        public bool ShouldSerializeVRedSocialIndivField() => VRedSocialIndiv.HasValue;
    }

/// <summary>Representa o grupo GCompraGov do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GCompraGov")]
    [ComVisible(true)]
#endif
    public class GCompraGov
    {
        /// <summary>Obtém ou define o conteúdo da tag tpEnteGov da NFeABI.</summary>
        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamentalNFeABI TpEnteGov { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pRedutor da NFeABI.</summary>
        [XmlIgnore]
        public double PRedutor { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pRedutor da NFeABI.</summary>
        [XmlElement("pRedutor")]
        public string PRedutorField { get => NFeABIFormat.Rate(PRedutor); set => PRedutor = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Tipo da operação governamental.</summary>
        [XmlElement("tpOperGov")]
        public TipoOperacaoGovernamentalNFeABI TpOperGov { get; set; }
    }

/// <summary>Representa o grupo GEstornoCred do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GEstornoCred")]
    [ComVisible(true)]
#endif
    public class GEstornoCred
    {
        /// <summary>Obtém ou define o conteúdo da tag vIBSEstCred da NFeABI.</summary>
        [XmlIgnore]
        public double VIBSEstCred { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vIBSEstCred da NFeABI.</summary>
        [XmlElement("vIBSEstCred")]
        public string VIBSEstCredField { get => NFeABIFormat.Money(VIBSEstCred); set => VIBSEstCred = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vCBSEstCred da NFeABI.</summary>
        [XmlIgnore]
        public double VCBSEstCred { get; set; }
        /// <summary>Valor do estorno de crédito da CBS.</summary>
        [XmlElement("vCBSEstCred")]
        public string VCBSEstCredField { get => NFeABIFormat.Money(VCBSEstCred); set => VCBSEstCred = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo IBSCBS do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.IBSCBS")]
    [ComVisible(true)]
#endif
    public class IBSCBS
    {
        /// <summary>Obtém ou define o conteúdo da tag CST da NFeABI.</summary>
        [XmlElement("CST")]
        public string CST { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cClassTrib da NFeABI.</summary>
        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gIBSCBS da NFeABI.</summary>
        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }
        /// <summary>Grupo de estorno de crédito.</summary>
        [XmlElement("gEstornoCred")]
        public GEstornoCred GEstornoCred { get; set; }
    }

/// <summary>Representa o grupo ProtNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.ProtNFeABI")]
    [ComVisible(true)]
#endif
    public class ProtNFeABI
    {
        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag infProt da NFeABI.</summary>
        [XmlElement("infProt")]
        public InfProtNFeABI InfProt { get; set; }
        /// <summary>Assinatura digital do protocolo.</summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

/// <summary>Representa o grupo InfProtNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfProtNFeABI")]
    [ComVisible(true)]
#endif
    public class InfProtNFeABI
    {
        /// <summary>Obtém ou define o atributo Id da NFeABI.</summary>
        [XmlAttribute("Id", DataType = "ID")]
        public string Id { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tpAmb da NFeABI.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag verAplic da NFeABI.</summary>
        [XmlElement("verAplic")]
        public string VerAplic { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag chNFeABI da NFeABI.</summary>
        [XmlElement("chNFeABI")]
        public string ChNFeABI { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhRecbto { get; set; }
#else
        public DateTimeOffset DhRecbto { get; set; }
#endif
        /// <summary>Obtém ou define o conteúdo da tag dhRecbto da NFeABI.</summary>
        [XmlElement("dhRecbto")]
        public string DhRecbtoField
        {
            get => DhRecbto.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhRecbto = DateTime.Parse(value);
#else
            set => DhRecbto = DateTimeOffset.Parse(value);
#endif
        }
        /// <summary>Obtém ou define o conteúdo da tag nProt da NFeABI.</summary>
        [XmlElement("nProt")]
        public string NProt { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag digVal da NFeABI.</summary>
        [XmlElement("digVal")]
        public string DigVal { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cStat da NFeABI.</summary>
        [XmlElement("cStat")]
        public int CStat { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xMotivo da NFeABI.</summary>
        [XmlElement("xMotivo")]
        public string XMotivo { get; set; }
    }
}
