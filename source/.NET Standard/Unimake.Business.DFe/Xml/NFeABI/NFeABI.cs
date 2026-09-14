#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.NFeABI
{
    internal static class NFeABIFormat
    {
        internal static string Money(double value) => value.ToString("F2", CultureInfo.InvariantCulture);
        internal static string Money(double? value) => value?.ToString("F2", CultureInfo.InvariantCulture);
        internal static string Rate(double value) => value.ToString("F4", CultureInfo.InvariantCulture);
        internal static string Rate(double? value) => value?.ToString("F4", CultureInfo.InvariantCulture);
        internal static double ParseDecimal(string value) => Converter.ToDouble(value);
    }

/// <summary>Representa o grupo NFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.NFeABI")]
    [ComVisible(true)]
#endif
    [Serializable]
    [XmlRoot("NFeABI", Namespace = "http://www.portalfiscal.inf.br/nfeabi", IsNullable = false)]
    public class NFeABI : XMLBase
    {
        /// <summary>Obtém ou define o conteúdo da tag infNFeABI da NFeABI.</summary>
        [XmlElement("infNFeABI")]
        public InfNFeABI InfNFeABI { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag infNFeSupl da NFeABI.</summary>
        [XmlElement("infNFeSupl")]
        public InfNFeSupl InfNFeSupl { get; set; }

        /// <summary>Assinatura XML digital da NFeABI.</summary>
        [XmlElement("Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

/// <summary>Representa o grupo InfNFeABI do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfNFeABI")]
    [ComVisible(true)]
#endif
    public class InfNFeABI
    {
        private string idField;

        /// <summary>Obtém ou define o atributo versao da NFeABI.</summary>
        [XmlAttribute("versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>Obtém ou define o atributo Id da NFeABI.</summary>
        [XmlAttribute("Id", DataType = "ID")]
        public string Id
        {
            get => string.IsNullOrWhiteSpace(idField) ? "NFeABI" + Chave : idField;
            set => idField = value;
        }

        /// <summary>Obtém a chave de acesso da NFeABI calculada a partir dos campos de identificação.</summary>
        [XmlIgnore]
        public string Chave
        {
            get
            {
                if (Ide == null) throw new NullReferenceException("A propriedade 'Ide' está nula.");
                if (Emit == null) throw new NullReferenceException("A propriedade 'Emit' está nula.");
                var documento = !string.IsNullOrWhiteSpace(Emit.CNPJ) ? Emit.CNPJ : Emit.CPF;
                if (string.IsNullOrWhiteSpace(documento)) throw new NullReferenceException("Emit.CNPJ ou Emit.CPF não foi informado.");

                var conteudo = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = Ide.CUF,
                    AnoEmissao = Ide.DhEmi.ToString("yy"),
                    MesEmissao = Ide.DhEmi.ToString("MM"),
                    CNPJCPFEmissor = documento.PadLeft(14, '0'),
                    Modelo = Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NNF,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    NSiteAutoriz = Ide.NSiteAutoriz,
                    CodigoNumerico = Ide.CNF
                };

                var chave = XMLUtility.MontarChaveNFGas(ref conteudo);
                Ide.CDV = conteudo.DigitoVerificador;
                return chave;
            }
            set => throw new Exception("Não é permitido atribuir valor para a propriedade Chave. Ela é calculada automaticamente.");
        }

        /// <summary>Obtém ou define o conteúdo da tag ide da NFeABI.</summary>
        [XmlElement("ide")]
        public Ide Ide { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag NFref da NFeABI.</summary>
        [XmlElement("NFref")]
        public List<NFref> NFref { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag emit da NFeABI.</summary>
        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag transmit da NFeABI.</summary>
        [XmlElement("transmit")]
        public List<Transmit> Transmit { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag adquirente da NFeABI.</summary>
        [XmlElement("adquirente")]
        public List<Adquirente> Adquirente { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag imovel da NFeABI.</summary>
        [XmlElement("imovel")]
        public Imovel Imovel { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag autXML da NFeABI.</summary>
        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag infOper da NFeABI.</summary>
        [XmlElement("infOper")]
        public InfOper InfOper { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag gInfTrib da NFeABI.</summary>
        [XmlElement("gInfTrib")]
        public GInfTrib GInfTrib { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag pag da NFeABI.</summary>
        [XmlElement("pag")]
        public Pag Pag { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag total da NFeABI.</summary>
        [XmlElement("total")]
        public Total Total { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag infAdic da NFeABI.</summary>
        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag infRespTec da NFeABI.</summary>
        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

#if INTEROP
        /// <summary>Adiciona um item à coleção NFref.</summary>
        public void AddNFref(NFref item) { if (NFref == null) NFref = new List<NFref>(); NFref.Add(item); }
        /// <summary>Obtém um item da coleção NFref pelo índice.</summary>
        public NFref GetNFref(int index) => NFref[index];
        /// <summary>Obtém a quantidade de itens da coleção NFref.</summary>
        public int GetNFrefCount => NFref?.Count ?? 0;
        /// <summary>Adiciona um item à coleção Transmit.</summary>
        public void AddTransmit(Transmit item) { if (Transmit == null) Transmit = new List<Transmit>(); Transmit.Add(item); }
        /// <summary>Obtém um item da coleção Transmit pelo índice.</summary>
        public Transmit GetTransmit(int index) => Transmit[index];
        /// <summary>Obtém a quantidade de itens da coleção Transmit.</summary>
        public int GetTransmitCount => Transmit?.Count ?? 0;
        /// <summary>Adiciona um item à coleção Adquirente.</summary>
        public void AddAdquirente(Adquirente item) { if (Adquirente == null) Adquirente = new List<Adquirente>(); Adquirente.Add(item); }
        /// <summary>Obtém um item da coleção Adquirente pelo índice.</summary>
        public Adquirente GetAdquirente(int index) => Adquirente[index];
        /// <summary>Obtém a quantidade de itens da coleção Adquirente.</summary>
        public int GetAdquirenteCount => Adquirente?.Count ?? 0;
        /// <summary>Adiciona um item à coleção AutXML.</summary>
        public void AddAutXML(AutXML item) { if (AutXML == null) AutXML = new List<AutXML>(); AutXML.Add(item); }
        /// <summary>Obtém um item da coleção AutXML pelo índice.</summary>
        public AutXML GetAutXML(int index) => AutXML[index];
        /// <summary>Obtém a quantidade de itens da coleção AutXML.</summary>
        public int GetAutXMLCount => AutXML?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo Ide do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Ide")]
    [ComVisible(true)]
#endif
    public class Ide
    {
        private string cNFField;

        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag cUF da NFeABI.</summary>
        [XmlElement("cUF")]
        public int CUFField { get => (int)CUF; set => CUF = (UFBrasil)value; }

        /// <summary>Obtém ou define o conteúdo da tag cNF da NFeABI.</summary>
        [XmlElement("cNF")]
        public string CNF
        {
            get => string.IsNullOrWhiteSpace(cNFField) ? XMLUtility.GerarCodigoNumerico(NNF, 7).ToString("0000000") : cNFField;
            set => cNFField = value;
        }

        /// <summary>Obtém ou define o conteúdo da tag mod da NFeABI.</summary>
        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; } = ModeloDFe.NFeABI;

        /// <summary>Obtém ou define o conteúdo da tag serie da NFeABI.</summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag nNF da NFeABI.</summary>
        [XmlElement("nNF")]
        public int NNF { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag dhEmi da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        /// <summary>Obtém ou define o conteúdo da tag dhEmi da NFeABI.</summary>
        [XmlElement("dhEmi")]
        public string DhEmiField
        {
            get => DhEmi.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhEmi = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhEmi = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }

        /// <summary>Obtém ou define o conteúdo da tag tpNF da NFeABI.</summary>
        [XmlElement("tpNF")]
        public TipoNotaFiscalNFeABI TpNF { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag tpImp da NFeABI.</summary>
        [XmlElement("tpImp")]
        public TipoImpressaoNFeABI TpImp { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag gModNat da NFeABI.</summary>
        [XmlElement("gModNat")]
        public GModNat GModNat { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag tpEmis da NFeABI.</summary>
        [XmlElement("tpEmis")]
        public TipoEmissaoNFeABI TpEmis { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag nSiteAutoriz da NFeABI.</summary>
        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag cDV da NFeABI.</summary>
        [XmlElement("cDV")]
        public int CDV { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag tpAmb da NFeABI.</summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag finNFe da NFeABI.</summary>
        [XmlElement("finNFe")]
        public FinalidadeNFeABI FinNFe { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag procEmi da NFeABI.</summary>
        [XmlElement("procEmi")]
        public ProcessoEmissaoNFeABI ProcEmi { get; set; }

        /// <summary>Obtém ou define o conteúdo da tag verProc da NFeABI.</summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        /// <summary>Dados da contingência.</summary>
        [XmlElement("gCont")]
        public GCont GCont { get; set; }
    }

/// <summary>Representa o grupo GModNat do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GModNat")]
    [ComVisible(true)]
#endif
    public class GModNat
    {
        /// <summary>Obtém ou define o conteúdo da tag modOper da NFeABI.</summary>
        [XmlElement("modOper")]
        public string ModOper { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag natOper da NFeABI.</summary>
        [XmlElement("natOper")]
        public string NatOper { get; set; }
        /// <summary>Detalhamento codificado da operação.</summary>
        [XmlElement("detOper")]
        public string DetOper { get; set; }
    }

/// <summary>Representa o grupo GCont do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GCont")]
    [ComVisible(true)]
#endif
    public class GCont
    {
        /// <summary>Obtém ou define o conteúdo da tag dhCont da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset DhCont { get; set; }
#endif
        /// <summary>Obtém ou define o conteúdo da tag dhCont da NFeABI.</summary>
        [XmlElement("dhCont")]
        public string DhContField
        {
            get => DhCont.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhCont = DateTime.Parse(value, CultureInfo.InvariantCulture);
#else
            set => DhCont = DateTimeOffset.Parse(value, CultureInfo.InvariantCulture);
#endif
        }
        /// <summary>Justificativa da contingência.</summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }
    }

/// <summary>Representa o grupo NFref do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.NFref")]
    [ComVisible(true)]
#endif
    public class NFref
    {
        /// <summary>Chave da NFeABI referenciada.</summary>
        [XmlElement("refNFeABI")]
        public string RefNFeABI { get; set; }
    }

/// <summary>Representa o grupo Emit do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Emit")]
    [ComVisible(true)]
#endif
    public class Emit
    {
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CPF da NFeABI.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xNome da NFeABI.</summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xFant da NFeABI.</summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag enderEmit da NFeABI.</summary>
        [XmlElement("enderEmit")]
        public Endereco EnderEmit { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tpEmit da NFeABI.</summary>
        [XmlElement("tpEmit")]
        public string TpEmit { get; set; }
        /// <summary>Indica se o membro CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        /// <summary>Indica se o membro CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);
    }

/// <summary>Representa o grupo Transmit do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Transmit")]
    [ComVisible(true)]
#endif
    public class Transmit
    {
        /// <summary>Obtém ou define o atributo nTransmit da NFeABI.</summary>
        [XmlAttribute("nTransmit")]
        public int NTransmit { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indDeclarante da NFeABI.</summary>
        [XmlElement("indDeclarante")]
        public IndicadorSimNaoNFeABI IndDeclarante { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CPF da NFeABI.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xNome da NFeABI.</summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag enderTransmit da NFeABI.</summary>
        [XmlElement("enderTransmit")]
        public Endereco EnderTransmit { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pTransIndiv da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? PTransIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pTransIndiv da NFeABI.</summary>
        [XmlElement("pTransIndiv")]
        public string PTransIndivField { get => NFeABIFormat.Rate(PTransIndiv); set => PTransIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag indContrib da NFeABI.</summary>
        [XmlElement("indContrib")]
        public IndicadorContribuinteNFeABI IndContrib { get; set; } = (IndicadorContribuinteNFeABI)(-1);
        /// <summary>Indica se o membro IndDeclarante deve ser serializado.</summary>
        public bool ShouldSerializeIndDeclarante() => IndDeclarante != (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Indica se o membro CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        /// <summary>Indica se o membro CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);
        /// <summary>Indica se o membro PTransIndivField deve ser serializado.</summary>
        public bool ShouldSerializePTransIndivField() => PTransIndiv.HasValue;
        /// <summary>Indica se o membro IndContrib deve ser serializado.</summary>
        public bool ShouldSerializeIndContrib() => IndContrib != (IndicadorContribuinteNFeABI)(-1);
    }

/// <summary>Representa o grupo Adquirente do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Adquirente")]
    [ComVisible(true)]
#endif
    public class Adquirente
    {
        /// <summary>Obtém ou define o atributo nAdquir da NFeABI.</summary>
        [XmlAttribute("nAdquir")]
        public int NAdquir { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indDeclarante da NFeABI.</summary>
        [XmlElement("indDeclarante")]
        public IndicadorSimNaoNFeABI IndDeclarante { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CPF da NFeABI.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xNome da NFeABI.</summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag enderAdquirente da NFeABI.</summary>
        [XmlElement("enderAdquirente")]
        public Endereco EnderAdquirente { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pAquisicao da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? PAquisicao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pAquisicao da NFeABI.</summary>
        [XmlElement("pAquisicao")]
        public string PAquisicaoField { get => NFeABIFormat.Rate(PAquisicao); set => PAquisicao = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag indContrib da NFeABI.</summary>
        [XmlElement("indContrib")]
        public IndicadorContribuinteNFeABI IndContrib { get; set; } = (IndicadorContribuinteNFeABI)(-1);
        /// <summary>Indica se o membro IndDeclarante deve ser serializado.</summary>
        public bool ShouldSerializeIndDeclarante() => IndDeclarante != (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Indica se o membro CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        /// <summary>Indica se o membro CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);
        /// <summary>Indica se o membro PAquisicaoField deve ser serializado.</summary>
        public bool ShouldSerializePAquisicaoField() => PAquisicao.HasValue;
        /// <summary>Indica se o membro IndContrib deve ser serializado.</summary>
        public bool ShouldSerializeIndContrib() => IndContrib != (IndicadorContribuinteNFeABI)(-1);
    }

/// <summary>Representa o grupo CadastroImovel do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.CadastroImovel")]
    [ComVisible(true)]
#endif
    public class CadastroImovel
    {
        /// <summary>Obtém ou define o conteúdo da tag cCIB da NFeABI.</summary>
        [XmlElement("cCIB")]
        public string CCIB { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indIPTU da NFeABI.</summary>
        [XmlElement("indIPTU")]
        public IndicadorSimNaoNFeABI IndIPTU { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag nIptu da NFeABI.</summary>
        [XmlElement("nIptu")]
        public string NIptu { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indIncra da NFeABI.</summary>
        [XmlElement("indIncra")]
        public IndicadorSimNaoNFeABI IndIncra { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag nIncra da NFeABI.</summary>
        [XmlElement("nIncra")]
        public string NIncra { get; set; }
        /// <summary>Indica se o membro IndIPTU deve ser serializado.</summary>
        public bool ShouldSerializeIndIPTU() => IndIPTU != (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Indica se o membro IndIncra deve ser serializado.</summary>
        public bool ShouldSerializeIndIncra() => IndIncra != (IndicadorSimNaoNFeABI)(-1);
    }

/// <summary>Representa o grupo Imovel do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Imovel")]
    [ComVisible(true)]
#endif
    public class Imovel
    {
        /// <summary>Obtém ou define o conteúdo da tag tipo da NFeABI.</summary>
        [XmlElement("tipo")]
        public TipoImovelNFeABI Tipo { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cadastro da NFeABI.</summary>
        [XmlElement("cadastro")]
        public CadastroImovel Cadastro { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag espImovel da NFeABI.</summary>
        [XmlElement("espImovel")]
        public string EspImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xOutrosEspecies da NFeABI.</summary>
        [XmlElement("xOutrosEspecies")]
        public string XOutrosEspecies { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag enquadramento da NFeABI.</summary>
        [XmlElement("enquadramento")]
        public string Enquadramento { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag enderImovel da NFeABI.</summary>
        [XmlElement("enderImovel")]
        public Endereco EnderImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag obsLocalImovel da NFeABI.</summary>
        [XmlElement("obsLocalImovel")]
        public string ObsLocalImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag areaTotal da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? AreaTotal { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag areaTotal da NFeABI.</summary>
        [XmlElement("areaTotal")]
        public string AreaTotalField { get => NFeABIFormat.Money(AreaTotal); set => AreaTotal = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag uAreaTotal da NFeABI.</summary>
        [XmlElement("uAreaTotal")]
        public string UAreaTotal { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag cartorioRegistro da NFeABI.</summary>
        [XmlElement("cartorioRegistro")]
        public string CartorioRegistro { get; set; }
        /// <summary>Matrícula ou transcrição do imóvel.</summary>
        [XmlElement("matricTransc")]
        public string MatricTransc { get; set; }
        /// <summary>Indica se o membro AreaTotalField deve ser serializado.</summary>
        public bool ShouldSerializeAreaTotalField() => AreaTotal.HasValue;
    }

/// <summary>Representa o grupo AutXML do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.AutXML")]
    [ComVisible(true)]
#endif
    public class AutXML
    {
        /// <summary>Obtém ou define o atributo nAutXML da NFeABI.</summary>
        [XmlAttribute("nAutXML")]
        public string NAutXML { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CPF da NFeABI.</summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }
        /// <summary>Indica se o membro CNPJ deve ser serializado.</summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        /// <summary>Indica se o membro CPF deve ser serializado.</summary>
        public bool ShouldSerializeCPF() => string.IsNullOrWhiteSpace(CNPJ) && !string.IsNullOrWhiteSpace(CPF);
    }

/// <summary>Representa o grupo InfOper do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfOper")]
    [ComVisible(true)]
#endif
    public class InfOper
    {
        /// <summary>Obtém ou define o conteúdo da tag pTransImovel da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? PTransImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag pTransImovel da NFeABI.</summary>
        [XmlElement("pTransImovel")]
        public string PTransImovelField { get => NFeABIFormat.Rate(PTransImovel); set => PTransImovel = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vTotalOperacao da NFeABI.</summary>
        [XmlIgnore]
        public double VTotalOperacao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTotalOperacao da NFeABI.</summary>
        [XmlElement("vTotalOperacao")]
        public string VTotalOperacaoField { get => NFeABIFormat.Money(VTotalOperacao); set => VTotalOperacao = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag indTorna da NFeABI.</summary>
        [XmlElement("indTorna")]
        public IndicadorSimNaoNFeABI IndTorna { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag gTorna da NFeABI.</summary>
        [XmlElement("gTorna")]
        public GTorna GTorna { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gIncorpLote da NFeABI.</summary>
        [XmlElement("gIncorpLote")]
        public GIncorpLote GIncorpLote { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag gRedAjusteImovel da NFeABI.</summary>
        [XmlElement("gRedAjusteImovel")]
        public GRedAjusteImovel GRedAjusteImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indRedSocial da NFeABI.</summary>
        [XmlElement("indRedSocial")]
        public IndicadorSimNaoNFeABI IndRedSocial { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag vRedSocial da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VRedSocial { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedSocial da NFeABI.</summary>
        [XmlElement("vRedSocial")]
        public string VRedSocialField { get => NFeABIFormat.Money(VRedSocial); set => VRedSocial = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag instrumento da NFeABI.</summary>
        [XmlElement("instrumento")]
        public Instrumento Instrumento { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag indIntermedCorret da NFeABI.</summary>
        [XmlElement("indIntermedCorret")]
        public IndicadorSimNaoNFeABI IndIntermedCorret { get; set; } = (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Obtém ou define o conteúdo da tag gIntermedCorret da NFeABI.</summary>
        [XmlElement("gIntermedCorret")]
        public List<GIntermedCorret> GIntermedCorret { get; set; }
        /// <summary>Indica se o campo pTransImovel deve ser serializado.</summary>
        public bool ShouldSerializePTransImovelField() => PTransImovel.HasValue;
        /// <summary>Indica se o campo indTorna deve ser serializado.</summary>
        public bool ShouldSerializeIndTorna() => IndTorna != (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Indica se o campo indRedSocial deve ser serializado.</summary>
        public bool ShouldSerializeIndRedSocial() => IndRedSocial != (IndicadorSimNaoNFeABI)(-1);
        /// <summary>Indica se o campo vRedSocial deve ser serializado.</summary>
        public bool ShouldSerializeVRedSocialField() => VRedSocial.HasValue;
        /// <summary>Indica se o campo indIntermedCorret deve ser serializado.</summary>
        public bool ShouldSerializeIndIntermedCorret() => IndIntermedCorret != (IndicadorSimNaoNFeABI)(-1);
#if INTEROP
        /// <summary>Adiciona um item à coleção GIntermedCorret.</summary>
        public void AddGIntermedCorret(GIntermedCorret item) { if (GIntermedCorret == null) GIntermedCorret = new List<GIntermedCorret>(); GIntermedCorret.Add(item); }
        /// <summary>Obtém um item da coleção GIntermedCorret pelo índice.</summary>
        public GIntermedCorret GetGIntermedCorret(int index) => GIntermedCorret[index];
        /// <summary>Obtém a quantidade de itens da coleção GIntermedCorret.</summary>
        public int GetGIntermedCorretCount => GIntermedCorret?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo GTorna do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GTorna")]
    [ComVisible(true)]
#endif
    public class GTorna
    {
        /// <summary>Obtém ou define o conteúdo da tag vTorna da NFeABI.</summary>
        [XmlIgnore]
        public double VTorna { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTorna da NFeABI.</summary>
        [XmlElement("vTorna")]
        public string VTornaField { get => NFeABIFormat.Money(VTorna); set => VTorna = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag imovelTorna da NFeABI.</summary>
        [XmlElement("imovelTorna")]
        public List<Imovel> ImovelTorna { get; set; }
#if INTEROP
        /// <summary>Adiciona um item à coleção ImovelTorna.</summary>
        public void AddImovelTorna(Imovel item) { if (ImovelTorna == null) ImovelTorna = new List<Imovel>(); ImovelTorna.Add(item); }
        /// <summary>Obtém um item da coleção ImovelTorna pelo índice.</summary>
        public Imovel GetImovelTorna(int index) => ImovelTorna[index];
        /// <summary>Obtém a quantidade de itens da coleção ImovelTorna.</summary>
        public int GetImovelTornaCount => ImovelTorna?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo GIncorpLote do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GIncorpLote")]
    [ComVisible(true)]
#endif
    public class GIncorpLote
    {
        /// <summary>Obtém ou define o conteúdo da tag indIncorpLote da NFeABI.</summary>
        [XmlElement("indIncorpLote")]
        public IndicadorSimNaoNFeABI IndIncorpLote { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag CNPJempImob da NFeABI.</summary>
        [XmlElement("CNPJempImob")]
        public string CNPJempImob { get; set; }
        /// <summary>Código CIB da obra.</summary>
        [XmlElement("cCIBObra")]
        public string CCIBObra { get; set; }
    }

/// <summary>Representa o grupo GRedAjusteImovel do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GRedAjusteImovel")]
    [ComVisible(true)]
#endif
    public class GRedAjusteImovel
    {
        /// <summary>Obtém ou define o conteúdo da tag vInicial da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VInicial { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vInicial da NFeABI.</summary>
        [XmlElement("vInicial")]
        public string VInicialField { get => NFeABIFormat.Money(VInicial); set => VInicial = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vITBI da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VITBI { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vITBI da NFeABI.</summary>
        [XmlElement("vITBI")]
        public string VITBIField { get => NFeABIFormat.Money(VITBI); set => VITBI = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vLaudemio da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VLaudemio { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vLaudemio da NFeABI.</summary>
        [XmlElement("vLaudemio")]
        public string VLaudemioField { get => NFeABIFormat.Money(VLaudemio); set => VLaudemio = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vContrap da NFeABI.</summary>
        [XmlIgnore]
#if INTEROP
        [ComVisible(false)]
#endif
        public double? VContrap { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vContrap da NFeABI.</summary>
        [XmlElement("vContrap")]
        public string VContrapField { get => NFeABIFormat.Money(VContrap); set => VContrap = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteImovel da NFeABI.</summary>
        [XmlIgnore]
        public double VRedAjusteImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteImovel da NFeABI.</summary>
        [XmlElement("vRedAjusteImovel")]
        public string VRedAjusteImovelField { get => NFeABIFormat.Money(VRedAjusteImovel); set => VRedAjusteImovel = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Indica se o membro VInicialField deve ser serializado.</summary>
        public bool ShouldSerializeVInicialField() => VInicial.HasValue;
        /// <summary>Indica se o membro VITBIField deve ser serializado.</summary>
        public bool ShouldSerializeVITBIField() => VITBI.HasValue;
        /// <summary>Indica se o membro VLaudemioField deve ser serializado.</summary>
        public bool ShouldSerializeVLaudemioField() => VLaudemio.HasValue;
        /// <summary>Indica se o membro VContrapField deve ser serializado.</summary>
        public bool ShouldSerializeVContrapField() => VContrap.HasValue;
    }

/// <summary>Representa o grupo Instrumento do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Instrumento")]
    [ComVisible(true)]
#endif
    public class Instrumento
    {
        /// <summary>Obtém ou define o conteúdo da tag tpInstrumento da NFeABI.</summary>
        [XmlElement("tpInstrumento")]
        public string TpInstrumento { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xOutrosInst da NFeABI.</summary>
        [XmlElement("xOutrosInst")]
        public string XOutrosInst { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag dtInstrumento da NFeABI.</summary>
        [XmlIgnore]
        public DateTime DtInstrumento { get; set; }
        /// <summary>Data do instrumento.</summary>
        [XmlElement("dtInstrumento")]
        public string DtInstrumentoField { get => DtInstrumento.ToString("yyyy-MM-dd"); set => DtInstrumento = DateTime.Parse(value, CultureInfo.InvariantCulture); }
    }

/// <summary>Representa o grupo GInfTrib do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.GInfTrib")]
    [ComVisible(true)]
#endif
    public class GInfTrib
    {
        /// <summary>Obtém ou define o conteúdo da tag gCompraGov da NFeABI.</summary>
        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }
        /// <summary>Tributação IBS e CBS.</summary>
        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }
    }

/// <summary>Representa o grupo DetPagImovel do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.DetPagImovel")]
    [ComVisible(true)]
#endif
    public class DetPagImovel
    {
        /// <summary>Obtém ou define o atributo nDetPag da NFeABI.</summary>
        [XmlAttribute("nDetPag")]
        public string NDetPag { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tPag da NFeABI.</summary>
        [XmlElement("tPag")]
        public string TPag { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xPag da NFeABI.</summary>
        [XmlElement("xPag")]
        public string XPag { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vPag da NFeABI.</summary>
        [XmlIgnore]
        public double VPag { get; set; }
        /// <summary>Valor do pagamento.</summary>
        [XmlElement("vPag")]
        public string VPagField { get => NFeABIFormat.Money(VPag); set => VPag = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo DetPagIncorpLote do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.DetPagIncorpLote")]
    [ComVisible(true)]
#endif
    public class DetPagIncorpLote
    {
        /// <summary>Obtém ou define o atributo nDetPag da NFeABI.</summary>
        [XmlAttribute("nDetPag")]
        public string NDetPag { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag tParcela da NFeABI.</summary>
        [XmlElement("tParcela")]
        public string TParcela { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag nParcela da NFeABI.</summary>
        [XmlElement("nParcela")]
        public string NParcela { get; set; }
        /// <summary>Descrição da parcela.</summary>
        [XmlElement("xParcela")]
        public string XParcela { get; set; }
    }

/// <summary>Representa o grupo Pag do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Pag")]
    [ComVisible(true)]
#endif
    public class Pag
    {
        /// <summary>Obtém ou define o conteúdo da tag detPagImovel da NFeABI.</summary>
        [XmlElement("detPagImovel")]
        public List<DetPagImovel> DetPagImovel { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag detPagIncorpLote da NFeABI.</summary>
        [XmlElement("detPagIncorpLote")]
        public List<DetPagIncorpLote> DetPagIncorpLote { get; set; }
#if INTEROP
        /// <summary>Adiciona um item à coleção DetPagImovel.</summary>
        public void AddDetPagImovel(DetPagImovel item) { if (DetPagImovel == null) DetPagImovel = new List<DetPagImovel>(); DetPagImovel.Add(item); }
        /// <summary>Obtém um item da coleção DetPagImovel pelo índice.</summary>
        public DetPagImovel GetDetPagImovel(int index) => DetPagImovel[index];
        /// <summary>Obtém a quantidade de itens da coleção DetPagImovel.</summary>
        public int GetDetPagImovelCount => DetPagImovel?.Count ?? 0;
        /// <summary>Adiciona um item à coleção DetPagIncorpLote.</summary>
        public void AddDetPagIncorpLote(DetPagIncorpLote item) { if (DetPagIncorpLote == null) DetPagIncorpLote = new List<DetPagIncorpLote>(); DetPagIncorpLote.Add(item); }
        /// <summary>Obtém um item da coleção DetPagIncorpLote pelo índice.</summary>
        public DetPagIncorpLote GetDetPagIncorpLote(int index) => DetPagIncorpLote[index];
        /// <summary>Obtém a quantidade de itens da coleção DetPagIncorpLote.</summary>
        public int GetDetPagIncorpLoteCount => DetPagIncorpLote?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo TribTot do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.TribTot")]
    [ComVisible(true)]
#endif
    public class TribTot
    {
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteIndiv da NFeABI.</summary>
        [XmlIgnore]
        public double VRedAjusteIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedAjusteIndiv da NFeABI.</summary>
        [XmlElement("vRedAjusteIndiv")]
        public string VRedAjusteIndivField { get => NFeABIFormat.Money(VRedAjusteIndiv); set => VRedAjusteIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vRedSocialIndiv da NFeABI.</summary>
        [XmlIgnore]
        public double VRedSocialIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vRedSocialIndiv da NFeABI.</summary>
        [XmlElement("vRedSocialIndiv")]
        public string VRedSocialIndivField { get => NFeABIFormat.Money(VRedSocialIndiv); set => VRedSocialIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vBC da NFeABI.</summary>
        [XmlIgnore]
        public double VBC { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vBC da NFeABI.</summary>
        [XmlElement("vBC")]
        public string VBCField { get => NFeABIFormat.Money(VBC); set => VBC = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vIBSUF da NFeABI.</summary>
        [XmlIgnore]
        public double VIBSUF { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vIBSUF da NFeABI.</summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField { get => NFeABIFormat.Money(VIBSUF); set => VIBSUF = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vIBSMun da NFeABI.</summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vIBSMun da NFeABI.</summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField { get => NFeABIFormat.Money(VIBSMun); set => VIBSMun = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vCBS da NFeABI.</summary>
        [XmlIgnore]
        public double VCBS { get; set; }
        /// <summary>Valor total da CBS.</summary>
        [XmlElement("vCBS")]
        public string VCBSField { get => NFeABIFormat.Money(VCBS); set => VCBS = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo Total do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Total")]
    [ComVisible(true)]
#endif
    public class Total
    {
        /// <summary>Obtém ou define o conteúdo da tag vTotalOperacao da NFeABI.</summary>
        [XmlIgnore]
        public double VTotalOperacao { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vTotalOperacao da NFeABI.</summary>
        [XmlElement("vTotalOperacao")]
        public string VTotalOperacaoField { get => NFeABIFormat.Money(VTotalOperacao); set => VTotalOperacao = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag vOperacIndiv da NFeABI.</summary>
        [XmlIgnore]
        public double VOperacIndiv { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vOperacIndiv da NFeABI.</summary>
        [XmlElement("vOperacIndiv")]
        public string VOperacIndivField { get => NFeABIFormat.Money(VOperacIndiv); set => VOperacIndiv = NFeABIFormat.ParseDecimal(value); }
        /// <summary>Obtém ou define o conteúdo da tag tribTot da NFeABI.</summary>
        [XmlElement("tribTot")]
        public TribTot TribTot { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag vNF da NFeABI.</summary>
        [XmlIgnore]
        public double VNF { get; set; }
        /// <summary>Valor total da NFeABI.</summary>
        [XmlElement("vNF")]
        public string VNFField { get => NFeABIFormat.Money(VNF); set => VNF = NFeABIFormat.ParseDecimal(value); }
    }

/// <summary>Representa o grupo Obs do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.Obs")]
    [ComVisible(true)]
#endif
    public class Obs
    {
        /// <summary>Obtém ou define o atributo xCampo da NFeABI.</summary>
        [XmlAttribute("xCampo")]
        public string XCampo { get; set; }
        /// <summary>Texto da observação.</summary>
        [XmlElement("xTexto")]
        public string XTexto { get; set; }
    }

/// <summary>Representa o grupo InfAdic do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfAdic")]
    [ComVisible(true)]
#endif
    public class InfAdic
    {
        /// <summary>Obtém ou define o conteúdo da tag infAdFisco da NFeABI.</summary>
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag infCpl da NFeABI.</summary>
        [XmlElement("infCpl")]
        public string InfCpl { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag obsCont da NFeABI.</summary>
        [XmlElement("obsCont")]
        public List<Obs> ObsCont { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag obsFisco da NFeABI.</summary>
        [XmlElement("obsFisco")]
        public List<Obs> ObsFisco { get; set; }
#if INTEROP
        /// <summary>Adiciona um item à coleção ObsCont.</summary>
        public void AddObsCont(Obs item) { if (ObsCont == null) ObsCont = new List<Obs>(); ObsCont.Add(item); }
        /// <summary>Obtém um item da coleção ObsCont pelo índice.</summary>
        public Obs GetObsCont(int index) => ObsCont[index];
        /// <summary>Obtém a quantidade de itens da coleção ObsCont.</summary>
        public int GetObsContCount => ObsCont?.Count ?? 0;
        /// <summary>Adiciona um item à coleção ObsFisco.</summary>
        public void AddObsFisco(Obs item) { if (ObsFisco == null) ObsFisco = new List<Obs>(); ObsFisco.Add(item); }
        /// <summary>Obtém um item da coleção ObsFisco pelo índice.</summary>
        public Obs GetObsFisco(int index) => ObsFisco[index];
        /// <summary>Obtém a quantidade de itens da coleção ObsFisco.</summary>
        public int GetObsFiscoCount => ObsFisco?.Count ?? 0;
#endif
    }

/// <summary>Representa o grupo InfRespTec do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfRespTec")]
    [ComVisible(true)]
#endif
    public class InfRespTec
    {
        /// <summary>Obtém ou define o conteúdo da tag CNPJ da NFeABI.</summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag xContato da NFeABI.</summary>
        [XmlElement("xContato")]
        public string XContato { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag email da NFeABI.</summary>
        [XmlElement("email")]
        public string Email { get; set; }
        /// <summary>Telefone do responsável técnico.</summary>
        [XmlElement("fone")]
        public string Fone { get; set; }
    }

/// <summary>Representa o grupo InfNFeSupl do contrato XML da NFeABI.</summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFeABI.InfNFeSupl")]
    [ComVisible(true)]
#endif
    public class InfNFeSupl
    {
        /// <summary>Obtém ou define o conteúdo da tag qrCode da NFeABI.</summary>
        [XmlElement("qrCode")]
        public string QrCode { get; set; }
        /// <summary>Obtém ou define o conteúdo da tag urlChave da NFeABI.</summary>
        [XmlElement("urlChave")]
        public string UrlChave { get; set; }
    }
}
