#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif

using System;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using System.Collections.Generic;
using System.Globalization;
using System.Xml;

namespace Unimake.Business.DFe.Xml.NFCom
{
    /// <summary>
    /// NFCom - Portal da Nota Fiscal Fatura de Serviço de Comunicação Eletrônica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.NFCom")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfcom")]
    [XmlRoot("NFCom", Namespace = "http://www.portalfiscal.inf.br/nfcom", IsNullable = false)]
    public class NFCom : XMLBase
    {
        [XmlElement("infNFCom")]
        public InfNFCom InfNFCom { get; set; }

        [XmlElement("infNFComSupl")]
        public InfNFComSupl InfNFComSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfNFCom")]
    [ComVisible(true)]
#endif
    public class InfNFCom
    {
        private string IdField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get
            {
                IdField = "NFCom" + Chave;
                return IdField;
            }
            set => IdField = value;
        }

        private string ChaveField;

        [XmlIgnore]
        public string Chave
        {
            get
            {
                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = (UFBrasil)(int)Ide.CUF,
                    AnoEmissao = Ide.DhEmi.ToString("yy"),
                    MesEmissao = Ide.DhEmi.ToString("MM"),
                    CNPJCPFEmissor = Emit.CNPJ.PadLeft(14, '0'),
                    Modelo = (ModeloDFe)(int)Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NNF,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    NSiteAutoriz = Ide.NSiteAutoriz,
                    CodigoNumerico = Ide.CNF
                };

                ChaveField = XMLUtility.MontarChaveNF3e(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

                return ChaveField;
            }

            set => throw new Exception("Não é permitido atribuir valor para a propriedade Chave. Ela é calculada automaticamente.");
        }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("assinante")]
        public Assinante Assinante { get; set; }

        [XmlElement("gSub")]
        public GSub GSub { get; set; }

        [XmlElement("gCofat")]
        public GCofat GCofat { get; set; }

        [XmlElement("det")]
        public List<Det> Det { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddDet(Det item)
        {
            if (Det == null)
            {
                Det = new List<Det>();
            }

            Det.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista Det (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Det</returns>
        public Det GetDet(int index)
        {
            if ((Det?.Count ?? 0) == 0)
            {
                return default;
            };

            return Det[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Det
        /// </summary>
        public int GetDetCount => (Det != null ? Det.Count : 0);
#endif

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("gFidelidade")]
        public GFidelidade GFidelidade { get; set; }

        [XmlElement("gFat")]
        public GFat GFat { get; set; }

        [XmlElement("gFatCentral")]
        public GFatCentral GFatCentral { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddAutXML(AutXML item)
        {
            if (AutXML == null)
            {
                AutXML = new List<AutXML>();
            }

            AutXML.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista AutXMLNFCom (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AutXMLNFCom</returns>
        public AutXML GetAutXML(int index)
        {
            if ((AutXML?.Count ?? 0) == 0)
            {
                return default;
            };

            return AutXML[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista AutXML
        /// </summary>
        public int GetAutXMLCount => (AutXML != null ? AutXML.Count : 0);
#endif

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("gRespTec")]
        public GRespTec GRespTec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Ide")]
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

        [XmlElement("nNF")]
        public int NNF { get; set; }

        [XmlElement("cNF")]
        public string CNF { get; set; }

        [XmlElement("cDV")]
        public int CDV { get; set; }

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
        public TipoEmissao TpEmis { get; set; }

        [XmlElement("nSiteAutoriz")]
        public string NSiteAutoriz { get; set; }

        [XmlElement("cMunFG")]
        public string CMunFG { get; set; }

        [XmlElement("finNFCom")]
        public FinalidadeNFCom FinNFCom { get; set; }

        [XmlElement("tpFat")]
        public TipoFaturamentoNFCom TpFat { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("indPrePago")]
#if INTEROP
        public IndicadorServicoPrePago IndPrePago { get; set; } = (IndicadorServicoPrePago)(-1);
#else
        public IndicadorServicoPrePago? IndPrePago { get; set; }
#endif

        [XmlElement("indCessaoMeiosRede")]
#if INTEROP
        public IndicadorCessaoMeiosDeRede IndCessaoMeiosRede { get; set; } = (IndicadorCessaoMeiosDeRede)(-1);
#else
        public IndicadorCessaoMeiosDeRede? IndCessaoMeiosRede { get; set; }
#endif

        [XmlElement("indNotaEntrada")]
#if INTEROP
        public IndicadorNotaEntrada IndNotaEntrada { get; set; } = (IndicadorNotaEntrada)(-1);
#else
        public IndicadorNotaEntrada? IndNotaEntrada { get; set; }
#endif

        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset DhCont { get; set; }
#endif

        [XmlElement("dhCont")]
        public string DhContField
        {
            get => DhCont.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhCont = DateTime.Parse(value);
#else
            set => DhCont = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xJust")]
        public string XJust { get; set; }

        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndPrePago() => IndPrePago != (IndicadorServicoPrePago)(-1);
#else
        public bool ShouldSerializeIndPrePago() => IndPrePago != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndCessaoMeiosRede() => IndCessaoMeiosRede != (IndicadorCessaoMeiosDeRede)(-1);
#else
        public bool ShouldSerializeIndCessaoMeiosRede() => IndCessaoMeiosRede != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndNotaEntrada() => IndNotaEntrada != (IndicadorNotaEntrada)(-1);
#else
        public bool ShouldSerializeIndNotaEntrada() => IndNotaEntrada != null;
#endif

        public bool ShouldSerializeDhContField() => TpEmis != (TipoEmissao)1;

        public bool ShouldSerializeXJust() => TpEmis != (TipoEmissao)1;

        #endregion ShouldSerialize
    }
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GCompraGov")]
    [ComVisible(true)]
#endif
    public class GCompraGov
    {
        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamental TpEnteGov { get; set; }

        [XmlIgnore]
        public double PRedutor { get; set; }

        [XmlElement("pRedutor")]
        public string PRedutorField
        {
            get => PRedutor.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedutor = Converter.ToDouble(value);
        }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Emit")]
    [ComVisible(true)]
#endif
    public class Emit
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IEUFDest")]
        public string IEUFDest { get; set; }

        [XmlElement("CRT")]
        public CRT CRT { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIEUFDest() => !string.IsNullOrEmpty(IEUFDest);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.EnderEmit")]
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

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrEmpty(XCpl);

        public bool ShouldSerializeFone() => !string.IsNullOrEmpty(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Dest")]
    [ComVisible(true)]
#endif
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
        public IndicadorIEDestinatario IndIEDest { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IM")]
        public string IM { get; set; }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrEmpty(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrEmpty(CPF);

        public bool ShouldSerializeIdOutros() => !string.IsNullOrEmpty(IdOutros);

        public bool ShouldSerializeIE() => !string.IsNullOrEmpty(IE);

        public bool ShouldSerializeIM() => !string.IsNullOrEmpty(IM);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.EnderDest")]
    [ComVisible(true)]
#endif
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

        [XmlElement("cPais")]
        public string CPais { get; set; }

        [XmlElement("xPais")]
        public string XPais { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrEmpty(XCpl);

        public bool ShouldSerializeFone() => !string.IsNullOrEmpty(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrEmpty(Email);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Assinante")]
    [ComVisible(true)]
#endif
    public class Assinante
    {
        [XmlElement("iCodAssinante")]
        public string ICodAssinante { get; set; }

        [XmlElement("tpAssinante")]
        public TipoAssinante TpAssinante { get; set; }

        [XmlElement("tpServUtil")]
        public TipoServicoUtilizado TpServUtil { get; set; }

        [XmlElement("nContrato")]
        public string NContrato { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DContratoIni { get; set; }
#else
        public DateTimeOffset DContratoIni { get; set; }
#endif

        [XmlElement("dContratoIni")]
        public string DContratoIniField
        {
            get => DContratoIni.ToString("yyyy-MM-dd");
#if INTEROP
            set => DContratoIni = DateTime.Parse(value);
#else
            set => DContratoIni = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DContratoFim { get; set; }
#else
        public DateTimeOffset DContratoFim { get; set; }
#endif

        [XmlElement("dContratoFim")]
        public string DContratoFimField
        {
            get => DContratoFim.ToString("yyyy-MM-dd");
#if INTEROP
            set => DContratoFim = DateTime.Parse(value);
#else
            set => DContratoFim = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("NroTermPrinc")]
        public string NroTermPrinc { get; set; }

        [XmlIgnore]
#if INTEROP
        public UFBrasil CUFPrinc { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? CUFPrinc { get; set; }
#endif

        [XmlElement("cUFPrinc")]
        public int CUFPrincField
        {
            get => (int)CUFPrinc;
            set => CUFPrinc = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        // Lista de objetos combinados
        [XmlIgnore]
        public List<TerminalAdicional> TerminaisAdicionais { get; set; } = new List<TerminalAdicional>();

        // Serializa "na unha" os terminais, intercalando tags
        [XmlAnyElement]
        public XmlElement[] TerminaisAdicionaisRaw
        {
            get
            {
                var doc = new XmlDocument();
                doc.LoadXml("<root xmlns='http://www.portalfiscal.inf.br/nfcom' />");
                var ns = doc.DocumentElement.NamespaceURI;
                var elements = new List<XmlElement>();

                foreach (var t in TerminaisAdicionais)
                {
                    var nroTerm = doc.CreateElement("NroTermAdic", ns);
                    nroTerm.InnerText = t.NroTermAdic;
                    elements.Add(nroTerm);

                    var cufTerm = doc.CreateElement("cUFAdic", ns);
                    cufTerm.InnerText = t.CUFAdicField.ToString();
                    elements.Add(cufTerm);
                }

                return elements.ToArray();
            }
            set
            {
                TerminaisAdicionais.Clear();
                if (value == null || value.Length == 0) return;

                for (int i = 0; i < value.Length - 1; i++)
                {
                    if (value[i].Name == "NroTermAdic" && value[i + 1].Name == "cUFAdic")
                    {
                        TerminaisAdicionais.Add(new TerminalAdicional
                        {
                            NroTermAdic = value[i].InnerText,
                            CUFAdic = (UFBrasil)int.Parse(value[i + 1].InnerText)
                        });
                        i++;
                    }
                }
            }
        }

#if INTEROP
        public void AddNroTermAdic(string item) => TerminaisAdicionais.Add(new TerminalAdicional { NroTermAdic = item });
        public string GetNroTermAdic(int index) => (index >= 0 && index < TerminaisAdicionais.Count) ? TerminaisAdicionais[index].NroTermAdic : default;
        public int GetNroTermAdicCount => TerminaisAdicionais.Count;

        public void AddCUFAdic(UFBrasil item) => TerminaisAdicionais.Add(new TerminalAdicional { CUFAdic = item });
        public int GetCUFAdic(int index) => (index >= 0 && index < TerminaisAdicionais.Count) ? (int)TerminaisAdicionais[index].CUFAdic : default;
        public int GetCUFAdicCount => TerminaisAdicionais.Count;
#endif

        #region ShouldSerialize
        public bool ShouldSerializeNContrato() => !string.IsNullOrEmpty(NContrato);
        public bool ShouldSerializeDContratoIniField() => DContratoIni != default;
        public bool ShouldSerializeDContratoFimField() => DContratoFim != default;
        public bool ShouldSerializeNroTermPrinc() => !string.IsNullOrEmpty(NroTermPrinc);
        public bool ShouldSerializeTerminaisAdicionaisRaw() => TerminaisAdicionais?.Count > 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.TerminalAdicional")]
    [ComVisible(true)]
#endif
    public class TerminalAdicional
    {
        public string NroTermAdic { get; set; }

        [XmlIgnore]
#if INTEROP
        public UFBrasil CUFAdic { get; set; } = (UFBrasil)(-1);
#else
        public UFBrasil? CUFAdic { get; set; }
#endif
        [XmlElement("cUFAdic")]
        public int CUFAdicField
        {
            get => (int)CUFAdic;
            set => CUFAdic = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GSub")]
    [ComVisible(true)]
#endif
    public class GSub
    {
        [XmlElement("chNFCom")]
        public string ChNFCom { get; set; }

        [XmlElement("gNF")]
        public GNF GNF { get; set; }

        [XmlElement("motSub")]
        public MotivoSubstituicaoNFCom MotSub { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GNF")]
    [ComVisible(true)]
#endif
    public class GNF
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("mod")]
        public string Mod { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nNF")]
        public string NNF { get; set; }

        [XmlElement("competEmis")]
        public string CompetEmis { get; set; }

        [XmlElement("hash115")]
        public string Hash115 { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeHash115() => !string.IsNullOrEmpty(Hash115);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GCofat")]
    [ComVisible(true)]
#endif
    public class GCofat
    {
        [XmlElement("chNFComLocal")]
        public string ChNFComLocal { get; set; }

        [XmlElement("gNF")]
        public GNF GNF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Det")]
    [ComVisible(true)]
#endif
    public class Det
    {
        [XmlAttribute(AttributeName = "nItem", DataType = "token")]
        public string NItem { get; set; }

        [XmlAttribute(AttributeName = "chNFComAnt", DataType = "string")]
        public string ChNFComAnt { get; set; }

        [XmlAttribute(AttributeName = "nItemAnt", DataType = "token")]
        public string NItemAnt { get; set; }

        [XmlAttribute(AttributeName = "indNFComAntPapelFatCentral", DataType = "token")]
        public string IndNFComAntPapelFatCentral { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        [XmlElement("gProcRef")]
        public GProcRef GProcRef { get; set; }

        [XmlElement("gRessarc")]
        public GRessarc GRessarc { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeChNFComAnt() => !string.IsNullOrEmpty(ChNFComAnt);

        public bool ShouldSerializeNItemAnt() => !string.IsNullOrEmpty(NItemAnt);

        public bool ShouldSerializeIndNFComAntPapelFatCentral() => !string.IsNullOrEmpty(IndNFComAntPapelFatCentral);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Prod")]
    [ComVisible(true)]
#endif
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

        [XmlElement("CNPJLD")]
        public string CNPJLD { get; set; }

        [XmlElement("uMed")]
        public UnidadeBasicaMedida UMed { get; set; }

        [XmlIgnore]
        public double QFaturada { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturadaField
        {
            get => QFaturada.ToString("F4", CultureInfo.InvariantCulture);
            set => QFaturada = Converter.ToDouble(value);
        }

        [XmlElement("vItem")]
        public decimal VItem { get; set; }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        [XmlElement("vProd")]
        public decimal VProd { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DExpiracao { get; set; }
#else
        public DateTimeOffset DExpiracao { get; set; }
#endif

        [XmlElement("dExpiracao")]
        public string DExpiracaoField
        {
            get => DExpiracao.ToString("yyyy-MM-dd");
#if INTEROP
            set => DExpiracao = DateTime.Parse(value);
#else
            set => DExpiracao = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indDevolucao")]
#if INTEROP
        public IndicadorDevolucao IndDevolucao { get; set; } = (IndicadorDevolucao)(-1);
#else
        public IndicadorDevolucao? IndDevolucao { get; set; }

#endif

        #region ShouldSerialize

        public bool ShouldSerializeCFOP() => !string.IsNullOrEmpty(CFOP);

        public bool ShouldSerializeCNPJLD() => !string.IsNullOrEmpty(CNPJLD);

        public bool ShouldSerializeVDescField() => VDesc > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeDExpiracaoField() => DExpiracao > DateTime.MinValue;

#if INTEROP
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != (IndicadorDevolucao)(-1);
#else
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Imposto")]
    [ComVisible(true)]
#endif
    public class Imposto
    {
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        [XmlElement("ICMS40")]
        public ICMS40 ICMS40 { get; set; }

        [XmlElement("ICMS51")]
        public ICMS51 ICMS51 { get; set; }

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("ICMSSN")]
        public ICMSSN ICMSSN { get; set; }

        [XmlElement("ICMSUFDest")]
        public List<ICMSUFDest> ICMSUFDest { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddICMSUFDest(ICMSUFDest item)
        {
            if (ICMSUFDest == null)
            {
                ICMSUFDest = new List<ICMSUFDest>();
            }

            ICMSUFDest.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista ICMSUFDest (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ICMSUFDest</returns>
        public ICMSUFDest GetICMSUFDest(int index)
        {
            if ((ICMSUFDest?.Count ?? 0) == 0)
            {
                return default;
            };

            return ICMSUFDest[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ICMSUFDest
        /// </summary>
        public int GetICMSUFDestCount => (ICMSUFDest != null ? ICMSUFDest.Count : 0);
#endif

        [XmlElement("indSemCST")]
#if INTEROP
        public SimNao IndSemCST { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndSemCST { get; set; }
#endif

        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        [XmlElement("FUST")]
        public FUST FUST { get; set; }

        [XmlElement("FUNTTEL")]
        public FUNTTEL FUNTTEL { get; set; }

        [XmlElement("retTrib")]
        public RetTrib RetTrib { get; set; }

        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeIndSemCST() => IndSemCST != (SimNao)(-1);
#else
        public bool ShouldSerializeIndSemCST() => IndSemCST != null;
#endif

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMS00")]
    [ComVisible(true)]
#endif
    public class ICMS00 
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePFCPField() => PFCP > 0;
        public bool ShouldSerializeVFCPField() => VFCP > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMS20")]
    [ComVisible(true)]
#endif
    public class ICMS20
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeCBenef() => !string.IsNullOrEmpty(CBenef);
        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMS40")]
    [ComVisible(true)]
#endif
    public class ICMS40
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeCBenef() => !string.IsNullOrEmpty(CBenef);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMS51")]
    [ComVisible(true)]
#endif
    public class ICMS51 : ICMS40 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMS90")]
    [ComVisible(true)]
#endif
    public class ICMS90
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        [XmlIgnore]
        public double PFCP { get; set; }

        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializePICMSField() => PICMS > 0;

        public bool ShouldSerializeVICMSField() => VICMS > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeCBenef() => !string.IsNullOrEmpty(CBenef);

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMSSN")]
    [ComVisible(true)]
#endif
    public class ICMSSN
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("indSN")]
        public SimNao IndSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMSUFDest")]
    [ComVisible(true)]
#endif
    public class ICMSUFDest
    {
        [XmlIgnore]
        public UFBrasil CUFDest { get; set; }

        [XmlAttribute(AttributeName = "cUFDest", DataType = "int")]
        public int CUFDestField
        {
            get => (int)CUFDest;
            set => CUFDest = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlIgnore]
        public double VBCUFDest { get; set; }

        [XmlElement("vBCUFDest")]
        public string VBCUFDestField
        {
            get => VBCUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPUFDest { get; set; }

        [XmlElement("pFCPUFDest")]
        public string PFCPUFDestField
        {
            get => PFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => PFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSUFDest { get; set; }

        [XmlElement("pICMSUFDest")]
        public string PICMSUFDestField
        {
            get => PICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPUFDest { get; set; }

        [XmlElement("vFCPUFDest")]
        public string VFCPUFDestField
        {
            get => VFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFDest { get; set; }

        [XmlElement("vICMSUFDest")]
        public string VICMSUFDestField
        {
            get => VICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFDest = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFEmi { get; set; }

        [XmlElement("vICMSUFEmi")]
        public string VICMSUFEmiField
        {
            get => VICMSUFEmi.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFEmi = Converter.ToDouble(value);
        }

        [XmlElement("cBenefUFDest")]
        public string CBenefUFDest { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCBenefUFDest() => !string.IsNullOrEmpty(CBenefUFDest);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.PIS")]
    [ComVisible(true)]
#endif
    public class PIS
    {
        [XmlElement("CST")]
        public CSTPisCofins CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PPIS { get; set; }

        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.COFINS")]
    [ComVisible(true)]
#endif
    public class COFINS
    {
        [XmlElement("CST")]
        public CSTPisCofins CST { get; set; }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PCOFINS { get; set; }

        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.FUST")]
    [ComVisible(true)]
#endif
    public class FUST
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFUST { get; set; }

        [XmlElement("pFUST")]
        public string PFUSTField
        {
            get => PFUST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFUST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFUST { get; set; }

        [XmlElement("vFUST")]
        public string VFUSTField
        {
            get => VFUST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFUST = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.FUNTTEL")]
    [ComVisible(true)]
#endif
    public class FUNTTEL
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFUNTTEL { get; set; }

        [XmlElement("pFUNTTEL")]
        public string PFUNTTELField
        {
            get => PFUNTTEL.ToString("F4", CultureInfo.InvariantCulture);
            set => PFUNTTEL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFUNTTEL { get; set; }

        [XmlElement("vFUNTTEL")]
        public string VFUNTTELField
        {
            get => VFUNTTEL.ToString("F2", CultureInfo.InvariantCulture);
            set => VFUNTTEL = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.RetTrib")]
    [ComVisible(true)]
#endif
    public class RetTrib
    {
        [XmlElement("vRetPIS")]
        public decimal VRetPIS { get; set; }

        [XmlElement("vRetCofins")]
        public decimal VRetCofins { get; set; }

        [XmlElement("vRetCSLL")]
        public decimal VRetCSLL { get; set; }

        [XmlElement("vBCIRRF")]
        public decimal VBCIRRF { get; set; }

        [XmlElement("vIRRF")]
        public decimal VIRRF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.IBSCBS")]
    [ComVisible(true)]
#endif
    public class IBSCBS
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSCBS")]
    [ComVisible(true)]
#endif
    public class GIBSCBS
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }

        /// <summary>
        /// Valor do IBS (soma de vIBSUF e vIBSMun)
        /// </summary>
        [XmlIgnore]
        public double VIBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => VIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBS = Converter.ToDouble(value);
        }

        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        [XmlElement("gIBSCredPres")]
        public GIBSCredPres GIBSCredPres { get; set; }

        [XmlElement("gCBSCredPres")]
        public GCBSCredPres GCBSCredPres { get; set; }

        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSUF")]
    [ComVisible(true)]
#endif
    public class GIBSUF
    {
        [XmlIgnore]
        public double PIBSUF { get; set; }

        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        [XmlElement("gDif")]
        public GDif GDif { get; set;}

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GDif")]
    [ComVisible(true)]
#endif
    public class GDif
    {
        [XmlIgnore]
        public double PDif { get; set; }

        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GDevTrib")]
    [ComVisible(true)]
#endif
    public class GDevTrib
    {
        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GRed")]
    [ComVisible(true)]
#endif
    public class GRed
    {
        [XmlIgnore]
        public double PRedAliq { get; set; }

        [XmlElement("pRedAliq")]
        public string PRedAliqField
        {
            get => PRedAliq.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedAliq = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfet { get; set; }

        [XmlElement("pAliqEfet")]
        public string PAliqEfetField
        {
            get => PAliqEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfet = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSMun")]
    [ComVisible(true)]
#endif
    public class GIBSMun
    {
        [XmlIgnore]
        public double PIBSMun { get; set; }

        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F2", CultureInfo.InvariantCulture);
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
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GCBS")]
    [ComVisible(true)]
#endif
    public class GCBS
    {
        [XmlIgnore]
        public double PCBS { get; set; }

        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => PCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        [XmlIgnore]
        public double VCBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GTribRegular")]
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
            get => PAliqEfetRegIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VtribRegIBSUF { get; set; }

        [XmlElement("vTribRegIBSUF")]
        public string VtribRegIBSUFField
        {
            get => VtribRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VtribRegIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetRegIBSMun { get; set; }

        [XmlElement("pAliqEfetRegIBSMun")]
        public string PAliqEfetRegIBSMunField
        {
            get => PAliqEfetRegIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VtribRegIBSMun { get; set; }

        [XmlElement("vTribRegIBSMun")]
        public string VtribRegIBSMunField
        {
            get => VtribRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VtribRegIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqEfetRegCBS { get; set; }

        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBSField
        {
            get => PAliqEfetRegCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => VTribRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegCBS = Converter.ToDouble(value);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSCredPres")]
    [ComVisible(true)]
#endif
    public class GIBSCredPres
    {
        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        [XmlIgnore]
        public double PCredPres { get; set; }

        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeVCredPresField() => VCredPresCondSus <= 0;
        public bool ShouldSerializeVCredPresCondSusField() => VCredPres <= 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GCBSCredPres")]
    [ComVisible(true)]
#endif
    public class GCBSCredPres
    {
        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        [XmlIgnore]
        public double PCredPres { get; set; }

        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }

        #region Should Serialize
        public bool ShouldSerializeVCredPresField() => VCredPresCondSus <= 0;
        public bool ShouldSerializeVCredPresCondSusField() => VCredPres <= 0;
        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GTribCompraGov")]
    [ComVisible(true)]
#endif
    public class GTribCompraGov
    {
        [XmlIgnore]
        public double PAliqIBSUF { get; set; }

        [XmlElement("pAliqIBSUF")]
        public string PAliqIBSUFField
        {
            get => PAliqIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribIBSUF { get; set; }

        [XmlElement("vTribIBSUF")]
        public string VTribIBSUFField
        {
            get => VTribIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribIBSUF = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqIBSMun { get; set; }

        [XmlElement("pAliqIBSMun")]
        public string PAliqIBSMunField
        {
            get => PAliqIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribIBSMun { get; set; }

        [XmlElement("vTribIBSMun")]
        public string VTribIBSMunField
        {
            get => VTribIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribIBSMun = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PAliqCBS { get; set; }

        [XmlElement("pAliqCBS")]
        public string PAliqCBSField
        {
            get => PAliqCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VTribCBS { get; set; }

        [XmlElement("vTribCBS")]
        public string VTribCBSField
        {
            get => VTribCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribCBS = Converter.ToDouble(value);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GProcRef")]
    [ComVisible(true)]
#endif
    public class GProcRef
    {
        [XmlElement("vItem")]
        public decimal VItem { get; set; }

        [XmlIgnore]
        public double QFaturada { get; set; }

        [XmlElement("qFaturada")]
        public string QFaturadaField
        {
            get => QFaturada.ToString("F4", CultureInfo.InvariantCulture);
            set => QFaturada = Converter.ToDouble(value);
        }

        [XmlElement("vProd")]
        public decimal VProd { get; set; }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        [XmlElement("indDevolucao")]
#if INTEROP
        public IndicadorDevolucao IndDevolucao { get; set; } = (IndicadorDevolucao)(-1);
#else
        public IndicadorDevolucao? IndDevolucao { get; set; }

#endif

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        [XmlElement("gProc")]
        public List<GProc> GProc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGProc(GProc item)
        {
            if (GProc == null)
            {
                GProc = new List<GProc>();
            }

            GProc.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GProc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GProc</returns>
        public GProc GetGProc(int index)
        {
            if ((GProc?.Count ?? 0) == 0)
            {
                return default;
            };

            return GProc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GProc
        /// </summary>
        public int GetGProcCount => (GProc != null ? GProc.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVDescField() => VDesc > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

#if INTEROP
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != (IndicadorDevolucao)(-1);
#else
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != null;
#endif

        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializePICMSField() => PICMS > 0;

        public bool ShouldSerializeVICMSField() => VICMS > 0;

        public bool ShouldSerializeVPISField() => VPIS > 0;

        public bool ShouldSerializeVCOFINSField() => VCOFINS > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GProc")]
    [ComVisible(true)]
#endif
    public class GProc
    {
        [XmlElement("tpProc")]
        public TipoProcessoNF3eNFCom TpProc { get; set; }

        [XmlElement("nProcesso")]
        public string NProcesso { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GRessarc")]
    [ComVisible(true)]
#endif
    public class GRessarc
    {
        [XmlElement("tpRessarc")]
        public TipoRessarcimento TpRessarc { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DRef { get; set; }
#else
        public DateTimeOffset DRef { get; set; }
#endif

        [XmlElement("dRef")]
        public string DRefField
        {
            get => DRef.ToString("yyyy-MM-dd");
#if INTEROP
            set => DRef = DateTime.Parse(value);
#else
            set => DRef = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("nProcesso")]
        public string NProcesso { get; set; }

        [XmlElement("nProtReclama")]
        public string NProtReclama { get; set; }

        [XmlElement("xObs")]
        public string XObs { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNProcesso() => !string.IsNullOrEmpty(NProcesso);

        public bool ShouldSerializeNProtReclama() => !string.IsNullOrEmpty(NProtReclama);

        public bool ShouldSerializeXObs() => !string.IsNullOrEmpty(XObs);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.Total")]
    [ComVisible(true)]
#endif
    public class Total
    {
        [XmlIgnore]
        public double VProd { get; set; }

        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }

        [XmlElement("ICMSTot")]
        public ICMSTot ICMSTot { get; set; }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFUNTTEL { get; set; }

        [XmlElement("vFUNTTEL")]
        public string VFUNTTELField
        {
            get => VFUNTTEL.ToString("F2", CultureInfo.InvariantCulture);
            set => VFUNTTEL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFUST { get; set; }

        [XmlElement("vFUST")]
        public string VFUSTField
        {
            get => VFUST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFUST = Converter.ToDouble(value);
        }

        [XmlElement("vRetTribTot")]
        public VRetTribTot VRetTribTot { get; set; }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VOutro { get; set; }

        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VNF { get; set; }

        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        [XmlElement("IBSCBSTot")]
        public IBSCBSTot IBSCBSTot { get; set; }

        [XmlIgnore]
        public double VTotDFe { get; set; }

        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => VTotDFe.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDFe = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.ICMSTot")]
    [ComVisible(true)]
#endif
    public class ICMSTot
    {
        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSDeson { get; set; }

        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.VRetTribTot")]
    [ComVisible(true)]
#endif
    public class VRetTribTot
    {
        [XmlIgnore]
        public double VRetPIS { get; set; }

        [XmlElement("vRetPIS")]
        public string VRetPISField
        {
            get => VRetPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPIS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCofins { get; set; }

        [XmlElement("vRetCofins")]
        public string VRetCofinsField
        {
            get => VRetCofins.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCofins = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRetCSLL { get; set; }

        [XmlElement("vRetCSLL")]
        public string VRetCSLLField
        {
            get => VRetCSLL.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCSLL = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIRRF { get; set; }

        [XmlElement("vIRRF")]
        public string VIRRFField
        {
            get => VIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIRRF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.IBSCBSTot")]
    [ComVisible(true)]
#endif

    public class IBSCBSTot
    {
        [XmlIgnore]
        public double VBCIBSCBS { get; set; }

        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBSField
        {
            get => VBCIBSCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIBSCBS = Converter.ToDouble(value);
        }

        [XmlElement("gIBS")]
        public GIBSTot GIBSTot { get; set; }

        [XmlElement("gCBS")]
        public GCBSTot GCBSTot { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSTot")]
    [ComVisible(true)]
#endif
    public class GIBSTot
    {
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUFTot { get; set; }

        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMunTot { get; set; }

        [XmlIgnore]
        public double VIBS { get; set; }

        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => VIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSUFTot")]
    [ComVisible(true)]
#endif
    public class GIBSUFTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSUF { get; set; }

        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GIBSMunTot")]
    [ComVisible(true)]
#endif
    public class GIBSMunTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VIBSMun { get; set; }

        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GCBSTot")]
    [ComVisible(true)]
#endif
    public class  GCBSTot
    {
        [XmlIgnore]
        public double VDif { get; set; }

        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDevTrib { get; set; }

        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPres { get; set; }

        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GFidelidade")]
    [ComVisible(true)]
#endif
    public class GFidelidade
    {
        [XmlElement("qtdSaldoPts")]
        public string QtdSaldoPts { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DRefSaldoPts { get; set; }
#else
        public DateTimeOffset DRefSaldoPts { get; set; }
#endif

        [XmlElement("dRefSaldoPts")]
        public string DRefSaldoPtsField
        {
            get => DRefSaldoPts.ToString("yyyy-MM-dd");
#if INTEROP
            set => DRefSaldoPts = DateTime.Parse(value);
#else
            set => DRefSaldoPts = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("qtdPtsResg")]
        public string QtdPtsResg { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DRefResgPts { get; set; }
#else
        public DateTimeOffset DRefResgPts { get; set; }
#endif

        [XmlElement("dRefResgPts")]
        public string DRefResgPtsField
        {
            get => DRefResgPts.ToString("yyyy-MM-dd");
#if INTEROP
            set => DRefResgPts = DateTime.Parse(value);
#else
            set => DRefResgPts = DateTimeOffset.Parse(value);
#endif
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GFat")]
    [ComVisible(true)]
#endif
    public class GFat
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DVencFat { get; set; }
#else
        public DateTimeOffset DVencFat { get; set; }
#endif

        [XmlElement("dVencFat")]
        public string DVencFatField
        {
            get => DVencFat.ToString("yyyy-MM-dd");
#if INTEROP
            set => DVencFat = DateTime.Parse(value);
#else
            set => DVencFat = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DPerUsoIni { get; set; }
#else
        public DateTimeOffset DPerUsoIni { get; set; }
#endif

        [XmlElement("dPerUsoIni")]
        public string DPerUsoIniField
        {
            get => DPerUsoIni.ToString("yyyy-MM-dd");
#if INTEROP
            set => DPerUsoIni = DateTime.Parse(value);
#else
            set => DPerUsoIni = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DPerUsoFim { get; set; }
#else
        public DateTimeOffset DPerUsoFim { get; set; }
#endif

        [XmlElement("dPerUsoFim")]
        public string DPerUsoFimField
        {
            get => DPerUsoFim.ToString("yyyy-MM-dd");
#if INTEROP
            set => DPerUsoFim = DateTime.Parse(value);
#else
            set => DPerUsoFim = DateTimeOffset.Parse(value);
#endif
        }

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

        #region ShouldSerialize

        public bool ShouldSerializeDPerUsoIniField() => DPerUsoIni > DateTime.MinValue;

        public bool ShouldSerializeDPerUsoFimField() => DPerUsoFim > DateTime.MinValue;

        public bool ShouldSerializeCodDebAuto() => !string.IsNullOrEmpty(CodDebAuto);

        public bool ShouldSerializeCodBanco() => !string.IsNullOrEmpty(CodBanco);

        public bool ShouldSerializeCodAgencia() => !string.IsNullOrEmpty(CodAgencia);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.EnderCorresp")]
    [ComVisible(true)]
#endif
    public class EnderCorresp : EnderEmit { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GPix")]
    [ComVisible(true)]
#endif
    public class GPix
    {
        [XmlElement("urlQRCodePIX")]
        public string UrlQRCodePIX { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GFatCentral")]
    [ComVisible(true)]
#endif
    public class GFatCentral
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.AutXML")]
    [ComVisible(true)]
#endif
    public class AutXML
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfAdic")]
    [ComVisible(true)]
#endif
    public class InfAdic
    {
        private string InfAdFiscoField;

        [XmlElement("infAdFisco")]
        public string InfAdFisco
        {
            get => InfAdFiscoField;
            set => InfAdFiscoField = value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(2000).Trim();
        }

        [XmlElement("infCpl")]
        public List<string> InfCpl { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddInfCpl(string item)
        {
            if (InfCpl == null)
            {
                InfCpl = new List<string>();
            }

            InfCpl.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCpl (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCpl</returns>
        public string GetInfCpl(int index)
        {
            if ((InfCpl?.Count ?? 0) == 0)
            {
                return default;
            }
            ;

            return InfCpl[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCpl
        /// </summary>
        public int GetInfCplCount => (InfCpl != null ? InfCpl.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.GRespTec")]
    [ComVisible(true)]
#endif
    public class GRespTec
    {
        private string HashCSRTField;

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

        /// <summary>
        /// Você pode informar o conteúdo já convertido para Sha1Hash + Base64, ou pode informar somente a concatenação do CSRT + Chave de Acesso que a DLL já converte para Sha1Hash + Base64
        /// </summary>
        [XmlElement("hashCSRT")]
        public string HashCSRT
        {

            get
            {
                if (string.IsNullOrWhiteSpace(HashCSRTField) || Converter.IsSHA1Base64(HashCSRTField))
                {
                    return HashCSRTField;
                }
                else
                {
                    return Converter.CalculateSHA1Hash(HashCSRTField);
                }
            }
            set => HashCSRTField = value;
        }

        /// <summary>
        /// Esta propriedade deve ser utilizada para informar o CSRT sem o hast, informando ela a DLL irá gerar o conteúdo da tag hashCSRT automaticamente
        /// </summary>
        [XmlIgnore]
        public string CSRT { get; set; }

        /// <summary>
        /// Gerar o conteúdo da tag HashCSRT
        /// </summary>
        /// <param name="chaveAcesso"></param>
        public void GerarHashCSRT(string chaveAcesso)
        {
            if (string.IsNullOrWhiteSpace(CSRT))
            {
                return;
            }

            if (!Converter.IsSHA1Base64(HashCSRT))
            {
                HashCSRT = Converter.CalculateSHA1Hash(CSRT + chaveAcesso);
            }
        }

        #region ShouldSerialize

        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);

        public bool ShouldSerializeHashCSRT() => !string.IsNullOrWhiteSpace(HashCSRT);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFCom.InfNFComSupl")]
    [ComVisible(true)]
#endif
    public class InfNFComSupl
    {
        [XmlElement("qrCodNFCom")]
        public string QrCodNFCom { get; set; }
    }
}
