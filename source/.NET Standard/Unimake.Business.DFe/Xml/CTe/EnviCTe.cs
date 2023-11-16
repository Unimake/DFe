#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnviCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("enviCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class EnviCTe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("idLote")]
        public string IdLote { get; set; }

        [XmlElement("CTe")]
        public List<CTe> CTe { get; set; }

        public override XmlDocument GerarXML()
        {
            var xmlDoc = base.GerarXML();

            foreach (var nodeEnvCTe in xmlDoc.GetElementsByTagName("enviCTe"))
            {
                var elemEnvCTe = (XmlElement)nodeEnvCTe;

                foreach (var nodeCTe in elemEnvCTe.GetElementsByTagName("CTe"))
                {
                    var elemCTe = (XmlElement)nodeCTe;

                    var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
                    elemCTe.SetAttribute("xmlns", attribute.Namespace);
                }
            }

            return xmlDoc;
        }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="cte">Elemento</param>
        public void AddCTe(CTe cte)
        {
            if (CTe == null)
            {
                CTe = new List<CTe>();
            }

            CTe.Add(cte);
        }

        /// <summary>
        /// Retorna o elemento da lista CTe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CTe</returns>
        public CTe GetCTe(int index)
        {
            if ((CTe?.Count ?? 0) == 0)
            {
                return default;
            };

            return CTe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CTe
        /// </summary>
        public int GetCTeCount => (CTe != null ? CTe.Count : 0);

#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.CTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    [XmlRoot("CTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class CTe : XMLBase
    {
        [XmlElement("infCte")]
        public InfCTe InfCTe { get; set; }

        [XmlElement("infCTeSupl")]
        public InfCTeSupl InfCTeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>
        /// Desserializar o XML no objeto CTe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto do CTe</returns>
        public CTe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CTe>(doc);
        }

        /// <summary>
        /// Deserializar o XML CTe no objeto CTe
        /// </summary>
        /// <param name="xml">string do XML CTe</param>
        /// <returns>Objeto da CTe</returns>
        public CTe LoadFromXML(string xml) => XMLUtility.Deserializar<CTe>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTe
    {
        private string IdField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("compl")]
        public Compl Compl { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("rem")]
        public Rem Rem { get; set; }

        [XmlElement("exped")]
        public Exped Exped { get; set; }

        [XmlElement("receb")]
        public Receb Receb { get; set; }

        [XmlElement("dest")]
        public Dest Dest { get; set; }

        [XmlElement("vPrest")]
        public VPrest VPrest { get; set; }

        [XmlElement("imp")]
        public Imp Imp { get; set; }

        [XmlElement("infCTeNorm")]
        public InfCTeNorm InfCTeNorm { get; set; }

        [XmlElement("infCteComp")]
        public InfCteComp InfCteComp { get; set; }

        [XmlElement("infCteAnu")]
        public InfCteAnu InfCteAnu { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        [XmlElement("infSolicNFF")]
        public InfSolicNFF InfSolicNFF { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id
        {
            get
            {
                IdField = "CTe" + Chave;
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
                ChaveField = ((int)Ide.CUF).ToString() +
                    Ide.DhEmi.ToString("yyMM") +
                    (string.IsNullOrWhiteSpace(Emit.CNPJ) ? Emit.CPF?.PadLeft(14, '0') : Emit.CNPJ.PadLeft(14, '0')) +
                    ((int)Ide.Mod).ToString().PadLeft(2, '0') +
                    Ide.Serie.ToString().PadLeft(3, '0') +
                    Ide.NCT.ToString().PadLeft(9, '0') +
                    ((int)Ide.TpEmis).ToString() +
                    Ide.CCT.PadLeft(8, '0');

                Ide.CDV = Utility.XMLUtility.CalcularDVChave(ChaveField);

                ChaveField += Ide.CDV.ToString();

                return ChaveField;
            }
            set => throw new Exception("Não é permitido atribuir valor para a propriedade Chave. Ela é calculada automaticamente.");
        }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="autxml">Elemento</param>
        public void AddAutXML(AutXML autxml)
        {
            if (AutXML == null)
            {
                AutXML = new List<AutXML>();
            }

            AutXML.Add(autxml);
        }

        /// <summary>
        /// Retorna o elemento da lista AutXML (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AutXML</returns>
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

        #endregion

        #region ShouldSerialize

        public bool ShouldSerializeInfCteAnu() => Convert.ToDecimal(Versao) <= 300;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Ide")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Ide
    {
        private string CCTField;
        private TipoEmissao TpEmisField;
        private ProcessoEmissao ProcEmiField;

        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        [XmlElement("cCT")]
        public string CCT
        {
            get
            {
                string retorno;
                if (string.IsNullOrWhiteSpace(CCTField))
                {
                    if (NCT == 0)
                    {
                        throw new Exception("Defina o conteúdo da TAG <nCT>, pois a mesma é utilizada como base para calcular o código numérico.");
                    }

                    retorno = Utility.XMLUtility.GerarCodigoNumerico(NCT).ToString("00000000");
                }
                else
                {
                    retorno = CCTField;
                }

                return retorno;
            }
            set => CCTField = value;
        }

        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        [XmlElement("natOp")]
        public string NatOp { get; set; }

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nCT")]
        public int NCT { get; set; }

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

        [XmlElement("tpImp")]
        public FormatoImpressaoDACTE TpImp { get; set; }

        [XmlElement("tpEmis")]
        public TipoEmissao TpEmis
        {
            get => TpEmisField;
            set
            {
                if (value == TipoEmissao.ContingenciaFSIA ||
                   value == TipoEmissao.ContingenciaOffLine ||
                   value == TipoEmissao.RegimeEspecialNFF ||
                   value == TipoEmissao.ContingenciaSVCAN)
                {
                    throw new Exception("Conteúdo da TAG <tpEmis> inválido! Valores aceitos: 1, 4, 5, 7 ou 8.");
                }

                TpEmisField = value;
            }
        }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("tpCTe")]
        public TipoCTe TpCTe { get; set; }

        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi
        {
            get => ProcEmiField;
            set
            {
                if (value == ProcessoEmissao.AvulsaPeloContribuinteSiteFisco ||
                    value == ProcessoEmissao.AvulsaPeloFisco)
                {
                    throw new Exception("Conteúdo da TAG <procEmi> inválido! Valores aceitos: 0 e 3.");
                }

                ProcEmiField = value;
            }
        }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("indGlobalizado")]
        public SimNao IndGlobalizado { get; set; }

        [XmlElement("cMunEnv")]
        public string CMunEnv { get; set; }

        [XmlElement("xMunEnv")]
        public string XMunEnv { get; set; }

        [XmlElement("UFEnv")]
        public UFBrasil UFEnv { get; set; }

        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal { get; set; }

        [XmlElement("tpServ")]
        public TipoServicoCTe TpServ { get; set; }

        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        [XmlElement("xMunIni")]
        public string XMunIni { get; set; }

        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        [XmlElement("xMunFim")]
        public string XMunFim { get; set; }

        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; }

        [XmlElement("retira")]
        public string RetiraField { get; set; }

        [XmlIgnore]
        public SimNao Retira
        {
            get => (RetiraField.Equals("0") ? SimNao.Sim : SimNao.Nao);
            set => RetiraField = (value == SimNao.Sim ? "0" : "1");
        }

        [XmlElement("xDetRetira")]
        public string XDetRetira { get; set; }

        [XmlElement("indIEToma")]
        public IndicadorIEDestinatario IndIEToma { get; set; }

        [XmlElement("toma3")]
        public Toma3 Toma3 { get; set; }

        [XmlElement("toma4")]
        public Toma4 Toma4 { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeIndGlobalizado() => IndGlobalizado == SimNao.Sim;

        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Toma3")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Toma3
    {
        private TomadorServicoCTe TomaField;

        [XmlElement("toma")]
        public TomadorServicoCTe Toma
        {
            get => TomaField;
            set
            {
                if (value == TomadorServicoCTe.Outros)
                {
                    throw new Exception("Conteúdo da TAG <toma>, filha da TAG <toma3>, inválido! Valores aceitos: 0, 1, 2, 3.");
                }

                TomaField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Toma4")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Toma4
    {
        //private TomadorServicoCTe TomaField;

        [XmlElement("toma")]
        public TomadorServicoCTe Toma
        {
            get => TomadorServicoCTe.Outros;
            set
            {
                if (value != TomadorServicoCTe.Outros)
                {
                    throw new Exception("Conteúdo da TAG <toma>, filha da TAG <toma4>, inválido! Valores aceitos: 4.");
                }

                //TomaField = value;
            }
        }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("enderToma")]
        public EnderToma EnderToma { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderToma")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderToma
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Compl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Compl
    {
        [XmlElement("xCaracAd")]
        public string XCaracAd { get; set; }

        [XmlElement("xCaracSer")]
        public string XCaracSer { get; set; }

        [XmlElement("xEmi")]
        public string XEmi { get; set; }

        [XmlElement("fluxo")]
        public Fluxo Fluxo { get; set; }

        [XmlElement("Entrega")]
        public Entrega Entrega { get; set; }

        [XmlElement("origCalc")]
        public string OrigCalc { get; set; }

        [XmlElement("destCalc")]
        public string DestCalc { get; set; }

        [XmlElement("xObs")]
        public string XObs { get; set; }

        [XmlElement("ObsCont")]
        public List<ObsCont> ObsCont { get; set; }

        [XmlElement("ObsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsCont">Elemento</param>
        public void AddObsCont(ObsCont obsCont)
        {
            if (ObsCont == null)
            {
                ObsCont = new List<ObsCont>();
            }

            ObsCont.Add(obsCont);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsCont</returns>
        public ObsCont GetObsCont(int index)
        {
            if ((ObsCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsCont
        /// </summary>
        public int GetObsContCount => (ObsCont != null ? ObsCont.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsFisco">Elemento</param>
        public void AddObsFisco(ObsFisco obsFisco)
        {
            if (ObsFisco == null)
            {
                ObsFisco = new List<ObsFisco>();
            }

            ObsFisco.Add(obsFisco);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsFisco (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsFisco</returns>
        public ObsFisco GetObsFisco(int index)
        {
            if ((ObsFisco?.Count ?? 0) == 0)
            {
                return default;
            };

            return ObsFisco[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsFisco
        /// </summary>
        public int GetObsFiscoCount => (ObsFisco != null ? ObsFisco.Count : 0);

#endif

        #endregion

        #region ShouldSerialize

        public bool ShouldSerializeXCaracAd() => !string.IsNullOrWhiteSpace(XCaracAd);

        public bool ShouldSerializeXCaracSer() => !string.IsNullOrWhiteSpace(XCaracSer);

        public bool ShouldSerializeXEmi() => !string.IsNullOrWhiteSpace(XEmi);

        public bool ShouldSerializeOrigCalc() => !string.IsNullOrWhiteSpace(OrigCalc);

        public bool ShouldSerializeDestCalc() => !string.IsNullOrWhiteSpace(DestCalc);

        public bool ShouldSerializeXObs() => !string.IsNullOrWhiteSpace(XObs);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Fluxo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Fluxo
    {
        [XmlElement("xOrig")]
        public string XOrig { get; set; }

        [XmlElement("pass")]
        public List<Pass> Pass { get; set; }

        [XmlElement("xDest")]
        public string XDest { get; set; }

        [XmlElement("xRota")]
        public string XRota { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="pass">Elemento</param>
        public void AddPass(Pass pass)
        {
            if (Pass == null)
            {
                Pass = new List<Pass>();
            }

            Pass.Add(pass);
        }

        /// <summary>
        /// Retorna o elemento da lista Pass (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Pass</returns>
        public Pass GetPass(int index)
        {
            if ((Pass?.Count ?? 0) == 0)
            {
                return default;
            };

            return Pass[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Pass
        /// </summary>
        public int GetPassCount => (Pass != null ? Pass.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeXOrig() => !string.IsNullOrWhiteSpace(XOrig);
        public bool ShouldSerializeXDest() => !string.IsNullOrWhiteSpace(XDest);
        public bool ShouldSerializeXRota() => !string.IsNullOrWhiteSpace(XRota);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Pass")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Pass
    {
        [XmlElement("xPass")]
        public string XPass { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXPass() => !string.IsNullOrWhiteSpace(XPass);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Entrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Entrega
    {
        [XmlElement("semData")]
        public SemData SemData { get; set; }

        [XmlElement("comData")]
        public ComData ComData { get; set; }

        [XmlElement("noPeriodo")]
        public NoPeriodo NoPeriodo { get; set; }

        [XmlElement("semHora")]
        public SemHora SemHora { get; set; }

        [XmlElement("comHora")]
        public ComHora ComHora { get; set; }

        [XmlElement("noInter")]
        public NoInter NoInter { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.SemData")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class SemData
    {
        //private TipoPeriodoEntregaCTe TpPerField;

        [XmlElement("tpPer")]
        public TipoPeriodoEntregaCTe TpPer
        {
            get => TipoPeriodoEntregaCTe.SemDataDefinida;
            set
            {
                if (value != TipoPeriodoEntregaCTe.SemDataDefinida)
                {
                    throw new Exception("Conteúdo da TAG <tpPer>, filha da TAG <semData><Entrega>, inválido! Valores aceitos: 0.");
                }

                //TpPerField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ComData")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ComData
    {
        private TipoPeriodoEntregaCTe TpPerField;

        [XmlElement("tpPer")]
        public TipoPeriodoEntregaCTe TpPer
        {
            get => TpPerField;
            set
            {
                if (value == TipoPeriodoEntregaCTe.SemDataDefinida || value == TipoPeriodoEntregaCTe.NoPeriodo)
                {
                    throw new Exception("Conteúdo da TAG <tpPer>, filha da TAG <comData><Entrega>, inválido! Valores aceitos: 1, 2 ou 3.");
                }

                TpPerField = value;
            }
        }

        [XmlIgnore]
        public DateTime DProg { get; set; }

        [XmlElement("dProg")]
        public string DProgField
        {
            get => DProg.ToString("yyyy-MM-dd");
            set => DProg = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.NoPeriodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class NoPeriodo
    {
        //private TipoPeriodoEntregaCTe TpPerField;

        [XmlElement("tpPer")]
        public TipoPeriodoEntregaCTe TpPer
        {
            get => TipoPeriodoEntregaCTe.NoPeriodo;
            set
            {
                if (value != TipoPeriodoEntregaCTe.NoPeriodo)
                {
                    throw new Exception("Conteúdo da TAG <tpPer>, filha da TAG <noPeriodo><Entrega>, inválido! Valores aceitos: 4.");
                }

                //TpPerField = value;
            }
        }

        [XmlIgnore]
        public DateTime DIni { get; set; }

        [XmlElement("dIni")]
        public string DIniField
        {
            get => DIni.ToString("yyyy-MM-dd");
            set => DIni = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DFim { get; set; }

        [XmlElement("dFim")]
        public string DFimField
        {
            get => DFim.ToString("yyyy-MM-dd");
            set => DFim = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.SemHora")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class SemHora
    {
        //private TipoHoraEntregaCTe TpHorField;

        [XmlElement("tpHor")]
        public TipoHoraEntregaCTe TpHor
        {
            get => TipoHoraEntregaCTe.SemHoraDefinida;
            set
            {
                if (value != TipoHoraEntregaCTe.SemHoraDefinida)
                {
                    throw new Exception("Conteúdo da TAG <tpHor>, filha da TAG <semHora><Entrega>, inválido! Valores aceitos: 0.");
                }

                //TpHorField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ComHora")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ComHora
    {
        private TipoHoraEntregaCTe TpHorField;

        [XmlElement("tpHor")]
        public TipoHoraEntregaCTe TpHor
        {
            get => TpHorField;
            set
            {
                if (value == TipoHoraEntregaCTe.SemHoraDefinida || value == TipoHoraEntregaCTe.NoIntervaloTempo)
                {
                    throw new Exception("Conteúdo da TAG <tpHor>, filha da TAG <comHora><Entrega>, inválido! Valores aceitos: 1, 2 ou 3.");
                }

                TpHorField = value;
            }
        }

        [XmlIgnore]
        public DateTime HProg { get; set; }

        [XmlElement("hProg")]
        public string HProgField
        {
            get => HProg.ToString("HH:mm:ss");
            set => HProg = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.NoInter")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class NoInter
    {
        //private TipoHoraEntregaCTe TpHorField;

        [XmlElement("tpHor")]
        public TipoHoraEntregaCTe TpHor
        {
            get => TipoHoraEntregaCTe.NoIntervaloTempo;
            set
            {
                if (value != TipoHoraEntregaCTe.NoIntervaloTempo)
                {
                    throw new Exception("Conteúdo da TAG <tpHor>, filha da TAG <noInter><Entrega>, inválido! Valores aceitos: 4.");
                }

                //TpHorField = value;
            }
        }

        [XmlIgnore]
        public DateTime HIni { get; set; }

        [XmlElement("hIni")]
        public string HIniField
        {
            get => HIni.ToString("HH:mm:ss");
            set => HIni = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime HFim { get; set; }

        [XmlElement("hFim")]
        public string HFimField
        {
            get => HFim.ToString("HH:mm:ss");
            set => HFim = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ObsCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ObsCont
    {
        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ObsFisco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ObsFisco
    {
        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Emit
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("IEST")]
        public string IEST { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        [XmlElement("CRT")]
#if INTEROP
        public CRT CRT { get; set; } = (CRT)(-1);
#else
        public CRT? CRT { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        public bool ShouldSerializeCRT() => CRT != (CRT)(-1);
#else
        public bool ShouldSerializeCRT() => CRT != null;
#endif

        public bool ShouldSerializeIEST() => !string.IsNullOrWhiteSpace(IEST);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize               

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Rem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Rem
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("xFant")]
        public string XFant { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("enderReme")]
        public EnderReme EnderReme { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderReme")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderReme
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize 

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Exped")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Exped
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("enderExped")]
        public EnderExped EnderExped { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderExped")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderExped
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize 

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Receb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Receb
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("enderReceb")]
        public EnderReceb EnderReceb { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderReceb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderReceb
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize 

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Dest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Dest
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("ISUF")]
        public string ISUF { get; set; }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize 

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.VPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class VPrest
    {
        [XmlIgnore]
        public double VTPrest { get; set; }

        [XmlElement("vTPrest")]
        public string VTPrestField
        {
            get => VTPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VTPrest = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VRec { get; set; }

        [XmlElement("vRec")]
        public string VRecField
        {
            get => VRec.ToString("F2", CultureInfo.InvariantCulture);
            set => VRec = Utility.Converter.ToDouble(value);
        }

        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comp">Elemento</param>
        public void AddComp(Comp comp)
        {
            if (Comp == null)
            {
                Comp = new List<Comp>();
            }

            Comp.Add(comp);
        }

        /// <summary>
        /// Retorna o elemento da lista Comp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comp</returns>
        public Comp GetComp(int index)
        {
            if ((Comp?.Count ?? 0) == 0)
            {
                return default;
            };

            return Comp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comp
        /// </summary>
        public int GetCompCount => (Comp != null ? Comp.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Comp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Comp
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlIgnore]
        public double VComp { get; set; }

        [XmlElement("vComp")]
        public string VCompField
        {
            get => VComp.ToString("F2", CultureInfo.InvariantCulture);
            set => VComp = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Imp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Imp
    {
        [XmlElement("ICMS")]
        public ICMS ICMS { get; set; }

        [XmlIgnore]
        public double VTotTrib { get; set; }

        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Utility.Converter.ToDouble(value);
        }

        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("ICMSUFFim")]
        public ICMSUFFim ICMSUFFim { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS
    {
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        [XmlElement("ICMS45")]
        public ICMS45 ICMS45 { get; set; }

        [XmlElement("ICMS60")]
        public ICMS60 ICMS60 { get; set; }

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("ICMSOutraUF")]
        public ICMSOutraUF ICMSOutraUF { get; set; }

        [XmlElement("ICMSSN")]
        public ICMSSN ICMSSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS00")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS00
    {
        private string CSTField = "00";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS20")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS20
    {
        private string CSTField = "20";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS45")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS45
    {
        private string CSTField = "";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS60")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS60
    {
        private string CSTField = "60";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlIgnore]
        public double VBCSTRet { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSTRet { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSSTRet { get; set; }

        [XmlElement("pICMSSTRet")]
        public string PICMSSTRetField
        {
            get => PICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSSTRet = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCred { get; set; }

        [XmlElement("vCred")]
        public string VCredField
        {
            get => VCred.ToString("F2", CultureInfo.InvariantCulture);
            set => VCred = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVCredField() => VCred > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS90")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS90
    {
        private string CSTField = "90";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlIgnore]
        public double PRedBC { get; set; }

        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMS { get; set; }

        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCred { get; set; }

        [XmlElement("vCred")]
        public string VCredField
        {
            get => VCred.ToString("F2", CultureInfo.InvariantCulture);
            set => VCred = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCField() => PRedBC > 0;

        public bool ShouldSerializeVCredField() => VCred > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMSOutraUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSOutraUF
    {
        private string CSTField = "90";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlIgnore]
        public double PRedBCOutraUF { get; set; }

        [XmlElement("pRedBCOutraUF")]
        public string PRedBCOutraUFField
        {
            get => PRedBCOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBCOutraUF = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCOutraUF { get; set; }

        [XmlElement("vBCOutraUF")]
        public string VBCOutraUFField
        {
            get => VBCOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCOutraUF = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSOutraUF { get; set; }

        [XmlElement("pICMSOutraUF")]
        public string PICMSOutraUFField
        {
            get => PICMSOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSOutraUF = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSOutraUF { get; set; }

        [XmlElement("vICMSOutraUF")]
        public string VICMSOutraUFField
        {
            get => VICMSOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSOutraUF = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCOutraUFField() => PRedBCOutraUF > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMSSN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSSN
    {
        private string CSTField = "90";

        [XmlElement("CST")]
        public string CST
        {
            get
            {
                return CSTField;
            }
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        [XmlElement("indSN")]
        public SimNao IndSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMSUFFim")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSUFFim
    {
        [XmlIgnore]
        public double VBCUFFim { get; set; }

        [XmlElement("vBCUFFim")]
        public string VBCUFFimField
        {
            get => VBCUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCUFFim = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPUFFim { get; set; }

        [XmlElement("pFCPUFFim")]
        public string PFCPUFFimField
        {
            get => PFCPUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => PFCPUFFim = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSUFFim { get; set; }

        [XmlElement("pICMSUFFim")]
        public string PICMSUFFimField
        {
            get => PICMSUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSUFFim = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSInter { get; set; }

        [XmlElement("pICMSInter")]
        public string PICMSInterField
        {
            get => PICMSInter.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSInter = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPUFFim { get; set; }

        [XmlElement("vFCPUFFim")]
        public string VFCPUFFimField
        {
            get => VFCPUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFFim = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFFim { get; set; }

        [XmlElement("vICMSUFFim")]
        public string VICMSUFFimField
        {
            get => VICMSUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFFim = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSUFIni { get; set; }

        [XmlElement("vICMSUFIni")]
        public string VICMSUFIniField
        {
            get => VICMSUFIni.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFIni = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeNorm")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeNorm
    {
        [XmlElement("infCarga")]
        public InfCarga InfCarga { get; set; }

        [XmlElement("infDoc")]
        public InfDoc InfDoc { get; set; }

        [XmlElement("docAnt")]
        public DocAnt DocAnt { get; set; }

        [XmlElement("infModal")]
        public InfModal InfModal { get; set; }

        [XmlElement("veicNovos")]
        public List<VeicNovos> VeicNovos { get; set; }

        [XmlElement("cobr")]
        public Cobr Cobr { get; set; }

        [XmlElement("infCteSub")]
        public InfCteSub InfCteSub { get; set; }

        [XmlElement("infGlobalizado")]
        public InfGlobalizado InfGlobalizado { get; set; }

        [XmlElement("infServVinc")]
        public InfServVinc InfServVinc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="veicNovos">Elemento</param>
        public void AddVeicNovos(VeicNovos veicNovos)
        {
            if (VeicNovos == null)
            {
                VeicNovos = new List<VeicNovos>();
            }

            VeicNovos.Add(veicNovos);
        }

        /// <summary>
        /// Retorna o elemento da lista VeicNovos (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da VeicNovos</returns>
        public VeicNovos GetVeicNovos(int index)
        {
            if ((VeicNovos?.Count ?? 0) == 0)
            {
                return default;
            };

            return VeicNovos[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista VeicNovos
        /// </summary>
        public int GetVeicNovosCount => (VeicNovos != null ? VeicNovos.Count : 0);

#endif

        #region ShouldSerialize

        //public bool ShouldSerialize() => !string.IsNullOrWhiteSpace();

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCarga
    {
        [XmlIgnore]
        public double VCarga { get; set; }

        [XmlElement("vCarga")]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Utility.Converter.ToDouble(value);
        }

        [XmlElement("proPred")]
        public string ProPred { get; set; }

        [XmlElement("xOutCat")]
        public string XOutCat { get; set; }

        [XmlElement("infQ")]
        public List<InfQ> InfQ { get; set; }

        [XmlIgnore]
        public double VCargaAverb { get; set; }

        [XmlElement("vCargaAverb")]
        public string VCargaAverbField
        {
            get => VCargaAverb.ToString("F2", CultureInfo.InvariantCulture);
            set => VCargaAverb = Utility.Converter.ToDouble(value);
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infq">Elemento</param>
        public void AddInfQ(InfQ infq)
        {
            if (InfQ == null)
            {
                InfQ = new List<InfQ>();
            }

            InfQ.Add(infq);
        }

        /// <summary>
        /// Retorna o elemento da lista InfQ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfQ</returns>
        public InfQ GetInfQ(int index)
        {
            if ((InfQ?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfQ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfQ
        /// </summary>
        public int GetInfQCount => (InfQ != null ? InfQ.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeVCargaField() => VCarga >= 0;
        public bool ShouldSerializeXOutCat() => !string.IsNullOrWhiteSpace(XOutCat);
        public bool ShouldSerializeVCargaAverbField() => VCargaAverb > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfQ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfQ
    {
        [XmlElement("cUnid")]
        public CodigoUnidadeMedidaCTe CUnid { get; set; }

        [XmlElement("tpMed")]
        public string TpMed { get; set; }

        [XmlIgnore]
        public double QCarga { get; set; }

        [XmlElement("qCarga")]
        public string QCargaField
        {
            get => QCarga.ToString("F4", CultureInfo.InvariantCulture);
            set => QCarga = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfDoc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfDoc
    {
        [XmlElement("infNF")]
        public List<InfNF> InfNF { get; set; }

        [XmlElement("infNFe")]
        public List<InfNFe> InfNFe { get; set; }

        [XmlElement("infOutros")]
        public List<InfOutros> InfOutros { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infnf">Elemento</param>
        public void AddInfNF(InfNF infnf)
        {
            if (InfNF == null)
            {
                InfNF = new List<InfNF>();
            }

            InfNF.Add(infnf);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNF</returns>
        public InfNF GetInfNF(int index)
        {
            if ((InfNF?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNF
        /// </summary>
        public int GetInfNFCount => (InfNF != null ? InfNF.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infnfe">Elemento</param>
        public void AddInfNFe(InfNFe infnfe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<InfNFe>();
            }

            InfNFe.Add(infnfe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFe</returns>
        public InfNFe GetInfNFe(int index)
        {
            if ((InfNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infoutros">Elemento</param>
        public void AddInfOutros(InfOutros infoutros)
        {
            if (InfOutros == null)
            {
                InfOutros = new List<InfOutros>();
            }

            InfOutros.Add(infoutros);
        }

        /// <summary>
        /// Retorna o elemento da lista InfOutros (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfOutros</returns>
        public InfOutros GetInfOutros(int index)
        {
            if ((InfOutros?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfOutros[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfOutros
        /// </summary>
        public int GetInfOutrosCount => (InfOutros != null ? InfOutros.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNF
    {
        [XmlElement("nRoma")]
        public string NRoma { get; set; }

        [XmlElement("nPed")]
        public string NPed { get; set; }

        [XmlElement("mod")]
        public ModeloNF Mod { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VBC { get; set; }

        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMS { get; set; }

        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VST { get; set; }

        [XmlElement("vST")]
        public string VSTField
        {
            get => VST.ToString("F2", CultureInfo.InvariantCulture);
            set => VST = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VProd { get; set; }

        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VNF { get; set; }

        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Utility.Converter.ToDouble(value);
        }

        [XmlElement("nCFOP")]
        public string NCFOP { get; set; }

        [XmlIgnore]
        public double NPeso { get; set; }

        [XmlElement("nPeso")]
        public string NPesoField
        {
            get => NPeso.ToString("F3", CultureInfo.InvariantCulture);
            set => NPeso = Utility.Converter.ToDouble(value);
        }

        [XmlElement("PIN")]
        public string PIN { get; set; }

        [XmlIgnore]
        public DateTime DPrev { get; set; }

        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCarga">Elemento</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCarga</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidTransp">Elemento</param>
        public void AddInfUnidTransp(InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidTransp</returns>
        public InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTransp
        /// </summary>
        public int GetInfUnidTranspCount => (InfUnidTransp != null ? InfUnidTransp.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeNRoma() => !string.IsNullOrWhiteSpace(NRoma);
        public bool ShouldSerializeNPed() => !string.IsNullOrWhiteSpace(NPed);
        public bool ShouldSerializeNPesoField() => NPeso > 0;
        public bool ShouldSerializePIN() => !string.IsNullOrWhiteSpace(PIN);
        public bool ShouldSerializeDPrevField() => DPrev > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfUnidCarga
    {
        [XmlElement("tpUnidCarga")]
        public virtual TipoUnidadeCarga TpUnidCarga { get; set; }

        [XmlElement("idUnidCarga")]
        public string IdUnidCarga { get; set; }

        [XmlElement("lacUnidCarga")]
        public List<LacUnidCarga> LacUnidCarga { get; set; }

        [XmlIgnore]
        public double QtdRat { get; set; }

        [XmlElement("qtdRat")]
        public string QtdRatField
        {
            get => QtdRat.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdRat = Utility.Converter.ToDouble(value);
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacUnidCarga">Elemento</param>
        public void AddLacUnidCarga(LacUnidCarga lacUnidCarga)
        {
            if (LacUnidCarga == null)
            {
                LacUnidCarga = new List<LacUnidCarga>();
            }

            LacUnidCarga.Add(lacUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista LacUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da LacUnidCarga</returns>
        public LacUnidCarga GetLacUnidCarga(int index)
        {
            if ((LacUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return LacUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista LacUnidCarga
        /// </summary>
        public int GetLacUnidCargaCount => (LacUnidCarga != null ? LacUnidCarga.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.LacUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LacUnidCarga
    {
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfUnidTransp
    {
        [XmlElement("tpUnidTransp")]
        public virtual TipoUnidadeTransporte TpUnidTransp { get; set; }

        [XmlElement("idUnidTransp")]
        public string IdUnidTransp { get; set; }

        [XmlElement("lacUnidTransp")]
        public List<LacUnidTransp> LacUnidTransp { get; set; }

        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        [XmlIgnore]
        public double QtdRat { get; set; }

        [XmlElement("qtdRat")]
        public string QtdRatField
        {
            get => QtdRat.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdRat = Utility.Converter.ToDouble(value);
        }


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacUnidTransp">Elemento</param>
        public void AddLacUnidTransp(LacUnidTransp lacUnidTransp)
        {
            if (LacUnidTransp == null)
            {
                LacUnidTransp = new List<LacUnidTransp>();
            }

            LacUnidTransp.Add(lacUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista LacUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da LacUnidTransp</returns>
        public LacUnidTransp GetLacUnidTransp(int index)
        {
            if ((LacUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            };

            return LacUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista LacUnidTransp
        /// </summary>
        public int GetLacUnidTranspCount => (LacUnidTransp != null ? LacUnidTransp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCarga">Elemento</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCarga</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.LacUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LacUnidTransp : LacUnidCarga { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNFe
    {
        [XmlElement("chave")]
        public string Chave { get; set; }

        [XmlElement("PIN")]
        public string PIN { get; set; }

        [XmlIgnore]
        public DateTime DPrev { get; set; }

        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCarga">Elemento</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCarga</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidTransp">Elemento</param>
        public void AddInfUnidTransp(InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidTransp</returns>
        public InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTransp
        /// </summary>
        public int GetInfUnidTranspCount => (InfUnidTransp != null ? InfUnidTransp.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializePIN() => !string.IsNullOrWhiteSpace(PIN);
        public bool ShouldSerializeDPrevField() => DPrev > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfOutros")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfOutros
    {
        [XmlElement("tpDoc")]
        public TipoDocumentoOriginarioCTe TpDoc { get; set; }

        [XmlElement("descOutros")]
        public string DescOutros { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VDocFisc { get; set; }

        [XmlElement("vDocFisc")]
        public string VDocFiscField
        {
            get => VDocFisc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDocFisc = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime DPrev { get; set; }

        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCarga">Elemento</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCarga</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidTransp">Elemento</param>
        public void AddInfUnidTransp(InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidTransp</returns>
        public InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTransp
        /// </summary>
        public int GetInfUnidTranspCount => (InfUnidTransp != null ? InfUnidTransp.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeDescOutros() => !string.IsNullOrWhiteSpace(DescOutros);
        public bool ShouldSerializeNDoc() => !string.IsNullOrWhiteSpace(NDoc);
        public bool ShouldSerializeDEmiField() => DEmi > DateTime.MinValue;
        public bool ShouldSerializeVDocFiscField() => VDocFisc > 0;
        public bool ShouldSerializeDPrevField() => DPrev > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DocAnt
    {
        [XmlElement("emiDocAnt")]
        public List<EmiDocAnt> EmiDocAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="emiDocAnt">Elemento</param>
        public void AddEmiDocAnt(EmiDocAnt emiDocAnt)
        {
            if (EmiDocAnt == null)
            {
                EmiDocAnt = new List<EmiDocAnt>();
            }

            EmiDocAnt.Add(emiDocAnt);
        }

        /// <summary>
        /// Retorna o elemento da lista EmiDocAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da EmiDocAnt</returns>
        public EmiDocAnt GetEmiDocAnt(int index)
        {
            if ((EmiDocAnt?.Count ?? 0) == 0)
            {
                return default;
            };

            return EmiDocAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista EmiDocAnt
        /// </summary>
        public int GetEmiDocAntCount => (EmiDocAnt != null ? EmiDocAnt.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EmiDocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EmiDocAnt
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("idDocAnt")]
        public List<IdDocAnt> IdDocAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="idDocAnt">Elemento</param>
        public void AddIdDocAnt(IdDocAnt idDocAnt)
        {
            if (IdDocAnt == null)
            {
                IdDocAnt = new List<IdDocAnt>();
            }

            IdDocAnt.Add(idDocAnt);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdDocAnt</returns>
        public IdDocAnt GetIdDocAnt(int index)
        {
            if ((IdDocAnt?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdDocAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAnt
        /// </summary>
        public int GetIdDocAntCount => (IdDocAnt != null ? IdDocAnt.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAnt
    {
        [XmlElement("idDocAntEle")]
        public List<IdDocAntEle> IdDocAntEle { get; set; }

        [XmlElement("idDocAntPap")]
        public List<IdDocAntPap> IdDocAntPap { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="idDocAntEle">Elemento</param>
        public void AddIdDocAntEle(IdDocAntEle idDocAntEle)
        {
            if (IdDocAntEle == null)
            {
                IdDocAntEle = new List<IdDocAntEle>();
            }

            IdDocAntEle.Add(idDocAntEle);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAntEle (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdDocAntEle</returns>
        public IdDocAntEle GetIdDocAntEle(int index)
        {
            if ((IdDocAntEle?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdDocAntEle[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAntEle
        /// </summary>
        public int GetIdDocAntEleCount => (IdDocAntEle != null ? IdDocAntEle.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="idDocAntPap">Elemento</param>
        public void AddIdDocAntPap(IdDocAntPap idDocAntPap)
        {
            if (IdDocAntPap == null)
            {
                IdDocAntPap = new List<IdDocAntPap>();
            }

            IdDocAntPap.Add(idDocAntPap);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAntPap (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da IdDocAntPap</returns>
        public IdDocAntPap GetIdDocAntPap(int index)
        {
            if ((IdDocAntPap?.Count ?? 0) == 0)
            {
                return default;
            };

            return IdDocAntPap[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAntPap
        /// </summary>
        public int GetIdDocAntPapCount => (IdDocAntPap != null ? IdDocAntPap.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAntPap")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAntPap
    {
        [XmlElement("tpDoc")]
        public TipoDocumentoTransporteAnteriorCTe TpDoc { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("subser")]
        public string Subser { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeSubser() => !string.IsNullOrWhiteSpace(Subser);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAntEle")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAntEle
    {
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfModal
    {
        [XmlAttribute(AttributeName = "versaoModal", DataType = "token")]
        public string VersaoModal { get; set; }

        [XmlElement("rodo")]
        public Rodo Rodo { get; set; }

        [XmlElement("multimodal")]
        public MultiModal MultiModal { get; set; }

        [XmlElement("duto")]
        public Duto Duto { get; set; }

        [XmlElement("aereo")]
        public Aereo Aereo { get; set; }

        [XmlElement("aquav")]
        public Aquav Aquav { get; set; }

        [XmlElement("ferrov")]
        public Ferrov Ferrov { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Rodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Rodo
    {
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        [XmlElement("occ")]
        public List<Occ> Occ { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="occ">Elemento</param>
        public void AddOcc(Occ occ)
        {
            if (Occ == null)
            {
                Occ = new List<Occ>();
            }

            Occ.Add(occ);
        }

        /// <summary>
        /// Retorna o elemento da lista Occ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Occ</returns>
        public Occ GetOcc(int index)
        {
            if ((Occ?.Count ?? 0) == 0)
            {
                return default;
            };

            return Occ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Occ
        /// </summary>
        public int GetOccCount => (Occ != null ? Occ.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Occ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Occ
    {
        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nOcc")]
        public int NOcc { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        [XmlElement("emiOcc")]
        public EmiOcc EmiOcc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeSerie() => !string.IsNullOrWhiteSpace(Serie);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EmiOcc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EmiOcc
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("cInt")]
        public string CInt { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.MultiModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class MultiModal
    {
        [XmlElement("COTM")]
        public string COTM { get; set; }

        [XmlElement("indNegociavel")]
        public IndicadorNegociavelCTe IndNegociavel { get; set; }

        [XmlElement("seg")]
        public Seg Seg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Seg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Seg
    {
        [XmlElement("infSeg")]
        public InfSeg InfSeg { get; set; }

        [XmlElement("nApol")]
        public string NApol { get; set; }

        [XmlElement("nAver")]
        public string NAver { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfSeg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfSeg
    {
        [XmlElement("xSeg")]
        public string XSeg { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Duto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Duto
    {
        [XmlIgnore]
        public double VTar { get; set; }

        [XmlElement("vTar")]
        public string VTarField
        {
            get => VTar.ToString("F6", CultureInfo.InvariantCulture);
            set => VTar = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime DIni { get; set; }

        [XmlElement("dIni")]
        public string DIniField
        {
            get => DIni.ToString("yyyy-MM-dd");
            set => DIni = DateTime.Parse(value);
        }

        [XmlIgnore]
        public DateTime DFim { get; set; }

        [XmlElement("dFim")]
        public string DFimField
        {
            get => DFim.ToString("yyyy-MM-dd");
            set => DFim = DateTime.Parse(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVTarField() => VTar > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Aereo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Aereo
    {
        [XmlElement("nMinu")]
        public string NMinu { get; set; }

        [XmlElement("nOCA")]
        public string NOCA { get; set; }

        [XmlIgnore]
        public DateTime DPrevAereo { get; set; }

        [XmlElement("dPrevAereo")]
        public string DPrevAereoField
        {
            get => DPrevAereo.ToString("yyyy-MM-dd");
            set => DPrevAereo = DateTime.Parse(value);
        }

        [XmlElement("natCarga")]
        public NatCarga NatCarga { get; set; }

        [XmlElement("tarifa")]
        public Tarifa Tarifa { get; set; }

        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="peri">Elemento</param>
        public void AddPeri(Peri peri)
        {
            if (Peri == null)
            {
                Peri = new List<Peri>();
            }

            Peri.Add(peri);
        }

        /// <summary>
        /// Retorna o elemento da lista Peri (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Peri</returns>
        public Peri GetPeri(int index)
        {
            if ((Peri?.Count ?? 0) == 0)
            {
                return default;
            };

            return Peri[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Peri
        /// </summary>
        public int GetPeriCount => (Peri != null ? Peri.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeNMinu() => !string.IsNullOrWhiteSpace(NMinu);
        public bool ShouldSerializeNOCA() => !string.IsNullOrWhiteSpace(NOCA);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.NatCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class NatCarga
    {
        [XmlElement("xDime")]
        public string XDime { get; set; }

        [XmlElement("cInfManu")]
        public List<InformacaoManuseioCTe> CInfManu { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="cInfManu">Elemento</param>
        public void AddCInfManu(InformacaoManuseioCTe cInfManu)
        {
            if (CInfManu == null)
            {
                CInfManu = new List<InformacaoManuseioCTe>();
            }

            CInfManu.Add(cInfManu);
        }

        /// <summary>
        /// Retorna o elemento da lista CInfManu (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da CInfManu</returns>
        public InformacaoManuseioCTe GetCInfManu(int index)
        {
            if ((CInfManu?.Count ?? 0) == 0)
            {
                return default;
            };

            return CInfManu[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CInfManu
        /// </summary>
        public int GetCInfManuCount => (CInfManu != null ? CInfManu.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeXDime() => !string.IsNullOrWhiteSpace(XDime);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Tarifa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Tarifa
    {
        [XmlElement("CL")]
        public string CL { get; set; }

        [XmlElement("cTar")]
        public string CTar { get; set; }

        [XmlIgnore]
        public double VTar { get; set; }

        [XmlElement("vTar")]
        public string VTarField
        {
            get => VTar.ToString("F2", CultureInfo.InvariantCulture);
            set => VTar = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCTar() => !string.IsNullOrWhiteSpace(CTar);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Peri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Peri
    {
        [XmlElement("nONU")]
        public string NONU { get; set; }

        [XmlElement("qTotEmb")]
        public string QTotEmb { get; set; }

        [XmlElement("infTotAP")]
        public InfTotAP InfTotAP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfTotAP")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfTotAP
    {
        [XmlIgnore]
        public double QTotProd { get; set; }

        [XmlElement("qTotProd")]
        public string QTotProdField
        {
            get => QTotProd.ToString("F4", CultureInfo.InvariantCulture);
            set => QTotProd = Utility.Converter.ToDouble(value);
        }

        [XmlElement("uniAP")]
        public UnidadeMedidaArtigoPerigoso UniAP { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Aquav")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Aquav
    {
        [XmlIgnore]
        public double VPrest { get; set; }

        [XmlElement("vPrest")]
        public string VPrestField
        {
            get => VPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VPrest = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VAFRMM { get; set; }

        [XmlElement("vAFRMM")]
        public string VAFRMMField
        {
            get => VAFRMM.ToString("F2", CultureInfo.InvariantCulture);
            set => VAFRMM = Utility.Converter.ToDouble(value);
        }

        [XmlElement("xNavio")]
        public string XNavio { get; set; }

        [XmlElement("balsa")]
        public List<Balsa> Balsa { get; set; }

        [XmlElement("nViag")]
        public string NViag { get; set; }

        [XmlElement("direc")]
        public DirecaoCTe Direc { get; set; }

        [XmlElement("irin")]
        public string Irin { get; set; }

        [XmlElement("detCont")]
        public List<DetCont> DetCont { get; set; }

        [XmlElement("tpNav")]
#if INTEROP
        public TipoNavegacao TpNav { get; set; } = TipoNavegacao.NaoDefinido;
#else
        public TipoNavegacao? TpNav { get; set; }
#endif

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="balsa">Elemento</param>
        public void AddBalsa(Balsa balsa)
        {
            if (Balsa == null)
            {
                Balsa = new List<Balsa>();
            }

            Balsa.Add(balsa);
        }

        /// <summary>
        /// Retorna o elemento da lista Balsa (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Balsa</returns>
        public Balsa GetBalsa(int index)
        {
            if ((Balsa?.Count ?? 0) == 0)
            {
                return default;
            };

            return Balsa[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Balsa
        /// </summary>
        public int GetBalsaCount => (Balsa != null ? Balsa.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="detCont">Elemento</param>
        public void AddDetCont(DetCont detCont)
        {
            if (DetCont == null)
            {
                DetCont = new List<DetCont>();
            }

            DetCont.Add(detCont);
        }

        /// <summary>
        /// Retorna o elemento da lista DetCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetCont</returns>
        public DetCont GetDetCont(int index)
        {
            if ((DetCont?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetCont
        /// </summary>
        public int GetDetContCount => (DetCont != null ? DetCont.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeNViag() => !string.IsNullOrWhiteSpace(NViag);
        public bool ShouldSerializeTpNav() => TpNav != null && TpNav != TipoNavegacao.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Balsa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Balsa
    {
        [XmlElement("xBalsa")]
        public string XBalsa { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetCont
    {
        [XmlElement("nCont")]
        public string NCont { get; set; }

        [XmlElement("lacre")]
        public List<Lacre> Lacre { get; set; }

        [XmlElement("infDoc")]
        public DetContInfDoc InfDoc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacre">Elemento</param>
        public void AddLacre(Lacre lacre)
        {
            if (Lacre == null)
            {
                Lacre = new List<Lacre>();
            }

            Lacre.Add(lacre);
        }

        /// <summary>
        /// Retorna o elemento da lista Lacre (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Lacre</returns>
        public Lacre GetLacre(int index)
        {
            if ((Lacre?.Count ?? 0) == 0)
            {
                return default;
            };

            return Lacre[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Lacre
        /// </summary>
        public int GetLacreCount => (Lacre != null ? Lacre.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Lacre")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Lacre
    {
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDoc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDoc
    {
        [XmlElement("infNF")]
        public List<DetContInfDocInfNF> InfNF { get; set; }

        [XmlElement("infNFe")]
        public List<DetContInfDocInfNFe> InfNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNF">Elemento</param>
        public void AddInfNF(DetContInfDocInfNF infNF)
        {
            if (InfNF == null)
            {
                InfNF = new List<DetContInfDocInfNF>();
            }

            InfNF.Add(infNF);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNF</returns>
        public DetContInfDocInfNF GetInfNF(int index)
        {
            if ((InfNF?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNF
        /// </summary>
        public int GetInfNFCount => (InfNF != null ? InfNF.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNFe">Elemento</param>
        public void AddInfNFe(DetContInfDocInfNFe infNFe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<DetContInfDocInfNFe>();
            }

            InfNFe.Add(infNFe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFe</returns>
        public DetContInfDocInfNFe GetInfNFe(int index)
        {
            if ((InfNFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDocInfNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDocInfNF
    {
        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlIgnore]
        public double UnidRat { get; set; }

        [XmlElement("unidRat")]
        public string UnidRatField
        {
            get => UnidRat.ToString("F2", CultureInfo.InvariantCulture);
            set => UnidRat = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeUnidRatField() => UnidRat > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDocInfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDocInfNFe
    {
        [XmlElement("chave")]
        public string Chave { get; set; }

        [XmlIgnore]
        public double UnidRat { get; set; }

        [XmlElement("unidRat")]
        public string UnidRatField
        {
            get => UnidRat.ToString("F2", CultureInfo.InvariantCulture);
            set => UnidRat = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeUnidRatField() => UnidRat > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Ferrov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Ferrov
    {
        [XmlElement("tpTraf")]
        public TipoTrafegoCTe TpTraf { get; set; }

        [XmlElement("trafMut")]
        public TrafMut TrafMut { get; set; }

        [XmlElement("fluxo")]
        public string Fluxo { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.TrafMut")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class TrafMut
    {
        [XmlElement("respFat")]
        public FerroviaCTe RespFat { get; set; }

        [XmlElement("ferrEmi")]
        public FerroviaCTe FerrEmi { get; set; }

        [XmlIgnore]
        public double VFrete { get; set; }

        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Utility.Converter.ToDouble(value);
        }

        [XmlElement("chCTeFerroOrigem")]
        public string ChCTeFerroOrigem { get; set; }

        [XmlElement("ferroEnv")]
        public List<FerroEnv> FerroEnv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="ferroEnv">Elemento</param>
        public void AddFerroEnv(FerroEnv ferroEnv)
        {
            if (FerroEnv == null)
            {
                FerroEnv = new List<FerroEnv>();
            }

            FerroEnv.Add(ferroEnv);
        }

        /// <summary>
        /// Retorna o elemento da lista FerroEnv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da FerroEnv</returns>
        public FerroEnv GetFerroEnv(int index)
        {
            if ((FerroEnv?.Count ?? 0) == 0)
            {
                return default;
            };

            return FerroEnv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista FerroEnv
        /// </summary>
        public int GetFerroEnvCount => (FerroEnv != null ? FerroEnv.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeChCTeFerroOrigem() => !string.IsNullOrWhiteSpace(ChCTeFerroOrigem);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.FerroEnv")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class FerroEnv
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("cInt")]
        public string CInt { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("enderFerro")]
        public EnderFerro EnderFerro { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderFerro")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderFerro
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
        public int CMun { get; set; }

        [XmlElement("xMun")]
        public string XMun { get; set; }

        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        #region ShouldSerialize 

        public bool ShouldSerializeNro() => !string.IsNullOrWhiteSpace(Nro);

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeXBairro() => !string.IsNullOrWhiteSpace(XBairro);

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.VeicNovos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class VeicNovos
    {
        [XmlElement("chassi")]
        public string Chassi { get; set; }

        [XmlElement("cCor")]
        public string CCor { get; set; }

        [XmlElement("xCor")]
        public string XCor { get; set; }

        [XmlElement("cMod")]
        public string CMod { get; set; }

        [XmlIgnore]
        public double VUnit { get; set; }

        [XmlElement("vUnit")]
        public string VUnitField
        {
            get => VUnit.ToString("F2", CultureInfo.InvariantCulture);
            set => VUnit = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFrete { get; set; }

        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Cobr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Cobr
    {
        [XmlElement("fat")]
        public Fat Fat { get; set; }

        [XmlElement("dup")]
        public List<Dup> Dup { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="dup">Elemento</param>
        public void AddDup(Dup dup)
        {
            if (Dup == null)
            {
                Dup = new List<Dup>();
            }

            Dup.Add(dup);
        }

        /// <summary>
        /// Retorna o elemento da lista Dup (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Dup</returns>
        public Dup GetDup(int index)
        {
            if ((Dup?.Count ?? 0) == 0)
            {
                return default;
            };

            return Dup[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dup
        /// </summary>
        public int GetDupCount => (Dup != null ? Dup.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Fat")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Fat
    {
        [XmlElement("nFat")]
        public string NFat { get; set; }

        [XmlIgnore]
        public double VOrig { get; set; }

        [XmlElement("vOrig")]
        public string VOrigField
        {
            get => VOrig.ToString("F2", CultureInfo.InvariantCulture);
            set => VOrig = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VDesc { get; set; }

        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VLiq { get; set; }

        [XmlElement("vLiq")]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVDescField() => VDesc > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Dup")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Dup
    {
        [XmlElement("nDup")]
        public string NDup { get; set; }

        [XmlIgnore]
        public DateTime DVenc { get; set; }

        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VDup { get; set; }

        [XmlElement("vDup")]
        public string VDupField
        {
            get => VDup.ToString("F2", CultureInfo.InvariantCulture);
            set => VDup = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteSub")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteSub
    {
        [XmlElement("chCte")]
        public string ChCte { get; set; }

        //TODO: Wandrey - Remover a tag RefCteAnu quando a versão 3.00 do CTe não existir mais.
        /// <summary>
        /// Propriedade só existe até a versão 3.00 do schema do CTe
        /// </summary>
        [XmlElement("refCteAnu")]
        public string RefCteAnu { get; set; } 

        [XmlElement("tomaICMS")]
        public TomaICMS TomaICMS { get; set; }

        [XmlIgnore]
        public SimNao IndAlteraToma { get; set; }

        [XmlElement("indAlteraToma")]
        public string IndAlteraTomaField
        {
            get => (IndAlteraToma == SimNao.Sim ? "1" : "");
            set => IndAlteraToma = (SimNao)Enum.Parse(typeof(SimNao), value.ToString());
        }

        #region ShouldSerialize

        public bool ShouldSerializeRefCteAnu() => !string.IsNullOrWhiteSpace(RefCteAnu);
        public bool ShouldSerializeIndAlteraTomaField() => IndAlteraToma == SimNao.Sim;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.TomaICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class TomaICMS
    {
        [XmlElement("refNFe")]
        public string RefNFe { get; set; }

        [XmlElement("refNF")]
        public RefNF RefNF { get; set; }

        [XmlElement("refCte")]
        public string RefCte { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeRefNFe() => !string.IsNullOrWhiteSpace(RefNFe);
        public bool ShouldSerializeRefCte() => !string.IsNullOrWhiteSpace(RefCte);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.RefNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class RefNF
    {
        private string ModField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set => ModField = value;
        }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("subserie")]
        public string Subserie { get; set; }

        [XmlElement("nro")]
        public int Nro { get; set; }

        [XmlIgnore]
        public double Valor { get; set; }

        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeSubserie() => !string.IsNullOrWhiteSpace(Subserie);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfGlobalizado")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfGlobalizado
    {
        private string XObsField;

        [XmlElement("xObs")]
        public string XObs
        {
            get => XObsField;
            set
            {
                if (value.Length < 15)
                {
                    throw new Exception("Conteúdo da TAG <xObs>, filha da TAG <infGlobalizado>, inválido! O conteúdo deve ter no mínimo 15 caracteres.");
                }
                else if (value.Length > 256)
                {
                    throw new Exception("Conteúdo da TAG <xObs>, filha da TAG <infGlobalizado>, inválido! O conteúdo deve ter no máximo 256 caracteres.");
                }

                XObsField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfServVinc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfServVinc
    {
        [XmlElement("infCTeMultimodal")]
        public List<InfCTeMultimodal> InfCTeMultimodal { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infCTeMultimodal">Elemento</param>
        public void AddInfCTeMultimodal(InfCTeMultimodal infCTeMultimodal)
        {
            if (InfCTeMultimodal == null)
            {
                InfCTeMultimodal = new List<InfCTeMultimodal>();
            }

            InfCTeMultimodal.Add(infCTeMultimodal);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCTeMultimodal (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCTeMultimodal</returns>
        public InfCTeMultimodal GetInfCTeMultimodal(int index)
        {
            if ((InfCTeMultimodal?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCTeMultimodal[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCTeMultimodal
        /// </summary>
        public int GetInfCTeMultimodalCount => (InfCTeMultimodal != null ? InfCTeMultimodal.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeMultimodal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeMultimodal
    {
        [XmlElement("chCTeMultimodal")]
        public string ChCTeMultimodal { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteComp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteComp
    {
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteAnu")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteAnu
    {
        [XmlElement("chCte")]
        public string ChCte { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.AutXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class AutXML
    {
        private string CNPJField;
        private string CPFField;

        [XmlElement("CNPJ")]
        public string CNPJ
        {
            get => CNPJField;
            set
            {
                if (!string.IsNullOrWhiteSpace(CPFField))
                {
                    throw new Exception("Não é permitido informar conteúdo na TAG <CPF> e <CNPJ>, filhas da TAG <auxXML>, ao mesmo tempo. Somente uma delas pode ter conteúdo.");
                }

                CNPJField = value;
            }
        }

        [XmlElement("CPF")]
        public string CPF
        {
            get => CPFField;
            set
            {
                if (!string.IsNullOrWhiteSpace(CNPJField))
                {
                    throw new Exception("Não é permitido informar conteúdo na TAG <CPF> e <CNPJ>, filhas da TAG <auxXML>, ao mesmo tempo. Somente uma delas pode ter conteúdo.");
                }

                CPFField = value;
            }
        }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfRespTec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
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

        [XmlElement("hashCSRT", DataType = "base64Binary")]
        public byte[] HashCSRT { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);

        public bool ShouldSerializeHashCSRT() => HashCSRT != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfSolicNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfSolicNFF
    {
        [XmlElement("xSolic", Order = 0)]
        public string XSolic { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeSupl
    {
        [XmlElement("qrCodCTe")]
        public string QrCodCTe { get; set; }
    }
}
