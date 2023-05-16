#pragma warning disable CS1591

#if INTEROP
// The result of the expression is always the same since a value of this type is never equal to 'null'
#pragma warning disable CS0472 

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

namespace Unimake.Business.DFe.Xml.CTeOS
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.CTeOS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    [XmlRoot("CTeOS", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class CTeOS : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("infCte")]
        public InfCTe InfCTe { get; set; }

        [XmlElement("infCTeSupl")]
        public InfCTeSupl InfCTeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        public override XmlDocument GerarXML()
        {
            var xmlDoc = base.GerarXML();

            foreach (var nodeCTe in xmlDoc.GetElementsByTagName("CTe"))
            {
                var elemCTe = (XmlElement)nodeCTe;

                var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
                elemCTe.SetAttribute("xmlns", attribute.Namespace);
            }

            return xmlDoc;
        }

        /// <summary>
        /// Deserializar o XML no objeto CTeOS
        /// </summary>
        /// <param name="filename">Localização do arquivo XML do CTeOS</param>
        /// <returns>Objeto do CTeOS</returns>
        public CTeOS LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CTeOS>(doc);
        }

        /// <summary>
        /// Deserializar o XML CTeOS no objeto CTeOS
        /// </summary>
        /// <param name="xml">string do XML CTeOS</param>
        /// <returns>Objeto da CTeOS</returns>
        public CTeOS LoadFromXML(string xml) => XMLUtility.Deserializar<CTeOS>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCTe")]
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

        [XmlElement("toma")]
        public Toma Toma { get; set; }

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
                    Emit.CNPJ.PadLeft(14, '0') +
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

        #region ShouldSerialize

        public bool ShouldSerializeInfCteAnu() => Convert.ToDecimal(Versao) <= 300;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Ide")]
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
                        throw new Exception("Defina antes o conteúdo da TAG <nCT>, pois a mesma é utilizada como base para calcular o código numérico.");
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
        public ModeloDFe Mod { get; set; } = ModeloDFe.CTeOS;

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
                    value == TipoEmissao.ContingenciaEPEC ||
                    value == TipoEmissao.RegimeEspecialNFF ||
                    value == TipoEmissao.ContingenciaOffLine ||
                    value == TipoEmissao.ContingenciaSVCAN)
                {
                    throw new Exception("Conteúdo da TAG <tpEmis> inválido! Valores aceitos: 1, 5, 7 ou 8.");
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

        [XmlElement("cMunEnv")]
        public string CMunEnv { get; set; }

        [XmlElement("xMunEnv")]
        public string XMunEnv { get; set; }

        [XmlElement("UFEnv")]
        public UFBrasil UFEnv { get; set; }

        private ModalidadeTransporteCTe ModalField;

        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal
        {
            get => ModalField;
            set
            {
                if (value == ModalidadeTransporteCTe.Dutoviario ||
                    value == ModalidadeTransporteCTe.Multimodal)
                {
                    throw new Exception("Conteúdo da TAG <Modal> inválido! Valores aceitos: 01, 02, 03 e 04.");
                }

                ModalField = value;
            }
        }

        [XmlElement("tpServ")]
        public TipoServicoCTeOS TpServ { get; set; }

        [XmlElement("indIEToma")]
        public IndicadorIEDestinatario IndIEToma { get; set; }

        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        [XmlElement("xMunIni")]
        public string XMunIni { get; set; }

#if INTEROP
        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UFIni")]
        public UFBrasil? UFIni { get; set; }
#endif

        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        [XmlElement("xMunFim")]
        public string XMunFim { get; set; }

#if INTEROP
        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UFFim")]
        public UFBrasil? UFFim { get; set; }
#endif

        [XmlElement("infPercurso")]
        public List<InfPercurso> InfPercurso { get; set; }

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

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infPercurso">Elemento</param>
        public void AddInfPercurso(InfPercurso infPercurso)
        {
            if (InfPercurso == null)
            {
                InfPercurso = new List<InfPercurso>();
            }

            InfPercurso.Add(infPercurso);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPercurso (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPercurso</returns>
        public InfPercurso GetInfPercurso(int index)
        {
            if ((InfPercurso?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPercurso[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPercurso
        /// </summary>
        public int GetInfPercursoCount => (InfPercurso != null ? InfPercurso.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeCMunIni() => !string.IsNullOrWhiteSpace(CMunIni);

        public bool ShouldSerializeXMunIni() => !string.IsNullOrWhiteSpace(XMunIni);

        public bool ShouldSerializeUFIni() => UFIni != null && UFIni != UFBrasil.NaoDefinido;

        public bool ShouldSerializeCMunFim() => !string.IsNullOrWhiteSpace(CMunFim);

        public bool ShouldSerializeXMunFim() => !string.IsNullOrWhiteSpace(XMunFim);

        public bool ShouldSerializeUFFim() => UFFim != null && UFFim != UFBrasil.NaoDefinido;

        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfPercurso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfPercurso
    {
        [XmlElement("UFPer")]
        public UFBrasil UFPer { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Compl")]
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

        [XmlElement("xObs")]
        public string XObs { get; set; }

        [XmlElement("ObsCont")]
        public List<ObsCont> ObsCont { get; set; }

        [XmlElement("ObsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeXCaracAd() => !string.IsNullOrWhiteSpace(XCaracAd);

        public bool ShouldSerializeXCaracSer() => !string.IsNullOrWhiteSpace(XCaracSer);

        public bool ShouldSerializeXEmi() => !string.IsNullOrWhiteSpace(XEmi);

        public bool ShouldSerializeXObs() => !string.IsNullOrWhiteSpace(XObs);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ObsCont")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ObsFisco")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
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

        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.EnderEmit")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Toma")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Toma
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.EnderToma")]
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

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais);

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.VPrest")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Comp")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Imp")]
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

        [XmlElement("infTribFed")]
        public InfTribFed InfTribFed { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMS")]
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

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("ICMSOutraUF")]
        public ICMSOutraUF ICMSOutraUF { get; set; }

        [XmlElement("ICMSSN")]
        public ICMSSN ICMSSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMS00")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS00
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "00";

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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMS20")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS20
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "20";

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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMS45")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS45
    {
        private string CSTField;

        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                if (value.Equals("40") || value.Equals("41") || value.Equals("51"))
                {
                    CSTField = value;
                }
                else
                {
                    throw new Exception("Conteúdo da TAG <CST> da <ICMS45> inválido! Valores aceitos: 40, 41 ou 51.");
                }
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMS90")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS90
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "90";

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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMSOutraUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSOutraUF
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "90";

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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMSSN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSSN
    {
        [XmlElement("CST")]
        public string CST { get; set; } = "90";

        [XmlElement("indSN")]
        public SimNao IndSN { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.ICMSUFFim")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfTribFed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfTribFed
    {
        [XmlIgnore]
        public double? VPIS { get; set; }

        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VIR { get; set; }

        [XmlElement("vIR")]
        public string VIRField
        {
            get => VIR?.ToString("F2", CultureInfo.InvariantCulture);
            set => VIR = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VINSS { get; set; }

        [XmlElement("vINSS")]
        public string VINSSField
        {
            get => VINSS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VINSS = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double? VCSLL { get; set; }

        [XmlElement("vCSLL")]
        public string VCSLLField
        {
            get => VCSLL?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCSLL = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVPISField() => VPIS != null;
        public bool ShouldSerializeVCOFINSField() => VCOFINS != null;
        public bool ShouldSerializeVIRField() => VIR != null;
        public bool ShouldSerializeVINSSField() => VINSS != null;
        public bool ShouldSerializeVCSLLField() => VCSLL != null;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCTeNorm")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeNorm
    {
        [XmlElement("infServico")]
        public InfServico InfServico { get; set; }

        [XmlElement("infDocRef")]
        public List<InfDocRef> InfDocRef { get; set; }

        [XmlElement("seg")]
        public List<Seg> Seg { get; set; }

        [XmlElement("infModal")]
        public InfModal InfModal { get; set; }

        [XmlElement("infCteSub")]
        public InfCteSub InfCteSub { get; set; }

        [XmlElement("refCTeCanc")]
        public string RefCTeCanc { get; set; }

        [XmlElement("cobr")]
        public Cobr Cobr { get; set; }

        [XmlElement("infGTVe")]
        public List<InfGTVe> InfGTVe { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infDocRef">Elemento</param>
        public void AddInfDocRef(InfDocRef infDocRef)
        {
            if (InfDocRef == null)
            {
                InfDocRef = new List<InfDocRef>();
            }

            InfDocRef.Add(infDocRef);
        }

        /// <summary>
        /// Retorna o elemento da lista InfDocRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfDocRef</returns>
        public InfDocRef GetInfDocRef(int index)
        {
            if ((InfDocRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfDocRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfDocRef
        /// </summary>
        public int GetInfDocRefCount => (InfDocRef != null ? InfDocRef.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="seg">Elemento</param>
        public void AddSeg(Seg seg)
        {
            if (Seg == null)
            {
                Seg = new List<Seg>();
            }

            Seg.Add(seg);
        }

        /// <summary>
        /// Retorna o elemento da lista Seg (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Seg</returns>
        public Seg GetSeg(int index)
        {
            if ((Seg?.Count ?? 0) == 0)
            {
                return default;
            };

            return Seg[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Seg
        /// </summary>
        public int GetSegCount => (Seg != null ? Seg.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infGTVe">Elemento</param>
        public void AddInfGTVe(InfGTVe infGTVe)
        {
            if (InfGTVe == null)
            {
                InfGTVe = new List<InfGTVe>();
            }

            InfGTVe.Add(infGTVe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfGTVe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfGTVe</returns>
        public InfGTVe GetInfGTVe(int index)
        {
            if ((InfGTVe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfGTVe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfGTVe
        /// </summary>
        public int GetInfGTVeCount => (InfGTVe != null ? InfGTVe.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeRefCTeCanc() => !string.IsNullOrWhiteSpace(RefCTeCanc);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfServico")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfServico
    {
        [XmlElement("xDescServ")]
        public string XDescServ { get; set; }

        [XmlElement("infQ")]
        public InfQ InfQ { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfQ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfQ
    {
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfDocRef")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfDocRef
    {
        [XmlElement("chBPe")]
        public string ChBPe { get; set; }

        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("subserie")]
        public string Subserie { get; set; }

        [XmlIgnore]
        public DateTime DEmi { get; set; }

        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VDoc { get; set; }

        [XmlElement("vDoc")]
        public string VDocField
        {
            get => VDoc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDoc = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeSerie() => !string.IsNullOrWhiteSpace(Serie);
        public bool ShouldSerializeSubserie() => !string.IsNullOrWhiteSpace(Subserie);
        public bool ShouldSerializeVDocField() => VDoc > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Seg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Seg
    {
        [XmlElement("respSeg")]
        public ResponsavelSeguroCTeOS RespSeg { get; set; }

        [XmlElement("xSeg")]
        public string XSeg { get; set; }

        [XmlElement("nApol")]
        public string NApol { get; set; }


        #region ShouldSerialize

        public bool ShouldSerializeXSeg() => !string.IsNullOrWhiteSpace(XSeg);
        public bool ShouldSerializeNApol() => !string.IsNullOrWhiteSpace(NApol);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCteSub")]
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

        #region ShouldSerialize

        public bool ShouldSerializeRefCteAnu() => !string.IsNullOrWhiteSpace(RefCteAnu);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.TomaICMS")]
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.RefNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class RefNF
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("mod")]
        public string Mod { get; set; }

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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Cobr")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfGTVe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfGTVe
    {
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }

        [XmlElement("Comp")]
        public List<InfGTVeComp> Comp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comp">Elemento</param>
        public void AddComp(InfGTVeComp comp)
        {
            if (Comp == null)
            {
                Comp = new List<InfGTVeComp>();
            }

            Comp.Add(comp);
        }

        /// <summary>
        /// Retorna o elemento da lista Comp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comp</returns>
        public InfGTVeComp GetComp(int index)
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfGTVeComp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfGTVeComp
    {
        [XmlElement("tpComp")]
        public TipoComponenteGTVe TpComp { get; set; }

        [XmlIgnore]
        public double VComp { get; set; }

        [XmlElement("vComp")]
        public string VCompField
        {
            get => VComp.ToString("F2", CultureInfo.InvariantCulture);
            set => VComp = Utility.Converter.ToDouble(value);
        }

        [XmlElement("xComp")]
        public string XComp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXComp() => !string.IsNullOrWhiteSpace(XComp);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Fat")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Dup")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCteComp")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCteAnu")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.AutXML")]
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
            set => CNPJField = value;
        }

        [XmlElement("CPF")]
        public string CPF
        {
            get => CPFField;
            set => CPFField = value;
        }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfRespTec")]
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
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfCTeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeSupl
    {
        [XmlElement("qrCodCTe")]
        public string QrCodCTe { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfModal
    {
        [XmlAttribute(AttributeName = "versaoModal", DataType = "token")]
        public string VersaoModal { get; set; }

        [XmlElement("rodoOS")]
        public RodoOS RodoOS { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.RodoOS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class RodoOS
    {
        private string TAFField;
        private string NroRegEstadualField;

        [XmlElement("TAF")]
        public string TAF
        {
            get => TAFField;
            set
            {
                if (!string.IsNullOrWhiteSpace(NroRegEstadualField))
                {
                    throw new Exception("Não é permitido informar conteúdo da TAG <TAF> e <NroRegEstadual>, filhas da TAG <rodoOS>, ao mesmo tempo, somente uma delas pode ter conteúdo.");
                }

                TAFField = value;
            }
        }

        [XmlElement("NroRegEstadual")]
        public string NroRegEstadual
        {
            get => NroRegEstadualField;
            set
            {
                if (!string.IsNullOrWhiteSpace(TAFField))
                {
                    throw new Exception("Não é permitido informar conteúdo da TAG <TAF> e <NroRegEstadual>, filhas da TAG <rodoOS>, ao mesmo tempo, somente uma delas pode ter conteúdo.");
                }

                NroRegEstadualField = value;
            }
        }

        [XmlElement("veic")]
        public Veic Veic { get; set; }

        [XmlElement("infFretamento")]
        public InfFretamento InfFretamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Veic")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Veic
    {
        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("RENAVAM")]
        public string RENAVAM { get; set; }

        [XmlElement("prop")]
        public Prop Prop { get; set; }

        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeRENAVAM() => !string.IsNullOrWhiteSpace(RENAVAM);
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.Prop")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Prop
    {
        private string TAFField;
        private string NroRegEstadualField;

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("TAF")]
        public string TAF
        {
            get => TAFField;
            set
            {
                if (!string.IsNullOrWhiteSpace(NroRegEstadualField))
                {
                    throw new Exception("Não é permitido informar conteúdo da TAG <TAF> e <NroRegEstadual>, filhas da TAG <prop>, ao mesmo tempo, somente uma delas pode ter conteúdo.");
                }

                TAFField = value;
            }
        }

        [XmlElement("NroRegEstadual")]
        public string NroRegEstadual
        {
            get => NroRegEstadualField;
            set
            {
                if (!string.IsNullOrWhiteSpace(TAFField))
                {
                    throw new Exception("Não é permitido informar conteúdo da TAG <TAF> e <NroRegEstadual>, filhas da TAG <prop>, ao mesmo tempo, somente uma delas pode ter conteúdo.");
                }

                NroRegEstadualField = value;
            }
        }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        [XmlElement("tpProp")]
        public TipoProprietarioMDFe TpProp { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
        public bool ShouldSerializeTpProp() => TpProp != TipoProprietarioMDFe.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeOS.InfFretamento")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfFretamento
    {
        [XmlElement("tpFretamento")]
        public TipoFretamentoCTeOS TpFretamento { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeDhViagemField() => DhViagem > DateTime.MinValue;

        #endregion
    }
}
