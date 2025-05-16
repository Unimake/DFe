#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;

namespace Unimake.Business.DFe.Xml.CTeSimp
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.CTeSimp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    [XmlRoot("CTeSimp", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class CTeSimp : XMLBase
    {
        [XmlElement("infCte")]
        public InfCTe InfCTe { get; set; }

        [XmlElement("infCTeSupl")]
        public CTe.InfCTeSupl InfCTeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>
        /// Desserializar o XML no objeto CTeSimp
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto do CTeSimp</returns>
        public CTeSimp LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CTeSimp>(doc);
        }

        /// <summary>
        /// Deserializar o XML CTeSimp no objeto CTeSimp
        /// </summary>
        /// <param name="xml">string do XML CTeSimp</param>
        /// <returns>Objeto da CTeSimp</returns>
        public CTeSimp LoadFromXML(string xml) => XMLUtility.Deserializar<CTeSimp>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.InfCTe")]
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
        public CTe.Emit Emit { get; set; }

        [XmlElement("toma")]
        public Toma Toma { get; set; }

        [XmlElement("infCarga")]
        public CTe.InfCarga InfCarga { get; set; }

        [XmlElement("det")]
        public List<Det> Det { get; set; }

        [XmlElement("infModal")]
        public CTe.InfModal InfModal { get; set; }

        [XmlElement("cobr")]
        public CTe.Cobr Cobr { get; set; }

        [XmlElement("infCteSub")]
        public InfCteSub InfCteSub { get; set; }

        [XmlElement("imp")]
        public CTe.Imp Imp { get; set; }

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("autXML")]
        public List<CTe.AutXML> AutXML { get; set; }

        [XmlElement("infRespTec")]
        public CTe.InfRespTec InfRespTec { get; set; }

        [XmlElement("infSolicNFF")]
        public CTe.InfSolicNFF InfSolicNFF { get; set; }

        /// <summary>
        /// Grupo de informação do Provedor de Assinatura e Autorização
        /// </summary>
        [XmlElement("infPAA")]
        public InfPAA InfPAA { get; set; }

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
                var conteudoChaveDFe = new XMLUtility.ConteudoChaveDFe
                {
                    UFEmissor = (UFBrasil)(int)Ide.CUF,
                    AnoEmissao = Ide.DhEmi.ToString("yy"),
                    MesEmissao = Ide.DhEmi.ToString("MM"),
                    CNPJCPFEmissor = (string.IsNullOrWhiteSpace(Emit.CNPJ) ? Emit.CPF?.PadLeft(14, '0') : Emit.CNPJ.PadLeft(14, '0')),
                    Modelo = (ModeloDFe)(int)Ide.Mod,
                    Serie = Ide.Serie,
                    NumeroDoctoFiscal = Ide.NCT,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    CodigoNumerico = Ide.CCT
                };
                ChaveField = XMLUtility.MontarChaveCTe(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

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
        public void AddAutXML(CTe.AutXML autxml)
        {
            if (AutXML == null)
            {
                AutXML = new List<CTe.AutXML>();
            }

            AutXML.Add(autxml);
        }

        /// <summary>
        /// Retorna o elemento da lista AutXML (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da AutXML</returns>
        public CTe.AutXML GetAutXML(int index)
        {
            if ((AutXML?.Count ?? 0) == 0)
            {
                return default;
            }

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
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.Ide")]
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
                   value == TipoEmissao.ContingenciaSVCAN ||
                   value == TipoEmissao.ContingenciaFSDA)
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
        public TipoCTeSimp TpCTe { get; set; }

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

        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal { get; set; }

        [XmlElement("tpServ")]
        public TipoServicoCTe TpServ { get; set; }

        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

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

        /// <summary>
        /// Grupo de compra governamental
        /// </summary>
        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        public bool ShouldSerializeXDetRetira() => !string.IsNullOrEmpty(XDetRetira);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.GCompraGov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GCompraGov : CTe.GCompraGov { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.Compl")]
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

        [XmlElement("fluxo")]
        public CTe.Fluxo Fluxo { get; set; }

        [XmlElement("xObs")]
        public string XObs { get; set; }

        [XmlElement("ObsCont")]
        public List<CTe.ObsCont> ObsCont { get; set; }

        [XmlElement("ObsFisco")]
        public List<CTe.ObsFisco> ObsFisco { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="obsCont">Elemento</param>
        public void AddObsCont(CTe.ObsCont obsCont)
        {
            if (ObsCont == null)
            {
                ObsCont = new List<CTe.ObsCont>();
            }

            ObsCont.Add(obsCont);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsCont</returns>
        public CTe.ObsCont GetObsCont(int index)
        {
            if ((ObsCont?.Count ?? 0) == 0)
            {
                return default;
            }

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
        public void AddObsFisco(CTe.ObsFisco obsFisco)
        {
            if (ObsFisco == null)
            {
                ObsFisco = new List<CTe.ObsFisco>();
            }

            ObsFisco.Add(obsFisco);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsFisco (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsFisco</returns>
        public CTe.ObsFisco GetObsFisco(int index)
        {
            if ((ObsFisco?.Count ?? 0) == 0)
            {
                return default;
            }

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

        public bool ShouldSerializeXObs() => !string.IsNullOrWhiteSpace(XObs);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.Toma")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Toma
    {
        [XmlElement("toma")]
        public TomadorServicoCTe TomaServico { get; set; }

        [XmlElement("indIEToma")]
        public IndicadorIEDestinatario IndIEToma { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("ISUF")]
        public string ISUF { get; set; }

        [XmlElement("fone")]
        public string Fone { get; set; }

        [XmlElement("enderToma")]
        public CTe.EnderToma EnderToma { get; set; }

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeISUF() => !string.IsNullOrWhiteSpace(ISUF);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.Det")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Det
    {
        [XmlAttribute(AttributeName = "nItem")]
        public string NItem { get; set; }

        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        [XmlElement("xMunIni")]
        public string XMunIni { get; set; }

        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        [XmlElement("xMunFim")]
        public string XMunFim { get; set; }

        [XmlIgnore]
        public double VPrest { get; set; }

        [XmlElement("vPrest")]
        public string VPrestField
        {
            get => VPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VPrest = Utility.Converter.ToDouble(value);
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
        public List<CTe.Comp> Comp { get; set; }

        [XmlElement("infNFe")]
        public List<InfNFe> InfNFe { get; set; }

        /// <summary>
        /// Documentos anteriores
        /// </summary>
        [XmlElement("infDocAnt")]
        public List<InfDocAnt> InfDocAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comp">Elemento</param>
        public void AddComp(CTe.Comp comp)
        {
            if (Comp == null)
            {
                Comp = new List<CTe.Comp>();
            }

            Comp.Add(comp);
        }

        /// <summary>
        /// Retorna o elemento da lista Comp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comp</returns>
        public CTe.Comp GetComp(int index)
        {
            if ((Comp?.Count ?? 0) == 0)
            {
                return default;
            }

            return Comp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comp
        /// </summary>
        public int GetCompCount => (Comp != null ? Comp.Count : 0);

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
            }

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infDocAnt">Elemento</param>
        public void AddInfDocAnt(InfDocAnt infDocAnt)
        {
            if (InfDocAnt == null)
            {
                InfDocAnt = new List<InfDocAnt>();
            }

            InfDocAnt.Add(infDocAnt);
        }

        /// <summary>
        /// Retorna o elemento da lista InfDocAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfDocAnt</returns>
        public InfDocAnt GetInfDocAnt(int index)
        {
            if ((InfDocAnt?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfDocAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfDocAnt
        /// </summary>
        public int GetInfDocAntCount => (InfDocAnt != null ? InfDocAnt.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.InfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNFe
    {
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

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
        public List<CTe.InfUnidCarga> InfUnidCarga { get; set; }

        [XmlElement("infUnidTransp")]
        public List<CTe.InfUnidTransp> InfUnidTransp { get; set; }


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCarga">Elemento</param>
        public void AddInfUnidCarga(CTe.InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<CTe.InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCarga</returns>
        public CTe.InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

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
        public void AddInfUnidTransp(CTe.InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<CTe.InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidTransp</returns>
        public CTe.InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            }

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
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.InfCteSub")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteSub
    {
        [XmlElement("chCte")]
        public string ChCte { get; set; }

        [XmlIgnore]
        public SimNao IndAlteraToma { get; set; }

        [XmlElement("indAlteraToma")]
        public string IndAlteraTomaField
        {
            get => (IndAlteraToma == SimNao.Sim ? "1" : "");
            set => IndAlteraToma = (SimNao)Enum.Parse(typeof(SimNao), value.ToString());
        }

        #region ShouldSerialize

        public bool ShouldSerializeIndAlteraTomaField() => IndAlteraToma == SimNao.Sim;

        #endregion
    }

    /// <summary>
    /// Documentos anteriores
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.InfDocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfDocAnt
    {
        /// <summary>
        /// Chave de acesso do CT-e
        /// </summary>
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }

        /// <summary>
        /// Indica se a prestação é total ou parcial em relação as notas do documento anterior
        /// </summary>
        [XmlElement("tpPrest")]
        public TipoPrestacaoCTe TpPrest { get; set; }

        /// <summary>
        /// Informando o tpPrest com “2 – Parcial” deve-se informar as chaves de acesso das NF-e que acobertam a carga transportada.
        /// </summary>
        [XmlElement("infNFeTranspParcial")]
        public List<InfNFeTranspParcial> InfNFeTranspParcial { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNFeTranspParcial">Elemento</param>
        public void AddInfNFeTranspParcial(InfNFeTranspParcial infNFeTranspParcial)
        {
            if (InfNFeTranspParcial == null)
            {
                InfNFeTranspParcial = new List<InfNFeTranspParcial>();
            }

            InfNFeTranspParcial.Add(infNFeTranspParcial);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFeTranspParcial (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFeTranspParcial</returns>
        public InfNFeTranspParcial GetInfNFeTranspParcial(int index)
        {
            if ((InfNFeTranspParcial?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfNFeTranspParcial[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFeTranspParcial
        /// </summary>
        public int GetInfNFeTranspParcialCount => (InfNFeTranspParcial != null ? InfNFeTranspParcial.Count : 0);

#endif
    }

    /// <summary>
    /// Informando o tpPrest com “2 – Parcial” deve-se informar as chaves de acesso das NF-e que acobertam a carga transportada.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.InfNFeTranspParcial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNFeTranspParcial
    {
        /// <summary>
        /// Informando o tpPrest com “2 – Parcial” deve-se informar as chaves de acesso das NF-e que acobertam a carga transportada.
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }
    }

    /// <summary>
    /// Valores Totais do CTe 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTeSimp.Total")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Total
    {
        /// <summary>
        /// Valor Total da Prestação do Serviço
        /// </summary>
        [XmlIgnore]
        public double VTPrest { get; set; }

        /// <summary>
        /// Propriedade auxiliar da VTPrest (Utilize sempre a VTPrest)
        /// </summary>
        [XmlElement("vTPrest")]
        public string VTPrestField
        {
            get => VTPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VTPrest = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total a Receber
        /// </summary>
        [XmlIgnore]
        public double VTRec { get; set; }

        /// <summary>
        /// Propriedade auxiliar da VRec (Utilize sempre a VRec)
        /// </summary>
        [XmlElement("vTRec")]
        public string VtRecField
        {
            get => VTRec.ToString("F2", CultureInfo.InvariantCulture);
            set => VTRec = Utility.Converter.ToDouble(value);
        }
    }
}