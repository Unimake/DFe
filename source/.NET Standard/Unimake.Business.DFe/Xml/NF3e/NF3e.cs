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

namespace Unimake.Business.DFe.Xml.NF3e
{
    /// <summary>
    /// NF3e - Nota Fiscal da Energia Elétrica Eletrônica
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NF3e")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nf3e")]
    [XmlRoot("NF3e", Namespace = "http://www.portalfiscal.inf.br/nf3e", IsNullable = false)]
    public class NF3e : XMLBase
    {
        [XmlElement("infNF3e")]
        public InfNF3e InfNF3e { get; set; }

        [XmlElement("infNF3eSupl")]
        public InfNF3eSupl InfNF3eSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }
    }

    #region

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.InfNF3e")]
    [ComVisible(true)]
#endif
    public class InfNF3e
    {
        private string IdField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "token")]
        public string Id
        {
            get
            {
                IdField = "NF3e" + Chave;
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

        [XmlElement("acessante")]
        public Acessante Acessante { get; set; }

        [XmlElement("gSub")]
        public GSub GSub { get; set; }

        [XmlElement("gJudic")]
        public GJudic GJudic { get; set; }

        [XmlElement("gGrContrat")]
        public List<GGrContrat> GGrContrats { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="GGrContrat">Elemento</param>
        public void AddGGrContrat(GGrContrat GGrContrat)
        {
            if (GGrContrats == null)
            {
                GGrContrats = new List<GGrContrat>();
            }

            GGrContrats.Add(GGrContrat);
        }

        /// <summary>
        /// Retorna o elemento da lista GGrContrat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GGrContrat</returns>
        public GGrContrat GetGGrContrat(int index)
        {
            if ((GGrContrats?.Count ?? 0) == 0)
            {
                return default;
            };

            return GGrContrats[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GGrContrat
        /// </summary>
        public int GetGGrContratCount => (GGrContrats != null ? GGrContrats.Count : 0);
#endif

        [XmlElement("gMed")]
        public List<GMed> GMed { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGMed(GMed item)
        {
            if (GMed == null)
            {
                GMed = new List<GMed>();
            }

            GMed.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GMed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GMed</returns>
        public GMed GetGMed(int index)
        {
            if ((GMed?.Count ?? 0) == 0)
            {
                return default;
            };

            return GMed[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GMed
        /// </summary>
        public int GetGMedCount => (GMed != null ? GMed.Count : 0);
#endif

        [XmlElement("gSCEE")]
        public GSCEE GSCEE { get; set; }

        [XmlElement("NFdet")]
        public List<NFdet> NFdet { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddNFdet(NFdet item)
        {
            if (NFdet == null)
            {
                NFdet = new List<NFdet>();
            }

            NFdet.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista NFdet (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFdet</returns>
        public NFdet GetNFdet(int index)
        {
            if ((NFdet?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFdet[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFdet
        /// </summary>
        public int GetNFdetCount => (NFdet != null ? NFdet.Count : 0);
#endif

        [XmlElement("total")]
        public Total Total { get; set; }

        [XmlElement("gFat")]
        public GFat GFat { get; set; }

        [XmlElement("gANEEL")]
        public GANEEL GANEEL { get; set; }

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

        [XmlElement("infAdic")]
        public InfAdicNF3e InfAdic { get; set; }

        [XmlElement("gRespTec")]
        public GRespTec GRespTec { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Ide")]
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

        [XmlElement("finNF3e")]
        public FinalidadeNF3e FinNF3e { get; set; }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

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

        public bool ShouldSerializeDhContField() => TpEmis != (TipoEmissao)1;

        public bool ShouldSerializeXJust() => TpEmis != (TipoEmissao)1;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo de compras governamentais
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GCompraGov")]
    [ComVisible(true)]
#endif
    public class GCompraGov
    {
        /// <summary>
        /// Tipo de ente governamental
        /// </summary>
        [XmlElement("tpEnteGov")]
        public TipoEnteGovernamental TpEnteGov { get; set; }

        /// <summary>
        /// Percentual de redução de alíquota em compra governamental
        /// </summary>
        [XmlIgnore]
        public double PRedutor { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedutor para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedutor")]
        public string PRedutorField
        {
            get => PRedutor.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedutor = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Emit")]
    [ComVisible(true)]
#endif
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EnderEmit")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Dest")]
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

        [XmlElement("cNIS")]
        public string CNIS { get; set; }

        [XmlElement("NB")]
        public string NB { get; set; }

        [XmlElement("xNomeAdicional")]
        public string XNomeAdicional { get; set; }

        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        #region ShoulSerialize 

        public bool ShouldSerializeIE() => !string.IsNullOrEmpty(IE);

        public bool ShouldSerializeIM() => !string.IsNullOrEmpty(IM);
        
        public bool ShouldSerializeCNIS() => !string.IsNullOrEmpty(CNIS);
        
        public bool ShouldSerializeNB() => !string.IsNullOrEmpty(NB);
        
        public bool ShouldSerializeXNomeAdicional() => !string.IsNullOrEmpty(XNomeAdicional);

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EnderDest")]
    [ComVisible(true)]
#endif
    public class EnderDest : EnderEmit { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Acessante")]
    [ComVisible(true)]
#endif
    public class Acessante
    {
        [XmlElement("idAcesso")]
        public string IdAcesso { get; set; }

        [XmlElement("idCodCliente")]
        public string IdCodCliente { get; set; }

        [XmlElement("tpAcesso")]
        public TipoAcessante TpAcesso { get; set; }

        [XmlElement("xNomeUC")]
        public string XNomeUC { get; set; }

        [XmlElement("tpClasse")]
#if INTEROP
        public TipoClasseConsumidora TpClasse { get; set; } = (TipoClasseConsumidora)(-1);
#else
        public TipoClasseConsumidora? TpClasse { get; set; }
#endif

        [XmlElement("tpSubClasse")]
#if INTEROP
        public TipoSubClasseConsumidora TpSubClasse { get; set; } = (TipoSubClasseConsumidora)(-1);
#else
        public TipoSubClasseConsumidora? TpSubClasse { get; set; }
#endif

        [XmlElement("tpFase")]
        public TipoLigacao TpFase { get; set; }

        [XmlElement("tpGrpTensao")]
        public GrupoSubGrupoTensao TpGrpTensao { get; set; }

        [XmlElement("tpModTar")]
        public ModalidadeTarifaria TpModTar { get; set; }

        [XmlElement("latGPS")]
        public string LatGPS { get; set; }

        [XmlElement("longGPS")]
        public string LongGPS { get; set; }

        [XmlElement("codRoteiroLeitura")]
        public string CodRoteiroLeitura { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIdCodCliente() => !string.IsNullOrWhiteSpace(IdCodCliente);

        public bool ShouldSerializeXNomeUC() => !string.IsNullOrWhiteSpace(XNomeUC);

#if INTEROP
        public bool ShouldSerializeTpClasse() => TpClasse != (TipoClasseConsumidora)(-1);
#else
        public bool ShouldSerializeTpClasse() => TpClasse != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpSubClasse() => TpSubClasse != (TipoSubClasseConsumidora)(-1);
#else
        public bool ShouldSerializeTpSubClasse() => TpSubClasse != null;
#endif

        public bool ShouldSerializeCodRoteiroLeitura() => !string.IsNullOrWhiteSpace(CodRoteiroLeitura);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GSub")]
    [ComVisible(true)]
#endif
    public class GSub
    {
        [XmlElement("chNF3e")]
        public string ChNF3e { get; set; }

        [XmlElement("gNF")]
        public GNF GNF { get; set; }

        [XmlElement("motSub")]
        public MotivoSubstituicao MotSub { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GNF")]
    [ComVisible(true)]
#endif
    public class GNF
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nNF")]
        public string NNF { get; set; }

        [XmlElement("competEmis")]
        public string CompetEmis { get; set; }

        [XmlElement("competApur")]
        public string CompetApur { get; set; }

        [XmlElement("hash115")]
        public string Hash115 { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeHash115() => !string.IsNullOrEmpty(Hash115);

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GJudic")]
    [ComVisible(true)]
#endif
    public class GJudic
    {
        [XmlElement("chNF3e")]
        public string ChNF3e { get; set; }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GGrContrat")]
    [ComVisible(true)]
#endif
    public class GGrContrat
    {
        [XmlAttribute(AttributeName = "nContrat", DataType = "string")]
        public string NContrat { get; set; }

        [XmlElement("tpGrContrat")]
        public TipoGrandezaContratada TpGrContrat { get; set; }

        [XmlElement("tpPosTar")]
        public TipoPostoTarifario TpPosTar { get; set; }

        [XmlElement("qUnidContrat")]
        public decimal QUnidContrat { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GMed")]
    [ComVisible(true)]
#endif
    public class GMed
    {
        [XmlAttribute(AttributeName = "nMed", DataType = "token")]
        public string NMed { get; set; }

        [XmlElement("idMedidor")]
        public string IdMedidor { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DMedAnt { get; set; }
#else
        public DateTimeOffset DMedAnt { get; set; }
#endif

        [XmlElement("dMedAnt")]
        public string DMedAntField
        {
            get => DMedAnt.ToString("yyyy-MM-dd");
#if INTEROP
            set => DMedAnt = DateTime.Parse(value);
#else
            set => DMedAnt = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DMedAtu { get; set; }
#else
        public DateTimeOffset DMedAtu { get; set; }
#endif

        [XmlElement("dMedAtu")]
        public string DMedAtuField
        {
            get => DMedAtu.ToString("yyyy-MM-dd");
#if INTEROP
            set => DMedAtu = DateTime.Parse(value);
#else
            set => DMedAtu = DateTimeOffset.Parse(value);
#endif
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GSCEE")]
    [ComVisible(true)]
#endif
    public class GSCEE
    {
        [XmlElement("tpPartComp")]
        public TipoParticipacaoCompensacao TpPartComp { get; set; }

        [XmlElement("gConsumidor")]
        public List<GConsumidor> GConsumidor { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGConsumidor(GConsumidor item)
        {
            if (GConsumidor == null)
            {
                GConsumidor = new List<GConsumidor>();
            }

            GConsumidor.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GConsumidor (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GConsumidor</returns>
        public GConsumidor GetGConsumidor(int index)
        {
            if ((GConsumidor?.Count ?? 0) == 0)
            {
                return default;
            };

            return GConsumidor[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GConsumidor
        /// </summary>
        public int GetGConsumidorCount => (GConsumidor != null ? GConsumidor.Count : 0);
#endif

        [XmlElement("gSaldoCred")]
        public List<GSaldoCred> GSaldoCred { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGSaldoCred(GSaldoCred item)
        {
            if (GSaldoCred == null)
            {
                GSaldoCred = new List<GSaldoCred>();
            }

            GSaldoCred.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GSaldoCred (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GSaldoCred</returns>
        public GSaldoCred GetGSaldoCred(int index)
        {
            if ((GSaldoCred?.Count ?? 0) == 0)
            {
                return default;
            };

            return GSaldoCred[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GSaldoCred
        /// </summary>
        public int GetGSaldoCredCount => (GSaldoCred != null ? GSaldoCred.Count : 0);
#endif

        [XmlElement("gTipoSaldo")]
        public List<GTipoSaldo> GTipoSaldo { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGTipoSaldo(GTipoSaldo item)
        {
            if (GTipoSaldo == null)
            {
                GTipoSaldo = new List<GTipoSaldo>();
            }

            GTipoSaldo.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GTipoSaldo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GTipoSaldo</returns>
        public GTipoSaldo GetGTipoSaldo(int index)
        {
            if ((GTipoSaldo?.Count ?? 0) == 0)
            {
                return default;
            };

            return GTipoSaldo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GTipoSaldo
        /// </summary>
        public int GetGTipoSaldoCount => (GTipoSaldo != null ? GTipoSaldo.Count : 0);
#endif

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GConsumidor")]
    [ComVisible(true)]
#endif
    public class GConsumidor
    {
        [XmlElement("idAcessGer")]
        public string IdAcessGer { get; set; }

        [XmlIgnore]
        public double VPotInst { get; set; }

        [XmlElement("vPotInst")]
        public string VPotInstField
        {
            get => VPotInst.ToString("F3", CultureInfo.InvariantCulture);
            set => VPotInst = Converter.ToDouble(value);
        }

        [XmlElement("tpFonteEnergia")]
        public TipoFonteEnergia TpFonteEnergia { get; set; }

        [XmlIgnore]
        public double EnerAloc { get; set; }

        [XmlElement("enerAloc")]
        public string EnerAlocField
        {
            get => EnerAloc.ToString("F3", CultureInfo.InvariantCulture);
            set => EnerAloc = Converter.ToDouble(value);
                }

        [XmlElement("tpPosTar")]
        public TipoPostoTarifario TpPosTar { get; set; }

        [XmlIgnore]
        public double EnerInjet { get; set; }

        [XmlElement("enerInjet")]
        public string EnerInjetField
        {
            get => EnerInjet.ToString("F3", CultureInfo.InvariantCulture);
            set => EnerInjet = Converter.ToDouble(value);
        }

        [XmlElement("tpPosTarInjet")]
#if INTEROP
        public TipoPostoTarifario TpPosTarInjet { get; set; } = (TipoPostoTarifario)(-1);
#else
        public TipoPostoTarifario? TpPosTarInjet { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeEnerInjetField() => EnerInjet > 0;

#if INTEROP
        public bool ShouldSerializeTpPosTarInjet() => TpPosTarInjet != (TipoPostoTarifario)(-1);
#else
        public bool ShouldSerializeTpPosTarInjet() => TpPosTarInjet != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GSaldoCred")]
    [ComVisible(true)]
#endif
    public class GSaldoCred
    {
        [XmlElement("tpPosTar")]
        public TipoPostoTarifario TpPosTar { get; set; }

        [XmlIgnore]
        public double VSaldAnt { get; set; }

        [XmlElement("vSaldAnt")]
        public string VSaldAntField
        {
            get => VSaldAnt.ToString("F3", CultureInfo.InvariantCulture);
            set => VSaldAnt = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredExpirado { get; set; }

        [XmlElement("vCredExpirado")]
        public string VCredExpiradoField
        {
            get => VCredExpirado.ToString("F3", CultureInfo.InvariantCulture);
            set => VCredExpirado = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VSaldAtual { get; set; }

        [XmlElement("vSaldAtual")]
        public string VSaldAtualField
        {
            get => VSaldAtual.ToString("F3", CultureInfo.InvariantCulture);
            set => VSaldAtual = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCredExpirar { get; set; }

        [XmlElement("vCredExpirar")]
        public string VCredExpirarField
        {
            get => VCredExpirar.ToString("F3", CultureInfo.InvariantCulture);
            set => VCredExpirar = Converter.ToDouble(value);
        }

        [XmlElement("CompetExpirar")]
        public string CompetExpirar { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVCredExpirarField() => VCredExpirar > 0;

        public bool ShouldSerializeCompetExpirar() => !string.IsNullOrEmpty(CompetExpirar);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GTipoSaldo")]
    [ComVisible(true)]
#endif
    public class GTipoSaldo
    {
        [XmlElement("gSaldoCred")]
        public List<GSaldoCred> GSaldoCred { get; set; }

        /// <summary>
        /// Número referente ao agrupador de saldos por posto tarifário.
        /// Aceita valores de 01 a 10. Serve para organizar a informação dos grupos de saldos.
        /// </summary>
        [XmlAttribute(AttributeName = "nTipoSaldo", DataType = "string")]
        public string NTipoSaldo { get; set; }
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.NFdet")]
    [ComVisible(true)]
#endif
    public class NFdet
    {
        [XmlAttribute(AttributeName = "chNF3eAnt", DataType = "token")]
        public string ChNF3eAnt { get; set; }

        [XmlAttribute(AttributeName = "mod6HashAnt", DataType = "token")]
        public string Mod6HashAnt { get; set; }

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
        /// Retorna o elemento da lista DetNF3e (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetNF3e</returns>
        public Det GetDetItem(int index)
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
        public int GetDetCount=> (Det != null ? Det.Count : 0);
#endif

        #region ShouldSerialize

        public bool ShouldSerializeMod6HashAnt() => !string.IsNullOrEmpty(Mod6HashAnt);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Det")]
    [ComVisible(true)]
#endif
    public class Det
    {
        [XmlAttribute(AttributeName = "nItem", DataType = "token")]
        public string NItem { get; set; }

        [XmlElement("gAjusteNF3eAnt")]
        public GAjusteNF3eAnt GAjusteNF3eAnt { get; set; }

        /// <summary>
        /// Grupo de informações referentes a item da NF-3e anterior
        /// Este grupo somente deverá ser preenchido para detalhar item ajustado de nota anterior nos casos de item alterado ou excluído
        /// </summary>
        [XmlElement("detItemAnt")]
        public DetItemAnt DetItemAnt { get; set; }

        [XmlElement("detItem")]
        public DetItem DetItem { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GAjusteNF3eAnt")]
    [ComVisible(true)]
#endif
    public class GAjusteNF3eAnt
    {
        [XmlElement("tpAjuste")]
        public TipoAjusteNF3e TpAjuste { get; set; }

        [XmlElement("motAjuste")]
        public MotivoAjuste MotAjuste { get; set; }
    }

    /// <summary>
    /// Grupo de informações referentes a item da NF-3e anterior
    /// Este grupo somente deverá ser preenchido para detalhar item ajustado de nota anterior nos casos de item alterado ou excluído
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.DetItemAnt")]
    [ComVisible(true)]
#endif
    public class DetItemAnt
    {
        [XmlAttribute("nItemAnt", DataType = "token")]
        public string NItemAnt { get; set; }

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

        /// <summary>
        /// Código de classificação
        /// Tabela de Classificação de Item da NF-3e de Energia Elétrica (validar por RV)
        /// </summary>
        [XmlElement("cClass")]
        public string CClass { get; set; }

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
        public double VFCP { get; set; }

        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
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
        public double VPISEfet { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfetField
        {
            get => VPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VPISEfet = Converter.ToDouble(value);
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
        public double VCOFINSEfet { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfetField
        {
            get => VCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINSEfet = Converter.ToDouble(value);
        }

        [XmlElement("retTrib")]
        public RetTribNF3e RetTribNF3e { get; set; }

        [XmlElement("indDevolucao")]
#if INTEROP
        public SimNao IndDevolucao { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDevolucao { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializePICMSField() => PICMS > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        public bool ShouldSerializeVBCSTField() => VBCST > 0;

        public bool ShouldSerializeVICMSSTField() => VICMSST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVPISField() => VPIS > 0;

        public bool ShouldSerializeVPISEfetField() => VPISEfet > 0;

        public bool ShouldSerializeVCOFINSField() => VCOFINS > 0;

        public bool ShouldSerializeVCOFINSEfetField() => VCOFINSEfet > 0;

#if INTEROP
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.DetItem")]
    [ComVisible(true)]
#endif
    public class DetItem
    {
        [XmlAttribute(AttributeName = "nItemAnt", DataType = "token")]
        public string NItemAnt { get; set; }

        /// <summary>
        /// Grupo de Tarifas por Período
        /// </summary>
        [XmlElement("gTarif")]
        public GTarif GTarif { get; set; }

        /// <summary>
        /// Grupo de Adicional de Bandeira
        /// </summary>
        [XmlElement("gAdBand")]
        public GAdBand GAdBand { get; set; }

        [XmlElement("prod")]
        public Prod Prod { get; set; }

        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        [XmlElement("gProcRef")]
        public GProcRef GProcRef { get; set; }

        [XmlElement("gContab")]
        public List<GContab> GContab { get; set; }

        [XmlElement("infAdProd")]
        public string InfAdProd { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeInfAdProd() => !string.IsNullOrWhiteSpace(InfAdProd);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GTarif")]
    [ComVisible(true)]
#endif
    public class GTarif
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DIniTarif { get; set; }
#else
        public DateTimeOffset DIniTarif { get; set; }
#endif

        [XmlElement("dIniTarif")]
        public string DIniTarifField
        {
            get => DIniTarif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DIniTarif = DateTime.Parse(value);
#else
            set => DIniTarif = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DFimTarif { get; set; }
#else
        public DateTimeOffset DFimTarif { get; set; }
#endif

        [XmlElement("dFimTarif")]
        public string DFimTarifField
        {
            get => DFimTarif.ToString("yyyy-MM-dd");
#if INTEROP
            set => DFimTarif = DateTime.Parse(value);
#else
            set => DFimTarif = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpAto")]
        public TipoAto TpAto { get; set; }

        [XmlElement("nAto")]
        public string NAto { get; set; }

        [XmlElement("anoAto")]
        public string AnoAto { get; set; }

        [XmlElement("tpTarif")]
        public TipoTarifa TpTarif { get; set; }

        [XmlElement("cPosTarif")]
        public TipoPostoTarifario CPosTarif { get; set; }

        [XmlElement("uMed")]
        public UnidadeMedidaEnergia UMed { get; set; }

        [XmlElement("vTarifHom")]
        public decimal VTarifHom { get; set; }

        [XmlElement("vTarifAplic")]
        public decimal VTarifAplic { get; set; }

        [XmlElement("motDifTarif")]
#if INTEROP
        public MotivoTarifaDiferente MotDifTarif {  get; set; } = (MotivoTarifaDiferente)(-1);
#else
        public MotivoTarifaDiferente? MotDifTarif { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVTarifAplic() => VTarifAplic > 0;

#if INTEROP
        public bool ShouldSerializeMotDifTarif() => MotDifTarif != (MotivoTarifaDiferente)(-1);
#else
        public bool ShouldSerializeMotDifTarif() => MotDifTarif != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GAdBand")]
    [ComVisible(true)]
#endif
    public class GAdBand
    {
        [XmlIgnore]
#if INTEROP
        public DateTime DIniAdBand { get; set; }
#else
        public DateTimeOffset DIniAdBand { get; set; }
#endif

        [XmlElement("dIniAdBand")]
        public string DIniAdBandField
        {
            get => DIniAdBand.ToString("yyyy-MM-dd");
#if INTEROP
            set => DIniAdBand = DateTime.Parse(value);
#else
            set => DIniAdBand = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DFimAdBand { get; set; }
#else
        public DateTimeOffset DFimAdBand { get; set; }
#endif

        [XmlElement("dFimAdBand")]
        public string DFimAdBandField
        {
            get => DFimAdBand.ToString("yyyy-MM-dd");
#if INTEROP
            set => DFimAdBand = DateTime.Parse(value);
#else
            set => DFimAdBand = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("tpBand")]
        public TipoBandeira TpBand { get; set; }

        [XmlElement("vAdBand")]
        public decimal VAdBand { get; set; }

        [XmlElement("vAdBandAplic")]
        public double VAdBandAplic { get; set; }

        [XmlElement("motDifBand")]
#if INTEROP
        public MotivoTarifaDiferente MotDifBand {  get; set; } = (MotivoTarifaDiferente)(-1);
#else
        public MotivoTarifaDiferente? MotDifBand { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVAdBandAplic() => VAdBandAplic > 0;

#if INTEROP
        public bool ShouldSerializeMotDifBand() => MotDifBand != (MotivoTarifaDiferente)(-1);
#else
        public bool ShouldSerializeMotDifBand() => MotDifBand != null;
#endif

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Prod")]
    [ComVisible(true)]
#endif
    public class Prod
    {
        [XmlElement("indOrigemQtd")]
        public IndicadorOrigemQuantidadeFaturada IndOrigemQtd { get; set; }

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
        public UnidadeMedidaEnergia UMed { get; set; }

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

        [XmlElement("vProd")]
        public decimal VProd { get; set; }

        [XmlElement("indDevolucao")]
#if INTEROP
        public SimNao IndDevolucao { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDevolucao { get; set; }
#endif

        [XmlElement("indPrecoACL")]
#if INTEROP
        public SimNao IndPrecoACL { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndPrecoACL { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCFOP() => !string.IsNullOrEmpty(CFOP);

#if INTEROP
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != null;
#endif

#if INTEROP
        public bool ShouldSerializeIndPrecoACL () => IndPrecoACL  != (SimNao)(-1);
#else
        public bool ShouldSerializeIndPrecoACL() => IndPrecoACL != null;
#endif

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GMedicao")]
    [ComVisible(true)]
#endif
    public class GMedicao
    {
        [XmlElement("nMed")]
        public string NMed { get; set; }

        [XmlElement("nContrat")]
        public string NContrat { get; set; }

        [XmlElement("gMedida")]
        public GMedida GMedida { get; set; }

        [XmlElement("tpMotNaoLeitura")]
#if INTEROP
        public TipoMotivoNaoLeitura TpMotNaoLeitura { get; set; } = (TipoMotivoNaoLeitura)(-1);
#else
        public TipoMotivoNaoLeitura? TpMotNaoLeitura { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeNContrat() => !string.IsNullOrEmpty(NContrat);

#if INTEROP
        public bool ShouldSerializeTpMotNaoLeitura() => TpMotNaoLeitura != (TipoMotivoNaoLeitura)(-1);
#else
        public bool ShouldSerializeTpMotNaoLeitura() => TpMotNaoLeitura != null;
#endif

        #endregion ShouldSerialize

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GMedida")]
    [ComVisible(true)]
#endif
    public class GMedida
    {
        [XmlElement("tpGrMed")]
        public TipoGrandezaMedida TpGrMed { get; set; }

        [XmlElement("cPosTarif")]
        public TipoPostoTarifario CPosTarif { get; set; }

        [XmlElement("uMed")]
        public UnidadeMedidaEnergia UMed { get; set; }

        [XmlIgnore]
        public double VMedAnt { get; set; }

        [XmlElement("vMedAnt")]
        public string VMedAntField
        {
            get => VMedAnt.ToString("F4", CultureInfo.InvariantCulture);
            set => VMedAnt = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VMedAtu { get; set; }

        [XmlElement("vMedAtu")]
        public string VMedAtuField
        {
            get => VMedAtu.ToString("F4", CultureInfo.InvariantCulture);
            set => VMedAtu = Converter.ToDouble(value);
        }

        [XmlElement("vConst")]
        public decimal VConst { get; set; }

        [XmlIgnore]
        public double VMed { get; set; }

        [XmlElement("vMed")]
        public string VMedField
        {
            get => VMed.ToString("F4", CultureInfo.InvariantCulture);
            set => VMed = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PPerdaTran { get; set; }

        [XmlElement("pPerdaTran")]
        public string PPerdaTranField
        {
            get => PPerdaTran.ToString("F2", CultureInfo.InvariantCulture);
            set => PPerdaTran = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VMedPerdaTran { get; set; }

        [XmlElement("vMedPerdaTran")]
        public string VMedPerdaTranField
        {
            get => VMedPerdaTran.ToString("F4", CultureInfo.InvariantCulture);
            set => VMedPerdaTran = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VMedPerdaTec { get; set; }

        [XmlElement("vMedPerdaTec")]
        public string VMedPerdaTecField
        {
            get => VMedPerdaTec.ToString("F4", CultureInfo.InvariantCulture);
            set => VMedPerdaTec = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVMedPerdaTecField() => VMedPerdaTec > 0;

        #endregion ShouldSerialize
    }


#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Imposto")]
    [ComVisible(true)]
#endif
    public class Imposto
    {
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

        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        [XmlElement("indSemCST")]
#if INTEROP
        public SimNao IndSemCST { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndSemCST { get; set; }
#endif

        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        [XmlElement("PISEfet")]
        public PISEfet PISEfet { get; set; }

        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        [XmlElement("COFINSEfet")]
        public COFINSEfet COFINSEfet { get; set; }

        [XmlElement("retTrib")]
        public RetTribNF3e RetTrib { get; set; }

        /// <summary>
        /// Grupo de informações da Tributação IBS/CBS
        /// </summary>
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS00")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS10")]
    [ComVisible(true)]
#endif
    public class ICMS10
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;
        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS20")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS40")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS51")]
    [ComVisible(true)]
#endif
    public class ICMS51 : ICMS40 { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS60")]
    [ComVisible(true)]
#endif
    public class ICMS60
    {
        [XmlElement("CST")]
        public string CST { get; set; }

        [XmlIgnore]
        public double VBCSTRet { get; set; }

        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSSTRet { get; set; }

        [XmlElement("pICMSSTRet")]
        public string PICMSSTRetField
        {
            get => PICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSubstituto { get; set; }

        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSSTRet { get; set; }

        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VBCEfet { get; set; }

        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSEfet { get; set; }

        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSEfet { get; set; }

        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
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

        #region ShouldSerialize

        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto > 0;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0;

        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0;

        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeCBenef() => !string.IsNullOrEmpty(CBenef);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMS90")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.PIS")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.PISEfet")]
    [ComVisible(true)]
#endif
    public class PISEfet
    {
        [XmlIgnore]
        public double VBCPISEfet { get; set; }

        [XmlElement("vBCPISEfet")]
        public string VBCPISEfetField
        {
            get => VBCPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCPISEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PPISEfet { get; set; }

        [XmlElement("pPISEfet")]
        public string PPISEfetField
        {
            get => PPISEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PPISEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VPISEfet { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfetField
        {
            get => VPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VPISEfet = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.COFINS")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.COFINSEfet")]
    [ComVisible(true)]
#endif
    public class COFINSEfet
    {
        [XmlIgnore]
        public double VBCCOFINSEfet { get; set; }

        [XmlElement("vBCCOFINSEfet")]
        public string VBCCOFINSEfetField
        {
            get => VBCCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCCOFINSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PCOFINSEfet { get; set; }

        [XmlElement("pCOFINSEfet")]
        public string PCOFINSEfetField
        {
            get => PCOFINSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINSEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINSEfet { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfetField
        {
            get => VCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINSEfet = Converter.ToDouble(value);
        }

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.RetTribNF3e")]
    [ComVisible(true)]
#endif
    public class RetTribNF3e
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

    /// <summary>
    /// Grupo de informações da Tributação IBS/CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.IBSCBS")]
    [ComVisible(true)]
#endif
    public class IBSCBS
    {
        /// <summary>
        /// Código da Situação Tributária do IBS/CBS
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Código da Classificação Tributária do IBS/CBS
        /// </summary>
        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        /// <summary>
        /// Grupo de informações específicas do IBS/CBS
        /// </summary>
        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }
    }

    /// <summary>
    /// Grupo de informações específicas do IBS/CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSCBS")]
    [ComVisible(true)]
#endif
    public class GIBSCBS
    {
        /// <summary>
        /// Valor da Base de cálculo comum a IBS/CBS
        /// </summary>
        [XmlIgnore]
        public double VBC {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de informações do IBS/CBS de competência das Unidades Federadas
        /// </summary>
        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF {  get; set; }

        /// <summary>
        /// Grupo de informações do IBS/CBS de competência do municipio
        /// </summary>
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

        /// <summary>
        /// Grupo de informações da CBS
        /// </summary>
        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        /// <summary>
        /// Grupo de informações da Tributação Regular. 
        /// Informar como seria a tributação caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        /// <summary>
        /// Grupo de Informações do Crédito Presumido, quando aproveitado pelo emitente do documento.
        /// </summary>
        [XmlElement("gIBSCredPres")]
        public GIBSCredPres GIBSCredPres { get; set; }

        /// <summary>
        /// Grupo de Informações do Crédito Presumido, quando aproveitado pelo emitente do documento.
        /// </summary>
        [XmlElement("gCBSCredPres")]
        public GCBSCredPres GCBSCredPres { get; set; }

        /// <summary>
        /// Grupo de informações da composição do valor do IBS e da CBS em compras governamental
        /// </summary>
        [XmlElement("gTribCompraGov")]
        public GTribCompraGov GTribCompraGov { get; set; }
    }

    /// <summary>
    /// Grupo de informações do IBS/CBS de competência das Unidades Federadas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSUF")]
    [ComVisible(true)]
#endif
    public class GIBSUF
    {
        /// <summary>
        /// Alíquota do IBS Estadual 
        /// </summary>
        [XmlIgnore]
        public double PIBSUF {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de campos do diferimento 
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib {  get; set; }

        /// <summary>
        /// Grupo de informações da redução de Alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor do IBS de competência da UF
        /// </summary>
        [XmlIgnore]
        public double VIBSUF {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de campos do diferimento 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GDif")]
    [ComVisible(true)]
#endif
    public class GDif
    {
        /// <summary>
        /// Percentual de diferimento
        /// </summary>
        [XmlIgnore]
        public double PDif {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do diferimento
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da devolução de tributos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GDevTrib")]
    [ComVisible(true)]
#endif
    public class GDevTrib
    {
        /// <summary>
        /// Valor do tributo devolvido. No fornecimento de energia elétrica, água, esgoto e gás natural e em outras hipóteses
        /// definidas no regulamento
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDevTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da redução de Alíquota
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GRed")]
    [ComVisible(true)]
#endif
    public class GRed
    {
        /// <summary>
        /// Percentual da redução de Alíquota do cClassTrib
        /// </summary>
        [XmlIgnore]
        public double PRedAliq { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedAliq para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedAliq")]
        public string PRedAliqField
        {
            get => PRedAliq.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedAliq = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota efetiva do IBS de competência das UF que será aplicada a base de cálculo, 
        /// incluindo ogCompraGov/pRedutor, se houver pAliqEfet = pIBSUF * (1 – pRedAliq) * (1 - gCompraGov / pRedutor)
        /// </summary>
        [XmlIgnore]
        public double PAliqEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfet")]
        public string PAliqEfetField
        {
            get => PAliqEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfet = Converter.ToDouble(value);
        }
    }


    /// <summary>
    /// Grupo de informações do IBS/CBS de competência do municipio
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSMun")]
    [ComVisible(true)]
#endif
    public class GIBSMun
    {
        /// <summary>
        /// Alíquota do IBS Municipal 
        /// </summary>
        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de campos do diferimento 
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        /// <summary>
        /// Grupo de informações da redução de Alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor do IBS de competência do município
        /// </summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GCBS")]
    [ComVisible(true)]
#endif
    public class GCBS
    {
        /// <summary>
        /// Alíquota da CBS
        /// </summary>
        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => PCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de campos do diferimento 
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        /// <summary>
        /// Grupo de informações da redução de Alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor da CBS 
        /// </summary>
        [XmlIgnore]
        public double VCBS {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da Tributação Regular. 
    /// Informar como seria a tributação caso não cumprida a condição resolutória/suspensiva
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GTribRegular")]
    [ComVisible(true)]
#endif
    public class GTribRegular
    {
        /// <summary>
        /// Código da Situação Tributária.
        /// Informado como seria o CST caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlElement("CSTReg")]
        public string CSTReg {  get; set; }

        /// <summary>
        /// Código de Classificação Tributária.
        /// Informado como seria o cClassTrib caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlElement("cClassTribReg")]
        public string CClassTribReg {  get; set; }

        /// <summary>
        /// Alíquota efetiva da UF.
        /// Informado a Alíquota caso não cumprida a resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqEfetRegIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegIBSUF")]
        public string PAliqEfetRegIBSUFField
        {
            get => PAliqEfetRegIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informado como seria o valor do Tributo da UF caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double VTribRegIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribRegIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegIBSUF")]
        public string VTribRegIBSUFField
        {
            get => VTribRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota efetiva do Município.
        /// Informado a Alíquota caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegIBSMun {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqEfetRegIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegIBSMun")]
        public string PAliqEfetRegIBSMunField
        {
            get => PAliqEfetRegIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informado como seria o valor do Tributo do Município caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double VTribRegIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribRegIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegIBSMun")]
        public string VTribRegIBSMunField
        {
            get => VTribRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota efetiva da CBS.
        /// Informado a Alíquota caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegCBS {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqEfetRegCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBSField
        {
            get => PAliqEfetRegCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informado como seria o valor do Tributo CBS caso não cumprida a condição resolutória/suspensiva
        /// </summary>
        [XmlIgnore]
        public double VTribRegCBS {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribRegCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => VTribRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações do Crédito Presumido, quando aproveitado pelo emitente do documento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSCredPres")]
    [ComVisible(true)]
#endif
    public class GIBSCredPres
    {
        /// <summary>
        /// Código do Crédito Presumido 
        /// </summary>
        [XmlElement("cCredPres")]
        public string CCredPres {  get; set; }

        /// <summary>
        /// Percentual de crédito presumido
        /// </summary>
        [XmlIgnore]
        public double PCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido Condição Suspensiva.
        /// Preencher apenas para cCredPres com indicação de Condição Suspensiva
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVCredPresField() => VCredPresCondSus <= 0;

        public bool ShouldSerializeVCredPresCondSusField() => VCredPres <= 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo de Informações do Crédito Presumido, quando aproveitado pelo emitente do documento.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GCBSCredPres")]
    [ComVisible(true)]
#endif
    public class GCBSCredPres
    {
        /// <summary>
        /// Código do Crédito Presumido 
        /// </summary>
        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        /// <summary>
        /// Percentual de crédito presumido
        /// </summary>
        [XmlIgnore]
        public double PCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido Condição Suspensiva.
        /// Preencher apenas para cCredPres com indicação de Condição Suspensiva
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVCredPresField() => VCredPresCondSus <= 0;

        public bool ShouldSerializeVCredPresCondSusField() => VCredPres <= 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Grupo de informações da composição do valor do IBS e da CBS em compras governamental
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GTribCompraGov")]
    [ComVisible(true)]
#endif
    public class GTribCompraGov
    {
        /// <summary>
        /// Alíquota IBS da UF utilizada
        /// </summary>
        [XmlIgnore]
        public double PAliqIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqIBSUF")]
        public string PAliqIBSUFField
        {
            get => PAliqIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo do IBS da UF
        /// </summary>
        [XmlIgnore]
        public double VTribIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribIBSUF")]
        public string VTribIBSUFField
        {
            get => VTribIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota IBS do Município utilizada
        /// </summary>
        [XmlIgnore]
        public double PAliqIBSMun {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqIBSMun")]
        public string PAliqIBSMunField
        {
            get => PAliqIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo do Município da UF
        /// </summary>
        [XmlIgnore]
        public double VTribIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribIBSMun")]
        public string VTribIBSMunField
        {
            get => VTribIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota IBS do CBS utilizada
        /// </summary>
        [XmlIgnore]
        public double PAliqCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PAliqCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqCBS")]
        public string PAliqCBSField
        {
            get => PAliqCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo da CBS
        /// </summary>
        [XmlIgnore]
        public double VTribCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTribCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribCBS")]
        public string VTribCBSField
        {
            get => VTribCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribCBS = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.Total")]
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

        [XmlElement("vRetTribTot")]
        public VRetTribTot VRetTribTot { get; set; }

        [XmlIgnore]
        public double VCOFINS { get; set; }

        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCOFINSEfet { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfetField
        {
            get => VCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINSEfet = Converter.ToDouble(value);
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
        public double VPISEfet { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfetField
        {
            get => VPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VPISEfet = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VNF { get; set; }

        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de informações para apuração do IBS e CBS declarado pelo contribuinte
        /// </summary>
        [XmlElement("IBSCBSTot")]
        public IBSCBSTot IBSCBSTot {  get; set; }

        /// <summary>
        /// Valor total do documento fiscal 
        /// vNF + total do IBS + total da CBS
        /// </summary>
        [XmlIgnore]
        public double VTotDFe {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTotDFe para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => VTotDFe.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDFe = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.ICMSTot")]
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

        [XmlIgnore]
        public double VBCST { get; set; }
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VST { get; set; }
        [XmlElement("vST")]
        public string VSTField
        {
            get => VST.ToString("F2", CultureInfo.InvariantCulture);
            set => VST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.VRetTribTot")]
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

    /// <summary>
    /// Grupo de informações para apuração do IBS e CBS declarado pelo contribuinte
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.IBSCBSTot")]
    [ComVisible(true)]
#endif
    public class IBSCBSTot
    {
        /// <summary>
        /// Total da Base de cálculo do IBS/CBS
        /// </summary>
        [XmlIgnore]
        public double VBCIBSCBS {  get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCIBSCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBSField
        {
            get => VBCIBSCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIBSCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Totalização do IBS
        /// </summary>
        [XmlElement("gIBS")]
        public GIBS GIBS { get; set; }

        /// <summary>
        /// Total do CBS
        /// </summary>
        [XmlElement("gCBS")]
        public GCBSTot GCBS { get; set; } 
    }

    /// <summary>
    /// Totalização do IBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBS")]
    [ComVisible(true)]
#endif
    public class GIBS
    {
        /// <summary>
        /// Totalização do IBS de competência da UF
        /// </summary>
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUF { get; set; }

        /// <summary>
        /// Totalização do IBS de competência Municipal
        /// </summary>
        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMun { get; set; }

        /// <summary>
        /// Total do IBS (IBS UF + IBS Mun)
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

        /// <summary>
        /// Total do Crédito Presumido
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do Crédito Presumido Condição Suspensiva
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Totalização do IBS de competência da UF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSUFTot")]
    [ComVisible(true)]
#endif
    public class GIBSUFTot
    {
        /// <summary>
        /// Total do Diferimento 
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de Devolução de Tributo IBS UF 
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDevTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do IBS UF
        /// </summary>
        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GIBSMunTot")]
    [ComVisible(true)]
#endif
    public class GIBSMunTot
    {
        /// <summary>
        /// Total do Diferimento 
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de Devolução de Tributo IBS Municipal 
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDevTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do IBS Municipal
        /// </summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Total do CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GCBSTot")]
    [ComVisible(true)]
#endif
    public class GCBSTot
    {
        /// <summary>
        /// Total do Diferimento 
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de Devolução de Tributo CBS
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDevTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do Crédito Presumido 
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do Crédito Presumido Condição Suspensiva
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GProcRef")]
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

        [XmlElement("indDevolucao")]
#if INTEROP
        public SimNao IndDevolucao { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDevolucao { get; set; }
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

        [XmlIgnore]
        public double VBCST { get; set; }

        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PICMSST { get; set; }

        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VICMSST { get; set; }

        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PFCPST { get; set; }

        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCPST { get; set; }

        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
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
        public double VPISEfet { get; set; }

        [XmlElement("vPISEfet")]
        public string VPISEfetField
        {
            get => VPISEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VPISEfet = Converter.ToDouble(value);
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
        public double VCOFINSEfet { get; set; }

        [XmlElement("vCOFINSEfet")]
        public string VCOFINSEfetField
        {
            get => VCOFINSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINSEfet = Converter.ToDouble(value);
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

#if INTEROP
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDevolucao() => IndDevolucao != null;
#endif
        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializePICMSField() => PICMS > 0;

        public bool ShouldSerializeVICMSField() => VICMS > 0;

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        public bool ShouldSerializeVBCSTField() => VBCST > 0;

        public bool ShouldSerializePICMSSTField() => PICMSST > 0;

        public bool ShouldSerializeVICMSSTField() => VICMSST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVPISField() => VPIS > 0;

        public bool ShouldSerializeVPISEfetField() => VPISEfet > 0;

        public bool ShouldSerializeVCOFINSField() => VCOFINS > 0;

        public bool ShouldSerializeVCOFINSEfetField() => VCOFINSEfet > 0;

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GContab")]
    [ComVisible(true)]
#endif
    public class GContab
    {
        [XmlElement("cContab")]
        public string CContab { get; set; }

        [XmlElement("xContab")]
        public string XContab { get; set; }

        [XmlIgnore]
        public double VContab { get; set; }

        [XmlElement("vContab")]
        public string VContabField
        {
            get => VContab.ToString("F2", CultureInfo.InvariantCulture);
            set => VContab = Converter.ToDouble(value);
        }

        [XmlElement("tpLanc")]
        public TipoLancamento TipoLancamento { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GProc")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GFAT")]
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
        public DateTime DApresFat { get; set; }
#else
        public DateTimeOffset DApresFat { get; set; }
#endif

        [XmlElement("dApresFat")]
        public string DApresFatField
        {
            get => DApresFat.ToString("yyyy-MM-dd");
#if INTEROP
            set => DApresFat = DateTime.Parse(value);
#else
            set => DApresFat = DateTimeOffset.Parse(value);
#endif
        }

        [XmlIgnore]
#if INTEROP
        public DateTime DProxLeitura { get; set; }
#else
        public DateTimeOffset DProxLeitura { get; set; }
#endif

        [XmlElement("dProxLeitura")]
        public string DProxLeituraField
        {
            get => DProxLeitura.ToString("yyyy-MM-dd");
#if INTEROP
            set => DProxLeitura = DateTime.Parse(value);
#else
            set => DProxLeitura = DateTimeOffset.Parse(value);
#endif
        }

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

        #region ShouldSerialize

        public bool ShouldSerializeDApresFatField() => DApresFat > DateTime.MinValue;

        public bool ShouldSerializeNFat() => !string.IsNullOrEmpty(NFat);

        #endregion ShouldSerialize
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.EnderCorresp")]
    [ComVisible(true)]
#endif
    public class EnderCorresp : EnderEmit { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GPix")]
    [ComVisible(true)]
#endif
    public class GPix
    {
        [XmlElement("urlQRCodePIX")]
        public string UrlQRCodePIX { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GANEEL")]
    [ComVisible(true)]
#endif
    public class GANEEL
    {
        [XmlElement("gHistFat")]
        public List<GHistFat> GHistFat { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGHistFat(GHistFat item)
        {
            if (GHistFat == null)
            {
                GHistFat = new List<GHistFat>();
            }

            GHistFat.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GHistFat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GHistFat</returns>
        public GHistFat GetGHistFat(int index)
        {
            if ((GHistFat?.Count ?? 0) == 0)
            {
                return default;
            };

            return GHistFat[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GHistFat
        /// </summary>
        public int GetGHistFatCount => (GHistFat != null ? GHistFat.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.AutXML")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GHistFat")]
    [ComVisible(true)]
#endif
    public class GHistFat
    {
        [XmlElement("xGrandFat")]
        public string XGrandFat { get; set; }

        [XmlElement("gGrandFat")]
        public List<GGrandFat> GGrandFat { get; set; }
#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddGGrandFat(GGrandFat item)
        {
            if (GGrandFat == null)
            {
                GGrandFat = new List<GGrandFat>();
            }

            GGrandFat.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista GGrandFat (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GGrandFat</returns>
        public GGrandFat GetGGrandFat(int index)
        {
            if ((GGrandFat?.Count ?? 0) == 0)
            {
                return default;
            };

            return GGrandFat[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GGrandFat
        /// </summary>
        public int GetGGrandFatCount => (GGrandFat != null ? GGrandFat.Count : 0);
#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GGrandFat")]
    [ComVisible(true)]
#endif
    public class GGrandFat
    {
        [XmlElement("CompetFat")]
        public string CompetFat { get; set; }

        [XmlIgnore]
        public double VFat { get; set; }

        [XmlElement("vFat")]
        public string VFatField
        {
            get => VFat.ToString("F4", CultureInfo.InvariantCulture);
            set => VFat = Converter.ToDouble(value);
        }

        [XmlElement("uMed")]
        public UnidadeMedidaEnergia UMed { get; set; }

        [XmlElement("qtdDias")]
        public string QtdDias { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NF3e.InfAdicNF3e")]
    [ComVisible(true)]
#endif
    public class InfAdicNF3e
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
            };

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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.GRespTec")]
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
    [ProgId("Unimake.Business.DFe.Xml.NF3e.InfNF3eSupl")]
    [ComVisible(true)]
#endif
    public class InfNF3eSupl
    {
        [XmlElement("qrCodNF3e")]
        public string QrCodNF3e { get; set; }
    }

#endregion

}
