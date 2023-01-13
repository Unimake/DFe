#pragma warning disable CS1591

#if INTEROP
using System.Runtime.InteropServices;
#endif
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;
using Unimake.Business.DFe.Servicos;
using Unimake.Business.DFe.Utility;
using System.Text;

namespace Unimake.Business.DFe.Xml.MDFe
{
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EnviMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("enviMDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class EnviMDFe : XMLBase
    {
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("idLote")]
        public string IdLote { get; set; }

        [XmlElement("MDFe")]
        public MDFe MDFe { get; set; }

        public override XmlDocument GerarXML()
        {
            var xmlDoc = base.GerarXML();

            foreach (var nodeEnvMDFe in xmlDoc.GetElementsByTagName("enviMDFe"))
            {
                var elemEnvMDFe = (XmlElement)nodeEnvMDFe;

                foreach (var nodeMDFe in elemEnvMDFe.GetElementsByTagName("MDFe"))
                {
                    var elemMDFe = (XmlElement)nodeMDFe;

                    var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
                    elemMDFe.SetAttribute("xmlns", attribute.Namespace);
                }
            }

            return xmlDoc;
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.MDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    [XmlRoot("MDFe", Namespace = "http://www.portalfiscal.inf.br/mdfe", IsNullable = false)]
    public class MDFe : XMLBase
    {
        [XmlElement("infMDFe")]
        public InfMDFe InfMDFe { get; set; }

        [XmlElement("infMDFeSupl")]
        public InfMDFeSupl InfMDFeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>
        /// Deserializar o XML no objeto MDFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML</param>
        /// <returns>Objeto do MDFe</returns>
        public MDFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<MDFe>(doc);
        }

        /// <summary>
        /// Deserializar o XML MDFe no objeto MDFe
        /// </summary>
        /// <param name="xml">string do XML MDFe</param>
        /// <returns>Objeto da MDFe</returns>
        public MDFe LoadFromXML(string xml) => XMLUtility.Deserializar<MDFe>(xml);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMDFe
    {
        private string IdField;

        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        [XmlElement("ide")]
        public Ide Ide { get; set; }

        [XmlElement("emit")]
        public Emit Emit { get; set; }

        [XmlElement("infModal")]
        public InfModal InfModal { get; set; }

        [XmlElement("infDoc")]
        public InfDocInfMDFe InfDoc { get; set; }

        [XmlElement("seg")]
        public List<Seg> Seg { get; set; }

        [XmlElement("prodPred")]
        public ProdPred ProdPred { get; set; }

        [XmlElement("tot")]
        public Tot Tot { get; set; }

        [XmlElement("lacres")]
        public List<Lacre> Lacres { get; set; }

        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id
        {
            get
            {
                IdField = "MDFe" + Chave;
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
                    Ide.NMDF.ToString().PadLeft(9, '0') +
                    ((int)Ide.TpEmis).ToString() +
                    Ide.CMDF.PadLeft(8, '0');

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
        /// <param name="lacre">Elemento</param>
        public void AddLacres(Lacre lacre)
        {
            if (Lacres == null)
            {
                Lacres = new List<Lacre>();
            }

            Lacres.Add(lacre);
        }

        /// <summary>
        /// Retorna o elemento da lista Lacres (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Lacres</returns>
        public Lacre GetLacres(int index)
        {
            if ((Lacres?.Count ?? 0) == 0)
            {
                return default;
            };

            return Lacres[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Lacres
        /// </summary>
        public int GetLacresCount => (Lacres != null ? Lacres.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Ide")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Ide
    {
        private string CMDFField;
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

        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        [XmlElement("tpEmit")]
        public TipoEmitenteMDFe TpEmit { get; set; }

        [XmlElement("tpTransp")]
#if INTEROP
        public TipoTransportadorMDFe TpTransp { get; set; } = (TipoTransportadorMDFe)(-1);
#else
        public TipoTransportadorMDFe? TpTransp { get; set; }
#endif

        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        [XmlElement("serie")]
        public int Serie { get; set; }

        [XmlElement("nMDF")]
        public int NMDF { get; set; }

        [XmlElement("cMDF")]
        public string CMDF
        {
            get
            {
                string retorno;
                if (string.IsNullOrWhiteSpace(CMDFField))
                {
                    if (NMDF == 0)
                    {
                        throw new Exception("Defina o conteúdo da TAG <nMDF>, pois a mesma é utilizada como base para calcular o código numérico.");
                    }

                    retorno = Utility.XMLUtility.GerarCodigoNumerico(NMDF).ToString("00000000");
                }
                else
                {
                    retorno = CMDFField;
                }

                return retorno;
            }
            set => CMDFField = value;
        }

        [XmlElement("cDV")]
        public int CDV { get; set; }

        [XmlElement("modal")]
        public ModalidadeTransporteMDFe Modal { get; set; }

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
        public TipoEmissao TpEmis
        {
            get => TpEmisField;
            set
            {
                if (value != TipoEmissao.Normal &&
                    value != TipoEmissao.RegimeEspecialNFF &&
                    value != TipoEmissao.ContingenciaFSIA)
                {
                    throw new Exception("Conteúdo da TAG <tpEmis> inválido! Valores aceitos: 1, 2 ou 3.");
                }

                TpEmisField = value;
            }
        }

        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi
        {
            get => ProcEmiField;
            set
            {
                if (value != ProcessoEmissao.AplicativoContribuinte)
                {
                    throw new Exception("Conteúdo da TAG <procEmi> inválido! Valor aceito: 0.");
                }

                ProcEmiField = value;
            }
        }

        [XmlElement("verProc")]
        public string VerProc { get; set; }

        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; }

        [XmlElement("infMunCarrega")]
        public List<InfMunCarrega> InfMunCarrega { get; set; }

        [XmlElement("infPercurso")]
        public List<InfPercurso> InfPercurso { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhIniViagem { get; set; }
#else
        public DateTimeOffset DhIniViagem { get; set; }
#endif


        [XmlElement("dhIniViagem")]
        public string DhIniViagemField
        {
            get => DhIniViagem.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhIniViagem = DateTime.Parse(value);
#else
            set => DhIniViagem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("indCanalVerde")]
        public SimNao IndCanalVerde { get; set; }

        [XmlElement("indCarregaPosterior")]
        public SimNao IndCarregaPosterior { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIndCanalVerde() => IndCanalVerde == SimNao.Sim;
        public bool ShouldSerializeIndCarregaPosterior() => IndCarregaPosterior == SimNao.Sim;
        public bool ShouldSerializeDhIniViagemField() => DhIniViagem > DateTime.MinValue;
#if INTEROP
        public bool ShouldSerializeTpTransp() => TpTransp != (TipoTransportadorMDFe)(-1);
#else
        public bool ShouldSerializeTpTransp() => TpTransp != null;
#endif

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infmuncarrega">Elemento</param>
        public void AddInfMunCarrega(InfMunCarrega infmuncarrega)
        {
            if (InfMunCarrega == null)
            {
                InfMunCarrega = new List<InfMunCarrega>();
            }

            InfMunCarrega.Add(infmuncarrega);
        }

        /// <summary>
        /// Retorna o elemento da lista InfMunCarrega (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfMunCarrega</returns>
        public InfMunCarrega GetInfMunCarrega(int index)
        {
            if ((InfMunCarrega?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfMunCarrega[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfMunCarrega
        /// </summary>
        public int GetInfMunCarregaCount => (InfMunCarrega != null ? InfMunCarrega.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infpercurso">Elemento</param>
        public void AddInfPercurso(InfPercurso infpercurso)
        {
            if (InfPercurso == null)
            {
                InfPercurso = new List<InfPercurso>();
            }

            InfPercurso.Add(infpercurso);
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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMunCarrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMunCarrega
    {
        [XmlElement("cMunCarrega")]
        public long CMunCarrega { get; set; }

        [XmlElement("xMunCarrega")]
        public string XMunCarrega { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfPercurso")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfPercurso
    {
        [XmlElement("UFPer")]
        public UFBrasil UFPer { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Emit
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

        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.EnderEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
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

        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize               

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfModal
    {
        [XmlAttribute(AttributeName = "versaoModal", DataType = "token")]
        public string VersaoModal { get; set; }

        [XmlElement("rodo")]
        public Rodo Rodo { get; set; }

        [XmlElement("aereo")]
        public Aereo Aereo { get; set; }

        [XmlElement("ferrov")]
        public Ferrov Ferrov { get; set; }

        [XmlElement("aquav")]
        public Aquav Aquav { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Rodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Rodo
    {
        [XmlElement("infANTT")]
        public InfANTT InfANTT { get; set; }

        [XmlElement("veicTracao")]
        public VeicTracao VeicTracao { get; set; }

        [XmlElement("veicReboque")]
        public List<VeicReboque> VeicReboque { get; set; }

        [XmlElement("codAgPorto")]
        public string CodAgPorto { get; set; }

        [XmlElement("lacRodo")]
        public List<LacRodo> LacRodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="veicreboque">Elemento</param>
        public void AddVeicReboque(VeicReboque veicreboque)
        {
            if (VeicReboque == null)
            {
                VeicReboque = new List<VeicReboque>();
            }

            VeicReboque.Add(veicreboque);
        }

        /// <summary>
        /// Retorna o elemento da lista VeicReboque (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da VeicReboque</returns>
        public VeicReboque GetVeicReboque(int index)
        {
            if ((VeicReboque?.Count ?? 0) == 0)
            {
                return default;
            };

            return VeicReboque[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista VeicReboque
        /// </summary>
        public int GetVeicReboqueCount => (VeicReboque != null ? VeicReboque.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacrodo">Elemento</param>
        public void AddLacRodo(LacRodo lacrodo)
        {
            if (LacRodo == null)
            {
                LacRodo = new List<LacRodo>();
            }

            LacRodo.Add(lacrodo);
        }

        /// <summary>
        /// Retorna o elemento da lista LacRodo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da LacRodo</returns>
        public LacRodo GetLacRodo(int index)
        {
            if ((LacRodo?.Count ?? 0) == 0)
            {
                return default;
            };

            return LacRodo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista LacRodo
        /// </summary>
        public int GetLacRodoCount => (LacRodo != null ? LacRodo.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeCodAgPorto() => !string.IsNullOrWhiteSpace(CodAgPorto);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfANTT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfANTT
    {
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        [XmlElement("infCIOT")]
        public List<InfCIOT> InfCIOT { get; set; }

        [XmlElement("valePed")]
        public List<ValePed> ValePed { get; set; }

        [XmlElement("infContratante")]
        public List<InfContratante> InfContratante { get; set; }

        [XmlElement("infPag")]
        public List<InfPag> InfPag { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infciot">Elemento</param>
        public void AddInfCIOT(InfCIOT infciot)
        {
            if (InfCIOT == null)
            {
                InfCIOT = new List<InfCIOT>();
            }

            InfCIOT.Add(infciot);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCIOT (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCIOT</returns>
        public InfCIOT GetInfCIOT(int index)
        {
            if ((InfCIOT?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCIOT[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCIOT
        /// </summary>
        public int GetInfCIOTCount => (InfCIOT != null ? InfCIOT.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="valeped">Elemento</param>
        public void AdValePed(ValePed valeped)
        {
            if (ValePed == null)
            {
                ValePed = new List<ValePed>();
            }

            ValePed.Add(valeped);
        }

        /// <summary>
        /// Retorna o elemento da lista ValePed (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ValePed</returns>
        public ValePed GetValePed(int index)
        {
            if ((ValePed?.Count ?? 0) == 0)
            {
                return default;
            };

            return ValePed[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ValePed
        /// </summary>
        public int GetValePedCount => (ValePed != null ? ValePed.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infcontratante">Elemento</param>
        public void AddInfContratante(InfContratante infcontratante)
        {
            if (InfContratante == null)
            {
                InfContratante = new List<InfContratante>();
            }

            InfContratante.Add(infcontratante);
        }

        /// <summary>
        /// Retorna o elemento da lista InfContratante (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfContratante</returns>
        public InfContratante GetInfContratante(int index)
        {
            if ((InfContratante?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfContratante[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfContratante
        /// </summary>
        public int GetInfContratanteCount => (InfContratante != null ? InfContratante.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infpag">Elemento</param>
        public void AddInfPag(InfPag infpag)
        {
            if (InfPag == null)
            {
                InfPag = new List<InfPag>();
            }

            InfPag.Add(infpag);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPag</returns>
        public InfPag GetInfPag(int index)
        {
            if ((InfPag?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPag
        /// </summary>
        public int GetInfPagCount => (InfPag != null ? InfPag.Count : 0);

#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfCIOT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfCIOT
    {
        [XmlElement("CIOT")]
        public string CIOT { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ValePed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class ValePed
    {
        [XmlElement("disp")]
        public List<Disp> Disp { get; set; }

        [XmlElement("categCombVeic")]
#if INTEROP
        public CategoriaCombinacaoVeicular CategCombVeic { get; set; } = (CategoriaCombinacaoVeicular)(-1);
#else
        public CategoriaCombinacaoVeicular? CategCombVeic { get; set; }
#endif

        #region ShouldSerialize
#if INTEROP
        public bool ShouldSerializeCategCombVeic() => CategCombVeic != (CategoriaCombinacaoVeicular)(-1);
#else
        public bool ShouldSerializeCategCombVeic() => CategCombVeic != null;
#endif

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="disp">Elemento</param>
        public void AddDisp(Disp disp)
        {
            if (Disp == null)
            {
                Disp = new List<Disp>();
            }

            Disp.Add(disp);
        }

        /// <summary>
        /// Retorna o elemento da lista Disp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Disp</returns>
        public Disp GetDisp(int index)
        {
            if ((Disp?.Count ?? 0) == 0)
            {
                return default;
            };

            return Disp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Disp
        /// </summary>
        public int GetDispCount => (Disp != null ? Disp.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Disp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Disp
    {
        [XmlElement("CNPJForn")]
        public string CNPJForn { get; set; }

        [XmlElement("CNPJPg")]
        public string CNPJPg { get; set; }

        [XmlElement("CPFPg")]
        public string CPFPg { get; set; }

        [XmlElement("nCompra")]
        public string NCompra { get; set; }

        [XmlIgnore]
        public double VValePed { get; set; }

        [XmlElement("vValePed")]
        public string VValePedField
        {
            get => VValePed.ToString("F2", CultureInfo.InvariantCulture);
            set => VValePed = Utility.Converter.ToDouble(value);
        }

        [XmlElement("tpValePed")]
#if INTEROP
        public TipoValePedagio TpValePed { get; set; } = (TipoValePedagio)(-1);
#else
        public TipoValePedagio? TpValePed { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCNPJPg() => !string.IsNullOrWhiteSpace(CNPJPg);

        public bool ShouldSerializeCPFPg() => !string.IsNullOrWhiteSpace(CPFPg);

        public bool ShouldSerializeNCompra() => (!string.IsNullOrWhiteSpace(CNPJPg) || !string.IsNullOrWhiteSpace(CPFPg) || !string.IsNullOrWhiteSpace(CNPJForn)) && !string.IsNullOrWhiteSpace(NCompra);

        public bool ShouldSerializeVValePedField() => !string.IsNullOrWhiteSpace(CNPJForn);

#if INTEROP
        public bool ShouldSerializeTpValePed() => TpValePed != (TipoValePedagio)(-1);
#else
        public bool ShouldSerializeTpValePed() => TpValePed != null;
#endif

        #endregion

    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfContratante")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfContratante
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("idEstrangeiro")]
        public string IdEstrangeiro { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeIdEstrangeiro() => !string.IsNullOrWhiteSpace(IdEstrangeiro);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfPag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfPag : InfContratante
    {
        private int IndAltoDesempField;

        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

        [XmlIgnore]
        public double VContrato { get; set; }

        [XmlElement("vContrato")]
        public string VContratoField
        {
            get => VContrato.ToString("F2", CultureInfo.InvariantCulture);
            set => VContrato = Utility.Converter.ToDouble(value);
        }

        [XmlElement("indAltoDesemp")]
        public int IndAltoDesemp
        {
            get => IndAltoDesempField;
            set
            {
                if (value != 1)
                {
                    throw new Exception("Conteúdo da TAG <indAltoDesemp> inválido! Valores aceitos: 1 ou não informe a TAG.");
                }

                IndAltoDesempField = value;
            }
        }

        [XmlElement("indPag")]
        public IndicadorPagamento IndPag { get; set; }

        [XmlIgnore]
        public double VAdiant { get; set; }

        [XmlElement("vAdiant")]
        public string VAdiantField
        {
            get => VAdiant.ToString("F2", CultureInfo.InvariantCulture);
            set => VAdiant = Utility.Converter.ToDouble(value);
        }

        [XmlElement("infPrazo")]
        public List<InfPrazo> InfPrazo { get; set; }

        [XmlElement("infBanc")]
        public InfBanc InfBanc { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeIndAltoDesemp() => IndAltoDesemp == 1;

        public bool ShouldSerializeVAdiantField() => VAdiant > 0;

        #endregion

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

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infprazo">Elemento</param>
        public void AddInfPrazo(InfPrazo infprazo)
        {
            if (InfPrazo == null)
            {
                InfPrazo = new List<InfPrazo>();
            }

            InfPrazo.Add(infprazo);
        }

        /// <summary>
        /// Retorna o elemento da lista InfPrazo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfPrazo</returns>
        public InfPrazo GetInfPrazo(int index)
        {
            if ((InfPrazo?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfPrazo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfPrazo
        /// </summary>
        public int GetInfPrazoCount => (InfPrazo != null ? InfPrazo.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Comp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Comp
    {
        [XmlElement("tpComp")]
        public TipoComponenteMDFe TpComp { get; set; }

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

        public bool ShouldSerializeXComp() => !string.IsNullOrWhiteSpace(XComp);
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfPrazo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfPrazo
    {
        [XmlElement("nParcela")]
        public string NParcela { get; set; }

        [XmlIgnore]
        public DateTime DVenc { get; set; }

        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        [XmlIgnore]
        public double VParcela { get; set; }

        [XmlElement("vParcela")]
        public string VParcelaField
        {
            get => VParcela.ToString("F2", CultureInfo.InvariantCulture);
            set => VParcela = Utility.Converter.ToDouble(value);
        }

        //#region ShouldSerialize
        //public bool ShouldSerializeNParcela() => !string.IsNullOrWhiteSpace(NParcela);
        //public bool ShouldSerializeDVencField() => DVenc > DateTime.MinValue;
        //#endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfBanc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfBanc
    {
        [XmlElement("codBanco")]
        public string CodBanco { get; set; }

        [XmlElement("codAgencia")]
        public string CodAgencia { get; set; }

        [XmlElement("CNPJIPEF")]
        public string CNPJIPEF { get; set; }

        [XmlElement("PIX")]
        public string PIX { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCodBanco() => !string.IsNullOrWhiteSpace(CodBanco);
        public bool ShouldSerializeCodAgencia() => !string.IsNullOrWhiteSpace(CodAgencia);
        public bool ShouldSerializeCNPJIPEF() => !string.IsNullOrWhiteSpace(CNPJIPEF);
        public bool ShouldSerializePIX() => !string.IsNullOrWhiteSpace(PIX);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.VeicTracao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class VeicTracao
    {
        [XmlElement("cInt")]
        public string CInt { get; set; }

        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("RENAVAM")]
        public string RENAVAM { get; set; }

        [XmlElement("tara")]
        public int Tara { get; set; }

        [XmlElement("capKG")]
        public int CapKG { get; set; }

        [XmlElement("capM3")]
        public int CapM3 { get; set; }

        [XmlElement("prop")]
        public Prop Prop { get; set; }

        [XmlElement("condutor")]
        public List<Condutor> Condutor { get; set; }

        [XmlElement("tpRod")]
        public TipoRodado TpRod { get; set; }

        [XmlElement("tpCar")]
        public TipoCarroceriaMDFe TpCar { get; set; }

        [XmlElement("UF")]

#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="condutor">Elemento</param>
        public void AddCondutor(Condutor condutor)
        {
            if (Condutor == null)
            {
                Condutor = new List<Condutor>();
            }

            Condutor.Add(condutor);
        }

        /// <summary>
        /// Retorna o elemento da lista Condutor (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Condutor</returns>
        public Condutor GetCondutor(int index)
        {
            if ((Condutor?.Count ?? 0) == 0)
            {
                return default;
            };

            return Condutor[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Condutor
        /// </summary>
        public int GetCondutorCount => (Condutor != null ? Condutor.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);
        public bool ShouldSerializeRENAVAM() => !string.IsNullOrWhiteSpace(RENAVAM);
        public bool ShouldSerializeCapM3() => CapM3 > 0;
        public bool ShouldSerializeCapKG() => CapKG > 0;
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Prop")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Prop
    {
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("IE")]
        public string IE { get; set; }


        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        [XmlElement("tpProp")]
#if INTEROP
        public TipoProprietarioMDFe TpProp { get; set; } = TipoProprietarioMDFe.NaoDefinido;
#else
        public TipoProprietarioMDFe? TpProp { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);
#if INTEROP
        public bool ShouldSerializeUF() => UF != UFBrasil.NaoDefinido;
        public bool ShouldSerializeTpProp() => TpProp != TipoProprietarioMDFe.NaoDefinido;
#else
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;
        public bool ShouldSerializeTpProp() => TpProp != TipoProprietarioMDFe.NaoDefinido && TpProp != null;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Condutor")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Condutor
    {
        [XmlElement("xNome")]
        public string XNome { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.VeicReboque")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class VeicReboque
    {
        [XmlElement("cInt")]
        public string CInt { get; set; }

        [XmlElement("placa")]
        public string Placa { get; set; }

        [XmlElement("RENAVAM")]
        public string RENAVAM { get; set; }

        [XmlElement("tara")]
        public int Tara { get; set; }

        [XmlElement("capKG")]
        public int CapKG { get; set; }

        [XmlElement("capM3")]
        public int CapM3 { get; set; }

        [XmlElement("prop")]
        public Prop Prop { get; set; }

        [XmlElement("tpCar")]
        public TipoCarroceriaMDFe TpCar { get; set; }

        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);
        public bool ShouldSerializeRENAVAM() => !string.IsNullOrWhiteSpace(RENAVAM);
        public bool ShouldSerializeCapM3() => CapM3 > 0;
#if INTEROP
        public bool ShouldSerializeUF() => UF != UFBrasil.NaoDefinido;
#else
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;
#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.LacRodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class LacRodo : Lacre { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Ferrov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Ferrov
    {
        [XmlElement("trem")]
        public Trem Trem { get; set; }

        [XmlElement("vag")]
        public List<Vag> Vag { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="vag">Elemento</param>
        public void AddVag(Vag vag)
        {
            if (Vag == null)
            {
                Vag = new List<Vag>();
            }

            Vag.Add(vag);
        }

        /// <summary>
        /// Retorna o elemento da lista Vag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Vag</returns>
        public Vag GetVag(int index)
        {
            if ((Vag?.Count ?? 0) == 0)
            {
                return default;
            };

            return Vag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Vag
        /// </summary>
        public int GetVagCount => (Vag != null ? Vag.Count : 0);


#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Trem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Trem
    {
        [XmlElement("xPref")]
        public string XPref { get; set; }

        [XmlIgnore]
#if INTEROP
        public DateTime DhTrem { get; set; }
#else
        public DateTimeOffset DhTrem { get; set; }
#endif

        [XmlElement("dhTrem")]
        public string DhTremField
        {
            get => DhTrem.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhTrem = DateTime.Parse(value);
#else
            set => DhTrem = DateTimeOffset.Parse(value);
#endif
        }

        [XmlElement("xOri")]
        public string XOri { get; set; }

        [XmlElement("xDest")]
        public string XDest { get; set; }

        [XmlElement("qVag")]
        public int QVag { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDhTremField() => DhTrem > DateTime.MinValue;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Vag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Vag
    {
        [XmlIgnore]
        public double PesoBC { get; set; }

        [XmlElement("pesoBC")]
        public string PesoBCField
        {
            get => PesoBC.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoBC = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double PesoR { get; set; }

        [XmlElement("pesoR")]
        public string PesoRField
        {
            get => PesoR.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoR = Utility.Converter.ToDouble(value);
        }

        [XmlElement("tpVag")]
        public string TpVag { get; set; }

        [XmlElement("serie")]
        public string Serie { get; set; }

        [XmlElement("nVag")]
        public long NVag { get; set; }

        [XmlElement("nSeq")]
        public long NSeq { get; set; }

        [XmlIgnore]
        public double TU { get; set; }

        [XmlElement("TU")]
        public string TUField
        {
            get => TU.ToString("F3", CultureInfo.InvariantCulture);
            set => TU = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeTpVag() => !string.IsNullOrWhiteSpace(TpVag);
        public bool ShouldSerializeNSeq() => NSeq > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Aereo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Aereo
    {
        [XmlElement("nac")]
        public string Nac { get; set; }

        [XmlElement("matr")]
        public string Matr { get; set; }

        [XmlElement("nVoo")]
        public string NVoo { get; set; }

        [XmlElement("cAerEmb")]
        public string CAerEmb { get; set; }

        [XmlElement("cAerDes")]
        public string CAerDes { get; set; }

        [XmlIgnore]
        public DateTime DVoo { get; set; }

        [XmlElement("dVoo")]
        public string DVooField
        {
            get => DVoo.ToString("yyyy-MM-dd");
            set => DVoo = DateTime.Parse(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Aquav")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Aquav
    {
        [XmlElement("irin")]
        public string Irin { get; set; }

        [XmlElement("tpEmb")]
        public string TpEmb { get; set; }

        [XmlElement("cEmbar")]
        public string CEmbar { get; set; }

        [XmlElement("xEmbar")]
        public string XEmbar { get; set; }

        [XmlElement("nViag")]
        public long NViag { get; set; }

        [XmlElement("cPrtEmb")]
        public string CPrtEmb { get; set; }

        [XmlElement("cPrtDest")]
        public string CPrtDest { get; set; }

        [XmlElement("prtTrans")]
        public string PrtTrans { get; set; }

        [XmlElement("tpNav")]
        public TipoNavegacao TpNav { get; set; }

        [XmlElement("infTermCarreg")]
        public List<InfTermCarreg> InfTermCarreg { get; set; }

        [XmlElement("infTermDescarreg")]
        public List<InfTermDescarreg> InfTermDescarreg { get; set; }

        [XmlElement("infEmbComb")]
        public List<InfEmbComb> InfEmbComb { get; set; }

        [XmlElement("infUnidCargaVazia")]
        public List<InfUnidCargaVazia> InfUnidCargaVazia { get; set; }

        [XmlElement("infUnidTranspVazia")]
        public List<InfUnidTranspVazia> InfUnidTranspVazia { get; set; }


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infTermCarreg">Elemento</param>
        public void AddInfTermCarreg(InfTermCarreg infTermCarreg)
        {
            if (InfTermCarreg == null)
            {
                InfTermCarreg = new List<InfTermCarreg>();
            }

            InfTermCarreg.Add(infTermCarreg);
        }

        /// <summary>
        /// Retorna o elemento da lista InfTermCarreg (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfTermCarreg</returns>
        public InfTermCarreg GetInfTermCarreg(int index)
        {
            if ((InfTermCarreg?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfTermCarreg[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfTermCarreg
        /// </summary>
        public int GetInfTermCarregCount => (InfTermCarreg != null ? InfTermCarreg.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infTermDescarreg">Elemento</param>
        public void AddInfTermDescarreg(InfTermDescarreg infTermDescarreg)
        {
            if (InfTermDescarreg == null)
            {
                InfTermDescarreg = new List<InfTermDescarreg>();
            }

            InfTermDescarreg.Add(infTermDescarreg);
        }

        /// <summary>
        /// Retorna o elemento da lista InfTermDescarreg (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfTermDescarreg</returns>
        public InfTermDescarreg GetInfTermDescarreg(int index)
        {
            if ((InfTermDescarreg?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfTermDescarreg[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfTermDescarreg
        /// </summary>
        public int GetInfTermDescarregCount => (InfTermDescarreg != null ? InfTermDescarreg.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infEmbComb">Elemento</param>
        public void AddInfEmbComb(InfEmbComb infEmbComb)
        {
            if (InfEmbComb == null)
            {
                InfEmbComb = new List<InfEmbComb>();
            }

            InfEmbComb.Add(infEmbComb);
        }

        /// <summary>
        /// Retorna o elemento da lista InfEmbComb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfEmbComb</returns>
        public InfEmbComb GetInfEmbComb(int index)
        {
            if ((InfEmbComb?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfEmbComb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfEmbComb
        /// </summary>
        public int GetInfEmbCombCount => (InfEmbComb != null ? InfEmbComb.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidCargaVazia">Elemento</param>
        public void AddInfUnidCargaVazia(InfUnidCargaVazia infUnidCargaVazia)
        {
            if (InfUnidCargaVazia == null)
            {
                InfUnidCargaVazia = new List<InfUnidCargaVazia>();
            }

            InfUnidCargaVazia.Add(infUnidCargaVazia);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCargaVazia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidCargaVazia</returns>
        public InfUnidCargaVazia GetInfUnidCargaVazia(int index)
        {
            if ((InfUnidCargaVazia?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidCargaVazia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCargaVazia
        /// </summary>
        public int GetInfUnidCargaVaziaCount => (InfUnidCargaVazia != null ? InfUnidCargaVazia.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infUnidTranspVazia">Elemento</param>
        public void AddInfUnidTranspVazia(InfUnidTranspVazia infUnidTranspVazia)
        {
            if (InfUnidTranspVazia == null)
            {
                InfUnidTranspVazia = new List<InfUnidTranspVazia>();
            }

            InfUnidTranspVazia.Add(infUnidTranspVazia);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTranspVazia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfUnidTranspVazia</returns>
        public InfUnidTranspVazia GetInfUnidTranspVazia(int index)
        {
            if ((InfUnidTranspVazia?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfUnidTranspVazia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTranspVazia
        /// </summary>
        public int GetInfUnidTranspVaziaCount => (InfUnidTranspVazia != null ? InfUnidTranspVazia.Count : 0);

#endif

        #region ShouldSerialize

        public bool ShouldSerializePrtTrans() => !string.IsNullOrWhiteSpace(PrtTrans);
        public bool ShouldSerialize() => TpNav != TipoNavegacao.NaoDefinido;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfTermCarreg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfTermCarreg
    {
        [XmlElement("cTermCarreg")]
        public string CTermCarreg { get; set; }

        [XmlElement("xTermCarreg")]
        public string XTermCarreg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfTermDescarreg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfTermDescarreg
    {
        [XmlElement("cTermDescarreg")]
        public string CTermDescarreg { get; set; }

        [XmlElement("xTermDescarreg")]
        public string XTermDescarreg { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfEmbComb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfEmbComb
    {
        [XmlElement("cEmbComb")]
        public string CEmbComb { get; set; }

        [XmlElement("xBalsa")]
        public string XBalsa { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfUnidCargaVazia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfUnidCargaVazia
    {
        [XmlElement("idUnidCargaVazia")]
        public string IdUnidCargaVazia { get; set; }

        [XmlElement("tpUnidCargaVazia")]
        public TipoUnidadeCarga TpUnidCargaVazia { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfUnidTranspVazia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfUnidTranspVazia
    {
        private TipoUnidadeTransporte TpUnidTranspVaziaField;

        [XmlElement("idUnidTranspVazia")]
        public string IdUnidTranspVazia { get; set; }

        [XmlElement("tpUnidTranspVazia")]
        public TipoUnidadeTransporte TpUnidTranspVazia
        {
            get => TpUnidTranspVaziaField;
            set
            {
                if (value != TipoUnidadeTransporte.RodoviarioTracao && value != TipoUnidadeTransporte.RodoviarioReboque)
                {
                    throw new Exception("Conteúdo da TAG <tpUnidTranspVazia>, filha da TAG <infUnidTranspVazia>, inválido! Valores aceitos: 1 e 2.");
                }

                TpUnidTranspVaziaField = value;
            }
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfDocInfMDFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfDocInfMDFe
    {
        [XmlElement("infMunDescarga")]
        public List<InfMunDescarga> InfMunDescarga { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infmundescarga">Elemento</param>
        public void AddInfMunDescarga(InfMunDescarga infmundescarga)
        {
            if (InfMunDescarga == null)
            {
                InfMunDescarga = new List<InfMunDescarga>();
            }

            InfMunDescarga.Add(infmundescarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfMunDescarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfMunDescarga</returns>
        public InfMunDescarga GetInfMunDescarga(int index)
        {
            if ((InfMunDescarga?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfMunDescarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfMunDescarga
        /// </summary>
        public int GetInfMunDescargaCount => (InfMunDescarga != null ? InfMunDescarga.Count : 0);

#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMunDescarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMunDescarga
    {
        [XmlElement("cMunDescarga")]
        public long CMunDescarga { get; set; }

        [XmlElement("xMunDescarga")]
        public string XMunDescarga { get; set; }

        [XmlElement("infCTe")]
        public List<InfMunDescargaInfCTe> InfCTe { get; set; }

        [XmlElement("infNFe")]
        public List<InfMunDescargaInfNFe> InfNFe { get; set; }

        [XmlElement("infMDFeTransp")]
        public List<InfMunDescargaInfMDFeTransp> InfMDFeTransp { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infMDFeTransp">Elemento</param>
        public void AddInfMDFeTransp(InfMunDescargaInfMDFeTransp infMDFeTransp)
        {
            if (InfMDFeTransp == null)
            {
                InfMDFeTransp = new List<InfMunDescargaInfMDFeTransp>();
            }

            InfMDFeTransp.Add(infMDFeTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfMDFeTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfMDFeTransp</returns>
        public InfMunDescargaInfMDFeTransp GetInfMDFeTransp(int index)
        {
            if ((InfMDFeTransp?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfMDFeTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfMDFeTransp
        /// </summary>
        public int GetInfMDFeTranspCount => (InfMDFeTransp != null ? InfMDFeTransp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNFe">Elemento</param>
        public void AddInfNFe(InfMunDescargaInfNFe infNFe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<InfMunDescargaInfNFe>();
            }

            InfNFe.Add(infNFe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFe</returns>
        public InfMunDescargaInfNFe GetInfNFe(int index)
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
        /// <param name="infCTe">Elemento</param>
        public void AddInfCTe(InfMunDescargaInfCTe infCTe)
        {
            if (InfCTe == null)
            {
                InfCTe = new List<InfMunDescargaInfCTe>();
            }

            InfCTe.Add(infCTe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCTe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfCTe</returns>
        public InfMunDescargaInfCTe GetInfCTe(int index)
        {
            if ((InfCTe?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfCTe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCTe
        /// </summary>
        public int GetInfCTeCount => (InfCTe != null ? InfCTe.Count : 0);

#endif

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMunDescargaInfCTe
    {
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }

        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        [XmlElement("infEntregaParcial")]
        public InfEntregaParcial InfEntregaParcial { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        #endregion

#if INTEROP

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfUnidTransp
    {
        [XmlElement("tpUnidTransp")]
        public TipoUnidadeTransporte TpUnidTransp { get; set; }

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


        #region ShouldSerialize

        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.LacUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class LacUnidTransp : Lacre { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfUnidCarga
    {
        [XmlElement("tpUnidCarga")]
        public TipoUnidadeCarga TpUnidCarga { get; set; }

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

        #region ShouldSerialize

        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.LacUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class LacUnidCarga : Lacre { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Peri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Peri
    {
        [XmlElement("nONU")]
        public string NONU { get; set; }

        [XmlElement("xNomeAE")]
        public string XNomeAE { get; set; }

        [XmlElement("xClaRisco")]
        public string XClaRisco { get; set; }

        [XmlElement("grEmb")]
        public string GrEmb { get; set; }

        [XmlElement("qTotProd")]
        public string QTotProd { get; set; }

        [XmlElement("qVolTipo")]
        public string QVolTipo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXNomeAE() => !string.IsNullOrWhiteSpace(XNomeAE);
        public bool ShouldSerializeXClaRisco() => !string.IsNullOrWhiteSpace(XClaRisco);
        public bool ShouldSerializeGrEmb() => !string.IsNullOrWhiteSpace(GrEmb);
        public bool ShouldSerializeQVolTipo() => !string.IsNullOrWhiteSpace(QVolTipo);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfEntregaParcial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfEntregaParcial
    {
        [XmlIgnore]
        public double QtdTotal { get; set; }

        [XmlElement("qtdTotal")]
        public string QtdTotalField
        {
            get => QtdTotal.ToString("F4", CultureInfo.InvariantCulture);
            set => QtdTotal = Utility.Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double QtdParcial { get; set; }

        [XmlElement("qtdParcial")]
        public string QtdParcialField
        {
            get => QtdParcial.ToString("F4", CultureInfo.InvariantCulture);
            set => QtdParcial = Utility.Converter.ToDouble(value);
        }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMunDescargaInfNFe
    {
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        #endregion

#if INTEROP

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMunDescargaInfMDFeTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMunDescargaInfMDFeTransp
    {
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        #endregion

#if INTEROP

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
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Seg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Seg
    {
        [XmlElement("infResp")]
        public InfResp InfResp { get; set; }

        [XmlElement("infSeg")]
        public InfSeg InfSeg { get; set; }

        [XmlElement("nApol")]
        public string NApol { get; set; }

        [XmlElement("nAver")]
        public List<string> NAver { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="naver">Elemento</param>
        public void AddNAver(string naver)
        {
            if (NAver == null)
            {
                NAver = new List<string>();
            }

            NAver.Add(naver);
        }

        /// <summary>
        /// Retorna o elemento da lista NAver (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NAver</returns>
        public string GetNAver(int index)
        {
            if ((NAver?.Count ?? 0) == 0)
            {
                return default;
            };

            return NAver[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NAver
        /// </summary>
        public int GetNAverCount => (NAver != null ? NAver.Count : 0);

#endif
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfResp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfResp
    {
        [XmlElement("respSeg")]
        public ResponsavelSeguroMDFe RespSeg { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfSeg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfSeg
    {
        [XmlElement("xSeg")]
        public string XSeg { get; set; }

        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.ProdPred")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class ProdPred
    {
        [XmlElement("tpCarga")]
        public TipoCargaMDFe TpCarga { get; set; }

        [XmlElement("xProd")]
        public string XProd { get; set; }

        [XmlElement("cEAN")]
        public string CEAN { get; set; }

        [XmlElement("NCM")]
        public string NCM { get; set; }

        [XmlElement("infLotacao")]
        public InfLotacao InfLotacao { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCEAN() => !string.IsNullOrWhiteSpace(CEAN);
        public bool ShouldSerializeNCM() => !string.IsNullOrWhiteSpace(NCM);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfLotacao")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfLotacao
    {
        [XmlElement("infLocalCarrega")]
        public InfLocalCarrega InfLocalCarrega { get; set; }

        [XmlElement("infLocalDescarrega")]
        public InfLocalDescarrega InfLocalDescarrega { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfLocalCarrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfLocalCarrega
    {
        [XmlElement("CEP")]
        public string CEP { get; set; }

        [XmlElement("latitude")]
        public string Latitude { get; set; }

        [XmlElement("longitude")]
        public string Longitude { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);
        public bool ShouldSerializeLatitude() => !string.IsNullOrWhiteSpace(Latitude) && string.IsNullOrWhiteSpace(CEP);
        public bool ShouldSerializeLongitude() => !string.IsNullOrWhiteSpace(Longitude) && string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfLocalDescarrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfLocalDescarrega : InfLocalCarrega { }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Tot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Tot
    {
        [XmlElement("qCTe")]
        public int QCTe { get; set; }

        [XmlElement("qNFe")]
        public int QNFe { get; set; }

        [XmlElement("qMDFe")]
        public int QMDFe { get; set; }

        [XmlIgnore]
        public double VCarga { get; set; }

        [XmlElement("vCarga")]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Utility.Converter.ToDouble(value);
        }

        [XmlElement("cUnid")]
        public CodigoUnidadeMedidaMDFe CUnid { get; set; }

        [XmlIgnore]
        public double QCarga { get; set; }

        [XmlElement("qCarga")]
        public string QCargaField
        {
            get => QCarga.ToString("F4", CultureInfo.InvariantCulture);
            set => QCarga = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQCTe() => QCTe > 0;
        public bool ShouldSerializeQNFe() => QNFe > 0;
        public bool ShouldSerializeQMDFe() => QMDFe > 0;

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.Lacre")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class Lacre
    {
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.AutXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfAdic")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfAdic
    {
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        [XmlElement("infCpl")]
        public string InfCpl { get; set; }
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfRespTec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfMDFeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfMDFeSupl
    {
        [XmlElement("qrCodMDFe")]
        public string QrCodMDFe { get; set; }
    }
}
