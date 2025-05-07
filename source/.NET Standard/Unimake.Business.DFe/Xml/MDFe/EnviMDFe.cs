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
        /// <summary>
        /// Versão do leiaute XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificador de controle do lote
        /// </summary>
        [XmlElement("idLote")]
        public string IdLote { get; set; }

        /// <summary>
        /// Manifesto de Documentos Fiscais Eletrônico
        /// </summary>
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
        /// <summary>
        /// Informações do Manifesto de Documentos Fiscais Eletrônico
        /// </summary>
        [XmlElement("infMDFe")]
        public InfMDFe InfMDFe { get; set; }

        /// <summary>
        /// Informações suplementares do Manifesto de Documentos Fiscais Eletrônico
        /// </summary>
        [XmlElement("infMDFeSupl")]
        public InfMDFeSupl InfMDFeSupl { get; set; }

        /// <summary>
        /// Assinatura XML
        /// </summary>
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

        /// <summary>
        /// Versão do leiaute XML
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificação do MDFe
        /// </summary>
        [XmlElement("ide")]
        public Ide Ide { get; set; }

        /// <summary>
        /// Identificação do Emitente do MDFe
        /// </summary>
        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>
        /// Informações do Modal
        /// </summary>
        [XmlElement("infModal")]
        public InfModal InfModal { get; set; }

        /// <summary>
        /// Informações dos Documentos
        /// </summary>
        [XmlElement("infDoc")]
        public InfDocInfMDFe InfDoc { get; set; }

        /// <summary>
        /// Informações do Seguro
        /// </summary>
        [XmlElement("seg")]
        public List<Seg> Seg { get; set; }

        /// <summary>
        /// Produto Predominante
        /// </summary>
        [XmlElement("prodPred")]
        public ProdPred ProdPred { get; set; }

        /// <summary>
        /// Totais
        /// </summary>
        [XmlElement("tot")]
        public Tot Tot { get; set; }

        /// <summary>
        /// Lacres
        /// </summary>
        [XmlElement("lacres")]
        public List<Lacre> Lacres { get; set; }

        /// <summary>
        /// Autorizados para download do XML
        /// </summary>
        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        /// <summary>
        /// Informações Adicionais
        /// </summary>
        [XmlElement("infAdic")]
        public InfAdic InfAdic { get; set; }

        /// <summary>
        /// Informações do Responsável Técnico
        /// </summary>
        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        /// <summary>
        /// Identificador da TAG a ser assinada
        /// </summary>
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

        /// <summary>
        /// Chave de acesso do MDFe
        /// </summary>
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
                    NumeroDoctoFiscal = Ide.NMDF,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    CodigoNumerico = Ide.CMDF
                };
                ChaveField = XMLUtility.MontarChaveMDFe(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

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

        /// <summary>
        /// Código da UF do emitente do MDFe
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "CUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Tipo do Ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Tipo do Emitente
        /// </summary>
        [XmlElement("tpEmit")]
        public TipoEmitenteMDFe TpEmit { get; set; }

        /// <summary>
        /// Tipo do Transportador
        /// </summary>
        [XmlElement("tpTransp")]
#if INTEROP
        public TipoTransportadorMDFe TpTransp { get; set; } = (TipoTransportadorMDFe)(-1);
#else
        public TipoTransportadorMDFe? TpTransp { get; set; }
#endif

        /// <summary>
        /// Modelo do Documento Fiscal
        /// </summary>
        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        /// <summary>
        /// Série do MDFe
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do MDFe
        /// </summary>
        [XmlElement("nMDF")]
        public int NMDF { get; set; }

        /// <summary>
        /// Código Numérico que compõe a Chave de Acesso
        /// </summary>
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

        /// <summary>
        /// Dígito Verificador da Chave de Acesso
        /// </summary>
        [XmlElement("cDV")]
        public int CDV { get; set; }

        /// <summary>
        /// Modalidade de Transporte
        /// </summary>
        [XmlElement("modal")]
        public ModalidadeTransporteMDFe Modal { get; set; }

        /// <summary>
        /// Data e Hora de Emissão
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhEmi" para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Tipo de Emissão
        /// </summary>
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

        /// <summary>
        /// Processo de Emissão do MDFe
        /// </summary>
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

        /// <summary>
        /// Versão do processo de emissão do MDFe
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        /// <summary>
        /// UF de Início da viagem
        /// </summary>
        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

        /// <summary>
        /// UF de Fim da viagem
        /// </summary>
        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; }

        /// <summary>
        /// Informações dos municípios de carregamento
        /// </summary>
        [XmlElement("infMunCarrega")]
        public List<InfMunCarrega> InfMunCarrega { get; set; }

        /// <summary>
        /// Informações do percurso
        /// </summary>
        [XmlElement("infPercurso")]
        public List<InfPercurso> InfPercurso { get; set; }

        /// <summary>
        /// Data e Hora de Início da Viagem
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhIniViagem { get; set; }
#else
        public DateTimeOffset DhIniViagem { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhIniViagem" para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Indicador de Canal Verde (Sim/Não)
        /// </summary>
        [XmlElement("indCanalVerde")]
        public SimNao IndCanalVerde { get; set; }

        /// <summary>
        /// Indicador de Carregamento Posterior (Sim/Não)
        /// </summary>
        [XmlElement("indCarregaPosterior")]
        public SimNao IndCarregaPosterior { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo IndCanalVerde (Sim)
        /// </summary>
        public bool ShouldSerializeIndCanalVerde() => IndCanalVerde == SimNao.Sim;

        /// <summary>
        /// Verifica se deve serializar o campo IndCarregaPosterior (Sim)
        /// </summary>
        public bool ShouldSerializeIndCarregaPosterior() => IndCarregaPosterior == SimNao.Sim;

        /// <summary>
        /// Verifica se deve serializar o campo DhIniViagemField (data maior que DateTime.MinValue)
        /// </summary>
        public bool ShouldSerializeDhIniViagemField() => DhIniViagem > DateTime.MinValue;

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo TpTransp para interop (diferente de -1)
        /// </summary>
        public bool ShouldSerializeTpTransp() => TpTransp != (TipoTransportadorMDFe)(-1);
#else
        /// <summary>
        /// Verifica se deve serializar o campo TpTransp (não nulo)
        /// </summary>
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
        /// <summary>
        /// Código do Município de Carregamento
        /// </summary>
        [XmlElement("cMunCarrega")]
        public long CMunCarrega { get; set; }

        /// <summary>
        /// Nome do Município de Carregamento
        /// </summary>
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
        /// <summary>
        /// UF do Percurso
        /// </summary>
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
        /// <summary>
        /// CNPJ do Emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Emitente
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do Emitente
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do Emitente
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Nome Fantasia do Emitente
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Endereço do Emitente
        /// </summary>
        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se deve serializar o campo XFant (não vazio)
        /// </summary>
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
        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do Município
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do Município
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// UF
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Telefone
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Email
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo XCpl (não vazio)
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se deve serializar o campo CEP (não vazio)
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        /// <summary>
        /// Verifica se deve serializar o campo Fone (não vazio)
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se deve serializar o campo Email (não vazio)
        /// </summary>
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
        /// <summary>
        /// Versão do Modal
        /// </summary>
        [XmlAttribute(AttributeName = "versaoModal", DataType = "token")]
        public string VersaoModal { get; set; }

        /// <summary>
        /// Informações do modal Rodoviário
        /// </summary>
        [XmlElement("rodo")]
        public Rodo Rodo { get; set; }

        /// <summary>
        /// Informações do modal Aéreo
        /// </summary>
        [XmlElement("aereo")]
        public Aereo Aereo { get; set; }

        /// <summary>
        /// Informações do modal Ferroviário
        /// </summary>
        [XmlElement("ferrov")]
        public Ferrov Ferrov { get; set; }

        /// <summary>
        /// Informações do modal Aquaviário
        /// </summary>
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
        /// <summary>
        /// Informações da ANTT
        /// </summary>
        [XmlElement("infANTT")]
        public InfANTT InfANTT { get; set; }

        /// <summary>
        /// Informações do Veículo de Tração
        /// </summary>
        [XmlElement("veicTracao")]
        public VeicTracao VeicTracao { get; set; }

        /// <summary>
        /// Informações dos Veículos de Reboque
        /// </summary>
        [XmlElement("veicReboque")]
        public List<VeicReboque> VeicReboque { get; set; }

        /// <summary>
        /// Código do Agenciamento do Porto
        /// </summary>
        [XmlElement("codAgPorto")]
        public string CodAgPorto { get; set; }

        /// <summary>
        /// Lacres do Modal Rodoviário
        /// </summary>
        [XmlElement("lacRodo")]
        public List<LacRodo> LacRodo { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista de Veículos de Reboque
        /// </summary>
        /// <param name="veicreboque">Elemento VeicReboque a ser adicionado</param>
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
        /// Adicionar novo elemento a lista de Lacres do Modal Rodoviário
        /// </summary>
        /// <param name="lacrodo">Elemento LacRodo a ser adicionado</param>
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

        /// <summary>
        /// Verifica se deve serializar o campo CodAgPorto (não vazio)
        /// </summary>
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
        /// <summary>
        /// Registro Nacional de Transportadores Rodoviários de Carga
        /// </summary>
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        /// <summary>
        /// Informações do CIOT
        /// </summary>
        [XmlElement("infCIOT")]
        public List<InfCIOT> InfCIOT { get; set; }

        /// <summary>
        /// Informações do Vale Pedágio
        /// </summary>
        [XmlElement("valePed")]
        public List<ValePed> ValePed { get; set; }

        /// <summary>
        /// Informações dos Contratantes
        /// </summary>
        [XmlElement("infContratante")]
        public List<InfContratante> InfContratante { get; set; }

        /// <summary>
        /// Informações de Pagamento
        /// </summary>
        [XmlElement("infPag")]
        public List<InfPag> InfPag { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações do CIOT
        /// </summary>
        /// <param name="infciot">Elemento InfCIOT a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de informações do Vale Pedágio
        /// </summary>
        /// <param name="valeped">Elemento ValePed a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de informações dos Contratantes
        /// </summary>
        /// <param name="infcontratante">Elemento InfContratante a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de informações de Pagamento
        /// </summary>
        /// <param name="infpag">Elemento InfPag a ser adicionado</param>
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
        /// <summary>
        /// Código Identificador da Operação de Transporte
        /// </summary>
        [XmlElement("CIOT")]
        public string CIOT { get; set; }

        /// <summary>
        /// CNPJ do responsável pelo pagamento do frete
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do responsável pelo pagamento do frete
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se deve serializar o campo CIOT (não vazio)
        /// </summary>
        public bool ShouldSerializeCIOT() => !string.IsNullOrWhiteSpace(CIOT);

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
        /// <summary>
        /// Detalhamento do Vale Pedágio
        /// </summary>
        [XmlElement("disp")]
        public List<Disp> Disp { get; set; }

        /// <summary>
        /// Categoria de Combinação Veicular
        /// </summary>
        [XmlElement("categCombVeic")]
#if INTEROP
        public CategoriaCombinacaoVeicular CategCombVeic { get; set; } = (CategoriaCombinacaoVeicular)(-1);
#else
        public CategoriaCombinacaoVeicular? CategCombVeic { get; set; }
#endif

        #region ShouldSerialize
#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo CategCombVeic para interop (diferente de -1)
        /// </summary>
        public bool ShouldSerializeCategCombVeic() => CategCombVeic != (CategoriaCombinacaoVeicular)(-1);
#else
        /// <summary>
        /// Verifica se deve serializar o campo CategCombVeic (não nulo)
        /// </summary>
        public bool ShouldSerializeCategCombVeic() => CategCombVeic != null;
#endif

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Detalhamento do Vale Pedágio
        /// </summary>
        /// <param name="disp">Elemento Disp a ser adicionado</param>
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
        /// <summary>
        /// CNPJ do Fornecedor
        /// </summary>
        [XmlElement("CNPJForn")]
        public string CNPJForn { get; set; }

        /// <summary>
        /// CNPJ do Pagador
        /// </summary>
        [XmlElement("CNPJPg")]
        public string CNPJPg { get; set; }

        /// <summary>
        /// CPF do Pagador
        /// </summary>
        [XmlElement("CPFPg")]
        public string CPFPg { get; set; }

        /// <summary>
        /// Número da Compra
        /// </summary>
        [XmlElement("nCompra")]
        public string NCompra { get; set; }

        /// <summary>
        /// Valor do Vale Pedágio
        /// </summary>
        [XmlIgnore]
        public double VValePed { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VValePed" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vValePed")]
        public string VValePedField
        {
            get => VValePed.ToString("F2", CultureInfo.InvariantCulture);
            set => VValePed = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Tipo do Vale Pedágio
        /// </summary>
        [XmlElement("tpValePed")]
#if INTEROP
        public TipoValePedagio TpValePed { get; set; } = (TipoValePedagio)(-1);
#else
        public TipoValePedagio? TpValePed { get; set; }
#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJPg (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJPg() => !string.IsNullOrWhiteSpace(CNPJPg);

        /// <summary>
        /// Verifica se deve serializar o campo CPFPg (não vazio)
        /// </summary>
        public bool ShouldSerializeCPFPg() => !string.IsNullOrWhiteSpace(CPFPg);

        /// <summary>
        /// Verifica se deve serializar o campo NCompra (não vazio) e se algum dos campos CNPJPg, CPFPg ou CNPJForn estiver preenchido
        /// </summary>
        public bool ShouldSerializeNCompra() => (!string.IsNullOrWhiteSpace(CNPJPg) || !string.IsNullOrWhiteSpace(CPFPg) || !string.IsNullOrWhiteSpace(CNPJForn)) && !string.IsNullOrWhiteSpace(NCompra);

        /// <summary>
        /// Verifica se deve serializar o campo VValePedField (CNPJForn não vazio)
        /// </summary>
        public bool ShouldSerializeVValePedField() => !string.IsNullOrWhiteSpace(CNPJForn);

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo TpValePed para interop (diferente de -1)
        /// </summary>
        public bool ShouldSerializeTpValePed() => TpValePed != (TipoValePedagio)(-1);
#else
        /// <summary>
        /// Verifica se deve serializar o campo TpValePed (não nulo)
        /// </summary>
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
        /// <summary>
        /// Nome do Contratante
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// CNPJ do Contratante
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Contratante
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Identificação de Contratante Estrangeiro
        /// </summary>
        [XmlElement("idEstrangeiro")]
        public string IdEstrangeiro { get; set; }

        /// <summary>
        /// Grupo de informações do contrato entre transportador e contratante
        /// </summary>
        [XmlElement("infContrato")]
        public InfContrato InfContrato { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se deve serializar o campo XNome (não vazio)
        /// </summary>
        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        /// <summary>
        /// Verifica se deve serializar o campo IdEstrangeiro (não vazio)
        /// </summary>
        public bool ShouldSerializeIdEstrangeiro() => !string.IsNullOrWhiteSpace(IdEstrangeiro);

        #endregion
    }

    /// <summary>
    /// Grupo de informações do contrato entre transportador e contratante
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfContrato")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfContrato
    {
        /// <summary>
        /// Número do contrato do transportador com o contratante quando este existir para prestações continuadas
        /// </summary>
        [XmlElement("NroContrato")]
        public string NroContrato { get; set; }

        /// <summary>
        /// Valor Global do Contrato
        /// </summary>
        [XmlIgnore]
        public double VContratoGlobal { get; set; }

        /// <summary>
        /// Valor Global do Contrato (Utilize a propriedade VContratoGlobal para setar ou recuperar o conteúdo)
        /// </summary>
        [XmlElement("vContratoGlobal")]
        public string VContratoGlobalField
        {
            get => VContratoGlobal.ToString("F2", CultureInfo.InvariantCulture);
            set => VContratoGlobal = Utility.Converter.ToDouble(value);
        }
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
        private int IndAntecipaAdiantField;

        /// <summary>
        /// Componentes do Valor a Pagar
        /// </summary>
        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

        /// <summary>
        /// Valor do Contrato
        /// </summary>
        [XmlIgnore]
        public double VContrato { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VContrato" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vContrato")]
        public string VContratoField
        {
            get => VContrato.ToString("F2", CultureInfo.InvariantCulture);
            set => VContrato = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Indicador de Alto Desempenho
        /// </summary>
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

        /// <summary>
        /// Indicador de Pagamento
        /// </summary>
        [XmlElement("indPag")]
        public IndicadorPagamento IndPag { get; set; }

        /// <summary>
        /// Valor do Adiantamento
        /// </summary>
        [XmlIgnore]
        public double VAdiant { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VAdiant" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAdiant")]
        public string VAdiantField
        {
            get => VAdiant.ToString("F2", CultureInfo.InvariantCulture);
            set => VAdiant = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Indicador para declarar concordância em antecipar o adiantamento (Informar a tag somente se for autorizado antecipar o adiantamento)
        /// </summary>
        [XmlElement("indAntecipaAdiant")]
        public int IndAntecipaAdiant
        {
            get => IndAntecipaAdiantField;
            set
            {
                if (value != 1)
                {
                    throw new Exception("Conteúdo da TAG <indAntecipaAdiant> inválido! Valores aceitos: 1 ou não informe a TAG.");
                }

                IndAntecipaAdiantField = value;
            }
        }

        /// <summary>
        /// Informações dos Prazos
        /// </summary>
        [XmlElement("infPrazo")]
        public List<InfPrazo> InfPrazo { get; set; }

        /// <summary>
        /// Tipo de Permissão em relação a antecipação das parcelas
        /// </summary>
        [XmlElement("tpAntecip")]
#if INTEROP
        public TipoPermissaoAtencipacaoParcela TpAntecip { get; set; } = (TipoPermissaoAtencipacaoParcela)(-1);
#else
        public TipoPermissaoAtencipacaoParcela? TpAntecip { get; set; }
#endif

        /// <summary>
        /// Informações Bancárias
        /// </summary>
        [XmlElement("infBanc")]
        public InfBanc InfBanc { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo IndAntecipaAdiant (igual a 1)
        /// </summary>
        public bool ShouldSerializeIndAntecipaAdiant() => IndAntecipaAdiant == 1;

        /// <summary>
        /// Verifica se deve serializar o campo VAdiantField (maior que 0)
        /// </summary>
        public bool ShouldSerializeVAdiantField() => VAdiant > 0;

        /// <summary>
        /// Verifica se deve serializar o campo IndAltoDesemp (igual a 1)
        /// </summary>
        public bool ShouldSerializeIndAltoDesemp() => IndAltoDesemp == 1;

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo TpAntecip para interop (diferente de -1)
        /// </summary>
        public bool ShouldSerializeTpAntecip() => TpAntecip != (TipoPermissaoAtencipacaoParcela)(-1);
#else
        /// <summary>
        /// Verifica se deve serializar o campo TpAntecip (não nulo)
        /// </summary>
        public bool ShouldSerializeTpAntecip() => TpAntecip != null;
#endif

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Componentes do Valor a Pagar
        /// </summary>
        /// <param name="comp">Elemento Comp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Informações dos Prazos
        /// </summary>
        /// <param name="infprazo">Elemento InfPrazo a ser adicionado</param>
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
        /// <summary>
        /// Tipo do Componente
        /// </summary>
        [XmlElement("tpComp")]
        public TipoComponenteMDFe TpComp { get; set; }

        /// <summary>
        /// Valor do Componente
        /// </summary>
        [XmlIgnore]
        public double VComp { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VComp" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vComp")]
        public string VCompField
        {
            get => VComp.ToString("F2", CultureInfo.InvariantCulture);
            set => VComp = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Descrição do Componente
        /// </summary>
        [XmlElement("xComp")]
        public string XComp { get; set; }

        /// <summary>
        /// Verifica se deve serializar o campo XComp (não vazio)
        /// </summary>
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
        /// <summary>
        /// Número da Parcela
        /// </summary>
        [XmlElement("nParcela")]
        public string NParcela { get; set; }

        /// <summary>
        /// Data de Vencimento
        /// </summary>
        [XmlIgnore]
        public DateTime DVenc { get; set; }

        /// <summary>
        /// Campo para serialização da Data de Vencimento
        /// </summary>
        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor da Parcela
        /// </summary>
        [XmlIgnore]
        public double VParcela { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VParcela" para atribuir ou resgatar o valor)
        /// </summary>
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
        /// <summary>
        /// Código do Banco
        /// </summary>
        [XmlElement("codBanco")]
        public string CodBanco { get; set; }

        /// <summary>
        /// Código da Agência
        /// </summary>
        [XmlElement("codAgencia")]
        public string CodAgencia { get; set; }

        /// <summary>
        /// CNPJ da Instituição de Pagamento Eletrônico de Frete
        /// </summary>
        [XmlElement("CNPJIPEF")]
        public string CNPJIPEF { get; set; }

        /// <summary>
        /// Chave PIX
        /// </summary>
        [XmlElement("PIX")]
        public string PIX { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CodBanco (não vazio)
        /// </summary>
        public bool ShouldSerializeCodBanco() => !string.IsNullOrWhiteSpace(CodBanco);

        /// <summary>
        /// Verifica se deve serializar o campo CodAgencia (não vazio)
        /// </summary>
        public bool ShouldSerializeCodAgencia() => !string.IsNullOrWhiteSpace(CodAgencia);

        /// <summary>
        /// Verifica se deve serializar o campo CNPJIPEF (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJIPEF() => !string.IsNullOrWhiteSpace(CNPJIPEF);

        /// <summary>
        /// Verifica se deve serializar o campo PIX (não vazio)
        /// </summary>
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
        /// <summary>
        /// Código Interno do Veículo
        /// </summary>
        [XmlElement("cInt")]
        public string CInt { get; set; }

        /// <summary>
        /// Placa do Veículo
        /// </summary>
        [XmlElement("placa")]
        public string Placa { get; set; }

        /// <summary>
        /// RENAVAM do Veículo
        /// </summary>
        [XmlElement("RENAVAM")]
        public string RENAVAM { get; set; }

        /// <summary>
        /// Tara em KG
        /// </summary>
        [XmlElement("tara")]
        public int Tara { get; set; }

        /// <summary>
        /// Capacidade em KG
        /// </summary>
        [XmlElement("capKG")]
        public int CapKG { get; set; }

        /// <summary>
        /// Capacidade em M3
        /// </summary>
        [XmlElement("capM3")]
        public int CapM3 { get; set; }

        /// <summary>
        /// Proprietário do Veículo
        /// </summary>
        [XmlElement("prop")]
        public Prop Prop { get; set; }

        /// <summary>
        /// Condutores do Veículo
        /// </summary>
        [XmlElement("condutor")]
        public List<Condutor> Condutor { get; set; }

        /// <summary>
        /// Tipo do Rodado
        /// </summary>
        [XmlElement("tpRod")]
        public TipoRodado TpRod { get; set; }

        /// <summary>
        /// Tipo da Carroceria
        /// </summary>
        [XmlElement("tpCar")]
        public TipoCarroceriaMDFe TpCar { get; set; }

        /// <summary>
        /// UF do Veículo
        /// </summary>
        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif


#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Condutores
        /// </summary>
        /// <param name="condutor">Elemento Condutor a ser adicionado</param>
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

        /// <summary>
        /// Verifica se deve serializar o campo CInt (não vazio)
        /// </summary>
        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);

        /// <summary>
        /// Verifica se deve serializar o campo RENAVAM (não vazio)
        /// </summary>
        public bool ShouldSerializeRENAVAM() => !string.IsNullOrWhiteSpace(RENAVAM);

        /// <summary>
        /// Verifica se deve serializar o campo CapM3 (maior que 0)
        /// </summary>
        public bool ShouldSerializeCapM3() => CapM3 > 0;

        /// <summary>
        /// Verifica se deve serializar o campo CapKG (maior que 0)
        /// </summary>
        public bool ShouldSerializeCapKG() => CapKG > 0;

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo UF para interop (diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeUF() => UF != UFBrasil.NaoDefinido;
#else
        /// <summary>
        /// Verifica se deve serializar o campo UF (não nulo)
        /// </summary>
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;
#endif

        #endregion ShouldSerialize
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
        /// <summary>
        /// CNPJ do Proprietário
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Proprietário
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// RNTRC do Proprietário
        /// </summary>
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        /// <summary>
        /// Nome do Proprietário
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Inscrição Estadual do Proprietário
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }


        /// <summary>
        /// UF do Proprietário
        /// </summary>
        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        /// <summary>
        /// Tipo do Proprietário
        /// </summary>
        [XmlElement("tpProp")]
#if INTEROP
        public TipoProprietarioMDFe TpProp { get; set; } = TipoProprietarioMDFe.NaoDefinido;
#else
        public TipoProprietarioMDFe? TpProp { get; set; }
#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo UF para interop (diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeUF() => UF != UFBrasil.NaoDefinido;

        /// <summary>
        /// Verifica se deve serializar o campo TpProp para interop (diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeTpProp() => TpProp != TipoProprietarioMDFe.NaoDefinido;
#else
        /// <summary>
        /// Verifica se deve serializar o campo UF (não nulo e diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        /// <summary>
        /// Verifica se deve serializar o campo TpProp (não nulo e diferente de NaoDefinido)
        /// </summary>
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
        /// <summary>
        /// Nome do Condutor
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// CPF do Condutor
        /// </summary>
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
        /// <summary>
        /// Código Interno do Reboque
        /// </summary>
        [XmlElement("cInt")]
        public string CInt { get; set; }

        /// <summary>
        /// Placa do Reboque
        /// </summary>
        [XmlElement("placa")]
        public string Placa { get; set; }

        /// <summary>
        /// RENAVAM do Reboque
        /// </summary>
        [XmlElement("RENAVAM")]
        public string RENAVAM { get; set; }

        /// <summary>
        /// Tara em KG
        /// </summary>
        [XmlElement("tara")]
        public int Tara { get; set; }

        /// <summary>
        /// Capacidade em KG
        /// </summary>
        [XmlElement("capKG")]
        public int CapKG { get; set; }

        /// <summary>
        /// Capacidade em M3
        /// </summary>
        [XmlElement("capM3")]
        public int CapM3 { get; set; }

        /// <summary>
        /// Proprietário do Reboque
        /// </summary>
        [XmlElement("prop")]
        public Prop Prop { get; set; }

        /// <summary>
        /// Tipo da Carroceria
        /// </summary>
        [XmlElement("tpCar")]
        public TipoCarroceriaMDFe TpCar { get; set; }

        /// <summary>
        /// UF do Reboque
        /// </summary>
        [XmlElement("UF")]
#if INTEROP
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        public UFBrasil? UF { get; set; }
#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CInt (não vazio)
        /// </summary>
        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);

        /// <summary>
        /// Verifica se deve serializar o campo RENAVAM (não vazio)
        /// </summary>
        public bool ShouldSerializeRENAVAM() => !string.IsNullOrWhiteSpace(RENAVAM);

        /// <summary>
        /// Verifica se deve serializar o campo CapM3 (maior que 0)
        /// </summary>
        public bool ShouldSerializeCapM3() => CapM3 > 0;

#if INTEROP
        /// <summary>
        /// Verifica se deve serializar o campo UF para interop (diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeUF() => UF != UFBrasil.NaoDefinido;
#else
        /// <summary>
        /// Verifica se deve serializar o campo UF (não nulo e diferente de NaoDefinido)
        /// </summary>
        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;
#endif

        #endregion
    }

    /// <summary>
    /// Lacre Rodoviário
    /// </summary>
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
        /// <summary>
        /// Informações do Trem
        /// </summary>
        [XmlElement("trem")]
        public Trem Trem { get; set; }

        /// <summary>
        /// Informações das Vagões
        /// </summary>
        [XmlElement("vag")]
        public List<Vag> Vag { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Vagões
        /// </summary>
        /// <param name="vag">Elemento Vag a ser adicionado</param>
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
        /// <summary>
        /// Prefixo do Trem
        /// </summary>
        [XmlElement("xPref")]
        public string XPref { get; set; }

        /// <summary>
        /// Data e Hora do Trem
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhTrem { get; set; }
#else
        public DateTimeOffset DhTrem { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhTrem" para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Origem do Trem
        /// </summary>
        [XmlElement("xOri")]
        public string XOri { get; set; }

        /// <summary>
        /// Destino do Trem
        /// </summary>
        [XmlElement("xDest")]
        public string XDest { get; set; }

        /// <summary>
        /// Quantidade de Vagões
        /// </summary>
        [XmlElement("qVag")]
        public int QVag { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo DhTremField (maior que DateTime.MinValue)
        /// </summary>
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
        /// <summary>
        /// Peso Bruto Calculado
        /// </summary>
        [XmlIgnore]
        public double PesoBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PesoBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pesoBC")]
        public string PesoBCField
        {
            get => PesoBC.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoBC = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Peso Real
        /// </summary>
        [XmlIgnore]
        public double PesoR { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PesoR" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pesoR")]
        public string PesoRField
        {
            get => PesoR.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoR = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Tipo da Vagão
        /// </summary>
        [XmlElement("tpVag")]
        public string TpVag { get; set; }

        /// <summary>
        /// Série da Vagão
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da Vagão
        /// </summary>
        [XmlElement("nVag")]
        public long NVag { get; set; }

        /// <summary>
        /// Número Sequencial da Vagão
        /// </summary>
        [XmlElement("nSeq")]
        public long NSeq { get; set; }

        /// <summary>
        /// Tara Útil
        /// </summary>
        [XmlIgnore]
        public double TU { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "TU" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("TU")]
        public string TUField
        {
            get => TU.ToString("F3", CultureInfo.InvariantCulture);
            set => TU = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo TpVag (não vazio)
        /// </summary>
        public bool ShouldSerializeTpVag() => !string.IsNullOrWhiteSpace(TpVag);

        /// <summary>
        /// Verifica se deve serializar o campo NSeq (maior que 0)
        /// </summary>
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
        /// <summary>
        /// Nacionalidade da Aeronave
        /// </summary>
        [XmlElement("nac")]
        public string Nac { get; set; }

        /// <summary>
        /// Matrícula da Aeronave
        /// </summary>
        [XmlElement("matr")]
        public string Matr { get; set; }

        /// <summary>
        /// Número do Voo
        /// </summary>
        [XmlElement("nVoo")]
        public string NVoo { get; set; }

        /// <summary>
        /// Código do Aeroporto de Embarque
        /// </summary>
        [XmlElement("cAerEmb")]
        public string CAerEmb { get; set; }

        /// <summary>
        /// Código do Aeroporto de Desembarque
        /// </summary>
        [XmlElement("cAerDes")]
        public string CAerDes { get; set; }

        /// <summary>
        /// Data do Voo
        /// </summary>
        [XmlIgnore]
        public DateTime DVoo { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DVoo" para atribuir ou resgatar o valor)
        /// </summary>
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
        /// <summary>
        /// Identificador do Registro de Informações da Navegação
        /// </summary>
        [XmlElement("irin")]
        public string Irin { get; set; }

        /// <summary>
        /// Tipo da Embarcação
        /// </summary>
        [XmlElement("tpEmb")]
        public string TpEmb { get; set; }

        /// <summary>
        /// Código da Embarcação
        /// </summary>
        [XmlElement("cEmbar")]
        public string CEmbar { get; set; }

        /// <summary>
        /// Nome da Embarcação
        /// </summary>
        [XmlElement("xEmbar")]
        public string XEmbar { get; set; }

        /// <summary>
        /// Número da Viagem
        /// </summary>
        [XmlElement("nViag")]
        public long NViag { get; set; }

        /// <summary>
        /// Código do Porto de Embarque
        /// </summary>
        [XmlElement("cPrtEmb")]
        public string CPrtEmb { get; set; }

        /// <summary>
        /// Código do Porto de Desembarque
        /// </summary>
        [XmlElement("cPrtDest")]
        public string CPrtDest { get; set; }

        /// <summary>
        /// Porto de Transbordo
        /// </summary>
        [XmlElement("prtTrans")]
        public string PrtTrans { get; set; }

        /// <summary>
        /// Tipo de Navegação
        /// </summary>
        [XmlElement("tpNav")]
        public TipoNavegacao TpNav { get; set; }

        /// <summary>
        /// Informações do Terminal de Carregamento
        /// </summary>
        [XmlElement("infTermCarreg")]
        public List<InfTermCarreg> InfTermCarreg { get; set; }

        /// <summary>
        /// Informações do Terminal de Descarregamento
        /// </summary>
        [XmlElement("infTermDescarreg")]
        public List<InfTermDescarreg> InfTermDescarreg { get; set; }

        /// <summary>
        /// Informações da Embarcação de Combustível
        /// </summary>
        [XmlElement("infEmbComb")]
        public List<InfEmbComb> InfEmbComb { get; set; }

        /// <summary>
        /// Informações da Unidade de Carga Vazia
        /// </summary>
        [XmlElement("infUnidCargaVazia")]
        public List<InfUnidCargaVazia> InfUnidCargaVazia { get; set; }

        /// <summary>
        /// Informações da Unidade de Transporte Vazia
        /// </summary>
        [XmlElement("infUnidTranspVazia")]
        public List<InfUnidTranspVazia> InfUnidTranspVazia { get; set; }

        /// <summary>
        /// Maritime Mobile Service Identify (MMSI) - Preencher com o MMSI (Maritime Mobile Service Identify) fornecido pela ANATEL ou autoridade de telecomunicações de origem da embarcação
        /// </summary>
        [XmlElement("MMSI")]
        public string MMSI { get; set; }

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
        public bool ShouldSerializeTpNav() => TpNav != TipoNavegacao.NaoDefinido;
        public bool ShouldSerializeMMSI() => !string.IsNullOrWhiteSpace(MMSI);

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
        /// <summary>
        /// Código do Terminal de Carregamento
        /// </summary>
        [XmlElement("cTermCarreg")]
        public string CTermCarreg { get; set; }

        /// <summary>
        /// Nome do Terminal de Carregamento
        /// </summary>
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
        /// <summary>
        /// Código do Terminal de Descarregamento
        /// </summary>
        [XmlElement("cTermDescarreg")]
        public string CTermDescarreg { get; set; }

        /// <summary>
        /// Nome do Terminal de Descarregamento
        /// </summary>
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
        /// <summary>
        /// Código da Embarcação de Combustível
        /// </summary>
        [XmlElement("cEmbComb")]
        public string CEmbComb { get; set; }

        /// <summary>
        /// Nome da Balsa
        /// </summary>
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
        /// <summary>
        /// Identificador da Unidade de Carga Vazia
        /// </summary>
        [XmlElement("idUnidCargaVazia")]
        public string IdUnidCargaVazia { get; set; }

        /// <summary>
        /// Tipo da Unidade de Carga Vazia
        /// </summary>
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

        /// <summary>
        /// Identificador da Unidade de Transporte Vazia
        /// </summary>
        [XmlElement("idUnidTranspVazia")]
        public string IdUnidTranspVazia { get; set; }

        /// <summary>
        /// Tipo da Unidade de Transporte Vazia
        /// </summary>
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
        /// <summary>
        /// Informações dos Municípios de Descarregamento
        /// </summary>
        [XmlElement("infMunDescarga")]
        public List<InfMunDescarga> InfMunDescarga { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Informações dos Municípios de Descarregamento
        /// </summary>
        /// <param name="infmundescarga">Elemento InfMunDescarga a ser adicionado</param>
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
        /// <summary>
        /// Código do Município de Descarregamento
        /// </summary>
        [XmlElement("cMunDescarga")]
        public long CMunDescarga { get; set; }

        /// <summary>
        /// Nome do Município de Descarregamento
        /// </summary>
        [XmlElement("xMunDescarga")]
        public string XMunDescarga { get; set; }

        /// <summary>
        /// Informações dos CTes
        /// </summary>
        [XmlElement("infCTe")]
        public List<InfMunDescargaInfCTe> InfCTe { get; set; }

        /// <summary>
        /// Informações das NFes
        /// </summary>
        [XmlElement("infNFe")]
        public List<InfMunDescargaInfNFe> InfNFe { get; set; }

        /// <summary>
        /// Informações dos MDFes de Transporte
        /// </summary>
        [XmlElement("infMDFeTransp")]
        public List<InfMunDescargaInfMDFeTransp> InfMDFeTransp { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Informações dos MDFes de Transporte
        /// </summary>
        /// <param name="infMDFeTransp">Elemento InfMunDescargaInfMDFeTransp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Informações das NFes
        /// </summary>
        /// <param name="infNFe">Elemento InfMunDescargaInfNFe a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Informações dos CTes
        /// </summary>
        /// <param name="infCTe">Elemento InfMunDescargaInfCTe a ser adicionado</param>
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
        /// <summary>
        /// Chave de acesso do CT-e
        /// </summary>
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }

        /// <summary>
        /// Segundo código de barras
        /// </summary>
        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        /// <summary>
        /// Indicador de Reentrega
        /// </summary>
        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        /// <summary>
        /// Informações das Unidades de Transporte
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        /// <summary>
        /// Período (Somente se informado o grupo de informações das Unidades de Transporte)
        /// </summary>
        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        /// <summary>
        /// Informações da Entrega Parcial
        /// </summary>
        [XmlElement("infEntregaParcial")]
        public InfEntregaParcial InfEntregaParcial { get; set; }

        /// <summary>
        /// Prestação é parcial? Sim ou Não (1 ou 0)
        /// </summary>
        [XmlElement("indPrestacaoParcial")]
        public SimNao IndPrestacaoParcial { get; set; } = SimNao.Nao;

        /// <summary>
        /// Grupo de informações das NFe entregues na prestação parcial do CTe (Este grupo sempre é informado quando indPrestacaoParcial for igual a Sim (1))
        /// </summary>
        [XmlElement("infNFePresParcial")]
        public List<InfNFePresParcial> InfNFePresParcial { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo SegCodBarra (não vazio)
        /// </summary>
        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);

        /// <summary>
        /// Verifica se deve serializar o campo IndReentrega (Sim)
        /// </summary>
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        /// <summary>
        /// Verifica se deve serializar o campo IndPrestacaoParcial (Sim)
        /// </summary>
        public bool ShouldSerializeIndPrestacaoParcial() => IndPrestacaoParcial == SimNao.Sim;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Informações das Unidades de Transporte
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Períodos
        /// </summary>
        /// <param name="peri">Elemento Peri a ser adicionado</param>
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

        /// <summary>
        /// Adicionar novo elemento à lista de Informações das NFes de Prestação Parcial
        /// </summary>
        /// <param name="elemento">Elemento InfNFePresParcial a ser adicionado</param>
        public void AddInfNFePresParcial(InfNFePresParcial elemento)
        {
            if (InfNFePresParcial == null)
            {
                InfNFePresParcial = new List<InfNFePresParcial>();
            }

            InfNFePresParcial.Add(elemento);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFePresParcial (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da InfNFePresParcial</returns>
        public InfNFePresParcial GetInfNFePresParcial(int index)
        {
            if ((InfNFePresParcial?.Count ?? 0) == 0)
            {
                return default;
            };

            return InfNFePresParcial[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFePresParcial
        /// </summary>
        public int GetInfNFePresParcialCount => (InfNFePresParcial != null ? InfNFePresParcial.Count : 0);

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
        /// <summary>
        /// Tipo da Unidade de Transporte
        /// </summary>
        [XmlElement("tpUnidTransp")]
        public TipoUnidadeTransporte TpUnidTransp { get; set; }

        /// <summary>
        /// Identificação da Unidade de Transporte
        /// </summary>
        [XmlElement("idUnidTransp")]
        public string IdUnidTransp { get; set; }

        /// <summary>
        /// Lacres da Unidade de Transporte
        /// </summary>
        [XmlElement("lacUnidTransp")]
        public List<LacUnidTransp> LacUnidTransp { get; set; }

        /// <summary>
        /// Informações das Unidades de Carga
        /// </summary>
        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        /// <summary>
        /// Quantidade rateada
        /// </summary>
        [XmlIgnore]
        public double QtdRat { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "QtdRat" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qtdRat")]
        public string QtdRatField
        {
            get => QtdRat.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdRat = Utility.Converter.ToDouble(value);
        }


        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo QtdRatField (maior que 0)
        /// </summary>
        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Lacres da Unidade de Transporte
        /// </summary>
        /// <param name="lacUnidTransp">Elemento LacUnidTransp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Informações das Unidades de Carga
        /// </summary>
        /// <param name="infUnidCarga">Elemento InfUnidCarga a ser adicionado</param>
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
        /// <summary>
        /// Tipo da Unidade de Carga
        /// </summary>
        [XmlElement("tpUnidCarga")]
        public TipoUnidadeCarga TpUnidCarga { get; set; }

        /// <summary>
        /// Identificação da Unidade de Carga
        /// </summary>
        [XmlElement("idUnidCarga")]
        public string IdUnidCarga { get; set; }

        /// <summary>
        /// Lacres da Unidade de Carga
        /// </summary>
        [XmlElement("lacUnidCarga")]
        public List<LacUnidCarga> LacUnidCarga { get; set; }

        /// <summary>
        /// Quantidade rateada
        /// </summary>
        [XmlIgnore]
        public double QtdRat { get; set; }

        /// <summary>
        /// Campo para serialização da Quantidade rateada
        /// </summary>
        [XmlElement("qtdRat")]
        public string QtdRatField
        {
            get => QtdRat.ToString("F2", CultureInfo.InvariantCulture);
            set => QtdRat = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo QtdRatField (maior que 0)
        /// </summary>
        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Lacres da Unidade de Carga
        /// </summary>
        /// <param name="lacUnidCarga">Elemento LacUnidCarga a ser adicionado</param>
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
        /// <summary>
        /// Número ONU
        /// </summary>
        [XmlElement("nONU")]
        public string NONU { get; set; }

        /// <summary>
        /// Nome apropriado para embarque
        /// </summary>
        [XmlElement("xNomeAE")]
        public string XNomeAE { get; set; }

        /// <summary>
        /// Classe de risco
        /// </summary>
        [XmlElement("xClaRisco")]
        public string XClaRisco { get; set; }

        /// <summary>
        /// Grupo de embalagem
        /// </summary>
        [XmlElement("grEmb")]
        public string GrEmb { get; set; }

        /// <summary>
        /// Quantidade total do produto
        /// </summary>
        [XmlElement("qTotProd")]
        public string QTotProd { get; set; }

        /// <summary>
        /// Quantidade e tipo de volumes
        /// </summary>
        [XmlElement("qVolTipo")]
        public string QVolTipo { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo XNomeAE (não vazio)
        /// </summary>
        public bool ShouldSerializeXNomeAE() => !string.IsNullOrWhiteSpace(XNomeAE);

        /// <summary>
        /// Verifica se deve serializar o campo XClaRisco (não vazio)
        /// </summary>
        public bool ShouldSerializeXClaRisco() => !string.IsNullOrWhiteSpace(XClaRisco);

        /// <summary>
        /// Verifica se deve serializar o campo GrEmb (não vazio)
        /// </summary>
        public bool ShouldSerializeGrEmb() => !string.IsNullOrWhiteSpace(GrEmb);

        /// <summary>
        /// Verifica se deve serializar o campo QVolTipo (não vazio)
        /// </summary>
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
        /// <summary>
        /// Quantidade total
        /// </summary>
        [XmlIgnore]
        public double QtdTotal { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "QtdTotal" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qtdTotal")]
        public string QtdTotalField
        {
            get => QtdTotal.ToString("F4", CultureInfo.InvariantCulture);
            set => QtdTotal = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade parcial
        /// </summary>
        [XmlIgnore]
        public double QtdParcial { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "QtdParcial" para atribuir ou resgatar o valor)
        /// </summary>
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
        /// <summary>
        /// Chave de acesso da NF-e
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Segundo código de barras
        /// </summary>
        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        /// <summary>
        /// Indicador de Reentrega
        /// </summary>
        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        /// <summary>
        /// Informações das Unidades de Transporte
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        /// <summary>
        /// Período (Somente se informado o grupo de informações das Unidades de Transporte)
        /// </summary>
        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo SegCodBarra (não vazio)
        /// </summary>
        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);

        /// <summary>
        /// Verifica se deve serializar o campo IndReentrega (Sim)
        /// </summary>
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Informações das Unidades de Transporte
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Períodos
        /// </summary>
        /// <param name="peri">Elemento Peri a ser adicionado</param>
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
        /// <summary>
        /// Chave de acesso do MDF-e de transporte
        /// </summary>
        [XmlElement("chMDFe")]
        public string ChMDFe { get; set; }

        /// <summary>
        /// Segundo código de barras
        /// </summary>
        [XmlElement("SegCodBarra")]
        public string SegCodBarra { get; set; }

        /// <summary>
        /// Indicador de Reentrega
        /// </summary>
        [XmlElement("indReentrega")]
        public SimNao IndReentrega { get; set; }

        /// <summary>
        /// Informações das Unidades de Transporte
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

        /// <summary>
        /// Período (Somente se informado o grupo de informações das Unidades de Transporte)
        /// </summary>
        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo SegCodBarra (não vazio)
        /// </summary>
        public bool ShouldSerializeSegCodBarra() => !string.IsNullOrWhiteSpace(SegCodBarra);

        /// <summary>
        /// Verifica se deve serializar o campo IndReentrega (Sim)
        /// </summary>
        public bool ShouldSerializeIndReentrega() => IndReentrega == SimNao.Sim;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Informações das Unidades de Transporte
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado</param>
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
        /// Adicionar novo elemento à lista de Períodos
        /// </summary>
        /// <param name="peri">Elemento Peri a ser adicionado</param>
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
        /// <summary>
        /// Informações do Responsável pelo Seguro
        /// </summary>
        [XmlElement("infResp")]
        public InfResp InfResp { get; set; }

        /// <summary>
        /// Informações do Seguro
        /// </summary>
        [XmlElement("infSeg")]
        public InfSeg InfSeg { get; set; }

        /// <summary>
        /// Número da Apólice
        /// </summary>
        [XmlElement("nApol")]
        public string NApol { get; set; }

        /// <summary>
        /// Números de Averbação
        /// </summary>
        [XmlElement("nAver")]
        public List<string> NAver { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de Números de Averbação
        /// </summary>
        /// <param name="naver">Elemento string a ser adicionado</param>
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
        /// <summary>
        /// Responsável pelo Seguro
        /// </summary>
        [XmlElement("respSeg")]
        public ResponsavelSeguroMDFe RespSeg { get; set; }

        /// <summary>
        /// CNPJ do Responsável pelo Seguro
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Responsável pelo Seguro
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
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
        /// <summary>
        /// Nome da Seguradora
        /// </summary>
        [XmlElement("xSeg")]
        public string XSeg { get; set; }

        /// <summary>
        /// CNPJ da Seguradora
        /// </summary>
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
        /// <summary>
        /// Tipo da Carga
        /// </summary>
        [XmlElement("tpCarga")]
        public TipoCargaMDFe TpCarga { get; set; }

        /// <summary>
        /// Descrição do Produto Predominante
        /// </summary>
        [XmlElement("xProd")]
        public string XProd { get; set; }

        /// <summary>
        /// Código de barras GTIN (Global Trade Item Number) do produto, antigo código EAN ou código de barras padrão da GS1
        /// </summary>
        [XmlElement("cEAN")]
        public string CEAN { get; set; }

        /// <summary>
        /// Código NCM (Nomenclatura Comum do Mercosul)
        /// </summary>
        [XmlElement("NCM")]
        public string NCM { get; set; }

        /// <summary>
        /// Informações da Lotacão
        /// </summary>
        [XmlElement("infLotacao")]
        public InfLotacao InfLotacao { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CEAN (não vazio)
        /// </summary>
        public bool ShouldSerializeCEAN() => !string.IsNullOrWhiteSpace(CEAN);

        /// <summary>
        /// Verifica se deve serializar o campo NCM (não vazio)
        /// </summary>
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
        /// <summary>
        /// Informações do Local de Carregamento
        /// </summary>
        [XmlElement("infLocalCarrega")]
        public InfLocalCarrega InfLocalCarrega { get; set; }

        /// <summary>
        /// Informações do Local de Descarregamento
        /// </summary>
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
        /// <summary>
        /// CEP do Local de Carregamento
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Latitude do Local de Carregamento
        /// </summary>
        [XmlElement("latitude")]
        public string Latitude { get; set; }

        /// <summary>
        /// Longitude do Local de Carregamento
        /// </summary>
        [XmlElement("longitude")]
        public string Longitude { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo CEP (não vazio)
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        /// <summary>
        /// Verifica se deve serializar o campo Latitude (não vazio e CEP vazio)
        /// </summary>
        public bool ShouldSerializeLatitude() => !string.IsNullOrWhiteSpace(Latitude) && string.IsNullOrWhiteSpace(CEP);

        /// <summary>
        /// Verifica se deve serializar o campo Longitude (não vazio e CEP vazio)
        /// </summary>
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
        /// <summary>
        /// Quantidade de CT-e
        /// </summary>
        [XmlElement("qCTe")]
        public int QCTe { get; set; }

        /// <summary>
        /// Quantidade de NF-e
        /// </summary>
        [XmlElement("qNFe")]
        public int QNFe { get; set; }

        /// <summary>
        /// Quantidade de MDF-e
        /// </summary>
        [XmlElement("qMDFe")]
        public int QMDFe { get; set; }

        /// <summary>
        /// Valor Total da Carga
        /// </summary>
        [XmlIgnore]
        public double VCarga { get; set; }

        /// <summary>
        /// Campo para serialização do Valor Total da Carga
        /// </summary>
        [XmlElement("vCarga")]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Código da Unidade de Medida
        /// </summary>
        [XmlElement("cUnid")]
        public CodigoUnidadeMedidaMDFe CUnid { get; set; }

        /// <summary>
        /// Quantidade Total da Carga
        /// </summary>
        [XmlIgnore]
        public double QCarga { get; set; }

        /// <summary>
        /// Campo para serialização da Quantidade Total da Carga
        /// </summary>
        [XmlElement("qCarga")]
        public string QCargaField
        {
            get => QCarga.ToString("F4", CultureInfo.InvariantCulture);
            set => QCarga = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo QCTe (maior que 0)
        /// </summary>
        public bool ShouldSerializeQCTe() => QCTe > 0;

        /// <summary>
        /// Verifica se deve serializar o campo QNFe (maior que 0)
        /// </summary>
        public bool ShouldSerializeQNFe() => QNFe > 0;

        /// <summary>
        /// Verifica se deve serializar o campo QMDFe (maior que 0)
        /// </summary>
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
        /// <summary>
        /// Número do Lacre
        /// </summary>
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

        /// <summary>
        /// CNPJ Autorizado
        /// </summary>
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

        /// <summary>
        /// CPF Autorizado
        /// </summary>
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

        /// <summary>
        /// Verifica se deve serializar o campo CNPJ (não vazio)
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se deve serializar o campo CPF (não vazio)
        /// </summary>
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
        /// <summary>
        /// Informações Adicionais de Interesse do Fisco
        /// </summary>
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        /// <summary>
        /// Informações Complementares de Interesse do Contribuinte
        /// </summary>
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
        /// <summary>
        /// CNPJ do Responsável Técnico
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Nome da Pessoa a ser contatada
        /// </summary>
        [XmlElement("xContato")]
        public string XContato { get; set; }

        /// <summary>
        /// Email do Responsável Técnico
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        /// <summary>
        /// Telefone do Responsável Técnico
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Identificador do CSRT
        /// </summary>
        [XmlElement("idCSRT")]
        public string IdCSRT { get; set; }

        /// <summary>
        /// Hash do CSRT
        /// </summary>
        [XmlElement("hashCSRT", DataType = "base64Binary")]
        public byte[] HashCSRT { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se deve serializar o campo IdCSRT (não vazio)
        /// </summary>
        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);

        /// <summary>
        /// Verifica se deve serializar o campo HashCSRT (não nulo)
        /// </summary>
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
        /// <summary>
        /// Código QR do MDF-e
        /// </summary>
        [XmlElement("qrCodMDFe")]
        public string QrCodMDFe { get; set; }
    }

    /// <summary> 
    /// Grupo de informações das NFe entregues na prestação parcial do CTe(Este grupo sempre é informado quando indPrestacaoParcial estiver informado)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.MDFe.InfNFePresParcial")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/mdfe")]
    public class InfNFePresParcial
    {
        /// <summary>
        /// Chave de acesso da NFe entregue na prestação parcial do CTe relacionado
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }
    }
}
