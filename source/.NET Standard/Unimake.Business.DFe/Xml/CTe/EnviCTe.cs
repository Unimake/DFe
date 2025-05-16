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
    /// <summary>
    /// Classe para envio de Conhecimento de transporte eletrônico (CTe)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnviCTe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("enviCTe", Namespace = "http://www.portalfiscal.inf.br/cte", IsNullable = false)]
    public class EnviCTe : XMLBase
    {
        /// <summary>
        /// Versão do leiaute utilizado.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificador de controle do lote de envio do CTe.
        /// </summary>
        [XmlElement("idLote")]
        public string IdLote { get; set; }

        /// <summary>
        /// Lista de Conhecimentos de Transporte Eletrônicos (CTe).
        /// </summary>
        [XmlElement("CTe")]
        public List<CTe> CTe { get; set; }

        /// <summary>
        /// Gera o XML do objeto EnviCTe.
        /// </summary>
        /// <returns>Documento XML gerado.</returns>
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
        /// Adicionar novo elemento a lista CTe.
        /// </summary>
        /// <param name="cte">Elemento CTe a ser adicionado.</param>
        public void AddCTe(CTe cte)
        {
            if (CTe == null)
            {
                CTe = new List<CTe>();
            }

            CTe.Add(cte);
        }

        /// <summary>
        /// Retorna o elemento da lista CTe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da CTe.</returns>
        public CTe GetCTe(int index)
        {
            if ((CTe?.Count ?? 0) == 0)
            {
                return default;
            }

            return CTe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CTe.
        /// </summary>
        public int GetCTeCount => (CTe != null ? CTe.Count : 0);

#endif

        #endregion
    }

    /// <summary>
    /// Conhecimento de transporte eletrônico
    /// </summary>
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
        /// <summary>
        /// Informações do CTe.
        /// </summary>
        [XmlElement("infCte")]
        public InfCTe InfCTe { get; set; }

        /// <summary>
        /// Informações suplementares do CTe.
        /// </summary>
        [XmlElement("infCTeSupl")]
        public InfCTeSupl InfCTeSupl { get; set; }

        /// <summary>
        /// Assinatura digital do CTe.
        /// </summary>
        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

        /// <summary>
        /// Desserializar o XML no objeto CTe.
        /// </summary>
        /// <param name="filename">Localização do arquivo XML.</param>
        /// <returns>Objeto do CTe.</returns>
        public CTe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<CTe>(doc);
        }

        /// <summary>
        /// Desserializar o XML CTe no objeto CTe.
        /// </summary>
        /// <param name="xml">string do XML CTe.</param>
        /// <returns>Objeto da CTe.</returns>
        public CTe LoadFromXML(string xml) => XMLUtility.Deserializar<CTe>(xml);
    }

    /// <summary>
    /// Informações do CTe.
    /// </summary>
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

        /// <summary>
        /// Versão do leiaute utilizado.
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificação do CTe.
        /// </summary>
        [XmlElement("ide")]
        public Ide Ide { get; set; }

        /// <summary>
        /// Informações complementares do CTe.
        /// </summary>
        [XmlElement("compl")]
        public Compl Compl { get; set; }

        /// <summary>
        /// Informações do emitente do CTe.
        /// </summary>
        [XmlElement("emit")]
        public Emit Emit { get; set; }

        /// <summary>
        /// Informações do remetente do CTe.
        /// </summary>
        [XmlElement("rem")]
        public Rem Rem { get; set; }

        /// <summary>
        /// Informações do expedidor do CTe.
        /// </summary>
        [XmlElement("exped")]
        public Exped Exped { get; set; }

        /// <summary>
        /// Informações do recebedor do CTe.
        /// </summary>
        [XmlElement("receb")]
        public Receb Receb { get; set; }

        /// <summary>
        /// Informações do destinatário do CTe.
        /// </summary>
        [XmlElement("dest")]
        public Dest Dest { get; set; }

        /// <summary>
        /// Informações dos valores da prestação de serviço.
        /// </summary>
        [XmlElement("vPrest")]
        public VPrest VPrest { get; set; }

        /// <summary>
        /// Informações dos impostos do CTe.
        /// </summary>
        [XmlElement("imp")]
        public Imp Imp { get; set; }

        /// <summary>
        /// Informações do CTe normal.
        /// </summary>
        [XmlElement("infCTeNorm")]
        public InfCTeNorm InfCTeNorm { get; set; }

        /// <summary>
        /// Informações do CTe complementar.
        /// </summary>
        [XmlElement("infCteComp")]
        public InfCteComp InfCteComp { get; set; }

        /// <summary>
        /// Informações do CTe de anulação.
        /// </summary>
        [XmlElement("infCteAnu")]
        public InfCteAnu InfCteAnu { get; set; }

        /// <summary>
        /// Lista de pessoas autorizadas a acessar o XML do CTe.
        /// </summary>
        [XmlElement("autXML")]
        public List<AutXML> AutXML { get; set; }

        /// <summary>
        /// Informações do responsável técnico pelo sistema emissor do CTe.
        /// </summary>
        [XmlElement("infRespTec")]
        public InfRespTec InfRespTec { get; set; }

        /// <summary>
        /// Grupo de Informações do Pedido da NFF
        /// </summary>
        [XmlElement("infSolicNFF")]
        public InfSolicNFF InfSolicNFF { get; set; }

        /// <summary>
        /// ID: "CTe" + Chave.
        /// </summary>
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

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "Chave" para atribuir ou resgatar o valor)
        /// </summary>
        private string ChaveField;

        /// <summary>
        /// ChaveCTe.
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

    /// <summary>
    /// Identificação do CTe.
    /// </summary>
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

        /// <summary>
        /// Código da UF do emitente do CTe.
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
        /// Código numérico que compõe a Chave de Acesso.
        /// </summary>
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

        /// <summary>
        /// Código Fiscal de Operações e Prestações.
        /// </summary>
        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        /// <summary>
        /// Natureza da Operação.
        /// </summary>
        [XmlElement("natOp")]
        public string NatOp { get; set; }

        /// <summary>
        /// Modelo do CTe.
        /// </summary>
        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        /// <summary>
        /// Série do CTe.
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do CTe.
        /// </summary>
        [XmlElement("nCT")]
        public int NCT { get; set; }

        /// <summary>
        /// Data e hora de emissão do CTe.
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
        /// Tipo de Impressão do DACTE.
        /// </summary>
        [XmlElement("tpImp")]
        public FormatoImpressaoDACTE TpImp { get; set; }

        /// <summary>
        /// Tipo de Emissão do CTe.
        /// </summary>
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

        /// <summary>
        /// Dígito Verificador da Chave de Acesso.
        /// </summary>
        [XmlElement("cDV")]
        public int CDV { get; set; }

        /// <summary>
        /// Tipo de Ambiente.
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Tipo do CTe.
        /// </summary>
        [XmlElement("tpCTe")]
        public TipoCTe TpCTe { get; set; }

        /// <summary>
        /// Processo de Emissão do CTe.
        /// </summary>
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

        /// <summary>
        /// Versão do Processo de Emissão.
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        /// <summary>
        /// Indicador de CTe Globalizado.
        /// </summary>
        [XmlElement("indGlobalizado")]
        public SimNao IndGlobalizado { get; set; }

        /// <summary>
        /// Código do Município de envio do CTe.
        /// </summary>
        [XmlElement("cMunEnv")]
        public string CMunEnv { get; set; }

        /// <summary>
        /// Nome do Município de envio do CTe.
        /// </summary>
        [XmlElement("xMunEnv")]
        public string XMunEnv { get; set; }

        /// <summary>
        /// Sigla da UF de envio do CTe.
        /// </summary>
        [XmlElement("UFEnv")]
        public UFBrasil UFEnv { get; set; }

        /// <summary>
        /// Modalidade do Transporte.
        /// </summary>
        [XmlElement("modal")]
        public ModalidadeTransporteCTe Modal { get; set; }

        /// <summary>
        /// Tipo do Serviço.
        /// </summary>
        [XmlElement("tpServ")]
        public TipoServicoCTe TpServ { get; set; }

        /// <summary>
        /// Código do Município de início da prestação.
        /// </summary>
        [XmlElement("cMunIni")]
        public string CMunIni { get; set; }

        /// <summary>
        /// Nome do Município de início da prestação.
        /// </summary>
        [XmlElement("xMunIni")]
        public string XMunIni { get; set; }

        /// <summary>
        /// Sigla da UF de início da prestação.
        /// </summary>
        [XmlElement("UFIni")]
        public UFBrasil UFIni { get; set; }

        /// <summary>
        /// Código do Município de término da prestação.
        /// </summary>
        [XmlElement("cMunFim")]
        public string CMunFim { get; set; }

        /// <summary>
        /// Nome do Município de término da prestação.
        /// </summary>
        [XmlElement("xMunFim")]
        public string XMunFim { get; set; }

        /// <summary>
        /// Sigla da UF de término da prestação.
        /// </summary>
        [XmlElement("UFFim")]
        public UFBrasil UFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "Retira" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("retira")]
        public string RetiraField { get; set; }

        /// <summary>
        /// Indica se o Recebedor retira no Remetente ou redespachador
        /// </summary>
        [XmlIgnore]
        public SimNao Retira
        {
            get => (RetiraField.Equals("0") ? SimNao.Sim : SimNao.Nao);
            set => RetiraField = (value == SimNao.Sim ? "0" : "1");
        }

        /// <summary>
        /// Detalhes do local de retirada.
        /// </summary>
        [XmlElement("xDetRetira")]
        public string XDetRetira { get; set; }

        /// <summary>
        /// Indicador da IE do tomador do serviço.
        /// </summary>
        [XmlElement("indIEToma")]
        public IndicadorIEDestinatario IndIEToma { get; set; }

        /// <summary>
        /// Informações do tomador do serviço (versão 3.00 ou anterior).
        /// </summary>
        [XmlElement("toma3")]
        public Toma3 Toma3 { get; set; }

        /// <summary>
        /// Informações do tomador do serviço (versão 4.00 ou posterior).
        /// </summary>
        [XmlElement("toma4")]
        public Toma4 Toma4 { get; set; }

        /// <summary>
        /// Data e hora de entrada em contingência.
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset DhCont { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DhCont" para atribuir ou resgatar o valor)
        /// </summary>
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

        /// <summary>
        /// Justificativa da entrada em contingência.
        /// </summary>
        [XmlElement("xJust")]
        public string XJust { get; set; }

        /// <summary>
        /// Grupo de compra governamental
        /// </summary>
        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade IndGlobalizado deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIndGlobalizado() => IndGlobalizado == SimNao.Sim;

        /// <summary>
        /// Verifica se a propriedade DhContField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        /// <summary>
        /// Verifica se a propriedade XJust deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        #endregion
    }

#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GCompraGov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GCompraGov
    {
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

    /// <summary>
    /// Informações do tomador do serviço 
    /// </summary>
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

        /// <summary>
        /// Informações do tomador do serviço 
        /// </summary>
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

    /// <summary>
    /// Informações do tomador do serviço
    /// </summary>
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


        /// <summary>
        /// Informações do tomador do serviço 
        /// </summary>
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

        /// <summary>
        /// CNPJ do tomador do serviço.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do tomador do serviço.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do tomador do serviço.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do tomador do serviço.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Nome Fantasia do tomador do serviço.
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Telefone do tomador do serviço.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Endereço do tomador do serviço.
        /// </summary>
        [XmlElement("enderToma")]
        public EnderToma EnderToma { get; set; }

        /// <summary>
        /// Email do tomador do serviço.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se a propriedade XFant deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade Email deve ser serializada.
        /// </summary>
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Informações sobre o endereço do tomador do serviço.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderToma")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderToma
    {
        /// <summary>
        /// Logradouro do tomador do serviço.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número do endereço do tomador do serviço.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do tomador do serviço.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do tomador do serviço.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do tomador do serviço.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do tomador do serviço.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do tomador do serviço.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Sigla da UF do tomador do serviço.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Código do país do tomador do serviço.
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país do tomador do serviço.
        /// </summary>
        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPais() => CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

    /// <summary>
    /// Informações complementares.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Compl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Compl
    {
        /// <summary>
        /// Características adicionais da prestação.
        /// </summary>
        [XmlElement("xCaracAd")]
        public string XCaracAd { get; set; }

        /// <summary>
        /// Características da prestação do serviço.
        /// </summary>
        [XmlElement("xCaracSer")]
        public string XCaracSer { get; set; }

        /// <summary>
        /// Emitente.
        /// </summary>
        [XmlElement("xEmi")]
        public string XEmi { get; set; }

        /// <summary>
        /// Informações do fluxo da prestação.
        /// </summary>
        [XmlElement("fluxo")]
        public Fluxo Fluxo { get; set; }

        /// <summary>
        /// Informações de entrega.
        /// </summary>
        [XmlElement("Entrega")]
        public Entrega Entrega { get; set; }

        /// <summary>
        /// Origem do cálculo do frete.
        /// </summary>
        [XmlElement("origCalc")]
        public string OrigCalc { get; set; }

        /// <summary>
        /// Destino do cálculo do frete.
        /// </summary>
        [XmlElement("destCalc")]
        public string DestCalc { get; set; }

        /// <summary>
        /// Observações gerais.
        /// </summary>
        [XmlElement("xObs")]
        public string XObs { get; set; }

        /// <summary>
        /// Observações do contribuinte.
        /// </summary>
        [XmlElement("ObsCont")]
        public List<ObsCont> ObsCont { get; set; }

        /// <summary>
        /// Observações do fisco.
        /// </summary>
        [XmlElement("ObsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

        #region Add (List - Interop)

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista ObsCont.
        /// </summary>
        /// <param name="obsCont">Elemento ObsCont a ser adicionado.</param>
        public void AddObsCont(ObsCont obsCont)
        {
            if (ObsCont == null)
            {
                ObsCont = new List<ObsCont>();
            }

            ObsCont.Add(obsCont);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsCont.</returns>
        public ObsCont GetObsCont(int index)
        {
            if ((ObsCont?.Count ?? 0) == 0)
            {
                return default;
            }

            return ObsCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsCont.
        /// </summary>
        public int GetObsContCount => (ObsCont != null ? ObsCont.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista ObsFisco.
        /// </summary>
        /// <param name="obsFisco">Elemento ObsFisco a ser adicionado.</param>
        public void AddObsFisco(ObsFisco obsFisco)
        {
            if (ObsFisco == null)
            {
                ObsFisco = new List<ObsFisco>();
            }

            ObsFisco.Add(obsFisco);
        }

        /// <summary>
        /// Retorna o elemento da lista ObsFisco (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da ObsFisco.</returns>
        public ObsFisco GetObsFisco(int index)
        {
            if ((ObsFisco?.Count ?? 0) == 0)
            {
                return default;
            }

            return ObsFisco[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ObsFisco.
        /// </summary>
        public int GetObsFiscoCount => (ObsFisco != null ? ObsFisco.Count : 0);

#endif

        #endregion

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade XCaracAd deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCaracAd() => !string.IsNullOrWhiteSpace(XCaracAd);

        /// <summary>
        /// Verifica se a propriedade XCaracSer deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCaracSer() => !string.IsNullOrWhiteSpace(XCaracSer);

        /// <summary>
        /// Verifica se a propriedade XEmi deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXEmi() => !string.IsNullOrWhiteSpace(XEmi);

        /// <summary>
        /// Verifica se a propriedade OrigCalc deve ser serializada.
        /// </summary>
        public bool ShouldSerializeOrigCalc() => !string.IsNullOrWhiteSpace(OrigCalc);

        /// <summary>
        /// Verifica se a propriedade DestCalc deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDestCalc() => !string.IsNullOrWhiteSpace(DestCalc);

        /// <summary>
        /// Verifica se a propriedade XObs deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXObs() => !string.IsNullOrWhiteSpace(XObs);

        #endregion
    }

    /// <summary>
    /// Informações do fluxo da prestação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Fluxo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Fluxo
    {
        /// <summary>
        /// Origem do fluxo de transporte.
        /// </summary>
        [XmlElement("xOrig")]
        public string XOrig { get; set; }

        /// <summary>
        /// Lista de locais de passagem do fluxo de transporte.
        /// </summary>
        [XmlElement("pass")]
        public List<Pass> Pass { get; set; }

        /// <summary>
        /// Destino do fluxo de transporte.
        /// </summary>
        [XmlElement("xDest")]
        public string XDest { get; set; }

        /// <summary>
        /// Rota do fluxo de transporte.
        /// </summary>
        [XmlElement("xRota")]
        public string XRota { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista Pass.
        /// </summary>
        /// <param name="pass">Elemento Pass a ser adicionado.</param>
        public void AddPass(Pass pass)
        {
            if (Pass == null)
            {
                Pass = new List<Pass>();
            }

            Pass.Add(pass);
        }

        /// <summary>
        /// Retorna o elemento da lista Pass (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da Pass.</returns>
        public Pass GetPass(int index)
        {
            if ((Pass?.Count ?? 0) == 0)
            {
                return default;
            }

            return Pass[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Pass.
        /// </summary>
        public int GetPassCount => (Pass != null ? Pass.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade XOrig deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXOrig() => !string.IsNullOrWhiteSpace(XOrig);

        /// <summary>
        /// Verifica se a propriedade XDest deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXDest() => !string.IsNullOrWhiteSpace(XDest);

        /// <summary>
        /// Verifica se a propriedade XRota deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXRota() => !string.IsNullOrWhiteSpace(XRota);

        #endregion
    }

    /// <summary>
    /// Lista de locais de passagem do fluxo de transporte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Pass")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Pass
    {
        /// <summary>
        /// Local de passagem do fluxo de transporte.
        /// </summary>
        [XmlElement("xPass")]
        public string XPass { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade XPass deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPass() => !string.IsNullOrWhiteSpace(XPass);

        #endregion
    }

    /// <summary>
    /// Informações de entrega.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Entrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Entrega
    {
        /// <summary>
        /// Entrega sem data definida.
        /// </summary>
        [XmlElement("semData")]
        public SemData SemData { get; set; }

        /// <summary>
        /// Entrega com data definida.
        /// </summary>
        [XmlElement("comData")]
        public ComData ComData { get; set; }

        /// <summary>
        /// Entrega em período definido.
        /// </summary>
        [XmlElement("noPeriodo")]
        public NoPeriodo NoPeriodo { get; set; }

        /// <summary>
        /// Entrega sem hora definida.
        /// </summary>
        [XmlElement("semHora")]
        public SemHora SemHora { get; set; }

        /// <summary>
        /// Entrega com hora definida.
        /// </summary>
        [XmlElement("comHora")]
        public ComHora ComHora { get; set; }

        /// <summary>
        /// Entrega em intervalo de horas definido.
        /// </summary>
        [XmlElement("noInter")]
        public NoInter NoInter { get; set; }
    }

    /// <summary>
    /// Entrega sem data definida.
    /// </summary>
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

        /// <summary>
        /// Tipo de período de entrega (sem data definida).
        /// </summary>
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

    /// <summary>
    /// Entrega com data definida.
    /// </summary>
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

        /// <summary>
        /// Tipo de período de entrega (com data definida).
        /// </summary>
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

        /// <summary>
        /// Data programada para entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DProg { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DProg" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dProg")]
        public string DProgField
        {
            get => DProg.ToString("yyyy-MM-dd");
            set => DProg = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Entrega em período definido.
    /// </summary>
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

        /// <summary>
        /// Tipo de período de entrega (em período definido).
        /// </summary>
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

        /// <summary>
        /// Data inicial do período de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DIni { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DIni" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dIni")]
        public string DIniField
        {
            get => DIni.ToString("yyyy-MM-dd");
            set => DIni = DateTime.Parse(value);
        }

        /// <summary>
        /// Data final do período de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dFim")]
        public string DFimField
        {
            get => DFim.ToString("yyyy-MM-dd");
            set => DFim = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Entrega sem hora definida.
    /// </summary>
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

    /// <summary>
    /// Observações do contribuinte.
    /// </summary>
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

        /// <summary>
        /// Tipo de hora de entrega (com hora definida).
        /// </summary>
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

        /// <summary>
        /// Hora programada para entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime HProg { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "HProg" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("hProg")]
        public string HProgField
        {
            get => HProg.ToString("HH:mm:ss");
            set => HProg = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Observações do contribuinte.
    /// </summary>
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

        /// <summary>
        /// Tipo de hora de entrega (em intervalo de horas definido).
        /// </summary>
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

        /// <summary>
        /// Hora inicial do intervalo de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime HIni { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "HIni" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("hIni")]
        public string HIniField
        {
            get => HIni.ToString("HH:mm:ss");
            set => HIni = DateTime.Parse(value);
        }

        /// <summary>
        /// Hora final do intervalo de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime HFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "HFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("hFim")]
        public string HFimField
        {
            get => HFim.ToString("HH:mm:ss");
            set => HFim = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Observações do contribuinte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ObsCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ObsCont
    {
        /// <summary>
        /// Texto da observação do contribuinte.
        /// </summary>
        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        /// <summary>
        /// Campo da observação do contribuinte.
        /// </summary>
        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

    /// <summary>
    /// Observação do fisco.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ObsFisco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ObsFisco
    {
        /// <summary>
        /// Texto da observação do fisco.
        /// </summary>
        [XmlElement("xTexto")]
        public string XTexto { get; set; }

        /// <summary>
        /// Campo da observação do fisco.
        /// </summary>
        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

    /// <summary>
    /// Informações do emitente do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Emit
    {
        /// <summary>
        /// CNPJ do emitente.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do emitente.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Inscrição Estadual do Substituto Tributário.
        /// </summary>
        [XmlElement("IEST")]
        public string IEST { get; set; }

        /// <summary>
        /// Razão Social ou Nome do emitente.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Nome Fantasia do emitente.
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Endereço do emitente.
        /// </summary>
        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        /// <summary>
        /// Código de Regime Tributário.
        /// </summary>
        [XmlElement("CRT")]
#if INTEROP
        public CRT CRT { get; set; } = (CRT)(-1);
#else
        public CRT? CRT { get; set; }
#endif

        #region ShouldSerialize

#if INTEROP
        /// <summary>
        /// Verifica se a propriedade CRT deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCRT() => CRT != (CRT)(-1);
#else
        /// <summary>
        /// Verifica se a propriedade CRT deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCRT() => CRT != null;
#endif

        /// <summary>
        /// Verifica se a propriedade IEST deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIEST() => !string.IsNullOrWhiteSpace(IEST);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade XFant deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        #endregion
    }

    /// <summary>
    /// Endereço do emitente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderEmit
    {
        /// <summary>
        /// Logradouro do endereço do emitente.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número do endereço do emitente.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do emitente.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do endereço do emitente.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do endereço do emitente.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do endereço do emitente.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do endereço do emitente.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Sigla da UF do endereço do emitente.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Telefone do endereço do emitente.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

    /// <summary>
    /// Informações do remetente do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Rem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Rem
    {
        /// <summary>
        /// CNPJ do remetente.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do remetente.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do remetente.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do remetente.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Nome Fantasia do remetente.
        /// </summary>
        [XmlElement("xFant")]
        public string XFant { get; set; }

        /// <summary>
        /// Telefone do remetente.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Endereço do remetente.
        /// </summary>
        [XmlElement("enderReme")]
        public EnderReme EnderReme { get; set; }

        /// <summary>
        /// Email do remetente.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade XFant deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXFant() => !string.IsNullOrWhiteSpace(XFant);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se a propriedade Email deve ser serializada.
        /// </summary>
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Endereço do remetente.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderReme")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderReme
    {
        /// <summary>
        /// Logradouro do endereço do remetente.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número do endereço do remetente.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do remetente.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do endereço do remetente.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do endereço do remetente.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do endereço do remetente.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do endereço do remetente.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// UF do endereço do remetente.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Código do país do endereço do remetente.
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país do endereço do remetente.
        /// </summary>
        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPais() => CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

    /// <summary>
    /// Informações do expedidor do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Exped")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Exped
    {
        /// <summary>
        /// CNPJ do expedidor.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do expedidor.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do expedidor.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do expedidor.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Telefone do expedidor.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Endereço do expedidor.
        /// </summary>
        [XmlElement("enderExped")]
        public EnderExped EnderExped { get; set; }

        /// <summary>
        /// Email do expedidor.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se a propriedade Email deve ser serializada.
        /// </summary>
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Endereço do expedidor.
    /// </summary>
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

        /// <summary>
        /// Número do endereço do remetente.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do remetente.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do endereço do remetente.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do endereço do remetente.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do endereço do remetente.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do endereço do remetente.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Sigla da UF do endereço do remetente.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Código do país do endereço do remetente.
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país do endereço do remetente.
        /// </summary>
        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPais() => CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

    /// <summary>
    /// Informações do recebedor do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Receb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Receb
    {
        /// <summary>
        /// CNPJ do recebedor.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do recebedor.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do recebedor.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do recebedor.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Telefone do recebedor.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Endereço do recebedor.
        /// </summary>
        [XmlElement("enderReceb")]
        public EnderReceb EnderReceb { get; set; }

        /// <summary>
        /// Email do recebedor.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se a propriedade Email deve ser serializada.
        /// </summary>
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Endereço do recebedor.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderReceb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderReceb
    {
        /// <summary>
        /// Logradouro do endereço do recebedor.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número do endereço do recebedor.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do recebedor.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do endereço do recebedor.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do endereço do recebedor.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do endereço do recebedor.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do endereço do recebedor.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Sigla da UF do endereço do recebedor.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Código do país do endereço do recebedor.
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país do endereço do recebedor.
        /// </summary>
        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPais() => CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

    /// <summary>
    /// Endereço do destinatário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Dest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Dest
    {
        /// <summary>
        /// CNPJ do destinatário.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do destinatário.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do destinatário.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do destinatário.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Telefone do destinatário.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Inscrição SUFRAMA do destinatário.
        /// </summary>
        [XmlElement("ISUF")]
        public string ISUF { get; set; }

        /// <summary>
        /// Endereço do destinatário.
        /// </summary>
        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        /// <summary>
        /// Email do destinatário.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        /// <summary>
        /// Verifica se a propriedade Email deve ser serializada.
        /// </summary>
        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Endereço do destinatário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderDest
    {
        /// <summary>
        /// Logradouro do endereço do destinatário.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número do endereço do destinatário.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento do endereço do destinatário.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro do endereço do destinatário.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do município do endereço do destinatário.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município do endereço do destinatário.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP do endereço do destinatário.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Sigla da UF do endereço do destinatário.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Código do país do endereço do destinatário.
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país do endereço do destinatário.
        /// </summary>
        [XmlElement("xPais")]
        public string XPais { get; set; } = "BRASIL";

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPais() => CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XPais deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && CPais > 0;

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade CEP deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        #endregion
    }

    /// <summary>
    /// Informações dos valores da prestação de serviço.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.VPrest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class VPrest
    {
        /// <summary>
        /// Valor total da prestação de serviço.
        /// </summary>
        [XmlIgnore]
        public double VTPrest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTPrest" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTPrest")]
        public string VTPrestField
        {
            get => VTPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VTPrest = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor a receber.
        /// </summary>
        [XmlIgnore]
        public double VRec { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VRec" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vRec")]
        public string VRecField
        {
            get => VRec.ToString("F2", CultureInfo.InvariantCulture);
            set => VRec = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Lista de componentes do valor da prestação.
        /// </summary>
        [XmlElement("Comp")]
        public List<Comp> Comp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista Comp.
        /// </summary>
        /// <param name="comp">Elemento Comp a ser adicionado.</param>
        public void AddComp(Comp comp)
        {
            if (Comp == null)
            {
                Comp = new List<Comp>();
            }

            Comp.Add(comp);
        }

        /// <summary>
        /// Retorna o elemento da lista Comp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comp.</returns>
        public Comp GetComp(int index)
        {
            if ((Comp?.Count ?? 0) == 0)
            {
                return default;
            }

            return Comp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comp.
        /// </summary>
        public int GetCompCount => (Comp != null ? Comp.Count : 0);

#endif
    }

    /// <summary>
    /// Lista de componentes do valor da prestação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Comp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Comp
    {
        /// <summary>
        /// Nome do componente do valor da prestação.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Valor do componente do valor da prestação.
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
    }

    /// <summary>
    /// Informações dos impostos do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Imp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Imp
    {
        /// <summary>
        /// Informações do ICMS.
        /// </summary>
        [XmlElement("ICMS")]
        public ICMS ICMS { get; set; }

        /// <summary>
        /// Valor total dos tributos.
        /// </summary>
        [XmlIgnore]
        public double VTotTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTotTrib" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Informações adicionais do fisco.
        /// </summary>
        [XmlElement("infAdFisco")]
        public string InfAdFisco { get; set; }

        /// <summary>
        /// Informações do ICMS para a UF de destino.
        /// </summary>
        [XmlElement("ICMSUFFim")]
        public ICMSUFFim ICMSUFFim { get; set; }

        /// <summary>
        /// Grupo de informações da Tributação IBS/CBS
        /// </summary>
        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        /// <summary>
        /// Valor total do documento fiscal
        /// </summary>
        [XmlIgnore]
        public double VTotDFe { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTotDFe" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotDFe")]
        public string VTotDFeField
        {
            get => VTotDFe.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDFe = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;
        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);
        public bool ShouldSerializeVTotDFeField() => VTotDFe > 0;        

        #endregion
    }

    /// <summary>
    /// Informações do ICMS.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMS
    {
        /// <summary>
        /// ICMS com tributação normal.
        /// </summary>
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        /// <summary>
        /// ICMS com redução de base de cálculo.
        /// </summary>
        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        /// <summary>
        /// ICMS isento.
        /// </summary>
        [XmlElement("ICMS45")]
        public ICMS45 ICMS45 { get; set; }

        /// <summary>
        /// ICMS cobrado anteriormente por substituição tributária.
        /// </summary>
        [XmlElement("ICMS60")]
        public ICMS60 ICMS60 { get; set; }

        /// <summary>
        /// Outros ICMS.
        /// </summary>
        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        /// <summary>
        /// ICMS devido à UF de origem da prestação, quando diferente da UF do emitente.
        /// </summary>
        [XmlElement("ICMSOutraUF")]
        public ICMSOutraUF ICMSOutraUF { get; set; }

        /// <summary>
        /// ICMS para contribuintes optantes pelo Simples Nacional.
        /// </summary>
        [XmlElement("ICMSSN")]
        public ICMSSN ICMSSN { get; set; }
    }

    /// <summary>
    /// ICMS com tributação normal.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Valor da BC do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS.
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// ICMS com redução de base de cálculo.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Percentual da redução de BC.
        /// </summary>
        [XmlIgnore]
        public double PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PRedBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS.
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS desonerado.
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSDeson" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF.
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade VICMSDesonField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        /// <summary>
        /// Verifica se a propriedade CBenef deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCBenef() => VICMSDeson > 0;

        #endregion
    }

    /// <summary>
    /// ICMS isento.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Valor do ICMS desonerado.
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSDeson" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF.
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade VICMSDesonField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        /// <summary>
        /// Verifica se a propriedade CBenef deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCBenef() => VICMSDeson > 0;

        #endregion
    }

    /// <summary>
    /// ICMS cobrado anteriormente por substituição tributária.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Valor da BC do ICMS ST retido.
        /// </summary>
        [XmlIgnore]
        public double VBCSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBCSTRet" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST retido.
        /// </summary>
        [XmlIgnore]
        public double VICMSSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSSTRet" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST retido.
        /// </summary>
        [XmlIgnore]
        public double PICMSSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMSSTRet" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSSTRet")]
        public string PICMSSTRetField
        {
            get => PICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSSTRet = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido.
        /// </summary>
        [XmlIgnore]
        public double VCred { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCred" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCred")]
        public string VCredField
        {
            get => VCred.ToString("F2", CultureInfo.InvariantCulture);
            set => VCred = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS desonerado.
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSDeson" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF.
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade VCredField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVCredField() => VCred > 0;

        /// <summary>
        /// Verifica se a propriedade VICMSDesonField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        /// <summary>
        /// Verifica se a propriedade CBenef deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCBenef() => VICMSDeson > 0;

        #endregion
    }

    /// <summary>
    /// Outros ICMS.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Percentual da redução de BC.
        /// </summary>
        [XmlIgnore]
        public double PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PRedBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBC = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS.
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMS = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do crédito presumido.
        /// </summary>
        [XmlIgnore]
        public double VCred { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCred" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCred")]
        public string VCredField
        {
            get => VCred.ToString("F2", CultureInfo.InvariantCulture);
            set => VCred = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS desonerado.
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSDeson" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF.
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade PRedBCField deve ser serializada.
        /// </summary>
        public bool ShouldSerializePRedBCField() => PRedBC > 0;

        /// <summary>
        /// Verifica se a propriedade VCredField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVCredField() => VCred > 0;

        /// <summary>
        /// Verifica se a propriedade VICMSDesonField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        /// <summary>
        /// Verifica se a propriedade CBenef deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCBenef() => VICMSDeson > 0;

        #endregion
    }

    /// <summary>
    /// ICMS devido à UF de origem da prestação, quando diferente da UF do emitente.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Percentual da redução de BC da outra UF.
        /// </summary>
        [XmlIgnore]
        public double PRedBCOutraUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PRedBCOutraUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCOutraUF")]
        public string PRedBCOutraUFField
        {
            get => PRedBCOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedBCOutraUF = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS da outra UF.
        /// </summary>
        [XmlIgnore]
        public double VBCOutraUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBCOutraUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCOutraUF")]
        public string VBCOutraUFField
        {
            get => VBCOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCOutraUF = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS da outra UF.
        /// </summary>
        [XmlIgnore]
        public double PICMSOutraUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMSOutraUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSOutraUF")]
        public string PICMSOutraUFField
        {
            get => PICMSOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSOutraUF = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da outra UF.
        /// </summary>
        [XmlIgnore]
        public double VICMSOutraUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSOutraUF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSOutraUF")]
        public string VICMSOutraUFField
        {
            get => VICMSOutraUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSOutraUF = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS desonerado.
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSDeson" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF.
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade PRedBCOutraUFField deve ser serializada.
        /// </summary>
        public bool ShouldSerializePRedBCOutraUFField() => PRedBCOutraUF > 0;

        /// <summary>
        /// Verifica se a propriedade VICMSDesonField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        /// <summary>
        /// Verifica se a propriedade CBenef deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCBenef() => VICMSDeson > 0;

        #endregion
    }

    /// <summary>
    /// ICMS para contribuintes optantes pelo Simples Nacional.
    /// </summary>
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

        /// <summary>
        /// Classificação Tributária do Serviço.
        /// </summary>
        [XmlElement("CST")]
        public string CST
        {
            get => CSTField;
            set
            {
                CSTField = "";
                if (!string.IsNullOrWhiteSpace(value))
                {
                    CSTField = value;
                }
            }
        }

        /// <summary>
        /// Indicador de contribuinte do Simples Nacional.
        /// </summary>
        [XmlElement("indSN")]
        public SimNao IndSN { get; set; }
    }

    /// <summary>
    /// Informações do ICMS para a UF de destino.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.ICMSUFFim")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class ICMSUFFim
    {
        /// <summary>
        /// Valor da Base de Cálculo do ICMS para a UF de término.
        /// </summary>
        [XmlIgnore]
        public double VBCUFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBCUFFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCUFFim")]
        public string VBCUFFimField
        {
            get => VBCUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCUFFim = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do Fundo de Combate à Pobreza (FCP) da UF de término.
        /// </summary>
        [XmlIgnore]
        public double PFCPUFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PFCPUFFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPUFFim")]
        public string PFCPUFFimField
        {
            get => PFCPUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => PFCPUFFim = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota interna da UF de término.
        /// </summary>
        [XmlIgnore]
        public double PICMSUFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMSUFFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSUFFim")]
        public string PICMSUFFimField
        {
            get => PICMSUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSUFFim = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota interestadual das UF envolvidas.
        /// </summary>
        [XmlIgnore]
        public double PICMSInter { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "PICMSInter" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSInter")]
        public string PICMSInterField
        {
            get => PICMSInter.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSInter = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Fundo de Combate à Pobreza (FCP) da UF de término.
        /// </summary>
        [XmlIgnore]
        public double VFCPUFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VFCPUFFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPUFFim")]
        public string VFCPUFFimField
        {
            get => VFCPUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFFim = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da UF de término.
        /// </summary>
        [XmlIgnore]
        public double VICMSUFFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSUFFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFFim")]
        public string VICMSUFFimField
        {
            get => VICMSUFFim.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFFim = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da UF de início.
        /// </summary>
        [XmlIgnore]
        public double VICMSUFIni { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMSUFIni" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFIni")]
        public string VICMSUFIniField
        {
            get => VICMSUFIni.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFIni = Utility.Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações do CTe normal.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeNorm")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeNorm
    {
        /// <summary>
        /// Informações da Carga.
        /// </summary>
        [XmlElement("infCarga")]
        public InfCarga InfCarga { get; set; }

        /// <summary>
        /// Informações dos documentos transportados.
        /// </summary>
        [XmlElement("infDoc")]
        public InfDoc InfDoc { get; set; }

        /// <summary>
        /// Informações dos documentos anteriores.
        /// </summary>
        [XmlElement("docAnt")]
        public DocAnt DocAnt { get; set; }

        /// <summary>
        /// Informações do modal.
        /// </summary>
        [XmlElement("infModal")]
        public InfModal InfModal { get; set; }

        /// <summary>
        /// Informações dos veículos novos.
        /// </summary>
        [XmlElement("veicNovos")]
        public List<VeicNovos> VeicNovos { get; set; }

        /// <summary>
        /// Informações da cobrança.
        /// </summary>
        [XmlElement("cobr")]
        public Cobr Cobr { get; set; }

        /// <summary>
        /// Informações do CT-e de substituição.
        /// </summary>
        [XmlElement("infCteSub")]
        public InfCteSub InfCteSub { get; set; }

        /// <summary>
        /// Informações do CT-e globalizado.
        /// </summary>
        [XmlElement("infGlobalizado")]
        public InfGlobalizado InfGlobalizado { get; set; }

        /// <summary>
        /// Informações dos serviços vinculados.
        /// </summary>
        [XmlElement("infServVinc")]
        public InfServVinc InfServVinc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de veículos novos.
        /// </summary>
        /// <param name="veicNovos">Elemento VeicNovos a ser adicionado.</param>
        public void AddVeicNovos(VeicNovos veicNovos)
        {
            if (VeicNovos == null)
            {
                VeicNovos = new List<VeicNovos>();
            }

            VeicNovos.Add(veicNovos);
        }

        /// <summary>
        /// Retorna o elemento da lista VeicNovos (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da VeicNovos.</returns>
        public VeicNovos GetVeicNovos(int index)
        {
            if ((VeicNovos?.Count ?? 0) == 0)
            {
                return default;
            }

            return VeicNovos[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista VeicNovos.
        /// </summary>
        public int GetVeicNovosCount => (VeicNovos != null ? VeicNovos.Count : 0);

#endif

        #region ShouldSerialize

        //public bool ShouldSerialize() => !string.IsNullOrWhiteSpace();

        #endregion
    }

    /// <summary>
    /// Informações da Carga.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCarga
    {
        /// <summary>
        /// Valor total da carga.
        /// </summary>
        [XmlIgnore]
        public double VCarga { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCarga" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCarga")]
        public string VCargaField
        {
            get => VCarga.ToString("F2", CultureInfo.InvariantCulture);
            set => VCarga = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Produto predominante.
        /// </summary>
        [XmlElement("proPred")]
        public string ProPred { get; set; }

        /// <summary>
        /// Outras características da carga.
        /// </summary>
        [XmlElement("xOutCat")]
        public string XOutCat { get; set; }

        /// <summary>
        /// Informações de quantidades da carga.
        /// </summary>
        [XmlElement("infQ")]
        public List<InfQ> InfQ { get; set; }

        /// <summary>
        /// Valor da carga para efeito de averbação.
        /// </summary>
        [XmlIgnore]
        public double VCargaAverb { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VCargaAverb" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCargaAverb")]
        public string VCargaAverbField
        {
            get => VCargaAverb.ToString("F2", CultureInfo.InvariantCulture);
            set => VCargaAverb = Utility.Converter.ToDouble(value);
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações de quantidades da carga.
        /// </summary>
        /// <param name="infq">Elemento InfQ a ser adicionado.</param>
        public void AddInfQ(InfQ infq)
        {
            if (InfQ == null)
            {
                InfQ = new List<InfQ>();
            }

            InfQ.Add(infq);
        }

        /// <summary>
        /// Retorna o elemento da lista InfQ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfQ.</returns>
        public InfQ GetInfQ(int index)
        {
            if ((InfQ?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfQ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfQ.
        /// </summary>
        public int GetInfQCount => (InfQ != null ? InfQ.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade VCargaField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVCargaField() => VCarga >= 0;

        /// <summary>
        /// Verifica se a propriedade XOutCat deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXOutCat() => !string.IsNullOrWhiteSpace(XOutCat);

        /// <summary>
        /// Verifica se a propriedade VCargaAverbField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVCargaAverbField() => VCargaAverb > 0;

        #endregion
    }

    /// <summary>
    /// Informações de quantidades da carga.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfQ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfQ
    {
        /// <summary>
        /// Código da unidade de medida.
        /// </summary>
        [XmlElement("cUnid")]
        public CodigoUnidadeMedidaCTe CUnid { get; set; }

        /// <summary>
        /// Tipo da medida.
        /// </summary>
        [XmlElement("tpMed")]
        public string TpMed { get; set; }

        /// <summary>
        /// Quantidade da carga.
        /// </summary>
        [XmlIgnore]
        public double QCarga { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "QCarga" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qCarga")]
        public string QCargaField
        {
            get => QCarga.ToString("F4", CultureInfo.InvariantCulture);
            set => QCarga = Utility.Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações dos documentos transportados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfDoc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfDoc
    {
        /// <summary>
        /// Informações das NF.
        /// </summary>
        [XmlElement("infNF")]
        public List<InfNF> InfNF { get; set; }

        /// <summary>
        /// Informações das NF-e.
        /// </summary>
        [XmlElement("infNFe")]
        public List<InfNFe> InfNFe { get; set; }

        /// <summary>
        /// Informações dos demais documentos.
        /// </summary>
        [XmlElement("infOutros")]
        public List<InfOutros> InfOutros { get; set; }

        /// <summary>
        /// Grupo de informações da Declaração de Conteúdo Eletrônica nos documentos originários
        /// </summary>
        [XmlElement("infDCe")]
        public List<InfDCe> InfDCe { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações das NF.
        /// </summary>
        /// <param name="infnf">Elemento InfNF a ser adicionado.</param>
        public void AddInfNF(InfNF infnf)
        {
            if (InfNF == null)
            {
                InfNF = new List<InfNF>();
            }

            InfNF.Add(infnf);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfNF.</returns>
        public InfNF GetInfNF(int index)
        {
            if ((InfNF?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfNF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNF.
        /// </summary>
        public int GetInfNFCount => (InfNF != null ? InfNF.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das NF-e.
        /// </summary>
        /// <param name="infnfe">Elemento InfNFe a ser adicionado.</param>
        public void AddInfNFe(InfNFe infnfe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<InfNFe>();
            }

            InfNFe.Add(infnfe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfNFe.</returns>
        public InfNFe GetInfNFe(int index)
        {
            if ((InfNFe?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe.
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações dos demais documentos.
        /// </summary>
        /// <param name="infoutros">Elemento InfOutros a ser adicionado.</param>
        public void AddInfOutros(InfOutros infoutros)
        {
            if (InfOutros == null)
            {
                InfOutros = new List<InfOutros>();
            }

            InfOutros.Add(infoutros);
        }

        /// <summary>
        /// Retorna o elemento da lista InfOutros (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfOutros.</returns>
        public InfOutros GetInfOutros(int index)
        {
            if ((InfOutros?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfOutros[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfOutros.
        /// </summary>
        public int GetInfOutrosCount => (InfOutros != null ? InfOutros.Count : 0);

#endif
    }

    /// <summary>
    /// Informações das NF.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNF
    {
        /// <summary>
        /// Número da romaneio da NF.
        /// </summary>
        [XmlElement("nRoma")]
        public string NRoma { get; set; }

        /// <summary>
        /// Número do pedido da NF.
        /// </summary>
        [XmlElement("nPed")]
        public string NPed { get; set; }

        /// <summary>
        /// Modelo da NF.
        /// </summary>
        [XmlElement("mod")]
        public ModeloNF Mod { get; set; }

        /// <summary>
        /// Série da NF.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da NF.
        /// </summary>
        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        /// <summary>
        /// Data de emissão da NF.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor da Base de Cálculo do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBC" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS.
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VICMS" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de Cálculo do ICMS ST.
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VBCST" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST.
        /// </summary>
        [XmlIgnore]
        public double VST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VST" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vST")]
        public string VSTField
        {
            get => VST.ToString("F2", CultureInfo.InvariantCulture);
            set => VST = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total dos produtos.
        /// </summary>
        [XmlIgnore]
        public double VProd { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VProd" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da NF.
        /// </summary>
        [XmlIgnore]
        public double VNF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VNF" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// CFOP.
        /// </summary>
        [XmlElement("nCFOP")]
        public string NCFOP { get; set; }

        /// <summary>
        /// Peso total da NF.
        /// </summary>
        [XmlIgnore]
        public double NPeso { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "NPeso" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("nPeso")]
        public string NPesoField
        {
            get => NPeso.ToString("F3", CultureInfo.InvariantCulture);
            set => NPeso = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// PIN SUFRAMA.
        /// </summary>
        [XmlElement("PIN")]
        public string PIN { get; set; }

        /// <summary>
        /// Data prevista de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DPrev { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DPrev" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        /// <summary>
        /// Informações das unidades de carga.
        /// </summary>
        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        /// <summary>
        /// Informações das unidades de transporte.
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de carga.
        /// </summary>
        /// <param name="infUnidCarga">Elemento InfUnidCarga a ser adicionado.</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidCarga.</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga.
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de transporte.
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado.</param>
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
            }

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

    /// <summary>
    /// Informações da unidade de carga.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfUnidCarga
    {
        /// <summary>
        /// Tipo da Unidade de Carga.
        /// </summary>
        [XmlElement("tpUnidCarga")]
        public virtual TipoUnidadeCarga TpUnidCarga { get; set; }

        /// <summary>
        /// Identificação da Unidade de Carga.
        /// </summary>
        [XmlElement("idUnidCarga")]
        public string IdUnidCarga { get; set; }

        /// <summary>
        /// Lacres da Unidade de Carga.
        /// </summary>
        [XmlElement("lacUnidCarga")]
        public List<LacUnidCarga> LacUnidCarga { get; set; }

        /// <summary>
        /// Quantidade rateada.
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

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de lacres da unidade de carga.
        /// </summary>
        /// <param name="lacUnidCarga">Elemento LacUnidCarga a ser adicionado.</param>
        public void AddLacUnidCarga(LacUnidCarga lacUnidCarga)
        {
            if (LacUnidCarga == null)
            {
                LacUnidCarga = new List<LacUnidCarga>();
            }

            LacUnidCarga.Add(lacUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista LacUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da LacUnidCarga.</returns>
        public LacUnidCarga GetLacUnidCarga(int index)
        {
            if ((LacUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

            return LacUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista LacUnidCarga.
        /// </summary>
        public int GetLacUnidCargaCount => (LacUnidCarga != null ? LacUnidCarga.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade QtdRatField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion
    }

    /// <summary>
    /// Lacres da Unidade de Carga.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.LacUnidCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LacUnidCarga
    {
        /// <summary>
        /// Número do lacre.
        /// </summary>
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

    /// <summary>
    /// Informações da unidade de transporte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfUnidTransp
    {
        /// <summary>
        /// Tipo da Unidade de Transporte.
        /// </summary>
        [XmlElement("tpUnidTransp")]
        public virtual TipoUnidadeTransporte TpUnidTransp { get; set; }

        /// <summary>
        /// Identificação da Unidade de Transporte.
        /// </summary>
        [XmlElement("idUnidTransp")]
        public string IdUnidTransp { get; set; }

        /// <summary>
        /// Lacres da Unidade de Transporte.
        /// </summary>
        [XmlElement("lacUnidTransp")]
        public List<LacUnidTransp> LacUnidTransp { get; set; }

        /// <summary>
        /// Informações das Unidades de Carga.
        /// </summary>
        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        /// <summary>
        /// Quantidade rateada.
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

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de lacres da unidade de transporte.
        /// </summary>
        /// <param name="lacUnidTransp">Elemento LacUnidTransp a ser adicionado.</param>
        public void AddLacUnidTransp(LacUnidTransp lacUnidTransp)
        {
            if (LacUnidTransp == null)
            {
                LacUnidTransp = new List<LacUnidTransp>();
            }

            LacUnidTransp.Add(lacUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista LacUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da LacUnidTransp.</returns>
        public LacUnidTransp GetLacUnidTransp(int index)
        {
            if ((LacUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            }

            return LacUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista LacUnidTransp.
        /// </summary>
        public int GetLacUnidTranspCount => (LacUnidTransp != null ? LacUnidTransp.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de carga.
        /// </summary>
        /// <param name="infUnidCarga">Elemento InfUnidCarga a ser adicionado.</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidCarga.</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga.
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade QtdRatField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeQtdRatField() => QtdRat > 0;

        #endregion
    }

    /// <summary>
    /// Lacre da Unidade de Transporte.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.LacUnidTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class LacUnidTransp : LacUnidCarga { }

    /// <summary>
    /// Informaçõe da NF-e.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfNFe
    {
        /// <summary>
        /// Chave de acesso da NF-e.
        /// </summary>
        [XmlElement("chave")]
        public string Chave { get; set; }

        /// <summary>
        /// PIN.
        /// </summary>
        [XmlElement("PIN")]
        public string PIN { get; set; }

        /// <summary>
        /// Data prevista de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DPrev { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DPrev" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        /// <summary>
        /// Informações das unidades de carga.
        /// </summary>
        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        /// <summary>
        /// Informações das unidades de transporte.
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de carga.
        /// </summary>
        /// <param name="infUnidCarga">Elemento InfUnidCarga a ser adicionado.</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidCarga.</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga.
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de transporte.
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado.</param>
        public void AddInfUnidTransp(InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidTransp.</returns>
        public InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTransp.
        /// </summary>
        public int GetInfUnidTranspCount => (InfUnidTransp != null ? InfUnidTransp.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade PIN deve ser serializada.
        /// </summary>
        public bool ShouldSerializePIN() => !string.IsNullOrWhiteSpace(PIN);

        /// <summary>
        /// Verifica se a propriedade DPrevField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDPrevField() => DPrev > DateTime.MinValue;

        #endregion
    }

    /// <summary>
    /// Informações dos demais documentos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfOutros")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfOutros
    {
        /// <summary>
        /// Tipo do Documento Originário.
        /// </summary>
        [XmlElement("tpDoc")]
        public TipoDocumentoOriginarioCTe TpDoc { get; set; }

        /// <summary>
        /// Descrição de outros documentos.
        /// </summary>
        [XmlElement("descOutros")]
        public string DescOutros { get; set; }

        /// <summary>
        /// Número do documento.
        /// </summary>
        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        /// <summary>
        /// Data de emissão do documento.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor do documento fiscal.
        /// </summary>
        [XmlIgnore]
        public double VDocFisc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VDocFisc" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDocFisc")]
        public string VDocFiscField
        {
            get => VDocFisc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDocFisc = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Data prevista de entrega.
        /// </summary>
        [XmlIgnore]
        public DateTime DPrev { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DPrev" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPrev")]
        public string DPrevField
        {
            get => DPrev.ToString("yyyy-MM-dd");
            set => DPrev = DateTime.Parse(value);
        }

        /// <summary>
        /// Informações das unidades de carga.
        /// </summary>
        [XmlElement("infUnidCarga")]
        public List<InfUnidCarga> InfUnidCarga { get; set; }

        /// <summary>
        /// Informações das unidades de transporte.
        /// </summary>
        [XmlElement("infUnidTransp")]
        public List<InfUnidTransp> InfUnidTransp { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de carga.
        /// </summary>
        /// <param name="infUnidCarga">Elemento InfUnidCarga a ser adicionado.</param>
        public void AddInfUnidCarga(InfUnidCarga infUnidCarga)
        {
            if (InfUnidCarga == null)
            {
                InfUnidCarga = new List<InfUnidCarga>();
            }

            InfUnidCarga.Add(infUnidCarga);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidCarga (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidCarga.</returns>
        public InfUnidCarga GetInfUnidCarga(int index)
        {
            if ((InfUnidCarga?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidCarga[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidCarga.
        /// </summary>
        public int GetInfUnidCargaCount => (InfUnidCarga != null ? InfUnidCarga.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das unidades de transporte.
        /// </summary>
        /// <param name="infUnidTransp">Elemento InfUnidTransp a ser adicionado.</param>
        public void AddInfUnidTransp(InfUnidTransp infUnidTransp)
        {
            if (InfUnidTransp == null)
            {
                InfUnidTransp = new List<InfUnidTransp>();
            }

            InfUnidTransp.Add(infUnidTransp);
        }

        /// <summary>
        /// Retorna o elemento da lista InfUnidTransp (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfUnidTransp.</returns>
        public InfUnidTransp GetInfUnidTransp(int index)
        {
            if ((InfUnidTransp?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfUnidTransp[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfUnidTransp.
        /// </summary>
        public int GetInfUnidTranspCount => (InfUnidTransp != null ? InfUnidTransp.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade DescOutros deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDescOutros() => !string.IsNullOrWhiteSpace(DescOutros);

        /// <summary>
        /// Verifica se a propriedade NDoc deve ser serializada.
        /// </summary>
        public bool ShouldSerializeNDoc() => !string.IsNullOrWhiteSpace(NDoc);

        /// <summary>
        /// Verifica se a propriedade DEmiField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDEmiField() => DEmi > DateTime.MinValue;

        /// <summary>
        /// Verifica se a propriedade VDocFiscField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVDocFiscField() => VDocFisc > 0;

        /// <summary>
        /// Verifica se a propriedade DPrevField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeDPrevField() => DPrev > DateTime.MinValue;

        #endregion
    }

    /// <summary>
    /// Grupo de informações da Declaração de Conteúdo Eletrônica nos documentos originários
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfDCe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfDCe
    {
        /// <summary>
        /// Chave de acesso da DCe
        /// </summary>
        [XmlElement("chave")]
        public string Chave { get; set; }
    }

    /// <summary>
    /// Informações dos documentos anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DocAnt
    {
        /// <summary>
        /// Informações dos emitentes dos documentos anteriores.
        /// </summary>
        [XmlElement("emiDocAnt")]
        public List<EmiDocAnt> EmiDocAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de emitentes dos documentos anteriores.
        /// </summary>
        /// <param name="emiDocAnt">Elemento EmiDocAnt a ser adicionado.</param>
        public void AddEmiDocAnt(EmiDocAnt emiDocAnt)
        {
            if (EmiDocAnt == null)
            {
                EmiDocAnt = new List<EmiDocAnt>();
            }

            EmiDocAnt.Add(emiDocAnt);
        }

        /// <summary>
        /// Retorna o elemento da lista EmiDocAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da EmiDocAnt.</returns>
        public EmiDocAnt GetEmiDocAnt(int index)
        {
            if ((EmiDocAnt?.Count ?? 0) == 0)
            {
                return default;
            }

            return EmiDocAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista EmiDocAnt.
        /// </summary>
        public int GetEmiDocAntCount => (EmiDocAnt != null ? EmiDocAnt.Count : 0);

#endif
    }

    /// <summary>
    /// Informações dos emitentes dos documentos anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EmiDocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EmiDocAnt
    {
        /// <summary>
        /// CNPJ do emitente do documento anterior.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente do documento anterior.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição Estadual do emitente do documento anterior.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// UF do emitente do documento anterior.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Razão Social ou Nome do emitente do documento anterior.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Identificação dos documentos anteriores.
        /// </summary>
        [XmlElement("idDocAnt")]
        public List<IdDocAnt> IdDocAnt { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de identificação dos documentos anteriores.
        /// </summary>
        /// <param name="idDocAnt">Elemento IdDocAnt a ser adicionado.</param>
        public void AddIdDocAnt(IdDocAnt idDocAnt)
        {
            if (IdDocAnt == null)
            {
                IdDocAnt = new List<IdDocAnt>();
            }

            IdDocAnt.Add(idDocAnt);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAnt (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da IdDocAnt.</returns>
        public IdDocAnt GetIdDocAnt(int index)
        {
            if ((IdDocAnt?.Count ?? 0) == 0)
            {
                return default;
            }

            return IdDocAnt[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAnt.
        /// </summary>
        public int GetIdDocAntCount => (IdDocAnt != null ? IdDocAnt.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

    /// <summary>
    /// Identificação dos documentos anteriores.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAnt")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAnt
    {
        /// <summary>
        /// Identificação dos documentos anteriores eletrônicos.
        /// </summary>
        [XmlElement("idDocAntEle")]
        public List<IdDocAntEle> IdDocAntEle { get; set; }

        /// <summary>
        /// Identificação dos documentos anteriores em papel.
        /// </summary>
        [XmlElement("idDocAntPap")]
        public List<IdDocAntPap> IdDocAntPap { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de documentos anteriores eletrônicos.
        /// </summary>
        /// <param name="idDocAntEle">Elemento IdDocAntEle a ser adicionado.</param>
        public void AddIdDocAntEle(IdDocAntEle idDocAntEle)
        {
            if (IdDocAntEle == null)
            {
                IdDocAntEle = new List<IdDocAntEle>();
            }

            IdDocAntEle.Add(idDocAntEle);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAntEle (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da IdDocAntEle.</returns>
        public IdDocAntEle GetIdDocAntEle(int index)
        {
            if ((IdDocAntEle?.Count ?? 0) == 0)
            {
                return default;
            }

            return IdDocAntEle[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAntEle.
        /// </summary>
        public int GetIdDocAntEleCount => (IdDocAntEle != null ? IdDocAntEle.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de documentos anteriores em papel.
        /// </summary>
        /// <param name="idDocAntPap">Elemento IdDocAntPap a ser adicionado.</param>
        public void AddIdDocAntPap(IdDocAntPap idDocAntPap)
        {
            if (IdDocAntPap == null)
            {
                IdDocAntPap = new List<IdDocAntPap>();
            }

            IdDocAntPap.Add(idDocAntPap);
        }

        /// <summary>
        /// Retorna o elemento da lista IdDocAntPap (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da IdDocAntPap.</returns>
        public IdDocAntPap GetIdDocAntPap(int index)
        {
            if ((IdDocAntPap?.Count ?? 0) == 0)
            {
                return default;
            }

            return IdDocAntPap[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista IdDocAntPap.
        /// </summary>
        public int GetIdDocAntPapCount => (IdDocAntPap != null ? IdDocAntPap.Count : 0);

#endif
    }

    /// <summary>
    /// Identificação dos documentos anteriores em papel.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAntPap")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAntPap
    {
        /// <summary>
        /// Tipo do Documento de Transporte Anterior.
        /// </summary>
        [XmlElement("tpDoc")]
        public TipoDocumentoTransporteAnteriorCTe TpDoc { get; set; }

        /// <summary>
        /// Série do documento.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Subsérie do documento.
        /// </summary>
        [XmlElement("subser")]
        public string Subser { get; set; }

        /// <summary>
        /// Número do documento.
        /// </summary>
        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        /// <summary>
        /// Data de emissão do documento.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade Subser deve ser serializada.
        /// </summary>
        public bool ShouldSerializeSubser() => !string.IsNullOrWhiteSpace(Subser);

        #endregion
    }

    /// <summary>
    /// Identificação dos documentos anteriores eletrônicos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IdDocAntEle")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IdDocAntEle
    {
        /// <summary>
        /// Chave de acesso do CT-e.
        /// </summary>
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }
    }

    /// <summary>
    /// Informações do modal.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfModal
    {
        /// <summary>
        /// Versão do modal.
        /// </summary>
        [XmlAttribute(AttributeName = "versaoModal", DataType = "token")]
        public string VersaoModal { get; set; }

        /// <summary>
        /// Informações do modal rodoviário.
        /// </summary>
        [XmlElement("rodo")]
        public Rodo Rodo { get; set; }

        /// <summary>
        /// Informações do modal multimodal.
        /// </summary>
        [XmlElement("multimodal")]
        public MultiModal MultiModal { get; set; }

        /// <summary>
        /// Informações do modal dutoviário.
        /// </summary>
        [XmlElement("duto")]
        public Duto Duto { get; set; }

        /// <summary>
        /// Informações do modal aéreo.
        /// </summary>
        [XmlElement("aereo")]
        public Aereo Aereo { get; set; }

        /// <summary>
        /// Informações do modal aquaviário.
        /// </summary>
        [XmlElement("aquav")]
        public Aquav Aquav { get; set; }

        /// <summary>
        /// Informações do modal ferroviário.
        /// </summary>
        [XmlElement("ferrov")]
        public Ferrov Ferrov { get; set; }
    }

    /// <summary>
    /// Informações do modal rodoviário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Rodo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Rodo
    {
        /// <summary>
        /// Registro Nacional de Transportadores Rodoviários de Carga.
        /// </summary>
        [XmlElement("RNTRC")]
        public string RNTRC { get; set; }

        /// <summary>
        /// Informações do Ocorrência.
        /// </summary>
        [XmlElement("occ")]
        public List<Occ> Occ { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de ocorrências.
        /// </summary>
        /// <param name="occ">Elemento Occ a ser adicionado.</param>
        public void AddOcc(Occ occ)
        {
            if (Occ == null)
            {
                Occ = new List<Occ>();
            }

            Occ.Add(occ);
        }

        /// <summary>
        /// Retorna o elemento da lista Occ (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da Occ.</returns>
        public Occ GetOcc(int index)
        {
            if ((Occ?.Count ?? 0) == 0)
            {
                return default;
            }

            return Occ[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Occ.
        /// </summary>
        public int GetOccCount => (Occ != null ? Occ.Count : 0);

#endif
    }

    /// <summary>
    /// Informações da Ocorrência.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Occ")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Occ
    {
        /// <summary>
        /// Série da Ocorrência.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da Ocorrência.
        /// </summary>
        [XmlElement("nOcc")]
        public int NOcc { get; set; }

        /// <summary>
        /// Data de Emissão da Ocorrência.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        /// <summary>
        /// Emitente da Ocorrência.
        /// </summary>
        [XmlElement("emiOcc")]
        public EmiOcc EmiOcc { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade Serie deve ser serializada.
        /// </summary>
        public bool ShouldSerializeSerie() => !string.IsNullOrWhiteSpace(Serie);

        #endregion
    }

    /// <summary>
    /// Emitente da Ocorrência.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EmiOcc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EmiOcc
    {
        /// <summary>
        /// CNPJ do Emitente da Ocorrência.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Código interno do Emitente da Ocorrência.
        /// </summary>
        [XmlElement("cInt")]
        public string CInt { get; set; }

        /// <summary>
        /// Inscrição Estadual do Emitente da Ocorrência.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// UF do Emitente da Ocorrência.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Telefone do Emitente da Ocorrência.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CInt deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);

        /// <summary>
        /// Verifica se a propriedade Fone deve ser serializada.
        /// </summary>
        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

    /// <summary>
    /// Informações do modal multimodal.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.MultiModal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class MultiModal
    {
        /// <summary>
        /// Código do Operador de Transporte Multimodal.
        /// </summary>
        [XmlElement("COTM")]
        public string COTM { get; set; }

        /// <summary>
        /// Indicador de Negociável.
        /// </summary>
        [XmlElement("indNegociavel")]
        public IndicadorNegociavelCTe IndNegociavel { get; set; }

        /// <summary>
        /// Informações do Seguro.
        /// </summary>
        [XmlElement("seg")]
        public Seg Seg { get; set; }
    }

    /// <summary>
    /// Informações do Seguro.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Seg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Seg
    {
        /// <summary>
        /// Informações do Seguro.
        /// </summary>
        [XmlElement("infSeg")]
        public InfSeg InfSeg { get; set; }

        /// <summary>
        /// Número da Apólice.
        /// </summary>
        [XmlElement("nApol")]
        public string NApol { get; set; }

        /// <summary>
        /// Número da Averbação.
        /// </summary>
        [XmlElement("nAver")]
        public string NAver { get; set; }
    }

    /// <summary>
    /// Informações do Seguro.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfSeg")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfSeg
    {
        /// <summary>
        /// Nome da Seguradora.
        /// </summary>
        [XmlElement("xSeg")]
        public string XSeg { get; set; }

        /// <summary>
        /// CNPJ da Seguradora.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }
    }

    /// <summary>
    /// Informações do modal dutoviário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Duto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Duto
    {
        /// <summary>
        /// Valor da Tarifa.
        /// </summary>
        [XmlIgnore]
        public double VTar { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTar" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTar")]
        public string VTarField
        {
            get => VTar.ToString("F6", CultureInfo.InvariantCulture);
            set => VTar = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Data de Início.
        /// </summary>
        [XmlIgnore]
        public DateTime DIni { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DIni" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dIni")]
        public string DIniField
        {
            get => DIni.ToString("yyyy-MM-dd");
            set => DIni = DateTime.Parse(value);
        }

        /// <summary>
        /// Data de Fim.
        /// </summary>
        [XmlIgnore]
        public DateTime DFim { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DFim" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dFim")]
        public string DFimField
        {
            get => DFim.ToString("yyyy-MM-dd");
            set => DFim = DateTime.Parse(value);
        }

        /// <summary>
        /// Classificação Dutoviário
        /// </summary>
        [XmlElement("classDuto")]
#if INTEROP
        public ClassificacaoDutoviarioCTe ClassDuto { get; set; } = (ClassificacaoDutoviarioCTe)(-1);
#else
        public ClassificacaoDutoviarioCTe? ClassDuto { get; set; }
#endif

        /// <summary>
        /// Tipo de contratação do serviço de transporte (apenas para gasoduto)
        /// </summary>
        [XmlElement("tpContratacao")]
#if INTEROP
        public TipoContratacaoTransporteCTe TpContratacao { get; set; } = (TipoContratacaoTransporteCTe)(-1);
#else
        public TipoContratacaoTransporteCTe? TpContratacao { get; set; }
#endif

        /// <summary>
        /// Código do Ponto de Entrada
        /// </summary>
        [XmlElement("codPontoEntrada")]
        public string CodPontoEntrada { get; set; }

        /// <summary>
        /// Código do Ponto de Saída
        /// </summary>
        [XmlElement("codPontoSaida")]
        public string CodPontoSaida { get; set; }

        /// <summary>
        /// Número do Contrato de Capacidade 
        /// </summary>
        [XmlElement("nContrato")]
        public string NContrato { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVTarField() => VTar > 0;

#if INTEROP
        public bool ShouldSerializeClassDuto() => ClassDuto != (ClassificacaoDutoviarioCTe)(-1);
        public bool ShouldSerializeTpContratacao() => TpContratacao != (TipoContratacaoTransporteCTe)(-1);
#else
        public bool ShouldSerializeClassDuto() => ClassDuto != null;
        public bool ShouldSerializeTpContratacao() => TpContratacao != null;
#endif
        public bool ShouldSerializeCodPontoEntrada() => !string.IsNullOrWhiteSpace(CodPontoEntrada);
        public bool ShouldSerializeCodPontoSaida() => !string.IsNullOrWhiteSpace(CodPontoSaida);
        public bool ShouldSerializeNContrato() => !string.IsNullOrWhiteSpace(NContrato);

        #endregion
    }

    /// <summary>
    /// Informações do modal aéreo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Aereo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Aereo
    {
        /// <summary>
        /// Número da Minuta.
        /// </summary>
        [XmlElement("nMinu")]
        public string NMinu { get; set; }

        /// <summary>
        /// Número do Conhecimento Aéreo.
        /// </summary>
        [XmlElement("nOCA")]
        public string NOCA { get; set; }

        /// <summary>
        /// Data Prevista.
        /// </summary>
        [XmlIgnore]
        public DateTime DPrevAereo { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DPrevAereo" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPrevAereo")]
        public string DPrevAereoField
        {
            get => DPrevAereo.ToString("yyyy-MM-dd");
            set => DPrevAereo = DateTime.Parse(value);
        }

        /// <summary>
        /// Natureza da Carga.
        /// </summary>
        [XmlElement("natCarga")]
        public NatCarga NatCarga { get; set; }

        /// <summary>
        /// Tarifa.
        /// </summary>
        [XmlElement("tarifa")]
        public Tarifa Tarifa { get; set; }

        /// <summary>
        /// Informações de Periculosidade.
        /// </summary>
        [XmlElement("peri")]
        public List<Peri> Peri { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações de periculosidade.
        /// </summary>
        /// <param name="peri">Elemento Peri a ser adicionado.</param>
        public void AddPeri(Peri peri)
        {
            if (Peri == null)
            {
                Peri = new List<Peri>();
            }

            Peri.Add(peri);
        }

        /// <summary>
        /// Retorna o elemento da lista Peri (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da Peri.</returns>
        public Peri GetPeri(int index)
        {
            if ((Peri?.Count ?? 0) == 0)
            {
                return default;
            }

            return Peri[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Peri.
        /// </summary>
        public int GetPeriCount => (Peri != null ? Peri.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade NMinu deve ser serializada.
        /// </summary>
        public bool ShouldSerializeNMinu() => !string.IsNullOrWhiteSpace(NMinu);

        /// <summary>
        /// Verifica se a propriedade NOCA deve ser serializada.
        /// </summary>
        public bool ShouldSerializeNOCA() => !string.IsNullOrWhiteSpace(NOCA);

        #endregion
    }

    /// <summary>
    /// Natureza da Carga.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.NatCarga")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class NatCarga
    {
        /// <summary>
        /// Dimensão da Carga.
        /// </summary>
        [XmlElement("xDime")]
        public string XDime { get; set; }

        /// <summary>
        /// Código da Informação de Manuseio.
        /// </summary>
        [XmlElement("cInfManu")]
        public List<InformacaoManuseioCTe> CInfManu { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de códigos de informação de manuseio.
        /// </summary>
        /// <param name="cInfManu">Elemento InformacaoManuseioCTe a ser adicionado.</param>
        public void AddCInfManu(InformacaoManuseioCTe cInfManu)
        {
            if (CInfManu == null)
            {
                CInfManu = new List<InformacaoManuseioCTe>();
            }

            CInfManu.Add(cInfManu);
        }

        /// <summary>
        /// Retorna o elemento da lista CInfManu (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da CInfManu.</returns>
        public InformacaoManuseioCTe GetCInfManu(int index)
        {
            if ((CInfManu?.Count ?? 0) == 0)
            {
                return default;
            }

            return CInfManu[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista CInfManu.
        /// </summary>
        public int GetCInfManuCount => (CInfManu != null ? CInfManu.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade XDime deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXDime() => !string.IsNullOrWhiteSpace(XDime);

        #endregion
    }

    /// <summary>
    /// Informações da Tarifa.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Tarifa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Tarifa
    {
        /// <summary>
        /// Classe.
        /// </summary>
        [XmlElement("CL")]
        public string CL { get; set; }

        /// <summary>
        /// Código da Tarifa.
        /// </summary>
        [XmlElement("cTar")]
        public string CTar { get; set; }

        /// <summary>
        /// Valor da Tarifa.
        /// </summary>
        [XmlIgnore]
        public double VTar { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VTar" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTar")]
        public string VTarField
        {
            get => VTar.ToString("F2", CultureInfo.InvariantCulture);
            set => VTar = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CTar deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCTar() => !string.IsNullOrWhiteSpace(CTar);

        #endregion
    }

    /// <summary>
    /// Informações de Periculosidade.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Peri")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Peri
    {
        /// <summary>
        /// Número ONU.
        /// </summary>
        [XmlElement("nONU")]
        public string NONU { get; set; }

        /// <summary>
        /// Quantidade Total de Embalagens.
        /// </summary>
        [XmlElement("qTotEmb")]
        public string QTotEmb { get; set; }

        /// <summary>
        /// Informações do Total de Artigos Perigosos.
        /// </summary>
        [XmlElement("infTotAP")]
        public InfTotAP InfTotAP { get; set; }
    }

    /// <summary>
    /// Informações do Total de Artigos Perigosos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfTotAP")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfTotAP
    {
        /// <summary>
        /// Quantidade Total do Produto.
        /// </summary>
        [XmlIgnore]
        public double QTotProd { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "QTotProd" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qTotProd")]
        public string QTotProdField
        {
            get => QTotProd.ToString("F4", CultureInfo.InvariantCulture);
            set => QTotProd = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Unidade de Medida de Artigos Perigosos.
        /// </summary>
        [XmlElement("uniAP")]
        public UnidadeMedidaArtigoPerigoso UniAP { get; set; }
    }

    /// <summary>
    /// Informações do modal aquaviário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Aquav")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Aquav
    {
        /// <summary>
        /// Valor da Prestação do Serviço.
        /// </summary>
        [XmlIgnore]
        public double VPrest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VPrest" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPrest")]
        public string VPrestField
        {
            get => VPrest.ToString("F2", CultureInfo.InvariantCulture);
            set => VPrest = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Adicional ao Frete para Renovação da Marinha Mercante.
        /// </summary>
        [XmlIgnore]
        public double VAFRMM { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VAFRMM" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAFRMM")]
        public string VAFRMMField
        {
            get => VAFRMM.ToString("F2", CultureInfo.InvariantCulture);
            set => VAFRMM = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Nome do Navio.
        /// </summary>
        [XmlElement("xNavio")]
        public string XNavio { get; set; }

        /// <summary>
        /// Informações das Balsas.
        /// </summary>
        [XmlElement("balsa")]
        public List<Balsa> Balsa { get; set; }

        /// <summary>
        /// Número da Viagem.
        /// </summary>
        [XmlElement("nViag")]
        public string NViag { get; set; }

        /// <summary>
        /// Direção.
        /// </summary>
        [XmlElement("direc")]
        public DirecaoCTe Direc { get; set; }

        /// <summary>
        /// Número da Inscrição no Registro Internacional de Navios.
        /// </summary>
        [XmlElement("irin")]
        public string Irin { get; set; }

        /// <summary>
        /// Detalhamento dos Contêineres.
        /// </summary>
        [XmlElement("detCont")]
        public List<DetCont> DetCont { get; set; }

        /// <summary>
        /// Tipo de Navegação.
        /// </summary>
        [XmlElement("tpNav")]
#if INTEROP
        public TipoNavegacao TpNav { get; set; } = TipoNavegacao.NaoDefinido;
#else
        public TipoNavegacao? TpNav { get; set; }
#endif

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de balsas.
        /// </summary>
        /// <param name="balsa">Elemento Balsa a ser adicionado.</param>
        public void AddBalsa(Balsa balsa)
        {
            if (Balsa == null)
            {
                Balsa = new List<Balsa>();
            }

            Balsa.Add(balsa);
        }

        /// <summary>
        /// Retorna o elemento da lista Balsa (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da Balsa.</returns>
        public Balsa GetBalsa(int index)
        {
            if ((Balsa?.Count ?? 0) == 0)
            {
                return default;
            }

            return Balsa[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Balsa.
        /// </summary>
        public int GetBalsaCount => (Balsa != null ? Balsa.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de detalhes dos contêineres.
        /// </summary>
        /// <param name="detCont">Elemento DetCont a ser adicionado.</param>
        public void AddDetCont(DetCont detCont)
        {
            if (DetCont == null)
            {
                DetCont = new List<DetCont>();
            }

            DetCont.Add(detCont);
        }

        /// <summary>
        /// Retorna o elemento da lista DetCont (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da DetCont.</returns>
        public DetCont GetDetCont(int index)
        {
            if ((DetCont?.Count ?? 0) == 0)
            {
                return default;
            }

            return DetCont[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetCont.
        /// </summary>
        public int GetDetContCount => (DetCont != null ? DetCont.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade NViag deve ser serializada.
        /// </summary>
        public bool ShouldSerializeNViag() => !string.IsNullOrWhiteSpace(NViag);

#if INTEROP
        /// <summary>
        /// Verifica se a propriedade TpNav deve ser serializada.
        /// </summary>
        public bool ShouldSerializeTpNav() => TpNav != TipoNavegacao.NaoDefinido;
#else
        /// <summary>
        /// Verifica se a propriedade TpNav deve ser serializada.
        /// </summary>
        public bool ShouldSerializeTpNav() => TpNav != null;
#endif

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Informação da Balsa.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Balsa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Balsa
    {
        /// <summary>
        /// Identificador da Balsa.
        /// </summary>
        [XmlElement("xBalsa")]
        public string XBalsa { get; set; }
    }

    /// <summary>
    /// Detalhamento dos Contêineres.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetCont
    {
        /// <summary>
        /// Número do Contêiner.
        /// </summary>
        [XmlElement("nCont")]
        public string NCont { get; set; }

        /// <summary>
        /// Lacres.
        /// </summary>
        [XmlElement("lacre")]
        public List<Lacre> Lacre { get; set; }

        /// <summary>
        /// Informações dos documentos do contêiner.
        /// </summary>
        [XmlElement("infDoc")]
        public DetContInfDoc InfDoc { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de lacres.
        /// </summary>
        /// <param name="lacre">Elemento Lacre a ser adicionado.</param>
        public void AddLacre(Lacre lacre)
        {
            if (Lacre == null)
            {
                Lacre = new List<Lacre>();
            }

            Lacre.Add(lacre);
        }

        /// <summary>
        /// Retorna o elemento da lista Lacre (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da Lacre.</returns>
        public Lacre GetLacre(int index)
        {
            if ((Lacre?.Count ?? 0) == 0)
            {
                return default;
            }

            return Lacre[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Lacre.
        /// </summary>
        public int GetLacreCount => (Lacre != null ? Lacre.Count : 0);

#endif
    }

    /// <summary>
    /// Lacre.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Lacre")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Lacre
    {
        /// <summary>
        /// Número do Lacre.
        /// </summary>
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

    /// <summary>
    /// Informações dos documentos do contêiner.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDoc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDoc
    {
        /// <summary>
        /// Informações das Notas Fiscais.
        /// </summary>
        [XmlElement("infNF")]
        public List<DetContInfDocInfNF> InfNF { get; set; }

        /// <summary>
        /// Informações das Notas Fiscais Eletrônicas.
        /// </summary>
        [XmlElement("infNFe")]
        public List<DetContInfDocInfNFe> InfNFe { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações das notas fiscais.
        /// </summary>
        /// <param name="infNF">Elemento DetContInfDocInfNF a ser adicionado.</param>
        public void AddInfNF(DetContInfDocInfNF infNF)
        {
            if (InfNF == null)
            {
                InfNF = new List<DetContInfDocInfNF>();
            }

            InfNF.Add(infNF);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNF (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfNF.</returns>
        public DetContInfDocInfNF GetInfNF(int index)
        {
            if ((InfNF?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfNF[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNF.
        /// </summary>
        public int GetInfNFCount => (InfNF != null ? InfNF.Count : 0);

        /// <summary>
        /// Adicionar novo elemento à lista de informações das notas fiscais eletrônicas.
        /// </summary>
        /// <param name="infNFe">Elemento DetContInfDocInfNFe a ser adicionado.</param>
        public void AddInfNFe(DetContInfDocInfNFe infNFe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<DetContInfDocInfNFe>();
            }

            InfNFe.Add(infNFe);
        }

        /// <summary>
        /// Retorna o elemento da lista InfNFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfNFe.</returns>
        public DetContInfDocInfNFe GetInfNFe(int index)
        {
            if ((InfNFe?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfNFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfNFe.
        /// </summary>
        public int GetInfNFeCount => (InfNFe != null ? InfNFe.Count : 0);

#endif
    }

    /// <summary>
    /// Informações das Notas Fiscais.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDocInfNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDocInfNF
    {
        /// <summary>
        /// Série da Nota Fiscal.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Número da Nota Fiscal.
        /// </summary>
        [XmlElement("nDoc")]
        public string NDoc { get; set; }

        /// <summary>
        /// Unidade de Rateio.
        /// </summary>
        [XmlIgnore]
        public double UnidRat { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "UnidRat" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("unidRat")]
        public string UnidRatField
        {
            get => UnidRat.ToString("F2", CultureInfo.InvariantCulture);
            set => UnidRat = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade UnidRatField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeUnidRatField() => UnidRat > 0;

        #endregion
    }

    /// <summary>
    /// Informações das Notas Fiscais Eletrônicas.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.DetContInfDocInfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class DetContInfDocInfNFe
    {
        /// <summary>
        /// Chave de acesso da Nota Fiscal Eletrônica.
        /// </summary>
        [XmlElement("chave")]
        public string Chave { get; set; }

        /// <summary>
        /// Unidade de Rateio.
        /// </summary>
        [XmlIgnore]
        public double UnidRat { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "UnidRat" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("unidRat")]
        public string UnidRatField
        {
            get => UnidRat.ToString("F2", CultureInfo.InvariantCulture);
            set => UnidRat = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade UnidRatField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeUnidRatField() => UnidRat > 0;

        #endregion
    }

    /// <summary>
    /// Informações do modal ferroviário.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Ferrov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Ferrov
    {
        /// <summary>
        /// Tipo de Tráfego.
        /// </summary>
        [XmlElement("tpTraf")]
        public TipoTrafegoCTe TpTraf { get; set; }

        /// <summary>
        /// Informações do Tráfego Mútuo.
        /// </summary>
        [XmlElement("trafMut")]
        public TrafMut TrafMut { get; set; }

        /// <summary>
        /// Fluxo Ferroviário.
        /// </summary>
        [XmlElement("fluxo")]
        public string Fluxo { get; set; }
    }

    /// <summary>
    /// Informações do Tráfego Mútuo.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.TrafMut")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class TrafMut
    {
        /// <summary>
        /// Responsável pelo Faturamento.
        /// </summary>
        [XmlElement("respFat")]
        public FerroviaCTe RespFat { get; set; }

        /// <summary>
        /// Ferrovia Emitente.
        /// </summary>
        [XmlElement("ferrEmi")]
        public FerroviaCTe FerrEmi { get; set; }

        /// <summary>
        /// Valor do Frete.
        /// </summary>
        [XmlIgnore]
        public double VFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VFrete" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Chave de acesso do CT-e Ferroviário de Origem.
        /// </summary>
        [XmlElement("chCTeFerroOrigem")]
        public string ChCTeFerroOrigem { get; set; }

        /// <summary>
        /// Ferroviários Envolvidos.
        /// </summary>
        [XmlElement("ferroEnv")]
        public List<FerroEnv> FerroEnv { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de ferroviários envolvidos.
        /// </summary>
        /// <param name="ferroEnv">Elemento FerroEnv a ser adicionado.</param>
        public void AddFerroEnv(FerroEnv ferroEnv)
        {
            if (FerroEnv == null)
            {
                FerroEnv = new List<FerroEnv>();
            }

            FerroEnv.Add(ferroEnv);
        }

        /// <summary>
        /// Retorna o elemento da lista FerroEnv (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da FerroEnv.</returns>
        public FerroEnv GetFerroEnv(int index)
        {
            if ((FerroEnv?.Count ?? 0) == 0)
            {
                return default;
            }

            return FerroEnv[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista FerroEnv.
        /// </summary>
        public int GetFerroEnvCount => (FerroEnv != null ? FerroEnv.Count : 0);

#endif

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade ChCTeFerroOrigem deve ser serializada.
        /// </summary>
        public bool ShouldSerializeChCTeFerroOrigem() => !string.IsNullOrWhiteSpace(ChCTeFerroOrigem);

        #endregion
    }

    /// <summary>
    /// Ferroviários Envolvidos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.FerroEnv")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class FerroEnv
    {
        /// <summary>
        /// CNPJ do Ferroviário Envolvido.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Código Interno do Ferroviário Envolvido.
        /// </summary>
        [XmlElement("cInt")]
        public string CInt { get; set; }

        /// <summary>
        /// Inscrição Estadual do Ferroviário Envolvido.
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Razão Social ou Nome do Ferroviário Envolvido.
        /// </summary>
        [XmlElement("xNome")]
        public string XNome { get; set; }

        /// <summary>
        /// Endereço do Ferroviário Envolvido.
        /// </summary>
        [XmlElement("enderFerro")]
        public EnderFerro EnderFerro { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CInt deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCInt() => !string.IsNullOrWhiteSpace(CInt);

        /// <summary>
        /// Verifica se a propriedade IE deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        #endregion
    }

    /// <summary>
    /// Endereço do Ferroviário Envolvido.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.EnderFerro")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class EnderFerro
    {
        /// <summary>
        /// Logradouro.
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr { get; set; }

        /// <summary>
        /// Número.
        /// </summary>
        [XmlElement("nro")]
        public string Nro { get; set; }

        /// <summary>
        /// Complemento.
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl { get; set; }

        /// <summary>
        /// Bairro.
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro { get; set; }

        /// <summary>
        /// Código do Município.
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do Município.
        /// </summary>
        [XmlElement("xMun")]
        public string XMun { get; set; }

        /// <summary>
        /// CEP.
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// UF.
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade Nro deve ser serializada.
        /// </summary>
        public bool ShouldSerializeNro() => !string.IsNullOrWhiteSpace(Nro);

        /// <summary>
        /// Verifica se a propriedade XCpl deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        /// <summary>
        /// Verifica se a propriedade XBairro deve ser serializada.
        /// </summary>
        public bool ShouldSerializeXBairro() => !string.IsNullOrWhiteSpace(XBairro);

        #endregion
    }

    /// <summary>
    /// Informações dos veículos novos.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.VeicNovos")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class VeicNovos
    {
        /// <summary>
        /// Chassi.
        /// </summary>
        [XmlElement("chassi")]
        public string Chassi { get; set; }

        /// <summary>
        /// Código da Cor.
        /// </summary>
        [XmlElement("cCor")]
        public string CCor { get; set; }

        /// <summary>
        /// Descrição da Cor.
        /// </summary>
        [XmlElement("xCor")]
        public string XCor { get; set; }

        /// <summary>
        /// Código do Modelo.
        /// </summary>
        [XmlElement("cMod")]
        public string CMod { get; set; }

        /// <summary>
        /// Valor Unitário.
        /// </summary>
        [XmlIgnore]
        public double VUnit { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VUnit" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vUnit")]
        public string VUnitField
        {
            get => VUnit.ToString("F2", CultureInfo.InvariantCulture);
            set => VUnit = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Frete.
        /// </summary>
        [XmlIgnore]
        public double VFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VFrete" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Utility.Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações da cobrança.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Cobr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Cobr
    {
        /// <summary>
        /// Informações da Fatura.
        /// </summary>
        [XmlElement("fat")]
        public Fat Fat { get; set; }

        /// <summary>
        /// Informações das Duplicatas.
        /// </summary>
        [XmlElement("dup")]
        public List<Dup> Dup { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de duplicatas.
        /// </summary>
        /// <param name="dup">Elemento Dup a ser adicionado.</param>
        public void AddDup(Dup dup)
        {
            if (Dup == null)
            {
                Dup = new List<Dup>();
            }

            Dup.Add(dup);
        }

        /// <summary>
        /// Retorna o elemento da lista Dup (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da Dup.</returns>
        public Dup GetDup(int index)
        {
            if ((Dup?.Count ?? 0) == 0)
            {
                return default;
            }

            return Dup[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Dup.
        /// </summary>
        public int GetDupCount => (Dup != null ? Dup.Count : 0);

#endif
    }

    /// <summary>
    /// Informações da Fatura.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Fat")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Fat
    {
        /// <summary>
        /// Número da Fatura.
        /// </summary>
        [XmlElement("nFat")]
        public string NFat { get; set; }

        /// <summary>
        /// Valor Original da Fatura.
        /// </summary>
        [XmlIgnore]
        public double VOrig { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VOrig" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOrig")]
        public string VOrigField
        {
            get => VOrig.ToString("F2", CultureInfo.InvariantCulture);
            set => VOrig = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Desconto da Fatura.
        /// </summary>
        [XmlIgnore]
        public double VDesc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VDesc" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Líquido da Fatura.
        /// </summary>
        [XmlIgnore]
        public double VLiq { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VLiq" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vLiq")]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Utility.Converter.ToDouble(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade VDescField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeVDescField() => VDesc > 0;

        #endregion
    }

    /// <summary>
    /// Informações das Duplicatas.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.Dup")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class Dup
    {
        /// <summary>
        /// Número da Duplicata.
        /// </summary>
        [XmlElement("nDup")]
        public string NDup { get; set; }

        /// <summary>
        /// Data de Vencimento da Duplicata.
        /// </summary>
        [XmlIgnore]
        public DateTime DVenc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DVenc" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor da Duplicata.
        /// </summary>
        [XmlIgnore]
        public double VDup { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "VDup" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDup")]
        public string VDupField
        {
            get => VDup.ToString("F2", CultureInfo.InvariantCulture);
            set => VDup = Utility.Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Informações do CT-e de substituição.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteSub")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteSub
    {
        /// <summary>
        /// Chave de acesso do CT-e.
        /// </summary>
        [XmlElement("chCte")]
        public string ChCte { get; set; }

        //TODO: Wandrey - Remover a tag RefCteAnu quando a versão 3.00 do CTe não existir mais.
        /// <summary>
        /// Propriedade só existe até a versão 3.00 do schema do CTe.
        /// </summary>
        [XmlElement("refCteAnu")]
        public string RefCteAnu { get; set; }

        /// <summary>
        /// Informações do Tomador do Serviço onde será entregue a mercadoria.
        /// </summary>
        [XmlElement("tomaICMS")]
        public TomaICMS TomaICMS { get; set; }

        /// <summary>
        /// Indicador de alteração do tomador.
        /// </summary>
        [XmlIgnore]
        public SimNao IndAlteraToma { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "IndAlteraToma" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("indAlteraToma")]
        public string IndAlteraTomaField
        {
            get => (IndAlteraToma == SimNao.Sim ? "1" : "");
            set => IndAlteraToma = (SimNao)Enum.Parse(typeof(SimNao), value.ToString());
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade RefCteAnu deve ser serializada.
        /// </summary>
        public bool ShouldSerializeRefCteAnu() => !string.IsNullOrWhiteSpace(RefCteAnu);

        /// <summary>
        /// Verifica se a propriedade IndAlteraTomaField deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIndAlteraTomaField() => IndAlteraToma == SimNao.Sim;

        #endregion
    }

    /// <summary>
    /// Informações do Tomador do Serviço onde será entregue a mercadoria.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.TomaICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class TomaICMS
    {
        /// <summary>
        /// Chave de acesso da NF-e.
        /// </summary>
        [XmlElement("refNFe")]
        public string RefNFe { get; set; }

        /// <summary>
        /// Informações da Nota Fiscal.
        /// </summary>
        [XmlElement("refNF")]
        public RefNF RefNF { get; set; }

        /// <summary>
        /// Chave de acesso do CT-e.
        /// </summary>
        [XmlElement("refCte")]
        public string RefCte { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade RefNFe deve ser serializada.
        /// </summary>
        public bool ShouldSerializeRefNFe() => !string.IsNullOrWhiteSpace(RefNFe);

        /// <summary>
        /// Verifica se a propriedade RefCte deve ser serializada.
        /// </summary>
        public bool ShouldSerializeRefCte() => !string.IsNullOrWhiteSpace(RefCte);

        #endregion
    }

    /// <summary>
    /// Informações da Nota Fiscal.
    /// </summary>
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

        /// <summary>
        /// CNPJ do Emitente da Nota Fiscal.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do Emitente da Nota Fiscal.
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Modelo da Nota Fiscal.
        /// </summary>
        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set => ModField = value;
        }

        /// <summary>
        /// Série da Nota Fiscal.
        /// </summary>
        [XmlElement("serie")]
        public string Serie { get; set; }

        /// <summary>
        /// Subserie da Nota Fiscal.
        /// </summary>
        [XmlElement("subserie")]
        public string Subserie { get; set; }

        /// <summary>
        /// Número da Nota Fiscal.
        /// </summary>
        [XmlElement("nro")]
        public int Nro { get; set; }

        /// <summary>
        /// Valor da Nota Fiscal.
        /// </summary>
        [XmlIgnore]
        public double Valor { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "Valor" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("valor")]
        public string ValorField
        {
            get => Valor.ToString("F2", CultureInfo.InvariantCulture);
            set => Valor = Utility.Converter.ToDouble(value);
        }

        /// <summary>
        /// Data de Emissão da Nota Fiscal.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        /// <summary>
        /// Verifica se a propriedade Subserie deve ser serializada.
        /// </summary>
        public bool ShouldSerializeSubserie() => !string.IsNullOrWhiteSpace(Subserie);

        #endregion
    }

    /// <summary>
    /// Informações do CT-e globalizado.
    /// </summary>
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

        /// <summary>
        /// Observações.
        /// </summary>
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

    /// <summary>
    /// Informações dos serviços vinculados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfServVinc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfServVinc
    {
        /// <summary>
        /// Informações dos CTes Multimodais vinculados.
        /// </summary>
        [XmlElement("infCTeMultimodal")]
        public List<InfCTeMultimodal> InfCTeMultimodal { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento à lista de informações de CTe Multimodal.
        /// </summary>
        /// <param name="infCTeMultimodal">Elemento InfCTeMultimodal a ser adicionado.</param>
        public void AddInfCTeMultimodal(InfCTeMultimodal infCTeMultimodal)
        {
            if (InfCTeMultimodal == null)
            {
                InfCTeMultimodal = new List<InfCTeMultimodal>();
            }

            InfCTeMultimodal.Add(infCTeMultimodal);
        }

        /// <summary>
        /// Retorna o elemento da lista InfCTeMultimodal (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista).
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero)).</param>
        /// <returns>Conteúdo do índice passado por parâmetro da InfCTeMultimodal.</returns>
        public InfCTeMultimodal GetInfCTeMultimodal(int index)
        {
            if ((InfCTeMultimodal?.Count ?? 0) == 0)
            {
                return default;
            }

            return InfCTeMultimodal[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista InfCTeMultimodal.
        /// </summary>
        public int GetInfCTeMultimodalCount => (InfCTeMultimodal != null ? InfCTeMultimodal.Count : 0);

#endif
    }

    /// <summary>
    /// Informações dos CTes Multimodais vinculados.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeMultimodal")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeMultimodal
    {
        /// <summary>
        /// Chave de acesso do CT-e Multimodal.
        /// </summary>
        [XmlElement("chCTeMultimodal")]
        public string ChCTeMultimodal { get; set; }
    }

    /// <summary>
    /// Informações do CTe complementar.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteComp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteComp
    {
        /// <summary>
        /// Chave de acesso do CT-e Complementar.
        /// </summary>
        [XmlElement("chCTe")]
        public string ChCTe { get; set; }
    }

    /// <summary>
    /// Informações do CTe de anulação.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCteAnu")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCteAnu
    {
        /// <summary>
        /// Chave de acesso do CT-e Anulado.
        /// </summary>
        [XmlElement("chCte")]
        public string ChCte { get; set; }

        /// <summary>
        /// Data de Emissão do CT-e Anulado.
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade "DEmi" para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Pessoas autorizadas a acessar o XML do CTe.
    /// </summary>
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

        /// <summary>
        /// CNPJ autorizado a acessar o XML.
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
        /// CPF autorizado a acessar o XML.
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
        /// Verifica se a propriedade CNPJ deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        /// <summary>
        /// Verifica se a propriedade CPF deve ser serializada.
        /// </summary>
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

    /// <summary>
    /// Informações do responsável técnico pelo sistema emissor do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfRespTec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfRespTec
    {
        /// <summary>
        /// CNPJ do responsável técnico.
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Nome da pessoa de contato do responsável técnico.
        /// </summary>
        [XmlElement("xContato")]
        public string XContato { get; set; }

        /// <summary>
        /// Email do responsável técnico.
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        /// <summary>
        /// Telefone do responsável técnico.
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Identificador do CSRT.
        /// </summary>
        [XmlElement("idCSRT")]
        public string IdCSRT { get; set; }

        /// <summary>
        /// Hash do CSRT.
        /// </summary>
        [XmlElement("hashCSRT", DataType = "base64Binary")]
        public byte[] HashCSRT { get; set; }

        #region ShouldSerialize

        /// <summary>
        /// Verifica se a propriedade IdCSRT deve ser serializada.
        /// </summary>
        public bool ShouldSerializeIdCSRT() => !string.IsNullOrWhiteSpace(IdCSRT);

        /// <summary>
        /// Verifica se a propriedade HashCSRT deve ser serializada.
        /// </summary>
        public bool ShouldSerializeHashCSRT() => HashCSRT != null;

        #endregion
    }

    /// <summary>
    /// Grupo de Informações do Pedido da NFF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfSolicNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfSolicNFF
    {
        /// <summary>
        /// Texto com a solicitação do tomador para geração de NF de serviço.
        /// </summary>
        [XmlElement("xSolic", Order = 0)]
        public string XSolic { get; set; }
    }

    /// <summary>
    /// Informações suplementares do CTe.
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.InfCTeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class InfCTeSupl
    {
        /// <summary>
        /// URL do QRCode do CT-e.
        /// </summary>
        [XmlElement("qrCodCTe")]
        public string QrCodCTe { get; set; }
    }

    /// <summary>
    /// Informações do Imposto de Bens e Serviços - IBS e da Contribuição de Bens e Serviços - CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.IBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class IBSCBS
    {
        /// <summary>
        /// Código de Situação Tributária do IBS e CBS
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Código de classificação tributária do IBS e CBS
        /// </summary>
        [XmlElement("cClassTrib")]
        public string CClassTrib { get; set; }

        /// <summary>
        /// Grupo de Informações específicas do IBS e da CBS
        /// </summary>
        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }
    }

    /// <summary>
    /// Grupo de Informações específicas do IBS e da CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GIBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSCBS
    {
        /// <summary>
        /// Base de cálculo do IBS e CBS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de Informações do IBS para a UF
        /// </summary>
        [XmlElement("gIBSUF")]
        public GIBSUF GIBSUF { get; set; }

        /// <summary>
        /// Grupo de Informações do IBS para o município
        /// </summary>
        [XmlElement("gIBSMun")]
        public GIBSMun GIBSMun { get; set; }

        /// <summary>
        /// Grupo de Informações da CBS
        /// </summary>
        [XmlElement("gCBS")]
        public GCBS GCBS { get; set; }

        /// <summary>
        /// Grupo de informações da Tributação Regular
        /// </summary>
        [XmlElement("gTribRegular")]
        public GTribRegular GTribRegular { get; set; }

        /// <summary>
        /// Grupo de Informações do Crédito Presumido referente ao IBS
        /// </summary>
        [XmlElement("gIBSCredPres")]
        public GIBSCredPres GIBSCredPres { get; set; }

        /// <summary>
        /// Grupo de Informações do Crédito Presumido referente ao CBS
        /// </summary>
        [XmlElement("GCBSCredPres")]
        public GCBSCredPres GCBSCredPres { get; set; }
    }

    /// <summary>
    /// Grupo de Informações do IBS para a UF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GIBSUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSUF
    {
        /// <summary>
        /// Alíquota do IBS de competência das UF
        /// </summary>
        [XmlIgnore]
        public double PIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIBSUF")]
        public string PIBSUFField
        {
            get => PIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de Informações do Diferimento
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de Informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        /// <summary>
        /// Grupo de informações da redução da alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor do IBS de competência da UF
        /// </summary>
        [XmlIgnore]
        public double VIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSUF")]
        public string VIBSUFField
        {
            get => VIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSUF = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações do Diferimento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GDif")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GDif
    {
        /// <summary>
        /// Alíquota do Diferimento
        /// </summary>
        [XmlIgnore]
        public double PDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Diferimento 
        /// </summary>
        [XmlIgnore]
        public double VDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDif")]
        public string VDifField
        {
            get => VDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VDif = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações da devolução de tributos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GDevTrib")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GDevTrib
    {
        /// <summary>
        /// Valor do tributo devolvido
        /// </summary>
        [XmlIgnore]
        public double VDevTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vDevTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDevTrib")]
        public string VDevTribField
        {
            get => VDevTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VDevTrib = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da redução da alíquota
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GRed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GRed
    {
        /// <summary>
        /// Percentual da redução de alíquota
        /// </summary>
        [XmlIgnore]
        public double PRedAliq { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pRedAliq para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedAliq")]
        public string PRedAliqField
        {
            get => PRedAliq.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedAliq = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota Efetiva do IBS/CBS que será aplicada a Base de Cálculo
        /// </summary>
        [XmlIgnore]
        public double PAliqEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pAliqEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfet")]
        public string PAliqEfetField
        {
            get => PAliqEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfet = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações do IBS para o município
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GIBSMun")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSMun
    {
        /// <summary>
        /// Alíquota do IBS de competência do Município
        /// </summary>
        [XmlIgnore]
        public double PIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIBSMun")]
        public string PIBSMunField
        {
            get => PIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de Informações do Diferimento
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de Informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        /// <summary>
        /// Grupo de informações da redução da alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor do IBS de competência do Município
        /// </summary>
        [XmlIgnore]
        public double VIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMun")]
        public string VIBSMunField
        {
            get => VIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMun = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações da CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GCBS
    {
        /// <summary>
        /// Alíquota do CBS
        /// </summary>
        [XmlIgnore]
        public double PCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCBS")]
        public string PCBSField
        {
            get => PCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo de Informações do Diferimento
        /// </summary>
        [XmlElement("gDif")]
        public GDif GDif { get; set; }

        /// <summary>
        /// Grupo de Informações da devolução de tributos
        /// </summary>
        [XmlElement("gDevTrib")]
        public GDevTrib GDevTrib { get; set; }

        /// <summary>
        /// Grupo de informações da redução da alíquota
        /// </summary>
        [XmlElement("gRed")]
        public GRed GRed { get; set; }

        /// <summary>
        /// Valor da CBS 
        /// </summary>
        [XmlIgnore]
        public double VCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBS")]
        public string VCBSField
        {
            get => VCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de informações da Tributação Regular
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GTribRegular")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GTribRegular
    {
        /// <summary>
        /// Código de Situação Tributária do IBS e CBS 
        /// </summary>
        [XmlElement("CSTReg")]
        public string CSTReg { get; set; }

        /// <summary>
        /// Código de Classificação Tributária do IBS e CBS 
        /// </summary>
        [XmlElement("cClassTribReg")]
        public string CClassTribReg { get; set; }

        /// <summary>
        /// Valor da alíquota do IBS da UF 
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pAliqEfetRegIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegIBSUF")]
        public string PAliqEfetRegIBSUFField
        {
            get => PAliqEfetRegIBSUF.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo do IBS da UF
        /// </summary>
        [XmlIgnore]
        public double VTribRegIBSUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vTribRegIBSUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegIBSUF")]
        public string VTribRegIBSUFField
        {
            get => VTribRegIBSUF.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSUF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da alíquota do IBS do Município
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pAliqEfetRegIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegIBSMun")]
        public string PAliqEfetRegIBSMunField
        {
            get => PAliqEfetRegIBSMun.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo do IBS do Município
        /// </summary>
        [XmlIgnore]
        public double VTribRegIBSMun { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vTribRegIBSMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegIBSMun")]
        public string VTribRegIBSMunField
        {
            get => VTribRegIBSMun.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegIBSMun = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da alíquota da CBS
        /// </summary>
        [XmlIgnore]
        public double PAliqEfetRegCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pAliqEfetRegCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pAliqEfetRegCBS")]
        public string PAliqEfetRegCBSField
        {
            get => PAliqEfetRegCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PAliqEfetRegCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Tributo da CBS
        /// </summary>
        [XmlIgnore]
        public double VTribRegCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vTribRegCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTribRegCBS")]
        public string VTribRegCBSField
        {
            get => VTribRegCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VTribRegCBS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações do Crédito Presumido referente ao IBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GIBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GIBSCredPres
    {
        /// <summary>
        /// Código de Classificação do Crédito Presumido
        /// </summary>
        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        /// <summary>
        /// Percentual do Crédito Presumido
        /// </summary>
        [XmlIgnore]
        public double PCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Crédito Presumido
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Crédito Presumido em condição suspensiva.
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Grupo de Informações do Crédito Presumido referente ao CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.CTe.GCBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/cte")]
    public class GCBSCredPres
    {
        /// <summary>
        /// Código de Classificação do Crédito Presumido
        /// </summary>
        [XmlElement("cCredPres")]
        public string CCredPres { get; set; }

        /// <summary>
        /// Percentual do Crédito Presumido
        /// </summary>
        [XmlIgnore]
        public double PCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredPres")]
        public string PCredPresField
        {
            get => PCredPres.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Crédito Presumido
        /// </summary>
        [XmlIgnore]
        public double VCredPres { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCredPres para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPres")]
        public string VCredPresField
        {
            get => VCredPres.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPres = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Crédito Presumido em condição suspensiva.
        /// </summary>
        [XmlIgnore]
        public double VCredPresCondSus { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCredPresCondSus para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresCondSus")]
        public string VCredPresCondSusField
        {
            get => VCredPresCondSus.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresCondSus = Converter.ToDouble(value);
        }
    }
}
