#pragma warning disable CS1591

#if INTEROP
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

namespace Unimake.Business.DFe.Xml.NFe
{
    /// <summary>
    /// Classe de lote da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnviNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlRoot("enviNFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class EnviNFe : XMLBase
    {
        /// <summary>
        /// Versão do schema do XML do lote da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Número do lote
        /// </summary>
        [XmlElement("idLote")]
        public string IdLote { get; set; }

        /// <summary>
        /// Indicador de processamento síncrono
        /// </summary>
        [XmlElement("indSinc")]
        public SimNao IndSinc { get; set; }

        /// <summary>
        /// NFe/NFCe
        /// </summary>
        [XmlElement("NFe")]
        public List<NFe> NFe { get; set; }

        public override XmlDocument GerarXML()
        {
            var xmlDoc = base.GerarXML();

            foreach (var nodeEnvNFe in xmlDoc.GetElementsByTagName("enviNFe"))
            {
                var elemEnvNFe = (XmlElement)nodeEnvNFe;

                foreach (var nodeNFe in elemEnvNFe.GetElementsByTagName("NFe"))
                {
                    var elemNFe = (XmlElement)nodeNFe;

                    var attribute = GetType().GetCustomAttribute<XmlRootAttribute>();
                    elemNFe.SetAttribute("xmlns", attribute.Namespace);
                }
            }

            return xmlDoc;
        }

        /// <summary>
        /// Desserializar o XML EnviNFe no objeto EnviNFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML EnviNFe</param>
        /// <returns>Objeto do EnviNFe</returns>
        public EnviNFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<EnviNFe>(doc);
        }

        /// <summary>
        /// Desserializar o XML EnviNFe no objeto EnviNFe
        /// </summary>
        /// <param name="xml">string do XML EnviNFe</param>
        /// <returns>Objeto da EnviNFe</returns>
        public EnviNFe LoadFromXML(string xml) => XMLUtility.Deserializar<EnviNFe>(xml);

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nfe">Elemento</param>
        public void AddNFe(NFe nfe)
        {
            if (NFe == null)
            {
                NFe = new List<NFe>();
            }

            NFe.Add(nfe);
        }

        /// <summary>
        /// Retorna o elemento da lista NFe (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFe</returns>
        public NFe GetNFe(int index)
        {
            if ((NFe?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFe[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFe
        /// </summary>
        public int GetNFeCount => (NFe != null ? NFe.Count : 0);

#endif
    }

    /// <summary>
    /// Classe da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    [XmlRoot("NFe", Namespace = "http://www.portalfiscal.inf.br/nfe", IsNullable = false)]
    public class NFe
    {
        /// <summary>
        /// Informações da NFe/NFCe
        /// </summary>
        [XmlElement("infNFe")]
        public List<InfNFe> InfNFe { get; set; }

        /// <summary>
        /// Informações suplementares da NFe/NFCe
        /// </summary>
        [XmlElement("infNFeSupl")]
        public InfNFeSupl InfNFeSupl { get; set; }

        [XmlElement(ElementName = "Signature", Namespace = "http://www.w3.org/2000/09/xmldsig#")]
        public Signature Signature { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="infNFe">Elemento</param>
        public void AddInfNFe(InfNFe infNFe)
        {
            if (InfNFe == null)
            {
                InfNFe = new List<InfNFe>();
            }

            InfNFe.Add(infNFe);
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

#endif
        /// <summary>
        /// Deserializar o XML NFe no objeto NFe
        /// </summary>
        /// <param name="filename">Localização do arquivo XML NFe</param>
        /// <returns>Objeto da NFe</returns>
        public NFe LoadFromFile(string filename)
        {
            var doc = new XmlDocument();
            doc.LoadXml(System.IO.File.ReadAllText(filename, Encoding.UTF8));
            return XMLUtility.Deserializar<NFe>(doc);
        }

        /// <summary>
        /// Deserializar o XML NFe no objeto NFe
        /// </summary>
        /// <param name="xml">string do XML NFe</param>
        /// <returns>Objeto da NFe</returns>
        public NFe LoadFromXML(string xml) => XMLUtility.Deserializar<NFe>(xml);
    }

    /// <summary>
    /// Classe de informações da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfNFe")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfNFe
    {
        private string IdField;

        /// <summary>
        /// Versão do schema do XML da NFe/NFCe
        /// </summary>
        [XmlAttribute(AttributeName = "versao", DataType = "token")]
        public string Versao { get; set; }

        /// <summary>
        /// Identificação da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("ide", Order = 1)]
#else
        [XmlElement("ide")]
#endif
        public Ide Ide { get; set; }

        /// <summary>
        /// Identificação do emitente
        /// </summary>
#if INTEROP
        [XmlElement("emit", Order = 2)]
#else
        [XmlElement("emit")]
#endif
        public Emit Emit { get; set; }

        /// <summary>
        /// Esta TAG é de uso exclusivo do FISCO, não precisa gerar nada, só temos ela para caso de alguma necessidade de desserialização.
        /// </summary>
#if INTEROP
        [XmlElement("avulsa", Order = 3)]
#else
        [XmlElement("avulsa")]
#endif
        public Avulsa Avulsa { get; set; }

        /// <summary>
        /// Identificação do destinatário
        /// </summary>
#if INTEROP
        [XmlElement("dest", Order = 4)]
#else
        [XmlElement("dest")]
#endif
        public Dest Dest { get; set; }

        /// <summary>
        /// Identificação do local de retirada (informar apenas quando for diferente do endereço do remetente)
        /// </summary>
#if INTEROP
        [XmlElement("retirada", Order = 5)]
#else
        [XmlElement("retirada")]
#endif
        public Retirada Retirada { get; set; }

        /// <summary>
        /// Identificação do Local de Entrega (informar apenas quando for diferente do endereço do destinatário)
        /// </summary>
#if INTEROP
        [XmlElement("entrega", Order = 6)]
#else
        [XmlElement("entrega")]
#endif
        public Entrega Entrega { get; set; }

        /// <summary>
        /// Pessoas autorizadas para o download do XML da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("autXML", Order = 7)]
#else
        [XmlElement("autXML")]
#endif
        public List<AutXML> AutXML { get; set; }

        /// <summary>
        /// Dados dos detalhes da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("det", Order = 8)]
#else
        [XmlElement("det")]
#endif
        public List<Det> Det { get; set; }

        /// <summary>
        /// Dados dos totais da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("total", Order = 9)]
#else
        [XmlElement("total")]
#endif
        public Total Total { get; set; }

        /// <summary>
        /// Dados dos transportes da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("transp", Order = 10)]
#else
        [XmlElement("transp")]
#endif
        public Transp Transp { get; set; }

        /// <summary>
        /// Dados da cobrança da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("cobr", Order = 11)]
#else
        [XmlElement("cobr")]
#endif
        public Cobr Cobr { get; set; }

        /// <summary>
        /// Dados de Pagamento. Obrigatório apenas para (NFC-e) NT 2012/004
        /// </summary>
#if INTEROP
        [XmlElement("pag", Order = 12)]
#else
        [XmlElement("pag")]
#endif
        public Pag Pag { get; set; }

        /// <summary>
        /// Grupo de Informações do Intermediador da Transação
        /// </summary>
#if INTEROP
        [XmlElement("infIntermed", Order = 13)]
#else
        [XmlElement("infIntermed")]
#endif
        public InfIntermed InfIntermed { get; set; }

        /// <summary>
        /// Informações adicionais da NFe/NFCe
        /// </summary>
#if INTEROP
        [XmlElement("infAdic", Order = 14)]
#else
        [XmlElement("infAdic")]
#endif
        public InfAdic InfAdic { get; set; }

        /// <summary>
        /// Informações de exportação
        /// </summary>
#if INTEROP
        [XmlElement("exporta", Order = 15)]
#else
        [XmlElement("exporta")]
#endif
        public Exporta Exporta { get; set; }

        /// <summary>
        /// Informações de compras  (Nota de Empenho, Pedido e Contrato)
        /// </summary>
#if INTEROP
        [XmlElement("compra", Order = 16)]
#else
        [XmlElement("compra")]
#endif
        public Compra Compra { get; set; }

        /// <summary>
        /// Informações de registro aquisições de cana
        /// </summary>
#if INTEROP
        [XmlElement("cana", Order = 17)]
#else
        [XmlElement("cana")]
#endif
        public Cana Cana { get; set; }

        /// <summary>
        /// Informações do Responsável Técnico pela emissão do DFe
        /// </summary>
#if INTEROP
        [XmlElement("infRespTec", Order = 18)]
#else
        [XmlElement("infRespTec")]
#endif
        public InfRespTec InfRespTec { get; set; }

        /// <summary>
        /// Grupo para informações da solicitação da NFF
        /// </summary>
#if INTEROP
        [XmlElement("infSolicNFF", Order = 19)]
#else
        [XmlElement("infSolicNFF")]
#endif
        public InfSolicNFF InfSolicNFF { get; set; }

        /// <summary>
        /// Grupo de tags de produtos agropecuários: animais, vegetais e florestais
        /// </summary>
#if INTEROP
        [XmlElement("agropecuario", Order = 20)]
#else
        [XmlElement("agropecuario")]
#endif
        public Agropecuario Agropecuario { get; set; }

        /// <summary>
        /// ID da NFe/NFCe. Composição: NFe + chave
        /// </summary>
        [XmlAttribute(AttributeName = "Id", DataType = "ID")]
        public string Id
        {
            get
            {
                IdField = "NFe" + Chave;
                return IdField;
            }
            set => IdField = value;
        }

        private string ChaveField;

        /// <summary>
        /// Chave de acesso da NFe/NFCe
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
                    NumeroDoctoFiscal = Ide.NNF,
                    TipoEmissao = (TipoEmissao)(int)Ide.TpEmis,
                    CodigoNumerico = Ide.CNF
                };
                ChaveField = XMLUtility.MontarChaveNFe(ref conteudoChaveDFe);
                Ide.CDV = conteudoChaveDFe.DigitoVerificador;

                if (InfRespTec != null)
                {
                    InfRespTec.GerarHashCSRT(ChaveField);
                }

                return ChaveField;
            }
            set => throw new Exception("Não é permitido atribuir valor para a propriedade Chave. Ela é calculada automaticamente.");
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="autXML">Elemento</param>
        public void AddAutXml(AutXML autXML)
        {
            if (AutXML == null)
            {
                AutXML = new List<AutXML>();
            }

            AutXML.Add(autXML);
        }

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="det">Elemento</param>
        public void AddDet(Det det)
        {
            if (Det == null)
            {
                Det = new List<Det>();
            }

            Det.Add(det);
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
    }

    /// <summary>
    /// Classe da identificação da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Ide")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Ide
    {
        private string CNFField;
        private string _natOp = "";
        private string XJustField;

        /// <summary>
        /// Código da UF do emitente do Documento Fiscal
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Código numérico que compõe a Chave de Acesso. Número aleatório gerado pelo emitente para cada NFe/NFCe
        /// </summary>
        [XmlElement("cNF")]
        public string CNF
        {
            get
            {
                string retorno;
                if (string.IsNullOrWhiteSpace(CNFField))
                {
                    if (NNF < 0)
                    {
                        throw new Exception("Defina o conteúdo da TAG <nNF>, pois a mesma é utilizada como base para calcular o código numérico.");
                    }

                    retorno = XMLUtility.GerarCodigoNumerico(NNF).ToString("00000000");
                }
                else
                {
                    retorno = CNFField;
                }

                return retorno;
            }
            set => CNFField = value;
        }

        /// <summary>
        /// Descrição da Natureza da Operação
        /// </summary>
        [XmlElement("natOp")]
        public string NatOp
        {
            get => _natOp;
            set => _natOp = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Código do modelo do Documento Fiscal. 55 = NF-e; 65 = NFC-e
        /// </summary>
        [XmlElement("mod")]
        public ModeloDFe Mod { get; set; }

        /// <summary>
        /// Série do Documento Fiscal
        /// Série normal: 0-889
        /// Avulsa Fisco: 890-899
        /// SCAN: 900-999
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do documento fiscal
        /// </summary>
        [XmlElement("nNF")]
        public int NNF { get; set; }

        /// <summary>
        /// Data e Hora de emissão do Documento Fiscal (AAAA-MM-DDThh:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhEmi { get; set; }
#else
        public DateTimeOffset DhEmi { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhEmi para atribuir ou resgatar o valor)
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
        /// Data e Hora da saída ou de entrada da mercadoria / produto (AAAA-MM-DDTHH:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhSaiEnt { get; set; }
#else
        public DateTimeOffset DhSaiEnt { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhSaiEnt para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dhSaiEnt")]
        public string DhSaiEntField
        {
            get => DhSaiEnt.ToString("yyyy-MM-ddTHH:mm:sszzz");
#if INTEROP
            set => DhSaiEnt = DateTime.Parse(value);
#else
            set => DhSaiEnt = DateTimeOffset.Parse(value);
#endif
        }

        /// <summary>
        /// Tipo do Documento Fiscal
        /// </summary>
        [XmlElement("tpNF")]
        public TipoOperacao TpNF { get; set; }

        /// <summary>
        /// Identificador de Local de destino da operação
        /// </summary>
        [XmlElement("idDest")]
        public DestinoOperacao IdDest { get; set; }

        /// <summary>
        /// Código do Município de Ocorrência do Fato Gerador (utilizar a tabela do IBGE)
        /// </summary>
        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }

        /// <summary>
        /// Informar o município de ocorrência do fato gerador do IBS / CBS.
        /// </summary>
        [XmlElement("cMunFGIBS")]
        public int CMunFGIBS { get; set; }

        /// <summary>
        /// Formato de impressão do DANFE
        /// </summary>
        [XmlElement("tpImp")]
        public FormatoImpressaoDANFE TpImp { get; set; }

        /// <summary>
        /// Forma de emissão da NFe/NFCe
        /// </summary>
        [XmlElement("tpEmis")]
        public TipoEmissao TpEmis { get; set; }

        /// <summary>
        /// Digito Verificador da Chave de Acesso
        /// </summary>
        [XmlElement("cDV")]
        public int CDV { get; set; }

        /// <summary>
        /// Tipo de ambiente
        /// </summary>
        [XmlElement("tpAmb")]
        public TipoAmbiente TpAmb { get; set; }

        /// <summary>
        /// Finalidade da emissão da NFe/NFCe
        /// </summary>
        [XmlElement("finNFe")]
        public FinalidadeNFe FinNFe { get; set; }

        /// <summary>
        /// Tipo de nota de débito
        /// </summary>
        [XmlElement("tpNFDebito")]

#if INTEROP
        public TipoNFDebito TpNFDebito { get; set; } = (TipoNFDebito)(-1);
#else
        public TipoNFDebito? TpNFDebito { get; set; }
#endif

        /// <summary>
        /// Tipo de nota de crédito
        /// </summary>
        [XmlElement("tpNFCredito")]
#if INTEROP
        public TipoNFCredito TpNFCredito { get; set; } = (TipoNFCredito)(-1);
#else
        public TipoNFCredito? TpNFCredito { get; set; }
#endif

        /// <summary>
        /// Indica operação com consumidor final
        /// </summary>
        [XmlElement("indFinal")]
        public SimNao IndFinal { get; set; }

        /// <summary>
        /// Indicador de presença do comprador no estabelecimento comercial no momento da oepração
        /// </summary>
        [XmlElement("indPres")]
        public IndicadorPresenca IndPres { get; set; }

        /// <summary>
        /// Indicador de intermediador/marketplace
        /// </summary>
#if INTEROP
        /* ¯\_(ツ)_/¯
            Interop não aceita nulos, logo, passo -1 e valido no ShouldSerializeIndIntermed
        */
        [XmlElement("indIntermed")]
        public IndicadorIntermediario IndIntermed { get; set; } = (IndicadorIntermediario)(-1);
#else
        [XmlElement("indIntermed")]
        public IndicadorIntermediario? IndIntermed { get; set; }
#endif

        /// <summary>
        /// Processo de emissão utilizado
        /// </summary>
        [XmlElement("procEmi")]
        public ProcessoEmissao ProcEmi { get; set; }

        /// <summary>
        /// Versão do aplicativo utilizado no processo de emissão
        /// </summary>
        [XmlElement("verProc")]
        public string VerProc { get; set; }

        /// <summary>
        /// Informar a data e hora de entrada em contingência contingência no formato  (AAAA-MM-DDThh:mm:ssTZD)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public DateTime DhCont { get; set; }
#else
        public DateTimeOffset DhCont { get; set; }
#endif

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DhCont para atribuir ou resgatar o valor)
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
        /// Informar a Justificativa da entrada em contingência
        /// </summary>
        [XmlElement("xJust")]
        public string XJust
        {
            get => XJustField;
            set => XJustField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(256).Trim());
        }

        /// <summary>
        /// Grupo de informações da NF referenciada
        /// </summary>
        [XmlElement("NFref")]
        public List<NFref> NFref { get; set; }

        /// <summary>
        /// Grupo de compra governamental
        /// </summary>
        [XmlElement("gCompraGov")]
        public GCompraGov GCompraGov { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeDhContField() => DhCont > DateTime.MinValue;

        public bool ShouldSerializeXJust() => !string.IsNullOrWhiteSpace(XJust);

        public bool ShouldSerializeCMunFGIBS() => CMunFGIBS > 0;

#if INTEROP
        public bool ShouldSerializeTpNFDebito() => TpNFDebito != (TipoNFDebito)(-1);
#else
        public bool ShouldSerializeTpNFDebito() => TpNFDebito != null;
#endif

#if INTEROP
        public bool ShouldSerializeTpNFCredito() => TpNFCredito != (TipoNFCredito)(-1);
#else
        public bool ShouldSerializeTpNFCredito() => TpNFCredito != null;
#endif

        public bool ShouldSerializeIndIntermed()
        {
            var retorna = false;

            if (IndIntermed != (IndicadorIntermediario)(-1))
            {
                if (IndIntermed != null)
                {
                    if (IndPres != IndicadorPresenca.NaoSeAplica && IndPres != IndicadorPresenca.PresencialForaEstabelecimento)
                    {
                        retorna = true;
                    }
                }
            }

            return retorna;
        }

        public bool ShouldSerializeDhSaiEntField()
        {
            // ~\uninfe\doc\NFCe e NFe 3.10\NT2012.004_v1.2_NFCe.pdf
            // Página 06 item #14
            // Nota: Para a NFC-e este campo não deve existir
            if (Mod == ModeloDFe.NFCe)
            {
                return false;
            }

            return DhSaiEnt > DateTime.MinValue;
        }

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nFref">Elemento</param>
        public void AddNFref(NFref nFref)
        {
            if (NFref == null)
            {
                NFref = new List<NFref>();
            }

            NFref.Add(nFref);
        }

        /// <summary>
        /// Retorna o elemento da lista NFref (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NFref</returns>
        public NFref GetNFref(int index)
        {
            if ((NFref?.Count ?? 0) == 0)
            {
                return default;
            };

            return NFref[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NFref
        /// </summary>
        public int GetNFrefCount => (NFref != null ? NFref.Count : 0);

#endif
    }

    /// <summary>
    /// Classe para as informações da NFe/NFCe referenciada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.NFref")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class NFref
    {
        /// <summary>
        /// Referencia uma NF-e (modelo 55) emitida anteriormente, vinculada a NF-e atual, ou uma NFC-e(modelo 65)
        /// </summary>
        [XmlElement("refNFe")]
        public string RefNFe { get; set; }

        /// <summary>
        /// Referencia uma NF-e (modelo 55) emitida anteriormente pela sua Chave de Acesso com código numérico zerado, permitindo manter o sigilo da NF-e referenciada.
        /// </summary>
        [XmlElement("refNFeSig")]
        public string RefNFeSig { get; set; }

        /// <summary>
        /// Dados da NF modelo 1/1A referenciada ou NF modelo 2 referenciada
        /// </summary>
        [XmlElement("refNF")]
        public RefNF RefNF { get; set; }

        /// <summary>
        /// Grupo com as informações NF de produtor referenciada
        /// </summary>
        [XmlElement("refNFP")]
        public RefNFP RefNFP { get; set; }

        /// <summary>
        /// Utilizar esta TAG para referenciar um CT-e emitido anteriormente, vinculada a NFe/NFCe atual
        /// </summary>
        [XmlElement("refCTe")]
        public string RefCTe { get; set; }

        /// <summary>
        /// Grupo do Cupom Fiscal vinculado à NFe/NFCe
        /// </summary>
        [XmlElement("refECF")]
        public RefECF RefECF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeRefNFe() => !string.IsNullOrWhiteSpace(RefNFe);

        public bool ShouldSerializeRefNFeSig() => !string.IsNullOrWhiteSpace(RefNFeSig);

        public bool ShouldSerializeRefCTe() => !string.IsNullOrWhiteSpace(RefCTe);

        #endregion
    }

    /// <summary>
    /// Classe de dados da NF modelo 1/1A referenciada ou NF modelo 2 referenciada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefNF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefNF
    {
        private string ModField;
        private string AAMMField;

        /// <summary>
        /// Código da UF do emitente do Documento Fiscal. Utilizar a Tabela do IBGE
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// AAMM da emissão
        /// </summary>
        [XmlElement("AAMM")]
        public string AAMM
        {
            get => AAMMField;
            set
            {
                var mesesValidos = "01-02-03-04-05-06-07-08-09-10-11-12";

                if (!mesesValidos.Contains(value.Substring(2)))
                {
                    throw new Exception("Conteúdo da TAG <AAMM>, filha da TAG <refNF>, inválido! Mês informado deve estar entre 01 e 12.");
                }

                AAMMField = value;
            }
        }

        /// <summary>
        /// CNPJ do emitente do documento fiscal referenciado
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Código do modelo do Documento Fiscal. Utilizar 01 para NF modelo 1/1A e 02 para NF modelo 02
        /// </summary>
        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set => ModField = value;
        }

        /// <summary>
        /// Série do Documento Fiscal, informar zero se inexistente
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do Documento Fiscal
        /// </summary>
        [XmlElement("nNF")]
        public int NNF { get; set; }
    }

    /// <summary>
    /// Classe do grupo com as informações NF de produtor referenciada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefNFP")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefNFP
    {
        private string ModField;
        private string AAMMField;

        /// <summary>
        /// 
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUF")]
        public int CUFField
        {
            get => (int)CUF;
            set => CUF = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// AAMM da emissão
        /// </summary>
        [XmlElement("AAMM")]
        public string AAMM
        {
            get => AAMMField;
            set
            {
                var mesesValidos = "01-02-03-04-05-06-07-08-09-10-11-12";

                if (!mesesValidos.Contains(value.Substring(2)))
                {
                    throw new Exception("Conteúdo da TAG <AAMM>, filha da TAG <refNFP>, inválido! Mês informado deve estar entre 01 e 12.");
                }

                AAMMField = value;
            }
        }

        /// <summary>
        /// CNPJ do emitente da NF de produtor
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente da NF de produtor
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Inscrição estadual do emitente da NF de produtor
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Código do modelo do Documento Fiscal - utilizar 04 para NF de produtor  ou 01 para NF Avulsa
        /// </summary>
        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set
            {
                if (value != "01" && value != "04")
                {
                    throw new Exception("Conteúdo da TAG <mod>, filha da TAG <refNF>, inválido! Valores aceitos: 01 e 04.");
                }

                ModField = value;
            }
        }

        /// <summary>
        /// Série do Documento Fiscal, informar zero se inexistente
        /// </summary>
        [XmlElement("serie")]
        public int Serie { get; set; }

        /// <summary>
        /// Número do Documento Fiscal - 1 – 999999999
        /// </summary>
        [XmlElement("nNF")]
        public int NNF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

    /// <summary>
    /// Classe do grupo do cupom fiscal vinculado a NFe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RefECF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RefECF
    {
        private string ModField;

        /// <summary>
        /// Código do modelo do Documento Fiscal
        /// </summary>
        [XmlElement("mod")]
        public string Mod
        {
            get => ModField;
            set
            {
                if (value != "2B" && value != "2C" && value != "2D")
                {
                    throw new Exception("Conteúdo da TAG <mod>, filha da TAG <refECF>, inválido! Valores aceitos: 2B, 2C e 2D.");
                }

                ModField = value;
            }
        }

        /// <summary>
        /// Informar o número de ordem seqüencial do ECF que emitiu o Cupom Fiscal vinculado à NFe/NFCe
        /// </summary>
        [XmlElement("nECF")]
        public int NECF { get; set; }

        /// <summary>
        /// Informar o Número do Contador de Ordem de Operação - COO vinculado à NFe/NFCe
        /// </summary>
        [XmlElement("nCOO")]
        public int NCOO { get; set; }

    }

    /// <summary>
    /// Classe do emitente da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Emit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Emit
    {
        private string XNomeField;
        private string XFantField;

        /// <summary>
        /// CNPJ do emitente
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do emitente
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Razão Social ou Nome do emitente
        /// </summary>
        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Nome fantasia
        /// </summary>
        [XmlElement("xFant")]
        public string XFant
        {
            get => XFantField;
            set => XFantField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Endereço do emitente
        /// </summary>
        [XmlElement("enderEmit")]
        public EnderEmit EnderEmit { get; set; }

        /// <summary>
        /// Inscrição estadual do emitente
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Inscricao Estadual do Substituto Tributário
        /// </summary>
        [XmlElement("IEST")]
        public string IEST { get; set; }

        /// <summary>
        /// Inscrição Municipal
        /// </summary>
        [XmlElement("IM")]
        public string IM { get; set; }

        /// <summary>
        /// CNAE Fiscal
        /// </summary>
        [XmlElement("CNAE")]
        public string CNAE { get; set; }

        /// <summary>
        /// Código de Regime Tributário
        /// </summary>
        [XmlElement("CRT")]
        public CRT CRT { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIEST() => !string.IsNullOrWhiteSpace(IEST);

        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);

        public bool ShouldSerializeCNAE() => !string.IsNullOrWhiteSpace(CNAE) && !string.IsNullOrWhiteSpace(IM);

        #endregion
    }

    /// <summary>
    /// Classe do endereço do emitente da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnderEmit")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class EnderEmit
    {
        private string XLgrField;
        private string XCplField;
        private string NroField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Número
        /// </summary>
        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set
            {
                if (value == null)
                {
                    XCplField = value;
                }
                else
                {
                    XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
                }
            }
        }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Código IBGE do município
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Nome do município
        /// </summary>
        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Sigla da UF
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Código do país
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país
        /// </summary>
        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Telefone com DDD + número
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais);

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

    /// <summary>
    /// Classe de informações de emissão de avulsa, dados do Fisco emitente
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Avulsa")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Avulsa
    {
        /// <summary>
        /// CNPJ do Órgão emissor
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Órgão emitente
        /// </summary>
        [XmlElement("xOrgao")]
        public string XOrgao { get; set; }

        /// <summary>
        /// Matrícula do agente
        /// </summary>
        [XmlElement("matr")]
        public string Matr { get; set; }

        /// <summary>
        /// Nome do agente
        /// </summary>
        [XmlElement("xAgente")]
        public string XAgente { get; set; }

        /// <summary>
        /// Telefone
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Sigla da UF
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// Número do Documento de Arrecadação de Receita
        /// </summary>
        [XmlElement("nDAR")]
        public string NDAR { get; set; }

        /// <summary>
        /// Data de emissão do DAR (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DEmi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DEmi para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dEmi")]
        public string DEmiField
        {
            get => DEmi.ToString("yyyy-MM-dd");
            set => DEmi = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor Total constante no DAR
        /// </summary>
        [XmlElement("vDAR")]
        public string VDAR { get; set; }

        /// <summary>
        /// Repartição Fiscal emitente
        /// </summary>
        [XmlElement("repEmi")]
        public string RepEmi { get; set; }

        /// <summary>
        /// Data de pagamento do DAR (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DPag { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DPag para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPag")]
        public string DPagField
        {
            get => DPag.ToString("yyyy-MM-dd");
            set => DPag = DateTime.Parse(value);
        }
    }

    /// <summary>
    /// Classe de informações do destinatário
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Dest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Dest
    {
        private string XNomeField;

        /// <summary>
        /// CNPJ do destinatário
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do destinatário
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        [XmlIgnore]
        public string CpfCnpj
        {
            set
            {
                if (value.Length <= 11)
                {
                    CPF = value;
                }
                else
                {
                    CNPJ = value;
                }
            }
        }

        /// <summary>
        /// Identificador do destinatário, em caso de comprador estrangeiro
        /// </summary>
        [XmlElement("idEstrangeiro")]
        public string IdEstrangeiro { get; set; }

        /// <summary>
        /// Razão Social ou nome do destinatário
        /// </summary>
        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Endereço do destinatário
        /// </summary>
        [XmlElement("enderDest")]
        public EnderDest EnderDest { get; set; }

        /// <summary>
        /// Indicador da IE do destinatário
        /// </summary>
        [XmlElement("indIEDest")]
        public IndicadorIEDestinatario IndIEDest { get; set; }

        /// <summary>
        /// Inscrição Estadual (obrigatório nas operações com contribuintes do ICMS)
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Inscrição na SUFRAMA (Obrigatório nas operações com as áreas com benefícios de incentivos fiscais sob controle da SUFRAMA)
        /// </summary>
        [XmlElement("ISUF")]
        public string ISUF { get; set; }

        /// <summary>
        /// Inscrição Municipal do tomador do serviço
        /// </summary>
        [XmlElement("IM")]
        public string IM { get; set; }

        /// <summary>
        /// Informar o e-mail do destinatário
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeIdEstrangeiro() => !string.IsNullOrWhiteSpace(IdEstrangeiro) || (string.IsNullOrWhiteSpace(CNPJ) && string.IsNullOrWhiteSpace(CPF) && !string.IsNullOrWhiteSpace(XNome));

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeISUF() => !string.IsNullOrWhiteSpace(ISUF);

        public bool ShouldSerializeIM() => !string.IsNullOrWhiteSpace(IM);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Classe 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.EnderDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class EnderDest
    {
        private string XLgrField;
        private string NroField;
        private string XCplField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Número
        /// </summary>
        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set => XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region cMun

        private int _CMun;

        /// <summary>
        /// Código do município
        /// </summary>
        [XmlIgnore]
        public int CMun
        {
            get => _CMun;
            set
            {
                if (value <= 0)
                {
                    throw new Exception("Código do município do destinatário (tag <cMun> da <enderDest>) está sem conteúdo. É obrigatório informar o código IBGE do município.");
                }

                _CMun = value;
            }
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cMun")]
        public string CMunField
        {
            get => CMun.ToString();
            set => CMun = Convert.ToInt32(string.IsNullOrWhiteSpace(value) ? "0" : value);
        }

        #endregion

        /// <summary>
        /// Nome do município
        /// </summary>
        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Sigla da UF
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Código do país
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país
        /// </summary>
        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Telefone com Código DDD + número
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeCPais() => CPais > 0;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais);

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        #endregion
    }

    /// <summary>
    /// Classe para definir informações base de local
    /// </summary>
    public abstract class LocalBase
    {
        private string XNomeField;
        private string XLgrField;
        private string NroField;
        private string XCplField;
        private string XBairroField;
        private string XMunField;
        private string XPaisField = "BRASIL";

        /// <summary>
        /// CNPJ
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Razão Social ou Nome do Expedidor/Recebedor
        /// </summary>
        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Logradouro
        /// </summary>
        [XmlElement("xLgr")]
        public string XLgr
        {
            get => XLgrField;
            set => XLgrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Número
        /// </summary>
        [XmlElement("nro")]
        public string Nro
        {
            get => NroField;
            set => NroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Complemento
        /// </summary>
        [XmlElement("xCpl")]
        public string XCpl
        {
            get => XCplField;
            set => XCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Bairro
        /// </summary>
        [XmlElement("xBairro")]
        public string XBairro
        {
            get => XBairroField;
            set => XBairroField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region cMun

        private int _CMun;

        /// <summary>
        /// Código IBGE do município
        /// </summary>
        [XmlIgnore]
        public int CMun
        {
            get => _CMun;
            set
            {
                if (value <= 0)
                {
                    throw new Exception("Código do município do local de " + GetType().Name.ToLower() + " (tag <cMun> da <" + GetType().Name.ToLower() + ">) está sem conteúdo. É obrigatório informar o código IBGE do município.");
                }

                _CMun = value;
            }
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CMun para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cMun")]
        public string CMunField
        {
            get => CMun.ToString();
            set => CMun = Convert.ToInt32(string.IsNullOrWhiteSpace(value) ? "0" : value);
        }

        #endregion

        /// <summary>
        /// Nome do município
        /// </summary>
        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Sigla da UF
        /// </summary>
        [XmlElement("UF")]
        public UFBrasil UF { get; set; }

        /// <summary>
        /// CEP
        /// </summary>
        [XmlElement("CEP")]
        public string CEP { get; set; }

        /// <summary>
        /// Código do país
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Nome do país
        /// </summary>
        [XmlElement("xPais")]
        public string XPais
        {
            get => XPaisField;
            set => XPaisField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Telefone com Código DDD + número
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Informar o e-mail do expedidor/Recebedor
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        /// <summary>
        /// Inscrição Estadual
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXCpl() => !string.IsNullOrWhiteSpace(XCpl);

        public bool ShouldSerializeCNPJ() => string.IsNullOrWhiteSpace(CPF); //Se não tiver o CPF tenho que colocar a TAG de CNPJ em branco ou gera erro de schema

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeCEP() => !string.IsNullOrWhiteSpace(CEP);

        public bool ShouldSerializeCPais() => CPais > 0 && CPais != 1058;

        public bool ShouldSerializeXPais() => !string.IsNullOrWhiteSpace(XPais) && XPais.ToUpper() != "BRASIL";

        public bool ShouldSerializeFone() => !string.IsNullOrWhiteSpace(Fone);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeEmail() => !string.IsNullOrWhiteSpace(Email);

        #endregion
    }

    /// <summary>
    /// Classe com informações do local de retirada
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Retirada")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Retirada : LocalBase { }

    /// <summary>
    /// Classe com informações do local de entrega
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Entrega")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Entrega : LocalBase { }

    /// <summary>
    /// Classe com a informação de Pessoas autorizadas para o download do XML da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.AutXML")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class AutXML
    {
        /// <summary>
        /// CNPJ autorizado
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF autorizado
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        #endregion
    }

    /// <summary>
    /// Classe de dados dos detalhes da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Det")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Det
    {
        private string InfAdProdField;

        /// <summary>
        /// Número do item do NF
        /// </summary>
        [XmlAttribute(AttributeName = "nItem")]
        public int NItem { get; set; }

        /// <summary>
        /// Dados dos produtos e serviços da NFe/NFCe
        /// </summary>
        [XmlElement("prod")]
        public Prod Prod { get; set; }

        /// <summary>
        /// Tributos incidentes nos produtos ou serviços da NFe/NFCe
        /// </summary>
        [XmlElement("imposto")]
        public Imposto Imposto { get; set; }

        /// <summary>
        /// Grupo de devolução de tributos
        /// </summary>
        [XmlElement("impostoDevol")]
        public ImpostoDevol ImpostoDevol { get; set; }

        /// <summary>
        /// Informações adicionais do produto (norma referenciada, informações complementares, etc)
        /// </summary>
        [XmlElement("infAdProd")]
        public string InfAdProd
        {
            get => string.IsNullOrWhiteSpace(InfAdProdField) ? null : InfAdProdField;
            set => InfAdProdField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(500).Trim());
        }

        /// <summary>
        /// Grupo de observações de uso livre (para o item da NFe/NFCe
        /// </summary>
        [XmlElement("obsItem")]
        public ObsItem ObsItem { get; set; }

        /// <summary>
        /// Valor total do Item, correspondente à sua participação no total da nota. A soma dos itens deverá corresponder ao total da nota.
        /// </summary>
        [XmlIgnore]
        public double VItem { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vItem para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vItem")]
        public string VItemField
        {
            get => VItem.ToString("F2", CultureInfo.InvariantCulture);
            set => VItem = Converter.ToDouble(value);
        }

        /// <summary>
        /// Documento Fiscal Eletrônico Referenciado 
        /// </summary>
        [XmlElement("DFeReferenciado")]
        public DFeReferenciado DFeReferenciado { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVItemField() => VItem > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do produto da NFe/NFCe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Prod")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Prod
    {
        private string XProdField;

        /// <summary>
        /// Código do produto ou serviço
        /// </summary>
        [XmlElement("cProd")]
        public string CProd { get; set; }

        private string CEANField = "";

        /// <summary>
        /// GTIN (Global Trade Item Number) do produto, antigo código EAN ou código de barras
        /// </summary>
        [XmlElement("cEAN")]
        public string CEAN
        {
            get => CEANField;
            set => CEANField = value;
        }

        /// <summary>
        /// Codigo de barras diferente do padrão GTIN
        /// </summary>
        [XmlElement("cBarra")]
        public string CBarra { get; set; } = "";

        /// <summary>
        /// Descrição do produto ou serviço
        /// </summary>
        [XmlElement("xProd")]
        public string XProd
        {
            get => XProdField;
            set => XProdField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(120).Trim());
        }

        /// <summary>
        /// Código NCM (8 posições)
        /// </summary>
        [XmlElement("NCM")]
        public string NCM { get; set; }

        /// <summary>
        /// Nomenclatura de Valor aduaneio e Estatístico
        /// </summary>
        [XmlElement("NVE")]
        public List<string> NVE { get; set; }

        /// <summary>
        /// Codigo especificador da Substuicao Tributaria - CEST
        /// </summary>
        [XmlElement("CEST")]
        public string CEST { get; set; }

        /// <summary>
        /// Indicador de escala relevante
        /// </summary>
        [XmlElement("indEscala")]
#if INTEROP
        public IndicadorEscalaRelevante IndEscala { get; set; } = (IndicadorEscalaRelevante)(-1);
#else
        public IndicadorEscalaRelevante? IndEscala { get; set; }
#endif

        /// <summary>
        /// CNPJ do Fabricante da Mercadoria, obrigatório para produto em escala NÃO relevante
        /// </summary>
        [XmlElement("CNPJFab")]
        public string CNPJFab { get; set; }

        /// <summary>
        /// Código de benefício fiscal
        /// </summary>
        [XmlElement("cBenef")]
        public string CBenef { get; set; }

        /// <summary>
        /// Grupo opcional para informações do Crédito Presumido. Obs.: A exigência do preenchimento das informações do crédito presumido fica a critério de cada UF.
        /// </summary>
        [XmlElement("gCred")]
        public List<GCred> GCred { get; set; }

        /// <summary>
        /// Código EX TIPI (3 posições)
        /// </summary>
        [XmlElement("EXTIPI")]
        public string EXTIPI { get; set; }

        /// <summary>
        /// CFOP
        /// </summary>
        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        /// <summary>
        /// Unidade comercial
        /// </summary>
        [XmlElement("uCom")]
        public string UCom { get; set; }

        /// <summary>
        /// Quantidade Comercial  do produto, alterado para aceitar de 0 a 4 casas decimais e 11 inteiros
        /// </summary>
        [XmlElement("qCom")]
        public decimal QCom { get; set; }

        /// <summary>
        /// Valor unitário de comercialização  - alterado para aceitar 0 a 10 casas decimais e 11 inteiros
        /// </summary>
        [XmlElement("vUnCom")]
        public decimal VUnCom { get; set; }

        /// <summary>
        /// Valor bruto do produto ou serviço
        /// </summary>
        [XmlIgnore]
        public double VProd { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VProd para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }

        private string CEANTribField = "";

        /// <summary>
        /// GTIN (Global Trade Item Number) da unidade tributável, antigo código EAN ou código de barras
        /// </summary>
        [XmlElement("cEANTrib")]
        public string CEANTrib
        {
            get => CEANTribField;
            set => CEANTribField = value;
        }

        /// <summary>
        /// Código de barras da unidade tributável diferente do padrão GTIN
        /// </summary>
        [XmlElement("cBarraTrib")]
        public string CBarraTrib { get; set; } = "";

        /// <summary>
        /// Unidade Tributável
        /// </summary>
        [XmlElement("uTrib")]
        public string UTrib { get; set; }

        /// <summary>
        /// Quantidade Tributável - alterado para aceitar de 0 a 4 casas decimais e 11 inteiros
        /// </summary>
        [XmlElement("qTrib")]
        public decimal QTrib { get; set; }

        /// <summary>
        /// Valor unitário de tributação - - alterado para aceitar 0 a 10 casas decimais e 11 inteiros
        /// </summary>
        [XmlElement("vUnTrib")]
        public decimal VUnTrib { get; set; }

        /// <summary>
        /// Valor Total do Frete
        /// </summary>
        [XmlIgnore]
        public double VFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFrete para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do seguro
        /// </summary>
        [XmlIgnore]
        public double VSeg { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VSeg para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vSeg")]
        public string VSegField
        {
            get => VSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VSeg = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do desconto
        /// </summary>
        [XmlIgnore]
        public double VDesc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDesc para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Outras despesas acessórias
        /// </summary>
        [XmlIgnore]
        public double VOutro { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VOutro para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        /// <summary>
        /// Este campo deverá ser preenchido:
        /// 0 – o valor do item (vProd) não compõe o valor total da NF-e (vProd);
        /// 1  – o valor do item(vProd) compõe o valor total da NF-e(vProd)
        /// </summary>
        [XmlElement("indTot")]
        public SimNao IndTot { get; set; }

        /// <summary>
        /// Declaração de Importação
        /// </summary>
        [XmlElement("DI")]
        public List<DI> DI { get; set; }

        /// <summary>
        /// Detalhe da exportação
        /// </summary>
        [XmlElement("detExport")]
        public List<DetExport> DetExport { get; set; }

        /// <summary>
        /// Pedido de compra - Informação de interesse do emissor para controle do B2B
        /// </summary>
        [XmlElement("xPed")]
        public string XPed { get; set; }

        /// <summary>
        /// Número do Item do Pedido de Compra - Identificação do número do item do pedido de Compra
        /// </summary>
        [XmlElement("nItemPed")]
        public string NItemPed { get; set; }

        /// <summary>
        /// Número de controle da FCI - Ficha de Conteúdo de Importação
        /// </summary>
        [XmlElement("nFCI")]
        public string NFCI { get; set; }

        /// <summary>
        /// Grupo de informações de rastreabilidade
        /// </summary>
        [XmlElement("rastro")]
        public List<Rastro> Rastro { get; set; }

        /// <summary>
        /// Informações mais detalhadas do produto
        /// </summary>
        [XmlElement("infProdNFF")]
        public InfProdNFF InfProdNFF { get; set; }

        /// <summary>
        /// Informações mais detalhadas do produto
        /// </summary>
        [XmlElement("infProdEmb")]
        public InfProdEmb InfProdEmb { get; set; }

        /// <summary>
        /// Veículos novos
        /// </summary>
        [XmlElement("veicProd")]
        public VeicProd VeicProd { get; set; }

        /// <summary>
        /// Grupo do detalhamento de Medicamentos e de matérias-primas farmacêuticas
        /// </summary>
        [XmlElement("med")]
        public Med Med { get; set; }

        /// <summary>
        /// Armamentos
        /// </summary>
        [XmlElement("arma")]
        public List<Arma> Arma { get; set; }

        /// <summary>
        /// Informar apenas para operações com combustíveis líquidos
        /// </summary>
        [XmlElement("comb")]
        public List<Comb> Comb { get; set; }

        /// <summary>
        /// Número do RECOPI
        /// </summary>
        [XmlElement("nRECOPI")]
        public string NRECOPI { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNVE() => NVE != null;

        public bool ShouldSerializeCEST() => !string.IsNullOrWhiteSpace(CEST);

        public bool ShouldSerializeCNPJFab() => !string.IsNullOrWhiteSpace(CNPJFab);

        public bool ShouldSerializeCBenef() => !string.IsNullOrWhiteSpace(CBenef);

        public bool ShouldSerializeEXTIPI() => !string.IsNullOrWhiteSpace(EXTIPI);

        public bool ShouldSerializeVFreteField() => VFrete > 0;

        public bool ShouldSerializeVSegField() => VSeg > 0;

        public bool ShouldSerializeVDescField() => VDesc > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeXPed() => !string.IsNullOrWhiteSpace(XPed);

        public bool ShouldSerializeNItemPed() => NItemPed != null;

        public bool ShouldSerializeNFCI() => !string.IsNullOrWhiteSpace(NFCI);

        public bool ShouldSerializeIndEscala() => IndEscala != null && IndEscala != (IndicadorEscalaRelevante)(-1);

        public bool ShouldSerializeNRECOPI() => !string.IsNullOrWhiteSpace(NRECOPI);

        public bool ShouldSerializeCBarra() => !string.IsNullOrWhiteSpace(CBarra);

        public bool ShouldSerializeCBarraTrib() => !string.IsNullOrWhiteSpace(CBarraTrib);

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="nVE">Elemento</param>
        public void AddNVE(string nVE)
        {
            if (NVE == null)
            {
                NVE = new List<string>();
            }

            NVE.Add(nVE);
        }

        /// <summary>
        /// Retorna o elemento da lista NVE (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da NVE</returns>
        public string GetNVE(int index)
        {
            if ((NVE?.Count ?? 0) == 0)
            {
                return default;
            };

            return NVE[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista NVE
        /// </summary>
        public int GetNVECount => (NVE != null ? NVE.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="di">Elemento</param>
        public void AddDI(DI di)
        {
            if (DI == null)
            {
                DI = new List<DI>();
            }

            DI.Add(di);
        }

        /// <summary>
        /// Retorna o elemento da lista DI (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DI</returns>
        public DI GetDI(int index)
        {
            if ((DI?.Count ?? 0) == 0)
            {
                return default;
            };

            return DI[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DI
        /// </summary>
        public int GetDICount => (DI != null ? DI.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="detExport">Elemento</param>
        public void AddDetExport(DetExport detExport)
        {
            if (DetExport == null)
            {
                DetExport = new List<DetExport>();
            }

            DetExport.Add(detExport);
        }

        /// <summary>
        /// Retorna o elemento da lista DetExport (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetExport</returns>
        public DetExport GetDetExport(int index)
        {
            if ((DetExport?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetExport[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetExport
        /// </summary>
        public int GetDetExportCount => (DetExport != null ? DetExport.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="rastro">Elemento</param>
        public void AddRastro(Rastro rastro)
        {
            if (Rastro == null)
            {
                Rastro = new List<Rastro>();
            }

            Rastro.Add(rastro);
        }

        /// <summary>
        /// Retorna o elemento da lista Rastro (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Rastro</returns>
        public Rastro GetRastro(int index)
        {
            if ((Rastro?.Count ?? 0) == 0)
            {
                return default;
            };

            return Rastro[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Rastro
        /// </summary>
        public int GetRastroCount => (Rastro != null ? Rastro.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="arma">Elemento</param>
        public void AddArma(Arma arma)
        {
            if (Arma == null)
            {
                Arma = new List<Arma>();
            }

            Arma.Add(arma);
        }

        /// <summary>
        /// Retorna o elemento da lista Arma (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Arma</returns>
        public Arma GetArma(int index)
        {
            if ((Arma?.Count ?? 0) == 0)
            {
                return default;
            };

            return Arma[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Arma
        /// </summary>
        public int GetArmaCount => (Arma != null ? Arma.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="comb">Elemento</param>
        public void AddComb(Comb comb)
        {
            if (Comb == null)
            {
                Comb = new List<Comb>();
            }

            Comb.Add(comb);
        }

        /// <summary>
        /// Retorna o elemento da lista Comb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Comb</returns>
        public Comb GetComb(int index)
        {
            if ((Comb?.Count ?? 0) == 0)
            {
                return default;
            };

            return Comb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Comb
        /// </summary>
        public int GetCombCount => (Comb != null ? Comb.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="elemento">Elemento</param>
        public void AddGCred(GCred elemento)
        {
            if (GCred == null)
            {
                GCred = new List<GCred>();
            }

            GCred.Add(elemento);
        }

        /// <summary>
        /// Retorna o elemento da lista GCred (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da GCred</returns>
        public GCred GetGCred(int index)
        {
            if ((GCred?.Count ?? 0) == 0)
            {
                return default;
            };

            return GCred[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista GCred
        /// </summary>
        public int GetGCredCount => (GCred != null ? GCred.Count : 0);

#endif
    }

    /// <summary>
    /// Classe do grupo de informações sobre o Crédito Presumido
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCred")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCred
    {
        /// <summary>
        /// Código de Benefício Fiscal de Crédito Presumido utilizado pela UF, aplicado ao item. Obs.: Deve ser utilizado o mesmo código adotado na EFD e outras declarações, nas UF que o exigem.
        /// </summary>
        [XmlElement("cCredPresumido")]
        public string CCredPresumido { get; set; }

        /// <summary>
        /// Informar o percentual do crédito presumido relativo ao código do crédito presumido informado.
        /// </summary>
        [XmlIgnore]
        public double PCredPresumido { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredPresumido para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredPresumido")]
        public string PCredPresumidoField
        {
            get => PCredPresumido.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredPresumido = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o valor do crédito presumido relativo ao código do crédito presumido informado.
        /// </summary>
        [XmlIgnore]
        public double VCredPresumido { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredPresumido para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredPresumido")]
        public string VCredPresumidoField
        {
            get => VCredPresumido.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredPresumido = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DI")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DI
    {
        /// <summary>
        /// Número do documento de importação
        /// </summary>
        [XmlElement("nDI")]
        public string NDI { get; set; }

        /// <summary>
        /// Data de registro da DI/DSI/DA (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DDI { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DDI para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dDI")]
        public string DDIField
        {
            get => DDI.ToString("yyyy-MM-dd");
            set => DDI = DateTime.Parse(value);
        }

        /// <summary>
        /// Local do desembaraço aduaneiro
        /// </summary>
        [XmlElement("xLocDesemb")]
        public string XLocDesemb { get; set; }

        /// <summary>
        /// UF onde ocorreu o desembaraço aduaneiro
        /// </summary>
        [XmlElement("UFDesemb")]
        public UFBrasil UFDesemb { get; set; }

        /// <summary>
        /// Data do desembaraço aduaneiro (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DDesemb { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DDesemb para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dDesemb")]
        public string DDesembField
        {
            get => DDesemb.ToString("yyyy-MM-dd");
            set => DDesemb = DateTime.Parse(value);
        }

        /// <summary>
        /// Via de transporte internacional informada na DI ou na Declaração Única de Importação
        /// </summary>
        [XmlElement("tpViaTransp")]
        public ViaTransporteInternacional TpViaTransp { get; set; }

        /// <summary>
        /// Valor Adicional ao frete para renovação de marinha mercante
        /// </summary>
        [XmlIgnore]
        public double VAFRMM { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VAFRMM para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAFRMM")]
        public string VAFRMMField
        {
            get => VAFRMM.ToString("F2", CultureInfo.InvariantCulture);
            set => VAFRMM = Converter.ToDouble(value);
        }

        /// <summary>
        /// Forma de Importação quanto a intermediação
        /// </summary>
        [XmlElement("tpIntermedio")]
        public FormaImportacaoIntermediacao TpIntermedio { get; set; }

        /// <summary>
        /// CNPJ do adquirente ou do encomendante
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do adquirente ou do encomendante
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// Sigla da UF do adquirente ou do encomendante
        /// </summary>
#if INTEROP
        [XmlElement("UFTerceiro")]
        public UFBrasil UFTerceiro { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UFTerceiro")]
        public UFBrasil? UFTerceiro { get; set; }
#endif

        /// <summary>
        /// Código do exportador (usado nos sistemas internos de informação do emitente da NFe/NFCe)
        /// </summary>
        [XmlElement("cExportador")]
        public string CExportador { get; set; }

        /// <summary>
        /// Adições (NT 2011/004)
        /// </summary>
        [XmlElement("adi")]
        public List<Adi> Adi { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVAFRMM() => VAFRMM > 0;

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);
        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeUFTerceiro() => UFTerceiro != null && UFTerceiro != UFBrasil.NaoDefinido;


        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="adi">Elemento</param>
        public void AddAdi(Adi adi)
        {
            if (Adi == null)
            {
                Adi = new List<Adi>();
            }

            Adi.Add(adi);
        }

        /// <summary>
        /// Retorna o elemento da lista Adi (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Adi</returns>
        public Adi GetAdi(int index)
        {
            if ((Adi?.Count ?? 0) == 0)
            {
                return default;
            };

            return Adi[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Adi
        /// </summary>
        public int GetAdiCount => (Adi != null ? Adi.Count : 0);

#endif
    }

    /// <summary>
    /// Classe das adições
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Adi")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Adi
    {
        /// <summary>
        /// Número da adição
        /// </summary>
        [XmlElement("nAdicao")]
        public int NAdicao { get; set; }

        /// <summary>
        /// Número sequencial do item
        /// </summary>
        [XmlElement("nSeqAdic")]
        public int NSeqAdic { get; set; }

        /// <summary>
        /// Código do fabricante estrangeiro (usado nos sistemas internos de informação do emitente da NFe/NFCe)
        /// </summary>
        [XmlElement("cFabricante")]
        public string CFabricante { get; set; }

        /// <summary>
        /// Valor do desconto do item
        /// </summary>
        [XmlIgnore]
        public double VDescDI { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDescDI para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDescDI")]
        public string VDescDIField
        {
            get => VDescDI.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescDI = Converter.ToDouble(value);
        }

        /// <summary>
        /// Número do ato concessório de Drawback
        /// </summary>
        [XmlElement("nDraw")]
        public string NDraw { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNDraw() => !string.IsNullOrWhiteSpace(NDraw);

        public bool ShouldSerializeVDescDIField() => VDescDI > 0;

        #endregion
    }

    /// <summary>
    /// Classe de detalhamento da exportação
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DetExport")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DetExport
    {
        /// <summary>
        /// Número do ato concessório de Drawback
        /// </summary>
        [XmlElement("nDraw")]
        public string NDraw { get; set; }

        /// <summary>
        /// Exportação indireta
        /// </summary>
        [XmlElement("exportInd")]
        public ExportInd ExportInd { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNDraw() => !string.IsNullOrWhiteSpace(NDraw);

        #endregion
    }

    /// <summary>
    /// Classe de informações da exportação indireta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ExportInd")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ExportInd
    {
        /// <summary>
        /// Registro de exportação
        /// </summary>
        [XmlElement("nRE")]
        public string NRE { get; set; }

        /// <summary>
        /// Chave de acesso da NF-e recebida para exportação
        /// </summary>
        [XmlElement("chNFe")]
        public string ChNFe { get; set; }

        /// <summary>
        /// Quantidade do item efetivamente exportado
        /// </summary>
        [XmlIgnore]
        public double QExport { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QExport para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qExport")]
        public string QExportField
        {
            get => QExport.ToString("F4", CultureInfo.InvariantCulture);
            set => QExport = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações da rastreabilidade
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Rastro")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Rastro
    {
        /// <summary>
        /// Número do lote do produto
        /// </summary>
        [XmlElement("nLote")]
        public string NLote { get; set; }

        /// <summary>
        /// Quantidade de produto no lote
        /// </summary>
        [XmlIgnore]
        public double QLote { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QLote para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qLote")]
        public string QLoteField
        {
            get => QLote.ToString("F3", CultureInfo.InvariantCulture);
            set => QLote = Converter.ToDouble(value);
        }

        /// <summary>
        /// Data de fabricação/produção. Formato AAAA-MM-DD
        /// </summary>
        [XmlIgnore]
        public DateTime DFab { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DFab para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dFab")]
        public string DFabField
        {
            get => DFab.ToString("yyyy-MM-dd");
            set => DFab = DateTime.Parse(value);
        }

        /// <summary>
        /// Data de validade. Informar o último dia do mês caso a validade não especifique o dia.
        /// Formato AAAA-MM-DD
        /// </summary>
        [XmlIgnore]
        public DateTime DVal { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DVal para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dVal")]
        public string DValField
        {
            get => DVal.ToString("yyyy-MM-dd");
            set => DVal = DateTime.Parse(value);
        }

        /// <summary>
        /// Código de agregação
        /// </summary>
        [XmlElement("cAgreg")]
        public string CAgreg { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCAgreg() => !string.IsNullOrWhiteSpace(CAgreg);

        #endregion
    }

    /// <summary>
    /// Classe de informações mais detalhadas do produto (usada na NFF)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProdNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfProdNFF
    {
        /// <summary>
        /// Código Fiscal do Produto
        /// </summary>
        [XmlElement("cProdFisco")]
        public string CProdFisco { get; set; }

        /// <summary>
        /// Código da operação selecionada na NFF e relacionada ao item
        /// </summary>
        [XmlElement("cOperNFF")]
        public int COperNFF { get; set; }

        [XmlElement("xEmb")]
        public string XEmb { get; set; }

        [XmlIgnore]
        public double QVolEmb { get; set; }

        [XmlElement("qVolEmb")]
        public string QVolEmbField
        {
            get => QVolEmb.ToString("F2", CultureInfo.InvariantCulture);
            set => QVolEmb = Converter.ToDouble(value);
        }

        [XmlElement("uEmb")]
        public string UEmb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXEmb() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeQVolEmbField() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeUEmb() => !string.IsNullOrWhiteSpace(XEmb);

        #endregion
    }

    /// <summary>
    /// Classe de informações mais detalhadas do produto (usada na NFF)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfProdEmb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfProdEmb
    {
        /// <summary>
        /// Embalagem do produto
        /// </summary>
        [XmlElement("xEmb")]
        public string XEmb { get; set; }

        /// <summary>
        /// Volume do produto na embalagem
        /// </summary>
        [XmlIgnore]
        public double QVolEmb { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QVolEmb para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qVolEmb")]
        public string QVolEmbField
        {
            get => QVolEmb.ToString("F3", CultureInfo.InvariantCulture);
            set => QVolEmb = Converter.ToDouble(value);
        }

        /// <summary>
        /// Unidade de Medida da Embalagem
        /// </summary>
        [XmlElement("uEmb")]
        public string UEmb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXEmb() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeQVolEmbField() => !string.IsNullOrWhiteSpace(XEmb);
        public bool ShouldSerializeUEmb() => !string.IsNullOrWhiteSpace(XEmb);

        #endregion
    }

    /// <summary>
    /// Classe de informações de armamento
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Arma")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Arma
    {
        private string DescrField;

        /// <summary>
        /// Indicador de tipo de arma de fogo
        /// </summary>
        [XmlElement("tpArma")]
        public TipoArma TpArma { get; set; }

        /// <summary>
        /// Número de série da arma
        /// </summary>
        [XmlElement("nSerie")]
        public string NSerie { get; set; }

        /// <summary>
        /// Número de série do cano
        /// </summary>
        [XmlElement("nCano")]
        public string NCano { get; set; }

        /// <summary>
        /// Descrição completa da arma, compreendendo: calibre, marca, capacidade, tipo de funcionamento, comprimento e demais elementos que permitam a sua perfeita identificação
        /// </summary>
        [XmlElement("descr")]
        public string Descr
        {
            get => DescrField;
            set => DescrField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(256).Trim());
        }
    }

    /// <summary>
    /// Classe de informações de combustível
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Comb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Comb
    {
        /// <summary>
        /// Código de produto da ANP. codificação de produtos do SIMP
        /// </summary>
        [XmlElement("cProdANP")]
        public string CProdANP { get; set; }

        /// <summary>
        /// Descrição do Produto conforme ANP. Utilizar a descrição de produtos do Sistema de Informações de Movimentação de Produtos - SIMP
        /// </summary>
        [XmlElement("descANP")]
        public string DescANP { get; set; }

        /// <summary>
        /// Percentual do GLP derivado do petróleo no produto GLP (cProdANP=210203001). Informar em número decimal o percentual do GLP derivado de petróleo no produto GLP.
        /// Valores 0 a 100.
        /// </summary>
        [XmlIgnore]
        public double PGLP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PGLP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pGLP")]
        public string PGLPField
        {
            get => PGLP.ToString("F4", CultureInfo.InvariantCulture);
            set => PGLP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de gás natural nacional - GLGNn para o produto GLP (cProdANP=210203001). Informar em número decimal o percentual do Gás Natural Nacional - GLGNn para o produto GLP.
        /// Valores de 0 a 100
        /// </summary>
        [XmlIgnore]
        public double PGNn { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PGNn para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pGNn")]
        public string PGNnField
        {
            get => PGNn.ToString("F4", CultureInfo.InvariantCulture);
            set => PGNn = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de gás natural importado GLGNi para o produto GLP (cProdANP=210203001). Informar em número deciaml o percentual do Gás Natural Importado - GLGNi para o produto GLP.
        /// Valores de 0 a 100.
        /// </summary>
        [XmlIgnore]
        public double PGNi { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PGNi para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pGNi")]
        public string PGNiField
        {
            get => PGNi.ToString("F4", CultureInfo.InvariantCulture);
            set => PGNi = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor de partida (cProdANP=210203001). Deve ser informado neste campo o valor por quilograma sem ICMS.
        /// </summary>
        [XmlIgnore]
        public double VPart { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPart para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPart")]
        public string VPartField
        {
            get => VPart.ToString("F2", CultureInfo.InvariantCulture);
            set => VPart = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de autorização / registro do CODIF. Informar apenas quando a UF utilizar o CODIF (Sistema de Controle do 			Diferimento do Imposto nas Operações com AEAC - Álcool Etílico Anidro Combustível).
        /// </summary>
        [XmlElement("CODIF")]
        public string CODIF { get; set; }

        /// <summary>
        /// Quantidade de combustível faturada à temperatura ambiente.
        /// </summary>
        [XmlIgnore]
        public double QTemp { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QTemp para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qTemp")]
        public string QTempField
        {
            get => QTemp.ToString("F4", CultureInfo.InvariantCulture);
            set => QTemp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Sigla da UF de Consumo
        /// </summary>
        [XmlElement("UFCons")]
        public UFBrasil UFCons { get; set; }

        /// <summary>
        /// CIDE Combustíveis
        /// </summary>
        [XmlElement("CIDE")]
        public CIDE CIDE { get; set; }

        /// <summary>
        /// Informações do grupo de encerrante
        /// </summary>
        [XmlElement("encerrante")]
        public Encerrante Encerrante { get; set; }

        /// <summary>
        /// Percentual do índice de mistura do Biodiesel (B100) no Óleo Diesel B instituído pelo órgão regulamentador
        /// </summary>
        [XmlIgnore]
        public double PBio { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PBio para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pBio")]
        public string PBioField
        {
            get => PBio.ToString("F4", CultureInfo.InvariantCulture);
            set => PBio = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo indicador da origem do combustível
        /// </summary>
        [XmlElement("origComb")]
        public List<OrigComb> OrigComb { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePGLPField() => PGLP > 0;

        public bool ShouldSerializePGNnField() => PGNn > 0;

        public bool ShouldSerializePGNiField() => PGNi > 0;

        public bool ShouldSerializeVPartField() => VPart > 0;

        public bool ShouldSerializeCODIF() => !string.IsNullOrWhiteSpace(CODIF);

        public bool ShouldSerializeQTempField() => QTemp > 0;

        public bool ShouldSerializePBioField() => PBio > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="item">Elemento</param>
        public void AddOrigComb(OrigComb item)
        {
            if (OrigComb == null)
            {
                OrigComb = new List<OrigComb>();
            }

            OrigComb.Add(item);
        }

        /// <summary>
        /// Retorna o elemento da lista OrigComb (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da OrigComb</returns>
        public OrigComb GetOrigComb(int index)
        {
            if ((OrigComb?.Count ?? 0) == 0)
            {
                return default;
            };

            return OrigComb[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista OrigComb
        /// </summary>
        public int GetOrigCombCount => (OrigComb != null ? OrigComb.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de CIDE Combustíveis
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.CIDE")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class CIDE
    {
        /// <summary>
        /// BC do CIDE (Quantidade comercializada)
        /// </summary>
        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        /// <summary>
        /// Alíquota do CIDE  (em reais)
        /// </summary>
        [XmlIgnore]
        public double VAliqProd { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VAliqProd para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAliqProd")]
        public string VAliqProdField
        {
            get => VAliqProd.ToString("F4", CultureInfo.InvariantCulture);
            set => VAliqProd = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do CIDE
        /// </summary>
        [XmlIgnore]
        public double VCIDE { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICDE para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCIDE")]
        public string VCIDEField
        {
            get => VCIDE.ToString("F2", CultureInfo.InvariantCulture);
            set => VCIDE = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do encerrante
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Encerrante")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Encerrante
    {
        /// <summary>
        /// Numero de identificação do Bico utilizado no abastecimento
        /// </summary>
        [XmlElement("nBico")]
        public int NBico { get; set; }

        /// <summary>
        /// Numero de identificação da bomba ao qual o bico está interligado
        /// </summary>
        [XmlElement("nBomba")]
        public int NBomba { get; set; }

        /// <summary>
        /// Numero de identificação do tanque ao qual o bico está interligado
        /// </summary>
        [XmlElement("nTanque")]
        public int NTanque { get; set; }

        /// <summary>
        /// Valor do Encerrante no ínicio do abastecimento
        /// </summary>
        [XmlIgnore]
        public double VEncIni { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VEncIni para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vEncIni")]
        public string VEncIniField
        {
            get => VEncIni.ToString("F3", CultureInfo.InvariantCulture);
            set => VEncIni = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Encerrante no final do abastecimento
        /// </summary>
        [XmlIgnore]
        public double VEncFin { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VEncFin para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vEncFin")]
        public string VEncFinField
        {
            get => VEncFin.ToString("F3", CultureInfo.InvariantCulture);
            set => VEncFin = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeNBomba() => NBomba > 0;

        #endregion
    }

    /// <summary>
    /// Classe do indicador da origem do combustível
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.OrigComb")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class OrigComb
    {
        /// <summary>
        /// Indicador de importação
        /// </summary>
        [XmlElement("indImport")]
        public IndicadorImportacao IndImport { get; set; }

        /// <summary>
        /// UF de origem do produtor ou do importado
        /// </summary>
        [XmlIgnore]
        public UFBrasil CUFOrig { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade CUFOrig para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("cUFOrig")]
        public int CUFOrigField
        {
            get => (int)CUFOrig;
            set => CUFOrig = (UFBrasil)Enum.Parse(typeof(UFBrasil), value.ToString());
        }

        /// <summary>
        /// Percentual do índice de mistura do Biodiesel (B100) no Óleo Diesel B instituído pelo órgão regulamentador
        /// </summary>
        [XmlIgnore]
        public double POrig { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade POrig para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pOrig")]
        public string POrigField
        {
            get => POrig.ToString("F4", CultureInfo.InvariantCulture);
            set => POrig = Converter.ToDouble(value);
        }

    }

    /// <summary>
    /// Classe de detalhamento de medicamentos e de matérias-primas farmacêuticas
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Med")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Med
    {
        private string XMotivoIsencaoField;

        /// <summary>
        /// Utilizar o número do registro ANVISA  ou preencher com o literal “ISENTO”, no caso de medicamento isento de registro na ANVISA
        /// </summary>
        [XmlElement("cProdANVISA")]
        public string CProdANVISA { get; set; }

        /// <summary>
        /// Para medicamento isento de registro na ANVISA, informar o número da decisão que o isenta, como por exemplo o número da Resolução da Diretoria Colegiada da ANVISA (RDC).
        /// </summary>
        [XmlElement("xMotivoIsencao")]
        public string XMotivoIsencao
        {
            get => XMotivoIsencaoField;
            set => XMotivoIsencaoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(255).Trim());
        }

        /// <summary>
        /// Preço Máximo ao Consumidor
        /// </summary>
        [XmlIgnore]
        public double VPMC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPMC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPMC")]
        public string VPMCField
        {
            get => VPMC.ToString("F2", CultureInfo.InvariantCulture);
            set => VPMC = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de detalhamento de veículos novos
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.VeicProd")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class VeicProd
    {
        /// <summary>
        /// Tipo da Operação: (1 - Venda concessionária; 2 - Faturamento direto; 3 - Venda direta; 0 - Outros)
        /// </summary>
        [XmlElement("tpOp")]
        public TipoOperacaoVeicNovo TpOp { get; set; }

        /// <summary>
        /// Chassi do veículo
        /// </summary>
        [XmlElement("chassi")]
        public string Chassi { get; set; }

        /// <summary>
        /// Cor do veículo (código de cada montadora)
        /// </summary>
        [XmlElement("cCor")]
        public string CCor { get; set; }

        /// <summary>
        /// Descrição da cor
        /// </summary>
        [XmlElement("xCor")]
        public string XCor { get; set; }

        /// <summary>
        /// Potência máxima do motor do veículo em cavalo vapor (CV). (potência-veículo)
        /// </summary>
        [XmlElement("pot")]
        public string Pot { get; set; }

        /// <summary>
        /// Capacidade voluntária do motor expressa em centímetros cúbicos (CC). (cilindradas)
        /// </summary>
        [XmlElement("cilin")]
        public string Cilin { get; set; }

        /// <summary>
        /// Peso líquido
        /// </summary>
        [XmlElement("pesoL")]
        public string PesoL { get; set; }

        /// <summary>
        /// Peso bruto
        /// </summary>
        [XmlElement("pesoB")]
        public string PesoB { get; set; }

        /// <summary>
        /// Serial (série)
        /// </summary>
        [XmlElement("nSerie")]
        public string NSerie { get; set; }

        /// <summary>
        /// Tipo de combustível-Tabela RENAVAM: 01-Álcool; 02-Gasolina; 03-Diesel; 16-Álcool/Gas.; 17-Gas./Álcool/GNV; 18-Gasolina/Elétrico
        /// </summary>
        [XmlElement("tpComb")]
        public string TpComb { get; set; }

        /// <summary>
        /// Número do motor
        /// </summary>
        [XmlElement("nMotor")]
        public string NMotor { get; set; }

        /// <summary>
        /// CMT-Capacidade Máxima de Tração - em Toneladas 4 casas decimais
        /// </summary>
        [XmlElement("CMT")]
        public string CMT { get; set; }

        /// <summary>
        /// Distância entre eixos
        /// </summary>
        [XmlElement("dist")]
        public string Dist { get; set; }

        /// <summary>
        /// Ano Modelo de Fabricação
        /// </summary>
        [XmlElement("anoMod")]
        public string AnoMod { get; set; }

        /// <summary>
        /// Ano de Fabricação
        /// </summary>
        [XmlElement("anoFab")]
        public string AnoFab { get; set; }

        /// <summary>
        /// Tipo de pintura
        /// </summary>
        [XmlElement("tpPint")]
        public string TpPint { get; set; }

        /// <summary>
        /// Tipo de veículo (utilizar tabela RENAVAM)
        /// </summary>
        [XmlElement("tpVeic")]
        public string TpVeic { get; set; }

        /// <summary>
        /// Espécie de veículo (utilizar tabela RENAVAM)
        /// </summary>
        [XmlElement("espVeic")]
        public string EspVeic { get; set; }

        /// <summary>
        /// Informa-se o veículo tem VIN (chassi) remarcado
        /// </summary>
        [XmlElement("VIN")]
        public CondicaoVIN VIN { get; set; }

        /// <summary>
        /// Condição do veículo (1 - acabado; 2 - inacabado; 3 - semi-acabado)
        /// </summary>
        [XmlElement("condVeic")]
        public CondicaoVeiculo CondVeic { get; set; }

        /// <summary>
        /// Código Marca Modelo (utilizar tabela RENAVAM)
        /// </summary>
        [XmlElement("cMod")]
        public int CMod { get; set; }

        /// <summary>
        /// Código da Cor Segundo as regras de pré-cadastro do DENATRAN:
        /// 01-AMARELO; 02-AZUL; 03-BEGE; 04-BRANCA; 05-CINZA; 06-DOURADA; 07-GRENA 
        /// 08-LARANJA; 09-MARROM; 10-PRATA; 11-PRETA; 12-ROSA; 13-ROXA; 14-VERDE; 15-VERMELHA; 16-FANTASIA
        /// </summary>
        [XmlElement("cCorDENATRAN")]
        public string CCorDENATRAN { get; set; }

        /// <summary>
        /// Quantidade máxima de permitida de passageiros sentados, inclusive motorista
        /// </summary>
        [XmlElement("lota")]
        public int Lota { get; set; }

        /// <summary>
        /// Restrição
        /// </summary>
        [XmlElement("tpRest")]
        public TipoRestricaoVeiculo TpRest { get; set; }
    }

    /// <summary>
    /// Classe de tributos incidentes nos produtos ou serviços da NF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Imposto")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Imposto
    {
        /// <summary>
        /// Valor estimado total de impostos federais, estaduais e municipais
        /// </summary>
        [XmlIgnore]
        public double VTotTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTotTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Converter.ToDouble(value);
        }

        /// <summary>
        /// Dados do ICMS Normal e ST
        /// </summary>
        [XmlElement("ICMS")]
        public ICMS ICMS { get; set; }

        /// <summary>
        /// Dados do IPI
        /// </summary>
        [XmlElement("IPI")]
        public IPI IPI { get; set; }

        /// <summary>
        /// Dados do Imposto de Importação
        /// </summary>
        [XmlElement("II")]
        public II II { get; set; }

        /// <summary>
        /// Dados do ISSQN
        /// </summary>
        [XmlElement("ISSQN")]
        public ISSQN ISSQN { get; set; }

        /// <summary>
        /// Dados do PIS
        /// </summary>
        [XmlElement("PIS")]
        public PIS PIS { get; set; }

        /// <summary>
        /// Dados do PISST
        /// </summary>
        [XmlElement("PISST")]
        public PISST PISST { get; set; }

        /// <summary>
        /// Dados do COFINS
        /// </summary>
        [XmlElement("COFINS")]
        public COFINS COFINS { get; set; }

        /// <summary>
        /// Dados do COFINSST
        /// </summary>
        [XmlElement("COFINSST")]
        public COFINSST COFINSST { get; set; }

        /// <summary>
        /// Dados do ICMSUFDest
        /// </summary>
        [XmlElement("ICMSUFDest")]
        public ICMSUFDest ICMSUFDest { get; set; }

        /// <summary>
        /// Informações do Imposto Seletivo
        /// </summary>
        [XmlElement("IS")]
        public IS IS { get; set; }

        /// <summary>
        /// Informações do Imposto de Bens e Serviços - IBS e da Contribuição de Bens e Serviços - CBS
        /// </summary>
        [XmlElement("IBSCBS")]
        public IBSCBS IBSCBS { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        #endregion
    }

    /// <summary>
    /// Classe para os dados do ICMS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS
    {
        /// <summary>
        /// Tributação pelo ICMS00 - Tributada integralmente
        /// </summary>
        [XmlElement("ICMS00")]
        public ICMS00 ICMS00 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS02 - Monofásica própria sobre combustíveis
        /// </summary>
        [XmlElement("ICMS02")]
        public ICMS02 ICMS02 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS10 - Tributada e com cobrança do ICMS por substituição tributária
        /// </summary>
        [XmlElement("ICMS10")]
        public ICMS10 ICMS10 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS15 - Monofásica própria e com responsabilidade pela retenção sobre combustíveis
        /// </summary>
        [XmlElement("ICMS15")]
        public ICMS15 ICMS15 { get; set; }

        /// <summary>
        /// Tributção pelo ICMS20 - Com redução de base de cálculo
        /// </summary>
        [XmlElement("ICMS20")]
        public ICMS20 ICMS20 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS30 - Isenta ou não tributada e com cobrança do ICMS por substituição tributária
        /// </summary>
        [XmlElement("ICMS30")]
        public ICMS30 ICMS30 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS40 - Isenta ou não tributada ou suspensa
        /// </summary>
        [XmlElement("ICMS40")]
        public ICMS40 ICMS40 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS51 - Diferimento. A exigência do preenchimento das informações do ICMS diferido fica à critério de cada UF
        /// </summary>
        [XmlElement("ICMS51")]
        public ICMS51 ICMS51 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS51 - Monofásica sobre combustíveis com recolhimento diferido
        /// </summary>
        [XmlElement("ICMS53")]
        public ICMS53 ICMS53 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS60 - ICMS cobrado anteriormente por substituição tributária
        /// </summary>
        [XmlElement("ICMS60")]
        public ICMS60 ICMS60 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS61 - Monofásica sobre combustíveis cobrada anteriormente
        /// </summary>
        [XmlElement("ICMS61")]
        public ICMS61 ICMS61 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS70 - Com redução de base de cálculo e cobrança do ICMS por substituição tributária
        /// </summary>
        [XmlElement("ICMS70")]
        public ICMS70 ICMS70 { get; set; }

        /// <summary>
        /// Tributação pelo ICMS90 - Outras
        /// </summary>
        [XmlElement("ICMS90")]
        public ICMS90 ICMS90 { get; set; }

        /// <summary>
        /// Partilha do ICMS entre a UF de origem e UF de destino ou a UF definida na legislação
        /// Operação interestadual para consumidor final com partilha do ICMS devido na operação entre a UF de origem e a 
        /// UF do destinatário ou ou a UF definida na legislação. (Ex.UF da concessionária de entrega do  veículos)
        /// </summary>
        [XmlElement("ICMSPart")]
        public ICMSPart ICMSPart { get; set; }

        /// <summary>
        /// Tributação do ICMSSN101 - Pelo SIMPLES NACIONAL e CSOSN=101
        /// </summary>
        [XmlElement("ICMSSN101")]
        public ICMSSN101 ICMSSN101 { get; set; }

        /// <summary>
        /// Tributação do ICMSSN102 - Pelo SIMPLES NACIONAL e CSOSN=102, 103, 300 ou 400
        /// </summary>
        [XmlElement("ICMSSN102")]
        public ICMSSN102 ICMSSN102 { get; set; }

        /// <summary>
        /// Tributação do ICMSSN201 - Pelo SIMPLES NACIONAL e CSOSN=201
        /// </summary>
        [XmlElement("ICMSSN201")]
        public ICMSSN201 ICMSSN201 { get; set; }

        /// <summary>
        /// Tributação do ICMSSN202 - Pelo SIMPLES NACIONAL e CSOSN=202 ou 203
        /// </summary>
        [XmlElement("ICMSSN202")]
        public ICMSSN202 ICMSSN202 { get; set; }

        /// <summary>
        /// Tributação do ICMSSN500 - Pelo SIMPLES NACIONAL,CRT=1 – Simples Nacional e CSOSN=500
        /// </summary>
        [XmlElement("ICMSSN500")]
        public ICMSSN500 ICMSSN500 { get; set; }

        /// <summary>
        /// Tributação do ICMSSN900 - Pelo SIMPLES NACIONAL, CRT=1 – Simples Nacional, CRT=4 - MEI e CSOSN=900
        /// </summary>
        [XmlElement("ICMSSN900")]
        public ICMSSN900 ICMSSN900 { get; set; }

        /// <summary>
        /// Grupo de informação do ICMSST devido para a UF de destino, nas operações interestaduais de produtos que tiveram 
        /// retenção antecipada de ICMS por ST na UF do remetente. Repasse via Substituto Tributário
        /// </summary>
        [XmlElement("ICMSST")]
        public ICMSST ICMSST { get; set; }
    }

    /// <summary>
    /// Classe de informações do ICMS00
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS00")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS00
    {
        /// <summary>
        /// Origem da mercadoro
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "00";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP).
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => PFCP > 0 || VFCP > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS02
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS02")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS02
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "02";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS próprio em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto. Alíquota ad rem do ICMS, estabelecida na legislação para o produto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio - O valor do ICMS é obtido pela multiplicação da alíquota ad rem pela quantidade do produto conforme unidade de medida estabelecida na legislação.
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS10
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS10")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS10
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "10";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Modalidade de determinação da BC do ICMSST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST { get; set; }

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP retido por substituicao tributaria
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS-ST desonerado
        /// </summary>
        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS-ST
        /// </summary>
        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPField() => VBCFCP > 0;

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => PFCP > 0;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS15
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS15")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS15
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "15";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS próprio em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade tributada sujeita a retenção - Informar a BC do ICMS sujeito a retenção em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMonoReten")]
        public decimal QBCMonoReten { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto com retenção
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMSReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMSReten")]
        public string AdRemICMSRetenField
        {
            get => AdRemICMSReten.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio com retenção
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoReten")]
        public string VICMSMonoRetenField
        {
            get => VICMSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução do valor da alíquota ad rem do ICMS
        /// </summary>
        [XmlIgnore]
        public double PRedAdRem { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedAdRem para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedAdRem")]
        public string PRedAdRemField
        {
            get => PRedAdRem.ToString("F2", CultureInfo.InvariantCulture);
            set => PRedAdRem = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da redução do adrem do ICMS. Só preencher se o pRedAdRem for maior que zero.
        /// </summary>
        [XmlElement("motRedAdRem")]
        public MotivoReducaoAdRem MotRedAdRem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;
        public bool ShouldSerializeQBCMonoReten() => QBCMonoReten > 0;
        public bool ShouldSerializePRedAdRemField() => PRedAdRem > 0;
        public bool ShouldSerializeMotRedAdRem() => PRedAdRem > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS20
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS20")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS20
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "20";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        /// <summary>
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de desoneração
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS
        /// </summary>
        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        /// <summary>
        /// Indica se o valor do ICMS desonerado (vICMSDeson) deduz do valor do item (vProd). 0=Não deduz   1=Sim, deduz.
        /// </summary>
        [XmlElement("indDeduzDeson")]
#if INTEROP
        public SimNao IndDeduzDeson { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDeduzDeson { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPField() => VBCFCP > 0;

        public bool ShouldSerializePFCPField() => PFCP > 0;

        public bool ShouldSerializeVFCPField() => VFCP > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

#if INTEROP
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != null;
#endif

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS30
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS30")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS30
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "30";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST { get; set; }

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de desoneração
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS
        /// </summary>
        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        /// <summary>
        /// Indica se o valor do ICMS desonerado (vICMSDeson) deduz do valor do item (vProd). 0=Não deduz   1=Sim, deduz.
        /// </summary>
        [XmlElement("indDeduzDeson")]
#if INTEROP
        public SimNao IndDeduzDeson { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDeduzDeson { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

#if INTEROP
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != null;
#endif

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS40
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS40")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS40
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// O valor do ICMS será informado apenas nas operações com veículos beneficiados com a desoneração condicional do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Este campo será preenchido quando o campo anterior estiver preenchido
        /// </summary>
        [XmlElement("motDesICMS")]
        public MotivoDesoneracaoICMS MotDesICMS { get; set; }

        /// <summary>
        /// Indica se o valor do ICMS desonerado (vICMSDeson) deduz do valor do item (vProd). 0=Não deduz   1=Sim, deduz.
        /// </summary>
        [XmlElement("indDeduzDeson")]
#if INTEROP
        public SimNao IndDeduzDeson { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDeduzDeson { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVICMSDesonField() => VICMSDeson > 0;

        public bool ShouldSerializeMotDesICMS() => VICMSDeson > 0;

#if INTEROP
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != (SimNao)(-1);
#else
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != null;
#endif


        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS51
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS51")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS51
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "51";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        /// <summary>
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double? PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código de Benefício Fiscal na UF aplicado ao item quando houver RBC
        /// </summary>
        [XmlElement("cBenefRBC")]
        public string CBenefRBC { get; set; }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double? VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do imposto
        /// </summary>
        [XmlIgnore]
        public double? PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da Operação
        /// </summary>
        [XmlIgnore]
        public double? VICMSOp { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSOp para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSOp")]
        public string VICMSOpField
        {
            get => VICMSOp?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSOp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do diferemento
        /// </summary>
        [XmlIgnore]
        public double? PDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDif")]
        public string PDifField
        {
            get => PDif?.ToString("F4", CultureInfo.InvariantCulture);
            set => PDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da diferido
        /// </summary>
        [XmlIgnore]
        public double? VICMSDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDif")]
        public string VICMSDifField
        {
            get => VICMSDif?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double? VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do diferimento do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCPDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPDif")]
        public string PFCPDifField
        {
            get => PFCPDif.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) diferido
        /// </summary>
        [XmlIgnore]
        public double VFCPDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPDif")]
        public string VFCPDifField
        {
            get => VFCPDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor efetivo do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCPEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPEfet")]
        public string VFCPEfetField
        {
            get => VFCPEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);
        public bool ShouldSerializePRedBCField() => PRedBC != null;
        public bool ShouldSerializeCBenefRBC() => !string.IsNullOrWhiteSpace(CBenefRBC) && PRedBC > 0;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializePICMSeField() => PICMS != null;
        public bool ShouldSerializeVICMSOpField() => VICMSOp != null;

        public bool ShouldSerializePDifField() => PDif != null;
        public bool ShouldSerializeVICMSDifField() => VICMSDif != null;
        public bool ShouldSerializeVICMSField() => VICMS != null;

        public bool ShouldSerializeVBCFCPField() => (VBCFCP + VFCP + PFCP) > 0;
        public bool ShouldSerializePFCPField() => (VBCFCP + VFCP + PFCP) > 0;
        public bool ShouldSerializeVFCPField() => (VBCFCP + VFCP + PFCP) > 0;

        public bool ShouldSerializePFCPDifField() => PFCPDif > 0;
        public bool ShouldSerializeVFCPDifField() => VFCPDif > 0;
        public bool ShouldSerializeVFCPEfetField() => VFCPEfet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS53
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS53")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS53
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "53";

        /// <summary>
        /// Quantidade tributada - Informar a BC do ICMS em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do ICMS estabelecida na legislação para o produto.
        /// </summary>
        [XmlIgnore]
        public double AdRemICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMS")]
        public string AdRemICMSField
        {
            get => AdRemICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS da operação - O valor do ICMS é obtido pela multiplicação da alíquota ad rem pela quantidade do produto conforme unidade de medida estabelecida em legislação, como se não houvesse o diferimento
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoOp { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoOp para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoOp")]
        public string VICMSMonoOpField
        {
            get => VICMSMonoOp.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoOp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do diferimento - No caso de diferimento total, informar o percentual de diferimento "100".
        /// </summary>
        [XmlIgnore]
        public double PDif { get; set; }

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
        /// Valor do ICMS próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoDif")]
        public string VICMSMonoDifField
        {
            get => VICMSMonoDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio devido - O valor do ICMS próprio devido é o resultado do valor do ICMS da operação menos valor do ICMS diferido.
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade tributada diferida - Informar a BC do ICMS diferido em quantidade conforme unidade de medida estabelecida na legislação para o produto.
        /// </summary>
        [XmlElement("qBCMonoDif")]
        public double QBCMonoDif { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto diferido
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMSDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMSDif")]
        public string AdRemICMSDifField
        {
            get => AdRemICMSDif.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSDif = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMono() => QBCMono > 0;
        public bool ShouldSerializeAdRemICMSField() => AdRemICMS > 0;
        public bool ShouldSerializeVICMSMonoOpField() => VICMSMonoOp > 0;
        public bool ShouldSerializePDifField() => PDif > 0;
        public bool ShouldSerializeVICMSMonoDifField() => VICMSMonoDif > 0;
        public bool ShouldSerializeVICMSMonoField() => VICMSMono > 0;
        public bool ShouldSerializeQBCMonoDif() => QBCMonoDif > 0;
        public bool ShouldSerializeAdRemICMSDifField() => AdRemICMSDif > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS60
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS60")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS60
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "60";

        /// <summary>
        /// Valor da BC do ICMS ST retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double? VBCSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Aliquota suportada pelo consumidor final
        /// </summary>
        [XmlIgnore]
        public double? PST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS Próprio do Substituto cobrado em operação anterior
        /// </summary>
        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSubstituto para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double? VICMSSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP retido anteriormente por ST
        /// </summary>
        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido anteriormente por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double VBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS efetiva
        /// </summary>
        [XmlIgnore]
        public double PICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS efetivo
        /// </summary>
        [XmlIgnore]
        public double VICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCSTRetField() => VBCSTRet != null;
        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;
        public bool ShouldSerializeVICMSSTRetField() => VICMSSTRet != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS61
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS61")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS61
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// CST - Código da situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "61";

        /// <summary>
        /// Quantidade tributada retida anteriormente - Informar a BC do ICMS em quantidade conforme unidade de medida estabelecida na legislação.
        /// </summary>
        [XmlElement("qBCMonoRet")]
        public decimal QBCMonoRet { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double AdRemICMSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemICMSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemICMSRet")]
        public string AdRemICMSRetField
        {
            get => AdRemICMSRet.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemICMSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoRet")]
        public string VICMSMonoRetField
        {
            get => VICMSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoRet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeQBCMonoRet() => QBCMonoRet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMS70
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS70")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS70
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "70";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        /// <summary>
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double? PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        private ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        private ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de desoneração
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS
        /// </summary>
        [XmlElement("motDesICMS")]
#if INTEROP
        public MotivoDesoneracaoICMS MotDesICMS { get; set; } = (MotivoDesoneracaoICMS)(-1);
#else
        public MotivoDesoneracaoICMS? MotDesICMS { get; set; }
#endif

        /// <summary>
        /// Indica se o valor do ICMS desonerado (vICMSDeson) deduz do valor do item (vProd). 0=Não deduz   1=Sim, deduz.
        /// </summary>
        [XmlElement("indDeduzDeson")]
#if INTEROP
        public SimNao IndDeduzDeson { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDeduzDeson { get; set; }
#endif

        /// <summary>
        /// Valor do ICMS-ST desonerado
        /// </summary>
        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS-ST
        /// </summary>
        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public virtual bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializePRedBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && PRedBC != null;

        public virtual bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVBCFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializeVFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePMVASTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PMVAST != null;

        public virtual bool ShouldSerializePRedBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PRedBCST != null;

        public virtual bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVBCFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializePFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVICMSDesonField() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1);

        public virtual bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeMotDesICMS() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;

#if INTEROP
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != (SimNao)(-1) && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;
#else
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != null && MotDesICMS != null && VICMSDeson > 0;
#endif

        public virtual bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public virtual bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Classe de informações do ICMS90
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMS90")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMS90
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "90";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCP")]
        public string VBCFCPField
        {
            get => VBCFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double PFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCP")]
        public string PFCPField
        {
            get => PFCP.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        private ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        private ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de desoneração
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS
        /// </summary>
        [XmlElement("motDesICMS")]
#if INTEROP
        public MotivoDesoneracaoICMS MotDesICMS { get; set; } = (MotivoDesoneracaoICMS)(-1);
#else
        public MotivoDesoneracaoICMS? MotDesICMS { get; set; }
#endif

        /// <summary>
        /// Indica se o valor do ICMS desonerado (vICMSDeson) deduz do valor do item (vProd). 0=Não deduz   1=Sim, deduz.
        /// </summary>
        [XmlElement("indDeduzDeson")]
#if INTEROP
        public SimNao IndDeduzDeson { get; set; } = (SimNao)(-1);
#else
        public SimNao? IndDeduzDeson { get; set; }
#endif

        /// <summary>
        /// Valor do ICMS-ST desonerado
        /// </summary>
        [XmlIgnore]
        public double VICMSSTDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTDeson")]
        public string VICMSSTDesonField
        {
            get => VICMSSTDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Motivo da desoneração do ICMS-ST
        /// </summary>
        [XmlElement("motDesICMSST")]
        public MotivoDesoneracaoICMS MotDesICMSST { get; set; }

        #region ShouldSerialize

        public virtual bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializePRedBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && PRedBC > 0;

        public virtual bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeVBCFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializeVFCPField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1) && (VBCFCP + PFCP + VFCP) > 0;

        public virtual bool ShouldSerializePMVASTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PMVAST != null;

        public virtual bool ShouldSerializePRedBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && PRedBCST != null;

        public virtual bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeVBCFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializePFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVFCPSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1) && (VBCFCPST + PFCPST + VFCPST) > 0;

        public virtual bool ShouldSerializeVICMSDesonField() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1);

        public virtual bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public virtual bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public virtual bool ShouldSerializeMotDesICMS() => MotDesICMS != null && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;

#if INTEROP
        public bool ShouldSerializeIndDeduzDeson() => IndDeduzDeson != (SimNao)(-1) && MotDesICMS != (MotivoDesoneracaoICMS)(-1) && VICMSDeson > 0;
#else
        public bool ShouldSerializeIndDeduzDeson() => MotDesICMS != null && IndDeduzDeson != null && VICMSDeson > 0;
#endif

        public virtual bool ShouldSerializeVICMSSTDesonField() => VICMSSTDeson > 0;

        public virtual bool ShouldSerializeMotDesICMSST() => VICMSSTDeson > 0;

        #endregion ShouldSerialize
    }

    /// <summary>
    /// Classe de informações do ICMSPart
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSPart")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSPart
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
        public ModalidadeBaseCalculoICMS ModBC { get; set; }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP retido por substituicao tributaria
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual para determinação do valor  da Base de Cálculo da operação própria
        /// </summary>
        [XmlIgnore]
        public double PBCOp { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PBCOp para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pBCOp")]
        public string PBCOpField
        {
            get => PBCOp.ToString("F4", CultureInfo.InvariantCulture);
            set => PBCOp = Converter.ToDouble(value);
        }

        /// <summary>
        /// Sigla da UF para qual é devido o ICMS ST da operação
        /// </summary>
        [XmlElement("UFST")]
        public UFBrasil UFST { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCField() => PRedBC > 0;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => PFCPST > 0;

        public bool ShouldSerializeVFCPSTField() => VFCPST > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSSN101
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN101")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN101
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// 101- Tributada pelo Simples Nacional com permissão de crédito
        /// </summary>
        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "101";

        /// <summary>
        /// Alíquota aplicável de cálculo do crédito (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double PCredSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double VCredICMSSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredICMSSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do ICMSSN102
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN102")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN102
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// 102 - Tributada pelo Simples Nacional sem permissão de crédito;
        /// 103 – Isenção do ICMS no Simples Nacional para faixa de receita bruta;
        /// 300 – Imune;
        /// 400 – Não tributda pelo Simples Nacional
        /// </summary>
        [XmlElement("CSOSN")]
        public virtual string CSOSN { get; set; } = "102";
    }

    /// <summary>
    /// Classe de informações do ICMSSN201
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN201")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN201
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// 201- Tributada pelo Simples Nacional com permissão de crédito e com cobrança do ICMS por Substituição Tributária
        /// </summary>
        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "201";

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota aplicável de cálculo do crédito (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double? PCredSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double? VCredICMSSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredICMSSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializePFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializeVFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;

        public bool ShouldSerializePCredSNField() => PCredSN != null;
        public bool ShouldSerializeVCredICMSSNField() => VCredICMSSN != null || PCredSN != null;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSSN202
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN202")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN202
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// 202- Tributada pelo Simples Nacional sem permissão de crédito e com cobrança do ICMS por Substituição Tributária;
        /// 203-  Isenção do ICMS nos Simples Nacional para faixa de receita bruta e com cobrança do ICMS por Substituição Tributária
        /// </summary>
        [XmlElement("CSOSN")]
        public string CSOSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializeVBCFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializePFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;
        public bool ShouldSerializeVFCPSTField() => (VBCFCPST + PFCPST + VFCPST) > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSSN500
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN500")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN500
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// 500 – ICMS cobrado anterirmente por substituição tributária (substituído) ou por antecipação
        /// </summary>
        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "500";

        /// <summary>
        /// Valor da BC do ICMS ST retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double? VBCSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Aliquota suportada pelo consumidor final
        /// </summary>
        [XmlIgnore]
        public double? PST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS próprio do substituto
        /// </summary>
        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSubstituto para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double? VICMSSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido anteriormente por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double VBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS efetiva
        /// </summary>
        [XmlIgnore]
        public double PICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS efetivo
        /// </summary>
        [XmlIgnore]
        public double VICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCSTRetField() => VBCSTRet != null;
        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;
        public bool ShouldSerializeVICMSSTRetField() => VICMSSTRet != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSSN900
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSSN900")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSSN900
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Tributação pelo ICMS 900 - Outros
        /// </summary>
        [XmlElement("CSOSN")]
        public string CSOSN { get; set; } = "900";

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// </summary>
        [XmlElement("modBC")]
#if INTEROP
        public ModalidadeBaseCalculoICMS ModBC { get; set; } = (ModalidadeBaseCalculoICMS)(-1);
#else
        public ModalidadeBaseCalculoICMS? ModBC { get; set; }
#endif

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Percentual de redução da BC
        /// </summary>
        [XmlIgnore]
        public double? PRedBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBC")]
        public string PRedBCField
        {
            get => PRedBC?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS
        /// </summary>
        [XmlIgnore]
        public double PICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMS")]
        public string PICMSField
        {
            get => PICMS.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade ModBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlIgnore]
#if INTEROP
        public ModalidadeBaseCalculoICMSST ModBCSTField { get; set; } = (ModalidadeBaseCalculoICMSST)(-1);

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// </summary>
        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#else
        public ModalidadeBaseCalculoICMSST? ModBCSTField { get; set; }

        [XmlElement("modBCST")]
        public ModalidadeBaseCalculoICMSST? ModBCST
        {
            get => ModBCSTField;
            set => ModBCSTField = value;
        }
#endif

        /// <summary>
        /// Percentual da Margem de Valor Adicionado ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PMVAST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PMVAST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pMVAST")]
        public string PMVASTField
        {
            get => PMVAST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PMVAST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da BC ICMS ST
        /// </summary>
        [XmlIgnore]
        public double? PRedBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCST")]
        public string PRedBCSTField
        {
            get => PRedBCST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double PICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSST")]
        public string PICMSSTField
        {
            get => PICMSST.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VICMSST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSST")]
        public string VICMSSTField
        {
            get => VICMSST.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de cálculo do FCP
        /// </summary>
        [XmlIgnore]
        public double VBCFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPST")]
        public string VBCFCPSTField
        {
            get => VBCFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPST")]
        public string PFCPSTField
        {
            get => PFCPST.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do FCP retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota aplicável de cálculo do crédito (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double? PCredSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCredSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCredSN")]
        public string PCredSNField
        {
            get => PCredSN?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCredSN = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)
        /// </summary>
        [XmlIgnore]
        public double? VCredICMSSN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCredICMSSN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCredICMSSN")]
        public string VCredICMSSNField
        {
            get => VCredICMSSN?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCredICMSSN = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeModBC() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1);

        public bool ShouldSerializeVBCField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializePRedBCField() => PRedBC != null;

        public bool ShouldSerializePICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVICMSField() => ModBC != null && ModBC != (ModalidadeBaseCalculoICMS)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeModBCST() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1);

        public bool ShouldSerializePMVASTField() => PMVAST != null;

        public bool ShouldSerializePRedBCSTField() => PRedBCST != null;

        public bool ShouldSerializeVBCSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializePICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVICMSSTField() => ModBCST != null && ModBCST != (ModalidadeBaseCalculoICMSST)(-1); //Se a modalidade for informada tem que ir esta TAG

        public bool ShouldSerializeVBCFCPSTField() => VBCFCPST > 0;

        public bool ShouldSerializePFCPSTField() => VBCFCPST > 0; // Se tiver base é obrigatório ter algo nesta TAG

        public bool ShouldSerializeVFCPSTField() => VBCFCPST > 0; // Se tiver base é obrigatório ter algo nesta TAG

        public bool ShouldSerializePCredSNField() => PCredSN != null;
        public bool ShouldSerializeVCredICMSSNField() => VCredICMSSN != null || PCredSN != null;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSST
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSST
    {
        /// <summary>
        /// Origem da mercadoria
        /// </summary>
        [XmlElement("orig")]
        public OrigemMercadoria Orig { get; set; }

        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Informar o valor da BC do ICMS ST retido na UF remetente
        /// </summary>
        [XmlIgnore]
        public double VBCSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCSTRet")]
        public string VBCSTRetField
        {
            get => VBCSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Aliquota suportada pelo consumidor final
        /// </summary>
        [XmlIgnore]
        public double? PST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pST")]
        public string PSTField
        {
            get => PST?.ToString("F4", CultureInfo.InvariantCulture);
            set => PST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS Próprio do Substituto cobrado em operação anterior
        /// </summary>
        [XmlIgnore]
        public double? VICMSSubstituto { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSubstituto para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSubstituto")]
        public string VICMSSubstitutoField
        {
            get => VICMSSubstituto?.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSubstituto = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o valor do ICMS ST retido na UF remetente
        /// </summary>
        [XmlIgnore]
        public double VICMSSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTRet")]
        public string VICMSSTRetField
        {
            get => VICMSSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o valor da BC do ICMS ST da UF destino
        /// </summary>
        [XmlIgnore]
        public double VBCSTDest { get; set; }

        /// <summary>
        /// Informar o valor da Base de Cálculo do FCP retido anteriormente por ST
        /// </summary>
        [XmlIgnore]
        public double VBCFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPSTRet")]
        public string VBCFCPSTRetField
        {
            get => VBCFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual relativo ao Fundo de Combate à Pobreza (FCP) retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double PFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPSTRet")]
        public string PFCPSTRetField
        {
            get => PFCPSTRet.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCSTDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCSTDest")]
        public string VBCSTDestField
        {
            get => VBCSTDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCSTDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o valor da BC do ICMS ST da UF destino
        /// </summary>
        [XmlIgnore]
        public double VICMSSTDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSSTDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSSTDest")]
        public string VICMSSTDestField
        {
            get => VICMSSTDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSSTDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de redução da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double PRedBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PRedBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pRedBCEfet")]
        public string PRedBCEfetField
        {
            get => PRedBCEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PRedBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da base de cálculo efetiva
        /// </summary>
        [XmlIgnore]
        public double VBCEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCEfet")]
        public string VBCEfetField
        {
            get => VBCEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do ICMS efetivo
        /// </summary>
        [XmlIgnore]
        public double PICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSEfet")]
        public string PICMSEfetField
        {
            get => PICMSEfet.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSEfet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS efetivo
        /// </summary>
        [XmlIgnore]
        public double VICMSEfet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSEfet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSEfet")]
        public string VICMSEfetField
        {
            get => VICMSEfet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSEfet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePSTField() => PST != null;
        public bool ShouldSerializeVICMSSubstitutoField() => VICMSSubstituto != null;

        public bool ShouldSerializeVBCFCPSTRetField() => VBCFCPSTRet > 0 || PFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializePFCPSTRetField() => PFCPSTRet > 0 || VBCFCPSTRet > 0 || VFCPSTRet > 0;
        public bool ShouldSerializeVFCPSTRetField() => VFCPSTRet > 0 || PFCPSTRet > 0 || VBCFCPSTRet > 0;

        public bool ShouldSerializePRedBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVBCEfetField() => VBCEfet > 0;
        public bool ShouldSerializePICMSEfetField() => VBCEfet > 0;
        public bool ShouldSerializeVICMSEfetField() => VBCEfet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do Imposto de Importação (II)
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.II")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class II
    {
        /// <summary>
        /// Base da BC do Imposto de Importação
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Valor das despesas aduaneiras
        /// </summary>
        [XmlIgnore]
        public double VDespAdu { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDespAdu para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDespAdu")]
        public string VDespAduField
        {
            get => VDespAdu.ToString("F2", CultureInfo.InvariantCulture);
            set => VDespAdu = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Imposto de Importação
        /// </summary>
        [XmlIgnore]
        public double VII { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VII para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vII")]
        public string VIIField
        {
            get => VII.ToString("F2", CultureInfo.InvariantCulture);
            set => VII = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do Imposto sobre Operações Financeiras
        /// </summary>
        [XmlIgnore]
        public double VIOF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIOF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIOF")]
        public string VIOFField
        {
            get => VIOF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIOF = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do IPI
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPI")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPI
    {
        /// <summary>
        /// CNPJ do produtor da mercadoria, quando diferente do emitente. Somente para os casos de exportação direta ou indireta
        /// </summary>
        [XmlElement("CNPJProd")]
        public string CNPJProd { get; set; }

        /// <summary>
        /// Código do selo de controle do IPI
        /// </summary>
        [XmlElement("cSelo")]
        public string CSelo { get; set; }

        /// <summary>
        /// Quantidade de selo de controle do IPI
        /// </summary>
        [XmlElement("qSelo")]
        public int? QSelo { get; set; }

        /// <summary>
        /// Código de Enquadramento Legal do IPI (tabela a ser criada pela RFB)
        /// </summary>
        [XmlElement("cEnq")]
        public string CEnq { get; set; }

        /// <summary>
        /// Grupo CST 01, 02, 03, 04, 51, 52, 53, 54 e 55
        /// </summary>
        [XmlElement("IPINT")]
        public IPINT IPINT { get; set; }

        /// <summary>
        /// Grupo do CST 00, 49, 50 e 99
        /// </summary>
        [XmlElement("IPITrib")]
        public IPITrib IPITrib { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJProd() => !string.IsNullOrWhiteSpace(CNPJProd);

        public bool ShouldSerializeCSelo() => !string.IsNullOrWhiteSpace(CSelo);

        public bool ShouldSerializeQSelo() => QSelo != null;

        #endregion
    }

    /// <summary>
    /// Grupo de informações do IPINT
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPINT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPINT
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }
    }

    /// <summary>
    /// Classe de informações do IPITrib
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPITrib")]
    [ComVisible(true)]
#endif
    [System.SerializableAttribute()]
    [System.Xml.Serialization.XmlTypeAttribute(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPITrib
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Valor da BC do IPI
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do IPI
        /// </summary>
        [XmlIgnore]
        public double PIPI { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PIPI para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIPI")]
        public string PIPIField
        {
            get => PIPI.ToString("F4", CultureInfo.InvariantCulture);
            set => PIPI = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade total na unidade padrão para tributação
        /// </summary>
        [XmlIgnore]
        public double QUnid { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QUnid para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qUnid")]
        public string QUnidField
        {
            get => QUnid.ToString("F4", CultureInfo.InvariantCulture);
            set => QUnid = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor por Unidade Tributável. Informar o valor do imposto Pauta por unidade de medida
        /// </summary>
        [XmlIgnore]
        public double VUnid { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VUnid para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vUnid")]
        public string VUnidField
        {
            get => VUnid.ToString("F4", CultureInfo.InvariantCulture);
            set => VUnid = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IPI
        /// </summary>
        [XmlIgnore]
        public double VIPI { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIPI para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIPI")]
        public string VIPIField
        {
            get => VIPI.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPI = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => QUnid <= 0;

        public bool ShouldSerializePIPIField() => QUnid <= 0;

        public bool ShouldSerializeQUnidField() => QUnid > 0;

        public bool ShouldSerializeVUnidField() => QUnid > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ISSQN
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ISSQN")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ISSQN
    {
        /// <summary>
        /// Valor da BC do ISSQN
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do ISSQN
        /// </summary>
        [XmlIgnore]
        public double VAliq { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VAliq para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vAliq")]
        public string VAliqField
        {
            get => VAliq.ToString("F4", CultureInfo.InvariantCulture);
            set => VAliq = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da do ISSQN
        /// </summary>
        [XmlIgnore]
        public double VISSQN { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VISSQN para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vISSQN")]
        public string VISSQNField
        {
            get => VISSQN.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSQN = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informar o município de ocorrência do fato gerador do ISSQN
        /// </summary>
        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }

        /// <summary>
        /// Informar o Item da lista de serviços da LC 116/03 em que se classifica o serviço
        /// </summary>
        [XmlElement("cListServ")]
        public ListaServicoISSQN CListServ { get; set; }

        /// <summary>
        /// Valor dedução para redução da base de cálculo
        /// </summary>
        [XmlIgnore]
        public double VDeducao { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDeducao para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDeducao")]
        public string VDeducaoField
        {
            get => VDeducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VDeducao = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor outras retenções
        /// </summary>
        [XmlIgnore]
        public double VOutro { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VOutro para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor desconto incondicionado
        /// </summary>
        [XmlIgnore]
        public double VDescIncond { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDescIncond para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDescIncond")]
        public string VDescIncondField
        {
            get => VDescIncond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescIncond = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor desconto condicionado
        /// </summary>
        [XmlIgnore]
        public double VDescCond { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDescCond para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDescCond")]
        public string VDescCondField
        {
            get => VDescCond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescCond = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Retenção ISS
        /// </summary>
        [XmlIgnore]
        public double VISSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VISSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vISSRet")]
        public string VISSRetField
        {
            get => VISSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Exibilidade do ISS
        /// </summary>
        [XmlElement("indISS")]
        public IndicadorExigibilidadeISSQN IndISS { get; set; }

        /// <summary>
        /// Código do serviço prestado dentro do município
        /// </summary>
        [XmlElement("cServico")]
        public string CServico { get; set; }

        /// <summary>
        /// Código do Município de Incidência do Imposto
        /// </summary>
        [XmlElement("cMun")]
        public int CMun { get; set; }

        /// <summary>
        /// Código do país
        /// </summary>
        [XmlElement("cPais")]
        public int CPais { get; set; } = 1058;

        /// <summary>
        /// Número do Processo administrativo ou judicial de suspenção do processo
        /// </summary>
        [XmlElement("nProcesso")]
        public string NProcesso { get; set; }

        /// <summary>
        /// Indicador de Incentivo Fiscal
        /// </summary>
        [XmlElement("indIncentivo")]
        public SimNao12 IndIncentivo { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVDeducaoField() => VDeducao > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeVDescIncondField() => VDescIncond > 0;

        public bool ShouldSerializeVDescCondField() => VDescCond > 0;

        public bool ShouldSerializeVISSRetField() => VISSRet > 0;

        public bool ShouldSerializeCServico() => string.IsNullOrWhiteSpace(CServico);

        public bool ShouldSerializeCMun() => CMun > 0;

        public bool ShouldSerializeCPais() => CPais > 0 && CPais != 1058;

        public bool ShouldSerializeNProcesso() => string.IsNullOrWhiteSpace(NProcesso);

        #endregion
    }

    /// <summary>
    /// Classe de informações do PIS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PIS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PIS
    {
        /// <summary>
        /// Dados do PIS tributado pela alíquota
        /// </summary>
        [XmlElement("PISAliq")]
        public PISAliq PISAliq { get; set; }

        /// <summary>
        /// Dados do PIS não tributado
        /// </summary>
        [XmlElement("PISNT")]
        public PISNT PISNT { get; set; }

        /// <summary>
        /// Dados do PIS outras operações
        /// </summary>
        [XmlElement("PISOutr")]
        public PISOutr PISOutr { get; set; }

        /// <summary>
        /// Dados do PIS tributado por Qtde
        /// </summary>
        [XmlElement("PISQtde")]
        public PISQtde PISQtde { get; set; }
    }

    /// <summary>
    /// Classe de informações do PISAliq
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISAliq")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISAliq
    {
        /// <summary>
        /// Código da substituição tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Valor da BC do PIS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do PIS (em percentual)
        /// </summary>
        [XmlIgnore]
        public double PPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        [XmlIgnore]
        public double VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do PISNT
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISNT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISNT
    {
        /// <summary>
        /// Código de situação de tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }
    }

    /// <summary>
    /// Classe de informações do PISOutr
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISOutr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISOutr
    {
        /// <summary>
        /// Código de situação de tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Valor da BC do PIS
        /// </summary>
        [XmlIgnore]
        public double? VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do PIS (em percentual)
        /// </summary>
        [XmlIgnore]
        public double? PPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        /// <summary>
        /// Alíquota do PIS (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        [XmlIgnore]
        public double? VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializePPISField() => PPIS != null;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVPISField() => VPIS != null;

        #endregion
    }

    /// <summary>
    /// Classe de informações do PISQtde
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISQtde")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISQtde
    {
        /// <summary>
        /// Código da substituição tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "03";

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        /// <summary>
        /// Alíquota do PIS (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double VAliqProd { get; set; }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        [XmlIgnore]
        public double VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do PISST
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.PISST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class PISST
    {
        /// <summary>
        /// Valor da BC do PIS ST
        /// </summary>
        [XmlIgnore]
        public double? VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do PIS ST (em percentual)
        /// </summary>
        [XmlIgnore]
        public double? PPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pPIS")]
        public string PPISField
        {
            get => PPIS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        /// <summary>
        /// Alíquota do PIS ST (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        /// <summary>
        /// Valor do PIS ST
        /// </summary>
        [XmlIgnore]
        public double? VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Indica se o valor do PISST compõe o valor total da NFe/NFCe
        /// </summary>
        [XmlElement("indSomaPISST")]
#if INTEROP
        public IndicaSomaPISST IndSomaPISST { get; set; } = (IndicaSomaPISST)(-1);
#else
        public IndicaSomaPISST? IndSomaPISST { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializePPISField() => PPIS != null;
        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVPISField() => VPIS != null;
        public bool ShouldSerializeIndSomaPISST() => IndSomaPISST != null && IndSomaPISST != (IndicaSomaPISST)(-1);

        #endregion
    }

    /// <summary>
    /// Classe de informações do COFINS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINS
    {
        /// <summary>
        /// Dados do COFINS tributado pela alíquota
        /// </summary>
        [XmlElement("COFINSAliq")]
        public COFINSAliq COFINSAliq { get; set; }

        /// <summary>
        /// Dados do COFINS não tributado
        /// </summary>
        [XmlElement("COFINSNT")]
        public COFINSNT COFINSNT { get; set; }

        /// <summary>
        /// Dados do COFINS outras operações
        /// </summary>
        [XmlElement("COFINSOutr")]
        public COFINSOutr COFINSOutr { get; set; }

        /// <summary>
        /// Dados do COFINS tributado por Qtde
        /// </summary>
        [XmlElement("COFINSQtde")]
        public COFINSQtde COFINSQtde { get; set; }
    }

    /// <summary>
    /// Classe de informações do COFINSAliq
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSAliq")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSAliq
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Valor da BC do COFINS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Alíquota do COFINS (em percentual)
        /// </summary>
        [XmlIgnore]
        public double PCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do COFINS
        /// </summary>
        [XmlIgnore]
        public double VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do COFINSNT
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSNT")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSNT
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }
    }

    /// <summary>
    /// Classe de informações de COFINSOutr
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSOutr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSOutr
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; }

        /// <summary>
        /// Valor da BC do COFINS
        /// </summary>
        [XmlIgnore]
        public double? VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do COFINS (em percentual)
        /// </summary>
        [XmlIgnore]
        public double? PCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        /// <summary>
        /// Alíquota do COFINS (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        /// <summary>
        /// Valor do COFINS
        /// </summary>
        [XmlIgnore]
        public double? VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializePCOFINSField() => PCOFINS != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVCOFINSField() => VCOFINS != null;

        #endregion
    }

    /// <summary>
    /// Classe de informações do COFINSQtde
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSQtde")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSQtde
    {
        /// <summary>
        /// Código de situação tributária
        /// </summary>
        [XmlElement("CST")]
        public string CST { get; set; } = "03";

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double QBCProd { get; set; }

        /// <summary>
        /// Alíquota do COFINS (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double VAliqProd { get; set; }

        /// <summary>
        /// Valor do COFINS
        /// </summary>
        [XmlIgnore]
        public double VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do COFINSST
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.COFINSST")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class COFINSST
    {
        /// <summary>
        /// Valor da BC do COFINS ST
        /// </summary>
        [XmlIgnore]
        public double? VBC { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBC para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBC")]
        public string VBCField
        {
            get => VBC?.ToString("F2", CultureInfo.InvariantCulture);
            set => VBC = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do COFINS ST (em percentual)
        /// </summary>
        [XmlIgnore]
        public double? PCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pCOFINS")]
        public string PCOFINSField
        {
            get => PCOFINS?.ToString("F4", CultureInfo.InvariantCulture);
            set => PCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Quantidade Vendida
        /// </summary>
        [XmlElement("qBCProd")]
        public double? QBCProd { get; set; }

        /// <summary>
        /// Alíquota do COFINS ST (em reais)
        /// </summary>
        [XmlElement("vAliqProd")]
        public double? VAliqProd { get; set; }

        /// <summary>
        /// Valor do COFINS ST
        /// </summary>
        [XmlIgnore]
        public double? VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS?.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Indica se o valor da COFINS ST compõe o valor total da NFe
        /// </summary>
        [XmlElement("indSomaCOFINSST")]
#if INTEROP
        public IndicaSomaCOFINSST IndSomaCOFINSST { get; set; } = (IndicaSomaCOFINSST)(-1);
#else
        public IndicaSomaCOFINSST? IndSomaCOFINSST { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeVBCField() => VBC != null;
        public bool ShouldSerializePCOFINSField() => PCOFINS != null;
        public bool ShouldSerializeQBCProd() => QBCProd != null;
        public bool ShouldSerializeVAliqProd() => VAliqProd != null;
        public bool ShouldSerializeVCOFINSField() => VCOFINS != null;
        public bool ShouldSerializeIndSomaCOFINSST() => IndSomaCOFINSST != null && IndSomaCOFINSST != (IndicaSomaCOFINSST)(-1);

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSUFDest
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSUFDest")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSUFDest
    {
        /// <summary>
        /// Valor da Base de Cálculo do ICMS na UF do destinatário
        /// </summary>
        [XmlIgnore]
        public double VBCUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCUFDest")]
        public string VBCUFDestField
        {
            get => VBCUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Base de Cálculo do FCP na UF do destinatário
        /// </summary>
        [XmlIgnore]
        public double VBCFCPUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCFCPUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCFCPUFDest")]
        public string VBCFCPUFDestField
        {
            get => VBCFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCFCPUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual adicional inserido na alíquota interna da UF de destino, relativo ao Fundo de Combate à Pobreza (FCP) naquela UF
        /// </summary>
        [XmlIgnore]
        public double PFCPUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PFCPUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pFCPUFDest")]
        public string PFCPUFDestField
        {
            get => PFCPUFDest.ToString("F4", CultureInfo.InvariantCulture);
            set => PFCPUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota adotada nas operações internas na UF do destinatário para o produto / mercadoria
        /// </summary>
        [XmlIgnore]
        public double PICMSUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSUFDest")]
        public string PICMSUFDestField
        {
            get => PICMSUFDest.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota interestadual das UF envolvidas
        /// </summary>
        [XmlIgnore]
        public double PICMSInter { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSInter para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSInter")]
        public string PICMSInterField
        {
            get => PICMSInter.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSInter = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual de partilha para a UF do destinatário
        /// </summary>
        [XmlIgnore]
        public double PICMSInterPart { get; set; } = 100;

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSInterPart para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSInterPart")]
        public string PICMSInterPartField
        {
            get => PICMSInterPart.ToString("F4", CultureInfo.InvariantCulture);
            set => PICMSInterPart = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) da UF de destino
        /// </summary>
        [XmlIgnore]
        public double VFCPUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPUFDest")]
        public string VFCPUFDestField
        {
            get => VFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de partilha para a UF do destinatário
        /// </summary>
        [XmlIgnore]
        public double VICMSUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFDest")]
        public string VICMSUFDestField
        {
            get => VICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS de partilha para a UF do remetente
        /// </summary>
        [XmlIgnore]
        public double VICMSUFRemet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSUFRemet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFRemet")]
        public string VICMSUFRemetField
        {
            get => VICMSUFRemet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFRemet = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCFCPUFDestField() => VBCFCPUFDest > 0;

        public bool ShouldSerializePFCPUFDestField() => PFCPUFDest > 0;

        public bool ShouldSerializeVFCPUFDestField() => VFCPUFDest > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ImpostoDevolPercentual de mercadoria devolvida
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ImpostoDevol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ImpostoDevol
    {
        /// <summary>
        /// Percentual de mercadoria devolvida
        /// </summary>
        [XmlIgnore]
        public double PDevol { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PDevol para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDevol")]
        public string PDevolField
        {
            get => PDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => PDevol = Converter.ToDouble(value);
        }

        /// <summary>
        /// Informação de IPI devolvido
        /// </summary>
        [XmlElement("IPI")]
        public IPIDevol IPI { get; set; }
    }

    /// <summary>
    /// Classe de informações do ObsItem
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsItem")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsItem
    {
        /// <summary>
        /// Grupo de observações de uso livre (para o item da NFe/NFCe)
        /// </summary>
        [XmlElement("obsCont")]
        public List<ObsCont> ObsCont { get; set; }

        /// <summary>
        /// Grupo de observações de uso livre do Fisco
        /// </summary>
        [XmlElement("obsFisco")]
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
    }

    /// <summary>
    /// Classe de informações do IPIDevol
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IPIDevol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IPIDevol
    {
        /// <summary>
        /// Valor do IPI devolvido
        /// </summary>
        [XmlIgnore]
        public double VIPIDevol { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIPIDevol para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIPIDevol")]
        public string VIPIDevolField
        {
            get => VIPIDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPIDevol = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do Total
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Total")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Total
    {
        /// <summary>
        /// Totais referentes ao ICMS
        /// </summary>
        [XmlElement("ICMSTot")]
        public ICMSTot ICMSTot { get; set; }

        /// <summary>
        /// Totais referentes ao ISSQN
        /// </summary>
        [XmlElement("ISSQNtot")]
        public ISSQNtot ISSQNtot { get; set; }

        /// <summary>
        /// Retenção de Tributos Federais
        /// </summary>
        [XmlElement("retTrib")]
        public RetTrib RetTrib { get; set; }

        /// <summary>
        /// Grupo total do imposto seletivo
        /// </summary>
        [XmlElement("ISTot")]
        public ISTot ISTot { get; set; }

        /// <summary>
        /// Totais da NF-e com IBS e CBS
        /// </summary>
        [XmlElement("IBSCBSTot")]
        public IBSCBSTot IBSCBSTot { get; set; }

        /// <summary>
        /// Valor total da NF-e com IBS / CBS / IS 
        /// </summary>
        [XmlIgnore]
        public double VNFTot { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vNFTot para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vNFTot")]
        public string VNFTotField
        {
            get => VNFTot.ToString("F2", CultureInfo.InvariantCulture);
            set => VNFTot = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVNFTotField() => VNFTot > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ICMSTot
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ICMSTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ICMSTot
    {
        /// <summary>
        /// BC do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Valor Total do ICMS
        /// </summary>
        [XmlIgnore]
        public double VICMS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMS")]
        public string VICMSField
        {
            get => VICMS.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do ICMS desonerado
        /// </summary>
        [XmlIgnore]
        public double VICMSDeson { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSDeson para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSDeson")]
        public string VICMSDesonField
        {
            get => VICMSDeson.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSDeson = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS relativo ao Fundo de Combate à Pobreza (FCP) para a UF de destino
        /// </summary>
        [XmlIgnore]
        public double VFCPUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPUFDest")]
        public string VFCPUFDestField
        {
            get => VFCPUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS de partilha para a UF do destinatário
        /// </summary>
        [XmlIgnore]
        public double VICMSUFDest { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSUFDest para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFDest")]
        public string VICMSUFDestField
        {
            get => VICMSUFDest.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFDest = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS de partilha para a UF do remetente
        /// </summary>
        [XmlIgnore]
        public double VICMSUFRemet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSUFRemet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSUFRemet")]
        public string VICMSUFRemetField
        {
            get => VICMSUFRemet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSUFRemet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do FCP (Fundo de Combate à Pobreza)
        /// </summary>
        [XmlIgnore]
        public double VFCP { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCP para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCP")]
        public string VFCPField
        {
            get => VFCP.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCP = Converter.ToDouble(value);
        }

        /// <summary>
        /// BC do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VBCST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCST")]
        public string VBCSTField
        {
            get => VBCST.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do ICMS ST
        /// </summary>
        [XmlIgnore]
        public double VST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vST")]
        public string VSTField
        {
            get => VST.ToString("F2", CultureInfo.InvariantCulture);
            set => VST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do FCP (Fundo de Combate à Pobreza) retido por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPST { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPST para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPST")]
        public string VFCPSTField
        {
            get => VFCPST.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPST = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do FCP (Fundo de Combate à Pobreza) retido anteriormente por substituição tributária
        /// </summary>
        [XmlIgnore]
        public double VFCPSTRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFCPSTRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFCPSTRet")]
        public string VFCPSTRetField
        {
            get => VFCPSTRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VFCPSTRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico próprio
        /// </summary>
        [XmlIgnore]
        public double QBCMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QBCMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qBCMono")]
        public string QBCMonoField
        {
            get => QBCMono.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS monofásico próprio
        /// </summary>
        [XmlIgnore]
        public double VICMSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMono")]
        public string VICMSMonoField
        {
            get => VICMSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double QBCMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QBCMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qBCMonoReten")]
        public string QBCMonoRetenField
        {
            get => QBCMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do ICMS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoReten")]
        public string VICMSMonoRetenField
        {
            get => VICMSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double QBCMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QBCMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qBCMonoRet")]
        public string QBCMonoRetField
        {
            get => QBCMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => QBCMonoRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS monofásico retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VICMSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSMonoRet")]
        public string VICMSMonoRetField
        {
            get => VICMSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSMonoRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total dos produtos e serviços
        /// </summary>
        [XmlIgnore]
        public double VProd { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VProd para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vProd")]
        public string VProdField
        {
            get => VProd.ToString("F2", CultureInfo.InvariantCulture);
            set => VProd = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do Frete
        /// </summary>
        [XmlIgnore]
        public double VFrete { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFrete para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFrete")]
        public string VFreteField
        {
            get => VFrete.ToString("F2", CultureInfo.InvariantCulture);
            set => VFrete = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do Seguro
        /// </summary>
        [XmlIgnore]
        public double VSeg { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VSeg para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vSeg")]
        public string VSegField
        {
            get => VSeg.ToString("F2", CultureInfo.InvariantCulture);
            set => VSeg = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do Desconto
        /// </summary>
        [XmlIgnore]
        public double VDesc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDesc para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do II
        /// </summary>
        [XmlIgnore]
        public double VII { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VII para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vII")]
        public string VIIField
        {
            get => VII.ToString("F2", CultureInfo.InvariantCulture);
            set => VII = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do IPI
        /// </summary>
        [XmlIgnore]
        public double VIPI { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIPI para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIPI")]
        public string VIPIField
        {
            get => VIPI.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPI = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total do IPI devolvido
        /// </summary>
        [XmlIgnore]
        public double VIPIDevol { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIPIDevol para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIPIDevol")]
        public string VIPIDevolField
        {
            get => VIPIDevol.ToString("F2", CultureInfo.InvariantCulture);
            set => VIPIDevol = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do PIS
        /// </summary>
        [XmlIgnore]
        public double VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do COFINS
        /// </summary>
        [XmlIgnore]
        public double VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Outras Despesas acessórias
        /// </summary>
        [XmlIgnore]
        public double VOutro { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VOutro para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total da NFe/NFCe
        /// </summary>
        [XmlIgnore]
        public double VNF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VNF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vNF")]
        public string VNFField
        {
            get => VNF.ToString("F2", CultureInfo.InvariantCulture);
            set => VNF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor estimado total de impostos federais, estaduais e municipais
        /// </summary>
        [XmlIgnore]
        public double VTotTrib { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTotTrib para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotTrib")]
        public string VTotTribField
        {
            get => VTotTrib.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotTrib = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVFCPUFDestField() => VFCPUFDest > 0;

        public bool ShouldSerializeVICMSUFDestField() => VICMSUFDest > 0;

        public bool ShouldSerializeVICMSUFRemetField() => VICMSUFRemet > 0;

        public bool ShouldSerializeVTotTribField() => VTotTrib > 0;

        public bool ShouldSerializeQBCMonoField() => QBCMono > 0;

        public bool ShouldSerializeVICMSMonoField() => VICMSMono > 0;

        public bool ShouldSerializeQBCMonoRetenField() => QBCMonoReten > 0;

        public bool ShouldSerializeVICMSMonoRetenField() => VICMSMonoReten > 0;

        public bool ShouldSerializeVICMSMonoRetField() => VICMSMonoRet > 0;

        public bool ShouldSerializeQBCMonoRetField() => QBCMonoRet > 0;

        #endregion
    }

    /// <summary>
    /// Classe de informações do ISSQNTot
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ISSQNtot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ISSQNtot
    {
        /// <summary>
        /// Valor Total dos Serviços sob não-incidência ou não tributados pelo ICMS
        /// </summary>
        [XmlIgnore]
        public double VServ { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VServ para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vServ")]
        public string VServField
        {
            get => VServ.ToString("F2", CultureInfo.InvariantCulture);
            set => VServ = Converter.ToDouble(value);
        }

        /// <summary>
        /// Base de Cálculo do ISS
        /// </summary>
        [XmlIgnore]
        public double VBC { get; set; }

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
        /// Valor Total do ISS
        /// </summary>
        [XmlIgnore]
        public double VISS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VISS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vISS")]
        public string VISSField
        {
            get => VISS.ToString("F2", CultureInfo.InvariantCulture);
            set => VISS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do PIS sobre serviços
        /// </summary>
        [XmlIgnore]
        public double VPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPIS")]
        public string VPISField
        {
            get => VPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do COFINS sobre serviços
        /// </summary>
        [XmlIgnore]
        public double VCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCOFINS")]
        public string VCOFINSField
        {
            get => VCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Data da prestação do serviço (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DCompet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DCompet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dCompet")]
        public string DCompetField
        {
            get => DCompet.ToString("yyyy-MM-dd");
            set => DCompet = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor dedução para redução da base de cálculo
        /// </summary>
        [XmlIgnore]
        public double VDeducao { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDeducao para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDeducao")]
        public string VDeducaoField
        {
            get => VDeducao.ToString("F2", CultureInfo.InvariantCulture);
            set => VDeducao = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor outras retenções
        /// </summary>
        [XmlIgnore]
        public double VOutro { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VOutro para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOutro")]
        public string VOutroField
        {
            get => VOutro.ToString("F2", CultureInfo.InvariantCulture);
            set => VOutro = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor desconto incondicionado
        /// </summary>
        [XmlIgnore]
        public double VDescIncond { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDescIncond para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDescIncond")]
        public string VDescIncondField
        {
            get => VDescIncond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescIncond = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor desconto condicionado
        /// </summary>
        [XmlIgnore]
        public double VDescCond { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDescCond para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDescCond")]
        public string VDescCondField
        {
            get => VDescCond.ToString("F2", CultureInfo.InvariantCulture);
            set => VDescCond = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total Retenção ISS
        /// </summary>
        [XmlIgnore]
        public double VISSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VISSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vISSRet")]
        public string VISSRetField
        {
            get => VISSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VISSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código do regime especial de tributação
        /// </summary>
        [XmlElement("cRegTrib")]
        public CodigoRegimeEspecialTributacao CRegTrib { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeVServField() => VServ > 0;

        public bool ShouldSerializeVBCField() => VBC > 0;

        public bool ShouldSerializeVISSField() => VISS > 0;

        public bool ShouldSerializeVPISField() => VPIS > 0;

        public bool ShouldSerializeVCOFINSField() => VCOFINS > 0;

        public bool ShouldSerializeVDeducaoField() => VDeducao > 0;

        public bool ShouldSerializeVOutroField() => VOutro > 0;

        public bool ShouldSerializeVDescIncondField() => VDescIncond > 0;

        public bool ShouldSerializeVDescCondField() => VDescCond > 0;

        public bool ShouldSerializeVISSRetField() => VISSRet > 0;

        public bool ShouldSerializeCRegTrib() => Enum.IsDefined(typeof(CodigoRegimeEspecialTributacao), CRegTrib);

        #endregion

    }

    /// <summary>
    /// Classe de informações do RetTrib
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetTrib")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RetTrib
    {
        /// <summary>
        /// Valor Retido de PIS
        /// </summary>
        [XmlIgnore]
        public double VRetPIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VRetPIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vRetPIS")]
        public string VRetPISField
        {
            get => VRetPIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Retido de COFINS
        /// </summary>
        [XmlIgnore]
        public double VRetCOFINS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VRetCOFINS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vRetCOFINS")]
        public string VRetCOFINSField
        {
            get => VRetCOFINS.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCOFINS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Retido de CSLL
        /// </summary>
        [XmlIgnore]
        public double VRetCSLL { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VRetCSLL para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vRetCSLL")]
        public string VRetCSLLField
        {
            get => VRetCSLL.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetCSLL = Converter.ToDouble(value);
        }

        /// <summary>
        /// Base de Cálculo do IRRF
        /// </summary>
        [XmlIgnore]
        public double VBCIRRF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCIRRF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCIRRF")]
        public string VBCIRRFField
        {
            get => VBCIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIRRF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Retido de IRRF
        /// </summary>
        [XmlIgnore]
        public double VIRRF { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIRRF para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIRRF")]
        public string VIRRFField
        {
            get => VIRRF.ToString("F2", CultureInfo.InvariantCulture);
            set => VIRRF = Converter.ToDouble(value);
        }

        /// <summary>
        /// Base de Cálculo da Retenção da Previdêncica Social
        /// </summary>
        [XmlIgnore]
        public double VBCRetPrev { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCRetPrev para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCRetPrev")]
        public string VBCRetPrevField
        {
            get => VBCRetPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCRetPrev = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da Retenção da Previdêncica Social
        /// </summary>
        [XmlIgnore]
        public double VRetPrev { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VRetPrev para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vRetPrev")]
        public string VRetPrevField
        {
            get => VRetPrev.ToString("F2", CultureInfo.InvariantCulture);
            set => VRetPrev = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVRetPISField() => VRetPIS > 0;

        public bool ShouldSerializeVRetCOFINSField() => VRetCOFINS > 0;

        public bool ShouldSerializeVRetCSLLField() => VRetCSLL > 0;

        public bool ShouldSerializeVBCIRRFField() => VBCIRRF > 0;

        public bool ShouldSerializeVIRRFField() => VIRRF > 0;

        public bool ShouldSerializeVBCRetPrevField() => VBCRetPrev > 0;

        public bool ShouldSerializeVRetPrevField() => VRetPrev > 0;

        #endregion

    }

    /// <summary>
    /// Classe de informações do Transp
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Transp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Transp
    {
        private string VagaoField;
        private string BalsaField;

        /// <summary>
        /// Modalidade do frete
        /// </summary>
        [XmlElement("modFrete")]
        public ModalidadeFrete ModFrete { get; set; }

        /// <summary>
        /// Dados do transportador
        /// </summary>
        [XmlElement("transporta")]
        public Transporta Transporta { get; set; }

        /// <summary>
        /// Dados da retenção ICMS do Transporte
        /// </summary>
        [XmlElement("retTransp")]
        public RetTransp RetTransp { get; set; }

        /// <summary>
        /// Dados do veículo
        /// </summary>
        [XmlElement("veicTransp")]
        public VeicTransp VeicTransp { get; set; }

        /// <summary>
        /// Dados do reboque/Dolly
        /// </summary>
        [XmlElement("reboque")]
        public List<Reboque> Reboque { get; set; }

        /// <summary>
        /// Identificação do vagão
        /// </summary>
        [XmlElement("vagao")]
        public string Vagao
        {
            get => VagaoField;
            set => VagaoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(20).Trim());
        }

        /// <summary>
        /// Identificação da balsa
        /// </summary>
        [XmlElement("balsa")]
        public string Balsa
        {
            get => BalsaField;
            set => BalsaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(20).Trim());
        }

        /// <summary>
        /// Dados dos volumes
        /// </summary>
        [XmlElement("vol")]
        public List<Vol> Vol { get; set; } = new List<Vol>();

        #region ShouldSerialize

        public bool ShouldSerializeVagao() => !string.IsNullOrWhiteSpace(Vagao);

        public bool ShouldSerializeBalsa() => !string.IsNullOrWhiteSpace(Balsa);

        public bool ShouldSerializeVol()
        {
            if (Vol.Count <= 0)
            {
                return false;
            }

            for (var i = 0; i < Vol.Count; i++)
            {

                if (Vol[i].QVol > 0 ||
                    !string.IsNullOrWhiteSpace(Vol[i].Esp) ||
                    !string.IsNullOrWhiteSpace(Vol[i].Marca) ||
                    !string.IsNullOrWhiteSpace(Vol[i].NVol) ||
                    Vol[i].PesoL > 0 ||
                    Vol[i].PesoB > 0)
                {
                    return true;
                }
            }

            return false;
        }

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="reboque">Elemento</param>
        public void AddReboque(Reboque reboque)
        {
            if (Reboque == null)
            {
                Reboque = new List<Reboque>();
            }

            Reboque.Add(reboque);
        }

        /// <summary>
        /// Retorna o elemento da lista Reboque (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Reboque</returns>
        public Reboque GetReboque(int index)
        {
            if ((Reboque?.Count ?? 0) == 0)
            {
                return default;
            };

            return Reboque[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Reboque
        /// </summary>
        public int GetReboqueCount => (Reboque != null ? Reboque.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="vol">Elemento</param>
        public void AddVol(Vol vol)
        {
            if (Vol == null)
            {
                Vol = new List<Vol>();
            }

            Vol.Add(vol);
        }

        /// <summary>
        /// Retorna o elemento da lista Vol (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Vol</returns>
        public Vol GetVol(int index)
        {
            if ((Vol?.Count ?? 0) == 0)
            {
                return default;
            };

            return Vol[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Vol
        /// </summary>
        public int GetVolCount => (Vol != null ? Vol.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações do Transporta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Transporta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Transporta
    {
        private string XNomeField;
        private string XEnderField;
        private string XMunField;

        /// <summary>
        /// CNPJ do transportador
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// CPF do transportador
        /// </summary>
        [XmlElement("CPF")]
        public string CPF { get; set; }

        /// <summary>
        /// 
        /// </summary>
        [XmlElement("xNome")]
        public string XNome
        {
            get => XNomeField;
            set => XNomeField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Inscrição Estadual
        /// </summary>
        [XmlElement("IE")]
        public string IE { get; set; }

        /// <summary>
        /// Endereço completo
        /// </summary>
        [XmlElement("xEnder")]
        public string XEnder
        {
            get => XEnderField;
            set => XEnderField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Nome do munícipio
        /// </summary>
        [XmlElement("xMun")]
        public string XMun
        {
            get => XMunField;
            set => XMunField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Sigla da UF
        /// </summary>
#if INTEROP
        [XmlElement("UF")]
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UF")]
        public UFBrasil? UF { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

        public bool ShouldSerializeCPF() => !string.IsNullOrWhiteSpace(CPF);

        public bool ShouldSerializeXNome() => !string.IsNullOrWhiteSpace(XNome);

        public bool ShouldSerializeIE() => !string.IsNullOrWhiteSpace(IE);

        public bool ShouldSerializeXEnder() => !string.IsNullOrWhiteSpace(XEnder);

        public bool ShouldSerializeXMun() => !string.IsNullOrWhiteSpace(XMun);

        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

    /// <summary>
    /// Classe de informações do RetTransp
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.RetTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class RetTransp
    {
        /// <summary>
        /// Valor do Serviço
        /// </summary>
        [XmlIgnore]
        public double VServ { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VServ para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vServ")]
        public string VServField
        {
            get => VServ.ToString("F2", CultureInfo.InvariantCulture);
            set => VServ = Converter.ToDouble(value);
        }

        /// <summary>
        /// BC da Retenção do ICMS
        /// </summary>
        [XmlIgnore]
        public double VBCRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VBCRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCRet")]
        public string VBCRetField
        {
            get => VBCRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota da Retenção
        /// </summary>
        [XmlIgnore]
        public double PICMSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pICMSRet")]
        public string PICMSRetField
        {
            get => PICMSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => PICMSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do ICMS Retido
        /// </summary>
        [XmlIgnore]
        public double VICMSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VICMSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vICMSRet")]
        public string VICMSRetRetField
        {
            get => VICMSRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VICMSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Código Fiscal de Operações e Prestações
        /// </summary>
        [XmlElement("CFOP")]
        public string CFOP { get; set; }

        /// <summary>
        /// Código do Município de Ocorrência do Fato Gerador (utilizar a tabela do IBGE)
        /// </summary>
        [XmlElement("cMunFG")]
        public int CMunFG { get; set; }
    }

    /// <summary>
    /// Classe base para informações em comum dos veículos
    /// </summary>
    public abstract class VeiculoBase
    {
        /// <summary>
        /// Placa do veículo
        /// </summary>
        [XmlElement("placa")]
        public string Placa { get; set; }

        /// <summary>
        /// Sigla da UF
        /// </summary>
#if INTEROP
        [XmlElement("UF")]
        public UFBrasil UF { get; set; } = UFBrasil.NaoDefinido;
#else
        [XmlElement("UF")]
        public UFBrasil? UF { get; set; }
#endif

        /// <summary>
        /// Registro Nacional de Transportador de Carga (ANTT)
        /// </summary>
        [XmlElement("RNTC")]
        public string RNTC { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeRNTC() => !string.IsNullOrWhiteSpace(RNTC);

        public bool ShouldSerializeUF() => UF != null && UF != UFBrasil.NaoDefinido;

        #endregion
    }

    /// <summary>
    /// Classe de informações de VeicTransp
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.VeicTransp")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class VeicTransp : VeiculoBase { }

    /// <summary>
    /// Classe de informações de Reboque
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Reboque")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Reboque : VeiculoBase { }

    /// <summary>
    /// Classe de informações de Vol
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Vol")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Vol
    {
        private string EspField;
        private string MarcaField;

        /// <summary>
        /// Quantidade de volumes transportados
        /// </summary>
        [XmlElement("qVol")]
        public double QVol { get; set; }

        /// <summary>
        /// Espécie dos volumes transportados
        /// </summary>
        [XmlElement("esp")]
        public string Esp
        {
            get => EspField;
            set => EspField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Marca dos volumes transportados
        /// </summary>
        [XmlElement("marca")]
        public string Marca
        {
            get => MarcaField;
            set => MarcaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Numeração dos volumes transportados
        /// </summary>
        [XmlElement("nVol")]
        public string NVol { get; set; }

        /// <summary>
        /// Peso líquido (em kg)
        /// </summary>
        [XmlIgnore]
        public double PesoL { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PesoL para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pesoL")]
        public string PesoLField
        {
            get => PesoL.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoL = Converter.ToDouble(value);
        }

        /// <summary>
        /// Peso bruto (em kg)
        /// </summary>
        [XmlIgnore]
        public double PesoB { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PesoB para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pesoB")]
        public string PesoBField
        {
            get => PesoB.ToString("F3", CultureInfo.InvariantCulture);
            set => PesoB = Converter.ToDouble(value);
        }

        /// <summary>
        /// Dados dos lacres
        /// </summary>
        [XmlElement("lacres")]
        public List<Lacres> Lacres { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeEsp() => !string.IsNullOrWhiteSpace(Esp);

        public bool ShouldSerializeMarca() => !string.IsNullOrWhiteSpace(Marca);

        public bool ShouldSerializeNVol() => !string.IsNullOrWhiteSpace(NVol);

        public bool ShouldSerializeQVol() => QVol > 0;

        public bool ShouldSerializePesoLField() => PesoL > 0;

        public bool ShouldSerializePesoBField() => PesoB > 0;

        #endregion

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="lacres">Elemento</param>
        public void AddLacres(Lacres lacres)
        {
            if (Lacres == null)
            {
                Lacres = new List<Lacres>();
            }

            Lacres.Add(lacres);
        }

        /// <summary>
        /// Retorna o elemento da lista Lacres (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Lacres</returns>
        public Lacres GetLacres(int index)
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

    /// <summary>
    /// Classe de informações dos lacres
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Lacres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Lacres
    {
        /// <summary>
        /// Número dos Lacres
        /// </summary>
        [XmlElement("nLacre")]
        public string NLacre { get; set; }
    }

    /// <summary>
    /// Classe de informações de Cobr
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Cobr")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Cobr
    {
        /// <summary>
        /// Dados da fatura
        /// </summary>
        [XmlElement("fat")]
        public Fat Fat { get; set; }

        /// <summary>
        /// Dados das duplicatas
        /// </summary>
        [XmlElement("dup")]
        public List<Dup> Dup { get; set; } = new List<Dup>();

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

    /// <summary>
    /// Classe de informações de Fat
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Fat")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Fat
    {
        /// <summary>
        /// Número da fatura
        /// </summary>
        [XmlElement("nFat")]
        public string NFat { get; set; }

        /// <summary>
        /// Valor original da fatura
        /// </summary>
        [XmlIgnore]
        public double VOrig { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VOrig para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vOrig")]
        public string VOrigField
        {
            get => VOrig.ToString("F2", CultureInfo.InvariantCulture);
            set => VOrig = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do desconto da fatura
        /// </summary>
        [XmlIgnore]
        public double VDesc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDesc para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDesc")]
        public string VDescField
        {
            get => VDesc.ToString("F2", CultureInfo.InvariantCulture);
            set => VDesc = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor líquido da fatura
        /// </summary>
        [XmlIgnore]
        public double VLiq { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VLiq para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vLiq")]
        public string VLiqField
        {
            get => VLiq.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiq = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações de Dup
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Dup")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Dup
    {
        /// <summary>
        /// Número da duplicata
        /// </summary>
        [XmlIgnore]
        public string NDup { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade NDup para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("nDup")]
        public string NDupField
        {
            get => NDup.PadLeft(3, '0');
            set => NDup = value;
        }

        /// <summary>
        /// Data de vencimento da duplicata (AAAA-MM-DD)
        /// </summary>
        [XmlIgnore]
        public DateTime DVenc { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DVenc para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dVenc")]
        public string DVencField
        {
            get => DVenc.ToString("yyyy-MM-dd");
            set => DVenc = DateTime.Parse(value);
        }

        /// <summary>
        /// Valor da duplicata
        /// </summary>
        [XmlIgnore]
        public double VDup { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDup para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDup")]
        public string VDupField
        {
            get => VDup.ToString("F2", CultureInfo.InvariantCulture);
            set => VDup = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações de Pag
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Pag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Pag
    {
        /// <summary>
        /// Grupo de detalhamento da forma de pagamento
        /// </summary>
        [XmlElement("detPag")]
        public List<DetPag> DetPag { get; set; } = new List<DetPag>();

        /// <summary>
        /// Valor do Troco
        /// </summary>
        [XmlIgnore]
        public double VTroco { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTroco para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTroco")]
        public string VTrocoField
        {
            get => VTroco.ToString("F2", CultureInfo.InvariantCulture);
            set => VTroco = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVTrocoField() => VTroco > 0;

        #endregion ShouldSerialize

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="detPag">Elemento</param>
        public void AddDetPag(DetPag detPag)
        {
            if (DetPag == null)
            {
                DetPag = new List<DetPag>();
            }

            DetPag.Add(detPag);
        }

        /// <summary>
        /// Retorna o elemento da lista DetPag (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da DetPag</returns>
        public DetPag GetDetPag(int index)
        {
            if ((DetPag?.Count ?? 0) == 0)
            {
                return default;
            };

            return DetPag[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista DetPag
        /// </summary>
        public int GetDetPagCount => (DetPag != null ? DetPag.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações do DetPag
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DetPag")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DetPag
    {
        private string XPagField { get; set; }

        /// <summary>
        /// Indicador da Forma de Pagamento
        /// </summary>
        [XmlElement("indPag")]
#if INTEROP
        public IndicadorPagamento IndPag { get; set; } = (IndicadorPagamento)(-1);
#else
        public IndicadorPagamento? IndPag { get; set; }
#endif

        /// <summary>
        /// Forma de Pagamento
        /// </summary>
        [XmlElement("tPag")]
        public MeioPagamento TPag { get; set; }

        /// <summary>
        /// Descrição do Meio de Pagamento
        /// </summary>
        [XmlElement("xPag")]
        public string XPag
        {
            get => XPagField;
            set => XPagField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Valor do Pagamento. Esta tag poderá ser omitida quando a tag tPag=90 (Sem Pagamento), caso contrário deverá ser preenchida
        /// </summary>
        [XmlIgnore]
        public double VPag { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VPag para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vPag")]
        public string VPagField
        {
            get => VPag.ToString("F2", CultureInfo.InvariantCulture);
            set => VPag = Converter.ToDouble(value);
        }

        /// <summary>
        /// Data do pagamento
        /// </summary>
        [XmlIgnore]
        public DateTime DPag { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade DPag para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("dPag")]
        public string DPagField
        {
            get => DPag.ToString("yyyy-MM-dd");
            set => DPag = DateTime.Parse(value);
        }

        /// <summary>
        /// CNPJ transacional do pagamento. Preencher informando o CNPJ do estabelecimento onde o pagamento foi processado/transacionado/recebido quando a emissão do documento fiscal ocorrer em estabelecimento distinto
        /// </summary>
        [XmlElement("CNPJPag")]
        public string CNPJPag { get; set; }

        /// <summary>
        /// UF do CNPJ do estabelecimento onde o pagamento foi processado/transacionado/recebido.
        /// </summary>
        [XmlElement("UFPag")]
        public UFBrasil UFPag { get; set; }

        /// <summary>
        /// Grupo de Cartões, PIX, Boletos e outros Pagamentos Eletrônicos
        /// </summary>
        [XmlElement("card")]
        public Card Card { get; set; }

        public bool ShouldSerializeIndPag() => IndPag != null && IndPag != (IndicadorPagamento)(-1);
        public bool ShouldSerializeXPag() => !string.IsNullOrWhiteSpace(XPag);
        public bool ShouldSerializeDPagField() => DPag > DateTime.MinValue;
        public bool ShouldSerializeCNPJPag() => !string.IsNullOrWhiteSpace(CNPJPag);
        public bool ShouldSerializeUFPag() => !string.IsNullOrWhiteSpace(CNPJPag);


#if INTEROP
        [ObsoleteAttribute("Este método está obsoleto e será excluído em futuras versões. Utilize a propriedade IndPag para atribuir o conteúdo desejado.", false)]
        public void SetIndPag(IndicadorPagamento indicadorPagamento) => IndPag = indicadorPagamento;
#endif
    }

    /// <summary>
    /// Classe de informações do Card
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Card")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Card
    {
        /// <summary>
        /// Tipo de Integração do processo de pagamento com o sistema de automação da empresa
        /// </summary>
        [XmlElement("tpIntegra")]
        public TipoIntegracaoPagamento TpIntegra { get; set; }

        /// <summary>
        /// CNPJ da instituição de pagamento
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Bandeira da operadora de cartão
        /// </summary>
        [XmlElement("tBand")]
#if INTEROP
        public BandeiraOperadoraCartao TBand { get; set; } = (BandeiraOperadoraCartao)(-1);
#else
        public BandeiraOperadoraCartao? TBand { get; set; }
#endif

        /// <summary>
        /// Número de autorização da operação com cartões, PIX, boletos e outros pagamentos eletrônicos
        /// </summary>
        [XmlElement("cAut")]
        public string CAut { get; set; }

        /// <summary>
        /// CNPJ do beneficiário do pagamento
        /// </summary>
        [XmlElement("CNPJReceb")]
        public string CNPJReceb { get; set; }

        /// <summary>
        /// Identificador do terminal de pagamento
        /// </summary>
        [XmlElement("idTermPag")]
        public string IdTermPag { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeCNPJ() => !string.IsNullOrWhiteSpace(CNPJ);

#if INTEROP
        public bool ShouldSerializeTBand() => TBand != (BandeiraOperadoraCartao)(-1);
#else
        public bool ShouldSerializeTBand() => TBand != null;
#endif

        public bool ShouldSerializeCAut() => !string.IsNullOrWhiteSpace(CAut);

        public bool ShouldSerializeCNPJReceb() => !string.IsNullOrWhiteSpace(CNPJReceb);

        public bool ShouldSerializeIdTermPag() => !string.IsNullOrWhiteSpace(IdTermPag);

        #endregion
    }

    /// <summary>
    /// 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfIntermed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfIntermed
    {
        private string IdCadIntTranField;

        /// <summary>
        /// CNPJ do Intermediador da Transação (agenciador, plataforma de delivery, marketplace e similar) de serviços e de negócios
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Identificador cadastrado no intermediador
        /// </summary>
        [XmlElement("idCadIntTran")]
        public string IdCadIntTran
        {
            get => IdCadIntTranField;
            set
            {
                if (value.Length < 2 || value.Length > 60)
                {
                    throw new Exception("Conteúdo da TAG <idCadIntTran> filha da TAG <infIntermed> deve ter entre 2 até 60 caracteres.");
                }

                IdCadIntTranField = value;
            }
        }
    }

    /// <summary>
    /// Classe de informações do InfAdic
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfAdic")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfAdic
    {
        private string InfAdFiscoField;
        private string InfCplField;

        /// <summary>
        /// Informações adicionais de interesse do Fisco
        /// </summary>
        [XmlElement("infAdFisco")]
        public string InfAdFisco
        {
            get => InfAdFiscoField;
            set => InfAdFiscoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(2000).Trim());
        }

        /// <summary>
        /// Informações complementares de interesse do Contribuinte
        /// </summary>
        [XmlElement("infCpl")]
        public string InfCpl
        {
            get => InfCplField;
            set => InfCplField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(5000).Trim());
        }

        /// <summary>
        /// Campo de uso livre do contribuinte informar o nome do campo no atributo xCampo e o conteúdo do campo no xTexto
        /// </summary>
        [XmlElement("obsCont")]
        public List<ObsCont> ObsCont { get; set; }

        /// <summary>
        /// Uso exclusivo do fisco. Existe somente para deserialização, não utilizar.
        /// </summary>
        [XmlElement("obsFisco")]
        public List<ObsFisco> ObsFisco { get; set; }

        /// <summary>
        /// Grupo de informações do processo referenciado
        /// </summary>
        [XmlElement("procRef")]
        public List<ProcRef> ProcRef { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeInfAdFisco() => !string.IsNullOrWhiteSpace(InfAdFisco);

        public bool ShouldSerializeInfCpl() => !string.IsNullOrWhiteSpace(InfCpl);

        #endregion

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

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="procRef">Elemento</param>
        public void AddProcRef(ProcRef procRef)
        {
            if (ProcRef == null)
            {
                ProcRef = new List<ProcRef>();
            }

            ProcRef.Add(procRef);
        }

        /// <summary>
        /// Retorna o elemento da lista ProcRef (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ProcRef</returns>
        public ProcRef GetProcRef(int index)
        {
            if ((ProcRef?.Count ?? 0) == 0)
            {
                return default;
            };

            return ProcRef[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ProcRef
        /// </summary>
        public int GetProcRefCount => (ProcRef != null ? ProcRef.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações do ObsCont
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsCont")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsCont
    {
        private string XTextoField;

        /// <summary>
        /// Conteúdo do campo
        /// </summary>
        [XmlElement("xTexto")]
        public string XTexto
        {
            get => XTextoField;
            set => XTextoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Identificação do campo
        /// </summary>
        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

    /// <summary>
    /// Classe de informações de ObsFisco
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ObsFisco")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ObsFisco
    {
        private string XTextoField;

        /// <summary>
        /// Conteúdo do campo de interesse do Fisco
        /// </summary>
        [XmlElement("xTexto")]
        public string XTexto
        {
            get => XTextoField;
            set => XTextoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Nome de identificação do campo
        /// </summary>
        [XmlAttribute(AttributeName = "xCampo")]
        public string XCampo { get; set; }
    }

    /// <summary>
    /// Classe de informações do ProcRef
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ProcRef")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ProcRef
    {
        /// <summary>
        /// Indentificador do processo ou ato concessório
        /// </summary>
        [XmlElement("nProc")]
        public string NProc { get; set; }

        /// <summary>
        /// Origem do processo
        /// </summary>
        [XmlElement("indProc")]
        public IndicadorOrigemProcesso IndProc { get; set; }

        /// <summary>
        /// Tipo do ato concessório
        /// </summary>
#if INTEROP
        [XmlElement("tpAto")]
        public TipoAtoConcessorio TpAto { get; set; } = (TipoAtoConcessorio)(-1);

#else
        [XmlElement("tpAto")]
        public TipoAtoConcessorio? TpAto { get; set; }
#endif

        #region ShouldSerialize

        public bool ShouldSerializeTpAto() => TpAto != null && TpAto != (TipoAtoConcessorio)(-1);

        #endregion
    }

    /// <summary>
    /// Classe de informações do Exporta
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Exporta")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Exporta
    {
        private UFBrasil UFSaidaPaisField;
        private string XLocExportaField;
        private string XLocDespachoField;

        /// <summary>
        /// Sigla da UF de Embarque ou de transposição de fronteira
        /// </summary>
        [XmlElement("UFSaidaPais")]
        public UFBrasil UFSaidaPais
        {
            get => UFSaidaPaisField;
            set
            {
                if (value == UFBrasil.EX || value == UFBrasil.AN)
                {
                    throw new Exception("Conteúdo da TAG <UFSaidaPais> inválido. Não pode ser informado EX ou AN.");
                }
                else
                {
                    UFSaidaPaisField = value;
                }
            }
        }

        /// <summary>
        /// Local de Embarque ou de transposição de fronteira
        /// </summary>
        [XmlElement("xLocExporta")]
        public string XLocExporta
        {
            get => XLocExportaField;
            set => XLocExportaField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Descrição do local de despacho
        /// </summary>
        [XmlElement("xLocDespacho")]
        public string XLocDespacho
        {
            get => XLocDespachoField;
            set => XLocDespachoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        #region ShouldSerialize

        public bool ShouldSerializeXLocDespacho() => !string.IsNullOrWhiteSpace(XLocDespacho);

        #endregion
    }

    /// <summary>
    /// Classe de informações de Compra
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Compra")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Compra
    {
        /// <summary>
        /// Informação da Nota de Empenho de compras públicas
        /// </summary>
        [XmlElement("xNEmp")]
        public string XNEmp { get; set; }

        /// <summary>
        /// Informação do pedido
        /// </summary>
        [XmlElement("xPed")]
        public string XPed { get; set; }

        /// <summary>
        /// Informação do contrato
        /// </summary>
        [XmlElement("xCont")]
        public string XCont { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeXNEmp() => !string.IsNullOrWhiteSpace(XNEmp);

        public bool ShouldSerializeXPed() => !string.IsNullOrWhiteSpace(XPed);

        public bool ShouldSerializeXCont() => !string.IsNullOrWhiteSpace(XCont);

        #endregion
    }

    /// <summary>
    /// Classe de informações de Cana
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Cana")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Cana
    {
        /// <summary>
        /// Identificação da safra
        /// </summary>
        [XmlElement("safra")]
        public string Safra { get; set; }

        /// <summary>
        /// Mês e Ano de Referência, formato: MM/AAAA
        /// </summary>
        [XmlElement("ref")]
        public string Ref { get; set; }

        /// <summary>
        /// Fornecimentos diários
        /// </summary>
        [XmlElement("forDia")]
        public List<ForDia> ForDia { get; set; }

        /// <summary>
        /// Total do mês
        /// </summary>
        [XmlIgnore]
        public double QTotMes { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QTotMes para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qTotMes")]
        public string QTotMesField
        {
            get => QTotMes.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotMes = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total Anterior
        /// </summary>
        [XmlIgnore]
        public double QTotAnt { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QTotAnt para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qTotAnt")]
        public string QTotAntField
        {
            get => QTotAnt.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotAnt = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total Geral
        /// </summary>
        [XmlIgnore]
        public double QTotGer { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade QTotGer para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qTotGer")]
        public string QTotGerField
        {
            get => QTotGer.ToString("F10", CultureInfo.InvariantCulture);
            set => QTotGer = Converter.ToDouble(value);
        }

        /// <summary>
        /// Deduções - Taxas e Contribuições
        /// </summary>
        [XmlElement("deduc")]
        public List<Deduc> Deduc { get; set; }

        /// <summary>
        /// Valor  dos fornecimentos
        /// </summary>
        [XmlIgnore]
        public double VFor { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VFor para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vFor")]
        public string VForField
        {
            get => VFor.ToString("F2", CultureInfo.InvariantCulture);
            set => VFor = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Total das Deduções
        /// </summary>
        [XmlIgnore]
        public double VTotDed { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VTotDed para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotDed")]
        public string VTotDedField
        {
            get => VTotDed.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotDed = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor Líquido dos fornecimentos
        /// </summary>
        [XmlIgnore]
        public double VLiqFor { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VLiqFor para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vLiqFor")]
        public string VLiqForField
        {
            get => VLiqFor.ToString("F2", CultureInfo.InvariantCulture);
            set => VLiqFor = Converter.ToDouble(value);
        }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="forDia">Elemento</param>
        public void AddForDia(ForDia forDia)
        {
            if (ForDia == null)
            {
                ForDia = new List<ForDia>();
            }

            ForDia.Add(forDia);
        }

        /// <summary>
        /// Retorna o elemento da lista ForDia (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da ForDia</returns>
        public ForDia GetForDia(int index)
        {
            if ((ForDia?.Count ?? 0) == 0)
            {
                return default;
            };

            return ForDia[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista ForDia
        /// </summary>
        public int GetForDiaCount => (ForDia != null ? ForDia.Count : 0);

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="deduc">Elemento</param>
        public void AddDeduc(Deduc deduc)
        {
            if (Deduc == null)
            {
                Deduc = new List<Deduc>();
            }

            Deduc.Add(deduc);
        }

        /// <summary>
        /// Retorna o elemento da lista Deduc (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Deduc</returns>
        public Deduc GetDeduc(int index)
        {
            if ((Deduc?.Count ?? 0) == 0)
            {
                return default;
            };

            return Deduc[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Deduc
        /// </summary>
        public int GetDeducCount => (Deduc != null ? Deduc.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações de ForDia
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ForDia")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ForDia
    {
        /// <summary>
        /// Quantidade em quilogramas - peso líquido
        /// </summary>
        [XmlIgnore]
        public double Qtde { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade Qtde para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("qtde")]
        public string QtdeField
        {
            get => Qtde.ToString("F10", CultureInfo.InvariantCulture);
            set => Qtde = Converter.ToDouble(value);
        }

        /// <summary>
        /// Número do dia
        /// </summary>
        [XmlAttribute(AttributeName = "dia")]
        public int Dia { get; set; }
    }

    /// <summary>
    /// Classe de informações do Deduc
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Deduc")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Deduc
    {
        private string XDedField;

        /// <summary>
        /// Descrição da Dedução
        /// </summary>
        [XmlElement("xDed")]
        public string XDed
        {
            get => XDedField;
            set => XDedField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Valor da dedução
        /// </summary>
        [XmlIgnore]
        public double VDed { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VDed para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vDed")]
        public string VDedField
        {
            get => VDed.ToString("F2", CultureInfo.InvariantCulture);
            set => VDed = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Classe de informações do InfRespTec
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfRespTec")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfRespTec
    {
        private string XContatoField;
        private string HashCSRTField;

        /// <summary>
        /// CNPJ
        /// </summary>
        [XmlElement("CNPJ")]
        public string CNPJ { get; set; }

        /// <summary>
        /// Informar o nome da pessoa a ser contatada na empresa desenvolvedora do sistema utilizado na emissão do documento fiscal eletrônico
        /// </summary>
        [XmlElement("xContato")]
        public string XContato
        {
            get => XContatoField;
            set => XContatoField = (value == null ? value : XMLUtility.UnescapeReservedCharacters(value).Truncate(60).Trim());
        }

        /// <summary>
        /// Informar o e-mail da pessoa a ser contatada na empresa desenvolvedora do sistema
        /// </summary>
        [XmlElement("email")]
        public string Email { get; set; }

        /// <summary>
        /// Informar o telefone da pessoa a ser contatada na empresa desenvolvedora do sistema. Preencher com o Código DDD + número do telefone
        /// </summary>
        [XmlElement("fone")]
        public string Fone { get; set; }

        /// <summary>
        /// Identificador do CSRT utilizado para montar o hash do CSRT
        /// </summary>
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
        /// Esta propriedade deve ser utilizada para informar o CSRT sem o hash, informando ela a DLL irá gerar o conteúdo da tag hashCSRT automaticamente
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

        #endregion
    }

    /// <summary>
    /// Classe de informações do InfSolicNFF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfSolicNFF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfSolicNFF
    {
        /// <summary>
        /// Solicitação do pedido de emissão da NFF
        /// </summary>
        [XmlElement("xSolic")]
        public string XSolic { get; set; }
    }

    /// <summary>
    /// Classe de informações do InfNFeSupl
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.InfNFeSupl")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class InfNFeSupl
    {
        /// <summary>
        /// Texto com o QR-Code impresso no DANFE NFC-e
        /// </summary>
        [XmlElement("qrCode")]
        public string QrCode { get; set; }

        /// <summary>
        /// Informar a URL da Consulta por chave de acesso da NFC-e
        /// </summary>
        [XmlElement("urlChave")]
        public string UrlChave { get; set; }
    }

    /// <summary>
    /// Classe de informações de Agropecuario
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Agropecuario")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Agropecuario
    {
        /// <summary>
        /// Grupo de defensivo agrícola / agrotóxico
        /// </summary>
        [XmlElement("defensivo")]
        public List<Defensivo> Defensivo { get; set; }

        /// <summary>
        /// Grupo de Guia de Trânsito
        /// </summary>
        [XmlElement("guiaTransito")]
        public GuiaTransito GuiaTransito { get; set; }

#if INTEROP

        /// <summary>
        /// Adicionar novo elemento a lista
        /// </summary>
        /// <param name="elemento">Elemento</param>
        public void AddDefensivo(Defensivo elemento)
        {
            if (Defensivo == null)
            {
                Defensivo = new List<Defensivo>();
            }

            Defensivo.Add(elemento);
        }

        /// <summary>
        /// Retorna o elemento da lista Defensivo (Utilizado para linguagens diferentes do CSharp que não conseguem pegar o conteúdo da lista)
        /// </summary>
        /// <param name="index">Índice da lista a ser retornado (Começa com 0 (zero))</param>
        /// <returns>Conteúdo do index passado por parâmetro da Defensivo</returns>
        public Defensivo GetDefensivo(int index)
        {
            if ((Defensivo?.Count ?? 0) == 0)
            {
                return default;
            };

            return Defensivo[index];
        }

        /// <summary>
        /// Retorna a quantidade de elementos existentes na lista Defensivo
        /// </summary>
        public int GetDefensivoCount => (Defensivo != null ? Defensivo.Count : 0);

#endif
    }

    /// <summary>
    /// Classe de informações de Defensivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.Defensivo")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class Defensivo
    {
        /// <summary>
        /// Informar o número da receita ou receituário de aplicação do defensivo
        /// </summary>
        [XmlElement("nReceituario")]
        public string NReceituario { get; set; }

        /// <summary>
        /// Informar o CPF do Responsável Técnico legalmente habilitado, como engenheiro agrônomo, engenheiro florestal e técnico agrícola.
        /// </summary>
        [XmlElement("CPFRespTec")]
        public string CPFRespTec { get; set; }
    }

    /// <summary>
    /// Classe de informações do GuiaTransito
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GuiaTransito")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GuiaTransito
    {
        /// <summary>
        /// Tipo da guia
        /// </summary>
        [XmlElement("tpGuia")]
        public TipoGuiaTransito TpGuia { get; set; }

        /// <summary>
        /// UF de emissão da guia
        /// </summary>
        [XmlElement("UFGuia")]
        public UFBrasil UFGuia { get; set; }

        /// <summary>
        /// Informar sempre que houver a série da guia
        /// </summary>
        [XmlElement("serieGuia")]
        public string SerieGuia { get; set; }

        /// <summary>
        /// Número da Guia
        /// </summary>
        [XmlElement("nGuia")]
        public string NGuia { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeSerieGuia() => !string.IsNullOrWhiteSpace(SerieGuia);

        #endregion
    }

    /// <summary>
    /// Grupo de Compra Governamental
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCompraGov")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    /// Informações do imposto seletivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IS
    {
        /// <summary>
        /// Código de Situação Tributária do Imposto Seletivo
        /// </summary>
        [XmlElement("CSTIS")]
        public string CSTIS { get; set; }

        /// <summary>
        /// Código de classificação tributária do imposto seletivo
        /// </summary>
        [XmlElement("cClassTribIS")]
        public string CClassTribIS { get; set; }

        /// <summary>
        /// Valor da BC do Imposto Seletivo
        /// </summary>
        [XmlIgnore]
        public double VBCIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vBCIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCIS")]
        public string VBCISField
        {
            get => VBCIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota do Imposto Seletivo
        /// </summary>
        [XmlIgnore]
        public double PIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pIS")]
        public string PISField
        {
            get => PIS.ToString("F4", CultureInfo.InvariantCulture);
            set => PIS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota específica por unidade de medida apropriada
        /// </summary>
        [XmlIgnore]
        public double PISEspec { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade PICMS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pISEspec")]
        public string PISEspecField
        {
            get => PISEspec.ToString("F4", CultureInfo.InvariantCulture);
            set => PISEspec = Converter.ToDouble(value);
        }

        /// <summary>
        /// Unidade de medida tributável
        /// </summary>
        [XmlElement("uTrib")]
        public string UTrib { get; set; }

        /// <summary>
        /// Quantidade Tributável
        /// </summary>
        [XmlElement("qTrib")]
        public double QTrib { get; set; }

        /// <summary>
        /// Valor do Imposto Seletivo
        /// </summary>
        [XmlIgnore]
        public double VIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIS")]
        public string VISField
        {
            get => VIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIS = Converter.ToDouble(value);
        }

        #region ShouldSerialize

        public bool ShouldSerializeVBCISField() => VBCIS > 0;
        public bool ShouldSerializePISField() => VBCIS > 0;
        public bool ShouldSerializePISEspecField() => VBCIS > 0 && PISEspec > 0;

        public bool ShouldSerializeUTrib() => !string.IsNullOrWhiteSpace(UTrib);
        public bool ShouldSerializeQTrib() => !string.IsNullOrWhiteSpace(UTrib);

        #endregion
    }

    /// <summary>
    /// Informações do Imposto de Bens e Serviços - IBS e da Contribuição de Bens e Serviços - CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
        /// Grupo de Informações do IBS e da CBS
        /// </summary>
        [XmlElement("gIBSCBS")]
        public GIBSCBS GIBSCBS { get; set; }

        /// <summary>
        /// Grupo de Informações do IBS e CBS em operações com imposto monofásico
        /// </summary>
        [XmlElement("gIBSCBSMono")]
        public GIBSCBSMono GIBSCBSMono { get; set; }
    }

    /// <summary>
    /// Grupo de Informações do IBS e da CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSUF")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GDif")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GDevTrib")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GRed")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMun")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBS")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GTribRegular")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSCredPres")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
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

    /// <summary>
    /// Grupo de Informações do IBS e CBS em operações com imposto monofásico
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSCBSMono")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSCBSMono
    {
        /// <summary>
        /// Quantidade tributada na monofasia
        /// </summary>
        [XmlElement("qBCMono")]
        public decimal QBCMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS
        /// </summary>
        [XmlIgnore]
        public double AdRemIBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade adRemIBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemIBS")]
        public string AdRemIBSField
        {
            get => AdRemIBS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemIBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota ad rem da CBS
        /// </summary>
        [XmlIgnore]
        public double AdRemCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade adRemCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemCBS")]
        public string AdRemCBSField
        {
            get => AdRemCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico
        /// </summary>
        [XmlIgnore]
        public double VIBSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMono")]
        public string VIBSMonoField
        {
            get => VIBSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do CBS monofásico
        /// </summary>
        [XmlIgnore]
        public double VCBSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMono")]
        public string VCBSMonoField
        {
            get => VCBSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMono = Converter.ToDouble(value);
        }

        #region Mono Reten

        /// <summary>
        /// Quantidade tributada sujeita à retenção na monofasia
        /// </summary>
        [XmlElement("qBCMonoReten")]
        public double QBCMonoReten { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS sujeito a retenção 
        /// </summary>
        [XmlIgnore]
        public double AdRemIBSReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemIBSReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemIBSReten")]
        public string AdRemIBSRetenField
        {
            get => AdRemIBSReten.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemIBSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMonoReten")]
        public string VIBSMonoRetenField
        {
            get => VIBSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota ad rem do CBS sujeito a retenção 
        /// </summary>
        [XmlIgnore]
        public double AdRemCBSReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemCBSReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemCBSReten")]
        public string AdRemCBSRetenField
        {
            get => AdRemCBSReten.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemCBSReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do CBS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMonoReten")]
        public string VCBSMonoRetenField
        {
            get => VCBSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }

        #endregion

        #region ShouldSerialize Mono Reten

        public bool ShouldSerializeQBCMonoReten() => QBCMonoReten > 0;
        public bool ShouldSerializeAdRemIBSRetenField() => AdRemIBSReten > 0;
        public bool ShouldSerializeVIBSMonoRetenField() => VIBSMonoReten > 0;
        public bool ShouldSerializeAdRemCBSRetenField() => AdRemCBSReten > 0;
        public bool ShouldSerializeVCBSMonoRetenField() => VCBSMonoReten > 0;

        #endregion

        #region Mono Ret

        /// <summary>
        /// Quantidade tributada retida anteriormente 
        /// </summary>
        [XmlElement("qBCMonoRet")]
        public double QBCMonoRet { get; set; }

        /// <summary>
        /// Alíquota ad rem do IBS retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double AdRemIBSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemIBSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemIBSRet")]
        public string AdRemIBSRetField
        {
            get => AdRemIBSRet.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemIBSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMonoRet")]
        public string VIBSMonoRetField
        {
            get => VIBSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMonoRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Alíquota ad rem da CBS retida anteriormente
        /// </summary>
        [XmlIgnore]
        public double AdRemCBSRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade AdRemCBSRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("adRemCBSRet")]
        public string AdRemCBSRetField
        {
            get => AdRemCBSRet.ToString("F4", CultureInfo.InvariantCulture);
            set => AdRemCBSRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor da CBS retida anteriormente
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMonoRet")]
        public string VCBSMonoRetField
        {
            get => VCBSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMonoRet = Converter.ToDouble(value);
        }

        #endregion

        #region ShouldSerialize Mono Ret

        public bool ShouldSerializeQBCMonoRet() => QBCMonoRet > 0;
        public bool ShouldSerializeAdRemIBSRetField() => AdRemIBSRet > 0;
        public bool ShouldSerializeVIBSMonoRetField() => VIBSMonoRet > 0;
        public bool ShouldSerializeAdRemCBSRetField() => AdRemCBSRet > 0;
        public bool ShouldSerializeVCBSMonoRetField() => VCBSMonoRet > 0;

        #endregion

        #region Mono Dif

        /// <summary>
        /// Percentual do diferimento do imposto monofásico
        /// </summary>
        [XmlIgnore]
        public double PDifIBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pDifIBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDifIBS")]
        public string PDifIBSField
        {
            get => PDifIBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PDifIBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do IBS monofásico diferido
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIBSMonoDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMonoDif")]
        public string VIBSMonoDifField
        {
            get => VIBSMonoDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMonoDif = Converter.ToDouble(value);
        }

        /// <summary>
        /// Percentual do diferimento do imposto monofásico
        /// </summary>
        [XmlIgnore]
        public double PDifCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade pDifCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("pDifCBS")]
        public string PDifCBSField
        {
            get => PDifCBS.ToString("F4", CultureInfo.InvariantCulture);
            set => PDifCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor do CBS monofásico diferido
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoDif { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vCBSMonoDif para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMonoDif")]
        public string VCBSMonoDifField
        {
            get => VCBSMonoDif.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMonoDif = Converter.ToDouble(value);
        }

        #endregion

        #region ShouldSerialize Mono Dif

        public bool ShouldSerializePDifIBSField() => PDifIBS > 0;
        public bool ShouldSerializeVIBSMonoDifField() => VIBSMonoDif > 0;
        public bool ShouldSerializePDifCBSField() => PDifCBS > 0;
        public bool ShouldSerializeVCBSMonoDifField() => VCBSMonoDif > 0;

        #endregion

        /// <summary>
        /// Total de IBS Monofásico do item
        /// </summary>
        [XmlIgnore]
        public double VTotIBSMonoItem { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vTotIBSMonoItem para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotIBSMonoItem")]
        public string VTotIBSMonoItemField
        {
            get => VTotIBSMonoItem.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotIBSMonoItem = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total de CBS Monofásico do item
        /// </summary>
        [XmlIgnore]
        public double VTotCBSMonoItem { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vTotCBSMonoItem para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vTotCBSMonoItem")]
        public string VTotCBSMonoItemField
        {
            get => VTotCBSMonoItem.ToString("F2", CultureInfo.InvariantCulture);
            set => VTotCBSMonoItem = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Documento Fiscal Eletrônico Referenciado 
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.DFeReferenciado")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class DFeReferenciado
    {
        /// <summary>
        /// Chave de acesso do DF-e referenciado
        /// </summary>
        [XmlElement("chaveAcesso")]
        public string ChaveAcesso { get; set; }

        /// <summary>
        /// Número do item do documento referenciado.
        /// Corresponde ao atributo "nItem" do elemento "det" do documento original.
        /// </summary>
        [XmlElement("nItem")]
        public string NItem { get; set; }

        #region ShouldSerialize

        public bool ShouldSerializeNItem() => !string.IsNullOrWhiteSpace(NItem);

        #endregion
    }

    /// <summary>
    /// Grupo total do imposto seletivo
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.ISTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class ISTot
    {
        /// <summary>
        /// Total do imposto seletivo
        /// </summary>
        [XmlIgnore]
        public double VIS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIS")]
        public string VISField
        {
            get => VIS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIS = Converter.ToDouble(value);
        }
    }

    /// <summary>
    /// Totais da NF-e com IBS e CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.IBSCBSTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class IBSCBSTot
    {
        /// <summary>
        /// Valor total da BC do IBS e da CBS
        /// </summary>
        [XmlIgnore]
        public double VBCIBSCBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vBCIBSCBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vBCIBSCBS")]
        public string VBCIBSCBSField
        {
            get => VBCIBSCBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VBCIBSCBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Grupo total do IBS
        /// </summary>
        [XmlElement("gIBS")]
        public GIBSTot GIBS { get; set; }

        /// <summary>
        /// Grupo total do CBS
        /// </summary>
        [XmlElement("gCBS")]
        public GCBSTot GCBS { get; set; }

        /// <summary>
        /// Grupo total da Monofasia
        /// </summary>
        [XmlElement("gMono")]
        public GMono GMono { get; set; }
    }

    /// <summary>
    /// Grupo total do IBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSTot
    {
        /// <summary>
        /// Grupo total do IBS da UF
        /// </summary>
        [XmlElement("gIBSUF")]
        public GIBSUFTot GIBSUF { get; set; }

        /// <summary>
        /// Grupo total do IBS do Município
        /// </summary>
        [XmlElement("gIBSMun")]
        public GIBSMunTot GIBSMun { get; set; }

        /// <summary>
        /// Valor total do IBS
        /// </summary>
        [XmlIgnore]
        public double VIBS { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade vIBS para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBS")]
        public string VIBSField
        {
            get => VIBS.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBS = Converter.ToDouble(value);
        }

        /// <summary>
        /// Valor total do crédito presumido
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
        /// Valor total do crédito presumido em condição suspensiva
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
    /// Grupo total do IBS da UF
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSUFTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSUFTot
    {
        /// <summary>
        /// Valor total do diferimento
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

        /// <summary>
        /// Valor total de devolução de tributos
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

        /// <summary>
        /// Valor total do IBS da UF
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
    /// Grupo total do IBS do Município
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GIBSMunTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GIBSMunTot
    {
        /// <summary>
        /// Valor total do diferimento
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

        /// <summary>
        /// Valor total de devolução de tributos
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

        /// <summary>
        /// Valor total do IBS do Município
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
    /// Grupo total da CBS
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GCBSTot")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GCBSTot
    {
        /// <summary>
        /// Valor total do crédito presumido
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
        /// Valor total do crédito presumido em condição suspensiva
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

        /// <summary>
        /// Valor total do diferimento
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

        /// <summary>
        /// Valor total de devolução de tributos
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

        /// <summary>
        /// Valor total do CBS
        /// </summary>
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
    }
    /// <summary>
    /// Grupo total da Monofasia
    /// </summary>
#if INTEROP
    [ClassInterface(ClassInterfaceType.AutoDual)]
    [ProgId("Unimake.Business.DFe.Xml.NFe.GMono")]
    [ComVisible(true)]
#endif
    [Serializable()]
    [XmlType(AnonymousType = true, Namespace = "http://www.portalfiscal.inf.br/nfe")]
    public class GMono
    {
        /// <summary>
        /// Total do IBS monofásico
        /// </summary>
        [XmlIgnore]
        public double VIBSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMono")]
        public string VIBSMonoField
        {
            get => VIBSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do CBS monofásico
        /// </summary>
        [XmlIgnore]
        public double VCBSMono { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMono para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMono")]
        public string VCBSMonoField
        {
            get => VCBSMono.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMono = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do IBS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMonoReten")]
        public string VIBSMonoRetenField
        {
            get => VIBSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do CBS monofásico sujeito a retenção
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoReten { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMonoReten para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMonoReten")]
        public string VCBSMonoRetenField
        {
            get => VCBSMonoReten.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMonoReten = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do IBS monofásico retido anteriormente
        /// </summary>
        [XmlIgnore]
        public double VIBSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VIBSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vIBSMonoRet")]
        public string VIBSMonoRetField
        {
            get => VIBSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VIBSMonoRet = Converter.ToDouble(value);
        }

        /// <summary>
        /// Total do CBS monofásico retida anteriormente
        /// </summary>
        [XmlIgnore]
        public double VCBSMonoRet { get; set; }

        /// <summary>
        /// Propriedade auxiliar para serialização/desserialização do XML (Utilize sempre a propriedade VCBSMonoRet para atribuir ou resgatar o valor)
        /// </summary>
        [XmlElement("vCBSMonoRet")]
        public string VCBSMonoRetField
        {
            get => VCBSMonoRet.ToString("F2", CultureInfo.InvariantCulture);
            set => VCBSMonoRet = Converter.ToDouble(value);
        }

    }
}